use std::{path::PathBuf, process::Output};

use serde_json::{Value, json};

use super::{GhClient, classify_failure, diagnostic, remote_result};
use crate::remote::{RemoteFailure, RemoteFailureKind};
use crate::review_source::{
    ReviewCommitDetail, ReviewCommitDetailRequest, ReviewCommitFile, ReviewComparison,
    ReviewComparisonRequest, ReviewSource, ReviewSourceRequest, valid_object_id,
};

const SOURCE_QUERY: &str = "query($owner:String!,$name:String!,$commit:String!,$expression:String!){repository(owner:$owner,name:$name){commit:object(expression:$commit){__typename oid} source:object(expression:$expression){__typename oid ... on Blob{byteSize}}}}";

impl GhClient {
    pub(super) async fn read_commit_detail_at(
        &self,
        directory: PathBuf,
        request: ReviewCommitDetailRequest,
    ) -> Result<ReviewCommitDetail, RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
            format!(
                "repos/{}/commits/{}",
                request.repository.repository_name(),
                request.commit
            )
            .into(),
            "--paginate".into(),
            "--slurp".into(),
        ];
        remote_result(
            self.run_native(directory, argument, None, 1024 * 1024, move |output| {
                decode_commit_pages(response(output)?, &request.commit)
            })
            .await,
        )
    }

    pub(super) async fn read_source_at(
        &self,
        directory: PathBuf,
        request: ReviewSourceRequest,
    ) -> Result<ReviewSource, RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        let repository_name = request.repository.repository_name();
        let (owner, name) = repository_name
            .split_once('/')
            .ok_or_else(|| invalid("repository omits owner"))?;
        let body = serde_json::to_vec(&json!({"query":SOURCE_QUERY,"variables":{
            "owner":owner,"name":name,"commit":request.commit,
            "expression":format!("{}:{}", request.commit, request.path)
        }}))
        .map_err(|error| invalid(error.to_string()))?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
            "graphql".into(),
        ];
        let commit = request.commit.clone();
        let identity = remote_result(
            self.run_native(
                directory.clone(),
                argument,
                Some(body),
                4096,
                move |output| decode_identity(response(output)?, &commit),
            )
            .await,
        )?;
        let Some((blob, size)) = identity else {
            return Ok(ReviewSource {
                request,
                blob: None,
                bytes: Vec::new(),
            });
        };
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
            "--header".into(),
            "Accept: application/vnd.github.raw+json".into(),
            format!(
                "repos/{}/git/blobs/{blob}",
                request.repository.repository_name()
            )
            .into(),
        ];
        let bytes = remote_result(
            self.run_native(directory, argument, None, 0, move |output| {
                let bytes = response_bytes(output)?;
                if bytes.len() != size {
                    return Err(invalid("Git blob size changed from immutable metadata"));
                }
                Ok(bytes)
            })
            .await,
        )?;
        Ok(ReviewSource {
            request,
            blob: Some(blob),
            bytes,
        })
    }

    pub(super) async fn read_comparison_at(
        &self,
        directory: PathBuf,
        request: ReviewComparisonRequest,
    ) -> Result<ReviewComparison, RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
            format!(
                "repos/{}/compare/{}...{}?per_page=1",
                request.repository.repository_name(),
                request.base,
                request.head
            )
            .into(),
        ];
        remote_result(
            self.run_native(directory, argument, None, 4096, move |output| {
                let value = response(output)?;
                let base = value.pointer("/base_commit/sha").and_then(Value::as_str);
                let merge_base = value
                    .pointer("/merge_base_commit/sha")
                    .and_then(Value::as_str)
                    .filter(|identity| valid_object_id(identity))
                    .ok_or_else(|| invalid("comparison omitted immutable merge base"))?;
                if base != Some(request.base.as_str()) {
                    return Err(invalid("comparison changed captured base commit"));
                }
                Ok(ReviewComparison {
                    base: request.base,
                    head: request.head,
                    merge_base: merge_base.into(),
                })
            })
            .await,
        )
    }
}

fn decode_commit_pages(
    value: Value,
    requested_commit: &str,
) -> Result<ReviewCommitDetail, RemoteFailure> {
    let pages = value
        .as_array()
        .ok_or_else(|| invalid("commit detail pages are not a collection"))?;
    let mut detail: Option<ReviewCommitDetail> = None;
    for page in pages {
        let mut next = decode_commit_detail(page.clone(), requested_commit)?;
        if let Some(previous) = &mut detail {
            if previous.parent != next.parent || previous.subject != next.subject {
                return Err(invalid("commit detail metadata changed during pagination"));
            }
            previous.file.append(&mut next.file);
        } else {
            detail = Some(next);
        }
    }
    detail.ok_or_else(|| invalid("commit detail pages are empty"))
}

fn decode_commit_detail(
    value: Value,
    requested_commit: &str,
) -> Result<ReviewCommitDetail, RemoteFailure> {
    let commit = value
        .get("sha")
        .and_then(Value::as_str)
        .filter(|commit| *commit == requested_commit && valid_object_id(commit))
        .ok_or_else(|| invalid("commit detail changed captured commit identity"))?;
    let parents = value
        .get("parents")
        .and_then(Value::as_array)
        .ok_or_else(|| invalid("commit detail omitted parent identities"))?;
    let parent = parents
        .first()
        .map(|parent| {
            parent
                .get("sha")
                .and_then(Value::as_str)
                .filter(|identity| valid_object_id(identity))
                .map(str::to_owned)
                .ok_or_else(|| invalid("commit detail contains an invalid parent identity"))
        })
        .transpose()?;
    let message = value
        .pointer("/commit/message")
        .and_then(Value::as_str)
        .ok_or_else(|| invalid("commit detail omitted its message"))?;
    let subject = message
        .lines()
        .next()
        .unwrap_or_default()
        .trim_end_matches('\r');
    if subject.is_empty() {
        return Err(invalid("commit detail has an invalid subject"));
    }
    let file = value
        .get("files")
        .and_then(Value::as_array)
        .ok_or_else(|| invalid("commit detail omitted changed files"))?;
    let file = file
        .iter()
        .map(|file| {
            let path = file
                .get("filename")
                .and_then(Value::as_str)
                .filter(|path| valid_path(path))
                .ok_or_else(|| invalid("commit detail contains an invalid file path"))?;
            let previous_path = file
                .get("previous_filename")
                .and_then(Value::as_str)
                .map(|path| {
                    valid_path(path)
                        .then(|| path.to_owned())
                        .ok_or_else(|| invalid("commit detail contains an invalid previous path"))
                })
                .transpose()?;
            let status = file
                .get("status")
                .and_then(Value::as_str)
                .filter(|status| {
                    matches!(
                        *status,
                        "added"
                            | "removed"
                            | "modified"
                            | "renamed"
                            | "copied"
                            | "changed"
                            | "unchanged"
                    )
                })
                .ok_or_else(|| invalid("commit detail contains an invalid file status"))?;
            let patch = file.get("patch").and_then(Value::as_str).map(str::to_owned);
            Ok(ReviewCommitFile {
                path: path.into(),
                previous_path,
                status: status.into(),
                patch,
            })
        })
        .collect::<Result<Vec<_>, RemoteFailure>>()?;
    Ok(ReviewCommitDetail {
        commit: commit.into(),
        parent,
        subject: subject.into(),
        file,
    })
}

fn valid_path(path: &str) -> bool {
    !path.is_empty()
        && !path.contains('\0')
        && !path.starts_with('/')
        && path.split('/').all(|part| !matches!(part, "" | "." | ".."))
}

fn decode_identity(value: Value, commit: &str) -> Result<Option<(String, usize)>, RemoteFailure> {
    if value
        .get("errors")
        .and_then(Value::as_array)
        .is_some_and(|errors| !errors.is_empty())
    {
        return Err(invalid("immutable source GraphQL read returned errors"));
    }
    let repository = value
        .pointer("/data/repository")
        .filter(|value| value.is_object())
        .ok_or_else(|| invalid("immutable source repository is unavailable"))?;
    if repository
        .pointer("/commit/__typename")
        .and_then(Value::as_str)
        != Some("Commit")
        || repository.pointer("/commit/oid").and_then(Value::as_str) != Some(commit)
    {
        return Err(invalid("immutable source commit is unavailable"));
    }
    let source = repository
        .get("source")
        .ok_or_else(|| invalid("source lookup omitted result"))?;
    if source.is_null() {
        return Ok(None);
    }
    if source.get("__typename").and_then(Value::as_str) != Some("Blob") {
        return Err(invalid("repository path is not a Git blob"));
    }
    let blob = source
        .get("oid")
        .and_then(Value::as_str)
        .filter(|identity| valid_object_id(identity))
        .ok_or_else(|| invalid("source omitted immutable blob identity"))?;
    let size = source
        .get("byteSize")
        .and_then(Value::as_u64)
        .and_then(|size| usize::try_from(size).ok())
        .ok_or_else(|| {
            invalid("source byteSize is missing or exceeds this platform's addressable size")
        })?;
    Ok(Some((blob.into(), size)))
}

pub(super) fn response(output: Output) -> Result<Value, RemoteFailure> {
    serde_json::from_slice(&response_bytes(output)?).map_err(|error| invalid(error.to_string()))
}

fn response_bytes(output: Output) -> Result<Vec<u8>, RemoteFailure> {
    if !output.status.success() {
        return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
            if output.stderr.is_empty() {
                &output.stdout
            } else {
                &output.stderr
            },
        ))));
    }
    Ok(output.stdout)
}

fn invalid(message: impl Into<String>) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: message.into(),
    }
}
