use std::{path::PathBuf, process::Output};

use serde_json::{Value, json};

use super::{GhClient, classify_failure, diagnostic, remote_result};
use crate::remote::{RemoteFailure, RemoteFailureKind};
use crate::review_source::{
    MAX_SOURCE_BYTES, ReviewCommitDetail, ReviewCommitDetailRequest, ReviewCommitFile,
    ReviewComparison, ReviewComparisonRequest, ReviewSource, ReviewSourceRequest, valid_object_id,
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
        ];
        remote_result(
            self.run_native(directory, argument, None, 1024 * 1024, move |output| {
                decode_commit_detail(response(output)?, &request.commit)
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
            self.run_native(directory, argument, None, MAX_SOURCE_BYTES, move |output| {
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
    if subject.is_empty() || subject.len() > 4096 {
        return Err(invalid("commit detail has an invalid subject"));
    }
    let file = value
        .get("files")
        .and_then(Value::as_array)
        .ok_or_else(|| invalid("commit detail omitted changed files"))?;
    if file.len() > 300 {
        return Err(invalid(
            "commit detail exceeds the 300-file expansion bound",
        ));
    }
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
            if patch
                .as_ref()
                .is_some_and(|patch| patch.len() > 1024 * 1024)
            {
                return Err(invalid("commit file patch exceeds the 1 MiB bound"));
            }
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
        && path.len() <= 4096
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
        .filter(|size| *size <= MAX_SOURCE_BYTES as u64)
        .ok_or_else(|| invalid("source exceeds the 8 MiB native bound"))? as usize;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn source_value(source: Value) -> Value {
        json!({"data":{"repository":{"commit":{"__typename":"Commit","oid":"a".repeat(40)},"source":source}}})
    }

    #[test]
    fn immutable_source_requires_a_proven_commit_before_absence() {
        assert!(
            decode_identity(source_value(Value::Null), &"a".repeat(40))
                .unwrap()
                .is_none()
        );
        assert!(decode_identity(source_value(Value::Null), &"b".repeat(40)).is_err());
        assert!(decode_identity(json!({"data":{"repository":null}}), &"a".repeat(40)).is_err());
        assert!(
            decode_identity(json!({"errors":[{"message":"denied"}]}), &"a".repeat(40)).is_err()
        );
    }

    #[test]
    fn immutable_source_preserves_blob_identity_and_rejects_trees_or_oversize() {
        let blob = "b".repeat(40);
        assert_eq!(
            decode_identity(
                source_value(json!({"__typename":"Blob","oid":blob,"byteSize":12})),
                &"a".repeat(40)
            )
            .unwrap(),
            Some((blob.clone(), 12))
        );
        assert!(
            decode_identity(
                source_value(json!({"__typename":"Tree","oid":blob})),
                &"a".repeat(40)
            )
            .is_err()
        );
        assert!(
            decode_identity(
                source_value(json!({"__typename":"Blob","oid":blob,"byteSize":MAX_SOURCE_BYTES+1})),
                &"a".repeat(40)
            )
            .is_err()
        );
    }

    #[test]
    fn commit_detail_preserves_first_parent_and_marks_root_commits() {
        let commit = "a".repeat(40);
        let parent = "b".repeat(40);
        let detail = decode_commit_detail(
            json!({"sha":commit,"commit":{"message":"subject\n\nbody"},"parents":[{"sha":parent}],"files":[{"filename":"src/lib.rs","status":"modified","patch":"@@ -1 +1 @@\n-old\n+new"}]}),
            &commit,
        )
        .unwrap();
        assert_eq!(detail.parent.as_deref(), Some(parent.as_str()));
        assert_eq!(detail.subject, "subject");
        assert_eq!(detail.file[0].path, "src/lib.rs");

        let root = decode_commit_detail(
            json!({"sha":commit,"commit":{"message":"root"},"parents":[],"files":[]}),
            &commit,
        )
        .unwrap();
        assert_eq!(root.parent, None);
        assert!(
            decode_commit_detail(
                json!({"sha":"c".repeat(40),"commit":{"message":"wrong"},"parents":[],"files":[]}),
                &commit
            )
            .is_err()
        );
    }
}
