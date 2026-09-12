use anyhow::{Context, Result, ensure};
use serde::Serialize;
use serde_json::Value;
use std::sync::Arc;

#[derive(Clone, Debug, Serialize)]
pub struct ReviewThread {
    pub node_id: String,
    pub path: String,
    pub resolved: bool,
    pub outdated: bool,
    pub side: crate::comments::CommentSide,
    pub line: Option<u64>,
    pub start_line: Option<u64>,
    pub original_line: Option<u64>,
    pub original_start_line: Option<u64>,
    pub can_reply: bool,
    pub can_resolve: bool,
    pub can_unresolve: bool,
    pub comment: Vec<Arc<ReviewThreadComment>>,
    pub next_cursor: Option<String>,
    pub total_comments: usize,
    #[serde(skip)]
    additional_bytes: usize,
}

#[derive(Debug, Serialize)]
pub struct ReviewThreadDelivery {
    #[serde(serialize_with = "serialize_thread_summary")]
    pub thread: Arc<ReviewThread>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<forge_buffer::patch::BufferPatch>,
    #[serde(skip)]
    _charge: Arc<super::section::SectionCharge>,
}

fn serialize_thread_summary<Serializer: serde::Serializer>(
    thread: &Arc<ReviewThread>,
    serializer: Serializer,
) -> std::result::Result<Serializer::Ok, Serializer::Error> {
    use serde::ser::SerializeStruct;
    let mut summary = serializer.serialize_struct("ReviewThreadSummary", 4)?;
    summary.serialize_field("node_id", &thread.node_id)?;
    summary.serialize_field("loaded_comments", &thread.comment.len())?;
    summary.serialize_field("total_comments", &thread.total_comments)?;
    summary.serialize_field("next_cursor", &thread.next_cursor)?;
    summary.end()
}

#[derive(Debug, Serialize)]
pub struct ReviewThreadComment {
    pub node_id: String,
    pub database_id: u64,
    pub author: Option<String>,
    pub viewer_did_author: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub created_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub updated_at: Option<String>,
    pub body: String,
    pub url: String,
    pub commit: Option<String>,
    pub original_commit: Option<String>,
    pub line: Option<u64>,
    pub original_line: Option<u64>,
    pub start_line: Option<u64>,
    pub original_start_line: Option<u64>,
    pub diff_hunk: String,
    pub review_node_id: Option<String>,
    pub review_state: Option<String>,
}

impl super::ReviewService {
    pub async fn read_thread(
        &self,
        document_id: &forge_buffer::identity::DocumentId,
        directory: std::path::PathBuf,
        remote: Arc<dyn forge_github::review_api::GithubReviewRemote>,
        thread_node_id: String,
        cursor: Option<String>,
    ) -> Result<ReviewThreadDelivery> {
        use super::{ReviewSectionItem, ReviewSectionKind};
        use forge_github::review_api::{ReviewReadRequest, ReviewSection};
        let owner = self.owner(document_id)?;
        owner.validate_directory(&directory)?;
        let analysis = self
            .analysis
            .clone()
            .context("thread analysis service is unavailable")?;
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("thread job admission is full")?;
        let continuation = cursor.is_some();
        let (request, captured_section, captured, index, width) = {
            let mut document = owner.document.lock().expect("review document poisoned");
            let section = document
                .section
                .get(&ReviewSectionKind::Threads)
                .context("thread section is not retained")?;
            let (index, thread) = section
                .item
                .iter()
                .enumerate()
                .find_map(|(index, item)| {
                    item.thread
                        .as_ref()
                        .filter(|thread| thread.node_id == thread_node_id)
                        .map(|thread| (index, thread.clone()))
                })
                .context("thread is not retained by this document")?;
            ensure!(
                cursor.is_none() || cursor == thread.next_cursor,
                "thread cursor is stale"
            );
            let captured_section = section.clone();
            let request = ReviewReadRequest {
                repository: document.target.repository.clone(),
                number: document.target.number,
                view: ReviewSection::ThreadComments {
                    thread_node_id: thread_node_id.clone(),
                },
                cursor: cursor.clone(),
            };
            ensure!(
                document.thread_loading.insert(thread_node_id.clone()),
                "thread read is already running"
            );
            (
                request,
                captured_section,
                thread,
                index,
                document.projection_width.clone(),
            )
        };
        let service = self.clone();
        let rollback = owner.clone();
        let rollback_thread = thread_node_id.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let spawned = self.spawn(async move {
            let _admission = admission;
            let observed = async {
                let page = service
                    .github
                    .review_page(directory, remote, request)
                    .await?;
                ensure!(
                    page.records.len() <= 100 && page.complete == page.next_cursor.is_none(),
                    "thread page completion is invalid"
                );
                ensure!(
                    page.next_cursor.is_none() || page.next_cursor != cursor,
                    "thread cursor did not advance"
                );
                let permit = analysis
                    .reserve(
                        forge_diff::workers::WorkPriority::Foreground,
                        forge_diff::workers::WorkBudget::new(8 * 1024 * 1024, None),
                    )
                    .map_err(|failure| {
                        anyhow::anyhow!("thread analysis admission failed: {failure:?}")
                    })?;
                let ticket = permit.ticket();
                let previous = captured.clone();
                let projection_width = width.clone();
                let (sender, receiver) = tokio::sync::oneshot::channel();
                permit.submit(move |_| {
                    let result = (|| {
                        let bytes = serde_json::to_vec(&page.records)?.len();
                        ensure!(bytes <= 8 * 1024 * 1024, "thread page exceeds byte limit");
                        let mut candidate = (*previous).clone();
                        if cursor.is_none() {
                            candidate.comment.clear();
                            candidate.additional_bytes = 0;
                        }
                        candidate.comment.extend(
                            page.records
                                .iter()
                                .map(|record| decode_comment(record).map(Arc::new))
                                .collect::<Result<Vec<_>>>()?,
                        );
                        let mut identity = std::collections::HashSet::new();
                        let mut database_identity = std::collections::HashSet::new();
                        ensure!(
                            candidate
                                .comment
                                .iter()
                                .all(|comment| identity.insert(&comment.node_id)
                                    && database_identity.insert(comment.database_id)),
                            "thread contains duplicate comments"
                        );
                        ensure!(
                            candidate.comment.len() <= candidate.total_comments
                                && (!page.complete
                                    || candidate.comment.len() == candidate.total_comments),
                            "thread membership changed during pagination"
                        );
                        candidate.additional_bytes = candidate
                            .additional_bytes
                            .checked_add(bytes)
                            .context("thread byte count overflow")?;
                        ensure!(
                            candidate.additional_bytes <= 8 * 1024 * 1024,
                            "thread collection exceeds byte limit"
                        );
                        candidate.next_cursor = page.next_cursor;
                        let projected = projection_width
                            .as_ref()
                            .map(|width| {
                                super::thread_projection::render_range(
                                    &candidate,
                                    width,
                                    if continuation {
                                        previous.comment.len()
                                    } else {
                                        0
                                    },
                                    !continuation,
                                )
                            })
                            .transpose()?;
                        Ok::<_, anyhow::Error>((candidate, projected))
                    })();
                    let _ = sender.send(result);
                });
                let result = receiver
                    .await
                    .context("thread analysis ended without a result");
                ticket.completed().await;
                result?
            }
            .await;
            let _publication = owner.publication.lock().await;
            let result = (|| {
                let mut document = owner.document.lock().expect("review document poisoned");
                document.thread_loading.remove(&thread_node_id);
                let (thread, projected) = observed?;
                ensure!(
                    document.projection_width == width,
                    "thread presentation width changed during read"
                );
                let thread = Arc::new(thread);
                let previous = document
                    .section
                    .get(&ReviewSectionKind::Threads)
                    .context("thread section was removed")?;
                let item = previous.item.get(index).context("thread section changed")?;
                ensure!(
                    item.thread
                        .as_ref()
                        .is_some_and(|thread| Arc::ptr_eq(thread, &captured)),
                    "thread capture was superseded"
                );
                let bytes = previous
                    .bytes
                    .checked_sub(captured.additional_bytes)
                    .and_then(|bytes| bytes.checked_add(thread.additional_bytes))
                    .context("thread section byte count overflow")?;
                ensure!(
                    bytes <= 8 * 1024 * 1024,
                    "thread section exceeds byte limit"
                );
                let other_bytes: usize = document
                    .section
                    .iter()
                    .filter(|(kind, _)| **kind != ReviewSectionKind::Threads)
                    .map(|(_, state)| state.bytes)
                    .sum();
                ensure!(
                    other_bytes + bytes <= 16 * 1024 * 1024,
                    "review document section limit reached"
                );
                let charge =
                    super::section::SectionCharge::reserve(service.section_bytes.clone(), bytes)?;
                let mut section = previous.clone();
                section.item[index] = Arc::new(ReviewSectionItem {
                    identity: item.identity.clone(),
                    title: item.title.clone(),
                    body: item.body.clone(),
                    url: item.url.clone(),
                    detail: item.detail.clone(),
                    source: item.source.clone(),
                    comparison: item.comparison.clone(),
                    commit: item.commit.clone(),
                    check: item.check.clone(),
                    review: item.review.clone(),
                    thread: Some(thread.clone()),
                });
                section.bytes = bytes;
                section.charge = Some(charge.clone());
                let next_revision = document
                    .section_revision
                    .checked_add(1)
                    .context("review section revision exhausted")?;
                let patch = projected
                    .map(|projected| {
                        document.project_thread(&thread_node_id, continuation, projected)
                    })
                    .transpose()?
                    .flatten();
                document.section_revision = next_revision;
                document.section.insert(ReviewSectionKind::Threads, section);
                Ok(ReviewThreadDelivery {
                    thread,
                    patch,
                    _charge: charge,
                })
            })();
            drop(captured_section);
            drop(_publication);
            drop(_admission);
            let _ = sender.send(result);
        });
        if let Err(failure) = spawned {
            rollback
                .document
                .lock()
                .expect("review document poisoned")
                .thread_loading
                .remove(&rollback_thread);
            return Err(failure);
        }
        receiver
            .await
            .context("thread read ended without a result")?
    }
}

pub(super) fn decode(record: &Value) -> Result<ReviewThread> {
    let connection = record
        .get("comments")
        .context("thread comments are missing")?;
    let nodes = connection
        .get("nodes")
        .and_then(Value::as_array)
        .context("thread comment nodes are missing")?;
    let total_comments = connection
        .get("totalCount")
        .and_then(Value::as_u64)
        .context("thread comment count is missing")?;
    ensure!(
        total_comments > 0
            && total_comments <= 10_000
            && nodes.len() <= 100
            && nodes.len() as u64 <= total_comments,
        "thread comment count exceeds limits"
    );
    let more = connection
        .pointer("/pageInfo/hasNextPage")
        .and_then(Value::as_bool)
        .context("thread comment completion is missing")?;
    let next_cursor = if more {
        Some(string(connection, "/pageInfo/endCursor", 4096)?)
    } else {
        None
    };
    ensure!(
        more || nodes.len() as u64 == total_comments,
        "thread comments are incomplete"
    );
    let comment = nodes
        .iter()
        .map(|record| decode_comment(record).map(Arc::new))
        .collect::<Result<Vec<_>>>()?;
    let mut identity = std::collections::HashSet::new();
    ensure!(
        comment
            .iter()
            .all(|comment| identity.insert(&comment.node_id)),
        "duplicate thread comment"
    );
    let path = string(record, "/path", 4096)?;
    forge_git::identity::RepositoryPath::new(path.as_bytes().to_vec())?;
    let side = match record.get("diffSide").and_then(Value::as_str) {
        Some("LEFT") => crate::comments::CommentSide::Left,
        Some("RIGHT") => crate::comments::CommentSide::Right,
        _ => anyhow::bail!("thread diff side is missing"),
    };
    Ok(ReviewThread {
        node_id: string(record, "/id", 256)?,
        path,
        resolved: boolean(record, "isResolved")?,
        outdated: boolean(record, "isOutdated")?,
        side,
        line: line(record, "line")?,
        start_line: line(record, "startLine")?,
        original_line: line(record, "originalLine")?,
        original_start_line: line(record, "originalStartLine")?,
        can_reply: boolean(record, "viewerCanReply")?,
        can_resolve: boolean(record, "viewerCanResolve")?,
        can_unresolve: boolean(record, "viewerCanUnresolve")?,
        comment,
        next_cursor,
        total_comments: total_comments as usize,
        additional_bytes: 0,
    })
}

fn decode_comment(record: &Value) -> Result<ReviewThreadComment> {
    let database_id = record
        .get("databaseId")
        .and_then(Value::as_u64)
        .context("thread comment numeric identity is missing")?;
    ensure!(
        database_id > 0,
        "thread comment numeric identity is invalid"
    );
    let body = record
        .get("body")
        .and_then(Value::as_str)
        .context("thread comment body is missing")?;
    ensure!(
        body.len() <= 256 * 1024 && !body.contains('\0'),
        "thread comment body exceeds limits"
    );
    let diff_hunk = record.get("diffHunk").and_then(Value::as_str).unwrap_or("");
    ensure!(
        diff_hunk.len() <= 4 * 1024 * 1024 && !diff_hunk.contains('\0'),
        "thread diff context exceeds limits"
    );
    let created_at = optional_timestamp(record, "createdAt")?;
    let updated_at = optional_timestamp(record, "updatedAt")?;
    Ok(ReviewThreadComment {
        node_id: string(record, "/id", 256)?,
        database_id,
        author: record
            .pointer("/author/login")
            .and_then(Value::as_str)
            .map(str::to_owned),
        viewer_did_author: boolean(record, "viewerDidAuthor")?,
        created_at,
        updated_at,
        body: body.into(),
        url: string(record, "/url", 4096)?,
        commit: commit(record, "/commit/oid")?,
        original_commit: commit(record, "/originalCommit/oid")?,
        line: line(record, "line")?,
        original_line: line(record, "originalLine")?,
        start_line: line(record, "startLine")?,
        original_start_line: line(record, "originalStartLine")?,
        diff_hunk: diff_hunk.into(),
        review_node_id: record
            .pointer("/pullRequestReview/id")
            .and_then(Value::as_str)
            .map(str::to_owned),
        review_state: record
            .pointer("/pullRequestReview/state")
            .and_then(Value::as_str)
            .map(str::to_owned),
    })
}

fn optional_timestamp(record: &Value, key: &str) -> Result<Option<String>> {
    let Some(value) = record.get(key) else {
        return Ok(None);
    };
    let value = value
        .as_str()
        .context("thread comment timestamp is invalid")?;
    ensure!(
        value.len() <= 128 && !value.contains('\0'),
        "thread comment timestamp exceeds limits"
    );
    Ok(Some(value.into()))
}

fn string(record: &Value, pointer: &str, maximum: usize) -> Result<String> {
    let value = record
        .pointer(pointer)
        .and_then(Value::as_str)
        .context("thread string field is missing")?;
    ensure!(
        !value.is_empty() && value.len() <= maximum && !value.contains('\0'),
        "invalid thread string field"
    );
    Ok(value.into())
}

fn boolean(record: &Value, key: &str) -> Result<bool> {
    record
        .get(key)
        .and_then(Value::as_bool)
        .context("thread boolean field is missing")
}

fn line(record: &Value, key: &str) -> Result<Option<u64>> {
    match record.get(key) {
        None | Some(Value::Null) => Ok(None),
        Some(value) => {
            let line = value.as_u64().context("thread source line is invalid")?;
            ensure!(
                line > 0 && line <= u32::MAX as u64,
                "thread source line exceeds limits"
            );
            Ok(Some(line))
        }
    }
}

fn commit(record: &Value, pointer: &str) -> Result<Option<String>> {
    match record.pointer(pointer) {
        None | Some(Value::Null) => Ok(None),
        Some(value) => {
            let commit = value.as_str().context("thread commit is invalid")?;
            ensure!(
                (commit.len() == 40 || commit.len() == 64)
                    && commit.bytes().all(|byte| byte.is_ascii_hexdigit()),
                "thread commit requires a full immutable identity"
            );
            Ok(Some(commit.into()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn record() -> Value {
        json!({"id":"THREAD_1","path":"src/lib.rs","isResolved":false,"isOutdated":true,
            "viewerCanReply":true,"viewerCanResolve":false,"viewerCanUnresolve":false,
            "diffSide":"RIGHT","line":null,"originalLine":5,"originalStartLine":3,
            "comments":{"totalCount":2,"pageInfo":{"hasNextPage":true,"endCursor":"next"},
                "nodes":[{"id":"COMMENT_1","databaseId":42,"body":"draft\r\n\n","viewerDidAuthor":true,
                    "url":"https://github.com/owner/repo/pull/7#discussion_r42","author":null,
                    "createdAt":"2026-09-08T10:00:00Z","updatedAt":"2026-09-08T11:00:00Z",
                    "originalCommit":{"oid":"a".repeat(40)},"originalLine":5,"originalStartLine":3,
                    "diffHunk":"@@ -3 +3 @@\n+source","pullRequestReview":{"id":"REVIEW_1","state":"PENDING"}}]}})
    }

    #[test]
    fn outdated_thread_retains_original_source_and_explicit_comment_continuation() {
        let thread = decode(&record()).unwrap();
        assert!(thread.outdated && thread.can_reply && !thread.can_resolve);
        assert_eq!(thread.line, None);
        assert_eq!(thread.original_line, Some(5));
        assert_eq!(thread.original_start_line, Some(3));
        assert_eq!(thread.next_cursor.as_deref(), Some("next"));
        assert_eq!(thread.total_comments, 2);
        assert_eq!(thread.comment[0].body, "draft\r\n\n");
        assert_eq!(thread.comment[0].author, None);
        assert_eq!(
            thread.comment[0].created_at.as_deref(),
            Some("2026-09-08T10:00:00Z")
        );
        assert_eq!(
            thread.comment[0].updated_at.as_deref(),
            Some("2026-09-08T11:00:00Z")
        );
        assert_eq!(
            thread.comment[0].original_commit.as_deref(),
            Some("a".repeat(40).as_str())
        );
        assert_eq!(thread.comment[0].review_state.as_deref(), Some("PENDING"));
    }

    #[test]
    fn thread_rejects_hidden_truncation_and_nonimmutable_source_identity() {
        let mut truncated = record();
        truncated["comments"]["pageInfo"]["hasNextPage"] = json!(false);
        assert!(
            decode(&truncated)
                .unwrap_err()
                .to_string()
                .contains("incomplete")
        );
        let mut invalid = record();
        invalid["comments"]["nodes"][0]["originalCommit"]["oid"] = json!("main");
        assert!(
            decode(&invalid)
                .unwrap_err()
                .to_string()
                .contains("immutable")
        );
    }
}
