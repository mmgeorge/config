use anyhow::{Context, Result, ensure};
use forge_buffer::{
    admission::{DocumentAdmission, DocumentAdmissionStore},
    block::{BufferBlock, TargetRange, TextPosition, TextRange},
    document::BufferDocument,
    identity::{BlockId, DocumentId, InputSequence, TargetId, ViewId},
    input::DocumentInput,
    patch::{BufferPatch, BufferSnapshot},
    sequence::SequenceEdit,
    view::DocumentViews,
    width::WidthProfile,
};
use forge_github::{
    notification::{
        GithubNotificationRemote, NotificationDetail, NotificationDetailRequest,
        NotificationReadRequest, NotificationRecord,
    },
    recovery::{RecoveryRecord, RecoveryResource, RecoveryResourceKind},
    review_mutation::{GithubReviewWriteRemote, ReviewMutation, ReviewMutationRequest},
    service::GithubService,
};
use serde::Serialize;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

pub trait NotificationRemote: GithubNotificationRemote + GithubReviewWriteRemote {}
impl<Remote: GithubNotificationRemote + GithubReviewWriteRemote> NotificationRemote for Remote {}
const MAX_BYTES: usize = 16 * 1024 * 1024;
const MAX_RECORDS: usize = 10_000;

pub struct NotificationDocumentService {
    github: GithubService,
    admission: Arc<DocumentAdmissionStore>,
    document: Mutex<HashMap<DocumentId, Arc<Mutex<NotificationDocument>>>>,
    local: Mutex<HashMap<String, Arc<Mutex<NotificationLocal>>>>,
    closed: AtomicBool,
}
#[derive(Default, Clone)]
struct NotificationLocal {
    saved: HashMap<String, NotificationRecord>,
    unread: HashSet<String>,
    done: VecDeque<NotificationRecord>,
}
struct NotificationDocument {
    admission: Arc<DocumentAdmission>,
    workspace: PathBuf,
    hostname: String,
    remote: Arc<dyn NotificationRemote>,
    local: Arc<Mutex<NotificationLocal>>,
    buffer: BufferDocument,
    views: DocumentViews,
    input: HashMap<ViewId, InputSequence>,
    record: Vec<NotificationRecord>,
    target: HashMap<TargetId, String>,
    identity: HashMap<String, TargetId>,
    detail: HashMap<String, NotificationDetail>,
    diagnostic: HashMap<String, String>,
    expanded: HashSet<String>,
    cursor: Option<u32>,
    busy: bool,
}
#[derive(Serialize)]
pub struct NotificationOpen {
    pub snapshot: BufferSnapshot,
    pub more: bool,
}
#[derive(Serialize)]
pub struct NotificationUpdate {
    pub patch: Option<BufferPatch>,
    pub more: bool,
    pub effect: Option<NotificationEffect>,
    #[serde(serialize_with = "serialize_recovery")]
    pub recovery: Option<RecoveryRecord>,
    pub diagnostic: Option<String>,
}
#[derive(Serialize)]
pub struct NotificationEffect {
    #[serde(flatten)]
    pub input: DocumentInput,
    pub id: String,
    pub kind: String,
    pub repository: forge_github::model::GithubRepositoryId,
    pub subject_kind: String,
    pub number: Option<u64>,
    pub url: Option<String>,
}
impl NotificationDocumentService {
    pub fn new(github: GithubService) -> Self {
        Self {
            github,
            admission: Arc::default(),
            document: Mutex::default(),
            local: Mutex::default(),
            closed: AtomicBool::new(false),
        }
    }
    pub async fn open(
        &self,
        id: DocumentId,
        workspace: PathBuf,
        hostname: String,
        remote: Arc<dyn NotificationRemote>,
    ) -> Result<NotificationOpen> {
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "notifications service is closed"
        );
        let admission = Arc::new(self.admission.admit(id.clone())?);
        let mut request = NotificationReadRequest {
            hostname,
            cursor: None,
        };
        request.validate()?;
        let local = {
            let mut local = self.local.lock().expect("notification local store");
            ensure!(
                local.contains_key(&request.hostname) || local.len() < 8,
                "notification host admission exceeded"
            );
            Arc::clone(local.entry(request.hostname.clone()).or_default())
        };
        let page = self
            .github
            .notification_page(workspace.clone(), remote.clone(), request.clone())
            .await?;
        admission.check()?;
        let mut document = NotificationDocument {
            admission,
            workspace,
            hostname: request.hostname,
            remote,
            local,
            buffer: BufferDocument::new(id.clone(), Vec::new())?,
            views: DocumentViews::default(),
            input: HashMap::new(),
            record: page.record,
            target: HashMap::new(),
            identity: HashMap::new(),
            detail: HashMap::new(),
            diagnostic: HashMap::new(),
            expanded: HashSet::new(),
            cursor: page.next_cursor,
            busy: false,
        };
        reproject(&mut document)?;
        let result = NotificationOpen {
            snapshot: document.buffer.snapshot(),
            more: document.cursor.is_some(),
        };
        let mut registry = self.document.lock().expect("notification registry");
        document.admission.check()?;
        registry.insert(id, Arc::new(Mutex::new(document)));
        Ok(result)
    }
    pub fn snapshot(&self, id: &DocumentId) -> Result<BufferSnapshot> {
        let owner = self.get(id)?;
        let document = owner.lock().expect("notification document");
        document.admission.check()?;
        Ok(document.buffer.snapshot())
    }
    pub fn view(
        &self,
        id: &DocumentId,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<Option<BufferPatch>> {
        let owner = self.get(id)?;
        let mut document = owner.lock().expect("notification document");
        document.admission.check()?;
        ensure!(!document.busy, "notification request is in progress");
        let changed = document.views.open(view.clone(), width)?;
        document.input.entry(view).or_insert(InputSequence(0));
        if changed {
            reproject(&mut document)
        } else {
            Ok(None)
        }
    }
    pub fn close_view(&self, id: &DocumentId, view: &ViewId) -> Result<Option<BufferPatch>> {
        let owner = self.get(id)?;
        let mut document = owner.lock().expect("notification document");
        document.input.remove(view);
        let changed = document.views.close(view);
        if changed && !document.busy {
            reproject(&mut document)
        } else {
            Ok(None)
        }
    }
    pub async fn close_collected(&self, id: &DocumentId) -> bool {
        let existed = self.admission.cancel(id);
        self.document
            .lock()
            .expect("notification registry")
            .remove(id);
        self.admission.wait_closed(id).await;
        existed
    }
    pub fn close_all(&self) {
        self.closed.store(true, Ordering::Release);
        self.admission.cancel_all();
        self.document.lock().expect("notification registry").clear();
        self.local.lock().expect("notification local store").clear();
    }
    pub async fn act(&self, input: DocumentInput) -> Result<NotificationUpdate> {
        let owner = self.get(&input.document)?;
        let (record, remote, workspace, hostname, cursor, admission) = {
            let mut document = owner.lock().expect("notification document");
            validate_input(&document, &input)?;
            ensure!(!document.busy, "notification request is in progress");
            ensure!(
                matches!(
                    input.action.as_str(),
                    "open"
                        | "browse"
                        | "save"
                        | "unread"
                        | "done"
                        | "expand"
                        | "count"
                        | "more"
                        | "refresh"
                ),
                "unknown notification action"
            );
            let record = if matches!(input.action.as_str(), "more" | "refresh") {
                None
            } else {
                let id = document
                    .target
                    .get(
                        input
                            .target
                            .as_ref()
                            .context("missing notification target")?,
                    )
                    .context("unknown notification target")?;
                Some(find_record(&document, id).context("notification is no longer present")?)
            };
            document.input.insert(input.view.clone(), input.sequence);
            document.busy = true;
            (
                record,
                document.remote.clone(),
                document.workspace.clone(),
                document.hostname.clone(),
                document.cursor,
                document.admission.clone(),
            )
        };
        let _busy = NotificationBusy(Arc::clone(&owner));
        let result = self
            .perform(&owner, &input, record, remote, workspace, hostname, cursor)
            .await;
        drop(admission);
        result
    }
    async fn perform(
        &self,
        owner: &Arc<Mutex<NotificationDocument>>,
        input: &DocumentInput,
        record: Option<NotificationRecord>,
        remote: Arc<dyn NotificationRemote>,
        workspace: PathBuf,
        hostname: String,
        cursor: Option<u32>,
    ) -> Result<NotificationUpdate> {
        let mut recovery = None;
        let mut diagnostic = None;
        if matches!(input.action.as_str(), "refresh" | "more") {
            ensure!(
                input.action == "refresh" || cursor.is_some(),
                "notification pages are complete"
            );
            let page = self
                .github
                .notification_page(
                    workspace,
                    remote,
                    NotificationReadRequest {
                        hostname,
                        cursor: if input.action == "refresh" {
                            None
                        } else {
                            cursor
                        },
                    },
                )
                .await?;
            let mut document = owner.lock().expect("notification document");
            document.admission.check()?;
            let mut records = if input.action == "refresh" {
                Vec::new()
            } else {
                document.record.clone()
            };
            let mut identity: HashSet<_> = records.iter().map(|record| record.id.clone()).collect();
            for record in page.record {
                if identity.insert(record.id.clone()) {
                    records.push(record);
                }
            }
            ensure!(
                records.len() <= MAX_RECORDS,
                "notification record limit exceeded"
            );
            ensure!(
                serde_json::to_vec(&records)?.len()
                    + if input.action == "refresh" {
                        0
                    } else {
                        serde_json::to_vec(&document.detail)?.len()
                    }
                    <= MAX_BYTES / 2,
                "notification source admission exceeded"
            );
            document.record = records;
            if input.action == "refresh" {
                document.diagnostic.clear();
                document.detail.clear();
            }
            document.cursor = page.next_cursor;
        } else {
            let record = record.context("notification action lacks captured record")?;
            if matches!(input.action.as_str(), "open" | "browse") {
                return Ok(NotificationUpdate {
                    patch: None,
                    more: cursor.is_some(),
                    recovery: None,
                    diagnostic: None,
                    effect: Some(NotificationEffect {
                        input: input.clone(),
                        id: uuid::Uuid::new_v4().to_string(),
                        kind: input.action.clone(),
                        repository: record.repository,
                        subject_kind: record.kind,
                        number: record.number,
                        url: record.browser_url,
                    }),
                });
            }
            if matches!(input.action.as_str(), "save" | "done" | "unread") {
                let local = owner.lock().expect("notification document").local.clone();
                {
                    let mut local = local.lock().expect("notification local store");
                    let mut candidate = local.clone();
                    candidate.saved.remove(&record.id);
                    candidate.unread.remove(&record.id);
                    candidate.done.retain(|previous| previous.id != record.id);
                    match input.action.as_str() {
                        "save" => {
                            candidate.saved.insert(record.id.clone(), record.clone());
                        }
                        "unread" => {
                            candidate.unread.insert(record.id.clone());
                        }
                        "done" => {
                            candidate.done.push_front(record.clone());
                            candidate.done.truncate(10);
                        }
                        _ => unreachable!(),
                    }
                    ensure!(
                        candidate.unread.len() <= MAX_RECORDS
                            && candidate.saved.len() <= MAX_RECORDS,
                        "notification local record limit exceeded"
                    );
                    let bytes = serde_json::to_vec(&candidate.saved)?.len()
                        + serde_json::to_vec(&candidate.done)?.len()
                        + serde_json::to_vec(&candidate.unread)?.len();
                    ensure!(
                        bytes <= 1024 * 1024,
                        "notification local choices exceed 1 MiB"
                    );
                    *local = candidate;
                }
                if input.action != "unread" {
                    let remote_result = async {
                        let actor = self
                            .github
                            .notification_actor(
                                workspace,
                                remote.clone(),
                                record.repository.clone(),
                            )
                            .await?;
                        owner
                            .lock()
                            .expect("notification document")
                            .admission
                            .check()?;
                        self.github
                            .review_mutation(
                                remote,
                                ReviewMutationRequest {
                                    parent_node_id: None,
                                    resource: RecoveryResource {
                                        repository: record.repository.clone(),
                                        kind: RecoveryResourceKind::Notification,
                                        number: record.id.parse()?,
                                    },
                                    operation_id: uuid::Uuid::new_v4().to_string(),
                                    actor_node_id: actor.node_id,
                                    edit_sequence: None,
                                    draft_target: None,
                                    mutation: if input.action == "save" {
                                        ReviewMutation::NotificationRead
                                    } else {
                                        ReviewMutation::NotificationDone
                                    },
                                },
                            )
                            .await
                    }
                    .await;
                    match remote_result {
                        Ok(record) => recovery = Some(record),
                        Err(failure) => {
                            diagnostic = Some(format!("{failure:#}").chars().take(8192).collect())
                        }
                    }
                }
            } else {
                let latest_comment = input.action == "expand";
                let fetch = {
                    let mut document = owner.lock().expect("notification document");
                    if latest_comment && !document.expanded.insert(record.id.clone()) {
                        document.expanded.remove(&record.id);
                        false
                    } else {
                        !document.diagnostic.contains_key(&record.id)
                            && !document.detail.get(&record.id).is_some_and(|detail| {
                                !latest_comment
                                    || detail.comment
                                    || record.comment_endpoint.is_none()
                            })
                    }
                };
                if fetch {
                    let result = self
                        .github
                        .notification_detail(
                            workspace,
                            remote,
                            NotificationDetailRequest {
                                record: record.clone(),
                                latest_comment,
                            },
                        )
                        .await;
                    let mut document = owner.lock().expect("notification document");
                    document.admission.check()?;
                    match result {
                        Ok(mut detail) => {
                            if detail.comments.is_none() {
                                detail.comments = document
                                    .detail
                                    .get(&record.id)
                                    .and_then(|previous| previous.comments);
                            }
                            let previous_bytes = document
                                .detail
                                .get(&record.id)
                                .map(serde_json::to_vec)
                                .transpose()?
                                .map_or(0, |bytes| bytes.len());
                            let retained = serde_json::to_vec(&document.record)?.len()
                                + serde_json::to_vec(&document.detail)?.len()
                                - previous_bytes
                                + serde_json::to_vec(&detail)?.len();
                            ensure!(
                                retained <= MAX_BYTES / 2,
                                "notification detail admission exceeded"
                            );
                            document.diagnostic.remove(&record.id);
                            document.detail.insert(record.id, detail);
                        }
                        Err(failure) => {
                            document.diagnostic.insert(
                                record.id,
                                format!("{failure:#}").chars().take(8192).collect(),
                            );
                        }
                    }
                }
            }
        }
        let mut document = owner.lock().expect("notification document");
        let patch = if document.admission.check().is_ok() {
            reproject(&mut document)?
        } else {
            None
        };
        Ok(NotificationUpdate {
            patch,
            more: document.cursor.is_some(),
            effect: None,
            recovery,
            diagnostic,
        })
    }
    fn get(&self, id: &DocumentId) -> Result<Arc<Mutex<NotificationDocument>>> {
        self.document
            .lock()
            .expect("notification registry")
            .get(id)
            .cloned()
            .context("unknown notification document")
    }
}
struct NotificationBusy(Arc<Mutex<NotificationDocument>>);
impl Drop for NotificationBusy {
    fn drop(&mut self) {
        self.0.lock().expect("notification document").busy = false;
    }
}
fn find_record(document: &NotificationDocument, id: &str) -> Option<NotificationRecord> {
    document
        .record
        .iter()
        .find(|record| record.id == id)
        .cloned()
        .or_else(|| {
            let local = document.local.lock().expect("notification local store");
            local
                .saved
                .get(id)
                .cloned()
                .or_else(|| local.done.iter().find(|record| record.id == id).cloned())
        })
}
fn validate_input(document: &NotificationDocument, input: &DocumentInput) -> Result<()> {
    input.validate()?;
    document.admission.check()?;
    ensure!(
        document.buffer.revision() == input.revision,
        "notification input uses stale revision"
    );
    ensure!(
        document
            .input
            .get(&input.view)
            .is_some_and(|previous| *previous < input.sequence),
        "notification input view or sequence is stale"
    );
    let block = document
        .buffer
        .block(&input.block)
        .context("notification input block disappeared")?;
    let row = block
        .text
        .row(input.position.row)
        .context("notification input row disappeared")?;
    ensure!(
        row.is_char_boundary(input.position.column),
        "notification input byte position is invalid"
    );
    let target = input
        .target
        .as_ref()
        .context("notification target is missing")?;
    ensure!(
        block.metadata.target.iter().any(|range| &range.id == target
            && range.range.start <= input.position
            && input.position < range.range.end),
        "notification target does not match captured position"
    );
    Ok(())
}
fn reproject(document: &mut NotificationDocument) -> Result<Option<BufferPatch>> {
    let mut block = Vec::new();
    let mut target = HashMap::new();
    block.push(plain(
        "notifications:hint",
        [
            "Hint: <tab> expand | <cr> open | b browse | S save | U unread | D done | r refresh | q close",
            "",
        ],
    )?);
    let local = document.local.lock().expect("notification local store");
    let mut records: Vec<_> = document.record.clone();
    let mut seen: HashSet<_> = records.iter().map(|record| record.id.clone()).collect();
    for record in local.saved.values().chain(local.done.iter()) {
        if seen.insert(record.id.clone()) {
            records.push(record.clone());
        }
    }
    ensure!(
        records.len() <= MAX_RECORDS,
        "notification record admission exceeded"
    );
    let mut retained =
        serde_json::to_vec(&records)?.len() + serde_json::to_vec(&document.detail)?.len();
    ensure!(
        retained <= MAX_BYTES / 2,
        "notification sources exceed 8 MiB"
    );
    for category in ["Unread", "Saved", "Done"] {
        let category_id = format!("notifications:{category}");
        block.push(plain(&category_id, [format!("{category}:")])?);
        let ordered: Vec<_> = if category == "Done" {
            local.done.iter().collect()
        } else {
            records.iter().collect()
        };
        let mut category_count = 0;
        for record in ordered {
            let saved = local.saved.contains_key(&record.id);
            let done = local.done.iter().any(|previous| previous.id == record.id);
            let group = if saved {
                "Saved"
            } else if done {
                "Done"
            } else if record.unread || local.unread.contains(&record.id) {
                "Unread"
            } else {
                continue;
            };
            if group != category {
                continue;
            }
            category_count += 1;
            let id = document
                .identity
                .entry(record.id.clone())
                .or_insert_with(|| TargetId(uuid::Uuid::new_v4().to_string()))
                .clone();
            target.insert(id.clone(), record.id.clone());
            let count = document
                .detail
                .get(&record.id)
                .and_then(|detail| detail.comments)
                .map(|count| count.to_string())
                .unwrap_or_else(|| "...".into());
            let number = record
                .number
                .map(|number| format!("#{number}"))
                .unwrap_or_else(|| record.kind.clone());
            let text = format!("{number} {} ({count})", record.title,);
            let mut header = BufferBlock {
                id: BlockId(format!("notification:{}", record.id)),
                text: forge_buffer::text::BufferText::from_rows([text])?,
                metadata: Default::default(),
            };
            header.metadata.target.clear();
            header.metadata.target.push(TargetRange {
                id: id.clone(),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: header.text.row_count(),
                        column: 0,
                    },
                },
            });
            block.push(header);
            if document.expanded.contains(&record.id) {
                let detail_rows = if let Some(error) = document.diagnostic.get(&record.id) {
                    prefixed_detail_rows(error)
                } else if let Some(detail) = document.detail.get(&record.id) {
                    let mut rows = Vec::new();
                    if detail.comment {
                        rows.push(format!(
                            "  Last comment by {}",
                            detail.author.as_deref().unwrap_or("unknown")
                        ));
                    }
                    rows.extend(prefixed_detail_rows(if detail.body.is_empty() {
                        "_No description._"
                    } else {
                        &detail.body
                    }));
                    rows
                } else {
                    vec!["  ...fetching...".into()]
                };
                let mut body = plain(&format!("notification:{}:detail", record.id), detail_rows)?;
                body.metadata.target.clear();
                body.metadata.target.push(TargetRange {
                    id,
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: body.text.row_count(),
                            column: 0,
                        },
                    },
                });
                let header_index = block.len() - 1;
                let header_row = block[header_index].text.row_count() - 1;
                block[header_index]
                    .metadata
                    .fold
                    .push(forge_buffer::block::FoldRange {
                        id: forge_buffer::identity::FoldId(format!(
                            "notification:{}:detail",
                            record.id
                        )),
                        start: TextPosition {
                            row: header_row,
                            column: 0,
                        },
                        end: forge_buffer::block::BlockAnchor {
                            block: body.id.clone(),
                            position: TextPosition {
                                row: body.text.row_count(),
                                column: 0,
                            },
                        },
                        closed: false,
                    });
                block.push(body);
            }
        }
        if category_count == 0 {
            block.push(plain(&format!("{category_id}:none"), ["none"])?);
        }
        if category != "Done" {
            block.push(plain(&format!("{category_id}:separator"), [""])?);
        }
    }
    drop(local);
    if document.cursor.is_some() {
        let mut footer = plain("notifications:page", ["Load more notifications"])?;
        footer.metadata.target.push(TargetRange {
            id: TargetId("notifications:page".into()),
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: footer.text.row_count(),
                    column: 0,
                },
            },
        });
        block.push(footer);
    }
    let projection_bytes = serde_json::to_vec(&block)?.len();
    ensure!(
        projection_bytes <= MAX_BYTES / 2,
        "notification projection exceeds 8 MiB"
    );
    retained += projection_bytes;
    document.identity.retain(|id, _| seen.contains(id));
    document.detail.retain(|id, _| seen.contains(id));
    document.diagnostic.retain(|id, _| seen.contains(id));
    document.expanded.retain(|id| seen.contains(id));
    ensure!(
        retained <= MAX_BYTES,
        "notification document exceeds 16 MiB"
    );
    let old: Vec<_> = document
        .buffer
        .blocks(document.buffer.revision(), 0..document.buffer.block_count())?
        .collect();
    let prefix = old
        .iter()
        .zip(&block)
        .take_while(|(left, right)| **left == *right)
        .count();
    let suffix = old[prefix..]
        .iter()
        .rev()
        .zip(block[prefix..].iter().rev())
        .take_while(|(left, right)| **left == *right)
        .count();
    let range = prefix..old.len() - suffix;
    let replacement = block[prefix..block.len() - suffix].to_vec();
    let patch = document.buffer.edit_many(vec![SequenceEdit {
        range,
        block: replacement,
    }])?;
    document.target = target;
    Ok(patch)
}
fn plain(id: &str, rows: impl IntoIterator<Item = impl AsRef<str>>) -> Result<BufferBlock> {
    Ok(BufferBlock {
        id: BlockId(id.into()),
        text: forge_buffer::text::BufferText::from_rows(rows)?,
        metadata: Default::default(),
    })
}

fn prefixed_detail_rows(detail: &str) -> Vec<String> {
    detail.split('\n').map(|row| format!("  {row}")).collect()
}

fn serialize_recovery<Serializer: serde::Serializer>(
    record: &Option<RecoveryRecord>,
    serializer: Serializer,
) -> std::result::Result<Serializer::Ok, Serializer::Error> {
    match record {
        None => serializer.serialize_none(),
        Some(record) => {
            let mut wire = serde_json::to_value(record).map_err(serde::ser::Error::custom)?;
            wire["resource"]["number"] =
                serde_json::Value::String(record.resource.number.to_string());
            wire.serialize(serializer)
        }
    }
}
#[cfg(test)]
mod tests;
