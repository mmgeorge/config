use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock, EditableRegion, TextPosition, TextRange},
    document::{BufferDocument, LocalEditPreparation, PreparedLocalEdit},
    editable::{EditAcknowledgement, LocalEdit, LocalEditResult},
    identity::{BlockId, DocumentId, RegionId, RegionRevision},
    patch::{BufferPatch, BufferSnapshot},
    text::BufferText,
    width::WidthProfile,
};
use forge_github::{
    model::{GithubRepositoryId, IssueDetail},
    review_mutation::ReviewMutation,
};
use serde::{Deserialize, Serialize};

use crate::edit::{EditBudget, EditLimits, EditStore, RegionEdit, SaveOutcome, SaveSubmission};

mod refresh;
mod view;
pub use view::IssueEffect;

const ISSUE_FIELD: [&str; 3] = ["title", "assignees", "body"];

/// Accepted issue fields and generated metadata share one document revision owner.
/// Captured saves retain their own text while subsequent edits remain independently dirty.
pub struct IssueDocument {
    pub repository: GithubRepositoryId,
    pub number: u64,
    pub node_id: String,
    buffer: BufferDocument,
    edits: EditStore,
    pending: Vec<SaveSubmission>,
    pending_operation: Option<String>,
    budget: EditBudget,
    fresh_required: bool,
    detail: IssueDetail,
    target: std::collections::HashMap<forge_buffer::identity::TargetId, String>,
    width: WidthProfile,
    source_generation: u64,
    views: forge_buffer::view::DocumentViews,
    input: std::collections::HashMap<
        forge_buffer::identity::ViewId,
        forge_buffer::identity::InputSequence,
    >,
}

#[derive(Serialize)]
pub struct IssueEditResult {
    #[serde(flatten)]
    pub acknowledgement: EditAcknowledgement,
    pub patch: BufferPatch,
}

#[derive(Serialize)]
pub struct IssueFieldState {
    pub region: RegionId,
    pub revision: RegionRevision,
    pub sequence: forge_buffer::identity::EditSequence,
    pub dirty: bool,
    pub uncertain: bool,
    pub conflict: bool,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueDraft {
    version: u32,
    #[serde(default)]
    fresh_required: bool,
    node_id: String,
    operation_id: Option<String>,
    field: std::collections::BTreeMap<String, IssueDraftField>,
}
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueDraftField {
    text: String,
    baseline: String,
    revision: u64,
    sequence: u64,
    remote: Option<String>,
    uncertain: bool,
    pending: Option<IssueDraftSave>,
}
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueDraftSave {
    revision: u64,
    sequence: u64,
    text: String,
}
pub struct PreparedIssueEdit<'document> {
    field: crate::edit::PreparedRegionEdit<'document>,
    buffer: Box<PreparedLocalEdit<'document>>,
    payload: serde_json::Value,
}
impl PreparedIssueEdit<'_> {
    pub fn take_draft(&mut self) -> serde_json::Value {
        std::mem::take(&mut self.payload)
    }
    pub fn commit(self) -> IssueEditResult {
        let acknowledgement = self.field.commit();
        let LocalEditResult::Accepted { patch, .. } = self.buffer.commit() else {
            unreachable!("prepared issue buffer edit")
        };
        IssueEditResult {
            acknowledgement,
            patch: *patch,
        }
    }
}

impl IssueDocument {
    pub fn new(
        id: DocumentId,
        repository: GithubRepositoryId,
        node_id: String,
        detail: IssueDetail,
        budget: EditBudget,
    ) -> Result<Self> {
        validate_detail(&repository, &node_id, &detail)?;
        let mut edits = EditStore::new(
            id.clone(),
            EditLimits {
                active_fields: 3,
                region_lifetimes: 3,
                pending_saves: 3,
                field_bytes: 4 * 1024 * 1024,
            },
            budget.clone(),
        )?;
        edits.insert(
            RegionId("title".into()),
            RegionRevision(0),
            detail.title.clone(),
        )?;
        edits.insert(
            RegionId("assignees".into()),
            RegionRevision(0),
            detail
                .assignees
                .iter()
                .map(|login| format!("@{login}"))
                .collect::<Vec<_>>()
                .join(" "),
        )?;
        edits.insert(
            RegionId("body".into()),
            RegionRevision(0),
            detail.body.clone(),
        )?;
        let width = WidthProfile::default();
        let fields = ISSUE_FIELD
            .into_iter()
            .map(|name| field_block(&edits, name))
            .collect::<Result<Vec<_>>>()?;
        let projection = crate::issue_presentation::project(&detail, fields, &width)?;
        Ok(Self {
            repository,
            number: detail.number,
            node_id,
            buffer: BufferDocument::new(id, projection.block)?,
            edits,
            pending: Vec::new(),
            pending_operation: None,
            budget,
            fresh_required: false,
            detail,
            target: projection.target,
            width,
            source_generation: 0,
            views: Default::default(),
            input: Default::default(),
        })
    }

    fn draft_state(&self) -> Result<IssueDraft> {
        let mut field = std::collections::BTreeMap::new();
        for name in ISSUE_FIELD {
            let region = RegionId(name.into());
            let snapshot = self.edits.snapshot(&region)?;
            field.insert(
                name.to_owned(),
                IssueDraftField {
                    text: snapshot.text.to_owned(),
                    baseline: snapshot.baseline.to_owned(),
                    revision: snapshot.revision.0,
                    sequence: snapshot.sequence.0,
                    remote: snapshot.remote.map(str::to_owned),
                    uncertain: snapshot.uncertain,
                    pending: self
                        .pending
                        .iter()
                        .find(|pending| pending.region() == &region)
                        .map(|pending| IssueDraftSave {
                            revision: pending.revision().0,
                            sequence: pending.sequence().0,
                            text: pending.text().to_owned(),
                        }),
                },
            );
        }
        Ok(IssueDraft {
            version: 2,
            fresh_required: self.fresh_required,
            node_id: self.node_id.clone(),
            operation_id: self.pending_operation.clone(),
            field,
        })
    }
    pub fn draft_payload(&self) -> Result<serde_json::Value> {
        Ok(
            serde_json::json!({"repo":self.repository.repository_name(), "number":self.number, "issue_document":self.draft_state()?}),
        )
    }
    pub fn fresh_required(&self) -> bool {
        self.fresh_required
    }
    pub fn resolution_payload(
        &self,
        outcome: SaveOutcome,
        fresh_required: bool,
    ) -> Result<serde_json::Value> {
        let mut draft = self.settlement_payload(outcome)?;
        draft["issue_document"]["fresh_required"] = serde_json::Value::Bool(fresh_required);
        Ok(draft)
    }
    pub fn resolve_pending(&mut self, outcome: SaveOutcome, fresh_required: bool) -> Result<()> {
        let mut pending_is_uncertain = true;
        for pending in &self.pending {
            pending_is_uncertain &= self.edits.snapshot(pending.region())?.uncertain;
        }
        if !pending_is_uncertain {
            self.complete_save(SaveOutcome::Uncertain)?;
        }
        self.recover_save(outcome)?;
        self.fresh_required = fresh_required;
        Ok(())
    }
    pub fn pending_operation(&self) -> Option<&str> {
        self.pending_operation.as_deref()
    }
    pub fn pending_sequence(&self) -> u64 {
        self.pending
            .iter()
            .map(|pending| pending.sequence().0)
            .max()
            .unwrap_or(0)
    }
    pub fn settlement_payload(&self, outcome: SaveOutcome) -> Result<serde_json::Value> {
        let mut draft = self.draft_state()?;
        for pending in &self.pending {
            let field = draft
                .field
                .get_mut(&pending.region().0)
                .context("pending issue field missing")?;
            if outcome == SaveOutcome::Confirmed {
                field.baseline = pending.text().to_owned();
            }
            field.uncertain = outcome == SaveOutcome::Uncertain;
            if outcome != SaveOutcome::Uncertain {
                field.pending = None;
            }
        }
        if outcome != SaveOutcome::Uncertain {
            draft.operation_id = None;
        }
        Ok(
            serde_json::json!({"repo":self.repository.repository_name(), "number":self.number, "issue_document":draft}),
        )
    }
    pub fn restore_draft(&mut self, payload: &serde_json::Value) -> Result<()> {
        let Some(raw) = payload.get("issue_document") else {
            return Ok(());
        };
        ensure!(
            payload.get("repo").and_then(serde_json::Value::as_str)
                == Some(self.repository.repository_name().as_str())
                && payload.get("number").and_then(serde_json::Value::as_u64) == Some(self.number),
            "issue draft belongs to another resource"
        );
        let draft: IssueDraft = serde_json::from_value(raw.clone())?;
        ensure!(
            draft.version == 2 && draft.node_id == self.node_id && draft.field.len() == 3,
            "issue draft identity or schema changed"
        );
        ensure!(
            draft
                .operation_id
                .as_ref()
                .is_none_or(|identity| !identity.is_empty()
                    && identity.len() <= 256
                    && identity.bytes().all(|byte| byte.is_ascii_graphic())),
            "invalid draft operation identity"
        );
        ensure!(
            draft.operation_id.is_some()
                == draft.field.values().any(|field| field.pending.is_some()),
            "issue draft pending operation is inconsistent"
        );
        ensure!(
            draft
                .field
                .values()
                .all(|field| !field.uncertain || field.pending.is_some()),
            "issue draft uncertainty lacks a pending capture"
        );
        let id = self.edits.document_id().clone();
        let mut edits = EditStore::new(
            id.clone(),
            EditLimits {
                active_fields: 3,
                region_lifetimes: 3,
                pending_saves: 3,
                field_bytes: 4 * 1024 * 1024,
            },
            self.budget.clone(),
        )?;
        let mut pending = Vec::new();
        for name in ISSUE_FIELD {
            let field = draft.field.get(name).context("issue draft field missing")?;
            ensure!(
                field.sequence <= forge_buffer::MAX_COUNTER
                    && (name == "body"
                        || (!field.text.contains(['\n', '\r']) && field.text.len() <= 16 * 1024)),
                "invalid issue draft field"
            );
            let region = RegionId(name.into());
            let observed = self.edits.snapshot(&region)?.text.to_owned();
            ensure!(
                field.pending.is_none() || draft.operation_id.is_some(),
                "pending issue draft lacks operation identity"
            );
            let restored = edits.restore_field(
                region.clone(),
                crate::edit::FieldRestoration {
                    revision: RegionRevision(field.revision),
                    sequence: forge_buffer::identity::EditSequence(field.sequence),
                    text: field.text.clone(),
                    baseline: field.baseline.clone(),
                    remote: field.remote.clone(),
                    pending: field
                        .pending
                        .as_ref()
                        .map(|submitted| crate::edit::SaveRestoration {
                            revision: RegionRevision(submitted.revision),
                            sequence: forge_buffer::identity::EditSequence(submitted.sequence),
                            text: submitted.text.clone(),
                            uncertain: true,
                        }),
                },
            )?;
            if let Some(submission) = restored {
                pending.push(submission);
            }
            if field.pending.is_none() {
                edits.merge(&region, observed)?;
            }
        }
        let mut replacement = Vec::new();
        for name in ISSUE_FIELD {
            let index = self
                .buffer
                .block_index(&BlockId(format!("region:{name}")))
                .context("issue field block missing")?;
            replacement.push(forge_buffer::sequence::SequenceEdit {
                range: index..index + 1,
                block: vec![field_block(&edits, name)?],
            });
        }
        self.buffer.edit_many(replacement)?;
        self.edits = edits;
        self.pending = pending;
        self.pending_operation = draft.operation_id;
        self.fresh_required = draft.fresh_required;
        Ok(())
    }
    pub fn recover_save(&mut self, outcome: SaveOutcome) -> Result<()> {
        ensure!(
            outcome != SaveOutcome::Uncertain,
            "uncertain issue recovery requires explicit resolution"
        );
        for pending in &self.pending {
            if outcome == SaveOutcome::Confirmed {
                self.edits
                    .reconcile_save(pending, pending.text().to_owned())?;
            } else {
                self.edits.close_unknown(pending)?;
            }
        }
        self.pending.clear();
        self.pending_operation = None;
        Ok(())
    }

    pub fn snapshot(&self) -> BufferSnapshot {
        self.buffer.snapshot()
    }

    pub fn fields(&self) -> Result<Vec<IssueFieldState>> {
        ISSUE_FIELD
            .iter()
            .map(|name| {
                let region = RegionId((*name).into());
                let field = self.edits.snapshot(&region)?;
                Ok(IssueFieldState {
                    region,
                    revision: field.revision,
                    sequence: field.sequence,
                    dirty: field.dirty,
                    uncertain: field.uncertain,
                    conflict: field.remote.is_some(),
                })
            })
            .collect()
    }

    /// Validates both replicas before committing one field edit and its incremental patch.
    pub fn edit(&mut self, edit: RegionEdit) -> Result<IssueEditResult> {
        Ok(self.prepare_edit(edit)?.commit())
    }

    pub fn prepare_edit(&mut self, edit: RegionEdit) -> Result<PreparedIssueEdit<'_>> {
        let mut draft = self.draft_state()?;
        ensure!(
            ISSUE_FIELD.contains(&edit.region.0.as_str()),
            "unknown issue field"
        );
        if edit.region.0 != "body" {
            ensure!(
                !edit.text.contains(['\n', '\r']) && edit.text.len() <= 16 * 1024,
                "issue scalar field exceeds its single-row bound"
            );
        }
        let prepared = match self.buffer.prepare_local_edit(LocalEdit {
            document: edit.document.clone(),
            region: edit.region.clone(),
            base: edit.base,
            sequence: edit.sequence,
            text: BufferText::from_rows(edit.text.split('\n'))?,
        })? {
            LocalEditPreparation::Prepared(prepared) => prepared,
            LocalEditPreparation::Rejected(failure) => {
                anyhow::bail!("issue document rejected local edit: {failure:?}")
            }
        };
        let region = edit.region.clone();
        let field = self.edits.prepare_accept(edit)?;
        let snapshot = field.snapshot()?;
        let updated = draft
            .field
            .get_mut(&region.0)
            .context("prepared issue field missing")?;
        updated.text = snapshot.text.to_owned();
        updated.baseline = snapshot.baseline.to_owned();
        updated.revision = snapshot.revision.0;
        updated.sequence = snapshot.sequence.0;
        updated.remote = snapshot.remote.map(str::to_owned);
        updated.uncertain = snapshot.uncertain;
        let payload = serde_json::json!({"repo":self.repository.repository_name(), "number":self.number, "issue_document":draft});
        ensure!(
            serde_json::to_vec(&payload)?.len() <= 8 * 1024 * 1024,
            "issue draft exceeds 8 MiB"
        );
        Ok(PreparedIssueEdit {
            field,
            buffer: prepared,
            payload,
        })
    }

    /// Captures dirty fields and an assignee delta without replacing unrelated remote assignments.
    pub fn begin_save(&mut self) -> Result<Option<ReviewMutation>> {
        ensure!(
            !self.fresh_required,
            "issue requires a fresh observation before saving"
        );
        ensure!(
            self.pending.is_empty(),
            "issue save is pending or requires reconciliation"
        );
        let title = self.edits.snapshot(&RegionId("title".into()))?;
        let body = self.edits.snapshot(&RegionId("body".into()))?;
        let assignees = self.edits.snapshot(&RegionId("assignees".into()))?;
        if !title.dirty && !body.dirty && !assignees.dirty {
            return Ok(None);
        }
        ensure!(
            !title.text.trim().is_empty() && title.text.len() <= 1024,
            "invalid issue title"
        );
        let current = assignee_logins(assignees.text)?;
        let baseline = assignee_logins(assignees.baseline)?;
        let add_assignees = current
            .iter()
            .filter(|name| !baseline.iter().any(|old| old.eq_ignore_ascii_case(name)))
            .cloned()
            .collect();
        let remove_assignees = baseline
            .iter()
            .filter(|name| !current.iter().any(|new| new.eq_ignore_ascii_case(name)))
            .cloned()
            .collect();
        let mutation = ReviewMutation::IssueEdit {
            title: title.dirty.then(|| title.text.to_owned()),
            body: body.dirty.then(|| body.text.to_owned()),
            add_assignees,
            remove_assignees,
        };
        if let ReviewMutation::IssueEdit {
            title: None,
            body: None,
            add_assignees,
            remove_assignees,
        } = &mutation
        {
            if add_assignees.is_empty() && remove_assignees.is_empty() {
                if let Some(submission) = self.edits.begin_save(&RegionId("assignees".into()))? {
                    self.edits
                        .complete_save(&submission, SaveOutcome::Confirmed)?;
                }
                return Ok(None);
            }
        }
        for field in ISSUE_FIELD {
            match self.edits.begin_save(&RegionId(field.into())) {
                Ok(Some(submission)) => self.pending.push(submission),
                Ok(None) => {}
                Err(failure) => {
                    for submission in self.pending.drain(..) {
                        self.edits
                            .complete_save(&submission, SaveOutcome::Rejected)?;
                    }
                    return Err(failure.into());
                }
            }
        }
        self.pending_operation = Some(uuid::Uuid::new_v4().to_string());
        Ok(Some(mutation))
    }

    pub fn complete_save(&mut self, outcome: SaveOutcome) -> Result<()> {
        ensure!(!self.pending.is_empty(), "issue has no captured save");
        for submission in &self.pending {
            self.edits.complete_save(submission, outcome)?;
        }
        if outcome != SaveOutcome::Uncertain {
            self.pending.clear();
            self.pending_operation = None;
        }
        Ok(())
    }
}

fn validate_detail(
    repository: &GithubRepositoryId,
    node_id: &str,
    detail: &IssueDetail,
) -> Result<()> {
    ensure!(
        detail.kind == "issue" && detail.number > 0 && detail.number <= i32::MAX as u64,
        "invalid issue document identity"
    );
    ensure!(
        detail
            .repo
            .eq_ignore_ascii_case(&repository.repository_name()),
        "issue detail belongs to another repository"
    );
    ensure!(
        !node_id.is_empty()
            && node_id.len() <= 256
            && node_id.bytes().all(|byte| byte.is_ascii_graphic()),
        "invalid issue node identity"
    );
    ensure!(
        !detail.title.contains(['\n', '\r']) && detail.title.len() <= 1024,
        "invalid issue title"
    );
    ensure!(
        detail.assignees.len() <= 100,
        "issue assignees exceed 100 entries"
    );
    ensure!(
        detail.comments.len() <= 10_000,
        "issue comments exceed document admission"
    );
    let source_bytes = [
        &detail.title,
        &detail.body,
        &detail.url,
        &detail.state,
        &detail.author,
        &detail.created_at,
        &detail.updated_at,
        &detail.milestone,
    ]
    .into_iter()
    .chain(detail.labels.iter())
    .chain(detail.projects.iter())
    .chain(detail.assignees.iter())
    .chain(detail.comments.iter().flat_map(|comment| {
        [
            &comment.body,
            &comment.author,
            &comment.created_at,
            &comment.updated_at,
            &comment.url,
        ]
    }))
    .try_fold(0usize, |total, value| total.checked_add(value.len()))
    .context("issue source size overflow")?;
    ensure!(
        source_bytes <= 8 * 1024 * 1024,
        "issue source exceeds 8 MiB"
    );
    Ok(())
}

fn field_block(edits: &EditStore, name: &str) -> Result<BufferBlock> {
    let region = RegionId(name.into());
    let field = edits.snapshot(&region)?;
    field_snapshot_block(region, field)
}
fn field_snapshot_block(
    region: RegionId,
    field: crate::edit::FieldSnapshot<'_>,
) -> Result<BufferBlock> {
    let text = BufferText::from_rows(field.text.split('\n'))?;
    let row = text
        .row_count()
        .checked_sub(1)
        .context("issue field omitted physical row")?;
    let range = TextRange {
        start: TextPosition { row: 0, column: 0 },
        end: TextPosition {
            row,
            column: text.row(row).context("issue field row unavailable")?.len(),
        },
    };
    Ok(BufferBlock {
        id: BlockId(format!("region:{}", region.0)),
        text,
        metadata: BlockMetadata {
            editable_region: vec![EditableRegion {
                id: region,
                revision: field.revision,
                sequence: field.sequence,
                range,
            }],
            ..BlockMetadata::default()
        },
    })
}

fn assignee_logins(text: &str) -> Result<Vec<String>> {
    let mut login: Vec<String> = Vec::new();
    for token in text
        .split(|character: char| character.is_whitespace() || character == ',')
        .filter(|token| !token.is_empty())
    {
        let token = token.strip_prefix('@').unwrap_or(token);
        ensure!(
            !token.is_empty()
                && token.len() <= 100
                && token.bytes().all(|byte| byte.is_ascii_alphanumeric()
                    || matches!(byte, b'-' | b'_' | b'[' | b']')),
            "invalid issue assignee login"
        );
        if !login
            .iter()
            .any(|previous| previous.eq_ignore_ascii_case(token))
        {
            login.push(token.to_owned());
        }
        ensure!(login.len() <= 100, "issue assignees exceed 100 entries");
    }
    Ok(login)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::identity::EditSequence;

    fn issue_document() -> IssueDocument {
        IssueDocument::new(
            DocumentId("issue-document".into()),
            GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
            "ISSUE_7".into(),
            IssueDetail {
                kind: "issue".into(),
                node_id: "ISSUE_7".into(),
                repo: "owner/repo".into(),
                number: 7,
                title: "Original title".into(),
                body: "exact\r\nbody\n".into(),
                url: "https://github.com/owner/repo/issues/7".into(),
                state: "OPEN".into(),
                author: "author".into(),
                created_at: "2026-09-07T00:00:00Z".into(),
                updated_at: "2026-09-07T00:00:00Z".into(),
                labels: vec!["bug".into()],
                assignees: vec!["keep".into(), "remove".into()],
                milestone: String::new(),
                projects: Vec::new(),
                comments_count: 0,
                comments: Vec::new(),
                subscription: "subscribed".into(),
                is_draft: false,
            },
            EditBudget::new(16 * 1024 * 1024).unwrap(),
        )
        .unwrap()
    }

    fn edit(
        document: &mut IssueDocument,
        region: &str,
        base: u64,
        sequence: u64,
        text: &str,
    ) -> IssueEditResult {
        document
            .edit(RegionEdit {
                document: DocumentId("issue-document".into()),
                region: RegionId(region.into()),
                base: RegionRevision(base),
                sequence: EditSequence(sequence),
                text: text.into(),
            })
            .unwrap()
    }

    #[test]
    fn native_issue_fields_preserve_newer_typing_and_capture_only_assignee_delta() {
        let mut document = issue_document();
        let initial = document.snapshot();
        let metadata = initial
            .block
            .iter()
            .find(|block| block.id.0 == "issue:metadata")
            .unwrap()
            .clone();
        edit(&mut document, "title", 0, 1, "Captured title");
        edit(&mut document, "assignees", 0, 1, "@keep @added");
        let capture = document.begin_save().unwrap().unwrap();
        let ReviewMutation::IssueEdit {
            title,
            body,
            add_assignees,
            remove_assignees,
        } = capture
        else {
            panic!("issue capture")
        };
        assert_eq!(title.as_deref(), Some("Captured title"));
        assert_eq!(body, None);
        assert_eq!(add_assignees, vec!["added"]);
        assert_eq!(remove_assignees, vec!["remove"]);
        edit(&mut document, "title", 1, 2, "Newer title");
        document.complete_save(SaveOutcome::Confirmed).unwrap();
        let field = document.fields().unwrap();
        assert!(field[0].dirty);
        assert!(!field[1].dirty && !field[2].dirty);
        let snapshot = document.snapshot();
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id.0 == "issue:metadata")
                .unwrap(),
            &metadata
        );
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id.0 == "region:body")
                .unwrap()
                .text
                .wire_rows(),
            vec!["exact\r", "body", ""]
        );
    }

    #[test]
    fn native_issue_empty_body_and_uncertain_save_keep_independent_revisions() {
        let mut document = issue_document();
        edit(&mut document, "body", 0, 1, "");
        let capture = document.begin_save().unwrap().unwrap();
        assert!(
            matches!(capture, ReviewMutation::IssueEdit { body: Some(ref body), .. } if body.is_empty())
        );
        document.complete_save(SaveOutcome::Uncertain).unwrap();
        edit(&mut document, "body", 1, 2, "newer\n");
        assert!(document.begin_save().is_err());
        let body = document.fields().unwrap().pop().unwrap();
        assert!(body.dirty && body.uncertain);
        assert_eq!(body.revision, RegionRevision(2));
        let mut equivalent = issue_document();
        edit(&mut equivalent, "assignees", 0, 1, " KEEP, @remove ");
        assert!(equivalent.begin_save().unwrap().is_none());
        assert!(!equivalent.fields().unwrap()[1].dirty);
    }
    #[test]
    fn prepared_issue_edit_abort_preserves_document_and_draft() {
        let mut document = issue_document();
        let before = serde_json::to_value(document.snapshot()).unwrap();
        let draft = document.draft_payload().unwrap();
        let mut prepared = document
            .prepare_edit(RegionEdit {
                document: DocumentId("issue-document".into()),
                region: RegionId("body".into()),
                base: RegionRevision(0),
                sequence: EditSequence(1),
                text: "candidate\r\n".into(),
            })
            .unwrap();
        let candidate = prepared.take_draft();
        assert_eq!(
            candidate["issue_document"]["field"]["body"]["text"],
            "candidate\r\n"
        );
        drop(prepared);
        assert_eq!(serde_json::to_value(document.snapshot()).unwrap(), before);
        assert_eq!(document.draft_payload().unwrap(), draft);
        edit(&mut document, "body", 0, 1, "accepted");
    }

    #[test]
    fn issue_draft_reopen_retains_newer_text_and_exact_pending_capture() {
        let mut original = issue_document();
        edit(&mut original, "body", 0, 1, "submitted\r\n");
        original.begin_save().unwrap().unwrap();
        edit(&mut original, "body", 1, 2, "newer\r\n");
        let draft = original.draft_payload().unwrap();
        let mut restored = issue_document();
        restored.restore_draft(&draft).unwrap();
        assert_eq!(restored.pending_operation(), original.pending_operation());
        assert!(restored.fields().unwrap()[2].uncertain);
        restored.recover_save(SaveOutcome::Confirmed).unwrap();
        let final_draft = restored.draft_payload().unwrap();
        assert_eq!(
            final_draft["issue_document"]["field"]["body"]["text"],
            "newer\r\n"
        );
        assert_eq!(
            final_draft["issue_document"]["field"]["body"]["baseline"],
            "submitted\r\n"
        );
        assert_eq!(restored.fields().unwrap()[2].sequence, EditSequence(2));
        assert!(restored.fields().unwrap()[2].dirty);
    }
    #[test]
    fn issue_refresh_prepares_remote_merge_and_preserves_dirty_field_text() {
        let mut document = issue_document();
        edit(&mut document, "body", 0, 1, "local body\r\n");
        let before = serde_json::to_value(document.snapshot()).unwrap();
        let mut detail = document.detail.clone();
        detail.title = "Fresh remote title".into();
        detail.body = "Conflicting remote body".into();
        let capture = document.capture_refresh(&detail).unwrap();
        let projection =
            crate::issue_presentation::project(&detail, capture.fields, &capture.width).unwrap();
        drop(
            document
                .prepare_refresh(capture.revision, capture.width, detail.clone(), projection)
                .unwrap(),
        );
        assert_eq!(serde_json::to_value(document.snapshot()).unwrap(), before);
        let capture = document.capture_refresh(&detail).unwrap();
        let projection =
            crate::issue_presentation::project(&detail, capture.fields, &capture.width).unwrap();
        let mut prepared = document
            .prepare_refresh(capture.revision, capture.width, detail, projection)
            .unwrap();
        let draft = prepared.take_draft();
        assert_eq!(
            draft["issue_document"]["field"]["body"]["text"],
            "local body\r\n"
        );
        assert!(prepared.commit().is_some());
        let fields = document.fields().unwrap();
        assert_eq!(fields[0].revision, RegionRevision(1));
        assert_eq!(fields[2].revision, RegionRevision(1));
        assert_eq!(fields[2].sequence, EditSequence(1));
        assert!(fields[2].dirty && fields[2].conflict);
        assert!(document.begin_save().is_err());
    }
    #[test]
    fn issue_closed_unknown_requires_durable_fresh_source_gate() {
        let mut document = issue_document();
        edit(&mut document, "body", 0, 1, "uncertain body");
        document.begin_save().unwrap().unwrap();
        let payload = document
            .resolution_payload(SaveOutcome::Rejected, true)
            .unwrap();
        document
            .resolve_pending(SaveOutcome::Rejected, true)
            .unwrap();
        assert!(document.begin_save().is_err());
        let mut reopened = issue_document();
        reopened.restore_draft(&payload).unwrap();
        assert!(reopened.fresh_required());
        assert!(reopened.begin_save().is_err());
    }
    #[test]
    fn issue_reopen_preserves_distinct_current_and_pending_counters() {
        let mut original = issue_document();
        edit(&mut original, "body", 0, 17, "captured\r\n");
        original.begin_save().unwrap().unwrap();
        edit(&mut original, "body", 1, 31, "newer\r\n");
        let draft = original.draft_payload().unwrap();
        assert_eq!(
            draft["issue_document"]["field"]["body"]["pending"]["revision"],
            1
        );
        assert_eq!(
            draft["issue_document"]["field"]["body"]["pending"]["sequence"],
            17
        );
        let mut restored = issue_document();
        restored.restore_draft(&draft).unwrap();
        assert_eq!(restored.pending_sequence(), 17);
        assert_eq!(restored.pending[0].revision(), RegionRevision(1));
        assert_eq!(restored.fields().unwrap()[2].revision, RegionRevision(2));
        assert_eq!(restored.fields().unwrap()[2].sequence, EditSequence(31));
        assert!(
            restored
                .edit(RegionEdit {
                    document: DocumentId("issue-document".into()),
                    region: RegionId("body".into()),
                    base: RegionRevision(2),
                    sequence: EditSequence(31),
                    text: "replayed".into()
                })
                .is_err()
        );
        restored
            .resolve_pending(SaveOutcome::Confirmed, false)
            .unwrap();
        let settled = restored.draft_payload().unwrap();
        assert_eq!(
            settled["issue_document"]["field"]["body"]["baseline"],
            "captured\r\n"
        );
        assert_eq!(settled["issue_document"]["field"]["body"]["sequence"], 31);
        assert_eq!(settled["issue_document"]["field"]["body"]["revision"], 2);
    }
    #[test]
    fn issue_draft_invalid_pending_counters_do_not_change_document() {
        let mut original = issue_document();
        edit(&mut original, "body", 0, 9, "pending");
        original.begin_save().unwrap().unwrap();
        let mut draft = original.draft_payload().unwrap();
        draft["issue_document"]["field"]["body"]["pending"]["sequence"] = serde_json::json!(10);
        let mut restored = issue_document();
        let before = serde_json::to_value(restored.snapshot()).unwrap();
        assert!(restored.restore_draft(&draft).is_err());
        assert_eq!(serde_json::to_value(restored.snapshot()).unwrap(), before);
    }
}
