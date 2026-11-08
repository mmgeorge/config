use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock, EditableRegion, TextPosition, TextRange},
    document::{BufferDocument, LocalEditPreparation, PreparedLocalEdit},
    editable::{EditAcknowledgement, LocalEdit},
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
    pub baseline: String,
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
        let patch = self.buffer.commit().patch;
        IssueEditResult {
            acknowledgement,
            patch,
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
        let mut edits =
            EditStore::new(id.clone(), EditLimits { pending_saves: 3 }, budget.clone())?;
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
            EditLimits { pending_saves: 3 },
            self.budget.clone(),
        )?;
        let mut pending = Vec::new();
        for name in ISSUE_FIELD {
            let field = draft.field.get(name).context("issue draft field missing")?;
            ensure!(
                field.sequence <= forge_buffer::MAX_COUNTER
                    && (name == "body" || !field.text.contains(['\n', '\r'])),
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
                    baseline: field.baseline.to_owned(),
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

    pub fn capture_draft(&mut self, capture: Vec<RegionEdit>) -> Result<()> {
        for edit in &capture {
            ensure!(
                ISSUE_FIELD.contains(&edit.region.0.as_str()),
                "unknown issue field"
            );
            if edit.region.0 != "body" {
                ensure!(
                    !edit.text.contains(['\n', '\r']),
                    "invalid issue scalar field"
                );
            }
        }
        let captured = self.edits.prepare_capture(capture, &[])?;
        let title = captured.snapshot(&RegionId("title".into()))?;
        ensure!(!title.text.trim().is_empty(), "invalid issue title");
        assignee_logins(captured.snapshot(&RegionId("assignees".into()))?.text)?;
        let changes = ISSUE_FIELD
            .iter()
            .map(|name| {
                let region = RegionId((*name).into());
                let block = field_snapshot_block(region.clone(), captured.snapshot(&region)?)?;
                let index = self
                    .buffer
                    .block_index(&block.id)
                    .context("issue field is missing")?;
                Ok(forge_buffer::sequence::SequenceEdit {
                    range: index..index + 1,
                    block: vec![block],
                })
            })
            .collect::<Result<Vec<_>>>()?;
        let projection = self.buffer.prepare_edits(changes)?;
        captured.commit();
        if let Some(projection) = projection {
            projection.commit();
        }
        Ok(())
    }

    pub fn prepare_edit(&mut self, edit: RegionEdit) -> Result<PreparedIssueEdit<'_>> {
        let mut draft = self.draft_state()?;
        ensure!(
            ISSUE_FIELD.contains(&edit.region.0.as_str()),
            "unknown issue field"
        );
        if edit.region.0 != "body" {
            ensure!(
                !edit.text.contains(['\n', '\r']),
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
        ensure!(!title.text.trim().is_empty(), "invalid issue title");
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
            && add_assignees.is_empty()
            && remove_assignees.is_empty()
        {
            if let Some(submission) = self.edits.begin_save(&RegionId("assignees".into()))? {
                self.edits
                    .complete_save(&submission, SaveOutcome::Confirmed)?;
            }
            return Ok(None);
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
        !node_id.is_empty() && node_id.bytes().all(|byte| byte.is_ascii_graphic()),
        "invalid issue node identity"
    );
    ensure!(!detail.title.contains(['\n', '\r']), "invalid issue title");
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
            EditBudget::default(),
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
}
