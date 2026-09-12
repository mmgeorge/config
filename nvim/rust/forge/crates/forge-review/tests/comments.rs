use forge_buffer::identity::{DocumentId, EditSequence, RegionRevision};
use forge_github::model::GithubRepositoryId;
use forge_github::pull_request::PullRequestTarget;
use forge_review::comments::*;
use forge_review::edit::{
    ConflictResolution, EditBudget, EditLimits, EditStore, MergeOutcome, RegionEdit, SaveOutcome,
};
use forge_review::review::ReviewMode;

fn target() -> PullRequestTarget {
    PullRequestTarget {
        repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        number: 7,
        node_id: "PR_test".into(),
    }
}

fn stores(mode: ReviewMode) -> (CommentStore, EditStore) {
    let document = DocumentId("review".into());
    (
        CommentStore::new(document.clone(), target(), mode, CommentLimits::default()).unwrap(),
        EditStore::new(
            document,
            EditLimits::default(),
            EditBudget::new(4096).unwrap(),
        )
        .unwrap(),
    )
}

fn anchor() -> CommentAnchor {
    CommentAnchor {
        revision: "a".repeat(40),
        path: "src/code.rs".into(),
        side: CommentSide::Right,
        first_line: 10,
        last_line: 12,
    }
}

fn remote(body: &str) -> RemoteComment {
    RemoteComment {
        identity: RemoteCommentIdentity {
            node_id: "COMMENT_test".into(),
            database_id: 123,
            url: "https://github.com/owner/repo/pull/7#discussion_r123".into(),
        },
        anchor: Some(anchor()),
        viewer_did_author: true,
        body: body.into(),
    }
}

fn change(comments: &CommentStore, edits: &mut EditStore, comment: CommentId, body: &str) {
    let field = comments.body(edits, comment).unwrap();
    let edit = RegionEdit {
        document: edits.document_id().clone(),
        region: comments.record(comment).unwrap().region.clone(),
        base: field.revision,
        sequence: EditSequence(field.sequence.0 + 1),
        text: body.into(),
    };
    comments.accept(edits, edit).unwrap();
}

#[test]
fn compact_and_focused_occurrences_share_one_comment_owner() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    let inline = comments
        .add_occurrence(comment, OccurrenceKind::Inline)
        .unwrap();
    let conversation = comments
        .add_occurrence(comment, OccurrenceKind::Conversation)
        .unwrap();
    assert_eq!(comments.focused(), None);
    comments.focus(inline, FocusReason::Cursor).unwrap();
    change(&comments, &mut edits, comment, "local draft");
    assert!(comments.focus(conversation, FocusReason::Cursor).is_err());
    assert_eq!(comments.focused(), Some(inline));
    comments.focus(conversation, FocusReason::Open).unwrap();
    assert_eq!(comments.focused(), Some(conversation));
    assert_eq!(comments.body(&edits, comment).unwrap().text, "local draft");
    comments.remove_occurrence(conversation).unwrap();
    assert_eq!(comments.focused(), None);
    comments.focus(inline, FocusReason::Open).unwrap();
    assert!(comments.body(&edits, comment).unwrap().dirty);
}

#[test]
fn remote_merge_preserves_dirty_owner_and_its_single_reply_draft() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    let reply = comments.reply_draft(&mut edits, comment).unwrap();
    change(&comments, &mut edits, comment, "local");
    change(&comments, &mut edits, reply, "reply draft\n");
    let (same, outcome) = comments.merge(&mut edits, remote("remote")).unwrap();
    assert_eq!(same, comment);
    assert_eq!(outcome, MergeOutcome::Conflict);
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!(
        (body.text, body.baseline, body.remote),
        ("local", "old", Some("remote"))
    );
    let revision = body.revision;
    assert_eq!(comments.reply_draft(&mut edits, comment).unwrap(), reply);
    assert_eq!(comments.body(&edits, reply).unwrap().text, "reply draft\n");
    assert_eq!(comments.record(reply).unwrap().reply_to, Some(comment));
    assert!(
        edits
            .begin_save(&comments.record(comment).unwrap().region)
            .is_err()
    );
    edits
        .resolve(
            &comments.record(comment).unwrap().region,
            revision,
            ConflictResolution::KeepLocal,
        )
        .unwrap();
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!(
        (body.text, body.baseline, body.remote),
        ("local", "remote", None)
    );
    assert!(body.dirty);
}

#[test]
fn clean_remote_update_advances_region_but_matching_local_text_only_advances_baseline() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    assert_eq!(
        comments.merge(&mut edits, remote("fresh")).unwrap().1,
        MergeOutcome::Updated {
            revision: RegionRevision(1)
        }
    );
    change(&comments, &mut edits, comment, "matching local");
    assert_eq!(
        comments
            .merge(&mut edits, remote("matching local"))
            .unwrap()
            .1,
        MergeOutcome::Converged
    );
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!(body.revision, RegionRevision(2));
    assert_eq!(body.sequence, EditSequence(1));
    assert!(!body.dirty);
}

#[test]
fn readonly_viewer_can_explicitly_open_but_cannot_edit_or_promote_on_cursor() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let mut observation = remote("foreign body");
    observation.viewer_did_author = false;
    let (comment, _) = comments.merge(&mut edits, observation).unwrap();
    let occurrence = comments
        .add_occurrence(comment, OccurrenceKind::Inline)
        .unwrap();
    assert!(comments.focus(occurrence, FocusReason::Cursor).is_err());
    comments.focus(occurrence, FocusReason::Open).unwrap();
    let result = comments.accept(
        &mut edits,
        RegionEdit {
            document: DocumentId("review".into()),
            region: comments.record(comment).unwrap().region.clone(),
            base: RegionRevision(0),
            sequence: EditSequence(1),
            text: "forbidden".into(),
        },
    );
    assert!(result.is_err());
    assert_eq!(comments.body(&edits, comment).unwrap().text, "foreign body");
    assert!(comments.reply_draft(&mut edits, comment).is_ok());
}

#[test]
fn batched_review_does_not_admit_inline_reply_drafts() {
    let (mut comments, mut edits) = stores(ReviewMode::Batched);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    assert!(comments.reply_draft(&mut edits, comment).is_err());
    assert_eq!(
        comments.draft(&mut edits, Some(anchor())).unwrap(),
        CommentId(2)
    );
}

#[test]
fn explicit_browser_targets_never_fall_back_to_adjacent_comment() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    let occurrence = comments
        .add_occurrence(comment, OccurrenceKind::Inline)
        .unwrap();
    let target = CommentActionTarget::Comment(occurrence);
    assert_eq!(
        comments.browser_target(&target).unwrap(),
        Some(CommentBrowserTarget::Comment(
            "https://github.com/owner/repo/pull/7#discussion_r123"
        ))
    );
    let code = CommentActionTarget::Code(anchor());
    assert!(matches!(
        comments.browser_target(&code).unwrap(),
        Some(CommentBrowserTarget::Code(_))
    ));
    let draft = comments.draft(&mut edits, Some(anchor())).unwrap();
    let draft_occurrence = comments
        .add_occurrence(draft, OccurrenceKind::Inline)
        .unwrap();
    assert!(
        comments
            .browser_target(&CommentActionTarget::Comment(draft_occurrence))
            .unwrap()
            .is_none()
    );
    comments.remove_occurrence(occurrence).unwrap();
    assert!(comments.browser_target(&target).is_err());
}

#[test]
fn pending_save_rejects_refresh_before_metadata_or_body_changes() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    change(&comments, &mut edits, comment, "submitted");
    let submission = edits
        .begin_save(&comments.record(comment).unwrap().region)
        .unwrap()
        .unwrap();
    let mut observation = remote("different remote");
    observation.viewer_did_author = false;
    assert!(comments.merge(&mut edits, observation).is_err());
    assert!(comments.record(comment).unwrap().viewer_did_author);
    assert_eq!(comments.body(&edits, comment).unwrap().text, "submitted");
    edits
        .complete_save(&submission, SaveOutcome::Rejected)
        .unwrap();
    assert_eq!(
        comments
            .merge(&mut edits, remote("different remote"))
            .unwrap()
            .1,
        MergeOutcome::Conflict
    );
}

#[test]
fn invalid_identity_anchor_and_cross_document_input_preserve_admission() {
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let mut invalid = remote("old");
    invalid.identity.url = "https://github.com/owner/other/pull/7#discussion_r123".into();
    assert!(comments.merge(&mut edits, invalid).is_err());
    let mut invalid = anchor();
    invalid.path = "../escape.rs".into();
    assert!(comments.draft(&mut edits, Some(invalid)).is_err());
    let mut invalid = anchor();
    invalid.revision = "HEAD".into();
    assert!(comments.draft(&mut edits, Some(invalid)).is_err());
    let mut other = EditStore::new(
        DocumentId("other".into()),
        EditLimits::default(),
        EditBudget::new(1024).unwrap(),
    )
    .unwrap();
    assert!(comments.merge(&mut other, remote("old")).is_err());
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    assert_eq!(comment, CommentId(1));
    let mut foreign = remote("wrong body");
    foreign.identity.database_id += 1;
    assert!(comments.merge(&mut edits, foreign).is_err());
    assert_eq!(comments.body(&edits, comment).unwrap().text, "old");
}

#[test]
fn occurrence_lifetime_and_shared_text_bounds_do_not_lose_drafts() {
    let document = DocumentId("review".into());
    let budget = EditBudget::new(3).unwrap();
    let mut edits =
        EditStore::new(document.clone(), EditLimits::default(), budget.clone()).unwrap();
    let mut comments = CommentStore::new(
        document,
        target(),
        ReviewMode::Overview,
        CommentLimits {
            comments: 1,
            occurrences: 1,
            occurrence_lifetimes: 2,
        },
    )
    .unwrap();
    assert!(comments.merge(&mut edits, remote("oversized")).is_err());
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    assert_eq!(comment, CommentId(1));
    assert!(comments.draft(&mut edits, None).is_err());
    let first = comments
        .add_occurrence(comment, OccurrenceKind::Inline)
        .unwrap();
    assert!(
        comments
            .add_occurrence(comment, OccurrenceKind::Inline)
            .is_err()
    );
    comments.remove_occurrence(first).unwrap();
    let second = comments
        .add_occurrence(comment, OccurrenceKind::Inline)
        .unwrap();
    assert_ne!(first, second);
    comments.remove_occurrence(second).unwrap();
    assert!(
        comments
            .add_occurrence(comment, OccurrenceKind::Inline)
            .is_err()
    );
    assert_eq!(budget.retained_bytes(), 3);
    assert_eq!(comments.body(&edits, comment).unwrap().text, "old");
}

#[test]
fn created_comment_keeps_identity_and_newer_native_text() {
    use forge_review::comments::{CommentMutation, CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let comment = comments.draft(&mut edits, Some(anchor())).unwrap();
    change(&comments, &mut edits, comment, "submitted");
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    assert!(matches!(save.mutation(), CommentMutation::Create { .. }));
    change(&comments, &mut edits, comment, "newer");
    assert_eq!(save.text(), "submitted");
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    comments
        .complete_save(
            &mut edits,
            &save,
            CommentSaveOutcome::Confirmed(remote("submitted")),
        )
        .unwrap();
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!((body.text, body.baseline), ("newer", "submitted"));
    assert!(body.dirty);
    assert_eq!(
        comments.merge(&mut edits, remote("submitted")).unwrap().0,
        comment
    );
    assert!(
        comments
            .complete_save(&mut edits, &save, CommentSaveOutcome::Rejected)
            .is_err()
    );
    let next = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    assert!(matches!(next.mutation(), CommentMutation::Edit { .. }));
}

#[test]
fn unknown_creation_cannot_be_cleared_by_absence_or_reposted() {
    use forge_review::comments::{CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let comment = comments.draft(&mut edits, Some(anchor())).unwrap();
    change(&comments, &mut edits, comment, "submitted");
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Uncertain)
        .unwrap();
    assert!(comments.reconcile_save(&mut edits, &save, None).is_err());
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    assert!(comments.body(&edits, comment).unwrap().uncertain);
    comments
        .reconcile_save(&mut edits, &save, Some(remote("submitted")))
        .unwrap();
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .unwrap()
            .is_none()
    );
}

#[test]
fn confirmed_reply_retires_parent_draft_slot_without_losing_newer_text() {
    use forge_review::comments::{CommentMutation, CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (parent, _) = comments.merge(&mut edits, remote("parent")).unwrap();
    let reply = comments.reply_draft(&mut edits, parent).unwrap();
    change(&comments, &mut edits, reply, "reply");
    let save = comments
        .save(&mut edits, reply, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    assert!(
        matches!(save.mutation(), CommentMutation::Reply { parent } if parent.node_id == "COMMENT_test")
    );
    change(&comments, &mut edits, reply, "reply edited");
    let mut observed = remote("reply");
    observed.identity.node_id = "COMMENT_reply".into();
    observed.identity.database_id = 124;
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Confirmed(observed))
        .unwrap();
    assert_eq!(comments.body(&edits, reply).unwrap().text, "reply edited");
    assert_eq!(comments.record(reply).unwrap().reply_to, None);
    assert_ne!(comments.reply_draft(&mut edits, parent).unwrap(), reply);
}

#[test]
fn confirmed_delete_captures_clean_body_and_preserves_later_edits() {
    use forge_review::comments::{CommentMutation, CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Delete)
        .unwrap()
        .unwrap();
    assert!(matches!(save.mutation(), CommentMutation::Delete { .. }));
    assert_eq!(save.text(), "old");
    change(&comments, &mut edits, comment, "newer");
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Deleted)
        .unwrap();
    assert!(comments.record(comment).unwrap().deleted);
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!((body.text, body.baseline), ("newer", ""));
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    assert!(comments.merge(&mut edits, remote("stale")).is_err());
    assert!(comments.reply_draft(&mut edits, comment).is_err());
}

#[test]
fn rejected_write_keeps_baseline_and_invalid_confirmation_requires_observation() {
    use forge_review::comments::{CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    change(&comments, &mut edits, comment, "submitted");
    let rejected = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    comments
        .complete_save(&mut edits, &rejected, CommentSaveOutcome::Rejected)
        .unwrap();
    assert_eq!(comments.body(&edits, comment).unwrap().baseline, "old");
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    assert!(
        comments
            .complete_save(
                &mut edits,
                &save,
                CommentSaveOutcome::Confirmed(remote("different"))
            )
            .is_err()
    );
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    let mut foreign = remote("submitted");
    foreign.identity.node_id = "COMMENT_other".into();
    assert!(
        comments
            .reconcile_save(&mut edits, &save, Some(foreign))
            .is_err()
    );
    comments
        .reconcile_save(&mut edits, &save, Some(remote("observed")))
        .unwrap();
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!((body.text, body.baseline), ("submitted", "observed"));
}

#[test]
fn uncertain_delete_observes_existing_remote_without_reissuing_delete() {
    use forge_review::comments::{CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Delete)
        .unwrap()
        .unwrap();
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Uncertain)
        .unwrap();
    comments
        .reconcile_save(&mut edits, &save, Some(remote("old")))
        .unwrap();
    assert!(!comments.record(comment).unwrap().deleted);
    assert!(!comments.body(&edits, comment).unwrap().dirty);
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .unwrap()
            .is_none()
    );
}

#[test]
fn foreign_receipt_and_empty_draft_rejections_leave_owner_unchanged() {
    use forge_review::comments::{CommentSaveAction, CommentSaveOutcome};
    let (mut comments, mut edits) = stores(ReviewMode::Overview);
    let (mut other, mut other_edits) = stores(ReviewMode::Overview);
    let comment = comments.draft(&mut edits, Some(anchor())).unwrap();
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Delete)
            .is_err()
    );
    let other_comment = other.draft(&mut other_edits, Some(anchor())).unwrap();
    change(&comments, &mut edits, comment, "first");
    change(&other, &mut other_edits, other_comment, "second");
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    let other_save = other
        .save(&mut other_edits, other_comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    assert!(
        comments
            .complete_save(&mut edits, &other_save, CommentSaveOutcome::Rejected)
            .is_err()
    );
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Rejected)
        .unwrap();
    assert_eq!(comments.body(&edits, comment).unwrap().text, "first");
}

#[test]
fn save_receipt_retains_its_shared_budget_after_document_drop() {
    use forge_review::comments::{CommentSaveAction, CommentSaveOutcome};
    let document = DocumentId("review".into());
    let budget = EditBudget::new(16).unwrap();
    let mut edits =
        EditStore::new(document.clone(), EditLimits::default(), budget.clone()).unwrap();
    let mut comments = CommentStore::new(
        document,
        target(),
        ReviewMode::Overview,
        CommentLimits::default(),
    )
    .unwrap();
    let comment = comments.draft(&mut edits, Some(anchor())).unwrap();
    change(&comments, &mut edits, comment, "submitted");
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Uncertain)
        .unwrap();
    drop(comments);
    drop(edits);
    assert_eq!(budget.retained_bytes(), 9);
    assert_eq!(save.text(), "submitted");
    drop(save);
    assert_eq!(budget.retained_bytes(), 0);
}

#[test]
fn failed_observation_allocation_keeps_uncertain_capture_and_identity() {
    use forge_review::comments::{CommentSaveAction, CommentSaveOutcome};
    let document = DocumentId("review".into());
    let budget = EditBudget::new(8).unwrap();
    let mut edits =
        EditStore::new(document.clone(), EditLimits::default(), budget.clone()).unwrap();
    let mut comments = CommentStore::new(
        document,
        target(),
        ReviewMode::Overview,
        CommentLimits::default(),
    )
    .unwrap();
    let (comment, _) = comments.merge(&mut edits, remote("old")).unwrap();
    change(&comments, &mut edits, comment, "local");
    let save = comments
        .save(&mut edits, comment, CommentSaveAction::Save)
        .unwrap()
        .unwrap();
    comments
        .complete_save(&mut edits, &save, CommentSaveOutcome::Uncertain)
        .unwrap();
    assert!(
        comments
            .reconcile_save(&mut edits, &save, Some(remote("remote")))
            .is_err()
    );
    let body = comments.body(&edits, comment).unwrap();
    assert_eq!((body.text, body.baseline), ("local", "old"));
    assert!(body.uncertain);
    assert_eq!(budget.retained_bytes(), 8);
    assert!(
        comments
            .save(&mut edits, comment, CommentSaveAction::Save)
            .is_err()
    );
    comments
        .reconcile_save(&mut edits, &save, Some(remote("local")))
        .unwrap();
    assert_eq!(budget.retained_bytes(), 5);
    assert!(!comments.body(&edits, comment).unwrap().dirty);
}
