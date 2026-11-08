use forge_buffer::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
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
        CommentStore::new(document.clone(), target(), mode).unwrap(),
        EditStore::new(document, EditLimits::default(), EditBudget::default()).unwrap(),
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
fn explicit_capture_admits_local_comments_and_fields_atomically() {
    let (mut comments, mut edits) = stores(ReviewMode::Batched);
    let summary = RegionId("review_summary".into());
    let region = RegionId("draft-comment/client-1/body".into());
    edits
        .insert(summary.clone(), RegionRevision(0), "Original".into())
        .unwrap();
    let capture = |base| {
        vec![
            RegionEdit {
                document: DocumentId("review".into()),
                region: region.clone(),
                base: RegionRevision(0),
                sequence: EditSequence(1),
                text: "Local body\r\n\n".into(),
            },
            RegionEdit {
                document: DocumentId("review".into()),
                region: summary.clone(),
                base: RegionRevision(base),
                sequence: EditSequence(1),
                text: "Captured summary".into(),
            },
        ]
    };
    let declaration = || {
        vec![DraftComment {
            region: region.clone(),
            anchor: Some(anchor()),
            reply_to: None,
        }]
    };
    assert!(
        comments
            .accept_capture(&mut edits, capture(9), declaration())
            .is_err()
    );
    assert!(!comments.contains_region(&region));
    assert!(edits.snapshot(&region).is_err());
    assert_eq!(edits.snapshot(&summary).unwrap().text, "Original");
    comments
        .accept_capture(&mut edits, capture(0), declaration())
        .unwrap();
    assert!(comments.contains_region(&region));
    let body = edits.snapshot(&region).unwrap();
    assert_eq!(
        (body.text, body.baseline, body.revision, body.sequence),
        ("Local body\r\n\n", "", RegionRevision(1), EditSequence(1))
    );
    assert_eq!(edits.snapshot(&summary).unwrap().text, "Captured summary");
    comments
        .accept_capture(&mut edits, capture(0), declaration())
        .unwrap();
    assert_eq!(edits.snapshot(&region).unwrap().revision, RegionRevision(1));
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
