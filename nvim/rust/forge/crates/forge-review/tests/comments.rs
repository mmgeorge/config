use forge_buffer::identity::{DocumentId, EditSequence};
use forge_github::model::GithubRepositoryId;
use forge_github::pull_request::PullRequestTarget;
use forge_review::comments::*;
use forge_review::edit::{EditBudget, EditLimits, EditStore, RegionEdit};
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
