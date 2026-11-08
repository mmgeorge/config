use super::*;

fn status(phase: SessionPhase) -> TimelineEntry {
    TimelineEntry::Status {
        id: "session:status".into(),
        created_at_ms: 0,
        status: phase,
        status_thoughts: true,
        agent_count: 0,
        checks: None,
    }
}

#[test]
fn status_lifetime_and_actions_are_independent_of_transcript_rows() -> Result<()> {
    let mut owner = SessionPresentation::new("session".into());
    owner.initialize(vec![status(SessionPhase::Idle)])?;
    let document = DocumentId("presentation".into());
    let view = ViewId("view".into());
    let opened = owner.open(document.clone(), view.clone(), WidthProfile::default())?;
    assert!(opened.status.block.is_empty());
    assert!(
        opened
            .transcript
            .block
            .iter()
            .all(|block| block.text.row_count() == 0)
    );
    assert_ne!(opened.status.document, opened.transcript.document);
    owner.reconcile(vec![status(SessionPhase::AwaitingPlanReview {
        plan_id: "plan".into(),
        revision: 1,
    })])?;
    let updated = owner.sync(&document, opened.transcript.revision)?;
    assert!(updated.patch.is_empty());
    assert!(updated.status.revision > opened.status.revision);
    let target = updated.status.block[0].metadata.target[0].id.clone();
    let action = |revision, sequence, view| PresentationRequest::StatusAction {
        document: document.clone(),
        status: updated.status.document.clone(),
        revision,
        view,
        sequence: InputSequence(sequence),
        target: target.clone(),
    };
    assert!(
        owner
            .dispatch(action(opened.status.revision, 1, view.clone()))
            .is_err()
    );
    assert!(
        owner
            .dispatch(action(updated.status.revision, 1, ViewId("unknown".into())))
            .is_err()
    );
    let accepted = owner.dispatch(action(updated.status.revision, 1, view.clone()))?;
    assert_eq!(accepted["plan_id"], "plan");
    assert!(
        owner
            .dispatch(action(updated.status.revision, 1, view.clone()))
            .is_err()
    );
    assert_eq!(
        owner.sync(&document, opened.transcript.revision)?.status,
        updated.status
    );
    owner.reconcile(vec![status(SessionPhase::Idle)])?;
    let empty = owner.sync(&document, opened.transcript.revision)?;
    assert!(empty.patch.is_empty());
    assert!(empty.status.block.is_empty());
    assert!(
        owner
            .dispatch(action(updated.status.revision, 2, view))
            .is_err()
    );
    owner.close(&document)?;
    assert!(owner.snapshot(&updated.status.document).is_err());
    Ok(())
}
