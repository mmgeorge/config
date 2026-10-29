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

#[test]
fn long_reasoning_preview_is_one_line_and_preserves_the_complete_summary() -> Result<()> {
    for columns in [1, 2, 3, 20, 80] {
        let mut owner = SessionPresentation::new("session".into());
        let reasoning = "unicode λ detail ".repeat(4096);
        owner.initialize(vec![status(SessionPhase::Working {
            duration_ms: 0,
            started_at_ms: None,
            activity: crate::session::state_machine::WorkflowActivity::Working,
            reasoning_summary: Some(reasoning.clone()),
            execution: None,
        })])?;
        let width = WidthProfile {
            columns,
            ..Default::default()
        };
        let opened = owner.open(
            DocumentId("presentation".into()),
            ViewId("view".into()),
            width.clone(),
        )?;
        let block = &opened.status.block[0];
        let preview_start = block
            .metadata
            .decoration
            .iter()
            .find(|decoration| decoration.capture == "ForgeHarnessCommentary")
            .unwrap()
            .range
            .start
            .row;
        let rows = block.text.wire_rows();
        assert_eq!(rows.len(), preview_start + 1);
        let preview = &rows[preview_start];
        assert!(preview.ends_with(&".".repeat(columns.min(3))));
        assert!(width.cells(preview, 0)? <= columns);
        assert!(
            matches!(&owner.timeline.entry_list()[0], TimelineEntry::Status {
        status: SessionPhase::Working { reasoning_summary: Some(retained), .. }, ..
    } if retained == &reasoning)
        );
        assert!(
            opened
                .transcript
                .block
                .iter()
                .all(|block| block.text.row_count() == 0)
        );
        owner.reconcile(Vec::new())?;
        assert!(owner.snapshot(&opened.status.document)?.block.is_empty());
    }
    Ok(())
}

#[test]
fn queued_inputs_keep_status_visible_through_phase_changes() -> Result<()> {
    let mut owner = SessionPresentation::new("session".into());
    owner.initialize(vec![status(SessionPhase::Idle)])?;
    let document = DocumentId("queued-presentation".into());
    let opened = owner.open(
        document.clone(),
        ViewId("queue-view".into()),
        WidthProfile::default(),
    )?;
    owner.dispatch(PresentationRequest::Staging {
        document: document.clone(),
        sequence: 1,
        queue: vec!["first queued".into(), "second queued".into()],
        steering: vec!["pending direction".into()],
    })?;
    let queued = owner.sync(&document, opened.transcript.revision)?;
    assert!(queued.patch.is_empty());
    assert_eq!(queued.status.block.len(), 1);
    assert_eq!(queued.status.block[0].id.0, "harness:staging");
    let rows = queued.status.block[0].text.wire_rows().join("\n");
    assert!(rows.find("first queued").unwrap() < rows.find("second queued").unwrap());
    owner.reconcile(vec![status(SessionPhase::Working {
        duration_ms: 0,
        started_at_ms: None,
        activity: crate::session::state_machine::WorkflowActivity::Working,
        reasoning_summary: Some("Active thought".into()),
        execution: None,
    })])?;
    let working = owner.snapshot(&opened.status.document)?;
    assert_eq!(working.block.len(), 3);
    assert_eq!(
        working.block[0].text.wire_rows().last().copied(),
        Some("Active thought")
    );
    assert_eq!(working.block[1].text.wire_rows(), [""]);
    assert_eq!(working.block[2].id.0, "harness:staging");
    owner.reconcile(vec![status(SessionPhase::Idle)])?;
    assert_eq!(
        owner.snapshot(&opened.status.document)?.block[0]
            .text
            .wire_rows()
            .join("\n"),
        rows
    );
    owner.dispatch(PresentationRequest::Staging {
        document: document.clone(),
        sequence: 2,
        queue: Vec::new(),
        steering: Vec::new(),
    })?;
    let cleared = owner.sync(&document, opened.transcript.revision)?;
    assert!(cleared.patch.is_empty());
    assert!(cleared.status.block.is_empty());
    Ok(())
}
