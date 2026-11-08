use super::test::{completed_interaction, git_repository, planning_question_broker};
use super::*;
use crate::exchange::{ExchangeOutcome, ExchangePhase};

async fn initial(broker: &mut HarnessBroker) -> crate::checkpoint::CheckpointRecord {
    let checkpoint = broker
        .checkpoint
        .as_ref()
        .unwrap()
        .capture(
            &broker.store.objects,
            &broker.repositories,
            &broker.session.id,
            1,
        )
        .await
        .unwrap();
    broker.store.save_checkpoint(&checkpoint).unwrap();
    checkpoint
}

fn running(broker: &HarnessBroker, before: &str) -> Exchange {
    let mut exchange = completed_interaction("capture", &broker.session.id, Vec::new());
    exchange.phase = ExchangePhase::Running;
    exchange.checkpoint_before = Some(before.into());
    exchange
}

#[tokio::test]
async fn captured_workspace_result_survives_formatting_external_edits_and_reopening() {
    let repository = git_repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let before = initial(&mut broker).await;
    let mut exchange = running(&broker, &before.id);
    let source = repository.path().join("seed.txt");
    std::fs::write(&source, "fn main(){first();}\n").unwrap();
    std::fs::write(&source, "fn main() {\n    first();\n}\n").unwrap();
    std::fs::write(&source, "fn main() {\n    final_call();\n}\n").unwrap();
    std::fs::write(repository.path().join("command.txt"), "command-created\n").unwrap();
    std::fs::write(repository.path().join("external.txt"), "external edit\n").unwrap();
    let transient = repository.path().join("transient.txt");
    std::fs::write(&transient, "temporary\n").unwrap();
    std::fs::remove_file(transient).unwrap();
    broker
        .capture_final_checkpoint(&mut exchange, ExchangeOutcome::Complete)
        .await
        .unwrap();
    let saved = exchange.checkpoint_diff_text.clone().unwrap();
    assert!(saved.contains("final_call"));
    assert!(saved.contains("command.txt") && saved.contains("external.txt"));
    assert!(!saved.contains("transient.txt"));
    assert!(exchange.attributed_diff_text.is_none());
    std::fs::write(&source, "later unrelated workspace\n").unwrap();
    let store = SqliteStore::read_only(data.path()).unwrap();
    let restored = store.load_exchange(&exchange.id).unwrap().unwrap();
    let projected = crate::buffer::projection::project(
        crate::buffer::projection::ProjectionSource::Exchange(&restored),
        &forge_buffer::width::WidthProfile::default(),
        crate::buffer::projection::FIXTURE_NOW_MS,
        false,
        &Default::default(),
        &Default::default(),
        &mut Default::default(),
    )
    .unwrap();
    assert!(
        matches!(&projected.action[&forge_buffer::identity::TargetId("capture:results:changes".into())],
        crate::buffer::projection::TranscriptAction::Diff { text, .. } if text == &saved)
    );
    let rendered = projected
        .entry
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(rendered.contains("Workspace changes 3 files"));
    assert!(!rendered.contains("later unrelated workspace"));
}
