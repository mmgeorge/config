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
    assert!(rendered.contains("Edits 3 files"));
    assert!(!rendered.contains("later unrelated workspace"));
}

/// Recover a baselined exchange whose host stopped mid-turn, run the finalization that session
/// start runs, and return the restored transcript.
async fn recovered_transcript(edited: bool) -> String {
    let repository = git_repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let exchange = super::test::save_host_interrupted_exchange(&mut broker, "capture").await;
    if edited {
        std::fs::write(
            repository.path().join("seed.txt"),
            "edited before the stop\n",
        )
        .unwrap();
    }

    broker
        .store
        .interrupt_detached_execution(&broker.session.id)
        .unwrap();
    broker.settle_recovered_finalization().await;

    let restored = broker.store.load_exchange(&exchange.id).unwrap().unwrap();
    assert_eq!(restored.state(), ExchangeState::Interrupted);
    assert!(
        restored.checkpoint_after.is_some(),
        "recovery finished the exchange without its closing checkpoint"
    );
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
    let rendered = projected
        .entry
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(rendered.contains("Interrupted"), "{rendered}");
    assert!(!rendered.contains("Edits unavailable"), "{rendered}");
    rendered
}

/// A host that stops mid-turn leaves a baselined exchange without its closing checkpoint.
/// Recovery keeps that exchange finalizing, and the finalization that session start runs
/// captures the workspace, so an edit made before the stop appears as Edits.
#[tokio::test]
async fn recovered_exchange_reports_edits_made_before_the_stop() {
    let rendered = recovered_transcript(true).await;
    assert!(rendered.contains("Edits 1 file"), "{rendered}");
}

/// The same recovery over an unchanged workspace records an empty comparison, which shows no
/// Edits row instead of reporting missing evidence.
#[tokio::test]
async fn recovered_exchange_over_an_unchanged_workspace_shows_no_edits_row() {
    let rendered = recovered_transcript(false).await;
    assert!(!rendered.contains("Edits"), "{rendered}");
}
