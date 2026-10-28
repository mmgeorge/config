use super::test::{completed_interaction, planning_question_broker, repository};
use super::*;
use crate::exchange::{ExchangeChangeResult, ExchangeOutcome, ExchangePhase};

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

fn assert_preserved(broker: &HarnessBroker, exchange: &Exchange, previous: &Exchange) {
    let persisted = broker.store.load_exchange(&exchange.id).unwrap().unwrap();
    for actual in [exchange, &persisted] {
        assert_eq!(actual.checkpoint_before, previous.checkpoint_before);
        assert_eq!(actual.checkpoint_after, previous.checkpoint_after);
        assert_eq!(actual.checkpoint_diff_text, previous.checkpoint_diff_text);
        assert_eq!(actual.attributed_diff_text, previous.attributed_diff_text);
        assert_eq!(
            actual.attributed_matches_checkpoint,
            previous.attributed_matches_checkpoint
        );
        assert_eq!(actual.state(), ExchangeState::Finalizing);
        assert!(actual.finalization_error.is_some());
    }
}

#[tokio::test]
async fn checkpoint_commit_failure_preserves_previous_comparison() {
    for (recapture, recover_initial) in [(false, false), (true, false), (false, true)] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let before = initial(&mut broker).await;
        let mut exchange = running(&broker, &before.id);
        if recapture {
            std::fs::write(repository.path().join("seed.txt"), "first\n").unwrap();
            broker
                .capture_final_checkpoint(&mut exchange, ExchangeOutcome::Complete)
                .await
                .unwrap();
            exchange.phase = ExchangePhase::Running;
        }
        if recover_initial {
            exchange.checkpoint_before = None;
        }
        let outcome = if recover_initial {
            ExchangeOutcome::Failed
        } else {
            ExchangeOutcome::Complete
        };
        let previous = exchange.clone();
        std::fs::write(repository.path().join("seed.txt"), "replacement\n").unwrap();
        let connection = rusqlite::Connection::open(data.path().join("harness.sqlite3")).unwrap();
        connection.execute_batch("CREATE TRIGGER reject_checkpoint BEFORE INSERT ON checkpoint_record BEGIN SELECT RAISE(ABORT, 'injected checkpoint commit failure'); END;").unwrap();
        let error = broker
            .capture_final_checkpoint(&mut exchange, outcome)
            .await
            .unwrap_err();
        assert!(format!("{error:#}").contains("injected checkpoint commit failure"));
        assert_preserved(&broker, &exchange, &previous);
        connection
            .execute_batch("DROP TRIGGER reject_checkpoint")
            .unwrap();
        broker
            .capture_final_checkpoint(&mut exchange, outcome)
            .await
            .unwrap();
        assert!(exchange.finalization_error.is_none());
        assert!(
            exchange
                .checkpoint_diff_text
                .as_deref()
                .is_some_and(|patch| if recover_initial {
                    patch.is_empty()
                } else {
                    patch.contains("+replacement")
                })
        );
        assert!(matches!(
            exchange.change_result(),
            ExchangeChangeResult::Captured(_)
        ));
        let persisted = broker.store.load_exchange(&exchange.id).unwrap().unwrap();
        assert_eq!(
            persisted.checkpoint_diff_text,
            exchange.checkpoint_diff_text
        );
        assert_eq!(persisted.checkpoint_after, exchange.checkpoint_after);
    }
}

#[tokio::test]
async fn checkpoint_comparison_failure_preserves_previous_comparison() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let before = initial(&mut broker).await;
    let mut exchange = running(&broker, &before.id);
    std::fs::write(repository.path().join("seed.txt"), "first\n").unwrap();
    broker
        .capture_final_checkpoint(&mut exchange, ExchangeOutcome::Complete)
        .await
        .unwrap();
    exchange.phase = ExchangePhase::Running;
    let previous = exchange.clone();
    let mut broken = before.clone();
    broken.file.push(crate::checkpoint::CheckpointFile {
        path: "seed.txt".into(),
        object_id: "0".repeat(64),
        mode: 0o100644,
    });
    broker.store.save_checkpoint(&broken).unwrap();
    std::fs::write(repository.path().join("seed.txt"), "second\n").unwrap();
    assert!(
        broker
            .capture_final_checkpoint(&mut exchange, ExchangeOutcome::Complete)
            .await
            .is_err()
    );
    assert_preserved(&broker, &exchange, &previous);
    broker.store.save_checkpoint(&before).unwrap();
    broker
        .capture_final_checkpoint(&mut exchange, ExchangeOutcome::Complete)
        .await
        .unwrap();
    assert!(exchange.finalization_error.is_none());
    assert!(
        exchange
            .checkpoint_diff_text
            .as_deref()
            .unwrap()
            .contains("+second")
    );
}

#[tokio::test]
async fn captured_workspace_result_survives_formatting_external_edits_and_reopening() {
    let repository = repository();
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
