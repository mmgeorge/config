use super::*;
use crate::{provider::ProviderLaunch, storage::SqliteStore};
use anyhow::Result;
use forge_git::test_support::{init_repository, set_identity};
use std::{path::Path, sync::Arc};

fn manager() -> Arc<BackgroundManager> {
    let diff = forge_diff::engine::DiffEngine::new(Default::default(), 1);
    let syntax = forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool());
    BackgroundManager::new(
        Arc::new(forge_git::store::RepositoryStore::default()),
        diff,
        syntax,
    )
}

async fn generate(
    manager: &Arc<BackgroundManager>,
    root: &Path,
    data: &Path,
) -> Result<BackgroundResult> {
    let context = BackgroundContext::read(&manager.repositories, root.to_str().unwrap()).await?;
    manager
        .generate(BackgroundRequest {
            cancellation: CancellationToken::new(),
            purpose: crate::provider::TextGeneration::CommitMessage,
            config: crate::service::TextGenerationConfig {
                provider: ProviderLaunch {
                    kind: forge_providers::registry::nonselectable_id()
                        .expect("registry supplies a nonselectable fixture identity"),
                    command: vec![],
                },
                model: "mock-model".into(),
                reasoning_effort: Some("low".into()),
                service_tier: Default::default(),
                data_root: data.into(),
                auto_compaction: Default::default(),
            },
            context,
            input: "Describe the current test change".into(),
            delivery: None,
        })
        .await
}

/// Create a fixture directory holding an empty repository and a state directory.
fn background_fixture() -> Result<(tempfile::TempDir, std::path::PathBuf, std::path::PathBuf)> {
    let fixture = tempfile::tempdir()?;
    let root = fixture.path().join("repository");
    std::fs::create_dir(&root)?;
    let data = fixture.path().join("state");
    init_repository(&root);
    set_identity(&root);
    Ok((fixture, root, data))
}

#[tokio::test]
async fn background_resume_marks_interrupted_compaction_unconfirmed() -> Result<()> {
    let (_fixture, root, data) = background_fixture()?;
    let first_manager = manager();
    let first = generate(&first_manager, &root, &data).await?;
    first_manager.shutdown().await?;
    drop(first_manager);
    {
        let mut store = SqliteStore::open(&data)?;
        store.save_session_event(&crate::timeline::SessionEventRecord {
            id: "interrupted-compaction".into(),
            session_id: first.session_id.clone(),
            owner: crate::timeline::TimelineOwner::session(&first.session_id),
            created_at_ms: 1,
            detail: crate::timeline::SessionEventKind::Compaction {
                compaction: (crate::provider::Compaction {
                    id: "interrupted-compaction".into(),
                    thread_id: "provider".into(),
                    state: crate::provider::CompactionState::Running,
                    manual: false,
                    started_at_ms: Some(1),
                    completed_at_ms: None,
                    result: Default::default(),
                })
                .into(),
            },
        })?;
    }
    let resumed_manager = manager();
    let resumed = generate(&resumed_manager, &root, &data).await?;
    resumed_manager.shutdown().await?;
    assert_eq!(
        first.session_id, resumed.session_id,
        "fresh manager lost durable session"
    );
    let events = SqliteStore::open(&data)?.list_session_event(&first.session_id)?;
    assert!(events.iter().any(|event| matches!(&event.detail,crate::timeline::SessionEventKind::Compaction { compaction } if compaction.state == crate::provider::CompactionState::Unconfirmed)));
    Ok(())
}
