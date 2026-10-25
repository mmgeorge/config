use super::*;
use crate::{backend::BackendLaunch, session::SessionStore, storage::SqliteStore};
use anyhow::{Context, Result};
use std::{path::Path, sync::Arc};

fn git(root: &Path, arguments: &[&str]) {
    let output = std::process::Command::new("git")
        .arg("-C")
        .arg(root)
        .args(arguments)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

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
            purpose: crate::backend::TextGeneration::CommitMessage,
            config: crate::service::TextGenerationConfig {
                backend: BackendLaunch {
                    kind: "mock".into(),
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

#[tokio::test]
async fn background_reuses_restarts_rotates_and_keeps_timeline() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let root = fixture.path().join("repository");
    std::fs::create_dir(&root)?;
    let data = fixture.path().join("state");
    git(&root, &["init", "-q", "-b", "main"]);
    git(&root, &["config", "user.name", "Fixture"]);
    git(&root, &["config", "user.email", "fixture@example.test"]);
    let first_manager = manager();
    let empty = first_manager
        .timeline(serde_json::json!({"data_root":data,"workspace":root}))
        .await?;
    assert!(empty["session"].is_null());
    assert!(SqliteStore::open(&data)?.list_session(None)?.is_empty());
    let first = generate(&first_manager, &root, &data).await?;
    first_manager.shutdown().await;
    assert!(!first.text.is_empty());
    let second = generate(&first_manager, &root, &data).await?;
    first_manager.shutdown().await;
    assert_eq!(first.session_id, second.session_id);
    git(
        &root,
        &["commit", "-q", "--allow-empty", "-m", "feat: initial"],
    );
    let third = generate(&first_manager, &root, &data).await?;
    first_manager.shutdown().await;
    assert_eq!(
        first.session_id, third.session_id,
        "first commit should advance an unborn branch"
    );
    let (left, right) = tokio::join!(
        generate(&first_manager, &root, &data),
        generate(&first_manager, &root, &data)
    );
    assert_eq!(
        left?.session_id, right?.session_id,
        "concurrent requests created different conversations"
    );
    first_manager.shutdown().await;

    drop(first_manager);
    {
        let mut store = SqliteStore::open(&data)?;
        store.save_session_event(&crate::timeline::SessionEventRecord {
            id: "interrupted-compaction".into(),
            session_id: first.session_id.clone(),
            owner: crate::timeline::TimelineOwner::session(&first.session_id),
            created_at_ms: 1,
            detail: crate::timeline::SessionEventKind::Compaction {
                compaction: crate::backend::Compaction {
                    id: "interrupted-compaction".into(),
                    thread_id: "provider".into(),
                    state: crate::backend::CompactionState::Running,
                    manual: false,
                    started_at_ms: Some(1),
                    completed_at_ms: None,
                    result: Default::default(),
                },
            },
        })?;
    }
    let resumed_manager = manager();
    let resumed = generate(&resumed_manager, &root, &data).await?;
    resumed_manager.shutdown().await;
    assert_eq!(
        first.session_id, resumed.session_id,
        "fresh manager lost durable session"
    );
    let events = SqliteStore::open(&data)?.list_session_event(&first.session_id)?;
    assert!(events.iter().any(|event| matches!(&event.detail,crate::timeline::SessionEventKind::Compaction { compaction } if compaction.state == crate::backend::CompactionState::Unconfirmed)));

    git(&root, &["switch", "-q", "-c", "feature"]);
    let branch = generate(&resumed_manager, &root, &data).await?;
    resumed_manager.shutdown().await;
    assert_ne!(first.session_id, branch.session_id);
    git(
        &root,
        &[
            "commit",
            "-q",
            "--allow-empty",
            "--amend",
            "-m",
            "feat: rewritten",
        ],
    );
    let rewritten = generate(&resumed_manager, &root, &data).await?;
    resumed_manager.shutdown().await;
    assert_ne!(branch.session_id, rewritten.session_id);
    let store = SqliteStore::open(&data)?;
    assert_eq!(store.list_exchange(&first.session_id)?.len(), 6);
    assert!(
        store
            .load_session(&first.session_id)?
            .context("session missing")?
            .backend_session_id
            .is_some()
    );
    Ok(())
}

#[tokio::test]
async fn background_reuses_one_provider_thread_for_commits_and_pull_requests() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let root = fixture.path().join("repository");
    std::fs::create_dir(&root)?;
    let data = fixture.path().join("state");
    git(&root, &["init", "-q", "-b", "main"]);
    git(&root, &["config", "user.name", "Fixture"]);
    git(&root, &["config", "user.email", "fixture@example.test"]);
    git(
        &root,
        &["commit", "-q", "--allow-empty", "-m", "feat: initial"],
    );
    let mut identity = None;
    let purposes = [
        crate::backend::TextGeneration::CommitMessage,
        crate::backend::TextGeneration::PullRequestDescription,
        crate::backend::TextGeneration::CommitMessage,
    ];
    for (purpose, service_tier) in purposes.into_iter().zip([
        crate::backend::ServiceTier::Fast,
        crate::backend::ServiceTier::Ultrafast,
        crate::backend::ServiceTier::Standard,
    ]) {
        let owner = manager();
        let context = BackgroundContext::read(&owner.repositories, root.to_str().unwrap()).await?;
        let result = owner
            .generate(BackgroundRequest {
                purpose,
                context,
                input: "Current fixture evidence".into(),
                delivery: None,
                config: crate::service::TextGenerationConfig {
                    backend: BackendLaunch {
                        kind: "mock".into(),
                        command: vec![],
                    },
                    model: "mock-model".into(),
                    reasoning_effort: Some("low".into()),
                    service_tier,
                    data_root: data.clone(),
                    auto_compaction: Default::default(),
                },
            })
            .await?;
        owner.shutdown().await;
        let store = SqliteStore::open(&data)?;
        let session = store
            .load_session(&result.session_id)?
            .context("shared session missing")?;
        assert_eq!(
            session.service_tier, service_tier,
            "Background speed was reset or inherited"
        );
        let current = (result.session_id.clone(), session.backend_session_id);
        if let Some(previous) = &identity {
            assert_eq!(&current, previous);
        }
        identity = Some(current);
        assert_eq!(store.list_session(None)?.len(), 1);
        let exchanges = store.list_exchange(&result.session_id)?;
        let prompt = &exchanges.last().context("task exchange missing")?.prompt;
        assert_eq!(
            prompt,
            &purpose.background_input("Current fixture evidence")
        );
    }
    let session_id = identity.context("shared identity missing")?.0;
    assert_eq!(
        SqliteStore::open(&data)?.list_exchange(&session_id)?.len(),
        3
    );
    Ok(())
}

#[tokio::test]
async fn history_keeps_ten_complete_messages_and_rejects_changed_head() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    git(fixture.path(), &["init", "-q", "-b", "main"]);
    git(fixture.path(), &["config", "user.name", "Fixture"]);
    git(
        fixture.path(),
        &["config", "user.email", "fixture@example.test"],
    );
    for index in 0..12 {
        git(
            fixture.path(),
            &[
                "commit",
                "-q",
                "--allow-empty",
                "-m",
                &format!("feat: entry {index}\n\nBody {index}"),
            ],
        );
    }
    let message = fixture.path().join("long-message.txt");
    std::fs::write(
        &message,
        format!(
            "feat: preserve full history\n\n{}\nlast-history-marker",
            "history ".repeat(70000)
        ),
    )?;
    git(
        fixture.path(),
        &[
            "commit",
            "-q",
            "--allow-empty",
            "-F",
            message.to_str().unwrap(),
        ],
    );
    let store = forge_git::store::RepositoryStore::default();
    let context = BackgroundContext::read(&store, fixture.path().to_str().unwrap()).await?;
    assert_eq!(context.history.matches("Commit ").count(), 10);
    assert!(!context.history.contains("entry 0\n"));
    assert!(context.history.contains("Body 11"));
    assert!(context.history.contains("last-history-marker") && context.history.len() > 500_000);
    git(
        fixture.path(),
        &["commit", "-q", "--allow-empty", "-m", "feat: later"],
    );
    assert!(context.verify(&store).await.is_err());
    Ok(())
}
