use std::sync::Arc;
use std::time::Duration;

use anyhow::{Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_git::store::RepositoryStore;
use forge_github::{remote::GhClient, service::GithubService};
use forge_harness::service::HarnessService;
use forge_review::service::ReviewService;

pub(crate) const SHUTDOWN_DEADLINE: Duration = Duration::from_secs(2);

#[path = "commit_message/mod.rs"]
pub(crate) mod commit_message;
#[path = "repository_write.rs"]
pub(crate) mod repository_write;

/// Owns shared repository resources for one host without opening a repository at construction.
pub struct ForgeRuntime {
    pub(crate) writes: repository_write::RepositoryWriteGateway,
    pub(crate) generation: commit_message::CommitMessageService,
    pub(crate) syntax: Arc<forge_diff::syntax::SyntaxEngine>,
    pub(crate) status: forge_status::StatusService,
    pub(crate) sources: forge_status::source_document::SourceDocumentService,
    pub(crate) walkthrough: forge_review::walkthrough::WalkthroughService,
    pub(crate) notifications: forge_review::notification::NotificationDocumentService,
    pub(crate) issues: forge_review::issue_service::IssueDocumentService,
    pub repositories: Arc<RepositoryStore>,
    pub diff: Arc<DiffEngine>,
    pub harness: HarnessService,
    pub github: GithubService,
    pub review: ReviewService,
    pub github_remote: Arc<GhClient>,
    pub requests: Arc<crate::host::RequestTaskStore>,
}

impl ForgeRuntime {
    pub fn new() -> Result<Self> {
        let repositories = Arc::new(RepositoryStore::default());
        let writes = repository_write::RepositoryWriteGateway::new(Arc::clone(&repositories))?;
        let diff = DiffEngine::with_cache(Arc::clone(&repositories.analysis), 4);
        let syntax = forge_diff::syntax::SyntaxEngine::new(
            diff.analysis_pool(),
            forge_diff::syntax::SyntaxLimits::default(),
        );
        let status = forge_status::StatusService::new(
            Arc::clone(&repositories),
            Arc::clone(&diff),
            Arc::clone(&syntax),
            Arc::clone(&writes.writer),
        );
        let sources = forge_status::source_document::SourceDocumentService::new(
            Arc::clone(&repositories),
            Arc::clone(&syntax),
        );
        let generation = commit_message::CommitMessageService::new(
            Arc::clone(&repositories),
            Arc::clone(&diff),
        )?;
        let harness = HarnessService::new(
            Arc::clone(&repositories),
            Arc::clone(&diff),
            Arc::clone(&syntax),
        );
        let github = GithubService::default();
        let review =
            ReviewService::with_engines(github.clone(), Arc::clone(&diff), Arc::clone(&syntax));
        let walkthrough =
            forge_review::walkthrough::WalkthroughService::new(Arc::clone(&repositories));
        let notifications =
            forge_review::notification::NotificationDocumentService::new(github.clone());
        let issues = forge_review::issue_service::IssueDocumentService::new(
            github.clone(),
            forge_review::edit::EditBudget::new(32 * 1024 * 1024)?,
            diff.analysis_pool(),
        );
        let github_remote = Arc::new(GhClient::new("gh".into(), std::env::current_dir()?)?);
        Ok(Self {
            writes,
            generation,
            syntax,
            status,
            sources,
            walkthrough,
            notifications,
            issues,
            repositories,
            diff,
            harness,
            github,
            review,
            github_remote,
            requests: Arc::new(crate::host::RequestTaskStore::default()),
        })
    }

    /// Quiesces Harness, closes shared read admission, and reports owners remaining at the deadline.
    pub async fn shutdown(&self) -> Result<()> {
        self.writes.close();
        self.generation.close();
        self.status.close_all();
        self.sources.close_all();
        self.walkthrough.close_all();
        self.notifications.close_all();
        self.issues.close();
        self.syntax.close();
        let deadline = tokio::time::Instant::now() + SHUTDOWN_DEADLINE;
        self.review.close();
        self.github.close();
        let harness = self.harness.shutdown(SHUTDOWN_DEADLINE).await;
        let remaining = deadline.saturating_duration_since(tokio::time::Instant::now());
        self.diff.close();
        let (report, unfinished_analysis, storage, remote, review, issues, generation) = tokio::join!(
            self.repositories.shutdown(remaining),
            self.diff.shutdown(remaining),
            self.github.shutdown(remaining),
            self.github_remote.shutdown(remaining),
            self.review.shutdown(remaining),
            self.issues.shutdown(remaining),
            self.generation.shutdown(remaining),
        );
        let unfinished_mutations = self.repositories.writes.close();
        self.repositories.evict_idle();
        ensure!(
            harness.is_ok()
                && report.unfinished.is_empty()
                && unfinished_mutations.is_empty()
                && unfinished_analysis.unfinished_jobs == 0
                && unfinished_analysis.unfinished_workers == 0
                && storage.unfinished_jobs == 0
                && storage.failed_jobs == 0
                && review.unfinished_jobs == 0
                && review.failed_jobs == 0
                && issues.unfinished_jobs == 0
                && issues.failed_jobs == 0
                && remote.is_ok()
                && generation.is_ok(),
            "Forge shutdown left Harness state: {:?}, repository read jobs: {:?}, mutations: {:?}, analyses: {:?}, issue storage: {:?}, GitHub native reads: {:?}, review jobs: {:?}, issue jobs: {:?}, generation: {:?}",
            harness.err(),
            report.unfinished,
            unfinished_mutations,
            unfinished_analysis,
            storage,
            remote.err(),
            review,
            issues,
            generation.err()
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construction_does_not_require_an_executor_or_repository() {
        let runtime = ForgeRuntime::new().unwrap();
        assert_eq!(runtime.repositories.evict_idle(), 0);
        assert_eq!(runtime.diff.usage().active_jobs, 0);
        assert_eq!(runtime.diff.usage().cache.admitted_entries, 0);
        assert_eq!(runtime.diff.usage().pool.live_workers, 0);
    }

    #[tokio::test]
    async fn shutdown_closes_discovery_admission() {
        let runtime = ForgeRuntime::new().unwrap();
        runtime.shutdown().await.unwrap();
        assert!(
            runtime
                .harness
                .prepare(None, forge_harness::protocol::HarnessMethod::StateGet)
                .await
                .is_err()
        );
        let empty = forge_diff::source::SourceVersion::new(
            Vec::new(),
            forge_diff::source::Representation::Raw,
        )
        .unwrap();
        assert!(matches!(
            runtime
                .diff
                .compare(forge_diff::engine::DiffRequest {
                    source: forge_diff::source::SourcePair {
                        old: empty.clone(),
                        new: empty
                    },
                    priority: forge_diff::workers::WorkPriority::Foreground,
                })
                .await,
            Err(forge_diff::engine::EngineError::Closed)
        ));
        let directory = tempfile::tempdir().unwrap();
        let database = directory.path().join("issues.redb");
        let store = forge_github::issue_store::IssueStore::new(
            database.clone(),
            "owner/repo",
            Duration::ZERO,
        )
        .unwrap();
        assert!(
            runtime
                .github
                .execute(store, forge_github::service::IssueOperation::State {})
                .await
                .is_err()
        );
        assert!(!database.exists());
        assert!(
            runtime
                .repositories
                .open(directory.path().to_owned())
                .await
                .is_err()
        );
    }
}
