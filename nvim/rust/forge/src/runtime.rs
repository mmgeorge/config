use std::sync::Arc;
use std::time::Duration;

use anyhow::{Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_git::store::RepositoryStore;
use forge_github::{remote::GhClient, service::GithubService};
use forge_harness::service::HarnessService;
use forge_review::service::ReviewService;

pub(crate) const SHUTDOWN_DEADLINE: Duration = Duration::from_secs(2);

pub(crate) mod commit_message;
pub(crate) mod repository_write;

/// Owns shared repository resources for one host without opening a repository at construction.
pub struct ForgeRuntime {
    pub(crate) writes: repository_write::RepositoryWriteGateway,
    pub(crate) generation: commit_message::GenerationService,
    pub(crate) text: forge_text::TextCorrectionService,
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
        let syntax = forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool());
        let status = forge_status::StatusService::new(
            Arc::clone(&repositories),
            Arc::clone(&diff),
            Arc::clone(&syntax),
            Arc::clone(&writes.writer),
        );
        let sources = forge_status::source_document::SourceDocumentService::new(
            Arc::clone(&repositories),
            Arc::clone(&diff),
            Arc::clone(&syntax),
        );
        let generation = commit_message::GenerationService::new(
            Arc::clone(&repositories),
            Arc::clone(&diff),
            Arc::clone(&syntax),
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
            forge_review::edit::EditBudget::default(),
            diff.analysis_pool(),
        );
        let github_remote = Arc::new(GhClient::new("gh".into(), std::env::current_dir()?)?);
        Ok(Self {
            writes,
            generation,
            text: forge_text::TextCorrectionService::new(),
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
        self.text.close();
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
        let (report, unfinished_analysis, storage, remote, review, issues, generation, text) = tokio::join!(
            self.repositories.shutdown(remaining),
            self.diff.shutdown(remaining),
            self.github.shutdown(remaining),
            self.github_remote.shutdown(remaining),
            self.review.shutdown(remaining),
            self.issues.shutdown(remaining),
            self.generation.shutdown(remaining),
            self.text.shutdown(remaining),
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
                && generation.is_ok()
                && text.is_ok(),
            "Forge shutdown left Harness state: {:?}, repository read jobs: {:?}, mutations: {:?}, analyses: {:?}, issue storage: {:?}, GitHub native reads: {:?}, review jobs: {:?}, issue jobs: {:?}, generation: {:?}, text: {:?}",
            harness.err(),
            report.unfinished,
            unfinished_mutations,
            unfinished_analysis,
            storage,
            remote.err(),
            review,
            issues,
            generation.err(),
            text.err()
        );
        Ok(())
    }
}
