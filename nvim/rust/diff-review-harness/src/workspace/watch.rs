use anyhow::{Context, Result};
use async_trait::async_trait;
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Duration;

/// Represents the observed filesystem state after waiting for event delivery to settle.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WatchObservation {
    pub generation: u64,
    pub healthy: bool,
    pub quiet: bool,
}

/// Defines the workspace-change signal consumed by thought checkpoint tracking.
#[async_trait]
pub trait WorkspaceWatch: Send {
    /// Return the monotonic generation advanced by workspace or provider activity.
    fn generation(&self) -> u64;

    /// Return whether the native watcher remains authoritative as a dirty signal.
    fn healthy(&self) -> bool;

    /// Mark a provider-reported mutation without waiting for native event delivery.
    fn mark_provider_change(&self);

    /// Wait for one quiet interval, bounded by a maximum thought-close delay.
    async fn wait_for_quiet(
        &self,
        quiet_duration: Duration,
        maximum_duration: Duration,
    ) -> WatchObservation;
}

#[derive(Default)]
struct WatchSignal {
    generation: AtomicU64,
    healthy: AtomicBool,
}

impl WatchSignal {
    fn changed(&self) {
        self.generation.fetch_add(1, Ordering::AcqRel);
    }

    fn failed(&self) {
        self.healthy.store(false, Ordering::Release);
        self.changed();
    }
}

/// Owns one recursive native watcher for an active Harness interaction.
pub struct NotifyWorkspaceWatch {
    _watcher: RecommendedWatcher,
    signal: Arc<WatchSignal>,
}

impl NotifyWorkspaceWatch {
    /// Start a recursive watcher before the interaction baseline is captured.
    pub fn start(workspace: &Path) -> Result<Self> {
        let signal = Arc::new(WatchSignal::default());
        signal.healthy.store(true, Ordering::Release);
        let callback_signal = Arc::clone(&signal);
        let git_metadata = workspace.join(".git");
        let mut watcher = notify::recommended_watcher(
            move |result: notify::Result<notify::Event>| match result {
                Ok(event) => {
                    if event.paths.is_empty()
                        || event
                            .paths
                            .iter()
                            .any(|path| !path.starts_with(&git_metadata))
                    {
                        callback_signal.changed();
                    }
                }
                Err(_error) => callback_signal.failed(),
            },
        )
        .context("create workspace watcher")?;
        watcher
            .watch(workspace, RecursiveMode::Recursive)
            .with_context(|| format!("watch workspace {}", workspace.display()))?;
        Ok(Self {
            _watcher: watcher,
            signal,
        })
    }
}

#[async_trait]
impl WorkspaceWatch for NotifyWorkspaceWatch {
    fn generation(&self) -> u64 {
        self.signal.generation.load(Ordering::Acquire)
    }

    fn healthy(&self) -> bool {
        self.signal.healthy.load(Ordering::Acquire)
    }

    fn mark_provider_change(&self) {
        self.signal.changed();
    }

    async fn wait_for_quiet(
        &self,
        quiet_duration: Duration,
        maximum_duration: Duration,
    ) -> WatchObservation {
        let started = tokio::time::Instant::now();
        let mut generation = self.generation();
        loop {
            let elapsed = started.elapsed();
            if elapsed >= maximum_duration {
                return WatchObservation {
                    generation: self.generation(),
                    healthy: self.healthy(),
                    quiet: false,
                };
            }
            tokio::time::sleep(quiet_duration.min(maximum_duration - elapsed)).await;
            let next_generation = self.generation();
            if next_generation == generation {
                return WatchObservation {
                    generation: next_generation,
                    healthy: self.healthy(),
                    quiet: true,
                };
            }
            generation = next_generation;
        }
    }
}

#[cfg(test)]
mod test {
    use super::{NotifyWorkspaceWatch, WorkspaceWatch};
    use anyhow::{Context, Result};
    use std::fs;
    use std::path::Path;
    use std::process::Command;
    use std::sync::Arc;
    use std::time::Duration;

    fn git(workspace: &Path, arguments: &[&str]) -> Result<()> {
        let output = Command::new("git")
            .args(arguments)
            .current_dir(workspace)
            .output()
            .with_context(|| format!("run git {}", arguments.join(" ")))?;
        anyhow::ensure!(
            output.status.success(),
            "git {} failed: {}",
            arguments.join(" "),
            String::from_utf8_lossy(&output.stderr)
        );
        Ok(())
    }

    async fn wait_for_change(watcher: &NotifyWorkspaceWatch, generation: u64) -> Result<u64> {
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let next_generation = watcher.generation();
                if next_generation > generation {
                    return next_generation;
                }
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
        })
        .await
        .context("workspace watcher did not report a change")
    }

    #[tokio::test]
    async fn watches_files_created_immediately_in_new_nested_directories() -> Result<()> {
        let repository = tempfile::tempdir()?;
        git(repository.path(), &["init", "-q"])?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        let generation = watcher.generation();
        let nested = repository.path().join("apps/new/deep/module");
        fs::create_dir_all(&nested)?;
        fs::write(nested.join("lib.rs"), "pub fn nested() {}\n")?;
        let changed_generation = wait_for_change(&watcher, generation).await?;
        let observation = watcher
            .wait_for_quiet(Duration::from_millis(20), Duration::from_millis(500))
            .await;
        assert!(observation.quiet);
        assert!(observation.healthy);
        assert!(observation.generation >= changed_generation);
        Ok(())
    }

    #[tokio::test]
    async fn detects_rename_delete_atomic_replace_and_bursts() -> Result<()> {
        let repository = tempfile::tempdir()?;
        git(repository.path(), &["init", "-q"])?;
        let original = repository.path().join("original.txt");
        fs::write(&original, "original\n")?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        let generation = watcher.generation();
        let renamed = repository.path().join("renamed.txt");
        fs::rename(&original, &renamed)?;
        let temporary = repository.path().join("replacement.tmp");
        fs::write(&temporary, "replacement\n")?;
        fs::rename(&temporary, &renamed)?;
        for index in 0..100 {
            fs::write(
                repository.path().join(format!("burst-{index}.txt")),
                "value\n",
            )?;
        }
        fs::remove_file(&renamed)?;
        wait_for_change(&watcher, generation).await?;
        let observation = watcher
            .wait_for_quiet(Duration::from_millis(20), Duration::from_secs(1))
            .await;
        assert!(observation.quiet);
        assert!(observation.healthy);
        Ok(())
    }

    #[tokio::test]
    async fn provider_changes_advance_the_same_generation() -> Result<()> {
        let repository = tempfile::tempdir()?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        let generation = watcher.generation();
        watcher.mark_provider_change();
        assert_eq!(watcher.generation(), generation + 1);
        Ok(())
    }

    #[tokio::test]
    async fn ignores_git_metadata_churn() -> Result<()> {
        let repository = tempfile::tempdir()?;
        git(repository.path(), &["init", "-q"])?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        let generation = watcher.generation();
        fs::write(
            repository.path().join(".git/harness-watch-test"),
            "metadata\n",
        )?;
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert_eq!(watcher.generation(), generation);
        Ok(())
    }

    #[tokio::test]
    async fn quiet_wait_stops_at_the_maximum_when_events_keep_arriving() -> Result<()> {
        let repository = tempfile::tempdir()?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        let signal = Arc::clone(&watcher.signal);
        let producer = tokio::spawn(async move {
            for _ in 0..20 {
                signal.changed();
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
        });
        let observation = watcher
            .wait_for_quiet(Duration::from_millis(20), Duration::from_millis(50))
            .await;
        producer.await?;
        assert!(!observation.quiet);
        assert!(observation.healthy);
        Ok(())
    }

    #[tokio::test]
    async fn watcher_errors_force_the_checkpoint_fallback() -> Result<()> {
        let repository = tempfile::tempdir()?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        watcher.signal.failed();
        let observation = watcher
            .wait_for_quiet(Duration::from_millis(1), Duration::from_millis(10))
            .await;
        assert!(!observation.healthy);
        assert!(observation.generation > 0);
        Ok(())
    }

    #[tokio::test]
    async fn dropping_the_watcher_stops_future_observation() -> Result<()> {
        let repository = tempfile::tempdir()?;
        let watcher = NotifyWorkspaceWatch::start(repository.path())?;
        let signal = std::sync::Arc::clone(&watcher.signal);
        drop(watcher);
        let generation = signal.generation.load(std::sync::atomic::Ordering::Acquire);
        fs::write(repository.path().join("after-drop.txt"), "value\n")?;
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert_eq!(
            signal.generation.load(std::sync::atomic::Ordering::Acquire),
            generation
        );
        Ok(())
    }
}
