use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result, ensure};
use forge_buffer::identity::TargetId;
use forge_git::read_pool::ReadCancellation;
use forge_git::store::RepositoryStore;
use serde::{Deserialize, Serialize};

pub struct StatusContextService {
    repository: Arc<RepositoryStore>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ContextCommit {
    pub oid: String,
    pub reference: String,
    pub subject: String,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusContext {
    pub workspace: PathBuf,
    pub head: Option<ContextCommit>,
    pub branch: Option<String>,
    pub upstream: Option<ContextCommit>,
    pub push: Option<ContextCommit>,
    pub recent: Vec<ContextCommit>,
    pub issues: Vec<u64>,
    pub branch_prefix: Option<String>,
    #[serde(skip)]
    pub config_source: Option<Vec<u8>>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ContextAction {
    Commit { oid: String },
    PullRequest,
    About,
    Issues { text: String },
}

impl StatusContextService {
    pub fn new(repository: Arc<RepositoryStore>) -> Self {
        Self { repository }
    }

    pub async fn collect(&self, workspace: PathBuf) -> Result<StatusContext> {
        let repository = self
            .repository
            .open(workspace)
            .await?
            .context("Status requires a Git worktree")?;
        self.collect_repository(repository).await
    }

    pub async fn collect_repository(
        &self,
        repository: Arc<forge_git::repository::RepositoryState>,
    ) -> Result<StatusContext> {
        let workspace = repository
            .identity
            .worktree_root
            .clone()
            .context("Status requires a worktree root")?;
        let generation = repository.generation();
        let bytes = workspace.as_os_str().as_encoded_bytes().len();
        let result = self
            .repository
            .read(
                Arc::clone(&repository),
                bytes,
                move |local, cancellation| collect_native(&local, &workspace, &cancellation),
            )
            .await?
            .value;
        ensure!(
            repository.generation() == generation,
            "Status context was superseded by a repository change"
        );
        Ok(result)
    }

    pub async fn commit_message(&self, workspace: PathBuf, oid: String) -> Result<String> {
        ensure!(
            (oid.len() == 40 || oid.len() == 64)
                && oid.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "commit message requires an exact object identity"
        );
        let repository = self
            .repository
            .open(workspace)
            .await?
            .context("commit message requires a Git repository")?;
        let bytes = oid.len();
        self.repository
            .read(repository, bytes, move |local, cancellation| {
                cancellation.check()?;
                let oid = gix::ObjectId::from_hex(oid.as_bytes())?;
                let commit = local.find_object(oid)?.try_into_commit()?;
                cancellation.check()?;
                Ok(String::from_utf8_lossy(commit.message_raw()?.as_ref())
                    .trim_end_matches(['\r', '\n'])
                    .to_owned())
            })
            .await
            .map(|result| result.value)
    }
}

impl StatusContext {
    pub fn issues_replacement(&self, text: &str) -> Result<Vec<u8>> {
        ensure!(text.len() <= 65536, "issue edit exceeds 64 KiB");
        let mut numbers = Vec::new();
        for token in text
            .split(|character: char| !character.is_ascii_digit())
            .filter(|token| !token.is_empty())
        {
            let number: u64 = token
                .parse()
                .context("issue number exceeds integer capacity")?;
            ensure!(
                number <= forge_buffer::MAX_COUNTER,
                "issue number exceeds exact editor integer range"
            );
            if number > 0 {
                numbers.push(number);
            }
            ensure!(numbers.len() <= 1024, "issue edit exceeds 1024 references");
        }
        numbers.sort_unstable();
        numbers.dedup();
        let mut config: serde_json::Value = match self.config_source.as_deref() {
            Some(bytes) if !bytes.iter().all(u8::is_ascii_whitespace) => {
                serde_json::from_slice(bytes).context("invalid saved .forge.json")?
            }
            _ => serde_json::json!({}),
        };
        let object = config
            .as_object_mut()
            .context(".forge.json must contain an object")?;
        object.insert("issues".into(), serde_json::to_value(numbers)?);
        let mut replacement = serde_json::to_vec_pretty(&config)?;
        replacement.push(b'\n');
        ensure!(
            replacement.len() <= 1024 * 1024,
            "repository config replacement exceeds 1 MiB"
        );
        Ok(replacement)
    }

    pub fn action(&self, target: &TargetId) -> Option<ContextAction> {
        match target.0.as_str() {
            "status:context:head" => self.head.as_ref().map(|commit| ContextAction::Commit {
                oid: commit.oid.clone(),
            }),
            "status:context:upstream" => {
                self.upstream.as_ref().map(|commit| ContextAction::Commit {
                    oid: commit.oid.clone(),
                })
            }
            "status:context:push" => self.push.as_ref().map(|commit| ContextAction::Commit {
                oid: commit.oid.clone(),
            }),
            "status:context:pr" => Some(ContextAction::PullRequest),
            "status:context:about" => Some(ContextAction::About),
            "status:context:issues" => Some(ContextAction::Issues {
                text: self
                    .issues
                    .iter()
                    .map(|number| format!("#{number}"))
                    .collect::<Vec<_>>()
                    .join(" "),
            }),
            _ => target
                .0
                .strip_prefix("status:context:recent:")
                .and_then(|oid| self.recent.iter().find(|commit| commit.oid == oid))
                .map(|commit| ContextAction::Commit {
                    oid: commit.oid.clone(),
                }),
        }
    }
}

fn collect_native(
    repository: &gix::Repository,
    workspace: &Path,
    cancellation: &ReadCancellation,
) -> Result<StatusContext> {
    cancellation.check()?;
    let head = repository.head().context("read Status context HEAD")?;
    let branch = head.referent_name().map(|name| {
        String::from_utf8_lossy(
            name.as_bstr()
                .strip_prefix(b"refs/heads/")
                .unwrap_or(name.as_bstr()),
        )
        .into_owned()
    });
    let upstream = tracking_commit(
        repository,
        head.referent_name(),
        gix::remote::Direction::Fetch,
    )?;
    let push = tracking_commit(
        repository,
        head.referent_name(),
        gix::remote::Direction::Push,
    )?;
    let head_id = head.id().map(|id| id.detach());
    let head = head_id
        .map(|id| context_commit(repository, id, branch.as_deref().unwrap_or("(detached)")))
        .transpose()?;
    let mut recent = Vec::new();
    let mut next = head_id;
    while let Some(id) = next.filter(|_| recent.len() < 5) {
        cancellation.check()?;
        let commit = repository.find_object(id)?.try_into_commit()?;
        recent.push(ContextCommit {
            oid: id.to_string(),
            subject: subject(&commit)?,
            reference: relative_time(commit.time()?.seconds),
        });
        next = commit.parent_ids().next().map(|parent| parent.detach());
    }
    cancellation.check()?;
    let config_source = read_config(workspace)?;
    let config: serde_json::Value = match config_source.as_deref() {
        Some(bytes) if !bytes.iter().all(u8::is_ascii_whitespace) => {
            serde_json::from_slice(bytes).context("invalid .forge.json")?
        }
        _ => serde_json::json!({}),
    };
    ensure!(config.is_object(), ".forge.json must contain an object");
    let mut issues = Vec::new();
    if let Some(values) = config.get("issues").and_then(serde_json::Value::as_array) {
        ensure!(values.len() <= 1024, "issue list exceeds 1024 entries");
        for value in values {
            if let Some(number) = value
                .as_u64()
                .or_else(|| value.as_str().and_then(|text| text.parse().ok()))
                .filter(|number| *number > 0 && *number <= forge_buffer::MAX_COUNTER)
            {
                issues.push(number);
            }
        }
        issues.sort_unstable();
        issues.dedup();
    }
    let branch_prefix = config
        .get("branch_prefix")
        .and_then(serde_json::Value::as_str)
        .filter(|value| !value.is_empty())
        .map(str::to_owned);
    ensure!(
        branch_prefix
            .as_ref()
            .is_none_or(|value| value.len() <= 4096),
        "branch prefix exceeds capacity"
    );
    Ok(StatusContext {
        workspace: workspace.to_owned(),
        head,
        branch,
        upstream,
        push,
        recent,
        issues,
        branch_prefix,
        config_source,
    })
}

fn tracking_commit(
    repository: &gix::Repository,
    branch: Option<&gix::refs::FullNameRef>,
    direction: gix::remote::Direction,
) -> Result<Option<ContextCommit>> {
    let Some(branch) = branch else {
        return Ok(None);
    };
    let Some(name) = repository
        .branch_remote_tracking_ref_name(branch, direction)
        .transpose()?
    else {
        return Ok(None);
    };
    let Some(reference) = repository.try_find_reference(name.as_ref())? else {
        return Ok(None);
    };
    let Some(oid) = reference.try_id().map(|id| id.detach()) else {
        return Ok(None);
    };
    let full_name = name.as_bstr();
    let label = String::from_utf8_lossy(
        full_name
            .strip_prefix(b"refs/remotes/")
            .unwrap_or(full_name),
    );
    context_commit(repository, oid, &label).map(Some)
}

fn context_commit(
    repository: &gix::Repository,
    oid: gix::ObjectId,
    reference: &str,
) -> Result<ContextCommit> {
    let commit = repository.find_object(oid)?.try_into_commit()?;
    Ok(ContextCommit {
        oid: oid.to_string(),
        reference: reference.to_owned(),
        subject: subject(&commit)?,
    })
}

fn subject(commit: &gix::Commit<'_>) -> Result<String> {
    Ok(String::from_utf8_lossy(commit.message_raw()?.as_ref())
        .lines()
        .next()
        .unwrap_or_default()
        .to_owned())
}

fn relative_time(seconds: i64) -> String {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |time| time.as_secs() as i64);
    let elapsed = now.saturating_sub(seconds).max(0);
    if elapsed < 60 {
        format!("{elapsed} seconds ago")
    } else if elapsed < 3600 {
        format!("{} minutes ago", elapsed / 60)
    } else if elapsed < 86_400 {
        format!("{} hours ago", elapsed / 3600)
    } else {
        format!("{} days ago", elapsed / 86_400)
    }
}

fn read_config(workspace: &Path) -> Result<Option<Vec<u8>>> {
    let path = workspace.join(".forge.json");
    let metadata = match std::fs::symlink_metadata(&path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error).context("inspect .forge.json"),
    };
    ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        ".forge.json must be a regular file"
    );
    let mut bytes = Vec::new();
    File::open(path)?
        .take(1024 * 1024 + 1)
        .read_to_end(&mut bytes)?;
    ensure!(bytes.len() <= 1024 * 1024, ".forge.json exceeds 1 MiB");
    Ok(Some(bytes))
}

#[cfg(test)]
mod tests {
    use std::process::Command;

    use super::*;

    fn git(workspace: &Path, arguments: &[&str]) {
        let status = Command::new("git")
            .arg("-C")
            .arg(workspace)
            .args(arguments)
            .output()
            .unwrap();
        assert!(
            status.status.success(),
            "{}",
            String::from_utf8_lossy(&status.stderr)
        );
    }

    #[tokio::test]
    async fn context_preserves_exact_commit_targets_and_repository_config() -> Result<()> {
        let directory = tempfile::tempdir()?;
        git(directory.path(), &["init", "--quiet"]);
        git(
            directory.path(),
            &[
                "-c",
                "user.name=Forge",
                "-c",
                "user.email=forge@example.test",
                "commit",
                "--quiet",
                "--allow-empty",
                "-m",
                "feat: exact subject",
            ],
        );
        let source =
            br##"{"issues":["12",3,3,0],"branch_prefix":"work/","preserved":{"enabled":true}}"##;
        std::fs::write(directory.path().join(".forge.json"), source)?;
        let service = StatusContextService::new(Arc::new(RepositoryStore::default()));
        let context = service.collect(directory.path().to_owned()).await?;
        assert_eq!(context.issues, vec![3, 12]);
        assert_eq!(context.branch_prefix.as_deref(), Some("work/"));
        assert_eq!(context.config_source.as_deref(), Some(source.as_slice()));
        let Some(ContextAction::Commit { oid }) =
            context.action(&TargetId("status:context:head".into()))
        else {
            panic!("missing exact commit action")
        };
        assert_eq!(oid, context.head.as_ref().unwrap().oid);
        assert_ne!(oid, "HEAD");
        assert_eq!(
            service
                .commit_message(directory.path().to_owned(), oid)
                .await?,
            "feat: exact subject"
        );
        assert!(
            service
                .commit_message(directory.path().to_owned(), "HEAD".into())
                .await
                .is_err()
        );
        assert_eq!(
            context.head.as_ref().unwrap().subject,
            "feat: exact subject"
        );
        assert_eq!(context.issues, vec![3, 12]);
        assert_eq!(context.recent.len(), 1);
        assert_eq!(context.recent[0].subject, "feat: exact subject");
        let value = serde_json::to_value(&context)?;
        assert!(value.get("config_source").is_none());
        assert!(value.get("block").is_none());
        assert!(matches!(
            context.action(&TargetId(format!(
                "status:context:recent:{}",
                context.recent[0].oid
            ))),
            Some(ContextAction::Commit { .. })
        ));
        assert!(context.action(&TargetId("unknown".into())).is_none());
        let replacement: serde_json::Value =
            serde_json::from_slice(&context.issues_replacement("Issues: #9 #4 #9")?)?;
        assert_eq!(replacement["issues"], serde_json::json!([4, 9]));
        assert_eq!(replacement["preserved"]["enabled"], true);
        assert_eq!(replacement["branch_prefix"], "work/");
        Ok(())
    }

    #[tokio::test]
    async fn context_resolves_configured_upstream_and_push_references() -> Result<()> {
        let directory = tempfile::tempdir()?;
        git(directory.path(), &["init", "--quiet"]);
        git(
            directory.path(),
            &[
                "-c",
                "user.name=Forge",
                "-c",
                "user.email=forge@example.test",
                "commit",
                "--quiet",
                "--allow-empty",
                "-m",
                "feat: tracked branch",
            ],
        );
        let branch = Command::new("git")
            .arg("-C")
            .arg(directory.path())
            .args(["branch", "--show-current"])
            .output()?;
        let branch = String::from_utf8(branch.stdout)?.trim().to_owned();
        let remote_ref = format!("refs/remotes/origin/{branch}");
        git(directory.path(), &["update-ref", &remote_ref, "HEAD"]);
        git(
            directory.path(),
            &[
                "config",
                "remote.origin.url",
                "https://example.test/origin.git",
            ],
        );
        git(
            directory.path(),
            &[
                "config",
                "remote.origin.fetch",
                "+refs/heads/*:refs/remotes/origin/*",
            ],
        );
        git(
            directory.path(),
            &["config", &format!("branch.{branch}.remote"), "origin"],
        );
        git(
            directory.path(),
            &[
                "config",
                &format!("branch.{branch}.merge"),
                &format!("refs/heads/{branch}"),
            ],
        );
        git(
            directory.path(),
            &["config", &format!("branch.{branch}.pushRemote"), "origin"],
        );
        let service = StatusContextService::new(Arc::new(RepositoryStore::default()));
        let context = service.collect(directory.path().to_owned()).await?;
        for commit in [context.upstream.as_ref(), context.push.as_ref()] {
            let commit = commit.context("configured tracking reference was not resolved")?;
            assert_eq!(commit.reference, format!("origin/{branch}"));
            assert_eq!(commit.subject, "feat: tracked branch");
        }
        Ok(())
    }

    #[tokio::test]
    async fn unborn_head_and_invalid_config_are_distinct_results() -> Result<()> {
        let directory = tempfile::tempdir()?;
        git(directory.path(), &["init", "--quiet"]);
        let service = StatusContextService::new(Arc::new(RepositoryStore::default()));
        let context = service.collect(directory.path().to_owned()).await?;
        assert!(context.head.is_none());
        assert!(context.branch.is_some());
        std::fs::write(directory.path().join(".forge.json"), b"{broken")?;
        assert!(
            service
                .collect(directory.path().to_owned())
                .await
                .unwrap_err()
                .to_string()
                .contains("invalid .forge.json")
        );
        Ok(())
    }
}
