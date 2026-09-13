use std::process::Command;
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use forge_git::command::{CommandLimits, read_command};
use serde::Serialize;
use serde_json::Value;

use super::{GhDirectory, classify_failure, diagnostic};
use crate::model::GithubRepositoryId;
use crate::remote::{RemoteFailure, RemoteFailureKind};

#[derive(Debug, Serialize)]
pub struct CreationContext {
    pub repository: GithubRepositoryId,
    pub repository_node_id: String,
    pub branch: String,
    pub head_commit: String,
}

impl GhDirectory {
    pub async fn creation_context(&self) -> Result<CreationContext> {
        let (repository, repository_node_id) = self
            .client
            .run_native(
                self.directory.clone(),
                ["repo", "view", "--json", "id,nameWithOwner,url"]
                    .into_iter()
                    .map(Into::into)
                    .collect(),
                None,
                4096,
                |output| {
                    if !output.status.success() {
                        return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
                            &output.stderr,
                        ))));
                    }
                    decode_repository(&output.stdout).map_err(|failure| RemoteFailure {
                        kind: RemoteFailureKind::InvalidResponse,
                        message: failure.to_string(),
                    })
                },
            )
            .await??;
        let directory = self.directory.clone();
        let (branch, head_commit) = self
            .client
            .requests
            .submit(
                directory.as_os_str().len() + 16 * 1024,
                move |cancellation| {
                    let read = |argument: &[&str]| -> Result<String> {
                        let mut command = Command::new("git");
                        command.current_dir(&directory).args(argument);
                        #[cfg(windows)]
                        {
                            use std::os::windows::process::CommandExt;
                            command.creation_flags(0x08000000);
                        }
                        let output = read_command(
                            &mut command,
                            CommandLimits {
                                stdout_bytes: 4096,
                                stderr_bytes: 8192,
                                timeout: Duration::from_secs(30),
                            },
                            || cancellation.check(),
                        )?;
                        ensure!(
                            output.status.success(),
                            "creation Git context failed: {}",
                            String::from_utf8_lossy(&output.stderr)
                        );
                        Ok(String::from_utf8(output.stdout)?
                            .trim_end_matches(['\r', '\n'])
                            .to_owned())
                    };
                    let branch = read(&["symbolic-ref", "--quiet", "--short", "HEAD"])?;
                    let head = read(&["rev-parse", "--verify", "HEAD"])?;
                    ensure!(
                        read(&["symbolic-ref", "--quiet", "--short", "HEAD"])? == branch,
                        "creation branch changed during context capture"
                    );
                    ensure!(
                        !branch.is_empty()
                            && branch.len() <= 1024
                            && !branch.chars().any(char::is_control),
                        "invalid creation branch"
                    );
                    ensure!(
                        matches!(head.len(), 40 | 64)
                            && head.bytes().all(|byte| byte.is_ascii_hexdigit()),
                        "invalid creation head commit"
                    );
                    Ok((branch, head))
                },
            )?
            .finish()
            .await?;
        Ok(CreationContext {
            repository,
            repository_node_id,
            branch,
            head_commit,
        })
    }
}

fn decode_repository(bytes: &[u8]) -> Result<(GithubRepositoryId, String)> {
    let value: Value = serde_json::from_slice(bytes)?;
    let name = value
        .get("nameWithOwner")
        .and_then(Value::as_str)
        .context("creation repository name is missing")?;
    let (owner, name) = name
        .split_once('/')
        .context("creation repository owner is missing")?;
    let url = value
        .get("url")
        .and_then(Value::as_str)
        .context("creation repository URL is missing")?;
    let hostname = url
        .strip_prefix("https://")
        .and_then(|url| url.split('/').next())
        .context("creation repository URL is invalid")?;
    let repository = GithubRepositoryId::new(hostname, owner, name).map_err(anyhow::Error::msg)?;
    let node = value
        .get("id")
        .and_then(Value::as_str)
        .filter(|node| {
            !node.is_empty()
                && node.len() <= 256
                && node.bytes().all(|byte| byte.is_ascii_graphic())
        })
        .context("creation repository node is missing")?;
    Ok((repository, node.into()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creation_repository_uses_explicit_enterprise_identity() {
        let (repository, node) = decode_repository(br#"{"id":"R_test","nameWithOwner":"Owner/Repo","url":"https://enterprise.example/Owner/Repo"}"#).unwrap();
        assert_eq!(repository.hostname(), "enterprise.example");
        assert_eq!(repository.repository_name(), "owner/repo");
        assert_eq!(node, "R_test");
        assert!(
            decode_repository(
                br#"{"id":"R_test","nameWithOwner":"Owner/Repo","url":"file:///tmp/repo"}"#
            )
            .is_err()
        );
    }
}
