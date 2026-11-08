use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use anyhow::{Context, Result, bail, ensure};
use forge_git::command::{CommandLimits, read_command};
use serde::Serialize;
use serde_json::{Value, json};

use super::{ApiRequest, GithubClient};
use crate::model::GithubRepositoryId;

const REPOSITORY_QUERY: &str = "query($owner:String!,$name:String!){repository(owner:$owner,name:$name){id nameWithOwner url}}";

#[derive(Debug, Serialize)]
pub struct CreationContext {
    pub repository: GithubRepositoryId,
    pub repository_node_id: String,
    pub branch: String,
    pub head_commit: String,
}

/// The checkout's GitHub repository as its selected Git remote names it.
#[derive(Debug, Eq, PartialEq)]
struct RemoteRepository {
    hostname: String,
    owner: String,
    name: String,
}

impl GithubClient {
    /// Captures the repository, branch, and head commit that a new pull request starts from.
    pub async fn creation_context(&self, directory: PathBuf) -> Result<CreationContext> {
        let checkout = std::path::absolute(directory)?;
        let git_directory = checkout.clone();
        let (remote, branch, head_commit) =
            tokio::task::spawn_blocking(move || read_checkout(&git_directory)).await??;
        let payload = json!({"query":REPOSITORY_QUERY,"variables":{
            "owner":remote.owner,"name":remote.name
        }});
        let response = self
            .send(
                ApiRequest::graphql(&remote.hostname, &payload),
                |response| response.into_json::<Value>(),
            )
            .await??;
        let (repository, repository_node_id) =
            decode_repository(response.pointer("/data/repository").unwrap_or(&Value::Null))
                .with_context(|| format!("resolve GitHub repository for {}", checkout.display()))?;
        Ok(CreationContext {
            repository,
            repository_node_id,
            branch,
            head_commit,
        })
    }
}

fn read_checkout(directory: &Path) -> Result<(RemoteRepository, String, String)> {
    let read = |argument: &[&str]| -> Result<String> {
        let mut command = Command::new("git");
        command.current_dir(directory).args(argument);
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            command.creation_flags(0x08000000);
        }
        let output = read_command(
            &mut command,
            CommandLimits {
                stderr_bytes: 8192,
                timeout: Duration::from_secs(30),
            },
            || Ok(()),
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
    let remote = select_remote(&read(&[
        "config",
        "--get-regexp",
        r"^remote\..*\.(url|gh-resolved)$",
    ])?)?;
    let branch = read(&["symbolic-ref", "--quiet", "--short", "HEAD"])?;
    let head = read(&["rev-parse", "--verify", "HEAD"])?;
    ensure!(
        read(&["symbolic-ref", "--quiet", "--short", "HEAD"])? == branch,
        "creation branch changed during context capture"
    );
    ensure!(
        !branch.is_empty() && !branch.chars().any(char::is_control),
        "invalid creation branch"
    );
    ensure!(
        matches!(head.len(), 40 | 64) && head.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "invalid creation head commit"
    );
    Ok((remote, branch, head))
}

/// Selects the remote `gh` would treat as the base repository.
///
/// A remote that `gh` marked with `gh-resolved = base` wins. Otherwise remotes rank as `gh`
/// ranks them: `upstream`, then `github`, then `origin`, then the rest in configuration order.
fn select_remote(configuration: &str) -> Result<RemoteRepository> {
    let mut remote_url: Vec<(String, String)> = Vec::new();
    let mut resolved_base = None;
    for line in configuration.lines() {
        let Some((key, value)) = line.split_once(' ') else {
            continue;
        };
        let Some(key) = key.strip_prefix("remote.") else {
            continue;
        };
        if let Some(remote) = key.strip_suffix(".url") {
            if !remote_url.iter().any(|(name, _)| name == remote) {
                remote_url.push((remote.to_owned(), value.trim().to_owned()));
            }
        } else if let Some(remote) = key.strip_suffix(".gh-resolved")
            && value.trim() == "base"
        {
            resolved_base = Some(remote.to_owned());
        }
    }
    let rank = |name: &str| match name {
        _ if resolved_base.as_deref() == Some(name) => 0,
        "upstream" => 1,
        "github" => 2,
        "origin" => 3,
        _ => 4,
    };
    remote_url.sort_by_key(|(name, _)| rank(name));
    remote_url
        .iter()
        .find_map(|(_, url)| parse_remote_url(url))
        .context("checkout has no GitHub remote")
}

/// Parses `https://host/owner/name`, `ssh://user@host[:port]/owner/name`, and scp-style
/// `user@host:owner/name` remote URLs, with or without a `.git` suffix.
fn parse_remote_url(url: &str) -> Option<RemoteRepository> {
    let (authority, path) = if let Some((scheme, rest)) = url.split_once("://") {
        if !matches!(scheme, "https" | "http" | "ssh" | "git+ssh" | "ssh+git") {
            return None;
        }
        let (authority, path) = rest.split_once('/')?;
        let host = authority.rsplit('@').next()?;
        let host = if scheme.contains("ssh") {
            host.split(':').next()?
        } else {
            host
        };
        (host, path)
    } else {
        let (authority, path) = url.split_once(':')?;
        if authority.contains('/') {
            return None;
        }
        (authority.rsplit('@').next()?, path)
    };
    let path = path.trim_end_matches('/');
    let path = path.strip_suffix(".git").unwrap_or(path);
    let (owner, name) = path.split_once('/')?;
    if authority.is_empty() || owner.is_empty() || name.is_empty() || name.contains('/') {
        return None;
    }
    Some(RemoteRepository {
        hostname: authority.to_ascii_lowercase(),
        owner: owner.into(),
        name: name.into(),
    })
}

fn decode_repository(value: &Value) -> Result<(GithubRepositoryId, String)> {
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
    let Some(node) = value.get("id").and_then(Value::as_str).filter(|node| {
        !node.is_empty() && node.len() <= 256 && node.bytes().all(|byte| byte.is_ascii_graphic())
    }) else {
        bail!("creation repository node is missing");
    };
    Ok((repository, node.into()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn remote(hostname: &str, owner: &str, name: &str) -> RemoteRepository {
        RemoteRepository {
            hostname: hostname.into(),
            owner: owner.into(),
            name: name.into(),
        }
    }

    #[test]
    fn parses_remote_url_forms() {
        let expected = remote("github.com", "owner", "repo");
        for url in [
            "https://github.com/owner/repo.git",
            "https://user@github.com/owner/repo",
            "ssh://git@github.com:22/owner/repo.git",
            "git@github.com:owner/repo.git",
            "github.com:owner/repo/",
        ] {
            assert_eq!(
                parse_remote_url(url),
                Some(remote("github.com", "owner", "repo")),
                "{url}"
            );
        }
        assert_eq!(
            parse_remote_url("https://GitHub.com/owner/repo"),
            Some(expected)
        );
        assert_eq!(parse_remote_url("/local/path/repo"), None);
        assert_eq!(parse_remote_url("file:///owner/repo"), None);
        assert_eq!(parse_remote_url("https://github.com/owner"), None);
    }

    #[test]
    fn selects_resolved_base_then_gh_remote_rank() {
        let configuration = "remote.origin.url git@github.com:fork/repo.git\n\
            remote.upstream.url https://github.com/base/repo\n\
            remote.mirror.url https://example.com/mirror/repo";
        assert_eq!(
            select_remote(configuration).unwrap(),
            remote("github.com", "base", "repo")
        );
        let resolved = format!("{configuration}\nremote.mirror.gh-resolved base");
        assert_eq!(
            select_remote(&resolved).unwrap(),
            remote("example.com", "mirror", "repo")
        );
        assert!(select_remote("").is_err());
    }
}
