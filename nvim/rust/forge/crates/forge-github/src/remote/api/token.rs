use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use forge_git::command::{CommandLimits, read_command};

use super::{RequestNotSent, diagnostic};
use crate::remote::RemoteFailureKind;

type HostToken = Arc<tokio::sync::Mutex<Option<Arc<str>>>>;

/// Caches one GitHub token per host, read from `gh auth token`.
///
/// `gh` resolves `GH_TOKEN`, `GH_ENTERPRISE_TOKEN`, and its own keyring, so Forge inherits the
/// user's existing login without reading credential storage itself. Each host's slot is locked
/// across its fetch, so concurrent requests to one host launch `gh` once.
pub(super) struct TokenStore {
    gh_executable: PathBuf,
    host_token_map: Mutex<HashMap<String, HostToken>>,
}

impl TokenStore {
    pub(super) fn new(gh_executable: PathBuf) -> Self {
        Self {
            gh_executable,
            host_token_map: Mutex::default(),
        }
    }

    pub(super) async fn token(&self, hostname: &str) -> Result<Arc<str>, RequestNotSent> {
        let slot = self.slot(hostname);
        let mut cached = slot.lock().await;
        if let Some(token) = cached.as_ref() {
            return Ok(Arc::clone(token));
        }
        let token: Arc<str> = self.fetch(hostname).await?.into();
        *cached = Some(Arc::clone(&token));
        Ok(token)
    }

    /// Discards a token GitHub rejected, unless a concurrent request already replaced it.
    pub(super) async fn invalidate(&self, hostname: &str, rejected: &Arc<str>) {
        let slot = self.slot(hostname);
        let mut cached = slot.lock().await;
        if cached
            .as_ref()
            .is_some_and(|token| Arc::ptr_eq(token, rejected))
        {
            *cached = None;
        }
    }

    fn slot(&self, hostname: &str) -> HostToken {
        let mut host_token_map = self
            .host_token_map
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        Arc::clone(host_token_map.entry(hostname.to_owned()).or_default())
    }

    async fn fetch(&self, hostname: &str) -> Result<String, RequestNotSent> {
        let executable = self.gh_executable.clone();
        let host = hostname.to_owned();
        let output = tokio::task::spawn_blocking(move || {
            let mut command = Command::new(executable);
            command.args(["auth", "token", "--hostname", &host]);
            #[cfg(windows)]
            {
                use std::os::windows::process::CommandExt;
                command.creation_flags(0x08000000);
            }
            read_command(
                &mut command,
                CommandLimits {
                    stderr_bytes: 8192,
                    timeout: Duration::from_secs(30),
                },
                || Ok(()),
            )
        })
        .await;
        let unauthorized = |detail: String| RequestNotSent {
            kind: RemoteFailureKind::Unauthorized,
            message: diagnostic(&format!(
                "GitHub token for {hostname} is unavailable: {detail}; run `gh auth login --hostname {hostname}`"
            )),
        };
        let output = match output {
            Ok(Ok(output)) => output,
            Ok(Err(failure)) => return Err(unauthorized(format!("{failure:#}"))),
            Err(failure) => return Err(unauthorized(failure.to_string())),
        };
        if !output.status.success() {
            return Err(unauthorized(
                String::from_utf8_lossy(&output.stderr).trim().to_owned(),
            ));
        }
        let token = String::from_utf8(output.stdout)
            .map_err(|_| unauthorized("gh printed a non-UTF-8 token".into()))?
            .trim()
            .to_owned();
        if token.is_empty() || token.chars().any(char::is_whitespace) {
            return Err(unauthorized("gh printed no token".into()));
        }
        Ok(token)
    }
}
