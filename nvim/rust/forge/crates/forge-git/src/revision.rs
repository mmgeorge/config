use std::{ffi::OsString, process::Command, sync::Arc, time::Duration};

use anyhow::{Context, Result, ensure};

use crate::{
    RepositoryPath,
    command::{CommandLimits, read_command},
    repository::{RepositoryRead, RepositoryState},
    resolve_argument,
    store::RepositoryStore,
};

pub mod comparison;

#[derive(Clone, Debug)]
pub enum RevisionOrigin {
    Commit(gix::ObjectId),
    Index,
    Object(gix::ObjectId),
}

#[derive(Clone, Debug)]
pub struct ResolvedFile {
    pub origin: RevisionOrigin,
    pub blob: gix::ObjectId,
}

pub async fn resolve_file(
    store: &RepositoryStore,
    repository: Arc<RepositoryState>,
    reference: String,
    path: RepositoryPath,
) -> Result<RepositoryRead<ResolvedFile>> {
    ensure!(
        !reference.is_empty()
            && reference.len() <= 4096
            && !reference.chars().any(char::is_control),
        "invalid revision reference"
    );
    let path_argument = resolve_argument(&path)?;
    let input_bytes = reference.capacity() + path.retained_bytes();
    store
        .read(repository, input_bytes, move |local, cancellation| {
            if let Some(object) = reference.strip_prefix("object:") {
                let blob = gix::ObjectId::from_hex(object.as_bytes())
                    .context("invalid captured blob identity")?;
                return Ok(ResolvedFile {
                    origin: RevisionOrigin::Object(blob),
                    blob,
                });
            }
            let root = local.workdir().context("file revision requires worktree")?;
            let commit = if reference == ":0" {
                None
            } else {
                let mut command = Command::new("git");
                command
                    .args(["--no-pager", "--no-optional-locks", "-C"])
                    .arg(root)
                    .args(["rev-parse", "--verify", "--end-of-options"])
                    .arg(format!("{reference}^{{commit}}"));
                let output = read_command(
                    &mut command,
                    CommandLimits {
                        stdout_bytes: 128,
                        stderr_bytes: 4096,
                        timeout: Duration::from_secs(30),
                    },
                    || cancellation.check(),
                )?;
                ensure!(
                    output.status.success(),
                    "revision is unavailable: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
                Some(
                    gix::ObjectId::from_hex(output.stdout.trim_ascii())
                        .context("invalid resolved commit")?,
                )
            };
            let mut spec =
                OsString::from(commit.map_or_else(|| ":0:".into(), |commit| format!("{commit}:")));
            spec.push(&path_argument);
            let mut command = Command::new("git");
            command
                .args(["--no-pager", "--no-optional-locks", "-C"])
                .arg(root)
                .args(["rev-parse", "--verify", "--end-of-options"])
                .arg(spec);
            let output = read_command(
                &mut command,
                CommandLimits {
                    stdout_bytes: 128,
                    stderr_bytes: 4096,
                    timeout: Duration::from_secs(30),
                },
                || cancellation.check(),
            )?;
            ensure!(
                output.status.success(),
                "revision file is unavailable: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            let blob = gix::ObjectId::from_hex(output.stdout.trim_ascii())
                .context("invalid resolved file object")?;
            Ok(ResolvedFile {
                origin: commit.map_or(RevisionOrigin::Index, RevisionOrigin::Commit),
                blob,
            })
        })
        .await
}
