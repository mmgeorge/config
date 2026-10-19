use crate::backend::BackendKind;
use anyhow::{Context, Result, bail, ensure};
use serde::Serialize;
use std::fs::File;
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use std::time::Duration;

const CLAUDE_VERSION: &str = "0.89.1";
const ANTIGRAVITY_VERSION: &str = "1.3.0";

#[derive(Serialize)]
/// Reports installation independently of authentication and provider session state.
pub(super) struct RuntimeStatus {
    pub(super) available: bool,
    pub(super) managed: bool,
    pub(super) version: Option<String>,
    pub(super) supported_version: &'static str,
    pub(super) command: Vec<String>,
    pub(super) installable: bool,
}

/// Owns pinned ACP installations without changing global provider installations.
pub(super) struct AcpRuntime {
    kind: BackendKind,
    command: Vec<String>,
    root: PathBuf,
}

impl AcpRuntime {
    /// Scope managed runtime files to the Harness data directory.
    pub(super) fn new(kind: BackendKind, command: Vec<String>, root: &Path) -> Self {
        let name = if kind == BackendKind::Claude {
            "claude"
        } else {
            "antigravity"
        };
        Self {
            kind,
            command,
            root: root.join("provider-runtime").join(name),
        }
    }

    fn version(&self) -> &'static str {
        if self.kind == BackendKind::Claude {
            CLAUDE_VERSION
        } else {
            ANTIGRAVITY_VERSION
        }
    }

    fn installable(&self) -> bool {
        self.command.len() == 1
            && self.command[0]
                == if self.kind == BackendKind::Claude {
                    "claude-agent-acp"
                } else {
                    "agy_acp_server"
                }
    }

    fn managed_command(&self, root: &Path) -> Option<Vec<String>> {
        if self.kind == BackendKind::Claude {
            let script =
                root.join("node_modules/@agentclientprotocol/claude-agent-acp/dist/index.js");
            let node = crate::process::executable("node")?;
            script.is_file().then(|| {
                vec![
                    node.to_string_lossy().into_owned(),
                    script.to_string_lossy().into_owned(),
                ]
            })
        } else {
            let program = root.join(if cfg!(windows) {
                "agy_acp_server.exe"
            } else {
                "agy_acp_server.par"
            });
            program.is_file().then(|| {
                let mut command = vec![program.to_string_lossy().into_owned()];
                if cfg!(target_os = "linux") {
                    command.push("--uid=".into());
                }
                command
            })
        }
    }

    /// Prefer the managed pinned runtime for default commands and honor explicit overrides.
    pub(super) fn status(&self) -> RuntimeStatus {
        let installable = self.installable();
        if installable && let Some(command) = self.managed_command(&self.root.join(self.version()))
        {
            return RuntimeStatus {
                available: true,
                managed: true,
                version: Some(self.version().into()),
                supported_version: self.version(),
                command,
                installable,
            };
        }
        let mut command = self.command.clone();
        let program = command
            .first()
            .and_then(|program| crate::process::executable(program));
        if let Some(program) = &program {
            command[0] = program.to_string_lossy().into_owned();
        }
        RuntimeStatus {
            available: program.is_some(),
            managed: false,
            version: None,
            supported_version: self.version(),
            command,
            installable,
        }
    }

    /// Resolve a launch command without implicitly downloading software.
    pub(super) fn resolve(&self) -> Result<Vec<String>> {
        let status = self.status();
        ensure!(
            status.available,
            "ACP runtime is not installed. Open /config → Provider setup to install it, or configure an explicit backend command"
        );
        Ok(status.command)
    }

    /// Publish a complete pinned installation after download and extraction succeed.
    pub(super) async fn install(&self) -> Result<RuntimeStatus> {
        ensure!(
            self.installable(),
            "Explicit backend commands must be installed by their owner"
        );
        std::fs::create_dir_all(&self.root)?;
        let lock = File::options()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(self.root.join("install.lock"))?;
        lock.try_lock()
            .context("Another ACP installation is in progress")?;
        let destination = self.root.join(self.version());
        if self.managed_command(&destination).is_some() {
            return Ok(self.status());
        }
        ensure!(
            !destination.exists(),
            "Incomplete ACP installation at {} must be removed before retrying",
            destination.display()
        );
        let staging = tempfile::Builder::new()
            .prefix("install-")
            .tempdir_in(&self.root)?;
        if self.kind == BackendKind::Claude {
            let node = crate::process::executable("node")
                .context("Node.js 22 or newer is required to install Claude ACP")?;
            let output = tool_output(
                node.to_string_lossy().as_ref(),
                &["--version".into()],
                staging.path(),
            )
            .await?;
            let version = semver::Version::parse(
                String::from_utf8_lossy(&output.stdout)
                    .trim()
                    .trim_start_matches('v'),
            )?;
            ensure!(
                version.major >= 22,
                "Claude ACP requires Node.js 22 or newer, found {version}"
            );
            let npm = crate::process::executable("npm")
                .context("npm is required to install Claude ACP")?;
            tool_output(
                npm.to_string_lossy().as_ref(),
                &[
                    "install".into(),
                    "--prefix".into(),
                    staging.path().to_string_lossy().into_owned(),
                    "--no-audit".into(),
                    "--no-fund".into(),
                    "--save-exact".into(),
                    format!("@agentclientprotocol/claude-agent-acp@{CLAUDE_VERSION}"),
                ],
                staging.path(),
            )
            .await?;
        } else {
            let (platform, architecture) = match (std::env::consts::OS, std::env::consts::ARCH) {
                ("windows", "x86_64") => ("windows", "windows-x86_64"),
                ("windows", "aarch64") => ("windows", "windows-arm64"),
                ("macos", "x86_64") => ("macos", "darwin-x86_64"),
                ("macos", "aarch64") => ("macos", "darwin-arm64"),
                ("linux", "x86_64") => ("linux", "linux-x86_64"),
                ("linux", "aarch64") => ("linux", "linux-arm64"),
                _ => bail!("No supported Antigravity ACP archive for this platform"),
            };
            let url = format!(
                "https://dl.google.com/agy-extensions/releases/{platform}/agy-acp-server-{ANTIGRAVITY_VERSION}-{architecture}.zip"
            );
            let mut response = reqwest::Client::builder()
                .connect_timeout(Duration::from_secs(10))
                .timeout(Duration::from_secs(120))
                .build()?
                .get(url)
                .send()
                .await?
                .error_for_status()?;
            let mut archive = tempfile::tempfile()?;
            while let Some(chunk) = response.chunk().await? {
                archive.write_all(&chunk)?;
            }
            let root = staging.path().to_owned();
            tokio::task::spawn_blocking(move || extract(archive, &root)).await??;
        }
        ensure!(
            self.managed_command(staging.path()).is_some(),
            "ACP installation omitted its executable"
        );
        std::fs::write(staging.path().join("version"), self.version())?;
        std::fs::rename(staging.path(), &destination).context("publish ACP installation")?;
        Ok(self.status())
    }
}

async fn tool_output(
    program: &str,
    argument: &[String],
    root: &Path,
) -> Result<std::process::Output> {
    let mut command = crate::process::command(program, argument);
    command
        .current_dir(root)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let child = crate::process::spawn_owned(command)?;
    let output = tokio::time::timeout(
        Duration::from_secs(120),
        Box::into_pin(child.wait_with_output()),
    )
    .await
    .context("ACP installation command exceeded 120 seconds")??;
    ensure!(
        output.status.success(),
        "ACP installation command failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(output)
}

fn extract(archive: File, root: &Path) -> Result<()> {
    let mut archive = zip::ZipArchive::new(archive)?;
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index)?;
        let relative = entry
            .enclosed_name()
            .context("ACP archive path escapes its installation directory")?;
        ensure!(
            relative
                .components()
                .all(|component| matches!(component, Component::Normal(_))),
            "ACP archive path is not relative"
        );
        ensure!(
            entry
                .unix_mode()
                .is_none_or(|mode| mode & 0o170000 != 0o120000),
            "ACP archive contains a symbolic link"
        );
        let destination = root.join(relative);
        if entry.is_dir() {
            std::fs::create_dir_all(destination)?;
            continue;
        }
        if let Some(parent) = destination.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let mut output = File::create(&destination)?;
        let expected_size = entry.size();
        let copied = std::io::copy(&mut entry, &mut output)?;
        ensure!(
            copied == expected_size,
            "ACP archive entry differs from its declared size"
        );
        #[cfg(unix)]
        if let Some(mode) = entry.unix_mode() {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(destination, std::fs::Permissions::from_mode(mode & 0o777))?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn extraction_rejects_traversal_and_preserves_existing_runtime() -> Result<()> {
        let root = tempfile::tempdir()?;
        let installation = root.path().join("installation");
        std::fs::create_dir(&installation)?;
        std::fs::write(installation.join("existing"), "retained")?;
        let archive = tempfile::tempfile()?;
        let mut writer = zip::ZipWriter::new(archive);
        writer.start_file("../escape", zip::write::SimpleFileOptions::default())?;
        writer.write_all(b"escaped")?;
        let archive = writer.finish()?;
        assert!(extract(archive, &installation).is_err());
        assert_eq!(
            std::fs::read_to_string(installation.join("existing"))?,
            "retained"
        );
        assert!(!root.path().join("escape").exists());
        Ok(())
    }
}
