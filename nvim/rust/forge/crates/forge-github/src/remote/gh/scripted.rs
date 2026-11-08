//! In-process stand-in for the `gh` executable, used by tests of decoding and routing.

use std::ffi::OsString;
use std::process::{ExitStatus, Output};
use std::sync::Arc;

use super::GhClient;

/// One request that `GhClient` would otherwise pass to the `gh` process.
pub(crate) struct ScriptedGhInvocation {
    /// Arguments without the `--input <file>` pair; the file's bytes are in `input`.
    pub(crate) argument: Vec<OsString>,
    pub(crate) input: Option<Vec<u8>>,
}

/// What the scripted `gh` prints and how it exits.
#[derive(Default)]
pub(crate) struct ScriptedGhOutcome {
    pub(crate) stdout: Vec<u8>,
    pub(crate) stderr: Vec<u8>,
    pub(crate) exit_code: i32,
}

impl ScriptedGhOutcome {
    pub(crate) fn stdout(stdout: impl Into<Vec<u8>>) -> Self {
        Self {
            stdout: stdout.into(),
            ..Self::default()
        }
    }

    pub(super) fn into_output(self) -> Output {
        #[cfg(unix)]
        let status = std::os::unix::process::ExitStatusExt::from_raw(self.exit_code << 8);
        #[cfg(windows)]
        let status = std::os::windows::process::ExitStatusExt::from_raw(
            u32::try_from(self.exit_code).unwrap(),
        );
        let status: ExitStatus = status;
        Output {
            status,
            stdout: self.stdout,
            stderr: self.stderr,
        }
    }
}

pub(super) type ScriptedGhRunner =
    Arc<dyn Fn(&ScriptedGhInvocation) -> ScriptedGhOutcome + Send + Sync>;

impl GhClient {
    /// Build a client whose requests are answered by `runner` instead of a `gh` process.
    pub(crate) fn scripted(
        runner: impl Fn(&ScriptedGhInvocation) -> ScriptedGhOutcome + Send + Sync + 'static,
    ) -> Self {
        let mut client = Self::new("scripted-gh".into(), std::env::temp_dir())
            .expect("scripted gh client configuration is valid");
        client.scripted = Some(Arc::new(runner));
        client
    }
}
