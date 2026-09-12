use crate::session::ExecutionMode;
use serde_json::{Map, Value, json};

const READ_PROFILE: &str = "harness_read";
const WRITE_PROFILE: &str = "harness_write";
const READ_PROFILE_OVERRIDE: &str =
    "permissions.harness_read={ extends = \":read-only\", network = { enabled = true } }";
const WRITE_PROFILE_OVERRIDE: &str = "permissions.harness_write={ extends = \":read-only\", filesystem = { \":workspace_roots\" = { \".\" = \"write\" } }, network = { enabled = true } }";

/// Projects one Harness execution mode into Codex process and request settings.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CodexSecurity {
    execution_mode: ExecutionMode,
}

impl CodexSecurity {
    pub const fn new(execution_mode: ExecutionMode) -> Self {
        Self { execution_mode }
    }

    /// Add Harness-owned permission profiles before the Codex app-server subcommand.
    pub fn launch_command(self, command: &[String]) -> Vec<String> {
        let mut launch_command = Vec::with_capacity(command.len() + 4);
        if let Some((program, argument_list)) = command.split_first() {
            launch_command.push(program.clone());
            launch_command.extend([
                "-c".into(),
                READ_PROFILE_OVERRIDE.into(),
                "-c".into(),
                WRITE_PROFILE_OVERRIDE.into(),
            ]);
            launch_command.extend(argument_list.iter().cloned());
        }
        launch_command
    }

    /// Merge one authoritative security projection into a Codex request object.
    pub fn apply(self, params: &mut Value, workspace: &str) {
        let object = params
            .as_object_mut()
            .expect("Codex request params must be an object");
        object.extend(self.settings(workspace));
    }

    pub const fn approval_policy(self) -> &'static str {
        if matches!(self.execution_mode, ExecutionMode::Yolo) {
            "never"
        } else {
            "on-request"
        }
    }

    pub const fn permission_profile(self) -> &'static str {
        match self.execution_mode {
            ExecutionMode::Read => READ_PROFILE,
            ExecutionMode::Write => WRITE_PROFILE,
            ExecutionMode::Full | ExecutionMode::Yolo => ":danger-full-access",
        }
    }

    fn settings(self, workspace: &str) -> Map<String, Value> {
        json!({
            "approvalPolicy": self.approval_policy(),
            "approvalsReviewer": "user",
            "permissions": self.permission_profile(),
            "runtimeWorkspaceRoots": [workspace],
        })
        .as_object()
        .expect("security settings must be an object")
        .clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn projects_every_execution_mode_without_combining_legacy_sandbox_fields() {
        for (mode, profile, approval_policy) in [
            (ExecutionMode::Read, READ_PROFILE, "on-request"),
            (ExecutionMode::Write, WRITE_PROFILE, "on-request"),
            (ExecutionMode::Full, ":danger-full-access", "on-request"),
            (ExecutionMode::Yolo, ":danger-full-access", "never"),
        ] {
            let mut params = json!({ "cwd": "D:/repo" });
            CodexSecurity::new(mode).apply(&mut params, "D:/repo");
            assert_eq!(params["permissions"], profile);
            assert_eq!(params["approvalPolicy"], approval_policy);
            assert!(params.get("sandbox").is_none());
            assert!(params.get("sandboxPolicy").is_none());
        }
    }

    #[test]
    fn inserts_profile_overrides_before_the_app_server_subcommand() {
        let command = CodexSecurity::new(ExecutionMode::Read)
            .launch_command(&["codex".into(), "app-server".into()]);
        assert_eq!(command.first().map(String::as_str), Some("codex"));
        assert_eq!(command.last().map(String::as_str), Some("app-server"));
        assert_eq!(
            command
                .iter()
                .filter(|value| value.as_str() == "-c")
                .count(),
            2
        );
    }
}
