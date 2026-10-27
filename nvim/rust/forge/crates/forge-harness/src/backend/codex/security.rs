use crate::session::{AccessPolicy, ApprovalMode, WindowsSandbox};
use serde_json::{Value, json};

/// Projects approvals and isolation into separate native Codex controls.
pub struct CodexSecurity<'a> {
    access: &'a AccessPolicy,
}

impl<'a> CodexSecurity<'a> {
    pub const fn new(_mode: ApprovalMode, access: &'a AccessPolicy) -> Self {
        Self { access }
    }

    fn approval_policy(&self) -> &'static str {
        "untrusted"
    }

    /// Configure a thread without changing isolation when approval mode changes.
    pub fn apply_thread(&self, params: &mut Value, workspace: &str) {
        params["approvalPolicy"] = json!(self.approval_policy());
        params["approvalsReviewer"] = json!("user");
        params["sandbox"] = json!(if self.access.sandbox
            && self.access.write_access == crate::session::WriteScope::Off
        {
            "read-only"
        } else if self.access.sandbox {
            "workspace-write"
        } else {
            "danger-full-access"
        });
        if !params["config"].is_object() {
            params["config"] = json!({});
        }
        let config = &mut params["config"];
        config["sandbox_workspace_write.writable_roots"] =
            json!(self.access.writable_roots(workspace));
        config["sandbox_workspace_write.network_access"] = json!(true);
        config["sandbox_workspace_write.exclude_tmpdir_env_var"] = json!(true);
        config["sandbox_workspace_write.exclude_slash_tmp"] = json!(true);
        config["windows.sandbox"] = json!(match self.access.windows_sandbox {
            WindowsSandbox::Elevated | WindowsSandbox::Mxc => "elevated",
            WindowsSandbox::Unelevated => "unelevated",
        });
        config["features.prefer_mxc"] = json!(self.access.windows_sandbox == WindowsSandbox::Mxc);
    }

    /// Override the turn policy using fields accepted by the app-server API.
    pub fn apply_turn(&self, params: &mut Value, workspace: &str) {
        params["approvalPolicy"] = json!(self.approval_policy());
        params["approvalsReviewer"] = json!("user");
        params["sandboxPolicy"] = if self.access.sandbox
            && self.access.write_access == crate::session::WriteScope::Off
        {
            json!({"type":"readOnly", "networkAccess":true})
        } else if self.access.sandbox {
            json!({"type":"workspaceWrite", "writableRoots":self.access.writable_roots(workspace),
                "networkAccess":true, "excludeSlashTmp":true, "excludeTmpdirEnvVar":true})
        } else {
            json!({"type":"dangerFullAccess"})
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn approval_changes_preserve_sandbox_and_directories() {
        let access = AccessPolicy {
            writable_directory: vec!["D:/shared".into()],
            ..Default::default()
        };
        for (mode, approval) in [
            (ApprovalMode::Ask, "untrusted"),
            (ApprovalMode::Deny, "untrusted"),
            (ApprovalMode::Allow, "untrusted"),
        ] {
            let security = CodexSecurity::new(mode, &access);
            let mut turn = json!({});
            security.apply_turn(&mut turn, "D:/repo");
            assert_eq!(turn["approvalPolicy"], approval);
            assert_eq!(turn["sandboxPolicy"]["type"], "workspaceWrite");
            assert_eq!(
                turn["sandboxPolicy"]["writableRoots"],
                json!(["D:/repo", "D:/shared"])
            );
            assert!(turn.get("permissions").is_none());
            let mut thread = json!({"config":{"model_reasoning_effort":"high"}});
            security.apply_thread(&mut thread, "D:/repo");
            assert_eq!(thread["sandbox"], "workspace-write");
            assert_eq!(thread["config"]["model_reasoning_effort"], "high");
        }
    }

    #[test]
    fn disabling_sandbox_does_not_disable_approvals() {
        let access = AccessPolicy {
            sandbox: false,
            ..Default::default()
        };
        let mut turn = json!({});
        CodexSecurity::new(ApprovalMode::Ask, &access).apply_turn(&mut turn, "D:/repo");
        assert_eq!(turn["sandboxPolicy"]["type"], "dangerFullAccess");
        assert_eq!(turn["approvalPolicy"], "untrusted");
    }

    #[test]
    fn write_off_uses_native_read_only_for_every_approval_mode() {
        let access = AccessPolicy {
            write_access: crate::session::WriteScope::Off,
            ..Default::default()
        };
        for mode in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            let security = CodexSecurity::new(mode, &access);
            let mut thread = json!({});
            security.apply_thread(&mut thread, "D:/repo");
            assert_eq!(thread["approvalPolicy"], "untrusted");
            assert_eq!(thread["sandbox"], "read-only");
            let mut turn = json!({});
            security.apply_turn(&mut turn, "D:/repo");
            assert_eq!(turn["sandboxPolicy"]["type"], "readOnly");
            assert_eq!(turn["approvalPolicy"], "untrusted");
        }
    }
}
