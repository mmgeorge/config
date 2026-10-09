use crate::session::{AccessPolicy, PermissionMode, WindowsSandbox};
use serde_json::{Value, json};

/// Projects approvals and isolation into separate native Codex controls.
pub struct CodexSecurity<'a> {
    mode: PermissionMode,
    access: &'a AccessPolicy,
}

impl<'a> CodexSecurity<'a> {
    pub const fn new(mode: PermissionMode, access: &'a AccessPolicy) -> Self {
        Self { mode, access }
    }

    fn approval_policy(&self) -> &'static str {
        match self.mode {
            PermissionMode::Read => "untrusted",
            PermissionMode::Write => "on-request",
            PermissionMode::Yolo => "never",
        }
    }

    /// Configure a thread without changing isolation when approval mode changes.
    pub fn apply_thread(&self, params: &mut Value, workspace: &str) {
        params["approvalPolicy"] = json!(self.approval_policy());
        params["approvalsReviewer"] = json!("user");
        params["sandbox"] = json!(if self.access.sandbox { "workspace-write" } else { "danger-full-access" });
        if !params["config"].is_object() { params["config"] = json!({}); }
        let config = &mut params["config"];
        config["sandbox_workspace_write.writable_roots"] = json!(self.access.writable_roots(workspace));
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
        params["sandboxPolicy"] = if self.access.sandbox {
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
        let access = AccessPolicy { writable_directory: vec!["D:/shared".into()], ..Default::default() };
        for (mode, approval) in [(PermissionMode::Read,"untrusted"),
            (PermissionMode::Write,"on-request"), (PermissionMode::Yolo,"never")] {
            let security = CodexSecurity::new(mode, &access);
            let mut turn = json!({});
            security.apply_turn(&mut turn, "D:/repo");
            assert_eq!(turn["approvalPolicy"], approval);
            assert_eq!(turn["sandboxPolicy"]["type"], "workspaceWrite");
            assert_eq!(turn["sandboxPolicy"]["writableRoots"], json!(["D:/repo","D:/shared"]));
            assert!(turn.get("permissions").is_none());
            let mut thread = json!({"config":{"model_reasoning_effort":"high"}});
            security.apply_thread(&mut thread,"D:/repo");
            assert_eq!(thread["sandbox"], "workspace-write");
            assert_eq!(thread["config"]["model_reasoning_effort"], "high");
        }
    }

    #[test]
    fn disabling_sandbox_does_not_disable_approvals() {
        let access = AccessPolicy { sandbox:false, ..Default::default() };
        let mut turn = json!({});
        CodexSecurity::new(PermissionMode::Read,&access).apply_turn(&mut turn,"D:/repo");
        assert_eq!(turn["sandboxPolicy"]["type"],"dangerFullAccess");
        assert_eq!(turn["approvalPolicy"],"untrusted");
    }
}
