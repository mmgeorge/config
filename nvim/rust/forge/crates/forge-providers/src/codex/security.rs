use forge_provider_api::access::AccessPolicy;
use forge_provider_api::capability::{ProviderFeature, UnsupportedCapability};
use serde_json::{Value, json};

/// Keep validated native isolation choices inside the Codex adapter.
#[derive(Clone, Copy)]
enum IsolationMode {
    Default,
    #[cfg(windows)]
    Elevated,
    #[cfg(windows)]
    Unelevated,
    #[cfg(windows)]
    Mxc,
}

/// Apply access settings only after validating native isolation support.
pub struct CodexSecurity<'a> {
    access: &'a AccessPolicy,
    isolation: IsolationMode,
}

impl<'a> CodexSecurity<'a> {
    /// Create native security settings without interpreting unknown values as a default.
    pub fn new(access: &'a AccessPolicy) -> Result<Self, UnsupportedCapability> {
        let isolation = match access.isolation_mode.as_deref() {
            None => IsolationMode::Default,
            #[cfg(windows)]
            Some("elevated") => IsolationMode::Elevated,
            #[cfg(windows)]
            Some("unelevated") => IsolationMode::Unelevated,
            #[cfg(windows)]
            Some("mxc") => IsolationMode::Mxc,
            Some(_) => return Err(UnsupportedCapability(ProviderFeature::IsolationMode)),
        };
        Ok(Self { access, isolation })
    }

    fn approval_policy(&self) -> &'static str {
        "untrusted"
    }

    /// Configure a thread without changing isolation when approval mode changes.
    pub fn apply_thread(&self, params: &mut Value, workspace: &str) {
        params["approvalPolicy"] = json!(self.approval_policy());
        params["approvalsReviewer"] = json!("user");
        params["sandbox"] = json!(if self.access.sandbox
            && self.access.write_access == forge_provider_api::access::WriteScope::Off
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
        match self.isolation {
            IsolationMode::Default => {
                #[cfg(windows)]
                {
                    config["windows.sandbox"] = json!("elevated");
                    config["features.prefer_mxc"] = json!(false);
                }
            }
            #[cfg(windows)]
            IsolationMode::Elevated | IsolationMode::Mxc => {
                config["windows.sandbox"] = json!("elevated");
                config["features.prefer_mxc"] = json!(matches!(self.isolation, IsolationMode::Mxc));
            }
            #[cfg(windows)]
            IsolationMode::Unelevated => {
                config["windows.sandbox"] = json!("unelevated");
                config["features.prefer_mxc"] = json!(false);
            }
        }
    }

    /// Override the turn policy using fields accepted by the app-server API.
    pub fn apply_turn(&self, params: &mut Value, workspace: &str) {
        params["approvalPolicy"] = json!(self.approval_policy());
        params["approvalsReviewer"] = json!("user");
        params["sandboxPolicy"] = if self.access.sandbox
            && self.access.write_access == forge_provider_api::access::WriteScope::Off
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
    fn disabling_sandbox_does_not_disable_approvals() {
        let access = AccessPolicy {
            sandbox: false,
            ..Default::default()
        };
        let mut turn = json!({});
        CodexSecurity::new(&access)
            .unwrap()
            .apply_turn(&mut turn, "D:/repo");
        assert_eq!(turn["sandboxPolicy"]["type"], "dangerFullAccess");
        assert_eq!(turn["approvalPolicy"], "untrusted");
    }

    #[test]
    fn write_off_uses_native_read_only() {
        let access = AccessPolicy {
            write_access: forge_provider_api::access::WriteScope::Off,
            ..Default::default()
        };
        let security = CodexSecurity::new(&access).unwrap();
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
