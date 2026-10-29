use super::*;

#[derive(Clone, Copy, Eq, PartialEq)]
pub(super) enum ConfigurationCommand {
    Permission,
    Configure,
    Compact,
}

impl HarnessBroker {
    pub(super) async fn apply_configuration(
        &mut self,
        action: Value,
        command: ConfigurationCommand,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let previous_permission = self.session.approval_mode;
        let previous_access = self.session.access.abbreviation();
        let mut access = self.session.access.clone();
        let mut permission = self.session.approval_mode;
        if command == ConfigurationCommand::Permission {
            let change: crate::task::PermissionChange = serde_json::from_value(
                action
                    .get("change")
                    .cloned()
                    .context("permission change is required")?,
            )?;
            if let Some(selection) = action.get("permission_selection") {
                let selection: crate::task::PermissionSelection =
                    serde_json::from_value(selection.clone())?;
                access.sandbox = selection.sandbox;
                access.read_access = selection.read_access;
                access.write_access = selection.write_access;
                permission = selection.approval_mode;
            } else {
                change.apply(
                    &mut access,
                    &mut permission,
                    crate::backend::BackendKind::parse(&self.session.backend)?.supports_sandbox(),
                );
            }
            access = access.validate()?;
        }
        let resume = action
            .get("resume")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if command == ConfigurationCommand::Configure {
            self.configuration_progress("applying").await?;
        }
        match command {
            ConfigurationCommand::Configure => {
                Box::pin(
                    self.configure_session(
                        action
                            .get("config")
                            .cloned()
                            .context("configuration is required")?,
                    ),
                )
                .await?;
            }
            ConfigurationCommand::Compact => {
                Box::pin(self.compact_session(true)).await?;
            }
            ConfigurationCommand::Permission => {
                Box::pin(self.configure_session(
                    json!({"access": access, "approval_mode": permission, "session_policy": true}),
                ))
                .await?;
            }
        }
        if command == ConfigurationCommand::Configure
            && let Some(revision) = action
                .pointer("/config/settings_revision")
                .and_then(Value::as_u64)
            && self.active_operation_id.is_some()
        {
            self.configuration_progress("verifying").await?;
            let operation = self
                .active_operation_id
                .as_deref()
                .expect("active configuration operation");
            let settings = crate::settings::SettingsStore::open(&self.data_root)?;
            let snapshot = settings.get(
                &self.session.workspace,
                &self.session.id,
                &self.session.backend,
            )?;
            anyhow::ensure!(
                snapshot.revision == revision,
                "settings changed before application confirmation"
            );
            let mut effective = snapshot.desired;
            effective.access = Some(self.session.access.clone());
            effective.shell = Some(self.session.shell);
            effective.default_approval = Some(self.session.default_approval);
            effective.plan_approval = self.session.plan_approval;
            effective.plan_compact = Some(self.session.plan_compact);
            effective.plan_auto_approve_revisions = Some(self.session.plan_auto_approve_revisions);
            effective.approval_timeout = Some(self.session.approval_timeout);
            effective.answer_timeout = Some(self.session.answer_timeout);
            settings.confirm_application(&self.session.id, operation, revision, effective)?;
        }
        if command == ConfigurationCommand::Configure {
            let request = self.backend_catalog_request();
            self.notify_unavailable_mcp(&request, &mut Vec::new())
                .await?;
        }
        self.emit_backend_event(
            BackendEvent {
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: "configuration_applied".into(),
                text: None,
                data: json!({"session":self.session,"access_abbreviation":self.session.access.abbreviation(),"operation_id":self.active_operation_id,
                "catalog":self.backend_catalog_request()}),
                activity: None,
                summary: None,
                checklist_update: None,
            },
            &mut Vec::new(),
        )
        .await?;
        let permission_notice = (command == ConfigurationCommand::Permission).then(|| format!(
            "The user changed filesystem access from {} to {} and approval mode from {} to {}. Continue the interrupted work under the updated policy. \
             Saved approval rules still apply. Preserve completed work. \
             Before retrying an interrupted operation, inspect its recorded outcome and any affected files.",
            previous_access, self.session.access.abbreviation(), previous_permission.label(), permission.label()
        ));
        if resume {
            self.configuration_progress("resuming").await?;
            let previous = self
                .store
                .latest_exchange(&self.session.id)?
                .context("interrupted exchange is missing")?;
            let notice = permission_notice.as_deref().unwrap_or("Continue the interrupted request using the current repository and completed tool results. Do not repeat completed actions.");
            let prompt = if previous.elicitation().is_some() {
                notice.to_owned()
            } else {
                format!("{notice}\n\nOriginal request: {}", previous.prompt)
            };
            return Box::pin(self.resume_exchange(json!({"text":prompt}))).await;
        }
        Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()))
    }
}
