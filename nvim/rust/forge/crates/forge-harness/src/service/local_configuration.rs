//! Decode local policy requests once and observe the command owner's typed completion.

use super::*;
use crate::exchange::command::{
    CheckPreferencesUpdate, Command, CommandId, CommandRequest, ConfigurationChange, InputControl,
    InputTimeoutConfiguration, PermissionChange,
};

/// Select a supported transport spelling before entering the typed configuration path.
#[derive(Clone, Copy)]
enum ConfigurationRoute {
    CheckEditing,
    CheckPreferences,
    CheckOverrides,
    InputTimeouts,
}

impl ConfigurationRoute {
    /// Decode external method spelling without exposing it to the owner.
    fn decode(request: &Request) -> Option<Self> {
        match request.method.as_str() {
            "session.check.override" => Some(Self::CheckOverrides),
            "session.check.editing.set" => Some(Self::CheckEditing),
            "session.input.timeout.configure" => Some(Self::InputTimeouts),
            "session.configure"
                if request.params.as_object().is_some_and(|fields| {
                    fields
                        .keys()
                        .any(|key| matches!(key.as_str(), "check_timing" | "automatic_checks"))
                        && fields.keys().all(|key| {
                            matches!(
                                key.as_str(),
                                "check_timing" | "automatic_checks" | "operation_id"
                            )
                        })
                }) =>
            {
                Some(Self::CheckPreferences)
            }
            _ => None,
        }
    }
}

/// Recognize local policy routes whose effects require no native restart.
pub(super) fn handles(request: &Request) -> bool {
    ConfigurationRoute::decode(request).is_some()
}

/// Normalize a local policy patch, execute it in FIFO order, and translate only at the RPC boundary.
pub(super) async fn apply(
    controller: &SessionController,
    request: &Request,
    messages: &MessageSender,
) -> Result<()> {
    let mut fields = request
        .params
        .as_object()
        .cloned()
        .context("configuration parameters must be an object")?;
    let id = match fields.remove("operation_id") {
        Some(value) => CommandId::new(value.as_str().context("operation_id must be a string")?)?,
        None => CommandId::new(uuid::Uuid::new_v4().to_string())?,
    };
    let change = match ConfigurationRoute::decode(request)
        .context("method is not a local configuration command")?
    {
        ConfigurationRoute::CheckEditing => {
            #[derive(serde::Deserialize)]
            #[serde(deny_unknown_fields)]
            struct Selection {
                enabled: bool,
            }
            let Selection { enabled } = serde_json::from_value(Value::Object(fields))?;
            ConfigurationChange::Permission(PermissionChange::SetCheckEditing { enabled })
        }
        ConfigurationRoute::CheckPreferences => {
            let patch: CheckPreferencesUpdate = serde_json::from_value(Value::Object(fields))?;
            ConfigurationChange::CheckPreferences(patch)
        }
        ConfigurationRoute::CheckOverrides => {
            let values: crate::checks::CheckOverrides =
                serde_json::from_value(Value::Object(fields))?;
            if let Some(previous) = controller.exchange_owner.command_status(id.clone()).await? {
                let Command::ConfigureSession(ConfigurationChange::CheckPreferences(patch)) =
                    previous.request.command
                else {
                    anyhow::bail!("command identity was reused for a different configuration");
                };
                ensure!(
                    patch
                        .overrides()
                        .is_some_and(|selection| selection.values() == &values),
                    "command identity was reused for different check preferences"
                );
                ConfigurationChange::CheckPreferences(patch)
            } else {
                let snapshot = controller.observer.snapshot()?;
                let configuration = crate::checks::ConfigSnapshot::load(std::path::Path::new(
                    &snapshot.session.workspace,
                ))?;
                ConfigurationChange::CheckPreferences(CheckPreferencesUpdate::capture_overrides(
                    snapshot.session.active_task.as_ref(),
                    configuration.digest,
                    values,
                )?)
            }
        }
        ConfigurationRoute::InputTimeouts => {
            let patch: InputTimeoutConfiguration = serde_json::from_value(Value::Object(fields))?;
            ConfigurationChange::InputTimeouts(patch)
        }
    };
    let command = Command::ConfigureSession(change);
    let result = controller
        .execute_command(
            CommandRequest {
                id,
                command: command.clone(),
                input_control: InputControl::default(),
            },
            messages,
        )
        .await;
    let response = match result {
        Ok(result) => {
            Response::success(request.id, controller.command_response(&command, &result)?)?
        }
        Err(error) => Response::failure(request.id, error.code(), error.to_string()),
    };
    messages.send_response(response).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep local configuration ordered behind a prior cancellation without taking native ownership.
    #[tokio::test]
    async fn timeout_configuration_waits_in_fifo_and_replays_without_rewriting_settings()
    -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "workspace":fixture.path(),"data_root":fixture.path().join("data"),
            "client_id":"local-policy-order","provider":{"kind":"mock","command":[]},
        }))?;
        let controller = SessionController::new(HarnessBroker::initialize(initialize)?).await?;
        let gate = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let execution = controller.coordinator.execution.acquire().await?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let cancel_request = Request {
            id: 1,
            method: "session.turn.cancel".into(),
            params: json!({"operation_id":"cancel-before-policy"}),
        };
        let mut cancel = Box::pin(cancellation::apply(&controller, &cancel_request, &messages));
        assert!(futures_util::poll!(cancel.as_mut()).is_pending());
        let request = Request {
            id: 2,
            method: "session.input.timeout.configure".into(),
            params: json!({
                "operation_id":"policy-after-cancel","approval_timeout":{"amount":2,"unit":"s"},
            }),
        };
        let mut configure = Box::pin(apply(&controller, &request, &messages));
        assert!(futures_util::poll!(configure.as_mut()).is_pending());
        let id = CommandId::new("policy-after-cancel")?;
        let queued = controller
            .exchange_owner
            .command_status(id)
            .await?
            .context("configuration command is missing")?;
        assert!(matches!(
            queued.state,
            crate::exchange::journal::CommandState::Queued { .. }
        ));
        assert_eq!(
            controller
                .permission
                .approval_timeout(controller.observer.session_id())?
                .amount,
            10
        );
        drop(gate);
        let (cancelled, configured) = async { tokio::join!(cancel, configure) }.await;
        cancelled?;
        configured?;
        assert_eq!(
            controller
                .permission
                .approval_timeout(controller.observer.session_id())?
                .amount,
            2
        );
        assert!(controller.coordinator.execution.try_acquire().is_err());
        let session = controller.observer.snapshot()?.session;
        let settings = crate::settings::SettingsStore::open(&controller.data_root)?;
        let before = settings
            .get(&session.workspace, &session.id, &session.provider)?
            .revision;
        apply(&controller, &request, &messages).await?;
        assert_eq!(
            settings
                .get(&session.workspace, &session.id, &session.provider)?
                .revision,
            before
        );
        drop(execution);
        controller.finish_exchange_owner().await?;
        Ok(())
    }
}
