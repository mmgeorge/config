//! Commit local configuration only when its command reaches the session owner.

use crate::session::SessionStore;

use super::command::{Command, ConfigurationChange, ConfigurationOutcome, PermissionChange};
use super::queue::QueuedCommand;
use super::services::OwnerServices;

/// Commit a validated local policy without taking native execution or cleanup ownership.
pub(super) fn apply_configuration(
    command: &QueuedCommand,
    services: &OwnerServices,
) -> Result<ConfigurationOutcome, ConfigurationCommitError> {
    let outcome = commit_configuration(command, services)?;
    let snapshot = services
        .observer
        .snapshot()
        .map_err(ConfigurationCommitError::Snapshot)?;
    services
        .publications
        .publish_batch(vec![forge_protocol::message::SessionEvent {
            session_id: services.observer.session_id().into(),
            event: "session_configured".into(),
            payload: serde_json::json!({
                "snapshot": snapshot,
                "operation_id": command.record.request.id,
            }),
        }])
        .map_err(ConfigurationCommitError::Publication)?;
    Ok(outcome)
}

/// Apply the local policy before publishing its complete committed projection.
fn commit_configuration(
    command: &QueuedCommand,
    services: &OwnerServices,
) -> Result<ConfigurationOutcome, ConfigurationCommitError> {
    let change = match &command.record.request.command {
        Command::ConfigureSession(change) => change,
        Command::Task(_)
        | Command::CancelExchange(_)
        | Command::CompactContext(_)
        | Command::Input(_)
        | Command::Agent(_)
        | Command::Rollback(_)
        | Command::ContinueWorkflow(_) => {
            unreachable!("configuration effects require an active configuration command")
        }
    };
    match change {
        ConfigurationChange::Permission(PermissionChange::SetCheckEditing { enabled }) => {
            services
                .observer
                .set_check_editing(*enabled)
                .map_err(ConfigurationCommitError::Settings)?;
            Ok(ConfigurationOutcome::CheckEditing { enabled: *enabled })
        }
        ConfigurationChange::Settings(_)
        | ConfigurationChange::Permission(PermissionChange::ExecutionPolicy { .. }) => {
            unreachable!("execution policy uses the staged native configuration path")
        }
        ConfigurationChange::CheckPreferences(patch) => {
            if let Some(selection) = patch.overrides() {
                use super::command::ConfigurationRejection;
                let current = services
                    .tasks
                    .current()
                    .map_err(ConfigurationCommitError::Session)?;
                if !selection.scope().validate(current.as_ref()) {
                    return Err(ConfigurationCommitError::Rejected(
                        ConfigurationRejection::CheckTaskChanged,
                    ));
                }
                let session = crate::storage::SqliteStore::read_only(&services.root)
                    .map_err(ConfigurationCommitError::Session)?
                    .load_session(services.observer.session_id())
                    .map_err(ConfigurationCommitError::Session)?
                    .ok_or(ConfigurationCommitError::MissingSession)?;
                let configuration =
                    crate::checks::ConfigSnapshot::load(std::path::Path::new(&session.workspace))
                        .map_err(ConfigurationCommitError::CheckPolicy)?;
                if configuration.digest != selection.configuration() {
                    return Err(ConfigurationCommitError::Rejected(
                        ConfigurationRejection::CheckDefinitionsChanged,
                    ));
                }
                if services
                    .checks
                    .active()
                    .map_err(ConfigurationCommitError::CheckPolicy)?
                    .is_some()
                {
                    return Err(ConfigurationCommitError::Rejected(
                        ConfigurationRejection::CheckRunActive,
                    ));
                }
                let values = selection.values();
                for id in values
                    .enabled
                    .keys()
                    .chain(values.auto.keys())
                    .chain(values.scopes.keys())
                {
                    let definition = configuration
                        .config
                        .definitions
                        .iter()
                        .find(|definition| &definition.id == id)
                        .ok_or(ConfigurationCommitError::Rejected(
                            ConfigurationRejection::CheckUnknown,
                        ))?;
                    if let Some(scope) = values.scopes.get(id) {
                        let supported = match scope {
                            crate::checks::CheckScope::All => true,
                            crate::checks::CheckScope::ChangedFiles => definition.files.is_some(),
                            crate::checks::CheckScope::ChangedCases
                            | crate::checks::CheckScope::NewCases => definition.cases.is_some(),
                        };
                        if !supported {
                            return Err(ConfigurationCommitError::Rejected(
                                ConfigurationRejection::CheckCoverageUnavailable,
                            ));
                        }
                    }
                }
                services
                    .checks
                    .set_overrides(selection.scope().task_id(), values.clone())
                    .map_err(ConfigurationCommitError::CheckPolicy)?;
                return Ok(ConfigurationOutcome::CheckOverrides {
                    scope: selection.scope().clone(),
                    overrides: values.clone(),
                    configuration: Box::new(configuration),
                });
            }
            let session = crate::storage::SqliteStore::read_only(&services.root)
                .map_err(ConfigurationCommitError::Session)?
                .load_session(services.observer.session_id())
                .map_err(ConfigurationCommitError::Session)?
                .ok_or(ConfigurationCommitError::MissingSession)?;
            let mut settings = crate::settings::SettingsStore::open(&services.root)
                .map_err(ConfigurationCommitError::Settings)?;
            settings
                .patch(
                    crate::settings::SettingTarget {
                        scope: crate::settings::SettingScope::Global,
                        workspace: &session.workspace,
                        session: &session.id,
                    },
                    &crate::settings::PortableSettings {
                        check_timing: patch.check_timing(),
                        automatic_checks: patch.automatic_checks(),
                        ..Default::default()
                    },
                )
                .map_err(ConfigurationCommitError::Settings)?;
            let effective = settings
                .get(&session.workspace, &session.id, &session.provider)
                .map_err(ConfigurationCommitError::Settings)?
                .desired;
            if !effective.automatic_checks_enabled() {
                services
                    .observer
                    .completion_queue
                    .finish()
                    .map_err(ConfigurationCommitError::CheckPolicy)?;
            }
            Ok(ConfigurationOutcome::CheckPreferences {
                check_timing: effective.check_timing.unwrap_or_default(),
                automatic_checks: effective.automatic_checks_enabled(),
                checks_deferred: services
                    .observer
                    .completion_queue
                    .deferred()
                    .map_err(ConfigurationCommitError::CheckPolicy)?,
            })
        }
        ConfigurationChange::InputTimeouts(patch) => {
            let session = crate::storage::SqliteStore::read_only(&services.root)
                .map_err(ConfigurationCommitError::Session)?
                .load_session(services.observer.session_id())
                .map_err(ConfigurationCommitError::Session)?
                .ok_or(ConfigurationCommitError::MissingSession)?;
            let mut settings = crate::settings::SettingsStore::open(&services.root)
                .map_err(ConfigurationCommitError::Settings)?;
            settings
                .patch(
                    crate::settings::SettingTarget {
                        scope: crate::settings::SettingScope::Global,
                        workspace: &session.workspace,
                        session: &session.id,
                    },
                    &crate::settings::PortableSettings {
                        approval_timeout: patch.approval_timeout(),
                        tool_timeout: patch.tool_timeout(),
                        ..Default::default()
                    },
                )
                .map_err(ConfigurationCommitError::Settings)?;
            let effective = settings
                .get(&session.workspace, &session.id, &session.provider)
                .map_err(ConfigurationCommitError::Settings)?
                .desired;
            let approval_timeout = effective.approval_timeout.unwrap_or_default();
            let tool_timeout = effective.tool_timeout.unwrap_or_default();
            services
                .permissions
                .configure_timeout(&session.id, approval_timeout)
                .map_err(ConfigurationCommitError::Permissions)?;
            Ok(ConfigurationOutcome::InputTimeouts {
                approval_timeout,
                tool_timeout,
            })
        }
    }
}

/// Preserve legacy storage causes while giving the owner finite commit-failure categories.
#[derive(Debug, thiserror::Error)]
pub(super) enum ConfigurationCommitError {
    /// Committed policy could not be published to session observers.
    #[error("configuration publication failed: {0}")]
    Publication(#[source] super::publication::PublicationError),
    /// The committed policy snapshot could not be read.
    #[error("configuration snapshot failed: {0}")]
    Snapshot(#[source] anyhow::Error),
    /// Validated policy could not apply to its captured scope or available definitions.
    #[error(transparent)]
    Rejected(super::command::ConfigurationRejection),
    /// Session authority could not be read from the existing storage API.
    #[error("configuration session read failed: {0}")]
    Session(#[source] anyhow::Error),
    /// The command's session was removed before its local policy could commit.
    #[error("configuration session is missing")]
    MissingSession,
    /// The existing settings store could not commit or read back the selected policy.
    #[error("configuration settings commit failed: {0}")]
    Settings(#[source] anyhow::Error),
    /// Runtime permission policy could not be synchronized with committed settings.
    #[error("configuration permission publication failed: {0}")]
    Permissions(#[source] anyhow::Error),
    /// Automatic-check admission state could not reflect the committed policy.
    #[error("configuration check policy commit failed: {0}")]
    CheckPolicy(#[source] anyhow::Error),
}
