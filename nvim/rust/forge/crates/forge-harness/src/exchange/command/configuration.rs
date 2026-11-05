//! Validate local configuration intent before durable owner admission.

use crate::session::InputTimeout;
use serde::{Deserialize, Serialize};

/// Change session policy through the same FIFO as input and exchange controls.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "change", content = "selection", rename_all = "snake_case")]
pub(crate) enum ConfigurationChange {
    /// Commit validated model, resource, and host settings under owner-selected transition policy.
    Settings(super::SessionConfigurationPatch),
    /// Commit host tool permissions without interrupting native execution.
    Permission(PermissionChange),
    /// Commit automatic-check admission policy without changing recorded check outcomes.
    CheckPreferences(CheckPreferencesUpdate),
    /// Configure future approval/tool deadlines without extending existing waits.
    InputTimeouts(InputTimeoutConfiguration),
}

/// Set a host permission without conflating it with an answer to a pending approval.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "permission", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum PermissionChange {
    /// Authorize or forbid editing project check definitions in this session.
    SetCheckEditing { enabled: bool },
    /// Change root invocation access/approval policy, collecting affected work before replacement.
    ExecutionPolicy {
        change: crate::task::PermissionChange,
    },
}

/// Retain a nonempty patch to the inherited automatic-check admission policy.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "CheckPreferenceFields")]
pub(crate) struct CheckPreferencesUpdate {
    #[serde(skip_serializing_if = "Option::is_none")]
    check_timing: Option<crate::checks::CheckTiming>,
    #[serde(skip_serializing_if = "Option::is_none")]
    automatic_checks: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    overrides: Option<ScopedCheckOverrides>,
}

impl CheckPreferencesUpdate {
    /// Capture check overrides for the selected task or current ad hoc conversation.
    pub(crate) fn capture_overrides(
        current: Option<&crate::task::Task>,
        configuration: String,
        values: crate::checks::CheckOverrides,
    ) -> Result<Self, super::CommandDecodeError> {
        Self::try_from(CheckPreferenceFields {
            check_timing: None,
            automatic_checks: None,
            overrides: Some(ScopedCheckOverrides::try_from(StoredCheckOverrides {
                scope: match current.filter(|task| !task.status().terminal()) {
                    Some(task) => CheckPreferenceScope::Task(super::TaskSelection::capture(task)),
                    None => CheckPreferenceScope::Conversation,
                },
                configuration,
                values,
            })?),
        })
    }

    /// Get captured per-check overrides without retargeting their scope.
    pub(crate) fn overrides(&self) -> Option<&ScopedCheckOverrides> {
        self.overrides.as_ref()
    }
    /// Get the selected boundary for future automatic checks.
    pub(crate) fn check_timing(&self) -> Option<crate::checks::CheckTiming> {
        self.check_timing
    }

    /// Get the explicit automatic-check enablement selection.
    pub(crate) fn automatic_checks(&self) -> Option<bool> {
        self.automatic_checks
    }
}

impl TryFrom<CheckPreferenceFields> for CheckPreferencesUpdate {
    type Error = super::CommandDecodeError;

    /// Reject an empty patch before the owner can persist a command for it.
    fn try_from(fields: CheckPreferenceFields) -> Result<Self, Self::Error> {
        let CheckPreferenceFields {
            check_timing,
            automatic_checks,
            overrides,
        } = fields;
        if overrides.is_some() && (check_timing.is_some() || automatic_checks.is_some()) {
            return Err(super::CommandDecodeError::EmptyConfiguration);
        }
        if check_timing.is_none() && automatic_checks.is_none() && overrides.is_none() {
            return Err(super::CommandDecodeError::EmptyConfiguration);
        }
        Ok(Self {
            check_timing,
            automatic_checks,
            overrides,
        })
    }
}

/// Select per-check policy ownership without treating the latest task as the original target.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "scope", content = "task", rename_all = "snake_case")]
pub(crate) enum CheckPreferenceScope {
    /// Apply to an ad hoc conversation only while no live task has replaced it.
    Conversation,
    /// Apply only to this captured live task revision.
    Task(super::TaskSelection),
}

impl CheckPreferenceScope {
    /// Get the storage key for the captured override scope.
    pub(crate) fn task_id(&self) -> Option<&str> {
        match self {
            Self::Conversation => None,
            Self::Task(task) => Some(task.id().as_str()),
        }
    }
    /// Require the same live task or ad hoc scope before applying overrides.
    pub(crate) fn validate(&self, current: Option<&crate::task::Task>) -> bool {
        match self {
            Self::Task(task) => task.validate(current).is_ok(),
            Self::Conversation => current.is_none_or(|task| task.status().terminal()),
        }
    }
}

/// Preserve scope and project configuration provenance for one complete override replacement.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "StoredCheckOverrides", into = "StoredCheckOverrides")]
pub(crate) struct ScopedCheckOverrides {
    scope: CheckPreferenceScope,
    configuration: String,
    values: crate::checks::CheckOverrides,
}

impl ScopedCheckOverrides {
    /// Get the selected policy owner.
    pub(crate) fn scope(&self) -> &CheckPreferenceScope {
        &self.scope
    }
    /// Get the check-definition digest captured before queue admission.
    pub(crate) fn configuration(&self) -> &str {
        &self.configuration
    }
    /// Get explicit enabled, automatic, and coverage selections.
    pub(crate) fn values(&self) -> &crate::checks::CheckOverrides {
        &self.values
    }
}

impl TryFrom<StoredCheckOverrides> for ScopedCheckOverrides {
    type Error = super::CommandDecodeError;
    /// Reject missing provenance and malformed check IDs during admission and recovery.
    fn try_from(stored: StoredCheckOverrides) -> Result<Self, Self::Error> {
        let StoredCheckOverrides {
            scope,
            configuration,
            values,
        } = stored;
        if configuration.trim().is_empty()
            || values
                .enabled
                .keys()
                .chain(values.auto.keys())
                .chain(values.scopes.keys())
                .any(|id| id.trim().is_empty())
        {
            return Err(super::CommandDecodeError::EmptyConfiguration);
        }
        Ok(Self {
            scope,
            configuration,
            values,
        })
    }
}

impl From<ScopedCheckOverrides> for StoredCheckOverrides {
    /// Persist the exact scope, definitions, and selected values together.
    fn from(selection: ScopedCheckOverrides) -> Self {
        let ScopedCheckOverrides {
            scope,
            configuration,
            values,
        } = selection;
        Self {
            scope,
            configuration,
            values,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredCheckOverrides {
    scope: CheckPreferenceScope,
    configuration: String,
    values: crate::checks::CheckOverrides,
}

/// Retain at least one validated timeout change; omitted fields preserve inherited policy.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "TimeoutFields")]
pub(crate) struct InputTimeoutConfiguration {
    #[serde(skip_serializing_if = "Option::is_none")]
    approval_timeout: Option<InputTimeout>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tool_timeout: Option<InputTimeout>,
}

impl InputTimeoutConfiguration {
    /// Get a validated update for future approval requests, including disabled expiry.
    pub(crate) fn approval_timeout(&self) -> Option<InputTimeout> {
        self.approval_timeout
    }

    /// Get a validated nonzero execution timeout for future tools and shell commands.
    pub(crate) fn tool_timeout(&self) -> Option<InputTimeout> {
        self.tool_timeout
    }
}

impl TryFrom<TimeoutFields> for InputTimeoutConfiguration {
    type Error = TimeoutConfigurationError;

    /// Validate the complete patch before any setting or timer can change.
    fn try_from(fields: TimeoutFields) -> Result<Self, Self::Error> {
        let TimeoutFields {
            approval_timeout,
            tool_timeout,
        } = fields;
        if approval_timeout.is_none() && tool_timeout.is_none() {
            return Err(TimeoutConfigurationError::Empty);
        }
        Ok(Self {
            approval_timeout: approval_timeout
                .map(InputTimeout::validate)
                .transpose()
                .map_err(TimeoutConfigurationError::Approval)?,
            tool_timeout: tool_timeout
                .map(InputTimeout::require_expiry)
                .transpose()
                .map_err(TimeoutConfigurationError::Tool)?,
        })
    }
}

/// Report the committed local policy without returning a provider or transport payload.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "configuration", rename_all = "snake_case")]
pub(crate) enum ConfigurationOutcome {
    /// Session settings committed and any required replacement was retained by the owner.
    Settings {
        disposition: InvocationDisposition,
        /// An explicitly committed override for future review questions; omission preserves it.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        ask_model: Option<crate::settings::AskModel>,
    },
    /// Effective session permission after applying the explicit selection.
    CheckEditing { enabled: bool },
    /// Root execution policy committed and any required replacement was retained by the owner.
    ExecutionPolicy { disposition: InvocationDisposition },
    /// Effective automatic-check admission policy; existing run evidence remains unchanged.
    CheckPreferences {
        check_timing: crate::checks::CheckTiming,
        automatic_checks: bool,
        checks_deferred: bool,
    },
    /// Per-check preferences committed for the captured task and exact project definitions.
    CheckOverrides {
        scope: CheckPreferenceScope,
        overrides: crate::checks::CheckOverrides,
        #[serde(rename = "check_configuration")]
        configuration: Box<crate::checks::ConfigSnapshot>,
    },
    /// Effective inherited deadlines after committing the requested update.
    InputTimeouts {
        approval_timeout: InputTimeout,
        tool_timeout: InputTimeout,
    },
}

/// Report the configuration milestone independently of subsequent native invocation progress.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "disposition", rename_all = "snake_case")]
pub(crate) enum InvocationDisposition {
    /// Policy committed and any continuation was retained by the owner for separate dispatch.
    Committed,
}

/// Describe a rejected configuration without exposing adapter errors or native diagnostics.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, thiserror::Error)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ConfigurationRejection {
    /// The requested model is absent from the normalized catalog.
    #[error("the selected model is unavailable")]
    ModelUnavailable,
    /// The selected model does not offer the requested reasoning choice.
    #[error("the selected reasoning effort is unavailable for this model")]
    EffortUnavailable,
    /// The selected model does not offer the requested context window.
    #[error("the selected context window is unavailable for this model")]
    ContextWindowUnavailable,
    /// The normalized capability or selected model does not support the requested tier.
    #[error("the selected service tier is unavailable")]
    ServiceTierUnavailable,
    /// Native compaction is absent from the normalized capability contract.
    #[error("context compaction is unavailable for this session")]
    CompactionUnavailable,
    /// A threshold is incompatible with the currently reported context capacity.
    #[error("the compaction policy is invalid for the current context capacity")]
    CompactionPolicyInvalid,
    /// The requested plan executor cannot be resolved from supported execution choices.
    #[error("the selected plan executor configuration is unavailable")]
    PlanExecutorUnavailable,
    /// The settings record changed before its requested revision could be applied.
    #[error("settings changed before the selected revision could be applied")]
    SettingsChanged,
    /// An access selection failed host validation before native execution was changed.
    #[error("the selected filesystem access policy is invalid")]
    AccessPolicyInvalid,
    /// The selected host shell is not executable in this environment.
    #[error("the selected check shell is unavailable")]
    ShellUnavailable,
    /// Check preferences cannot change during an active run.
    #[error("check preferences cannot change while a run is active")]
    CheckRunActive,
    /// A selected check no longer exists in the captured definitions.
    #[error("the selected check is unknown")]
    CheckUnknown,
    /// The selected check cannot provide the requested coverage.
    #[error("the selected check does not support this coverage")]
    CheckCoverageUnavailable,
    /// The captured task was replaced or revised before application.
    #[error("the selected check preference task changed")]
    CheckTaskChanged,
    /// Project check definitions changed after selection.
    #[error("check definitions changed before preferences could apply")]
    CheckDefinitionsChanged,
    /// The provider positively rejected the change without altering its configuration.
    #[error("configuration was rejected ({0:?}); existing settings remain active")]
    Provider(forge_provider_api::ProviderFailureKind),
    /// A prerequisite could not be read or validated before any execution effect.
    #[error("configuration prerequisites are unavailable; execution was not changed")]
    Unavailable,
}

impl ConfigurationRejection {
    /// Get a stable failure code without interpreting a provider's message text.
    pub(crate) fn code(self) -> &'static str {
        match self {
            Self::ModelUnavailable => "configuration_model_unavailable",
            Self::EffortUnavailable => "configuration_effort_unavailable",
            Self::ContextWindowUnavailable => "configuration_context_window_unavailable",
            Self::ServiceTierUnavailable => "configuration_service_tier_unavailable",
            Self::CompactionUnavailable => "configuration_compaction_unavailable",
            Self::CompactionPolicyInvalid => "configuration_compaction_policy_invalid",
            Self::PlanExecutorUnavailable => "configuration_plan_executor_unavailable",
            Self::SettingsChanged => "configuration_settings_changed",
            Self::AccessPolicyInvalid => "configuration_access_policy_invalid",
            Self::ShellUnavailable => "configuration_shell_unavailable",
            Self::CheckRunActive => "check_preferences_run_active",
            Self::CheckUnknown => "check_preferences_unknown_check",
            Self::CheckCoverageUnavailable => "check_preferences_coverage_unavailable",
            Self::CheckTaskChanged => "check_preferences_task_changed",
            Self::CheckDefinitionsChanged => "check_preferences_definitions_changed",
            Self::Provider(_) => "configuration_provider_rejected",
            Self::Unavailable => "configuration_unavailable",
        }
    }
}

/// Distinguish malformed timeout patches before command admission.
#[derive(Debug, thiserror::Error)]
pub(crate) enum TimeoutConfigurationError {
    /// An empty patch cannot change timeout policy.
    #[error("timeout configuration requires approval_timeout or tool_timeout")]
    Empty,
    /// Approval timing must fit the platform timer range.
    #[error("invalid approval timeout: {0}")]
    Approval(#[source] forge_provider_api::deadline::InputTimeoutError),
    /// Execution requires a nonzero timeout that fits the platform timer range.
    #[error("invalid tool timeout: {0}")]
    Tool(#[source] forge_provider_api::deadline::InputTimeoutError),
}

/// Decode the supported automatic-check policy fields at the transport/journal boundary.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CheckPreferenceFields {
    check_timing: Option<crate::checks::CheckTiming>,
    automatic_checks: Option<bool>,
    overrides: Option<ScopedCheckOverrides>,
}

/// Decode only supported timeout fields before constructing a valid patch.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TimeoutFields {
    approval_timeout: Option<InputTimeout>,
    tool_timeout: Option<InputTimeout>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Scoped check overrides retain definition provenance and reject mixed global mutation intent.
    #[test]
    fn scoped_check_preferences_preserve_authority_on_recovery() -> anyhow::Result<()> {
        let captured = CheckPreferencesUpdate::capture_overrides(
            None,
            "definitions".into(),
            crate::checks::CheckOverrides::default(),
        )?;
        let stored = serde_json::to_value(&captured)?;
        let restored: CheckPreferencesUpdate = serde_json::from_value(stored.clone())?;
        assert_eq!(restored.overrides().unwrap().configuration(), "definitions");
        assert!(matches!(
            restored.overrides().unwrap().scope(),
            CheckPreferenceScope::Conversation
        ));
        let mut invalid = stored.clone();
        invalid["overrides"]["configuration"] = json!(" ");
        assert!(serde_json::from_value::<CheckPreferencesUpdate>(invalid).is_err());
        let mut mixed = stored;
        mixed["automatic_checks"] = json!(true);
        assert!(serde_json::from_value::<CheckPreferencesUpdate>(mixed).is_err());
        Ok(())
    }

    /// Reject invalid execution deadlines even when decoding a previously admitted command.
    #[test]
    fn timeout_command_decode_preserves_validation() {
        for value in [
            json!({}),
            json!({"tool_timeout":{"amount":0,"unit":"s"}}),
            json!({"tool_timeout":{"amount":1,"unit":"s"},"native_timeout":"x"}),
        ] {
            assert!(serde_json::from_value::<InputTimeoutConfiguration>(value).is_err());
        }
        let valid: InputTimeoutConfiguration = serde_json::from_value(json!({
            "approval_timeout":{"amount":0,"unit":"s"},"tool_timeout":{"amount":1,"unit":"m"},
        }))
        .unwrap();
        assert_eq!(valid.approval_timeout().unwrap().amount, 0);
        assert_eq!(valid.tool_timeout().unwrap().amount, 1);
        let restored: InputTimeoutConfiguration =
            serde_json::from_value(serde_json::to_value(&valid).unwrap()).unwrap();
        assert_eq!(restored.approval_timeout(), valid.approval_timeout());
        assert_eq!(restored.tool_timeout(), valid.tool_timeout());
    }
}
