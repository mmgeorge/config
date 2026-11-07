//! Validate settings patches before they cross the durable command boundary.

use super::SelectionChange;
use serde::{Deserialize, Serialize};

/// Retain an explicit, nonempty settings patch without caller-selected restart authority.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "SettingsFields", into = "SettingsFields")]
pub(crate) struct SessionConfigurationPatch(SettingsFields);

impl SessionConfigurationPatch {
    /// Normalize the restricted queued model selection without caller application/restart authority.
    pub(crate) fn queued_model(
        settings: super::QueuedModelSettings,
    ) -> Result<Self, serde_json::Error> {
        serde_json::from_value(settings.encode())
    }
    /// Require native authority only for invocation changes or provider-backed validation.
    pub(crate) fn requires_native_authority(&self) -> bool {
        let SettingsFields {
            model,
            effort,
            context_window,
            service_tier,
            access,
            approval_mode,
            shell,
            auto_compaction: _,
            check_timing: _,
            automatic_checks: _,
            approval_timeout: _,
            tool_timeout: _,
            subagent_tools,
            lsp_tools,
            session_tools,
            status_thoughts: _,
            plan_executor_enabled,
            plan_executor_model,
            plan_executor_effort,
            plan_compact: _,
            settings_revision,
            ask_model,
            setting_fields: _,
            session_policy: _,
        } = &self.0;
        model.is_some()
            || effort.is_some()
            || !matches!(context_window, SelectionChange::Keep)
            || service_tier.is_some()
            || access.is_some()
            || approval_mode.is_some()
            || shell.is_some()
            || subagent_tools.is_some()
            || lsp_tools.is_some()
            || session_tools.is_some()
            || *plan_executor_enabled == Some(true)
            || plan_executor_model.is_some()
            || plan_executor_effort.is_some()
            || settings_revision.is_some()
            || matches!(ask_model, SelectionChange::Set(selection) if selection.model.is_some())
    }

    /// Create the owner's resolved session policy without persisting an admission-time toggle result.
    pub(crate) fn execution_policy(
        access: Option<crate::session::AccessPolicy>,
        approval_mode: crate::session::ApprovalMode,
    ) -> Self {
        Self(SettingsFields {
            access,
            approval_mode: Some(approval_mode),
            session_policy: true,
            ..Default::default()
        })
    }

    /// Get the externally committed settings revision this command must apply exactly.
    pub(crate) fn application_revision(&self) -> Option<u64> {
        self.0.settings_revision
    }

    /// Get an explicit question-model override, including restoration of model inheritance.
    pub(crate) fn question_model(&self) -> Option<crate::settings::AskModel> {
        match &self.0.ask_model {
            SelectionChange::Keep => None,
            SelectionChange::Clear => Some(crate::settings::AskModel::default()),
            SelectionChange::Set(selection) => Some(selection.to_legacy()),
        }
    }

    /// Encode the existing host reducer's configuration shape during its migration.
    pub(crate) fn encode_legacy(&self) -> Result<serde_json::Value, serde_json::Error> {
        serde_json::to_value(&self.0)
    }
}

impl TryFrom<SettingsFields> for SessionConfigurationPatch {
    type Error = SettingsPatchError;

    /// Reject invalid values in live requests and recovered command records alike.
    fn try_from(mut fields: SettingsFields) -> Result<Self, Self::Error> {
        let SettingsFields {
            model,
            effort,
            context_window,
            service_tier,
            access,
            approval_mode,
            shell,
            auto_compaction,
            check_timing,
            automatic_checks,
            approval_timeout,
            tool_timeout,
            subagent_tools,
            lsp_tools,
            session_tools,
            status_thoughts,
            plan_executor_enabled,
            plan_executor_model,
            plan_executor_effort,
            plan_compact,
            settings_revision,
            ask_model,
            setting_fields: _,
            session_policy: _,
        } = &fields;
        if model.is_none()
            && effort.is_none()
            && matches!(context_window, SelectionChange::Keep)
            && service_tier.is_none()
            && access.is_none()
            && approval_mode.is_none()
            && shell.is_none()
            && auto_compaction.is_none()
            && check_timing.is_none()
            && automatic_checks.is_none()
            && approval_timeout.is_none()
            && tool_timeout.is_none()
            && subagent_tools.is_none()
            && lsp_tools.is_none()
            && session_tools.is_none()
            && status_thoughts.is_none()
            && plan_executor_enabled.is_none()
            && plan_executor_model.is_none()
            && plan_executor_effort.is_none()
            && plan_compact.is_none()
            && settings_revision.is_none()
            && matches!(ask_model, SelectionChange::Keep)
        {
            return Err(SettingsPatchError::Empty);
        }
        if let Some(timeout) = fields.approval_timeout {
            fields.approval_timeout = Some(
                timeout
                    .validate()
                    .map_err(SettingsPatchError::ApprovalTimeout)?,
            );
        }
        if let Some(timeout) = fields.tool_timeout {
            fields.tool_timeout = Some(
                timeout
                    .require_expiry()
                    .map_err(SettingsPatchError::ToolTimeout)?,
            );
        }
        if fields
            .auto_compaction
            .as_ref()
            .is_some_and(|policy| policy.threshold_tokens == 0)
        {
            return Err(SettingsPatchError::CompactionThreshold);
        }
        Ok(Self(fields))
    }
}

impl From<SessionConfigurationPatch> for SettingsFields {
    /// Encode a validated patch without changing absent-versus-cleared selections.
    fn from(patch: SessionConfigurationPatch) -> Self {
        patch.0
    }
}

/// Distinguish malformed settings before admission or journal recovery.
#[derive(Debug, thiserror::Error)]
pub(crate) enum SettingsPatchError {
    /// Tuning an inherited question model requires an explicit model selection.
    #[error("question-model effort or service tier requires an explicit model")]
    QuestionModelRequired,
    /// Metadata alone does not request a configuration change.
    #[error("session configuration requires at least one setting")]
    Empty,
    /// Catalog identities have an open vocabulary, but cannot be empty.
    #[error("a model, effort, or context-window selection cannot be empty")]
    EmptySelection,
    /// Compaction must use a positive retained-token threshold.
    #[error("compaction threshold must be greater than zero")]
    CompactionThreshold,
    /// Approval timers must fit the platform timer range.
    #[error("invalid approval timeout: {0}")]
    ApprovalTimeout(#[source] crate::session::InputTimeoutError),
    /// Tool execution must have a nonzero deadline.
    #[error("invalid tool timeout: {0}")]
    ToolTimeout(#[source] crate::session::InputTimeoutError),
}

macro_rules! catalog_selection {
    ($name:ident, $description:literal) => {
        #[doc = $description]
        #[derive(Clone, Debug, Deserialize, Serialize)]
        #[serde(try_from = "String", into = "String")]
        struct $name(String);

        impl TryFrom<String> for $name {
            type Error = SettingsPatchError;

            /// Validate an open catalog identity without inventing provider vocabulary.
            fn try_from(value: String) -> Result<Self, Self::Error> {
                if value.trim().is_empty() {
                    return Err(SettingsPatchError::EmptySelection);
                }
                Ok(Self(value))
            }
        }

        impl From<$name> for String {
            /// Encode the selected identity at the persistence/reducer boundary.
            fn from(value: $name) -> Self {
                value.0
            }
        }
    };
}

catalog_selection!(
    ModelSelection,
    "Identify a model from the normalized provider catalog."
);
catalog_selection!(
    EffortSelection,
    "Identify a reasoning choice from the normalized model catalog."
);
catalog_selection!(
    ContextWindowSelection,
    "Identify a context-window choice from the normalized model catalog."
);

/// Retain a validated override for future review-question interactions, independent of root execution.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "QuestionModelFields")]
struct QuestionModelSelection {
    #[serde(skip_serializing_if = "Option::is_none")]
    model: Option<ModelSelection>,
    #[serde(skip_serializing_if = "Option::is_none")]
    effort: Option<EffortSelection>,
    #[serde(skip_serializing_if = "Option::is_none")]
    service_tier: Option<crate::provider::ServiceTier>,
}

impl QuestionModelSelection {
    /// Encode already-validated open catalog identities for the existing question-model store.
    fn to_legacy(&self) -> crate::settings::AskModel {
        crate::settings::AskModel {
            model: self.model.clone().map(String::from),
            effort: self.effort.clone().map(String::from),
            service_tier: self.service_tier,
        }
    }
}

impl TryFrom<QuestionModelFields> for QuestionModelSelection {
    type Error = SettingsPatchError;

    /// Reject incomplete overrides when admitting or recovering an owner command.
    fn try_from(fields: QuestionModelFields) -> Result<Self, Self::Error> {
        let QuestionModelFields {
            model,
            effort,
            service_tier,
        } = fields;
        if model.is_none() && (effort.is_some() || service_tier.is_some()) {
            return Err(SettingsPatchError::QuestionModelRequired);
        }
        Ok(Self {
            model,
            effort,
            service_tier,
        })
    }
}

/// Decode omitted question-model fields as inherited policy, while rejecting explicit null tuning.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct QuestionModelFields {
    #[serde(default, deserialize_with = "present_value")]
    model: Option<ModelSelection>,
    #[serde(default, deserialize_with = "present_value")]
    effort: Option<EffortSelection>,
    #[serde(default, deserialize_with = "present_value")]
    service_tier: Option<crate::provider::ServiceTier>,
}

/// Decode only supported settings; restart/resume and native payloads are never accepted.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
struct SettingsFields {
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    model: Option<ModelSelection>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    effort: Option<EffortSelection>,
    #[serde(
        deserialize_with = "decode_selection",
        serialize_with = "encode_selection",
        skip_serializing_if = "keep_selection"
    )]
    context_window: SelectionChange<ContextWindowSelection>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    service_tier: Option<crate::provider::ServiceTier>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    access: Option<crate::session::AccessPolicy>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    shell: Option<crate::session::CheckShell>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    auto_compaction: Option<crate::session::AutoCompaction>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    check_timing: Option<crate::checks::CheckTiming>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    automatic_checks: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    approval_timeout: Option<crate::session::InputTimeout>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    tool_timeout: Option<crate::session::InputTimeout>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    subagent_tools: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    lsp_tools: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    session_tools: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    status_thoughts: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    plan_executor_enabled: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    plan_executor_model: Option<ModelSelection>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    plan_executor_effort: Option<EffortSelection>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    plan_compact: Option<bool>,
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    settings_revision: Option<u64>,
    #[serde(
        deserialize_with = "decode_selection",
        serialize_with = "encode_selection",
        skip_serializing_if = "keep_selection"
    )]
    ask_model: SelectionChange<QuestionModelSelection>,
    /// Audit labels are presentation metadata, never authority for replacement decisions.
    #[serde(
        deserialize_with = "present_value",
        skip_serializing_if = "Option::is_none"
    )]
    setting_fields: Option<Vec<String>>,
    session_policy: bool,
}

/// Reject explicit null for settings that have no clear/inherit operation.
fn present_value<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    T::deserialize(deserializer).map(Some)
}

/// Decode explicit null as clearing, preserving omission as the default Keep selection.
fn decode_selection<'de, D, T>(deserializer: D) -> Result<SelectionChange<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Ok(match Option::<T>::deserialize(deserializer)? {
        Some(value) => SelectionChange::Set(value),
        None => SelectionChange::Clear,
    })
}

/// Encode nullable settings without inventing sentinel catalog identities.
fn encode_selection<S, T>(selection: &SelectionChange<T>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
    T: Serialize,
{
    match selection {
        SelectionChange::Set(value) => value.serialize(serializer),
        SelectionChange::Clear | SelectionChange::Keep => serializer.serialize_none(),
    }
}

/// Omit Keep so decoding it cannot accidentally become Clear.
fn keep_selection<T>(selection: &SelectionChange<T>) -> bool {
    matches!(selection, SelectionChange::Keep)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Preserve nullable selection semantics and reject internal lifecycle authority on reload.
    #[test]
    fn settings_patch_roundtrip_preserves_clear_and_rejects_invalid_intent() {
        for value in [
            json!({"context_window":null}),
            json!({"context_window":"large"}),
            json!({"model":"model-a"}),
            json!({"ask_model":null}),
            json!({"ask_model":{}}),
            json!({"model":"model-a","ask_model":{"model":"question-model","effort":"high"}}),
        ] {
            let patch: SessionConfigurationPatch = serde_json::from_value(value.clone()).unwrap();
            let encoded = patch.encode_legacy().unwrap();
            assert_eq!(encoded.get("context_window"), value.get("context_window"));
            assert_eq!(encoded.get("ask_model"), value.get("ask_model"));
            let restored: SessionConfigurationPatch =
                serde_json::from_value(encoded.clone()).unwrap();
            assert_eq!(restored.encode_legacy().unwrap(), encoded);
        }
        for value in [
            json!({}),
            json!({"validate":true}),
            json!({"model":" "}),
            json!({"model":42}),
            json!({"ask_model":{"effort":"high"}}),
            json!({"ask_model":{"model":" "}}),
            json!({"ask_model":{"model":"question-model","native_mode":"x"}}),
            json!({"model":"a","resume":true}),
            json!({"tool_timeout":{"amount":0,"unit":"s"}}),
        ] {
            assert!(serde_json::from_value::<SessionConfigurationPatch>(value).is_err());
        }
    }
}
