use anyhow::{Context, Result, ensure};
use rusqlite::{OptionalExtension, params};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

const SETTINGS_FORMAT_VERSION: u32 = 5;

enum SettingLayerChange<'setting> {
    Replace {
        expected_revision: u64,
        setting: &'setting PortableSettings,
    },
    Patch(&'setting PortableSettings),
}

#[derive(Deserialize, Serialize)]
struct SettingsRecord<Payload> {
    format_version: u32,
    payload: Payload,
}

impl<Payload> SettingsRecord<Payload> {
    fn encode(payload: &Payload) -> Result<String>
    where
        Payload: Serialize,
    {
        Ok(serde_json::to_string(&SettingsRecord {
            format_version: SETTINGS_FORMAT_VERSION,
            payload,
        })?)
    }

    fn decode(encoded: &str) -> Result<Option<Payload>>
    where
        Payload: serde::de::DeserializeOwned,
    {
        let record: Value = serde_json::from_str(encoded).context("read saved settings record")?;
        if record.get("format_version").and_then(Value::as_u64)
            != Some(u64::from(SETTINGS_FORMAT_VERSION))
        {
            anyhow::bail!("unsupported saved settings record format");
        }
        let record: Self =
            serde_json::from_value(record).context("decode current settings record")?;
        Ok(Some(record.payload))
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects preference inheritance without coupling preferences to conversation formats.
pub enum SettingScope {
    /// Applies to every workspace and provider unless overridden.
    Global,
    /// Overrides global values for one canonical workspace.
    Workspace,
    /// Overrides inherited values for one Harness conversation.
    Session,
}

/// Identifies the preference layer and its workspace and session boundaries.
#[derive(Clone, Copy)]
pub(crate) struct SettingTarget<'scope> {
    pub scope: SettingScope,
    pub workspace: &'scope str,
    pub session: &'scope str,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Retains portable choices without copying provider authentication or native files.
pub struct PortableSettings {
    /// Default approval policy for newly created sessions.
    pub approval_mode: Option<crate::session::ApprovalMode>,
    /// Requests native compaction after completed turns reach this token threshold.
    pub auto_compaction: Option<crate::session::AutoCompaction>,
    /// Controls reported file authorization and requested OS isolation independently of approval mode.
    pub access: Option<crate::session::AccessPolicy>,
    /// Selects the check shell used by Harness execution.
    pub shell: Option<crate::session::CheckShell>,
    /// Automatic validation boundary for the editor's work queue.
    pub check_timing: Option<crate::checks::CheckTiming>,
    /// Enables automatic completion checks, defaulting to disabled when omitted.
    pub automatic_checks: Option<bool>,
    /// Allows the root agent to edit project checks in this conversation.
    pub check_editing: Option<bool>,
    /// Selects compaction before accepted plan execution.
    pub plan_compact: Option<bool>,
    /// Overrides the duration of future approval requests.
    pub approval_timeout: Option<crate::session::InputTimeout>,
    /// Limits each tool invocation and shell command; defaults to ten minutes.
    pub tool_timeout: Option<crate::session::InputTimeout>,
    /// Enables the root's model-facing child tools without restricting manual admission.
    pub subagent_tools: Option<bool>,
    /// Enables editor-backed code queries and permission-checked rename tools.
    pub lsp_tools: Option<bool>,
    /// Exposes read-only session history tools independently of configuration inspection.
    pub session_tools: Option<bool>,
    /// Shows the latest thought beside live status, enabled by default.
    pub status_thoughts: Option<bool>,
    #[serde(default)]
    /// Records selection by source identity, independent of provider display names.
    pub skill: BTreeMap<String, bool>,
    #[serde(default)]
    /// Records server selection by catalog identity.
    pub mcp: BTreeMap<String, bool>,
}

impl PortableSettings {
    /// Resolve completion-check enablement without changing inherited settings.
    pub fn automatic_checks_enabled(&self) -> bool {
        self.automatic_checks.unwrap_or(false)
    }

    fn inherit(&mut self, overriding: Self) {
        if overriding.approval_mode.is_some() {
            self.approval_mode = overriding.approval_mode;
        }
        if overriding.auto_compaction.is_some() {
            self.auto_compaction = overriding.auto_compaction;
        }
        if overriding.access.is_some() {
            self.access = overriding.access;
        }
        if overriding.shell.is_some() {
            self.shell = overriding.shell;
        }
        if overriding.check_timing.is_some() {
            self.check_timing = overriding.check_timing;
        }
        if overriding.automatic_checks.is_some() {
            self.automatic_checks = overriding.automatic_checks;
        }
        if overriding.check_editing.is_some() {
            self.check_editing = overriding.check_editing;
        }
        if overriding.plan_compact.is_some() {
            self.plan_compact = overriding.plan_compact;
        }
        if overriding.subagent_tools.is_some() {
            self.subagent_tools = overriding.subagent_tools;
        }
        if overriding.lsp_tools.is_some() {
            self.lsp_tools = overriding.lsp_tools;
        }
        if overriding.session_tools.is_some() {
            self.session_tools = overriding.session_tools;
        }
        if overriding.status_thoughts.is_some() {
            self.status_thoughts = overriding.status_thoughts;
        }
        if overriding.approval_timeout.is_some() {
            self.approval_timeout = overriding.approval_timeout;
        }
        if overriding.tool_timeout.is_some() {
            self.tool_timeout = overriding.tool_timeout;
        }
        self.skill.extend(overriding.skill);
        self.mcp.extend(overriding.mcp);
    }

    /// Apply validated portable choices while preserving provider-local model identity.
    pub fn apply(
        &self,
        session: &mut crate::session::HarnessSession,
        native_compact: bool,
    ) -> Result<()> {
        if let Some(mode) = self.approval_mode {
            session.approval_mode = mode;
        }
        if let Some(policy) = &self.auto_compaction {
            policy.validate(session.context_usage.as_ref())?;
            session.auto_compaction = policy.clone();
        }
        if let Some(access) = &self.access {
            session.access = access.clone().validate()?;
        }
        session.access = forge_providers::registry::resolve(&forge_providers::registry::select(
            &session.provider,
        )?)?
        .capability
        .resolve_access(session.access.clone());
        if let Some(shell) = self.shell {
            shell.executable()?;
            session.shell = shell;
        }
        if let Some(enabled) = self.plan_compact {
            session.plan_compact = enabled && native_compact;
        }
        if let Some(timeout) = self.approval_timeout {
            session.approval_timeout = timeout.validate()?;
        }
        if let Some(timeout) = self.tool_timeout {
            session.tool_timeout = timeout.require_expiry()?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Carries optimistic concurrency across windows, processes, and providers.
pub struct SettingsSnapshot {
    /// Changes on every accepted preference mutation.
    pub revision: u64,
    /// Contains the resolved global, workspace, and session preferences.
    pub desired: PortableSettings,
    /// Contains only this provider's model and tuning choices.
    pub provider: Value,
    /// Identifies the settings revision at which the global provider defaults were last saved
    /// explicitly, or none for defaults saved before saves recorded it.
    #[serde(skip)]
    pub provider_revision: Option<u64>,
}

/// Names the provider-layer field that records the revision of its last explicit save.
const PROVIDER_REVISION_FIELD: &str = "confirmed_revision";

impl SettingsSnapshot {
    /// Get whether the workspace's last confirmed model choice is newer than the global
    /// provider defaults, so a new session in that workspace starts from the workspace choice.
    ///
    /// A choice confirmed in a session belongs to its workspace and provider. An explicit edit of
    /// the global defaults afterwards is the newer default for every workspace, so the most recent
    /// of the two decides. A workspace record that never confirmed a choice, such as the one a
    /// first session writes, yields to the global defaults.
    pub fn workspace_choice_is_newer(
        &self,
        preference: Option<&crate::session::HarnessPreference>,
    ) -> bool {
        match (
            preference.and_then(|preference| preference.confirmed_revision),
            self.provider_revision,
        ) {
            (Some(workspace), Some(global)) => workspace >= global,
            (Some(_), None) => true,
            (None, _) => false,
        }
    }

    /// Apply provider-local defaults only when creating a conversation, unless the workspace has
    /// confirmed a newer choice of its own (`workspace_choice_is_newer`).
    pub fn apply_provider_defaults(
        &self,
        session: &mut crate::session::HarnessSession,
        preference: Option<&crate::session::HarnessPreference>,
    ) -> Result<()> {
        if self.workspace_choice_is_newer(preference) {
            return Ok(());
        }
        if let Some(model) = self.provider.get("model").and_then(Value::as_str) {
            ensure!(!model.trim().is_empty(), "saved model cannot be empty");
            // Saved configuration does not confirm the model used by a native turn, and a
            // label learned for a different model no longer names this one.
            let model_label = (session.model == model)
                .then(|| session.model_label.clone())
                .flatten();
            session.select_model(model.to_owned(), model_label);
        }
        if let Some(effort) = self.provider.get("effort").and_then(Value::as_str) {
            ensure!(!effort.trim().is_empty(), "saved effort cannot be empty");
            session.effort = effort.to_owned();
        }
        if let Some(value) = self.provider.get("service_tier") {
            session.service_tier = serde_json::from_value(value.clone())?;
        }
        if let Some(value) = self.provider.get("context_window") {
            session.context_window = serde_json::from_value(value.clone())?;
        }
        if let Some(value) = self.provider.get("plan_executor") {
            session.plan_executor = serde_json::from_value(value.clone())?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Separates persisted intent from the configuration confirmed for one conversation.
pub struct SettingsApplication {
    /// Identifies the newest configuration operation accepted for this conversation.
    pub operation_id: Option<String>,
    /// Identifies the preferences the operation attempts to apply.
    pub desired_revision: u64,
    /// Identifies the last successfully applied preference revision.
    pub applied_revision: Option<u64>,
    /// Increments after each confirmed configuration application.
    pub generation: u64,
    /// Reports application independently of any subsequent execution resume.
    pub state: String,
    /// Retains the last confirmed portable configuration after failed applications.
    pub effective: Option<PortableSettings>,
}

/// Keeps catalog bindings and enablement intent in the same committed database snapshot.
pub(crate) struct CatalogSnapshot {
    /// Contains the preference revision governing this catalog.
    pub(crate) settings: SettingsSnapshot,
    /// Contains shared and explicitly imported skill sources.
    pub(crate) skill: Vec<super::SkillResource>,
    /// Contains shared and explicitly imported MCP definitions without resolved credentials.
    pub(crate) server: Vec<super::catalog::ServerResource>,
}

/// Owns Forge preference persistence separately from provider configuration files.
///
/// The connection opens history as its main database and attaches settings as `preferences`,
/// so the session layer and application snapshots it writes beside portable settings commit in
/// one transaction that locks history before settings. The connection also retains the state
/// version lease.
pub struct SettingsStore {
    connection: crate::storage::database::HistoryConnection,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains one accepted preference revision and the fields changed by that mutation.
pub(crate) struct SettingsMutation {
    /// Identifies the desired state committed before runtime application begins.
    pub(crate) revision: u64,
    /// Lists changed field names without recording preference values in the timeline.
    pub(crate) field: Vec<String>,
}

/// Carry a configuration's recovery intent and how long its settings half may retry contention.
pub(crate) struct IntentCommit<'intent> {
    pub(crate) intent: &'intent crate::storage::configuration::ConfigurationIntent,
    pub(crate) retry_until: Option<std::time::Instant>,
}

#[derive(Deserialize, Serialize)]
struct SettingsReceipt {
    request: Value,
    mutation: SettingsMutation,
}

impl SettingsStore {
    #[cfg(test)]
    pub(crate) fn execute_test_sql(&self, sql: &str) -> Result<()> {
        self.connection.execute_batch(sql)?;
        Ok(())
    }
    /// Read persistent settings without creating or repairing their schema.
    pub(crate) fn read_only(data_root: &Path) -> Result<Self> {
        crate::storage::layout::prepare(data_root)?;
        Self::with_layer_view(crate::storage::database::HistoryConnection::read_only(
            data_root,
        )?)
    }

    /// Open persistent settings writable beside their disposable application state.
    pub fn open(data_root: &Path) -> Result<Self> {
        Self::with_layer_view(
            crate::storage::database::HistoryConnection::open_settings_writer(data_root)?,
        )
    }

    /// Present persistent and session layers as one table for reads and writes.
    fn with_layer_view(connection: crate::storage::database::HistoryConnection) -> Result<Self> {
        connection.execute_batch("CREATE TEMP VIEW setting_layer AS SELECT * FROM preferences.persistent_setting_layer UNION ALL SELECT * FROM main.session_setting_layer;
            CREATE TEMP TRIGGER setting_layer_insert INSTEAD OF INSERT ON setting_layer BEGIN
            INSERT INTO persistent_setting_layer SELECT NEW.scope,NEW.identity,NEW.payload WHERE NEW.scope <> 'session'
                ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload;
            INSERT INTO session_setting_layer SELECT NEW.scope,NEW.identity,NEW.payload WHERE NEW.scope = 'session'
                ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload;
            END;")?;
        Ok(Self { connection })
    }

    /// Register the physical dotfiles root without importing native preference state.
    pub fn configure_catalog(&self, config_dir: &Path) -> Result<()> {
        let root = dunce::canonicalize(config_dir).context("config_dir does not exist")?;
        self.connection.execute("INSERT INTO setting_catalog VALUES (1,?1) ON CONFLICT(id) DO UPDATE SET root=excluded.root",
            [root.to_string_lossy().as_ref()])?;
        Ok(())
    }

    /// Resolve the global Rulesync definition source independently of the active workspace.
    pub fn catalog_root(&self) -> Result<Option<PathBuf>> {
        Ok(self
            .connection
            .query_row("SELECT root FROM setting_catalog WHERE id=1", [], |row| {
                row.get::<_, String>(0)
            })
            .optional()?
            .map(PathBuf::from))
    }

    /// Read catalog additions and disabled-by-default selections from one SQLite snapshot.
    pub(crate) fn catalog(
        &self,
        workspace: &str,
        session: &str,
        provider: &str,
    ) -> Result<CatalogSnapshot> {
        let transaction = rusqlite::Transaction::new_unchecked(
            &self.connection,
            rusqlite::TransactionBehavior::Deferred,
        )?;
        let settings = self.get(workspace, session, provider)?;
        let skill = self.skills(workspace)?;
        let server = self.servers(workspace)?;
        transaction.commit()?;
        Ok(CatalogSnapshot {
            settings,
            skill,
            server,
        })
    }

    /// Resolve shared and explicitly imported skills for one workspace.
    pub(crate) fn skills(&self, workspace: &str) -> Result<Vec<super::SkillResource>> {
        let mut skill =
            super::ResourceCatalog::skills(self.catalog_root()?.as_deref(), Path::new(workspace))?;
        for (scope, resource) in self.imported(workspace)? {
            if let super::ImportedResource::Skill { mut resource } = resource {
                resource.scope = Some(scope);
                skill.push(resource);
            }
        }
        Ok(skill)
    }

    /// Resolve shared and explicitly imported MCP definitions without credential values.
    pub(crate) fn servers(&self, workspace: &str) -> Result<Vec<super::catalog::ServerResource>> {
        let mut server =
            super::ResourceCatalog::servers(self.catalog_root()?.as_deref(), Path::new(workspace))?;
        for (scope, resource) in self.imported(workspace)? {
            if let Some(mut resource) = resource.server()? {
                resource.scope = Some(scope);
                server.push(resource);
            }
        }
        Ok(server)
    }

    fn imported(
        &self,
        workspace: &str,
    ) -> Result<Vec<(crate::provider::ResourceScope, super::ImportedResource)>> {
        let mut statement = self.connection.prepare(
            "SELECT scope, payload FROM setting_resource
            WHERE scope='global' OR (scope='workspace' AND identity=?1) ORDER BY id",
        )?;
        statement
            .query_map([workspace], |row| {
                Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
            })?
            .map(|entry| {
                let (scope, payload) = entry?;
                let scope = match scope.as_str() {
                    "global" => crate::provider::ResourceScope::Global,
                    "workspace" => crate::provider::ResourceScope::Workspace,
                    _ => anyhow::bail!("unsupported resource scope {scope}"),
                };
                Ok((scope, serde_json::from_str(&payload)?))
            })
            .collect()
    }

    /// Preview native definitions and report collisions against the current effective selection.
    pub(crate) fn preview_import(
        &self,
        kind: super::ImportKind,
        source: &Path,
        scope: SettingScope,
        workspace: &str,
        session: &str,
        provider: &str,
    ) -> Result<super::NativeImport> {
        let namespace = match scope {
            SettingScope::Global => "global".to_owned(),
            SettingScope::Workspace => format!("workspace:{workspace}"),
            SettingScope::Session => {
                anyhow::bail!("Resource imports require global or workspace scope")
            }
        };
        let mut preview =
            super::NativeImport::read(kind, source, &namespace, Path::new(workspace))?;
        let catalog = self.catalog(workspace, session, provider)?;
        preview.revision = catalog.settings.revision;
        let desired = catalog.settings.desired;
        for candidate in &mut preview.candidate {
            candidate.conflict = if candidate.kind == "skill" {
                catalog
                    .skill
                    .iter()
                    .filter(|resource| {
                        resource.name == candidate.name
                            && resource.id != candidate.id
                            && desired.skill.get(&resource.id) != Some(&false)
                    })
                    .map(|resource| resource.id.clone())
                    .collect()
            } else {
                catalog
                    .server
                    .iter()
                    .filter(|resource| {
                        resource.name == candidate.name
                            && resource.id != candidate.id
                            && desired.mcp.get(&resource.id) != Some(&false)
                    })
                    .map(|resource| resource.id.clone())
                    .collect()
            };
        }
        Ok(preview)
    }

    /// Resolve current portable layers and provider-local tuning without implicit imports.
    /// Reject unsupported or malformed saved settings without replacing user choices.
    pub fn get(&self, workspace: &str, session: &str, provider: &str) -> Result<SettingsSnapshot> {
        let (revision, global, workspace, session, provider) = self.connection.query_row(
            "SELECT revision,
                (SELECT payload FROM setting_layer WHERE scope='global' AND identity=''),
                (SELECT payload FROM setting_layer WHERE scope='workspace' AND identity=?1),
                (SELECT payload FROM setting_layer WHERE scope='session' AND identity=?2),
                (SELECT payload FROM setting_layer WHERE scope='provider' AND identity=?3)
             FROM setting_revision WHERE id=1",
            params![workspace, session, provider],
            |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, Option<String>>(1)?,
                    row.get::<_, Option<String>>(2)?,
                    row.get::<_, Option<String>>(3)?,
                    row.get::<_, Option<String>>(4)?,
                ))
            },
        )?;
        let mut desired = PortableSettings::default();
        for payload in [global, workspace, session].into_iter().flatten() {
            if let Some(settings) = SettingsRecord::decode(&payload)? {
                desired.inherit(settings);
            }
        }
        let mut provider = provider
            .map(|payload| serde_json::from_str(&payload))
            .transpose()?
            .unwrap_or(Value::Null);
        // The save revision orders the defaults against workspace choices; readers of the
        // defaults themselves never see it.
        let provider_revision = provider
            .as_object_mut()
            .and_then(|fields| fields.remove(PROVIDER_REVISION_FIELD))
            .map(|value| {
                value
                    .as_u64()
                    .context("saved provider default revision is not a number")
            })
            .transpose()?;
        Ok(SettingsSnapshot {
            revision: u64::try_from(revision)?,
            desired,
            provider,
            provider_revision,
        })
    }

    /// Replace one complete layer atomically, rejecting stale revisions without effects.
    pub fn update(
        &mut self,
        expected_revision: u64,
        scope: SettingScope,
        workspace: &str,
        session: &str,
        setting: &PortableSettings,
    ) -> Result<u64> {
        Ok(self
            .write_layer(
                SettingLayerChange::Replace {
                    expected_revision,
                    setting,
                },
                crate::settings::SettingTarget {
                    scope,
                    workspace,
                    session,
                },
                None,
                &[],
            )?
            .revision)
    }

    /// Merge explicit fields into the latest layer while holding its write transaction.
    pub(crate) fn patch(
        &mut self,
        target: SettingTarget<'_>,
        setting: &PortableSettings,
    ) -> Result<u64> {
        Ok(self
            .write_layer(SettingLayerChange::Patch(setting), target, None, &[])?
            .revision)
    }

    /// Commit the host's recovery intent before writing settings without holding either database lock across the other writer.
    ///
    /// Only a command that applies an external settings revision passes `expected_revision`;
    /// every other configuration merges its explicit fields at the current revision.
    pub(crate) fn commit_configuration<T>(
        &mut self,
        expected_revision: Option<u64>,
        target: SettingTarget<'_>,
        portable: Option<&PortableSettings>,
        provider: Option<(&str, &Value)>,
        intent: Option<IntentCommit<'_>>,
        apply: impl FnOnce() -> Result<T>,
    ) -> Result<T> {
        if let Some(expected_revision) = expected_revision {
            let revision = u64::try_from(self.connection.query_row(
                "SELECT revision FROM setting_revision WHERE id=1",
                [],
                |row| row.get::<_, i64>(0),
            )?)?;
            ensure!(
                revision == expected_revision,
                "settings revision conflict: expected {expected_revision}, current {revision}"
            );
        }
        let value = apply()?;
        if let Some(IntentCommit {
            intent,
            retry_until,
        }) = intent
        {
            // The host half has committed, and replaying an intent is idempotent, so contention
            // retries without risking a second application.
            crate::storage::database::retry_while_busy(retry_until, || {
                self.recover_configuration(intent)
            })?;
        } else {
            let transaction = self
                .connection
                .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
            let revision = u64::try_from(transaction.query_row(
                "SELECT revision FROM setting_revision WHERE id=1",
                [],
                |row| row.get::<_, i64>(0),
            )?)?;
            Self::write_configuration(
                &transaction,
                revision,
                target,
                portable,
                provider,
                None,
                || Ok(()),
            )?;
            transaction.commit()?;
        }
        Ok(value)
    }

    /// Apply the settings half of a configuration whose session half already committed.
    ///
    /// The host commits the session and its intent row before the settings transaction, so a
    /// process that stops between the two leaves the intent for recovery. Another session may
    /// have advanced the settings revision in the meantime; the intent holds explicit fields
    /// rather than a whole document, so they are merged at the current revision instead of
    /// failing the captured-revision check on every reopen. A replay of an intent the settings
    /// store already recorded is a no-op, checked inside the same write transaction.
    pub(crate) fn recover_configuration(
        &mut self,
        intent: &crate::storage::configuration::ConfigurationIntent,
    ) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let recorded: Option<String> = transaction
            .query_row(
                "SELECT session_id FROM setting_operation WHERE id=?1",
                [&intent.id],
                |row| row.get(0),
            )
            .optional()?;
        if let Some(owner) = recorded {
            ensure!(
                owner == intent.session,
                "settings operation belongs to another session"
            );
            return Ok(());
        }
        let revision = u64::try_from(transaction.query_row(
            "SELECT revision FROM setting_revision WHERE id=1",
            [],
            |row| row.get::<_, i64>(0),
        )?)?;
        Self::write_configuration(
            &transaction,
            revision,
            SettingTarget {
                scope: intent.scope,
                workspace: &intent.workspace,
                session: &intent.session,
            },
            intent.portable.as_ref(),
            intent
                .provider_setting
                .as_ref()
                .map(|value| (intent.provider.as_str(), value)),
            Some(intent),
            || Ok(()),
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Merge one configuration's explicit fields at `revision` inside an open write transaction.
    fn write_configuration<T>(
        transaction: &rusqlite::Transaction<'_>,
        revision: u64,
        target: SettingTarget<'_>,
        portable: Option<&PortableSettings>,
        provider: Option<(&str, &Value)>,
        intent: Option<&crate::storage::configuration::ConfigurationIntent>,
        apply: impl FnOnce() -> Result<T>,
    ) -> Result<T> {
        let mut next = revision;
        if let Some(patch) = portable {
            let (scope, identity) = match target.scope {
                SettingScope::Global => ("global", ""),
                SettingScope::Workspace => ("workspace", target.workspace),
                SettingScope::Session => ("session", target.session),
            };
            let payload = transaction
                .query_row(
                    "SELECT payload FROM setting_layer WHERE scope=?1 AND identity=?2",
                    params![scope, identity],
                    |row| row.get::<_, String>(0),
                )
                .optional()?;
            let mut setting = payload
                .map(|payload| SettingsRecord::<PortableSettings>::decode(&payload))
                .transpose()?
                .flatten()
                .unwrap_or_default();
            setting.inherit(patch.clone());
            if let Some(policy) = &setting.auto_compaction {
                policy.validate(None)?;
            }
            if let Some(timeout) = setting.approval_timeout {
                timeout.validate()?;
            }
            if let Some(timeout) = setting.tool_timeout {
                timeout.require_expiry()?;
            }
            if let Some(access) = &setting.access {
                access.clone().validate()?;
            }
            if let Some(shell) = setting.shell {
                shell.executable()?;
            }
            transaction.execute(
                "INSERT INTO setting_layer VALUES (?1,?2,?3)",
                params![scope, identity, SettingsRecord::encode(&setting)?],
            )?;
            next = next.checked_add(1).context("settings revision exhausted")?;
        }
        if let Some((provider, value)) = provider {
            let fields = value
                .as_object()
                .context("provider preferences must be an object")?;
            let payload = transaction
                .query_row(
                    "SELECT payload FROM setting_layer WHERE scope='provider' AND identity=?1",
                    [provider],
                    |row| row.get::<_, String>(0),
                )
                .optional()?;
            let mut preference: Value = payload
                .map(|payload| serde_json::from_str(&payload))
                .transpose()?
                .unwrap_or_else(|| serde_json::json!({}));
            let destination = preference
                .as_object_mut()
                .context("saved provider preferences must be an object")?;
            for (field, value) in fields {
                if value.is_null() {
                    destination.remove(field);
                } else {
                    destination.insert(field.clone(), value.clone());
                }
            }
            transaction.execute(
                "INSERT INTO setting_layer VALUES ('provider',?1,?2)",
                params![provider, serde_json::to_string(&preference)?],
            )?;
            next = next.checked_add(1).context("settings revision exhausted")?;
        }
        if next != revision {
            transaction.execute(
                "UPDATE setting_revision SET revision=?1 WHERE id=1",
                [i64::try_from(next)?],
            )?;
        }
        let value = apply()?;
        if let Some(intent) = intent {
            let receipt = SettingsReceipt {
                request: serde_json::to_value(intent)?,
                mutation: SettingsMutation {
                    revision: next,
                    field: Vec::new(),
                },
            };
            transaction.execute(
                "INSERT INTO setting_operation VALUES (?1,?2,?3)",
                params![intent.id, intent.session, SettingsRecord::encode(&receipt)?],
            )?;
        }
        Ok(value)
    }

    /// Merge explicit directory grants into global settings without replacing concurrent preferences.
    pub(crate) fn grant_directories(
        &mut self,
        grants: &[crate::permissions::directory::DirectoryGrant],
    ) -> Result<()> {
        use crate::permissions::directory::DirectoryOperation;
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let payload = transaction
            .query_row(
                "SELECT payload FROM setting_layer WHERE scope='global' AND identity=''",
                [],
                |row| row.get::<_, String>(0),
            )
            .optional()?;
        let mut settings: PortableSettings = payload
            .map(|payload| SettingsRecord::decode(&payload))
            .transpose()?
            .flatten()
            .unwrap_or_default();
        let access = settings.access.get_or_insert_with(Default::default);
        for grant in grants {
            let directories = match grant.operation {
                DirectoryOperation::Read => &mut access.readable_directory,
                DirectoryOperation::Write => &mut access.writable_directory,
            };
            if !directories.contains(&grant.path) {
                directories.push(grant.path.clone());
            }
        }
        *access = access.clone().validate()?;
        transaction.execute(
            "INSERT INTO setting_layer VALUES ('global','',?1)
           ",
            [SettingsRecord::encode(&settings)?],
        )?;
        transaction.execute(
            "UPDATE setting_revision SET revision=revision+1 WHERE id=1",
            [],
        )?;
        transaction.commit()?;
        Ok(())
    }

    /// Recover an accepted mutation before validating a retry against the newer revision.
    pub(crate) fn operation(
        &self,
        session: &str,
        operation: &str,
        request: &Value,
    ) -> Result<Option<SettingsMutation>> {
        let saved = self
            .connection
            .query_row(
                "SELECT session_id,payload FROM setting_operation WHERE id=?1",
                [operation],
                |row| Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?)),
            )
            .optional()?;
        let Some((owner, payload)) = saved else {
            return Ok(None);
        };
        let Some(receipt) = SettingsRecord::<SettingsReceipt>::decode(&payload)? else {
            return Ok(None);
        };
        ensure!(
            owner == session,
            "settings operation belongs to another session"
        );
        ensure!(
            receipt.request == *request,
            "settings operation ID was reused with a different request"
        );
        Ok(Some(receipt.mutation))
    }

    /// Commit desired state and its replay receipt in one SQLite transaction.
    pub(crate) fn update_operation(
        &mut self,
        expected_revision: u64,
        target: SettingTarget<'_>,
        setting: &PortableSettings,
        operation: &str,
        request: &Value,
    ) -> Result<SettingsMutation> {
        self.write_layer(
            SettingLayerChange::Replace {
                expected_revision,
                setting,
            },
            target,
            Some((operation, request)),
            &[],
        )
    }

    /// Commit imported definitions, disabled selections, and retry ownership in one revision.
    pub(crate) fn import_operation(
        &mut self,
        expected_revision: u64,
        target: SettingTarget<'_>,
        resource: &[super::ImportedResource],
        operation: &str,
        request: &Value,
    ) -> Result<SettingsMutation> {
        let SettingTarget {
            scope,
            workspace,
            session,
        } = target;
        ensure!(
            !matches!(scope, SettingScope::Session),
            "Resource imports require global or workspace scope"
        );
        let mut setting = self.layer_settings(scope, workspace, session)?;
        let mut selected_name = std::collections::BTreeSet::new();
        for resource in resource {
            ensure!(
                selected_name.insert((resource.kind(), resource.name())),
                "Native import selected multiple definitions with the same name"
            );
            match resource {
                super::ImportedResource::Skill { .. } => {
                    setting.skill.entry(resource.id().into()).or_insert(false);
                }
                super::ImportedResource::Mcp { .. } => {
                    setting.mcp.entry(resource.id().into()).or_insert(false);
                }
            }
        }
        self.write_layer(
            SettingLayerChange::Replace {
                expected_revision,
                setting: &setting,
            },
            crate::settings::SettingTarget {
                scope,
                workspace,
                session,
            },
            Some((operation, request)),
            resource,
        )
    }

    fn write_layer(
        &mut self,
        change: SettingLayerChange<'_>,
        target: SettingTarget<'_>,
        receipt: Option<(&str, &Value)>,
        imported: &[super::ImportedResource],
    ) -> Result<SettingsMutation> {
        let SettingTarget {
            scope,
            workspace,
            session,
        } = target;
        let (scope, identity) = match scope {
            SettingScope::Global => ("global", ""),
            SettingScope::Workspace => ("workspace", workspace),
            SettingScope::Session => ("session", session),
        };
        let transaction = rusqlite::Transaction::new_unchecked(
            &self.connection,
            rusqlite::TransactionBehavior::Immediate,
        )?;
        if let Some((operation, request)) = receipt
            && let Some(mutation) = self.operation(session, operation, request)?
        {
            return Ok(mutation);
        }
        let revision = u64::try_from(transaction.query_row(
            "SELECT revision FROM setting_revision WHERE id=1",
            [],
            |row| row.get::<_, i64>(0),
        )?)?;
        if let SettingLayerChange::Replace {
            expected_revision, ..
        } = change
        {
            ensure!(
                revision == expected_revision,
                "settings revision conflict: expected {expected_revision}, current {revision}"
            );
        }
        let previous = self
            .layer(scope, identity)?
            .map(|payload| SettingsRecord::<PortableSettings>::decode(&payload))
            .transpose()?
            .flatten()
            .unwrap_or_default();
        let setting = match change {
            SettingLayerChange::Replace { setting, .. } => setting.clone(),
            SettingLayerChange::Patch(patch) => {
                let mut setting = previous.clone();
                setting.inherit(patch.clone());
                setting
            }
        };
        if let Some(policy) = &setting.auto_compaction {
            policy.validate(None)?;
        }
        if let Some(timeout) = setting.approval_timeout {
            timeout.validate()?;
        }
        if let Some(timeout) = setting.tool_timeout {
            timeout.require_expiry()?;
        }
        if let Some(access) = &setting.access {
            access.clone().validate()?;
        }
        if let Some(shell) = setting.shell {
            shell.executable()?;
        }
        let previous = serde_json::to_value(previous)?;
        let current = serde_json::to_value(&setting)?;
        let mut field: Vec<String> = current
            .as_object()
            .context("settings must be an object")?
            .keys()
            .filter(|field| previous.get(*field) != current.get(*field))
            .cloned()
            .collect();
        for resource in imported {
            transaction.execute(
                "INSERT INTO setting_resource VALUES (?1,?2,?3,?4)
                ON CONFLICT(id) DO UPDATE SET payload=excluded.payload",
                params![
                    resource.id(),
                    scope,
                    identity,
                    serde_json::to_string(resource)?
                ],
            )?;
            let definition_field = format!("{}_definition", resource.kind());
            if !field.contains(&definition_field) {
                field.push(definition_field);
            }
        }
        transaction.execute(
            "INSERT INTO setting_layer VALUES (?1,?2,?3)",
            params![scope, identity, SettingsRecord::encode(&setting)?],
        )?;
        let next = revision
            .checked_add(1)
            .context("settings revision exhausted")?;
        transaction.execute(
            "UPDATE setting_revision SET revision=?1 WHERE id=1",
            [i64::try_from(next)?],
        )?;
        let mutation = SettingsMutation {
            revision: next,
            field,
        };
        if let Some((operation, request)) = receipt {
            transaction.execute(
                "INSERT INTO setting_operation VALUES (?1,?2,?3)",
                params![
                    operation,
                    session,
                    SettingsRecord::encode(&SettingsReceipt {
                        request: request.clone(),
                        mutation: mutation.clone()
                    })?
                ],
            )?;
        }
        transaction.commit()?;
        Ok(mutation)
    }

    /// Reset one layer to inheritance rather than converting absent choices to false.
    pub fn reset(
        &mut self,
        expected_revision: u64,
        scope: SettingScope,
        workspace: &str,
        session: &str,
    ) -> Result<u64> {
        self.update(
            expected_revision,
            scope,
            workspace,
            session,
            &PortableSettings::default(),
        )
    }

    /// Copy explicit conversation overrides into a fresh fork without freezing inherited layers.
    pub(crate) fn fork_session(&mut self, source: &str, target: &str) -> Result<()> {
        ensure!(
            source != target && !target.is_empty(),
            "settings fork requires a distinct target session"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let payload = transaction
            .query_row(
                "SELECT payload FROM setting_layer WHERE scope='session' AND identity=?1",
                [source],
                |row| row.get::<_, String>(0),
            )
            .optional()?;
        if let Some(payload) = payload
            && SettingsRecord::<PortableSettings>::decode(&payload)?.is_some()
        {
            transaction.execute(
                "INSERT INTO setting_layer VALUES ('session',?1,?2)",
                params![target, payload],
            )?;
            let revision = transaction.query_row(
                "SELECT revision FROM setting_revision WHERE id=1",
                [],
                |row| row.get::<_, i64>(0),
            )?;
            let next = revision
                .checked_add(1)
                .context("settings revision exhausted")?;
            transaction.execute("UPDATE setting_revision SET revision=?1 WHERE id=1", [next])?;
        }
        transaction.commit()?;
        Ok(())
    }

    /// Save provider-local tuning without translating model identifiers across providers.
    pub fn save_provider(
        &mut self,
        expected_revision: u64,
        provider: &str,
        setting: &Value,
    ) -> Result<u64> {
        let field_map = setting
            .as_object()
            .context("provider preferences must be an object")?;
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let revision = u64::try_from(transaction.query_row(
            "SELECT revision FROM setting_revision WHERE id=1",
            [],
            |row| row.get::<_, i64>(0),
        )?)?;
        ensure!(
            revision == expected_revision,
            "settings revision conflict: expected {expected_revision}, current {revision}"
        );
        let payload = transaction
            .query_row(
                "SELECT payload FROM setting_layer WHERE scope='provider' AND identity=?1",
                [provider],
                |row| row.get::<_, String>(0),
            )
            .optional()?;
        let mut preference: Value = payload
            .map(|payload| serde_json::from_str(&payload))
            .transpose()?
            .unwrap_or_else(|| serde_json::json!({}));
        let destination = preference
            .as_object_mut()
            .context("saved provider preferences must be an object")?;
        for (field, value) in field_map {
            if value.is_null() {
                destination.remove(field);
            } else {
                destination.insert(field.clone(), value.clone());
            }
        }
        let next = revision
            .checked_add(1)
            .context("settings revision exhausted")?;
        // An explicit edit of the main model defaults is newer than every workspace's earlier
        // choice; the Ask model is not a main model default and leaves that order alone.
        if field_map.keys().any(|field| field != "ask_model") {
            destination.insert(PROVIDER_REVISION_FIELD.into(), next.into());
        }
        transaction.execute(
            "INSERT INTO setting_layer VALUES ('provider',?1,?2)",
            params![provider, serde_json::to_string(&preference)?],
        )?;
        transaction.execute(
            "UPDATE setting_revision SET revision=?1 WHERE id=1",
            [i64::try_from(next)?],
        )?;
        transaction.commit()?;
        Ok(next)
    }

    fn layer(&self, scope: &str, identity: &str) -> Result<Option<String>> {
        Ok(self
            .connection
            .query_row(
                "SELECT payload FROM setting_layer WHERE scope=?1 AND identity=?2",
                params![scope, identity],
                |row| row.get(0),
            )
            .optional()?)
    }

    /// Read one writable layer without copying inherited overrides into its source scope.
    pub fn layer_settings(
        &self,
        scope: SettingScope,
        workspace: &str,
        session: &str,
    ) -> Result<PortableSettings> {
        let (scope, identity) = match scope {
            SettingScope::Global => ("global", ""),
            SettingScope::Workspace => ("workspace", workspace),
            SettingScope::Session => ("session", session),
        };
        Ok(self
            .layer(scope, identity)?
            .map(|payload| SettingsRecord::decode(&payload))
            .transpose()?
            .flatten()
            .unwrap_or_default())
    }

    /// Read confirmed configuration without treating newly saved preferences as applied.
    pub fn application(&self, session: &str) -> Result<SettingsApplication> {
        let payload = self
            .connection
            .query_row(
                "SELECT payload FROM main.setting_application WHERE session_id=?1",
                [session],
                |row| row.get::<_, String>(0),
            )
            .optional()?;
        Ok(payload
            .map(|payload| SettingsRecord::decode(&payload))
            .transpose()?
            .flatten()
            .unwrap_or_else(|| SettingsApplication {
                state: "unverified".into(),
                ..Default::default()
            }))
    }

    /// Record an operation phase without clearing the previously confirmed configuration.
    pub fn begin_application(&self, session: &str, operation: &str, revision: u64) -> Result<()> {
        let transaction = rusqlite::Transaction::new_unchecked(
            &self.connection,
            rusqlite::TransactionBehavior::Immediate,
        )?;
        let mut application = self.application(session)?;
        application.operation_id = Some(operation.into());
        application.desired_revision = revision;
        application.state = "accepted".into();
        self.save_application(session, &application)?;
        transaction.commit()?;
        Ok(())
    }

    /// Confirm portable application while keeping resume outcome separate from configuration.
    pub fn confirm_application(
        &self,
        session: &str,
        operation: &str,
        revision: u64,
        effective: PortableSettings,
    ) -> Result<()> {
        let transaction = rusqlite::Transaction::new_unchecked(
            &self.connection,
            rusqlite::TransactionBehavior::Immediate,
        )?;
        let mut application = self.application(session)?;
        ensure!(
            application.operation_id.as_deref() == Some(operation),
            "configuration operation was superseded"
        );
        ensure!(
            application.desired_revision == revision,
            "configuration revision was superseded"
        );
        application.applied_revision = Some(revision);
        application.generation = application
            .generation
            .checked_add(1)
            .context("configuration generation exhausted")?;
        application.effective = Some(effective);
        application.state = "applied".into();
        self.save_application(session, &application)?;
        transaction.commit()?;
        Ok(())
    }

    fn save_application(&self, session: &str, application: &SettingsApplication) -> Result<()> {
        self.connection.execute("INSERT INTO main.setting_application VALUES (?1,?2) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload",
            params![session,SettingsRecord::encode(application)?])?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn configuration_commit_rolls_back_settings_when_host_fails() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let mut store = SettingsStore::open(fixture.path())?;
        let expected = store.get("workspace", "session", "mock")?.revision;
        let patch = PortableSettings {
            status_thoughts: Some(true),
            ..Default::default()
        };
        let result = store.commit_configuration(
            Some(expected),
            SettingTarget {
                scope: SettingScope::Session,
                workspace: "workspace",
                session: "session",
            },
            Some(&patch),
            None,
            None,
            || -> Result<()> { anyhow::bail!("host failed") },
        );
        assert!(result.is_err());
        let after = store.get("workspace", "session", "mock")?;
        assert_eq!(after.revision, expected);
        assert_ne!(after.desired.status_thoughts, Some(true));
        Ok(())
    }

    /// A configuration whose host commit outlived its settings commit is recovered even after
    /// another session advanced the settings revision, and replaying it changes nothing.
    #[test]
    fn configuration_recovery_applies_at_the_current_revision_once() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let mut store = SettingsStore::open(fixture.path())?;
        let captured = store.get("workspace", "session", "mock")?.revision;
        let intent = crate::storage::configuration::ConfigurationIntent {
            id: "configuration:recovered".into(),
            session: "session".into(),
            workspace: "workspace".into(),
            provider: "mock".into(),
            revision: captured,
            scope: SettingScope::Session,
            portable: Some(PortableSettings {
                status_thoughts: Some(true),
                ..Default::default()
            }),
            provider_setting: None,
        };
        store.patch(
            SettingTarget {
                scope: SettingScope::Global,
                workspace: "workspace",
                session: "other-session",
            },
            &PortableSettings {
                plan_compact: Some(true),
                ..Default::default()
            },
        )?;
        let advanced = store.get("workspace", "session", "mock")?.revision;
        assert_ne!(advanced, captured);
        store.recover_configuration(&intent)?;
        let recovered = store.get("workspace", "session", "mock")?;
        assert_eq!(recovered.desired.status_thoughts, Some(true));
        assert_eq!(recovered.desired.plan_compact, Some(true));
        store.recover_configuration(&intent)?;
        assert_eq!(
            store.get("workspace", "session", "mock")?.revision,
            recovered.revision,
            "a replayed recovery must not apply the intent twice"
        );
        Ok(())
    }

    /// Reject unsupported saved values while retaining the user's original payload.
    #[test]
    fn incompatible_settings_report_their_cause_without_reset() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let store = SettingsStore::open(fixture.path())?;
        let obsolete = r#"{"format_version":4,"payload":{"status_thoughts":false}}"#;
        store.connection.execute(
            "INSERT INTO setting_layer VALUES ('global','',?1)",
            [obsolete],
        )?;
        assert!(
            store
                .get("workspace", "session", "mock")
                .unwrap_err()
                .to_string()
                .contains("unsupported")
        );
        assert_eq!(store.layer("global", "")?.as_deref(), Some(obsolete));
        Ok(())
    }

    /// A history write transaction must never take the settings write lock: every history
    /// handle attaches settings read-only, so SQLite opens only a read transaction there.
    #[test]
    fn history_connections_attach_settings_read_only() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        drop(SettingsStore::open(fixture.path())?);
        let history = crate::storage::database::HistoryConnection::open(fixture.path())?;
        let error = history
            .execute(
                "INSERT INTO preferences.host_preference VALUES ('probe','1')",
                [],
            )
            .unwrap_err();
        assert_eq!(
            error.sqlite_error_code(),
            Some(rusqlite::ErrorCode::ReadOnly),
            "{error}"
        );
        Ok(())
    }

    /// Settings writes that span both databases lock history before settings, the same order
    /// every history transaction uses, because SQLite locks attached databases in index order.
    #[test]
    fn settings_writes_lock_history_before_settings() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let store = SettingsStore::open(fixture.path())?;
        let database_list = store
            .connection
            .prepare("SELECT seq, name, file FROM pragma_database_list ORDER BY seq")?
            .query_map([], |row| {
                Ok((
                    row.get::<_, i64>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, String>(2)?,
                ))
            })?
            .collect::<rusqlite::Result<Vec<_>>>()?;
        let history = database_list
            .iter()
            .find(|(_, _, file)| file.ends_with("harness.sqlite3"))
            .expect("settings store attaches history");
        let settings = database_list
            .iter()
            .find(|(_, _, file)| file.ends_with("settings.db"))
            .expect("settings store opens settings");
        assert!(history.0 < settings.0, "{database_list:?}");
        Ok(())
    }

    #[test]
    fn provider_patch_preserves_other_preferences_and_rejects_stale_writers() {
        let fixture = tempfile::tempdir().unwrap();
        let mut first = SettingsStore::open(fixture.path()).unwrap();
        let mut second = SettingsStore::open(fixture.path()).unwrap();
        first
            .save_provider(
                0,
                "fixture-gamma",
                &serde_json::json!({"model":"haiku","effort":"low"}),
            )
            .unwrap();
        assert!(
            second
                .save_provider(0, "fixture-gamma", &serde_json::json!({"model":"other"}))
                .is_err()
        );
        second
            .save_provider(1, "fixture-gamma", &serde_json::json!({"effort":"high"}))
            .unwrap();
        let snapshot = first.get("workspace", "session", "fixture-gamma").unwrap();
        assert_eq!(snapshot.revision, 2);
        assert_eq!(
            snapshot.provider,
            serde_json::json!({"model":"haiku","effort":"high"})
        );
        assert!(
            first
                .get("workspace", "session", "fixture-delta")
                .unwrap()
                .provider
                .is_null()
        );
        first
            .save_provider(2, "fixture-gamma", &serde_json::json!({"effort":null}))
            .unwrap();
        assert_eq!(
            first
                .get("workspace", "session", "fixture-gamma")
                .unwrap()
                .provider,
            serde_json::json!({"model":"haiku"})
        );
    }
}
