use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OptionalExtension, params};
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
            return Ok(None);
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
    /// Shows the latest thought beneath live status, enabled by default.
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
}

impl SettingsSnapshot {
    /// Apply provider-local defaults only when creating a conversation.
    pub fn apply_provider_defaults(
        &self,
        session: &mut crate::session::HarnessSession,
    ) -> Result<()> {
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
pub struct SettingsStore {
    connection: Connection,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains one accepted preference revision and the fields changed by that mutation.
pub(crate) struct SettingsMutation {
    /// Identifies the desired state committed before runtime application begins.
    pub(crate) revision: u64,
    /// Lists changed field names without recording preference values in the timeline.
    pub(crate) field: Vec<String>,
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
    pub(crate) fn read_only(data_root: &Path) -> Result<Self> {
        let connection = Connection::open_with_flags(
            data_root.join("settings.db"),
            rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
        )?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        let version: i64 = connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
        anyhow::ensure!(
            version == 1,
            "Settings database schema {version} is incompatible with this Harness; initialize or update Harness"
        );
        Ok(Self { connection })
    }

    /// Opens settings without repeating schema writes for a current database.
    pub fn open(data_root: &Path) -> Result<Self> {
        std::fs::create_dir_all(data_root)?;
        let mut connection = Connection::open(data_root.join("settings.db"))?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        connection.pragma_update(None, "foreign_keys", "ON")?;
        let version: i64 = connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
        anyhow::ensure!(
            version <= 1,
            "Settings database schema {version} is newer than supported schema 1; update Harness"
        );
        if version == 0 {
            let _bootstrap = crate::storage::bootstrap_lock(data_root, "settings")?;
            connection.pragma_update(None, "journal_mode", "WAL")?;
            let transaction =
                connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
            let version: i64 =
                transaction.pragma_query_value(None, "user_version", |row| row.get(0))?;
            anyhow::ensure!(
                version <= 1,
                "Settings database changed to an unsupported schema"
            );
            if version == 0 {
                transaction.execute_batch("
            CREATE TABLE IF NOT EXISTS setting_revision (id INTEGER PRIMARY KEY CHECK(id=1), revision INTEGER NOT NULL);
            INSERT OR IGNORE INTO setting_revision VALUES (1,0);
            CREATE TABLE IF NOT EXISTS setting_layer (scope TEXT NOT NULL, identity TEXT NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(scope,identity));
            CREATE TABLE IF NOT EXISTS setting_catalog (id INTEGER PRIMARY KEY CHECK(id=1), root TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS setting_operation (id TEXT PRIMARY KEY, session_id TEXT NOT NULL, payload TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS setting_application (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS setting_resource (id TEXT PRIMARY KEY, scope TEXT NOT NULL, identity TEXT NOT NULL, payload TEXT NOT NULL);")?;
                transaction.pragma_update(None, "user_version", 1)?;
            }
            transaction.commit()?;
        }
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
    /// Ignore unsupported saved formats and propagate current-format decoding failures.
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
        let provider = provider
            .map(|payload| serde_json::from_str(&payload))
            .transpose()?
            .unwrap_or(Value::Null);
        Ok(SettingsSnapshot {
            revision: u64::try_from(revision)?,
            desired,
            provider,
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

    /// Hold the settings revision stable while committing a captured host configuration.
    pub(crate) fn commit_configuration<T>(
        &mut self,
        expected_revision: u64,
        target: SettingTarget<'_>,
        portable: Option<&PortableSettings>,
        provider: Option<(&str, &Value)>,
        intent: Option<&crate::storage::configuration::ConfigurationIntent>,
        apply: impl FnOnce() -> Result<T>,
    ) -> Result<T> {
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
            transaction.execute("INSERT INTO setting_layer VALUES (?1,?2,?3) ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload", params![scope, identity, SettingsRecord::encode(&setting)?])?;
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
            transaction.execute("INSERT INTO setting_layer VALUES ('provider',?1,?2) ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload", params![provider, serde_json::to_string(&preference)?])?;
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
        transaction.commit()?;
        Ok(value)
    }

    pub(crate) fn configuration_applied(
        &self,
        intent: &crate::storage::configuration::ConfigurationIntent,
    ) -> Result<bool> {
        Ok(self
            .operation(&intent.session, &intent.id, &serde_json::to_value(intent)?)?
            .is_some())
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
            ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload",
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
        let Some(mut receipt) = SettingsRecord::<SettingsReceipt>::decode(&payload)? else {
            return Ok(None);
        };
        ensure!(
            owner == session,
            "settings operation belongs to another session"
        );
        // Version-one receipts retain wire names from before the interface rename.
        if let Some(method) = receipt.request.get_mut("method") {
            match method.as_str() {
                Some("backend.mcp.set_enabled") => {
                    *method = serde_json::json!("settings.mcp.set_enabled")
                }
                Some("backend.skills.set_enabled") => {
                    *method = serde_json::json!("settings.skill.set_enabled")
                }
                _ => {}
            }
        }
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
        transaction.execute("INSERT INTO setting_layer VALUES (?1,?2,?3) ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload",
            params![scope,identity,SettingsRecord::encode(&setting)?])?;
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
        transaction.execute("INSERT INTO setting_layer VALUES ('provider',?1,?2) ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload",
            params![provider,serde_json::to_string(&preference)?])?;
        let next = revision
            .checked_add(1)
            .context("settings revision exhausted")?;
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
                "SELECT payload FROM setting_application WHERE session_id=?1",
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
        self.connection.execute("INSERT INTO setting_application VALUES (?1,?2) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload",
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
            expected,
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

    #[test]
    fn incompatible_settings_stay_hidden_across_every_persistence_path() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let mut store = SettingsStore::open(fixture.path())?;
        let obsolete = r#"{"format_version":4,"payload":{"access":{"sandbox":true,"write_access":"workspace","writable_directory":[],"windows_sandbox":true},"status_thoughts":false}}"#;
        for (scope, identity) in [
            ("global", ""),
            ("workspace", "workspace"),
            ("session", "source"),
        ] {
            store.connection.execute(
                "INSERT INTO setting_layer VALUES (?1,?2,?3)",
                params![scope, identity, obsolete],
            )?;
        }
        store.connection.execute(
            "INSERT INTO setting_application VALUES ('source',?1)",
            [format!(r#"{{"desired_revision":0,"generation":0,"state":"applied","effective":{obsolete}}}"#)],
        )?;
        store.connection.execute(
            "INSERT INTO setting_operation VALUES ('old','source',?1)",
            [r#"{"request":{},"mutation":{"revision":0,"field":["default_write_permission"]}}"#],
        )?;
        store.save_provider(0, "mock", &serde_json::json!({"model":"saved-model"}))?;
        let before = store.get("workspace", "source", "mock")?;
        assert!(before.desired.check_editing.is_none());
        assert!(before.desired.status_thoughts.is_none());
        assert_eq!(before.provider["model"], "saved-model");
        for scope in [
            SettingScope::Global,
            SettingScope::Workspace,
            SettingScope::Session,
        ] {
            assert!(
                store
                    .layer_settings(scope, "workspace", "source")?
                    .check_editing
                    .is_none()
            );
        }
        assert_eq!(store.application("source")?.state, "unverified");
        assert!(
            store
                .operation("source", "old", &serde_json::json!({}))?
                .is_none()
        );
        assert!(
            store
                .operation("other", "old", &serde_json::json!({}))?
                .is_none()
        );
        store.fork_session("source", "obsolete-fork")?;
        assert!(store.layer("session", "obsolete-fork")?.is_none());
        assert_eq!(store.layer("global", "")?.as_deref(), Some(obsolete));

        store.grant_directories(&[crate::permissions::directory::DirectoryGrant {
            operation: crate::permissions::directory::DirectoryOperation::Read,
            path: fixture.path().to_string_lossy().into_owned(),
        }])?;
        let current = PortableSettings {
            check_editing: Some(true),
            status_thoughts: Some(true),
            ..Default::default()
        };
        let revision = store.get("workspace", "source", "mock")?.revision;
        store.update(
            revision,
            SettingScope::Workspace,
            "workspace",
            "source",
            &current,
        )?;
        let revision = store.get("workspace", "source", "mock")?.revision;
        let request = serde_json::json!({"settings": current});
        let mutation = store.update_operation(
            revision,
            crate::settings::SettingTarget {
                scope: SettingScope::Session,
                workspace: "workspace",
                session: "source",
            },
            &current,
            "current",
            &request,
        )?;
        store.begin_application("source", "current", mutation.revision)?;
        store.confirm_application("source", "current", mutation.revision, current)?;
        store.fork_session("source", "current-fork")?;
        drop(store);

        let store = SettingsStore::open(fixture.path())?;
        for session in ["source", "current-fork"] {
            let settings = store.get("workspace", session, "mock")?;
            assert_eq!(settings.desired.check_editing, Some(true));
            assert_eq!(settings.desired.status_thoughts, Some(true));
            assert_eq!(settings.desired.access.unwrap().readable_directory.len(), 1);
            assert_eq!(settings.provider["model"], "saved-model");
        }
        assert_eq!(store.application("source")?.state, "applied");
        assert_eq!(
            store
                .operation("source", "current", &request)?
                .unwrap()
                .revision,
            mutation.revision
        );
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
