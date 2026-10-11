use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OptionalExtension, params};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

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

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Retains portable choices without copying provider authentication or native files.
pub struct PortableSettings {
    /// Controls requested OS isolation independently of approval handling.
    pub access: Option<crate::session::AccessPolicy>,
    /// Selects the check shell used by Harness execution.
    pub shell: Option<crate::session::CheckShell>,
    /// Selects the ordinary writable approval mode.
    pub default_write_permission: Option<crate::session::PermissionMode>,
    /// Selects the permission used when planning.
    pub plan_permission: Option<crate::session::PermissionMode>,
    /// Selects compaction before accepted plan execution.
    pub plan_compact: Option<bool>,
    /// Selects automatic review of execution plan revisions.
    pub plan_auto_approve_revisions: Option<bool>,
    /// Enables the root's model-facing child tools without restricting manual admission.
    pub subagent_tools: Option<bool>,
    #[serde(default)]
    /// Records selection by source identity, independent of provider display names.
    pub skill: BTreeMap<String, bool>,
    #[serde(default)]
    /// Records server selection by catalog identity.
    pub mcp: BTreeMap<String, bool>,
}

impl PortableSettings {
    fn inherit(&mut self, overriding: Self) {
        if overriding.access.is_some() { self.access = overriding.access; }
        if overriding.shell.is_some() { self.shell = overriding.shell; }
        if overriding.default_write_permission.is_some() { self.default_write_permission = overriding.default_write_permission; }
        if overriding.plan_permission.is_some() { self.plan_permission = overriding.plan_permission; }
        if overriding.plan_compact.is_some() { self.plan_compact = overriding.plan_compact; }
        if overriding.plan_auto_approve_revisions.is_some() { self.plan_auto_approve_revisions = overriding.plan_auto_approve_revisions; }
        if overriding.subagent_tools.is_some() { self.subagent_tools = overriding.subagent_tools; }
        self.skill.extend(overriding.skill);
        self.mcp.extend(overriding.mcp);
    }

    /// Apply validated portable choices while preserving provider-local model identity.
    pub fn apply(&self, session: &mut crate::session::HarnessSession, native_compact: bool) -> Result<()> {
        if let Some(access) = &self.access { session.access = access.clone().validate()?; }
        if let Some(shell) = self.shell { shell.executable()?; session.shell = shell; }
        if let Some(permission) = self.default_write_permission { session.default_write_permission = permission; }
        if let Some(permission) = self.plan_permission { session.plan_permission = Some(permission); }
        if let Some(enabled) = self.plan_compact { session.plan_compact = enabled && native_compact; }
        if let Some(enabled) = self.plan_auto_approve_revisions { session.plan_auto_approve_revisions = enabled; }
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
    pub fn apply_provider_defaults(&self, session: &mut crate::session::HarnessSession) -> Result<()> {
        if let Some(model) = self.provider.get("model").and_then(Value::as_str) {
            ensure!(!model.trim().is_empty(), "saved model cannot be empty");
            session.model = model.to_owned();
            session.resolved_model = (model != "default").then(|| model.to_owned());
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
    /// Open the current preference schema with transactional revision checks.
    pub fn open(data_root: &Path) -> Result<Self> {
        std::fs::create_dir_all(data_root)?;
        let connection = Connection::open(data_root.join("settings.db"))?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        connection.execute_batch("PRAGMA journal_mode=WAL;
            CREATE TABLE IF NOT EXISTS setting_revision (id INTEGER PRIMARY KEY CHECK(id=1), revision INTEGER NOT NULL);
            INSERT OR IGNORE INTO setting_revision VALUES (1,0);
            CREATE TABLE IF NOT EXISTS setting_layer (scope TEXT NOT NULL, identity TEXT NOT NULL, payload TEXT NOT NULL, PRIMARY KEY(scope,identity));
            CREATE TABLE IF NOT EXISTS setting_catalog (id INTEGER PRIMARY KEY CHECK(id=1), root TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS setting_operation (id TEXT PRIMARY KEY, session_id TEXT NOT NULL, payload TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS setting_application (session_id TEXT PRIMARY KEY, payload TEXT NOT NULL);
            CREATE TABLE IF NOT EXISTS setting_resource (id TEXT PRIMARY KEY, scope TEXT NOT NULL, identity TEXT NOT NULL, payload TEXT NOT NULL);")?;
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
        Ok(self.connection.query_row("SELECT root FROM setting_catalog WHERE id=1", [], |row|row.get::<_,String>(0))
            .optional()?.map(PathBuf::from))
    }

    /// Read catalog additions and disabled-by-default selections from one SQLite snapshot.
    pub(crate) fn catalog(&self, workspace: &str, session: &str, backend: &str) -> Result<CatalogSnapshot> {
        let transaction = rusqlite::Transaction::new_unchecked(&self.connection, rusqlite::TransactionBehavior::Deferred)?;
        let settings = self.get(workspace, session, backend)?;
        let skill = self.skills(workspace)?;
        let server = self.servers(workspace)?;
        transaction.commit()?;
        Ok(CatalogSnapshot { settings, skill, server })
    }

    /// Resolve shared and explicitly imported skills for one workspace.
    pub(crate) fn skills(&self, workspace: &str) -> Result<Vec<super::SkillResource>> {
        let mut skill = super::ResourceCatalog::skills(self.catalog_root()?.as_deref(), Path::new(workspace))?;
        for resource in self.imported(workspace)? {
            if let super::ImportedResource::Skill { resource } = resource { skill.push(resource); }
        }
        Ok(skill)
    }

    /// Resolve shared and explicitly imported MCP definitions without credential values.
    pub(crate) fn servers(&self, workspace: &str) -> Result<Vec<super::catalog::ServerResource>> {
        let mut server = super::ResourceCatalog::servers(self.catalog_root()?.as_deref(), Path::new(workspace))?;
        for resource in self.imported(workspace)? {
            if let Some(resource) = resource.server()? { server.push(resource); }
        }
        Ok(server)
    }

    fn imported(&self, workspace: &str) -> Result<Vec<super::ImportedResource>> {
        let mut statement = self.connection.prepare("SELECT payload FROM setting_resource
            WHERE scope='global' OR (scope='workspace' AND identity=?1) ORDER BY id")?;
        statement.query_map([workspace], |row| row.get::<_, String>(0))?
            .map(|payload| Ok(serde_json::from_str(&payload?)?)).collect()
    }

    /// Preview native definitions and report collisions against the current effective selection.
    pub(crate) fn preview_import(&self, kind: super::ImportKind, source: &Path, scope: SettingScope,
        workspace: &str, session: &str, backend: &str) -> Result<super::NativeImport> {
        let namespace = match scope {
            SettingScope::Global => "global".to_owned(),
            SettingScope::Workspace => format!("workspace:{workspace}"),
            SettingScope::Session => anyhow::bail!("Resource imports require global or workspace scope"),
        };
        let mut preview = super::NativeImport::read(kind, source, &namespace, Path::new(workspace))?;
        let catalog = self.catalog(workspace, session, backend)?;
        preview.revision = catalog.settings.revision;
        let desired = catalog.settings.desired;
        for candidate in &mut preview.candidate {
            candidate.conflict = if candidate.kind == "skill" {
                catalog.skill.iter().filter(|resource| resource.name == candidate.name && resource.id != candidate.id
                    && desired.skill.get(&resource.id) != Some(&false)).map(|resource| resource.id.clone()).collect()
            } else {
                catalog.server.iter().filter(|resource| resource.name == candidate.name && resource.id != candidate.id
                    && desired.mcp.get(&resource.id) != Some(&false)).map(|resource| resource.id.clone()).collect()
            };
        }
        Ok(preview)
    }

    /// Resolve portable layers and provider-local tuning without implicit imports.
    pub fn get(&self, workspace: &str, session: &str, backend: &str) -> Result<SettingsSnapshot> {
        let (revision, global, workspace, session, provider) = self.connection.query_row(
            "SELECT revision,
                (SELECT payload FROM setting_layer WHERE scope='global' AND identity=''),
                (SELECT payload FROM setting_layer WHERE scope='workspace' AND identity=?1),
                (SELECT payload FROM setting_layer WHERE scope='session' AND identity=?2),
                (SELECT payload FROM setting_layer WHERE scope='provider' AND identity=?3)
             FROM setting_revision WHERE id=1",
            params![workspace,session,backend], |row| Ok((row.get::<_,i64>(0)?,
                row.get::<_,Option<String>>(1)?,row.get::<_,Option<String>>(2)?,
                row.get::<_,Option<String>>(3)?,row.get::<_,Option<String>>(4)?)))?;
        let mut desired = PortableSettings::default();
        for payload in [global,workspace,session].into_iter().flatten() {
            desired.inherit(serde_json::from_str(&payload)?);
        }
        let provider = provider.map(|payload|serde_json::from_str(&payload)).transpose()?.unwrap_or(Value::Null);
        Ok(SettingsSnapshot { revision:u64::try_from(revision)?, desired, provider })
    }

    /// Replace one complete layer atomically, rejecting stale revisions without effects.
    pub fn update(&mut self, expected_revision: u64, scope: SettingScope, workspace: &str, session: &str,
        setting: &PortableSettings) -> Result<u64> {
        Ok(self.write_layer(expected_revision, scope, workspace, session, setting, None, &[])?.revision)
    }

    /// Recover an accepted mutation before validating a retry against the newer revision.
    pub(crate) fn operation(&self, session: &str, operation: &str, request: &Value) -> Result<Option<SettingsMutation>> {
        let saved = self.connection.query_row(
            "SELECT session_id,payload FROM setting_operation WHERE id=?1", [operation],
            |row|Ok((row.get::<_,String>(0)?,row.get::<_,String>(1)?))).optional()?;
        let Some((owner, payload)) = saved else { return Ok(None); };
        ensure!(owner == session, "settings operation belongs to another session");
        let receipt: SettingsReceipt = serde_json::from_str(&payload)?;
        ensure!(receipt.request == *request, "settings operation ID was reused with a different request");
        Ok(Some(receipt.mutation))
    }

    /// Commit desired state and its replay receipt in one SQLite transaction.
    pub(crate) fn update_operation(&mut self, expected_revision: u64, scope: SettingScope, workspace: &str,
        session: &str, setting: &PortableSettings, operation: &str, request: &Value) -> Result<SettingsMutation> {
        self.write_layer(expected_revision, scope, workspace, session, setting, Some((operation,request)), &[])
    }

    /// Commit imported definitions, disabled selections, and retry ownership in one revision.
    pub(crate) fn import_operation(&mut self, expected_revision: u64, scope: SettingScope, workspace: &str,
        session: &str, resource: &[super::ImportedResource], operation: &str, request: &Value) -> Result<SettingsMutation> {
        ensure!(!matches!(scope, SettingScope::Session), "Resource imports require global or workspace scope");
        let mut setting = self.layer_settings(scope, workspace, session)?;
        let mut selected_name = std::collections::BTreeSet::new();
        for resource in resource {
            ensure!(selected_name.insert((resource.kind(), resource.name())), "Native import selected multiple definitions with the same name");
            match resource {
                super::ImportedResource::Skill { .. } => { setting.skill.entry(resource.id().into()).or_insert(false); }
                super::ImportedResource::Mcp { .. } => { setting.mcp.entry(resource.id().into()).or_insert(false); }
            }
        }
        self.write_layer(expected_revision, scope, workspace, session, &setting, Some((operation,request)), resource)
    }

    fn write_layer(&mut self, expected_revision: u64, scope: SettingScope, workspace: &str, session: &str,
        setting: &PortableSettings, receipt: Option<(&str,&Value)>, imported: &[super::ImportedResource]) -> Result<SettingsMutation> {
        if let Some(access) = &setting.access { access.clone().validate()?; }
        if let Some(shell) = setting.shell { shell.executable()?; }
        if let Some(permission) = setting.default_write_permission {
            ensure!(permission != crate::session::PermissionMode::Read, "default_write_permission must be Write or Yolo");
        }
        let (scope, identity) = match scope { SettingScope::Global => ("global", ""),
            SettingScope::Workspace => ("workspace",workspace), SettingScope::Session => ("session",session) };
        let transaction = rusqlite::Transaction::new_unchecked(&self.connection, rusqlite::TransactionBehavior::Immediate)?;
        if let Some((operation, request)) = receipt {
            if let Some(mutation) = self.operation(session,operation,request)? { return Ok(mutation); }
        }
        let revision = u64::try_from(transaction.query_row("SELECT revision FROM setting_revision WHERE id=1", [], |row|row.get::<_,i64>(0))?)?;
        ensure!(revision == expected_revision, "settings revision conflict: expected {expected_revision}, current {revision}");
        let previous = self.layer(scope,identity)?.map(|payload|serde_json::from_str::<PortableSettings>(&payload))
            .transpose()?.unwrap_or_default();
        let previous = serde_json::to_value(previous)?;
        let current = serde_json::to_value(setting)?;
        let mut field: Vec<String> = current.as_object().context("settings must be an object")?.keys()
            .filter(|field|previous.get(*field) != current.get(*field)).cloned().collect();
        for resource in imported {
            transaction.execute("INSERT INTO setting_resource VALUES (?1,?2,?3,?4)
                ON CONFLICT(id) DO UPDATE SET payload=excluded.payload",
                params![resource.id(), scope, identity, serde_json::to_string(resource)?])?;
            let definition_field = format!("{}_definition", resource.kind());
            if !field.contains(&definition_field) { field.push(definition_field); }
        }
        transaction.execute("INSERT INTO setting_layer VALUES (?1,?2,?3) ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload",
            params![scope,identity,serde_json::to_string(setting)?])?;
        let next = revision.checked_add(1).context("settings revision exhausted")?;
        transaction.execute("UPDATE setting_revision SET revision=?1 WHERE id=1", [i64::try_from(next)?])?;
        let mutation = SettingsMutation { revision:next, field };
        if let Some((operation, request)) = receipt {
            transaction.execute("INSERT INTO setting_operation VALUES (?1,?2,?3)",
                params![operation,session,serde_json::to_string(&SettingsReceipt { request:request.clone(),mutation:mutation.clone() })?])?;
        }
        transaction.commit()?;
        Ok(mutation)
    }

    /// Reset one layer to inheritance rather than converting absent choices to false.
    pub fn reset(&mut self, expected_revision: u64, scope: SettingScope, workspace: &str, session: &str) -> Result<u64> {
        self.update(expected_revision, scope, workspace, session, &PortableSettings::default())
    }

    /// Copy explicit conversation overrides into a fresh fork without freezing inherited layers.
    pub(crate) fn fork_session(&mut self, source: &str, target: &str) -> Result<()> {
        ensure!(source != target && !target.is_empty(), "settings fork requires a distinct target session");
        let transaction = self.connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let payload = transaction.query_row("SELECT payload FROM setting_layer WHERE scope='session' AND identity=?1",
            [source], |row| row.get::<_, String>(0)).optional()?;
        if let Some(payload) = payload {
            transaction.execute("INSERT INTO setting_layer VALUES ('session',?1,?2)", params![target,payload])?;
            let revision = transaction.query_row("SELECT revision FROM setting_revision WHERE id=1", [], |row| row.get::<_,i64>(0))?;
            let next = revision.checked_add(1).context("settings revision exhausted")?;
            transaction.execute("UPDATE setting_revision SET revision=?1 WHERE id=1", [next])?;
        }
        transaction.commit()?;
        Ok(())
    }

    /// Save provider-local tuning without translating model identifiers across providers.
    pub fn save_provider(&mut self, expected_revision: u64, backend: &str, setting: &Value) -> Result<u64> {
        let field_map = setting.as_object().context("provider preferences must be an object")?;
        let transaction = self.connection.transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let revision = u64::try_from(transaction.query_row("SELECT revision FROM setting_revision WHERE id=1", [], |row|row.get::<_,i64>(0))?)?;
        ensure!(revision == expected_revision, "settings revision conflict: expected {expected_revision}, current {revision}");
        let payload = transaction.query_row("SELECT payload FROM setting_layer WHERE scope='provider' AND identity=?1", [backend], |row|row.get::<_,String>(0)).optional()?;
        let mut preference: Value = payload.map(|payload|serde_json::from_str(&payload)).transpose()?.unwrap_or_else(||serde_json::json!({}));
        let destination = preference.as_object_mut().context("saved provider preferences must be an object")?;
        for (field, value) in field_map {
            if value.is_null() { destination.remove(field); }
            else { destination.insert(field.clone(), value.clone()); }
        }
        transaction.execute("INSERT INTO setting_layer VALUES ('provider',?1,?2) ON CONFLICT(scope,identity) DO UPDATE SET payload=excluded.payload",
            params![backend,serde_json::to_string(&preference)?])?;
        let next = revision.checked_add(1).context("settings revision exhausted")?;
        transaction.execute("UPDATE setting_revision SET revision=?1 WHERE id=1", [i64::try_from(next)?])?;
        transaction.commit()?;
        Ok(next)
    }

    fn layer(&self, scope: &str, identity: &str) -> Result<Option<String>> {
        Ok(self.connection.query_row("SELECT payload FROM setting_layer WHERE scope=?1 AND identity=?2",
            params![scope,identity], |row|row.get(0)).optional()?)
    }

    /// Read one writable layer without copying inherited overrides into its source scope.
    pub fn layer_settings(&self, scope: SettingScope, workspace: &str, session: &str) -> Result<PortableSettings> {
        let (scope, identity) = match scope { SettingScope::Global => ("global",""),
            SettingScope::Workspace => ("workspace",workspace), SettingScope::Session => ("session",session) };
        Ok(self.layer(scope,identity)?.map(|payload|serde_json::from_str(&payload)).transpose()?.unwrap_or_default())
    }

    /// Read confirmed configuration without treating newly saved preferences as applied.
    pub fn application(&self, session: &str) -> Result<SettingsApplication> {
        let payload = self.connection.query_row("SELECT payload FROM setting_application WHERE session_id=?1", [session], |row|row.get::<_,String>(0)).optional()?;
        Ok(payload.map(|payload|serde_json::from_str(&payload)).transpose()?.unwrap_or_else(||SettingsApplication { state:"unverified".into(), ..Default::default() }))
    }

    /// Record an operation phase without clearing the previously confirmed configuration.
    pub fn begin_application(&self, session: &str, operation: &str, revision: u64) -> Result<()> {
        let transaction = rusqlite::Transaction::new_unchecked(&self.connection, rusqlite::TransactionBehavior::Immediate)?;
        let mut application = self.application(session)?;
        application.operation_id = Some(operation.into());
        application.desired_revision = revision;
        application.state = "accepted".into();
        self.save_application(session, &application)?;
        transaction.commit()?;
        Ok(())
    }

    /// Confirm portable application while keeping resume outcome separate from configuration.
    pub fn confirm_application(&self, session: &str, operation: &str, revision: u64, effective: PortableSettings) -> Result<()> {
        let transaction = rusqlite::Transaction::new_unchecked(&self.connection, rusqlite::TransactionBehavior::Immediate)?;
        let mut application = self.application(session)?;
        ensure!(application.operation_id.as_deref() == Some(operation), "configuration operation was superseded");
        ensure!(application.desired_revision == revision, "configuration revision was superseded");
        application.applied_revision = Some(revision);
        application.generation = application.generation.checked_add(1).context("configuration generation exhausted")?;
        application.effective = Some(effective);
        application.state = "applied".into();
        self.save_application(session, &application)?;
        transaction.commit()?;
        Ok(())
    }

    /// Retain failure state only if this operation still owns the pending application.
    pub fn finish_application(&self, session: &str, operation: &str, state: &str) -> Result<()> {
        let transaction = rusqlite::Transaction::new_unchecked(&self.connection, rusqlite::TransactionBehavior::Immediate)?;
        let mut application = self.application(session)?;
        if application.operation_id.as_deref() == Some(operation) && application.state != "applied" {
            application.state = state.into();
            self.save_application(session, &application)?;
        }
        transaction.commit()?;
        Ok(())
    }

    fn save_application(&self, session: &str, application: &SettingsApplication) -> Result<()> {
        self.connection.execute("INSERT INTO setting_application VALUES (?1,?2) ON CONFLICT(session_id) DO UPDATE SET payload=excluded.payload",
            params![session,serde_json::to_string(application)?])?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fork_preserves_explicit_selection_without_freezing_inheritance() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let mut store = SettingsStore::open(fixture.path())?;
        let mut global = PortableSettings { plan_compact: Some(false), ..Default::default() };
        store.update(0, SettingScope::Global, "workspace", "source", &global)?;
        let mut session = PortableSettings::default();
        session.skill.insert("skill-id".into(), false);
        session.mcp.insert("mcp-id".into(), true);
        store.update(1, SettingScope::Session, "workspace", "source", &session)?;
        store.fork_session("source", "fork")?;
        assert!(store.fork_session("source", "fork").is_err());
        assert!(store.fork_session("source", "source").is_err());
        global.plan_compact = Some(true);
        store.update(3, SettingScope::Global, "workspace", "source", &global)?;
        let fork = store.get("workspace", "fork", "claude")?;
        assert_eq!(fork.desired.plan_compact, Some(true));
        assert_eq!(fork.desired.skill, session.skill);
        assert_eq!(fork.desired.mcp, session.mcp);
        session.mcp.insert("mcp-id".into(), false);
        store.update(4, SettingScope::Session, "workspace", "source", &session)?;
        assert_eq!(store.get("workspace", "fork", "claude")?.desired.mcp["mcp-id"], true);
        assert!(store.layer_settings(SettingScope::Session, "workspace", "new")?.mcp.is_empty());
        Ok(())
    }

    #[test]
    fn concurrent_snapshot_keeps_revision_and_preferences_from_one_commit() {
        let fixture = tempfile::tempdir().unwrap();
        let reader = SettingsStore::open(fixture.path()).unwrap();
        let root = fixture.path().to_path_buf();
        let writer = std::thread::spawn(move || {
            let mut writer = SettingsStore::open(&root).unwrap();
            for revision in 0..128 {
                let mut setting = PortableSettings::default();
                setting.skill.insert((revision + 1).to_string(), true);
                writer.update(revision,SettingScope::Global,"workspace","session",&setting).unwrap();
            }
        });
        loop {
            let snapshot = reader.get("workspace","session","codex").unwrap();
            if snapshot.revision > 0 {
                assert_eq!(snapshot.desired.skill.keys().collect::<Vec<_>>(), [&snapshot.revision.to_string()]);
            }
            if writer.is_finished() { break; }
        }
        writer.join().unwrap();
        assert_eq!(reader.get("workspace","session","codex").unwrap().revision,128);
    }

    #[test]
    fn committed_operation_recovers_after_restart_without_reapplying_or_overwriting() {
        let fixture = tempfile::tempdir().unwrap();
        let request = serde_json::json!({"expected_revision":0,"desired":{"plan_compact":true}});
        let setting = PortableSettings { plan_compact:Some(true), ..Default::default() };
        let mut first = SettingsStore::open(fixture.path()).unwrap();
        let accepted = first.update_operation(0,SettingScope::Global,"workspace","session",
            &setting,"operation",&request).unwrap();
        assert_eq!(accepted.revision,1);
        assert_eq!(accepted.field,["plan_compact"]);
        drop(first);
        let mut reopened = SettingsStore::open(fixture.path()).unwrap();
        let replay = reopened.update_operation(0,SettingScope::Global,"workspace","session",
            &setting,"operation",&request).unwrap();
        assert_eq!(replay.revision,1);
        assert_eq!(reopened.get("workspace","session","claude").unwrap().revision,1);
        assert!(reopened.operation("other-session","operation",&request).is_err());
        assert!(reopened.operation("session","operation",&serde_json::json!({"desired":{}})).is_err());
        assert!(reopened.update_operation(0,SettingScope::Global,"workspace","session",
            &PortableSettings::default(),"stale-operation",&request).is_err());
        assert!(reopened.operation("session","stale-operation",&request).unwrap().is_none());
        assert_eq!(reopened.get("workspace","session","claude").unwrap().desired.plan_compact,Some(true));
    }

    #[test]
    fn preference_scope_survives_provider_switch_and_rejects_stale_writes() {
        let fixture = tempfile::tempdir().unwrap();
        let mut store = SettingsStore::open(fixture.path()).unwrap();
        let mut global = PortableSettings::default();
        global.mcp.insert("rulesync:sem".into(), false);
        assert_eq!(store.update(0,SettingScope::Global,"workspace","session",&global).unwrap(),1);
        let mut overriding = PortableSettings::default();
        overriding.mcp.insert("rulesync:sem".into(),true);
        assert!(store.update(0,SettingScope::Session,"workspace","session",&overriding).is_err());
        store.update(1,SettingScope::Session,"workspace","session",&overriding).unwrap();
        for provider in ["codex","copilot","claude","antigravity"] {
            assert!(store.get("workspace","session",provider).unwrap().desired.mcp["rulesync:sem"]);
            assert!(!store.get("workspace","another-session",provider).unwrap().desired.mcp["rulesync:sem"]);
        }
        store.reset(2,SettingScope::Session,"workspace","session").unwrap();
        drop(store);
        assert!(!SettingsStore::open(fixture.path()).unwrap().get("workspace","session","claude").unwrap().desired.mcp["rulesync:sem"]);
    }

    #[test]
    fn provider_patch_preserves_other_preferences_and_rejects_stale_writers() {
        let fixture = tempfile::tempdir().unwrap();
        let mut first = SettingsStore::open(fixture.path()).unwrap();
        let mut second = SettingsStore::open(fixture.path()).unwrap();
        first.save_provider(0, "claude", &serde_json::json!({"model":"haiku","effort":"low"})).unwrap();
        assert!(second.save_provider(0, "claude", &serde_json::json!({"model":"other"})).is_err());
        second.save_provider(1, "claude", &serde_json::json!({"effort":"high"})).unwrap();
        let snapshot = first.get("workspace", "session", "claude").unwrap();
        assert_eq!(snapshot.revision, 2);
        assert_eq!(snapshot.provider, serde_json::json!({"model":"haiku","effort":"high"}));
        assert!(first.get("workspace", "session", "antigravity").unwrap().provider.is_null());
        first.save_provider(2, "claude", &serde_json::json!({"effort":null})).unwrap();
        assert_eq!(first.get("workspace", "session", "claude").unwrap().provider, serde_json::json!({"model":"haiku"}));
    }

    #[test]
    fn application_preserves_confirmed_state_across_failure_and_supersession() {
        let fixture = tempfile::tempdir().unwrap();
        let store = SettingsStore::open(fixture.path()).unwrap();
        assert_eq!(store.application("session").unwrap().applied_revision, None);
        store.begin_application("session", "first", 1).unwrap();
        store.confirm_application("session", "first", 1, PortableSettings::default()).unwrap();
        store.finish_application("session", "first", "failed").unwrap();
        assert_eq!(store.application("session").unwrap().state, "applied");
        store.begin_application("session", "second", 2).unwrap();
        store.begin_application("session", "third", 3).unwrap();
        assert!(store.confirm_application("session", "second", 2, PortableSettings::default()).is_err());
        store.finish_application("session", "second", "failed").unwrap();
        store.finish_application("session", "third", "failed").unwrap();
        drop(store);
        let application = SettingsStore::open(fixture.path()).unwrap().application("session").unwrap();
        assert_eq!(application.applied_revision, Some(1));
        assert_eq!(application.generation, 1);
        assert_eq!(application.desired_revision, 3);
        assert_eq!(application.state, "failed");
    }
}
