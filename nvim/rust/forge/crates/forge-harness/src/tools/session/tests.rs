use super::*;
use crate::session::SessionStore;
use crate::storage::SqliteStore;

fn session(id: &str, workspace: &Path) -> HarnessSession {
    serde_json::from_value(json!({
        "id":id,"primary_agent_id":HarnessSession::primary_agent_id(id),"name":id,
        "workspace":workspace,"provider":"fixture-beta","model":"default","effort":"medium",
        "provider_fork_state":{"state":"ready"},"approval_mode":"ask",
        "created_at_ms":1,"updated_at_ms":1
    }))
    .unwrap()
}

fn exchange(session: &HarnessSession, id: &str, text: &str) -> Exchange {
    serde_json::from_value(json!({
        "id":id,"session_id":session.id,"agent_id":session.primary_agent_id,"ordinal":1,
        "prompt":text,"state":"complete","created_at_ms":0,"completed_at_ms":1,
        "attributed_matches_checkpoint":true,"node_list":[]
    }))
    .unwrap()
}

struct Fixture {
    root: tempfile::TempDir,
    workspace: tempfile::TempDir,
    scope: Arc<HarnessInspectionScope>,
    _lease: InspectionLease,
}

impl Fixture {
    fn new() -> Result<Self> {
        let root = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let session = session("current", workspace.path());
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&session)?;
        store.save_exchange(&exchange(&session, "exchange", "Recover deletion labels"))?;
        let settings = SettingsStore::open(root.path())?;
        settings.configure_catalog(workspace.path())?;
        let policy = Arc::new(CompiledPermissionDocument::compile(
            crate::permissions::document::default_permission_document(),
            workspace.path(),
        ));
        let (scope, lease) = HarnessInspectionScope::open(
            session,
            root.path().into(),
            "permissions.json".into(),
            policy,
            Arc::new(AtomicBool::new(false)),
            Arc::new(crate::trace::TraceStore::open(root.path())?),
        )?;
        Ok(Self {
            root,
            workspace,
            scope,
            _lease: lease,
        })
    }

    fn invoke(&self, name: &str, args: Value) -> Result<Value> {
        crate::tools::validate_arguments(&crate::tools::ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: name.into(),
            arguments: args.clone(),
        })?;
        crate::codec::from_str(&self.scope.read(name, args)?).map_err(Into::into)
    }
}

#[test]
fn search_respects_workspace_read_boundary_and_explicit_denials() -> Result<()> {
    let mut fixture = Fixture::new()?;
    let outside = tempfile::tempdir()?;
    let other = session("outside", outside.path());
    let mut store = SqliteStore::open(fixture.root.path())?;
    store.save_session(&other)?;
    store.save_exchange(&exchange(
        &other,
        "outside-exchange",
        "Recover private work",
    ))?;
    let session = HarnessSession {
        access: crate::session::AccessPolicy {
            read_access: crate::session::DirectoryScope::Workspace,
            ..Default::default()
        },
        ..fixture.scope.session.clone()
    };
    let (scope, lease) = HarnessInspectionScope::open(
        session,
        fixture.root.path().into(),
        "permissions.json".into(),
        fixture.scope.policy.clone(),
        Arc::new(AtomicBool::new(false)),
        fixture.scope.trace.clone(),
    )?;
    fixture.scope = scope;
    fixture._lease = lease;
    assert_eq!(
        fixture.invoke("harness_session_search", json!({"query":"recover"}))?["items"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert!(
        fixture
            .invoke("harness_session_status", json!({"session_id":"outside"}))
            .is_err()
    );
    let denied = CompiledPermissionDocument::compile(
        crate::permissions::document::parse_permission_document(
            r#"{"permission":{"read":{"*":"deny"}}}"#,
        )?,
        fixture.workspace.path(),
    );
    let (scope, _lease) = HarnessInspectionScope::open(
        fixture.scope.session.clone(),
        fixture.root.path().into(),
        "permissions.json".into(),
        Arc::new(denied),
        Arc::new(AtomicBool::new(false)),
        fixture.scope.trace.clone(),
    )?;
    assert!(!scope.allowed(&fixture.scope.session.workspace)?);
    Ok(())
}
