use super::*;

pub(super) fn handles(method: RuntimeMethod) -> bool {
    matches!(
        method,
        RuntimeMethod::SettingsGet
            | RuntimeMethod::SettingsUpdate
            | RuntimeMethod::SettingsImportPreview
            | RuntimeMethod::SettingsImportApply
            | RuntimeMethod::SettingsSkillSetEnabled
            | RuntimeMethod::SettingsMcpSetEnabled
            | RuntimeMethod::AgentProfileGet
            | RuntimeMethod::AgentProfileSave
            | RuntimeMethod::ProviderModelPinList
            | RuntimeMethod::ProviderModelPinSet
            | RuntimeMethod::HostTraceStatus
            | RuntimeMethod::HostTraceConfigure
            | RuntimeMethod::SessionTraceClear
            | RuntimeMethod::PermissionsDocumentOpen
            | RuntimeMethod::PermissionsDocumentSave
            | RuntimeMethod::SessionList
            | RuntimeMethod::SessionNew
    )
}

pub(super) async fn dispatch(
    registry: Arc<SessionControllerRegistry>,
    session_id: Option<String>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let method = RuntimeMethod::decode(&request.method)?;
    if let Some(id) = &session_id {
        ensure!(
            SqliteStore::read_only(PathBuf::from(&registry.initialize.data_root).as_path())?
                .load_session(id)?
                .is_some(),
            "session context does not exist"
        );
    }
    match method {
        RuntimeMethod::SettingsGet
        | RuntimeMethod::SettingsUpdate
        | RuntimeMethod::SettingsImportPreview
        | RuntimeMethod::SettingsImportApply
        | RuntimeMethod::SettingsSkillSetEnabled
        | RuntimeMethod::SettingsMcpSetEnabled => {
            settings(registry, session_id, request, message_sink).await
        }
        RuntimeMethod::AgentProfileGet | RuntimeMethod::AgentProfileSave => {
            profiles(registry, request, message_sink).await
        }
        RuntimeMethod::ProviderModelPinList | RuntimeMethod::ProviderModelPinSet => {
            pins(registry, request, message_sink).await
        }
        RuntimeMethod::HostTraceStatus
        | RuntimeMethod::HostTraceConfigure
        | RuntimeMethod::SessionTraceClear => {
            trace(registry, session_id, request, message_sink).await
        }
        RuntimeMethod::PermissionsDocumentOpen | RuntimeMethod::PermissionsDocumentSave => {
            permissions(registry, request, message_sink).await
        }
        RuntimeMethod::SessionList => list_sessions(registry, request, message_sink).await,
        RuntimeMethod::SessionNew => {
            create_session(registry, session_id, request, message_sink).await
        }
        _ => anyhow::bail!("request is not owned by shared services"),
    }
}

async fn settings(
    registry: Arc<SessionControllerRegistry>,
    session_id: Option<String>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let method = RuntimeMethod::decode(&request.method)?;
    let controller = match &session_id {
        Some(id) => Some(registry.resolve(id).await?),
        None => None,
    };
    let workspace = if let Some(controller) = &controller {
        controller.catalog_request.read().await.workspace.clone()
    } else {
        let path = request
            .params
            .get("workspace")
            .and_then(Value::as_str)
            .unwrap_or(&registry.initialize.workspace);
        match crate::workspace::resolve(std::path::Path::new(path))? {
            crate::workspace::WorkspaceKind::Git(path)
            | crate::workspace::WorkspaceKind::Untracked(path) => {
                path.to_string_lossy().into_owned()
            }
        }
    };
    // The empty storage key selects no session layer and never resolves a session.
    let session_key = session_id.as_deref().unwrap_or_default();
    let mut settings = crate::settings::SettingsStore::open(
        PathBuf::from(&registry.initialize.data_root).as_path(),
    )?;
    if method == RuntimeMethod::SettingsGet {
        let snapshot =
            settings.catalog(&workspace, session_key, &registry.initialize.backend.kind)?;
        let mut value = serde_json::to_value(snapshot.settings)?;
        value["catalog"] = json!({"mcp":snapshot.server,"skill":snapshot.skill});
        value["layers"] = json!({
            "global":settings.layer_settings(crate::settings::SettingScope::Global,&workspace,session_key)?,
            "workspace":settings.layer_settings(crate::settings::SettingScope::Workspace,&workspace,session_key)?,
            "session":settings.layer_settings(crate::settings::SettingScope::Session,&workspace,session_key)?,
        });
        value["application"] = serde_json::to_value(settings.application(session_key)?)?;
        message_sink
            .send_response(Response::success(request.id, value)?)
            .await?;
        return Ok(());
    }
    if method == RuntimeMethod::SettingsImportPreview {
        let (_, preview) = preview_native_import(
            settings,
            &request.params,
            &workspace,
            session_key,
            &registry.initialize.backend.kind,
        )
        .await?;
        let result = serde_json::to_value(preview)?;
        message_sink
            .send_response(Response::success(request.id, result)?)
            .await?;
        return Ok(());
    }
    let operation_id = request
        .params
        .get("operation_id")
        .and_then(Value::as_str)
        .map(str::to_owned)
        .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    let receipt = json!({"method":request.method,"params":request.params});
    let mutation = if let Some(mutation) =
        settings.operation(session_key, &operation_id, &receipt)?
    {
        mutation
    } else {
        let resource_mutation = matches!(
            method,
            RuntimeMethod::SettingsSkillSetEnabled | RuntimeMethod::SettingsMcpSetEnabled
        );
        let revision = match request.params["expected_revision"].as_u64() {
            Some(revision) => revision,
            None if resource_mutation => {
                settings
                    .get(&workspace, session_key, &registry.initialize.backend.kind)?
                    .revision
            }
            None => anyhow::bail!("settings require expected_revision"),
        };
        let default_scope = if method == RuntimeMethod::SettingsImportApply {
            "workspace"
        } else {
            "global"
        };
        let scope: crate::settings::SettingScope = serde_json::from_value(
            request
                .params
                .get("scope")
                .cloned()
                .unwrap_or(json!(default_scope)),
        )?;
        ensure!(
            session_id.is_some() || !matches!(scope, crate::settings::SettingScope::Session),
            "session settings require an explicit session_id"
        );
        if method == RuntimeMethod::SettingsImportApply {
            let fingerprint = request.params["fingerprint"]
                .as_str()
                .context("native import preview fingerprint is required")?;
            let selected: Vec<String> = serde_json::from_value(request.params["selected"].clone())?;
            let (restored, preview) = preview_native_import(
                settings,
                &request.params,
                &workspace,
                session_key,
                &registry.initialize.backend.kind,
            )
            .await?;
            settings = restored;
            let resource = preview.select(fingerprint, &selected)?;
            settings.import_operation(
                revision,
                crate::settings::SettingTarget {
                    scope,
                    workspace: &workspace,
                    session: session_key,
                },
                &resource,
                &operation_id,
                &receipt,
            )?
        } else {
            let desired = if resource_mutation {
                let name = request.params["name"]
                    .as_str()
                    .context("resource selection requires name")?;
                let requested_id = request.params["resource_id"].as_str();
                let enabled = request.params["enabled"]
                    .as_bool()
                    .context("resource selection requires enabled")?;
                let source = if method == RuntimeMethod::SettingsSkillSetEnabled {
                    settings
                        .skills(&workspace)?
                        .into_iter()
                        .map(|entry| (entry.id, entry.name))
                        .collect::<Vec<_>>()
                } else {
                    settings
                        .servers(&workspace)?
                        .into_iter()
                        .map(|entry| (entry.id, entry.name))
                        .collect::<Vec<_>>()
                };
                let matching: Vec<_> = source
                    .into_iter()
                    .filter(|(id, candidate)| {
                        requested_id.map_or(candidate == name, |requested| requested == id)
                    })
                    .collect();
                let [(id, _)] = matching.as_slice() else {
                    anyhow::bail!(
                        "Resource selection requires one canonical source. Import native configuration explicitly or select a resource ID"
                    );
                };
                let id = id.clone();
                let mut desired = settings.layer_settings(scope, &workspace, session_key)?;
                if method == RuntimeMethod::SettingsSkillSetEnabled {
                    desired.skill.insert(id, enabled);
                } else {
                    desired.mcp.insert(id, enabled);
                }
                desired
            } else {
                serde_json::from_value(request.params["desired"].clone())?
            };
            settings.update_operation(
                revision,
                crate::settings::SettingTarget {
                    scope,
                    workspace: &workspace,
                    session: session_key,
                },
                &desired,
                &operation_id,
                &receipt,
            )?
        }
    };
    drop(settings);
    if let Some(controller) = controller {
        let request = Request {
            id: request.id,
            method: "session.command.apply".into(),
            params: json!({"operation_id":operation_id,"action":"configure","config":{"settings_revision":mutation.revision,"setting_fields":mutation.field}}),
        };
        return route_task_transition(
            registry,
            controller,
            session_id.context("settings session is missing")?,
            request,
            message_sink,
        )
        .await;
    }
    message_sink.send_response(Response::success(request.id, json!({"id":operation_id,"state":"saved","revision":mutation.revision,"restart_required":false}))?).await?;
    Ok(())
}

async fn profiles(
    registry: Arc<SessionControllerRegistry>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let method = RuntimeMethod::decode(&request.method)?;
    let profiles = &registry.runtime.profiles;
    let snapshot = if method == RuntimeMethod::AgentProfileSave {
        let revision = request.params["expected_revision"]
            .as_str()
            .context("profile save requires expected_revision")?;
        profiles.save(
            revision,
            serde_json::from_value(request.params["document"].clone())?,
        )?
    } else {
        profiles.snapshot()?
    };
    message_sink
        .send_response(Response::success(request.id, snapshot)?)
        .await?;
    Ok(())
}

async fn pins(
    registry: Arc<SessionControllerRegistry>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let method = RuntimeMethod::decode(&request.method)?;
    let data_root = PathBuf::from(&registry.initialize.data_root);
    let parameter = request.params;
    let identifier_list = tokio::task::spawn_blocking(move || -> Result<Vec<String>> {
        let backend = parameter
            .get("backend")
            .and_then(Value::as_str)
            .context("model pin request requires backend")?;
        let mut store = crate::storage::model_pin::ModelPinStore::open(&data_root)?;
        if method == RuntimeMethod::ProviderModelPinList {
            store.load(backend)
        } else {
            let model = parameter
                .get("model")
                .and_then(Value::as_str)
                .context("backend.model_pin requires model")?;
            let pinned = parameter
                .get("pinned")
                .and_then(Value::as_bool)
                .context("backend.model_pin requires pinned")?;
            store.set(backend, model, pinned)
        }
    })
    .await
    .context("model pin storage task failed")??;
    message_sink
        .send_response(Response::success(request.id, identifier_list)?)
        .await?;
    Ok(())
}

async fn trace(
    registry: Arc<SessionControllerRegistry>,
    session_id: Option<String>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let method = RuntimeMethod::decode(&request.method)?;
    let trace = registry.runtime.trace();
    match method {
        RuntimeMethod::HostTraceConfigure => {
            let enabled = request
                .params
                .get("enabled")
                .and_then(Value::as_bool)
                .context("trace enabled is required")?;
            if request.params.get("default_only").and_then(Value::as_bool) == Some(true) {
                trace.configure_default(enabled)?;
            } else {
                trace.configure(enabled)?;
            }
        }
        RuntimeMethod::SessionTraceClear => {
            trace.clear_session(
                session_id
                    .as_deref()
                    .context("trace clear requires session_id")?,
            )?;
        }
        _ => {}
    }
    let status = match session_id.as_deref() {
        Some(session_id) => trace.session_status(session_id),
        None => trace.status(),
    };
    message_sink
        .send_response(Response::success(
            request.id,
            serde_json::to_value(status)?,
        )?)
        .await?;
    Ok(())
}

async fn preview_native_import(
    settings: crate::settings::SettingsStore,
    params: &Value,
    workspace: &str,
    session_id: &str,
    backend: &str,
) -> Result<(
    crate::settings::SettingsStore,
    crate::settings::NativeImport,
)> {
    let kind = serde_json::from_value(params["kind"].clone())?;
    let source = PathBuf::from(
        params["source"]
            .as_str()
            .context("native import source is required")?,
    );
    let scope = serde_json::from_value(params.get("scope").cloned().unwrap_or(json!("workspace")))?;
    let workspace = workspace.to_owned();
    let session_id = session_id.to_owned();
    let backend = backend.to_owned();
    tokio::task::spawn_blocking(move || {
        let preview =
            settings.preview_import(kind, &source, scope, &workspace, &session_id, &backend)?;
        Ok((settings, preview))
    })
    .await
    .context("native import source inspection failed")?
}

async fn permissions(
    registry: Arc<SessionControllerRegistry>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let value = {
        if request.method == "permissions.document.save" {
            let source = request.params["source"]
                .as_str()
                .context("permission source is required")?;
            registry
                .runtime
                .permission_store
                .write()
                .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                .save(source)?;
        }
        let (path, source) = registry
            .runtime
            .permission_store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .open();
        json!({"path":path,"source":source})
    };
    message_sink
        .send_response(Response::success(request.id, value)?)
        .await?;
    Ok(())
}

async fn list_sessions(
    registry: Arc<SessionControllerRegistry>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let workspace = request
        .params
        .get("workspace")
        .and_then(Value::as_str)
        .unwrap_or(&registry.initialize.workspace);
    let workspace = crate::workspace::resolve(std::path::Path::new(workspace))?;
    let workspace = match workspace {
        crate::workspace::WorkspaceKind::Git(path)
        | crate::workspace::WorkspaceKind::Untracked(path) => path.to_string_lossy().into_owned(),
    };
    let scope = request
        .params
        .get("scope")
        .and_then(Value::as_str)
        .unwrap_or("repo");
    ensure!(
        matches!(scope, "repo" | "all"),
        "unknown session list scope"
    );
    let store = SqliteStore::read_only(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let sessions = store
        .list_session((scope != "all").then_some(workspace.as_str()))?
        .into_iter()
        .filter(|session| session.background.is_none())
        .collect::<Vec<_>>();
    message_sink
        .send_response(Response::success(request.id, sessions)?)
        .await?;
    Ok(())
}

async fn create_session(
    registry: Arc<SessionControllerRegistry>,
    session_id: Option<String>,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let source = request
        .params
        .get("source_session_id")
        .and_then(Value::as_str);
    if let (Some(source), Some(context)) = (source, &session_id) {
        ensure!(source == context, "session source and context disagree");
    }
    if let Some(source) = source.or(session_id.as_deref()) {
        return route_new_session(registry.clone(), source.to_owned(), request, message_sink).await;
    }
    let mut initialize = registry.initialize.clone();
    initialize.session_id = None;
    initialize.lease_conflict_action = Some("new".into());
    initialize.new_session_name = Some(
        request
            .params
            .get("name")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned(),
    );
    let runtime = registry.runtime.clone();
    let broker = tokio::task::spawn_blocking(move || {
        HarnessBroker::initialize_with_runtime(initialize, runtime)
    })
    .await
    .context("new session initialization worker failed")??;
    let snapshot = broker.snapshot()?;
    let session = snapshot.session.clone();
    let controller = SessionController::new(broker)?;
    registry
        .controller_by_id
        .write()
        .await
        .insert(session.id.clone(), controller);
    message_sink
        .send_response(Response::success(request.id, snapshot)?)
        .await?;
    message_sink
        .send_event(SessionEvent {
            session_id: session.id.clone(),
            event: "session_created".into(),
            payload: serde_json::to_value(session)?,
        })
        .await?;
    Ok(())
}
