use super::bootstrap::{initialize, service};
use super::response_for;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::time::Duration;

#[tokio::test]
async fn global_defaults_do_not_create_or_modify_sessions() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .initialize_services(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    assert!(registry.controller_by_id.read().await.is_empty());
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            None,
            Request {
                id: 2,
                method: "settings.get".into(),
                params: json!({"provider":"mock", "include_models":true}),
            },
            &sink,
        )
        .await?;
    let defaults = response_for(&mut output, 2).await?["result"].clone();
    assert!(defaults["model_catalog"]["models"].is_array());
    let revision = defaults["revision"]
        .as_u64()
        .context("defaults revision missing")?;
    service
        .dispatch(
            None,
            Request {
                id: 3,
                method: "settings.update".into(),
                params: json!({"scope":"global", "expected_revision":revision,
            "desired":{"approval_mode":"deny", "status_thoughts":false, "subagent_limit":12}}),
            },
            &sink,
        )
        .await?;
    let saved = response_for(&mut output, 3).await?["result"].clone();
    let revision = saved["revision"]
        .as_u64()
        .context("saved revision missing")?;
    service
        .dispatch(
            None,
            Request {
                id: 4,
                method: "settings.update".into(),
                params: json!({"provider":"mock", "expected_revision":revision,
            "provider_settings":{"model":"mock-model", "effort":"low"}}),
            },
            &sink,
        )
        .await?;
    let saved = response_for(&mut output, 4).await?["result"].clone();
    let revision = saved["revision"]
        .as_u64()
        .context("provider revision missing")?;
    assert!(
        registry.controller_by_id.read().await.is_empty(),
        "defaults created a controller"
    );

    let opened = service
        .open_session(5, initialize(&fixture, "mock"))
        .await?;
    let opened = opened.result().context("session result missing")?;
    assert_eq!(opened["subagent_limit"], 12);
    let session_id = opened["session"]["id"]
        .as_str()
        .context("session id missing")?
        .to_owned();
    let controller = registry.resolve(&session_id).await?;
    let before = controller.broker.lock().await.snapshot()?.session;
    assert_eq!(before.approval_mode, crate::session::ApprovalMode::Deny);
    assert_eq!(before.model, "mock-model");
    service
        .dispatch(
            None,
            Request {
                id: 6,
                method: "settings.update".into(),
                params: json!({"scope":"global", "expected_revision":revision,
            "desired":{"approval_mode":"ask", "status_thoughts":true, "subagent_limit":20}}),
            },
            &sink,
        )
        .await?;
    let saved = response_for(&mut output, 6).await?["result"].clone();
    let revision = saved["revision"]
        .as_u64()
        .context("updated revision missing")?;
    service
        .dispatch(
            None,
            Request {
                id: 7,
                method: "settings.update".into(),
                params: json!({"provider":"mock", "expected_revision":revision,
            "provider_settings":{"effort":"high"}}),
            },
            &sink,
        )
        .await?;
    response_for(&mut output, 7).await?;
    let after = controller.broker.lock().await.snapshot()?;
    assert_eq!(after.session.approval_mode, before.approval_mode);
    assert_eq!(after.session.effort, before.effort);
    assert_eq!(serde_json::to_value(after.subagent_limit)?, 12);
    let next = crate::broker::prepare_new_session(
        std::path::Path::new(&registry.initialize.data_root),
        "service-test",
        "mock",
        false,
        &session_id,
        &json!({}),
        1,
    )?;
    assert_eq!(next.approval_mode, crate::session::ApprovalMode::Ask);
    assert_eq!(next.effort, "high");
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}

/// Reject unsupported live resource changes before advancing the persisted settings revision.
#[tokio::test]
async fn unsupported_live_resource_edits_preserve_settings() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let rulesync = fixture.path().join(".rulesync");
    std::fs::create_dir(&rulesync)?;
    std::fs::write(
        rulesync.join("mcp.json"),
        r#"{"mcpServers":{"fixture-server":{"type":"http","url":"http://localhost/mcp"}}}"#,
    )?;
    let service = service();
    let opened = service.open_session(1, initialize(&fixture, "mock")).await?;
    let session_id = opened.result().context("session result missing")?["session"]["id"]
        .as_str()
        .context("session id missing")?
        .to_owned();
    let registry = service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let provider_id = forge_providers::registry::select("antigravity")?;
    let descriptor = forge_providers::registry::descriptor(&provider_id)?;
    let opening = std::sync::Arc::new(tokio::sync::Semaphore::new(0));
    let finish_open = std::sync::Arc::new(tokio::sync::Semaphore::new(0));
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |_request, _sink, _start, _cancelled| async {
            Err(crate::provider::ProviderError::Cancelled)
        },
    ).with_session_open({
        let opening = opening.clone();
        let finish_open = finish_open.clone();
        move |request| {
            let opening = opening.clone();
            let finish_open = finish_open.clone();
            async move {
                assert_eq!(request.configuration.resources.mcp[0].name, "fixture-server");
                opening.add_permits(1);
                finish_open.acquire().await.unwrap().forget();
                Ok(())
            }
        }
    }).with_session_configure(|_, configuration| async move {
        assert_eq!(configuration.resources.mcp[0].name, "fixture-server");
        Ok(())
    });
    let provider = std::sync::Arc::new(crate::provider::Provider::new(
        native.registration(),
        controller.permission.clone(),
        controller.trace.clone(),
        controller.data_root.clone(),
    ));
    let previous_provider = controller.provider_handle();
    *controller.provider.write().unwrap() = provider.clone();

    let workspace = controller.catalog_request.read().await.workspace.clone();
    let settings = crate::settings::SettingsStore::open(&controller.data_root)?;
    let revision = settings.get(&workspace, &session_id, "mock")?.revision;
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "settings.update".into(),
                params: json!({"scope":"session","expected_revision":revision,
                    "desired":{"skill":{"fixture-skill":false}}}),
            },
            &sink,
        )
        .await?;
    response_for(&mut output, 2).await?;
    let opened_settings = settings.get(&workspace, &session_id, "mock")?;
    assert_eq!(opened_settings.revision, revision + 1);
    assert_eq!(opened_settings.desired.skill.get("fixture-skill"), Some(&false));

    let request = controller.broker.lock().await.provider_request(
        crate::provider::ProviderInput::from_user_text(""),
        crate::provider::PromptMode::Chat,
    )?;
    let mut open = Box::pin(provider.thread_open(request.clone()));
    tokio::select! {
        started = opening.acquire() => started?.forget(),
        result = &mut open => anyhow::bail!("binding opened before its resource snapshot barrier: {result:?}"),
    }
    let mut concurrent_edit = Box::pin(service.dispatch(
        Some(session_id.clone()),
        Request {
            id: 9,
            method: "settings.mcp.set_enabled".into(),
            params: json!({"name":"fixture-server","enabled":false}),
        },
        &sink,
    ));
    assert!(futures_util::poll!(concurrent_edit.as_mut()).is_pending());
    assert_eq!(settings.get(&workspace, &session_id, "mock")?.revision, opened_settings.revision);
    // Stopping a prepared prompt drops this waiter while the adapter's exact open remains owned.
    drop(open);
    assert_eq!(concurrent_edit.await.unwrap_err().to_string(), "unsupported provider capability: ResourceSelection");

    for (id, method, params) in [
        (
            3,
            "settings.mcp.set_enabled",
            json!({"name":"fixture-server","enabled":false}),
        ),
        (
            4,
            "settings.update",
            json!({"scope":"session","expected_revision":opened_settings.revision,
                "desired":{"skill":{"fixture-skill":true}}}),
        ),
    ] {
        let error = service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id,
                    method: method.into(),
                    params,
                },
                &sink,
            )
            .await
            .expect_err("unsupported mutation reached settings application");
        assert_eq!(
            error.to_string(),
            "unsupported provider capability: ResourceSelection"
        );
        assert_eq!(
            settings.get(&workspace, &session_id, "mock")?.revision,
            opened_settings.revision
        );
    }
    service
        .dispatch(
            None,
            Request {
                id: 5,
                method: "settings.mcp.set_enabled".into(),
                params: json!({"scope":"workspace","workspace":workspace,"name":"fixture-server","enabled":false}),
            },
            &sink,
        )
        .await?;
    assert_eq!(response_for(&mut output, 5).await?["result"]["state"], "saved");
    let saved = settings.get(&workspace, &session_id, "mock")?;
    assert_eq!(saved.revision, opened_settings.revision + 1);
    assert_eq!(saved.desired.mcp.values().copied().collect::<Vec<_>>(), [false]);
    finish_open.add_permits(1);
    assert!(provider.thread_open(request.clone()).await?.is_some());
    let live_edit = service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 8,
                method: "settings.mcp.set_enabled".into(),
                params: json!({"scope":"session","name":"fixture-server","enabled":false}),
            },
            &sink,
        )
        .await
        .expect_err("the live binding accepted a resource edit");
    assert_eq!(live_edit.to_string(), "unsupported provider capability: ResourceSelection");
    assert_eq!(settings.get(&workspace, &session_id, "mock")?.revision, saved.revision);
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 6,
                method: "session.provider.mcp.list".into(),
                params: json!({}),
            },
            &sink,
        )
        .await?;
    let catalog = response_for(&mut output, 6).await?;
    assert_eq!(catalog["result"][0]["name"], "fixture-server");
    assert_eq!(catalog["result"][0]["enabled"], true);
    provider.thread_configure(request, &saved.desired, None).await?;
    let catalog_request = controller.catalog_request.read().await.clone();
    let crate::provider::ConfigurationResources::Immutable(resources) =
        provider.configuration_resources(&catalog_request).await?
    else {
        anyhow::bail!("configuration lost the binding's initial resource selection");
    };
    settings.begin_application(&session_id, "fixture-settings-confirm", saved.revision)?;
    controller.broker.lock().await.confirm_configuration_application(
        &json!({"operation_id":"fixture-settings-confirm","config":{"settings_revision":saved.revision}}),
        Some(&resources),
    )?;
    let effective = settings
        .application(&session_id)?
        .effective
        .context("settings confirmation has no effective selection")?;
    assert_eq!(effective.mcp.values().copied().collect::<Vec<_>>(), [true]);
    *controller.provider.write().unwrap() = previous_provider;
    provider.shutdown().await?;
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}

/// Start a controller over a provider whose turns complete at once.
async fn permissions_fixture(
    root: &tempfile::TempDir,
) -> Result<(
    std::sync::Arc<super::super::SessionController>,
    crate::provider::test_support::ProviderTestSession,
    forge_protocol::outbound::MessageSender,
    tokio::task::JoinHandle<Result<()>>,
)> {
    crate::checks::configure_fixture(root.path());
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |_request, _sink, _start, _cancelled| async move {
            Err(crate::provider::ProviderError::Cancelled)
        },
    );
    super::approval_deadline::start_controller(root, native).await
}

/// Run one session command through the owner, as the editor's command path does.
async fn execute(
    controller: &super::super::SessionController,
    sink: &forge_protocol::outbound::MessageSender,
    method: &str,
    params: serde_json::Value,
) -> Result<()> {
    let request = Request {
        id: 1,
        method: method.into(),
        params,
    };
    let command = if method == "session.configure" {
        super::super::native_settings::decode_command(&request)?
    } else {
        super::super::native_permission::decode_command(&request)?
    };
    controller
        .exchange_owner
        .execute_identified(command, sink.clone())
        .await
        .with_context(|| format!("{method} failed"))?;
    Ok(())
}

/// A provider whose model list answers late does not hold a session's open, which a provider
/// switch waits on: the controller exists while the catalog query still waits, and the late
/// catalog then corrects an inherited thinking level the model does not offer.
#[tokio::test(start_paused = true)]
async fn session_open_does_not_wait_for_a_slow_model_list() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let (answer, answered) = tokio::sync::watch::channel(false);
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |_request, _sink, _start, _cancelled| async move {
            Err(crate::provider::ProviderError::Cancelled)
        },
    )
    .with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"], "default_reasoning":"medium",
        "context_window":[], "vision":false, "is_default":true,
    }))?])
    .with_model_list_answer(move || {
        let mut answered = answered.clone();
        async move {
            answered
                .wait_for(|answered| *answered)
                .await
                .map_err(|_| crate::provider::ProviderError::Cancelled)?;
            Ok(())
        }
    });
    let request = initialize(&root, "mock");
    let data_root = std::path::PathBuf::from(&request.data_root);
    crate::storage::layout::prepare(&data_root)?;
    crate::settings::SettingsStore::open(&data_root)?.save_provider(
        0,
        "mock",
        &json!({"effort":"xhigh"}),
    )?;
    let mut broker = crate::broker::HarnessBroker::initialize(request)?;
    let provider =
        crate::provider::test_support::ProviderTestSession::from_native_with_permissions(
            native,
            broker.permission_coordinator(),
        )?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = super::super::SessionController::new(broker).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        let opened = controller.observer.snapshot()?.session;
        anyhow::ensure!(
            opened.effort == "xhigh",
            "the open resolved the inherited level before the catalog answered: {}",
            opened.effort
        );
        controller.start_provider(&sink).await;
        answer.send_replace(true);
        // The paused clock bounds a correction that never comes without waiting in real time.
        tokio::time::timeout(Duration::from_secs(3600), async {
            while controller.observer.snapshot()?.session.effort != "medium" {
                tokio::time::sleep(Duration::from_millis(10)).await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("the late catalog never corrected the inherited thinking level")??;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = super::approval_deadline::stop_controller(controller, provider, reader).await;
    scenario?;
    teardown
}

/// A thinking level chosen in a session is that workspace's default, never the global one: another
/// workspace's new sessions keep the global provider default, this workspace's new sessions start
/// at the session's choice, and a later explicit global edit is the newer default everywhere.
#[tokio::test]
async fn session_thinking_choice_stays_with_its_workspace() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (controller, provider, sink, reader) = permissions_fixture(&root).await?;
    let scenario = async {
        let session = controller.observer.snapshot()?.session;
        let settings = || crate::settings::SettingsStore::open(&controller.data_root);
        let revision = settings()?
            .get(&session.workspace, "", &session.provider)?
            .revision;
        settings()?.save_provider(revision, &session.provider, &json!({"effort":"low"}))?;
        execute(
            &controller,
            &sink,
            "session.configure",
            json!({"effort":"high"}),
        )
        .await?;
        let global = settings()?
            .get("another-workspace", "", &session.provider)?
            .provider;
        anyhow::ensure!(
            global["effort"] == "low",
            "a session's thinking choice changed the global provider default: {global}"
        );
        let new_session = || {
            crate::broker::prepare_new_session(
                &controller.data_root,
                "service-test",
                &session.provider,
                false,
                &session.id,
                &json!({}),
                1,
            )
        };
        let same_workspace = new_session()?;
        anyhow::ensure!(
            same_workspace.effort == "high",
            "a new session in the workspace starts at {} instead of its last confirmed level",
            same_workspace.effort
        );
        let revision = settings()?
            .get(&session.workspace, "", &session.provider)?
            .revision;
        settings()?.save_provider(revision, &session.provider, &json!({"effort":"medium"}))?;
        let after_global_edit = new_session()?;
        anyhow::ensure!(
            after_global_edit.effort == "medium",
            "a later global default edit was ignored: the new session starts at {}",
            after_global_edit.effort
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = super::approval_deadline::stop_controller(controller, provider, reader).await;
    scenario?;
    teardown
}

/// The Permissions tab edits the current session, as the slash commands do, so the dialog's
/// saved access agrees with the session's effective scope after mixing both.
#[tokio::test]
async fn config_write_scope_after_yolo_shows_the_session_scope() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (controller, provider, sink, reader) = permissions_fixture(&root).await?;
    let scenario = async {
        let yolo = json!({"action":"permission","change":{"kind":"enable_yolo"}});
        execute(&controller, &sink, "session.command.apply", yolo.clone()).await?;
        execute(&controller, &sink, "session.command.apply", yolo).await?;
        let mut access = serde_json::to_value(controller.observer.snapshot()?.session.access)?;
        access["write_access"] = json!("workspace");
        execute(
            &controller,
            &sink,
            "session.configure",
            json!({"access": access}),
        )
        .await?;
        execute(
            &controller,
            &sink,
            "session.command.apply",
            json!({"action":"permission","change":{"kind":"set_approval","mode":"ask"}}),
        )
        .await?;
        let snapshot = controller.broker.lock().await.snapshot()?;
        anyhow::ensure!(
            snapshot.session.access.write_access == crate::session::WriteScope::Workspace,
            "the session's write scope is {:?}",
            snapshot.session.access.write_access
        );
        anyhow::ensure!(
            snapshot.access_preference.write_access == crate::session::WriteScope::Workspace,
            "/config shows write scope {:?} for a workspace session",
            snapshot.access_preference.write_access
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = super::approval_deadline::stop_controller(controller, provider, reader).await;
    scenario?;
    teardown
}

/// A session's timeout edit stays with that session: the global defaults that other sessions
/// and new sessions read are unchanged, and the session's own approval deadline follows it.
#[tokio::test]
async fn session_timeout_edit_stays_out_of_the_global_defaults() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (controller, provider, sink, reader) = permissions_fixture(&root).await?;
    let scenario = async {
        let session_id = controller.observer.session_id().to_owned();
        let workspace = controller.observer.snapshot()?.session.workspace;
        let request = Request {
            id: 1,
            method: "session.input.timeout.configure".into(),
            params: json!({"approval_timeout":{"amount":45,"unit":"s"}}),
        };
        super::super::local_configuration::apply(&controller, &request, &sink).await?;
        let settings = crate::settings::SettingsStore::open(&controller.data_root)?;
        let global = settings
            .layer_settings(
                crate::settings::SettingScope::Global,
                &workspace,
                &session_id,
            )?
            .approval_timeout;
        anyhow::ensure!(
            global.is_none(),
            "the session's timeout edit changed the global defaults: {global:?}"
        );
        let other = settings
            .get(&workspace, "other-session", "mock")?
            .desired
            .approval_timeout;
        anyhow::ensure!(
            other.is_none(),
            "another session sees this session's timeout: {other:?}"
        );
        let expected = crate::session::InputTimeout {
            amount: 45,
            unit: crate::session::TimeoutUnit::S,
        };
        anyhow::ensure!(
            controller.permission.approval_timeout(&session_id)? == expected,
            "the session's approval deadline did not follow its edit"
        );
        // A later configuration must not restore the timeout the session started with.
        execute(
            &controller,
            &sink,
            "session.command.apply",
            json!({"action":"permission","change":{"kind":"set_approval","mode":"deny"}}),
        )
        .await?;
        let snapshot = controller.broker.lock().await.snapshot()?;
        anyhow::ensure!(
            controller.permission.approval_timeout(&session_id)? == expected
                && snapshot.session.approval_timeout == expected,
            "a later configuration restored a stale approval timeout"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = super::approval_deadline::stop_controller(controller, provider, reader).await;
    scenario?;
    teardown
}

/// Apply and publish a session override without changing the inherited limit of other sessions.
#[tokio::test]
async fn subagent_limit_edit_applies_to_the_current_session() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (controller, provider, sink, reader) = permissions_fixture(&root).await?;
    let scenario = async {
        let initial = controller.observer.snapshot()?;
        assert_eq!(serde_json::to_value(initial.subagent_limit)?, 8);
        execute(
            &controller,
            &sink,
            "session.configure",
            json!({"subagent_limit":24}),
        )
        .await?;

        let current = controller.broker.lock().await.snapshot()?;
        assert_eq!(serde_json::to_value(current.subagent_limit)?, 24);
        let settings = crate::settings::SettingsStore::read_only(&controller.data_root)?;
        let saved = settings.get(&current.session.workspace, &current.session.id, "mock")?;
        assert_eq!(serde_json::to_value(saved.desired.subagent_limit)?, 24);
        let other = settings.get(&current.session.workspace, "other-session", "mock")?;
        assert!(other.desired.subagent_limit.is_none());
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = super::approval_deadline::stop_controller(controller, provider, reader).await;
    scenario?;
    teardown
}
