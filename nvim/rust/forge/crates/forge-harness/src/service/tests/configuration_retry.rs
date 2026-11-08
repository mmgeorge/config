//! Verify deferred native configuration against actual child availability and durable desired revisions.
use super::super::{HarnessBroker, SessionController, native_settings, prompt};
use super::bootstrap::initialize;
use crate::provider::ScopeOwner;
use anyhow::{Context, Result, ensure};
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, AtomicUsize, Ordering},
};

/// Retain newest preferences through a live child without treating metadata as collection or reviving a canceled root.
#[tokio::test]
async fn latest_configuration_waits_for_actual_child_end_and_preserves_canceled_root() -> Result<()>
{
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let parent = Arc::new(Mutex::new(None));
    let prompts = Arc::new(AtomicUsize::new(0));
    let attempts = Arc::new(AtomicUsize::new(0));
    let child_active = Arc::new(AtomicBool::new(false));
    let closed = Arc::new(AtomicUsize::new(0));
    let started = Arc::new(tokio::sync::Notify::new());
    let native = crate::provider::test_support::EventFixtureProvider::new(
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().unwrap(),
        )?,
        {
            let parent = parent.clone();
            let prompts = prompts.clone();
            let started = started.clone();
            let child_active = child_active.clone();
            move |request, writer, start, mut cancelled| {
                let parent = parent.clone();
                let prompts = prompts.clone();
                let started = started.clone();
                let child_active = child_active.clone();
                async move {
                    prompts.fetch_add(1, Ordering::AcqRel);
                    *parent.lock().unwrap() = Some(request.scope.clone());
                    start.observed(1).await?;
                    writer
                        .send_wait(forge_provider_api::AdapterActivity::Agent {
                            agent_id: forge_provider_api::AgentId::new("background-child").unwrap(),
                            event: forge_provider_api::agent::AgentEvent::Lifecycle {
                                event: forge_provider_api::agent::AgentLifecycleEvent::Started {
                                    agent: forge_provider_api::agent::AgentDescription {
                                        parent_agent_id: Some(
                                            request.scope.owner().agent_id().clone(),
                                        ),
                                        delegation: Some(
                                            forge_provider_api::agent::AgentDelegation {
                                                parent_thread_id: request.thread_id.clone(),
                                                parent_turn_id: request.turn_id.clone(),
                                                parent_item_id: None,
                                            },
                                        ),
                                        thread_id: Some(
                                            forge_provider_api::ThreadId::new("child-thread")
                                                .unwrap(),
                                        ),
                                        definition: None,
                                        nickname: None,
                                        task: Some("Continue independent work".into()),
                                    },
                                    state: forge_provider_api::agent::AgentActiveState::Running,
                                },
                            },
                        })
                        .await?;
                    child_active.store(true, Ordering::Release);
                    started.notify_one();
                    let _ = cancelled.wait_for(|cancelled| *cancelled).await;
                    Ok(crate::provider::test_support::native_result(
                        &request,
                        forge_provider_api::turn::TurnOutcome::Interrupted,
                    ))
                }
            }
        },
    );
    native.with_models(vec![serde_json::from_value(json!({"id":"fixture-model","reasoning":["low","medium","high"],"context_window":[],"vision":false,"is_default":true}))?]);
    native.with_session_configure_guard({
        let child_active = child_active.clone();
        let attempts = attempts.clone();
        move |_, _| {
            let child_active = child_active.clone();
            let attempts = attempts.clone();
            async move {
                attempts.fetch_add(1, Ordering::AcqRel);
                if child_active.load(Ordering::Acquire) {
                    return Err(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::ThreadConfigure,
                        forge_provider_api::ProviderFailureKind::ExecutionActive,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Admission,
                        forge_provider_api::FailureDisposition::Unchanged,
                    )
                    .into());
                }
                Ok(())
            }
        }
    });
    native.with_session_close({
        let closed = closed.clone();
        move |_| {
            closed.fetch_add(1, Ordering::AcqRel);
            async { Ok(()) }
        }
    });
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native.clone())?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        controller.start_provider(&sink).await;
        async { while !controller.provider_ready().await { tokio::task::yield_now().await; } }.await;
        controller.attach_input(&sink).await?;
        prompt::apply(&controller, Request { id: 1, method: "session.prompt.submit".into(), params: json!({"operation_id":"canceled-root","text":"Do not revive this canceled request"}) }, &sink).await?;
        let session = controller.observer.session_id().to_owned();
        started.notified().await;
        let parent = parent.lock().unwrap().clone().context("parent scope missing")?;
        let owner = crate::provider::ThreadOwner::root(&session);
        let writer = native.event_sink(&owner).await?;
        let binding = writer.binding_id().clone();
        let child = forge_provider_api::AgentId::new("background-child")?;
        let description = forge_provider_api::agent::AgentDescription {
            parent_agent_id: Some(owner.agent_id().clone()),
            delegation: Some(forge_provider_api::agent::AgentDelegation { parent_thread_id: owner.thread_id(), parent_turn_id: parent.turn_id()?, parent_item_id: None }),
            thread_id: Some(forge_provider_api::ThreadId::new("child-thread")?), definition: None, nickname: None, task: Some("Continue independent work".into()),
        };
        let workspace = controller.catalog_request.read().await.workspace.clone();
        let toggle = !controller.observer.snapshot()?.session_tools;
        let mut latest = 0;
        for amount in [1, 2] {
            let mut settings = crate::settings::SettingsStore::open(&controller.data_root)?;
            let revision = settings.get(&workspace, &session, "mock")?.revision;
            latest = settings.update(revision, crate::settings::SettingScope::Session, &workspace, &session,
                &crate::settings::PortableSettings { session_tools: Some(toggle), tool_timeout: Some(crate::session::InputTimeout { amount, unit: crate::session::TimeoutUnit::M }), ..Default::default() })?;
            let command = native_settings::decode_command(&Request { id: 2, method: "session.command.apply".into(), params: json!({"operation_id":format!("desired-{amount}"),"action":"configure","config":{"settings_revision":latest}}) })?;
            let error = controller.exchange_owner.execute_identified(command, sink.clone()).await.expect_err("active child configuration unexpectedly committed");
            ensure!(matches!(error, crate::exchange::command::ExecuteCommandError::Failed(crate::exchange::command::CommandFailure::ConfigurationRejected(crate::exchange::command::ConfigurationRejection::Provider(forge_provider_api::ProviderFailureKind::ExecutionActive)))), "guard did not certify unchanged native-child rejection: {error:#}");
            ensure!(settings.application(&session)?.applied_revision != Some(latest), "rejected native configuration was marked applied");
        }
        let canceled = controller.exchange_owner.execute_identified(crate::exchange::command::CommandRequest {
            id: crate::exchange::command::CommandId::new("ordered-root-cancel")?,
            command: crate::exchange::command::Command::CancelExchange(Default::default()),
            input_control: crate::exchange::command::InputControl::AtExecution,
        }, sink.clone()).await.context("explicit ordered root cancellation failed")?;
        ensure!(canceled.sequence > 0, "cancel lost its durable ordered identity");
        ensure!(closed.load(Ordering::Acquire) == 0, "root cancellation retired a binding with admitted live native child");
        let before = attempts.load(Ordering::Acquire);
        let availability = controller.observer.session_events().configuration_availability();
        let history = controller.observer.snapshot()?.session.provider_session_id.context("native history missing")?;
        writer.send_wait(forge_provider_api::AdapterActivity::Thread { thread_id: owner.thread_id(), event: forge_provider_api::ThreadEvent::IdentityResolved(history.try_into()?) }).await.context("identity delivery failed after ordered root cancellation")?;
        writer.send_wait(forge_provider_api::AdapterActivity::Thread { thread_id: owner.thread_id(), event: forge_provider_api::ThreadEvent::ContextUsage(forge_provider_api::context::ContextUsage::reported(7, 100).unwrap()) }).await?;
        async {
            while controller.observer.snapshot()?.session.context_usage.as_ref().map(|usage| usage.used) != Some(7) { tokio::task::yield_now().await; }
            Ok::<(), anyhow::Error>(())
        }.await?;
        ensure!(controller.observer.session_events().configuration_availability() == availability, "identity or token metadata invented availability");
        ensure!(attempts.load(Ordering::Acquire) == before, "identity or token metadata retried native configuration");
        child_active.store(false, Ordering::Release);
        writer.send_wait(forge_provider_api::AdapterActivity::Agent { agent_id: child, event: forge_provider_api::agent::AgentEvent::Lifecycle { event: forge_provider_api::agent::AgentLifecycleEvent::Finished { agent: description, outcome: forge_provider_api::agent::AgentOutcome::Completed } } }).await?;
        async {
            loop {
                let settings = crate::settings::SettingsStore::open(&controller.data_root)?;
                if settings.application(&session)?.applied_revision == Some(latest) { return Ok::<(), anyhow::Error>(()); }
                tokio::task::yield_now().await;
            }
        }.await?;
        ensure!(attempts.load(Ordering::Acquire) == before + 1, "latest desired configuration was not applied once");
        ensure!(prompts.load(Ordering::Acquire) == 1, "configuration resurrected the canceled root prompt");
        ensure!(closed.load(Ordering::Acquire) == 0 && native.event_sink(&owner).await?.binding_id() == &binding, "configuration fenced the unchanged native binding");
        let snapshot = controller.observer.snapshot()?;
        ensure!(snapshot.session_tools == toggle && snapshot.session.tool_timeout.amount == 2, "older desired preferences won the retry");
        let exchanges = crate::storage::SqliteStore::read_only(&controller.data_root)?.list_exchange(&session)?;
        ensure!(exchanges.len() == 1 && exchanges[0].state() == crate::exchange::ExchangeState::Cancelled, "configuration replaced canceled request history");
        Ok::<(), anyhow::Error>(())
    }.await;
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    let cleanup = async {
        controller.finish_exchange_owner().await?;
        provider.provider_handle().shutdown().await?;
        controller.finish_session_events().await?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    reader.abort();
    if let Err(error) = scenario {
        return Err(super::owner_fixture::retain_cleanup_failure(
            root,
            &controller,
            error,
        ));
    }
    cleanup?;
    Ok(())
}
