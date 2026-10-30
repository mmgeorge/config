fn goal_command_request(transport: &str, command: &str, id: u64) -> Request {
    let operation = format!("{transport}-{command}-{id}");
    let text = if command == "goal" {
        "/goal Preserve the held provider".to_owned()
    } else {
        format!("/goal {command}")
    };
    let (method, params) = match transport {
        "transition" => (
            "session.command.apply".to_owned(),
            if command == "goal" {
                json!({"operation_id":operation,"action":"goal","text":"Preserve the held provider"})
            } else {
                json!({"operation_id":operation,"action":command})
            },
        ),
        "rpc" => (
            if command == "goal" {
                "session.goal.set".into()
            } else {
                format!("session.goal.{command}")
            },
            json!({"operation_id":operation,"objective":"Preserve the held provider"}),
        ),
        "prompt" => (
            "session.prompt.submit".into(),
            json!({"operation_id":operation,"text":text}),
        ),
        "steer" => (
            "session.turn.steer".into(),
            json!({"operation_id":operation,"text":text}),
        ),
        "input" => (
            "session.input.apply".into(),
            json!({"id":operation,"operation_id":operation,
            "action":{"kind":"submit","message":{"id":operation,"text":text,"intent":"auto","attachments":[]}}}),
        ),
        _ => unreachable!(),
    };
    Request { id, method, params }
}

#[tokio::test]
async fn goal_command_aliases_preserve_held_provider_and_idempotent_receipts() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    for transport in ["transition", "rpc", "prompt", "input", "steer"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let control = controller.backend.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().unwrap().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.input.apply".into(),
                    params: json!({"id":"original","action":{"kind":"submit","message":{
                "id":"original","text":"Inspect startup","intent":"auto","attachments":[]}}}),
                },
                &sink,
            )
            .await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let store = SqliteStore::open(&controller.data_root)?;
        let original = store.latest_exchange(&session_id)?.unwrap();
        for (index, command) in ["goal", "pause", "resume", "clear"].into_iter().enumerate() {
            let request = goal_command_request(transport, command, 10 + index as u64);
            tokio::time::timeout(
                Duration::from_secs(1),
                service.dispatch(Some(session_id.clone()), request.clone(), &sink),
            )
            .await??;
            if matches!(command, "goal" | "resume") {
                tokio::time::timeout(Duration::from_secs(5), async {
                    loop {
                        if let Some(task) = controller.task_controller.current()?
                            && task.status() == crate::task::TaskStatus::Running
                            && task.entry_accepted()
                            && let Some(entry) = &task.entry
                            && store
                                .load_session_event(
                                    &session_id,
                                    &format!("{session_id}:task-start:{}", entry.id.as_str()),
                                )?
                                .is_some()
                        {
                            break;
                        }
                        tokio::task::yield_now().await;
                    }
                    Ok::<(), anyhow::Error>(())
                })
                .await
                .context("goal command entry was not acknowledged")??;
            } else if command == "pause" {
                assert_eq!(
                    controller.task_controller.current()?.unwrap().status(),
                    crate::task::TaskStatus::Paused
                );
            } else {
                assert!(controller.task_controller.current()?.is_none());
            }
            let before = serde_json::to_value(controller.task_controller.current()?)?;
            let events = store.list_session_event(&session_id)?.len();
            assert_eq!(
                store
                    .load_task_operation(
                        &session_id,
                        request.params["operation_id"].as_str().unwrap()
                    )?
                    .unwrap()
                    .state,
                "completed"
            );
            tokio::time::timeout(
                Duration::from_secs(1),
                service.dispatch(Some(session_id.clone()), request, &sink),
            )
            .await??;
            assert_eq!(
                serde_json::to_value(controller.task_controller.current()?)?,
                before,
                "{transport} {command} replay mutated the task"
            );
            assert_eq!(store.list_session_event(&session_id)?.len(), events);
            assert!(!*controller.cancellation.subscribe().borrow());
            assert_eq!(controller.coordinator.execution.available_permits(), 0);
            let history = store.list_exchange(&session_id)?;
            assert_eq!(
                history.len(),
                1,
                "{transport} {command} replaced the active exchange"
            );
            assert_eq!(history[0].id, original.id);
            assert_eq!(history[0].state(), crate::exchange::ExchangeState::Running);
            assert!(!history[0].node_list.iter().any(|node| matches!(node,
                crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text.trim_start().starts_with("/goal"))));
        }
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert_eq!(store.list_exchange(&session_id)?.len(), 1);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn goal_continue_aliases_reuse_the_completed_source_and_receipt() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    for transport in ["transition", "prompt", "input", "steer"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 2,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":"Inspect startup"}),
                },
                &sink,
            )
            .await?;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let source = store
            .latest_exchange(&session_id)?
            .context("completed source missing")?;
        assert!(source.is_finished());
        controller.task_controller.change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::ActiveTask::goal(
                "Preserve the held provider".into(),
                crate::session::continuation::ContinuationBudget::new(20, 2),
                false,
                1,
            ))),
            1,
        )?;
        let control = controller.backend.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().unwrap().to_owned();
        let request = goal_command_request(transport, "continue", 3);
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let task = serde_json::to_value(controller.task_controller.current()?)?;
        assert_eq!(
            store
                .load_task_operation(
                    &session_id,
                    request.params["operation_id"].as_str().unwrap()
                )?
                .unwrap()
                .state,
            "completed"
        );
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request, &sink),
        )
        .await??;
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?)?,
            task
        );
        let history = store.list_exchange(&session_id)?;
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].id, source.id);
        assert!(matches!(
            history[1].origin,
            crate::exchange::ExchangeOrigin::TaskContinuation { .. }
        ));
        assert!(history[1].prompt.contains("Preserve the held provider"));
        assert!(!history[1].prompt.trim_start().starts_with("/goal"));
        assert!(!*controller.cancellation.subscribe().borrow());
        service
            .dispatch(
                Some(session_id.clone()),
                goal_command_request("transition", "clear", 4),
                &sink,
            )
            .await?;
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert_eq!(store.list_exchange(&session_id)?.len(), 2);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn queued_input_edited_to_goal_is_a_state_command_with_one_receipt() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    use crate::input::{InputAction, InputEdit, InputIntent, InputMessage, InputOperation};
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let control = controller.backend.mock_response_control().unwrap();
    let hold = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = hold["id"].as_str().unwrap().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.input.apply".into(),
                params: json!({"id":"original","action":{"kind":"submit","message":{
            "id":"original","text":"Inspect startup","intent":"auto","attachments":[]}}}),
            },
            &sink,
        )
        .await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    let store = SqliteStore::open(&controller.data_root)?;
    let original = store.latest_exchange(&session_id)?.unwrap();
    controller.coordinator.submissions.apply(InputOperation {
        id: "queue-earlier".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage {
                id: "earlier".into(),
                revision: 0,
                text: "Keep this earlier queued follow-up".into(),
                purpose: Default::default(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Queue,
                delivery: Default::default(),
                route: None,
                reservation: None,
                submission_id: None,
                error: None,
            }),
        },
    })?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "queue-original".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage {
                id: "queued".into(),
                revision: 0,
                text: "Ordinary queued follow-up".into(),
                purpose: Default::default(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
                delivery: Default::default(),
                route: None,
                reservation: None,
                submission_id: None,
                error: None,
            }),
        },
    })?;
    let queued = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == "queued")
        .unwrap();
    let reserved = controller.coordinator.submissions.apply(InputOperation {
        id: "reserve-edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Reserve {
            id: queued.id.clone(),
            revision: queued.revision,
            reservation: "editor".into(),
        },
    })?;
    let reserved_revision = reserved
        .upsert
        .iter()
        .find(|message| message.id == "queued")
        .unwrap()
        .revision;
    let edit = InputOperation {
        id: "commit-edit".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Commit {
            reservation: "editor".into(),
            edits: vec![InputEdit {
                id: queued.id,
                revision: reserved_revision,
                text: "/goal Preserve the edited intent".into(),
                attachments: Vec::new(),
                config: None,
            }],
        },
    };
    let update = controller.coordinator.submissions.apply(edit.clone())?;
    let revision = update
        .upsert
        .iter()
        .find(|message| message.id == "queued")
        .unwrap()
        .revision;
    let receipt_id = format!("input-command:queued:{revision}");
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if store
                .load_task_operation(&session_id, &receipt_id)?
                .is_some()
                && controller
                    .task_controller
                    .current()?
                    .is_some_and(|task| task.entry_accepted())
                && !controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .iter()
                    .any(|message| message.id == "queued")
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("edited goal command was not admitted")??;
    let task = controller.task_controller.current()?.unwrap();
    assert_eq!(task.title(), "Preserve the edited intent");
    let event_id = format!(
        "{session_id}:task-start:{}",
        task.entry.as_ref().unwrap().id.as_str()
    );
    tokio::time::timeout(Duration::from_secs(5), async {
        while store.load_session_event(&session_id, &event_id)?.is_none() {
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await??;
    let event_count = store.list_session_event(&session_id)?.len();
    assert_eq!(
        store
            .load_task_operation(&session_id, &receipt_id)?
            .unwrap()
            .state,
        "completed"
    );
    controller.coordinator.submissions.apply(edit)?;
    assert_eq!(
        controller.task_controller.current()?.unwrap().revision,
        task.revision
    );
    assert!(
        !controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .any(|message| message.id == "queued")
    );
    assert_eq!(store.list_session_event(&session_id)?.len(), event_count);
    let earlier = controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .find(|message| message.id == "earlier")
        .context("edited command removed earlier queued input")?;
    assert_eq!(earlier.text, "Keep this earlier queued follow-up");
    assert_eq!(earlier.intent, InputIntent::Queue);
    assert!(!*controller.cancellation.subscribe().borrow());
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    let history = store.list_exchange(&session_id)?;
    assert_eq!(history.len(), 1);
    assert_eq!(history[0].id, original.id);
    assert!(!history[0].node_list.iter().any(|node| matches!(node,
        crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text.starts_with("/goal"))));
    controller.coordinator.submissions.apply(InputOperation {
        id: "remove-fixture-follow-up".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Remove {
            id: earlier.id,
            revision: earlier.revision,
        },
    })?;
    service
        .dispatch(
            Some(session_id.clone()),
            goal_command_request("transition", "clear", 3),
            &sink,
        )
        .await?;
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
        .await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(store.list_exchange(&session_id)?.len(), 1);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

#[tokio::test]
async fn plan_replacement_waits_for_turn_boundary_without_cancelling() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    for planning in [false, true] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let control = controller.backend.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        if planning {
            let request = Request {
                id: 2,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"original-plan","action":"plan","text":"Original plan"}),
            };
            service.prepare_task(Some(&session_id), &request).await?;
            service
                .dispatch(Some(session_id.clone()), request, &sink)
                .await?;
        } else {
            controller.coordinator.submissions.apply(InputOperation {
                id: "original".into(),
                control_revision: 0,
                after_control: None,
                action: InputAction::Submit {
                    message: Box::new(InputMessage {
                        id: "original-input".into(),
                        revision: 0,
                        text: "Inspect startup".into(),
                        purpose: Default::default(),
                        attachments: Vec::new(),
                        config: None,
                        intent: InputIntent::Auto,
                        delivery: Default::default(),
                        route: None,
                        reservation: None,
                        submission_id: None,
                        error: None,
                    }),
                },
            })?;
        }
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let original = store.latest_exchange(&session_id)?.unwrap();
        let request = Request {
            id: 3,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"replacement-plan","action":"plan","text":"Replacement plan"}),
        };
        service.prepare_task(Some(&session_id), &request).await?;
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        let pending = controller.task_controller.current()?.unwrap();
        assert_eq!(pending.title(), "Replacement plan");
        assert!(!pending.entry_accepted());
        assert!(!*controller.cancellation.subscribe().borrow());
        assert_eq!(store.list_exchange(&session_id)?.len(), 1);
        assert_eq!(store.latest_exchange(&session_id)?.unwrap().id, original.id);
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let current = controller.task_controller.current()?.unwrap();
                if matches!(&current.workflow, crate::task::TaskWorkflow::Plan { plan }
                    if plan.artifact.state == crate::plan::PlanState::AwaitingReview)
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("replacement plan did not reach review")??;
        let current = controller.task_controller.current()?.unwrap();
        assert!(current.entry_accepted());
        let crate::task::TaskWorkflow::Plan { plan } = &current.workflow else {
            unreachable!()
        };
        assert_eq!(
            store.load_plan(&plan.artifact.id)?.unwrap().state,
            crate::plan::PlanState::AwaitingReview
        );
        let observed = controller.observer.snapshot()?;
        assert_eq!(
            serde_json::to_value(observed.active_plan.as_ref().unwrap())?,
            serde_json::to_value(&plan.artifact)?
        );
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(exchanges.len(), 2);
        assert!(
            matches!(&exchanges[1].origin, crate::exchange::ExchangeOrigin::TaskStarted { start }
            if start.title == "Replacement plan")
        );
        assert!(exchanges[1].prompt.contains("Replacement plan"));
        service
            .dispatch(Some(session_id.clone()), request, &sink)
            .await?;
        assert_eq!(
            controller.task_controller.current()?.unwrap().revision,
            current.revision
        );
        assert_eq!(store.list_exchange(&session_id)?.len(), 2);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn goal_continuation_uses_durable_input_and_yields_to_user_queue() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    for queued_user in [false, true] {
        let fixture = tempfile::tempdir()?;
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let control = controller.backend.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        let access = crate::session::AccessPolicy {
            write_access: crate::session::WriteScope::Off,
            ..Default::default()
        };
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 4,
                    method: "session.configure".into(),
                    params: json!({"access":access}),
                },
                &sink,
            )
            .await?;
        controller.attach_input(&sink).await?;
        service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "session.command.apply".into(),
            params: json!({"operation_id":"start-goal","action":"goal","text":"Finish the work"}),
        }, &sink).await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        if queued_user {
            controller
                .input_request(
                    Request {
                        id: 3,
                        method: "session.input.apply".into(),
                        params: json!({"id":"queued-user","action":{"kind":"submit","message":{
                            "id":"user-message", "text":"Use the revised requirement",
                            "intent":"queue", "attachments":[],"config":null,
                            "reservation":null,"submission_id":null,"error":null
                        }}}),
                    },
                    RuntimeMethod::SessionInputApply,
                    &sink,
                )
                .await?;
        }
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if controller.task_controller.current()?.is_some_and(|task| {
                    matches!(task.workflow, crate::task::TaskWorkflow::Goal { goal }
                        if goal.state == crate::goal::GoalState::Stalled)
                }) && controller.coordinator.execution.available_permits() > 0
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("goal queue did not settle")??;
        let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(exchanges.len(), 2);
        assert!(
            exchanges
                .iter()
                .all(|exchange| exchange.state() == crate::exchange::ExchangeState::Complete)
        );
        if queued_user {
            assert!(matches!(
                exchanges[1].origin,
                crate::exchange::ExchangeOrigin::User
            ));
            assert_eq!(exchanges[1].prompt, "Use the revised requirement");
        } else {
            assert!(matches!(
                exchanges[1].origin,
                crate::exchange::ExchangeOrigin::TaskContinuation { .. }
            ));
            assert_eq!(
                exchanges[1].prompt,
                "Continue working toward this goal: Finish the work"
            );
        }
        let task = store.load_active_task(&session_id)?.unwrap();
        assert_eq!(task.turn.unwrap().id.exchange_ordinal, exchanges[1].ordinal);
        assert!(!*controller.cancellation.subscribe().borrow());
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn task_recovery_keeps_one_unsent_continuation() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    let fixture = tempfile::tempdir()?;
    let original_service = service();
    original_service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = original_service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let access = crate::session::AccessPolicy {
        write_access: crate::session::WriteScope::Off,
        ..Default::default()
    };
    original_service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 2,
                method: "session.configure".into(),
                params: json!({"access":access}),
            },
            &sink,
        )
        .await?;
    let control = controller.backend.mock_response_control().unwrap();
    let held = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = held["id"].as_str().unwrap().to_owned();
    controller.attach_input(&sink).await?;
    original_service.dispatch(Some(session_id.clone()), Request {
        id: 3, method: "session.command.apply".into(),
        params: json!({"operation_id":"restart-goal","action":"goal","text":"Finish after restart"}),
    }, &sink).await?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    let admission = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
        .await?;
    let pending = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if let Some(task) = controller.task_controller.current()?
                && let Some(input) = task
                    .turn
                    .as_ref()
                    .and_then(|turn| turn.continuation.as_ref())
                && controller.coordinator.execution.available_permits() > 0
            {
                break Ok::<_, anyhow::Error>(input.clone());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .with_context(|| {
        format!(
            "pending continuation did not settle: permits={}, task={:?}",
            controller.coordinator.execution.available_permits(),
            controller
                .task_controller
                .current()
                .map(|task| task.map(|task| (task.status(), task.turn)))
        )
    })??;
    assert_eq!(pending.delivery, crate::task::TaskInputDelivery::Pending);
    original_service.shutdown(Duration::from_secs(2)).await?;
    drop(admission);
    drop(controller);
    drop(registry);
    drop(original_service);
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;

    let recovered_service = service();
    let mut request = initialize(&fixture, "mock");
    request.session_id = Some(session_id.clone());
    recovered_service.open_session(4, request).await?;
    let registry = recovered_service.registry().await?;
    let controller = registry.resolve(&session_id).await?;
    let task = controller.task_controller.current()?.unwrap();
    assert_eq!(task.status(), crate::task::TaskStatus::Running);
    assert_eq!(
        task.turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .delivery,
        crate::task::TaskInputDelivery::Pending
    );
    assert_eq!(
        task.turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .id,
        pending.id
    );
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let resumed = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller.task_controller.current()?.is_some_and(|task| {
                matches!(task.workflow, crate::task::TaskWorkflow::Goal { goal }
                    if goal.state == crate::goal::GoalState::Stalled)
            }) && controller.coordinator.execution.available_permits() > 0
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await;
    resumed.with_context(|| {
        format!(
            "recovered continuation did not settle: input={:?}, task={:?}",
            controller.coordinator.submissions.snapshot(),
            controller
                .task_controller
                .current()
                .map(|task| task.map(|task| (task.status(), task.turn)))
        )
    })??;
    let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let exchanges = store.list_exchange(&session_id)?;
    assert_eq!(exchanges.len(), 2);
    assert!(matches!(
        exchanges[1].origin,
        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
    ));
    assert_eq!(exchanges[1].prompt, pending.instruction);
    recovered_service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

#[tokio::test]
async fn running_goal_promotion_steers_without_cancelling() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let control = controller.backend.mock_response_control().unwrap();
    let held = control
        .dispatch(
            &session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = held["id"].as_str().unwrap().to_owned();
    let (sink, mut output) = forge_protocol::outbound::channel();
    let accepted_session = Arc::new(AtomicBool::new(false));
    let observed_session = accepted_session.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "session_changed"
                && event["payload"]["active_task"]["entry"]["delivery"] == "accepted"
            {
                observed_session.store(true, Ordering::Release);
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "original".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage {
                id: "original-input".into(),
                revision: 0,
                text: "Inspect startup".into(),
                purpose: Default::default(),
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
                delivery: Default::default(),
                route: None,
                reservation: None,
                submission_id: None,
                error: None,
            }),
        },
    })?;
    tokio::time::timeout(
        Duration::from_secs(5),
        control.dispatch(
            &session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    let store = SqliteStore::open(PathBuf::from(&registry.initialize.data_root).as_path())?;
    let original = store
        .latest_exchange(&session_id)?
        .context("original exchange missing")?;
    let request = Request {
        id: 2,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"live-goal","action":"goal","text":"Finish startup work"}),
    };
    service.prepare_task(Some(&session_id), &request).await?;
    tokio::time::timeout(
        Duration::from_secs(1),
        service.dispatch(Some(session_id.clone()), request, &sink),
    )
    .await??;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .task_controller
                .current()?
                .is_some_and(|task| task.entry_accepted())
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("goal entry was not acknowledged during the active turn")??;
    tokio::time::timeout(Duration::from_secs(2), async {
        while !accepted_session.load(Ordering::Acquire) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("acknowledged task state was not published to the client")?;
    assert_eq!(controller.coordinator.execution.available_permits(), 0);
    assert!(!*controller.cancellation.subscribe().borrow());
    let task = controller.task_controller.current()?.unwrap();
    controller.task_controller.admit_goal_control(true)?;
    let entry = task.entry.clone().unwrap();
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if store
                .load_session_event(
                    &session_id,
                    &format!("{session_id}:task-start:{}", entry.id.as_str()),
                )?
                .is_some()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("acknowledged task entry was not published")??;
    let exchanges = store.list_exchange(&session_id)?;
    assert_eq!(exchanges.len(), 1);
    assert_eq!(exchanges[0].id, original.id);
    assert_eq!(
        exchanges[0].state(),
        crate::exchange::ExchangeState::Running
    );
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 4,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"pause-live-goal","action":"pause"}),
            },
            &sink,
        )
        .await?;
    controller.coordinator.submissions.apply(InputOperation {
        id: "failed-old-entry".into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage {
                id: "failed-old-entry".into(),
                revision: 0,
                text: "Obsolete entry".into(),
                purpose: crate::input::InputPurpose::TaskEntry {
                    revision: task.revision.clone(),
                    id: crate::task::TaskInputId::fresh(),
                    start: crate::exchange::TaskStart {
                        kind: task.kind(),
                        title: task.title().into(),
                    },
                },
                attachments: Vec::new(),
                config: None,
                intent: InputIntent::Auto,
                delivery: Default::default(),
                route: None,
                reservation: None,
                submission_id: None,
                error: None,
            }),
        },
    })?;
    controller.coordinator.submissions.delivery(
        "failed-old-entry",
        Some(crate::input::InputDelivery::Failed),
        None,
        Some("old delivery failure".into()),
    )?;
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 5,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"resume-live-goal","action":"resume"}),
            },
            &sink,
        )
        .await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if controller
                .task_controller
                .current()?
                .is_some_and(|current| {
                    current.revision != task.revision && current.entry_accepted()
                })
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    })
    .await
    .context("resume was blocked by a stale failed entry")??;
    assert!(
        !controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .iter()
            .any(|message| message.id == "failed-old-entry")
    );
    service
        .dispatch(
            Some(session_id.clone()),
            Request {
                id: 3,
                method: "session.command.apply".into(),
                params: json!({"operation_id":"clear-live-goal","action":"clear"}),
            },
            &sink,
        )
        .await?;
    assert!(!*controller.cancellation.subscribe().borrow());
    control
        .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
        .await?;
    tokio::time::timeout(Duration::from_secs(5), async {
        while controller.coordinator.execution.available_permits() == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert!(controller.task_controller.current()?.is_none());
    assert_eq!(store.list_exchange(&session_id)?.len(), 1);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader)
        .await
        .context("shutdown retained a session subscription sender")???;
    Ok(())
}

async fn reviewed_execution_fixture(
    service: &HarnessService,
    controller: &Arc<SessionController>,
    session_id: &str,
    sink: &MessageSender,
) -> Result<Value> {
    service.dispatch(Some(session_id.to_owned()), Request {
        id: 31, method: "session.command.apply".into(),
        params: json!({"operation_id":"fixture-plan","action":"plan","text":"Inspect the project"}),
    }, sink).await?;
    let plan = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            if let Some(task) = controller.task_controller.current()?
                && let crate::task::TaskWorkflow::Plan { plan } = task.workflow
                && plan.artifact.state == crate::plan::PlanState::AwaitingReview
                && controller.coordinator.execution.available_permits() > 0
            {
                break Ok::<_, anyhow::Error>(plan.artifact);
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("fixture plan did not reach review")??;
    Ok(
        json!({"operation_id":"fixture-execute","action":"execute","plan_id":plan.id,"digest":plan.review_digest}),
    )
}

#[tokio::test]
async fn execute_promotion_queues_without_cancelling_and_clear_rejects_receipt() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    for acceptance in ["transition", "alias", "review"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let mut params =
            reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 32,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"fixture-pause-plan","action":"pause"}),
                },
                &sink,
            )
            .await?;
        let control = controller.backend.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        controller
            .input_request(
                Request {
                    id: 33,
                    method: "session.input.apply".into(),
                    params: json!({
                        "id":"held-chat", "action":{"kind":"submit","message":{"id":"held-chat",
                        "text":"Inspect startup", "intent":"auto", "attachments":[], "config":null,
                        "reservation":null,"submission_id":null,"error":null}}
                    }),
                },
                RuntimeMethod::SessionInputApply,
                &sink,
            )
            .await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let method = if acceptance == "alias" {
            params.as_object_mut().unwrap().remove("action");
            "session.plan.accept"
        } else if acceptance == "review" {
            let mut begin = params.clone();
            begin["operation_id"] = json!("begin-held-review");
            begin.as_object_mut().unwrap().remove("action");
            tokio::time::timeout(
                Duration::from_secs(1),
                service.dispatch(
                    Some(session_id.clone()),
                    Request {
                        id: 34,
                        method: "session.plan.acceptance.begin".into(),
                        params: begin,
                    },
                    &sink,
                ),
            )
            .await??;
            let task = controller.task_controller.current()?.unwrap();
            let mut store = SqliteStore::open(&controller.data_root)?;
            controller
                .task_controller
                .update(&mut store, &task.permit(), 10, |task| {
                    let crate::task::TaskWorkflow::Plan { plan } = &mut task.workflow else {
                        unreachable!()
                    };
                    let acceptance = plan
                        .artifact
                        .acceptance
                        .as_mut()
                        .context("review acceptance missing")?;
                    acceptance
                        .elicitation
                        .answer
                        .push(crate::plan::PlanQuestionAnswer {
                            source: crate::session::ResolutionSource::User,
                            question_id: "acceptance-execution-mode".into(),
                            response: crate::plan::PlanQuestionResponse::Selected {
                                option: "ask".into(),
                                feedback: None,
                            },
                        });
                    Ok(())
                })?;
            let before = serde_json::to_value(controller.task_controller.current()?.unwrap())?;
            for (index, (plan_id, digest)) in [
                (
                    "replaced-plan".to_owned(),
                    params["digest"].as_str().unwrap().to_owned(),
                ),
                (
                    params["plan_id"].as_str().unwrap().to_owned(),
                    "stale-digest".to_owned(),
                ),
            ]
            .into_iter()
            .enumerate()
            {
                let error = tokio::time::timeout(Duration::from_secs(1), service.dispatch(Some(session_id.clone()),
                    Request { id:50 + index as u64, method:"session.command.apply".into(), params:json!({
                        "operation_id":format!("stale-acceptance-{index}"), "action":"complete_acceptance",
                        "plan_id":plan_id, "digest":digest,
                    }) }, &sink)).await?.expect_err("stale acceptance must fail");
                assert!(
                    error
                        .to_string()
                        .contains("plan acceptance changed before completion"),
                    "{error:#}"
                );
                assert_eq!(
                    serde_json::to_value(controller.task_controller.current()?.unwrap())?,
                    before
                );
                assert!(!*controller.cancellation.subscribe().borrow());
                assert_eq!(controller.coordinator.execution.available_permits(), 0);
            }
            params = json!({"operation_id":"complete-held-review"});
            "session.question.continue"
        } else {
            "session.command.apply"
        };
        let request = Request {
            id: 35,
            method: method.into(),
            params,
        };
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request.clone(), &sink),
        )
        .await??;
        let task = controller.task_controller.current()?.unwrap();
        assert_eq!(task.kind(), crate::task::TaskKind::Execute);
        assert!(!task.entry_accepted());
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(Some(session_id.clone()), request, &sink),
        )
        .await??;
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?.unwrap())?,
            serde_json::to_value(&task)?,
            "repeated acceptance mutated execution"
        );
        assert!(!*controller.cancellation.subscribe().borrow());
        let entry = task.entry.as_ref().unwrap();
        let mut store = SqliteStore::open(&controller.data_root)?;
        let exchange_count = store.list_exchange(&session_id)?.len();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 35,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"clear-pending-execution","action":"clear"}),
                },
                &sink,
            )
            .await?;
        assert!(!controller.task_controller.acknowledge_entry(
            &mut store,
            &task.revision,
            &entry.id,
            42
        )?);
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert!(controller.task_controller.current()?.is_none());
        assert_eq!(store.list_exchange(&session_id)?.len(), exchange_count);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn saved_plan_actions_preserve_held_provider_and_reject_foreign_artifacts() -> Result<()> {
    use crate::backend::mock_response::MockResponseCommand;
    for action in ["attach_plan", "fork"] {
        let fixture = tempfile::tempdir()?;
        crate::checks::configure_fixture(fixture.path());
        let service = service();
        service
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let reviewed =
            reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
        let plan_id = reviewed["plan_id"].as_str().unwrap().to_owned();
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 41,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"pause-saved-source","action":"pause"}),
                },
                &sink,
            )
            .await?;
        let control = controller.backend.mock_response_control().unwrap();
        let held = control
            .dispatch(
                &session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = held["id"].as_str().unwrap().to_owned();
        controller.input_request(Request {
            id: 42, method: "session.input.apply".into(),
            params: json!({"id":"held-plan-action-chat","action":{"kind":"submit","message":{
                "id":"held-plan-action-chat","text":"Inspect current workspace","intent":"auto",
                "attachments":[],"config":null,"reservation":null,"submission_id":null,"error":null}}}),
        }, RuntimeMethod::SessionInputApply, &sink).await?;
        tokio::time::timeout(
            Duration::from_secs(5),
            control.dispatch(
                &session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            ),
        )
        .await??;
        let mut store = SqliteStore::open(&controller.data_root)?;
        let held_exchange = store.latest_exchange(&session_id)?.unwrap();
        let count = store.list_exchange(&session_id)?.len();
        let before = controller.task_controller.current()?.unwrap();
        let child = prepare_new_session(
            &controller.data_root,
            &registry.initialize.client_id,
            "mock",
            false,
            &session_id,
            &json!({"name":"foreign plan owner"}),
            43,
        )?;
        let mut foreign_plan = store.load_plan(&plan_id)?.unwrap();
        foreign_plan.id = uuid::Uuid::new_v4().to_string();
        foreign_plan.session_id = child.id;
        store.save_plan(&foreign_plan)?;
        let failure = tokio::time::timeout(Duration::from_secs(1), service.dispatch(Some(session_id.clone()),
            Request { id: 43, method: "session.command.apply".into(), params: json!({
                "operation_id":format!("foreign-{action}"),"action":action,"plan_id":foreign_plan.id,
            }) }, &sink)).await?.expect_err("foreign plan selection must fail");
        assert!(failure.to_string().contains("another"), "{failure:#}");
        assert_eq!(
            serde_json::to_value(controller.task_controller.current()?.unwrap())?,
            serde_json::to_value(&before)?
        );
        tokio::time::timeout(
            Duration::from_secs(1),
            service.dispatch(
                Some(session_id.clone()),
                Request {
                    id: 44,
                    method: "session.command.apply".into(),
                    params: json!({
                        "operation_id":format!("saved-{action}"),"action":action,"plan_id":plan_id,
                    }),
                },
                &sink,
            ),
        )
        .await??;
        let selected = controller.task_controller.current()?.unwrap();
        let crate::task::TaskWorkflow::Plan { plan } = &selected.workflow else {
            panic!("saved plan action did not select a Plan task")
        };
        if action == "attach_plan" {
            assert_eq!(plan.artifact.id, plan_id);
            assert_eq!(plan.artifact.state, crate::plan::PlanState::AwaitingReview);
            assert!(
                selected
                    .entry
                    .as_ref()
                    .is_none_or(|entry| entry.delivery != crate::task::TaskInputDelivery::Pending)
            );
        } else {
            assert_ne!(plan.artifact.id, plan_id);
            assert_eq!(plan.artifact.state, crate::plan::PlanState::Generating);
            assert_eq!(
                selected.entry.as_ref().unwrap().delivery,
                crate::task::TaskInputDelivery::Pending
            );
        }
        assert!(!*controller.cancellation.subscribe().borrow());
        assert_eq!(controller.coordinator.execution.available_permits(), 0);
        assert_eq!(store.list_exchange(&session_id)?.len(), count);
        assert_eq!(
            store.latest_exchange(&session_id)?.unwrap().id,
            held_exchange.id
        );
        assert_eq!(
            store.latest_exchange(&session_id)?.unwrap().state(),
            crate::exchange::ExchangeState::Running
        );
        service
            .dispatch(
                Some(session_id.clone()),
                Request {
                    id: 45,
                    method: "session.command.apply".into(),
                    params: json!({"operation_id":"clear-saved-action","action":"clear"}),
                },
                &sink,
            )
            .await?;
        control
            .dispatch(&session_id, MockResponseCommand::Release { id: hold_id })
            .await?;
        tokio::time::timeout(Duration::from_secs(5), async {
            while controller.coordinator.execution.available_permits() == 0 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert_eq!(store.list_exchange(&session_id)?.len(), count);
        service.shutdown(Duration::from_secs(2)).await?;
        drop(sink);
        tokio::time::timeout(Duration::from_secs(2), reader).await???;
    }
    Ok(())
}

#[tokio::test]
async fn checking_continuation_returns_to_verify_without_provider_exchange() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    crate::checks::configure_fixture(fixture.path());
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let mut params = reviewed_execution_fixture(&service, &controller, &session_id, &sink).await?;
    let admission = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let plan_id = params["plan_id"].as_str().unwrap();
    let mut plan = store.load_plan(plan_id)?.unwrap();
    let mut document = controller
        .observer
        .plan_file
        .read_working_document(&session_id, plan_id)?;
    document
        .design
        .as_mut()
        .unwrap()
        .document
        .verification
        .push("Inspect the completed result".into());
    controller
        .observer
        .plan_file
        .write_working_document(&session_id, plan_id, &document)?;
    plan.model_revision += 1;
    let (_, _, digest) = controller.observer.plan_file.submit_document_revision(
        &session_id,
        plan_id,
        plan.model_revision,
        document.version,
    )?;
    plan.review_digest = Some(digest.clone());
    let planning = controller.task_controller.current()?.unwrap();
    controller
        .task_controller
        .update(&mut store, &planning.permit(), 99, |task| {
            let crate::task::TaskWorkflow::Plan { plan: current } = &mut task.workflow else {
                unreachable!()
            };
            current.artifact = plan;
            Ok(())
        })?;
    params["digest"] = json!(digest);
    let (mut task, effects) = controller.observer.prepare_execution(&params, 20, 100)?;
    std::fs::create_dir_all(fixture.path().join("src"))?;
    std::fs::write(
        fixture.path().join("src/change.rs"),
        "pub fn requested_change() {}\n",
    )?;
    task.entry.as_mut().unwrap().execution = None;
    let current = controller.task_controller.current()?.unwrap();
    controller.task_controller.change(
        &mut store,
        Some(&current.revision),
        crate::task::TaskChange::Install(Box::new(task)),
        100,
    )?;
    for plan in effects.plan {
        store.save_plan(&plan)?;
    }
    let task = controller.task_controller.current()?.unwrap();
    controller.task_controller.acknowledge_entry(
        &mut store,
        &task.revision,
        &task.entry.as_ref().unwrap().id,
        101,
    )?;
    controller
        .task_controller
        .update(&mut store, &task.permit(), 102, |task| {
            let crate::task::TaskWorkflow::Execute { execution } = &mut task.workflow else {
                unreachable!()
            };
            execution.record.phase = crate::plan::PlanPhase::Checking;
            Ok(())
        })?;
    let task = controller.task_controller.current()?.unwrap();
    let source = store.latest_exchange(&session_id)?.unwrap();
    let count = store.list_exchange(&session_id)?.len();
    controller.task_controller.reserve_checks(
        &mut store,
        &task.permit(),
        crate::task::TaskTurnId {
            exchange_ordinal: source.ordinal,
            turn_ordinal: source.turn.len(),
        },
        103,
    )?;
    let before = controller.task_controller.current()?.unwrap();
    assert_eq!(
        before
            .turn
            .as_ref()
            .unwrap()
            .continuation
            .as_ref()
            .unwrap()
            .work,
        crate::task::TaskContinuationWork::Checks
    );
    controller.broker.lock().await.run_task_checks().await?;
    let after = controller.task_controller.current()?.unwrap();
    assert!(
        matches!(&after.workflow, crate::task::TaskWorkflow::Execute { execution }
        if execution.record.phase == crate::plan::PlanPhase::Verify)
    );
    let continuation = after.turn.as_ref().unwrap().continuation.as_ref().unwrap();
    assert_eq!(
        continuation.work,
        crate::task::TaskContinuationWork::Provider
    );
    assert!(continuation.instruction.contains("Verify"));
    assert_eq!(store.list_exchange(&session_id)?.len(), count);
    controller.task_controller.change(
        &mut store,
        Some(&after.revision),
        crate::task::TaskChange::Clear,
        104,
    )?;
    drop(admission);
    service.shutdown(Duration::from_secs(2)).await?;
    drop(sink);
    tokio::time::timeout(Duration::from_secs(2), reader).await???;
    Ok(())
}

#[tokio::test]
async fn native_goal_reconciliation_tracks_acknowledged_session_state() -> Result<()> {
    use crate::session::SessionStore;
    use crate::task::{ActiveTask, TaskChange};
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    controller.observer.set_native_goal_for_test(true)?;
    let (startup_sink, _startup_output) = forge_protocol::outbound::channel();
    controller.start_provider(&startup_sink).await;
    while !controller.provider_ready().await {
        tokio::task::yield_now().await;
    }
    let mut store = SqliteStore::open(&controller.data_root)?;
    let mut session = store.load_session(&session_id)?.unwrap();
    session.backend_session_id = Some("native-thread".into());
    store.save_session(&session)?;
    let mut task = ActiveTask::goal("Original objective".into(), Default::default(), true, 1);
    task.request_entry("Work toward the original objective".into(), 1)?;
    controller
        .task_controller
        .change(&mut store, None, TaskChange::Install(Box::new(task)), 1)?;
    let task = controller.task_controller.current()?.unwrap();
    let mut applied = None;
    controller.reconcile_native_goal(&mut applied).await?;
    assert!(applied.as_ref().unwrap().objective.is_none());
    controller.task_controller.acknowledge_entry(
        &mut store,
        &task.revision,
        &task.entry.as_ref().unwrap().id,
        2,
    )?;
    controller.reconcile_native_goal(&mut applied).await?;
    assert_eq!(
        applied.as_ref().unwrap().objective.as_deref(),
        Some("Original objective")
    );
    controller
        .task_controller
        .change(&mut store, Some(&task.revision), TaskChange::Pause, 3)?;
    controller.reconcile_native_goal(&mut applied).await?;
    assert!(applied.as_ref().unwrap().objective.is_none());
    let paused = controller.task_controller.current()?.unwrap();
    controller.task_controller.change(
        &mut store,
        Some(&paused.revision),
        TaskChange::ResumeWithEntry("/goal resume".into()),
        4,
    )?;
    controller.reconcile_native_goal(&mut applied).await?;
    assert!(applied.as_ref().unwrap().objective.is_none());
    let resumed = controller.task_controller.current()?.unwrap();
    controller.task_controller.acknowledge_entry(
        &mut store,
        &resumed.revision,
        &resumed.entry.as_ref().unwrap().id,
        5,
    )?;
    controller.reconcile_native_goal(&mut applied).await?;
    assert_eq!(
        applied.as_ref().unwrap().objective.as_deref(),
        Some("Original objective")
    );
    let mut replacement =
        ActiveTask::goal("Replacement objective".into(), Default::default(), true, 6);
    replacement.request_entry("Work toward the replacement objective".into(), 6)?;
    controller.task_controller.change(
        &mut store,
        Some(&resumed.revision),
        TaskChange::Install(Box::new(replacement)),
        6,
    )?;
    controller.reconcile_native_goal(&mut applied).await?;
    assert!(applied.as_ref().unwrap().objective.is_none());
    let replacement = controller.task_controller.current()?.unwrap();
    controller.task_controller.change(
        &mut store,
        Some(&replacement.revision),
        TaskChange::Clear,
        7,
    )?;
    controller.reconcile_native_goal(&mut applied).await?;
    assert!(applied.as_ref().unwrap().objective.is_none());
    assert!(!*controller.cancellation.subscribe().borrow());
    service.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}
