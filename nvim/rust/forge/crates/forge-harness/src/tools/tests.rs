use super::checks::is_tool as is_check_tool;
use super::schema::plan_edit_input_schema;
use crate::provider::ProviderCompletion;
use serde_json::{Value, json};

#[test]
fn direct_argument_decoding_preserves_nested_error_paths() {
    #[derive(serde::Deserialize)]
    struct Arguments {
        #[serde(rename = "values")]
        _values: Vec<u64>,
    }
    let invocation = ControlToolInvocation {
        call_id: None,
        task_permit: None,
        name: "example".into(),
        arguments: json!({"values":[1,"invalid"]}),
    };
    let error = super::invocation::decode_arguments::<Arguments>(&invocation)
        .err()
        .unwrap();
    assert!(error.to_string().contains("decode example arguments"));
    let response = control_tool_failure_json(&invocation, &error, None);
    assert!(response.contains("values[1]"), "{response}");
}

#[test]
fn every_tool_has_one_family_and_valid_schema() {
    let definitions = super::ControlToolRegistry.definition_list();
    let mut names = std::collections::BTreeSet::new();
    for tool in definitions {
        assert!(names.insert(tool.name), "duplicate tool {}", tool.name);
        let owners = [
            super::agents::is_tool(tool.name),
            super::checks::is_tool(tool.name),
            super::goals::is_tool(tool.name),
            super::lsp::is_tool(tool.name),
            super::plan::is_tool(tool.name),
            super::questions::is_tool(tool.name),
            super::repository::is_tool(tool.name),
            super::session::is_tool(tool.name),
            tool.name == super::config::NAME,
        ];
        assert_eq!(
            owners.into_iter().filter(|owner| *owner).count(),
            1,
            "{}",
            tool.name
        );
        jsonschema::validator_for(&tool.input_schema).unwrap();
    }
    assert!(!names.contains("plan_checks_add"));
}

#[tokio::test]
async fn every_tool_validates_routes_and_returns_a_controlled_result() {
    use crate::provider::PromptMode;
    for tool in ControlToolRegistry.definition_list() {
        let question = json!({"header":"Choice","question":"Which behavior?","options":[]});
        let arguments = match tool.name {
            "agent_spawn" => json!({"text":"Inspect the current state"}),
            "agent_send" | "agent_followup" => json!({"target":"child","message":"Continue"}),
            "agent_interrupt" | "agent_activity" => json!({"target":"child"}),
            "agent_tool_output" => json!({"call_ref":"call"}),
            "agent_list"
            | "repository_status"
            | "repository_changed_paths"
            | "repository_diff"
            | "check_definition_list"
            | "check_list"
            | "lsp_overview"
            | "lsp_symbols"
            | "lsp_definition"
            | "lsp_context"
            | "lsp_references"
            | "lsp_diagnostics"
            | "lsp_status"
            | "harness_config"
            | "harness_session_list"
            | "harness_session_exchange_list"
            | "harness_session_log"
            | "harness_session_status" => json!({}),
            "repository_file_diff" => json!({"path_hex":"61"}),
            "plan_file_read" => json!({"plan_id":"plan","path":"src/lib.rs"}),
            "plan_file_edit" => {
                json!({"plan_id":"plan","expected_version":1,"patch":"*** Begin Patch\n*** End Patch"})
            }
            "plan_overview_read" => json!({"plan_id":"plan"}),
            "plan_overview_edit" => {
                json!({"plan_id":"plan","expected_version":1,"objective":"Inspect ownership"})
            }
            "plan_submit" => json!({"plan_id":"plan","expected_version":1}),
            "plan_phase_done" => json!({"phase":"implement","revision":1,"summary":"Done"}),
            "question_ask" => json!({"questions":[question]}),
            "question_reply" => {
                json!({"question_id":"question","revision":1,"message_id":"message","text":"Explanation"})
            }
            "question_revise" => json!({"question_id":"question","revision":1,"draft":question}),
            "question_withdraw" => {
                json!({"question_id":"question","revision":1,"reason":"Resolved"})
            }
            "goal_complete" => json!({"summary":"Done"}),
            "goal_blocked" => json!({"reason":"Missing input"}),
            "goal_status" => json!({"status":"Working"}),
            "check_definition_details" => json!({"check_id":"check"}),
            "check_definition_edit" => json!({"expected_revision":"revision","config":{}}),
            "check_run" => json!({"check":"check"}),
            "check_status" | "check_detail" | "check_cancel" => json!({"run_id":"run"}),
            "lsp_rename_symbol" => json!({"symbol":"Store","new_name":"Shelf"}),
            "lsp_rename_file" => json!({"old_path":"a.rs","new_path":"b.rs"}),
            "lsp_calls" => json!({"direction":"incoming"}),
            "lsp_paths" => json!({"from":{"symbol":"start"},"to":{"symbol":"end"}}),
            "harness_session_search" => json!({"query":"topic"}),
            "harness_session_exchange" => json!({"exchange_id":"exchange"}),
            "harness_session_tool" => json!({"exchange_id":"exchange","tool_call_id":"tool"}),
            name => panic!("add a routing scenario for {name}"),
        };
        let invocation = ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: tool.name.into(),
            arguments: arguments.clone(),
        };
        validate_arguments(&invocation).unwrap_or_else(|error| panic!("{}: {error}", tool.name));
        let mut invalid = invocation.clone();
        invalid.arguments["unexpected_field"] = json!(true);
        let error = validate_arguments(&invalid).unwrap_err();
        let response: Value =
            serde_json::from_str(&control_tool_failure_json(&invalid, &error, None)).unwrap();
        assert_eq!(response["ok"], false, "{}", tool.name);
        assert_eq!(response["code"], "invalid_arguments", "{}", tool.name);
        let mut runtime = ControlToolRuntime::new(ControlTurnContext::inactive(PromptMode::Chat));
        match runtime.invoke(invocation.clone()).await {
            Ok(result) => assert!(!result.message.is_empty(), "{}", tool.name),
            Err(error) => {
                let response: Value =
                    serde_json::from_str(&control_tool_failure_json(&invocation, &error, None))
                        .unwrap();
                assert_eq!(response["ok"], false, "{}", tool.name);
                assert_eq!(response["tool"], tool.name);
                assert!(
                    !response
                        .to_string()
                        .contains("unknown Harness control tool"),
                    "{}: {response}",
                    tool.name
                );
            }
        }
    }
}

#[tokio::test]
async fn wrong_plan_id_returns_a_typed_tool_error_and_allows_retry() {
    let mut context = ControlTurnContext::inactive(crate::provider::PromptMode::Plan);
    let mut document = crate::plan::test_fixture("active", "Plan");
    document.design = Some(crate::plan::DeclarationDesign::default());
    context.plan_state = Some(crate::plan::PlanState::Generating);
    context.plan_document = Some(document.clone());
    let mut runtime = ControlToolRuntime::new(context);
    let invocation = ControlToolInvocation {
        call_id: None,
        task_permit: None,
        name: "plan_overview_read".into(),
        arguments: json!({"plan_id":"wrong"}),
    };
    let error = runtime.invoke(invocation.clone()).await.unwrap_err();
    assert!(
        matches!(&error, ToolError::PlanIdMismatch{requested,active} if requested=="wrong" && active=="active")
    );
    let response: Value = serde_json::from_str(&control_tool_failure_json(
        &invocation,
        &error,
        Some(&document),
    ))
    .unwrap();
    assert_eq!(response["code"], "plan_id_mismatch");
    assert_eq!(response["violation"][0]["path"], "plan_id");
    assert_eq!(response["plan_id"], "active");
    assert_eq!(response["active_version"], document.version);
    runtime
        .invoke(ControlToolInvocation {
            call_id: None,
            arguments: json!({"plan_id":"active"}),
            ..invocation
        })
        .await
        .unwrap();
}

/// Accept host-owned question identities and expose waiting only as an explicit choice.
#[test]
fn question_tools_generate_ids_and_expose_waiting() {
    let arguments =
        json!({"questions":[{"header":"Layout", "question":"Which layout?", "options":[]}]});
    for wait in [None, Some(false), Some(true)] {
        let mut arguments = arguments.clone();
        if let Some(wait) = wait {
            arguments["wait"] = json!(wait);
        }
        validate_arguments(&ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "question_ask".into(),
            arguments,
        })
        .unwrap();
    }
    for extra in ["id", "operation_id"] {
        let mut invalid = arguments.clone();
        invalid[extra] = json!("model-owned");
        assert!(
            validate_arguments(&ControlToolInvocation {
                call_id: None,
                task_permit: None,
                name: "question_ask".into(),
                arguments: invalid
            })
            .is_err()
        );
    }
    let mut invalid = arguments;
    invalid["questions"][0]["id"] = json!("model-owned");
    assert!(
        validate_arguments(&ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "question_ask".into(),
            arguments: invalid
        })
        .is_err()
    );
    for tool in ControlToolRegistry
        .definition_list()
        .into_iter()
        .filter(|tool| {
            matches!(
                tool.name,
                "question_reply" | "question_revise" | "question_withdraw"
            )
        })
    {
        assert!(
            tool.input_schema["properties"]
                .get("operation_id")
                .is_none()
        );
    }
}

#[test]
fn repository_schema_preserves_complete_offsets_and_path_identities() {
    assert!(
        validate_arguments(&ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "repository_file_diff".into(),
            arguments: json!({"offset":65537,"path_hex":"61".repeat(4097)}),
        })
        .is_ok()
    );
}

#[test]
fn repository_tools_reject_caller_authority_and_only_accept_advertised_inputs() {
    for name in [
        "repository_status",
        "repository_changed_paths",
        "repository_diff",
        "repository_file_diff",
    ] {
        let mut arguments = if name == "repository_file_diff" {
            json!({ "path_hex": "612e747874" })
        } else {
            json!({})
        };
        assert!(
            validate_arguments(&ControlToolInvocation {
                call_id: None,
                task_permit: None,
                name: name.into(),
                arguments: arguments.clone()
            })
            .is_ok()
        );
        for field in [
            "workspace",
            "session_id",
            "interaction_id",
            "permission",
            "command",
        ] {
            arguments[field] = json!("forged");
            assert!(
                validate_arguments(&ControlToolInvocation {
                    call_id: None,
                    task_permit: None,
                    name: name.into(),
                    arguments: arguments.clone()
                })
                .is_err()
            );
            arguments.as_object_mut().unwrap().remove(field);
        }
    }
    assert!(
        validate_arguments(&ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "repository_file_diff".into(),
            arguments: json!({})
        })
        .is_err()
    );
    assert!(
        validate_arguments(&ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "repository_status".into(),
            arguments: json!({ "side": "staged" })
        })
        .is_err()
    );
    assert!(
        validate_arguments(&ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "repository_diff".into(),
            arguments: json!({ "side": "other" })
        })
        .is_err()
    );
}
use super::*;
#[test]
fn checks_catalog_matches_runtime_requests_and_root_ownership() {
    let requests = [
        ("check_definition_list", json!({})),
        (
            "check_definition_edit",
            json!({"expected_revision":"revision","config":{}}),
        ),
        (
            "check_run",
            json!({"check":"unit","inputs":{"package":"engine","test":"tests::example"}}),
        ),
        ("check_status", json!({"run_id":"run","wait_ms":100})),
        ("check_cancel", json!({"run_id":"run"})),
        ("check_list", json!({})),
        ("check_detail", json!({"run_id":"run"})),
        ("check_definition_details", json!({"check_id":"unit"})),
    ];
    let catalog = ControlToolRegistry.mcp_tool_list();
    for removed in [
        "checks_list",
        "checks_configure",
        "checks_run",
        "checks_status",
        "checks_cancel",
    ] {
        assert!(!catalog.iter().any(|tool| tool["name"] == removed));
    }
    for (name, arguments) in [
        ("check_run", json!({"check":"unit","wait_ms":10})),
        (
            "check_status",
            json!({"run_id":"run","wait_for_completion":true}),
        ),
        ("check_status", json!({"run_id":"run","wait_ms":60001})),
        ("check_detail", json!({"run_id":"run","stream":"unknown"})),
    ] {
        assert!(
            validate_arguments(&ControlToolInvocation {
                call_id: None,
                task_permit: None,
                name: name.into(),
                arguments,
            })
            .is_err()
        );
    }
    for (name, arguments) in requests {
        let description = catalog.iter().find(|tool| tool["name"] == name).unwrap()["description"]
            .as_str()
            .unwrap();
        let usage = description.to_ascii_lowercase();
        assert!(
            usage.contains("supply") || usage.contains("no arguments"),
            "{name} lacks request usage"
        );
        assert!(
            catalog.iter().any(|tool| {
                let related = tool["name"].as_str().unwrap();
                related != name && is_check_tool(related) && description.contains(related)
            }),
            "{name} lacks a related check-tool reference"
        );
        assert_eq!(
            catalog.iter().filter(|tool| tool["name"] == name).count(),
            1
        );
        apply_invocation(
            &ControlToolInvocation {
                call_id: None,
                task_permit: None,
                name: name.into(),
                arguments: arguments.clone(),
            },
            &mut ProviderCompletion::default(),
        )
        .unwrap();
        let mut invalid = arguments;
        invalid["workspace"] = json!("untrusted");
        assert!(
            validate_arguments(&ControlToolInvocation {
                call_id: None,
                task_permit: None,
                name: name.into(),
                arguments: invalid
            })
            .is_err()
        );
    }
    let root = crate::provider::ThreadOwner::root("session");
    let child = crate::provider::ThreadOwner::new(
        "session".into(),
        forge_provider_api::AgentId::new("child").unwrap(),
    )
    .expect("registered session and agent identities are nonempty");
    assert_eq!(
        ControlToolRegistry
            .definition_list_for_owner(&root, false, false)
            .iter()
            .filter(|tool| is_check_tool(tool.name))
            .count(),
        8
    );
    assert!(
        !ControlToolRegistry
            .definition_list_for_owner(&child, true, true)
            .iter()
            .any(|tool| is_check_tool(tool.name))
    );
    let context = ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
    assert!(
        !ControlToolRegistry
            .definition_list_for_context(Some(&context), true)
            .iter()
            .any(|tool| is_check_tool(tool.name))
    );
}

#[test]
fn plan_tools_separate_files_and_overview() {
    let definition = ControlToolRegistry.definition_list();
    for name in [
        "plan_file_read",
        "plan_file_edit",
        "plan_overview_read",
        "plan_overview_edit",
    ] {
        assert!(definition.iter().any(|tool| tool.name == name));
    }
    assert!(!definition.iter().any(|tool| matches!(
        tool.name,
        "design_patch"
            | "plan_patch"
            | "plan_read"
            | "harness_plan_edit"
            | "harness_plan_deviation"
            | "harness_plan_task_report"
    )));
    let invocation = ControlToolInvocation {
        call_id: None,
        task_permit: None,
        name: "plan_file_edit".into(),
        arguments: json!({"plan_id":"plan","expected_version":1,"patch":"*** Begin Patch\n*** Add File: lib.rs\n+pub struct Owner;\n*** End Patch"}),
    };
    let mut output = ProviderCompletion::default();
    apply_invocation(&invocation, &mut output).unwrap();
    assert_eq!(output.design_patch.len(), 1);
    let mut invalid = invocation;
    invalid.arguments["baseline"] = json!({});
    assert!(apply_invocation(&invalid, &mut ProviderCompletion::default()).is_err());
}

fn relation_violation_list(relation: Value) -> Vec<failure::ControlToolViolation> {
    let plan_schema = plan_edit_input_schema();
    let schema = json!({
        "$schema": "http://json-schema.org/draft-07/schema#",
        "$ref": "#/definitions/PlanFlowRelation",
        "definitions": plan_schema
            .get("definitions")
            .expect("generated plan schema must expose definitions")
            .clone(),
    });
    let validator = jsonschema::validator_for(&schema).expect("flow relation schema must compile");
    validator
        .iter_errors(&relation)
        .flat_map(|error| schema_violation_list(&error, &schema))
        .collect()
}

#[test]
fn accepts_each_typed_relation_string() {
    for relation in [
        "construct",
        "call",
        "read",
        "write",
        "send",
        "emit",
        "return",
    ] {
        assert!(relation_violation_list(json!(relation)).is_empty());
    }
}

#[test]
fn rejects_legacy_relation_objects() {
    assert!(!relation_violation_list(json!({ "kind": "call" })).is_empty());
    assert!(!relation_violation_list(json!("dispatch")).is_empty());
}
