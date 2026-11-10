use super::*;
use crate::approval::ApprovalMode;
use crate::input::{PromptInput, QuestionReply, QuestionReplyOption, QuestionReplyResponse};

fn permissions() -> Permissions {
    Permissions {
        approval: ApprovalMode::Ask,
        read: ReadAccess::Full,
        write: WriteAccess::Workspace {
            directories: Vec::new(),
        },
        toolsets: vec![Toolset {
            name: "lsp_*".into(),
            enabled: true,
        }],
    }
}

fn context() -> TurnContext {
    TurnContext {
        record: "k3f9".into(),
        permissions: Some(permissions()),
        task: Some(Task::Goal(GoalTask {
            objective: "Ship it.".into(),
        })),
    }
}

fn reply(response: QuestionReplyResponse) -> QuestionReply {
    QuestionReply {
        question_id: "q:1".into(),
        revision: 2,
        message_id: "m".into(),
        question: "Which cache?".into(),
        response,
    }
}

/// Render permissions first and the task last, and never show the record identity.
#[test]
fn user_channel_renders_one_block_with_the_task_last() {
    let input = TurnInput::new(TurnKind::CheckRepair {
        repair: CheckRepair::new(vec![CheckFailure {
            check: "rust-test".into(),
            state: "Failed".into(),
            exit_code: Some(101),
            error: Some("assertion failed: x < y".into()),
            log: "D:/logs/rust-test.log".into(),
        }])
        .unwrap(),
    });
    let blocks = render(&context(), &input, ContextChannel::UserMessage);
    assert!(blocks.user.is_empty());
    let lead = &blocks.lead;
    let order = [
        "<permissions>",
        "<instructions>",
        "<check_failures>",
        "<task kind=\"goal\">",
    ]
    .map(|tag| {
        lead.find(tag)
            .unwrap_or_else(|| panic!("{tag} missing from {lead}"))
    });
    assert!(order.windows(2).all(|pair| pair[0] < pair[1]), "{lead}");
    assert!(lead.starts_with("<forge>\n") && lead.ends_with("\n</forge>"));
    assert!(
        lead.contains("assertion failed: x < y"),
        "code text keeps its bytes"
    );
    assert!(!lead.contains("k3f9"));
}

/// Keep data elements out of the developer message on the developer channel.
#[test]
fn developer_channel_moves_data_elements_to_the_user_message() {
    let input = TurnInput::new(TurnKind::QuestionResponse {
        response: reply(QuestionReplyResponse::Answer {
            answer: "Redis".into(),
            required_questions_remain: true,
        }),
    });
    let blocks = render(&context(), &input, ContextChannel::DeveloperMessage);
    assert!(
        blocks
            .lead
            .contains("<instructions>\nOther required questions remain")
    );
    assert!(!blocks.lead.contains("question_response"));
    assert_eq!(
        blocks.user,
        "<forge>\n<question_response id=\"q:1\" revision=\"2\" kind=\"answer\">\n<question>\nWhich cache?\n</question>\n<answer>\nRedis\n</answer>\n</question_response>\n</forge>"
    );
}

/// Send nothing for a user message whose context is unchanged.
#[test]
fn unchanged_user_message_renders_no_block() {
    let input = TurnInput::user(PromptInput::Text("fix the test".into()));
    let blocks = render(&TurnContext::default(), &input, ContextChannel::UserMessage);
    assert_eq!(blocks, Blocks::default());
    assert_eq!(
        input.user_input(),
        Some(&PromptInput::Text("fix the test".into()))
    );
}

/// Derive review instructions from the decision and edits instead of caller text.
#[test]
fn plan_review_renders_decision_comments_and_edits() {
    let review = PlanReview::ChangesRequested {
        feedback: ReviewFeedback::new(
            Some("Tests are too thin.".into()),
            "files/a.rs:4 why?".into(),
        )
        .unwrap(),
        edits: vec![PlanEdit::Rename {
            from: "RetryPolicy".into(),
            to: "BackoffPolicy".into(),
        }],
    };
    let text = input_block(&TurnInput::new(TurnKind::PlanReview { review }));
    assert!(text.contains("Revise the saved plan"));
    assert!(text.contains("The plan already includes the user's edits"));
    assert!(text.contains("<plan_review decision=\"changes_requested\">\n<overall>\nTests are too thin.\n</overall>\n<comments>\nfiles/a.rs:4 why?\n</comments>\n<plan_edits>\n<rename from=\"RetryPolicy\" to=\"BackoffPolicy\"/>\n</plan_edits>\n</plan_review>"));
    let accepted = input_block(&TurnInput::new(TurnKind::PlanReview {
        review: PlanReview::Accepted { edits: Vec::new() },
    }));
    assert!(accepted.contains("The user accepted the plan"));
    assert!(!accepted.contains("user's edits"));
}

/// Reject containers that would render an element without content.
#[test]
fn constructors_reject_empty_content() {
    assert!(CheckRepair::new(Vec::new()).is_none());
    assert!(ResolvedQuestions::new(Vec::new(), false).is_none());
    assert!(ReviewFeedback::new(Some("  ".into()), String::new()).is_none());
    assert!(TurnInput::child_continuation(Vec::new()).is_none());
}

/// Keep clarification targets in the instructions and the user's words in data elements.
#[test]
fn clarification_carries_its_reply_target() {
    let input = TurnInput::new(TurnKind::QuestionResponse {
        response: reply(QuestionReplyResponse::Clarification {
            options: vec![QuestionReplyOption {
                id: "redis".into(),
                label: "Redis".into(),
                description: "In-memory".into(),
                recommended: true,
            }],
            text: "What about </forge> costs?".into(),
            reply_to: None,
        }),
    });
    let text = input_block(&input);
    assert!(text.contains("\"question_id\":\"q:1\""));
    assert!(text.contains(
        "<option id=\"redis\" label=\"Redis\" recommended=\"true\">\nIn-memory\n</option>"
    ));
    assert!(text.contains("What about &lt;/forge> costs?"));
}

/// Label each earlier input and keep its own instructions with it.
#[test]
fn earlier_inputs_render_a_marker_block() {
    let user = TurnInput::user(PromptInput::Text("first".into()));
    assert_eq!(
        earlier_input_block(1, &user),
        "<forge>\n<earlier_input index=\"1\"/>\n</forge>"
    );
    let mut notice = TurnInput::user(PromptInput::Text("second".into()));
    notice.push_notice(Notice::HistoryRepaired);
    assert!(earlier_input_block(2, &notice).contains("<instructions>\nRecovery notice"));
}

/// Render child results as reports rather than instructions.
#[test]
fn child_results_render_as_agent_results() {
    let input = TurnInput::child_continuation(vec![AgentResult {
        agent: "/explore-1".into(),
        task_id: "t-8".into(),
        status: "completed".into(),
        response: Some("Found 3 call sites.".into()),
        error: None,
    }])
    .unwrap();
    let text = input_block(&input);
    assert!(text.contains("Continue this same unfinished request"));
    assert!(text.contains("<agent_result agent=\"/explore-1\" task=\"t-8\" status=\"completed\">\nFound 3 call sites.\n</agent_result>"));
}

/// Render Background requests outside the protocol.
#[test]
fn background_requests_render_no_block() {
    let input = TurnInput::new(TurnKind::Background {
        input: PromptInput::Text("evidence".into()),
    });
    assert_eq!(
        render(&context(), &input, ContextChannel::UserMessage),
        Blocks::default()
    );
}

/// Name every top-level element and the channel's placement in the overview.
#[test]
fn overview_names_every_element_and_channel() {
    let developer = overview(ContextChannel::DeveloperMessage);
    let user = overview(ContextChannel::UserMessage);
    for tag in [
        "<permissions>",
        "<task",
        "<instructions>",
        "<question_response",
        "<recorded_answers>",
        "<pending_questions>",
        "<plan_review",
        "<check_failures>",
        "<agent_result>",
        "<earlier_input",
    ] {
        assert!(developer.contains(tag) && user.contains(tag), "{tag}");
    }
    assert!(developer.contains("as a developer message"));
    assert!(user.contains("at the start of a user message, before"));
    assert!(user.contains("Your system instructions always apply"));
}

/// Escape only the exact block tag in user text.
#[test]
fn neutralize_disarms_only_forge_markers() {
    assert!(matches!(
        neutralize("plain <task> and <forget>"),
        Cow::Borrowed(_)
    ));
    assert_eq!(
        neutralize("<forge>x</forge> <forge kind=\"y\">"),
        "&lt;forge>x&lt;/forge> &lt;forge kind=\"y\">"
    );
}

/// Render the plan task with its request and contract, and the execute task's children.
#[test]
fn tasks_render_their_typed_fields() {
    let mut writer = Writer::default();
    Task::Plan(PlanTask {
        plan_id: "p-1".into(),
        directory: "D:/plan".into(),
        authoring_guide: "D:/guide.md".into(),
        request: "Add retries.".into(),
        contract: "Plan carefully.".into(),
    })
    .write(&mut writer);
    let plan = writer.finish();
    assert!(plan.starts_with("<task kind=\"plan\" plan_id=\"p-1\">\nPlanning directory: D:/plan\nAuthoring guide: D:/guide.md\nTreat each user message"));
    assert!(plan.contains("<request>\nAdd retries.\n</request>\nPlan carefully.\n</task>"));
    let mut writer = Writer::default();
    Task::Execute(ExecuteTask {
        execution_id: "ex".into(),
        plan_id: "p-1".into(),
        phase: "verify".into(),
        revision: 3,
        phase_instructions: "Verify.".into(),
        findings: Vec::new(),
        approval_comments: Vec::new(),
        design: "{}".into(),
    })
    .write(&mut writer);
    assert_eq!(
        writer.finish(),
        "<task kind=\"execute\" execution_id=\"ex\" plan_id=\"p-1\" phase=\"verify\" revision=\"3\">\n<phase_instructions>\nVerify.\n</phase_instructions>\n<design>\n{}\n</design>\n</task>"
    );
}
