//! Frames the Claude Code CLI writes to stdout in `--output-format stream-json` mode.
//!
//! Decoding has two stages. The first reads only the `type` (and, for `system` and `result`, the
//! `subtype`) tag. The second decodes the kinds this adapter interprets into typed structs and
//! fails on malformed payloads, keeps kinds the CLI schema names but the adapter ignores as
//! `Recognized` with their raw payload, and reports only kinds the schema does not name as
//! `Unknown`. A CLI upgrade that adds frames therefore never breaks decoding.

use super::content::{AssistantMessage, Usage, UserMessage};
use super::control::{ControlResponse, InboundControlRequest};
use serde::Deserialize;
use serde_json::{Map, Value};

/// One decoded stdout frame.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum StdoutFrame {
    Assistant(AssistantFrame),
    User(UserFrame),
    Result(ResultFrame),
    System(SystemFrame),
    StreamEvent(StreamEventFrame),
    ControlRequest(InboundControlRequest),
    ControlResponse(ControlResponse),
    ControlCancelRequest {
        request_id: String,
    },
    CommandLifecycle(CommandLifecycleFrame),
    /// A frame kind the CLI schema names but this adapter does not interpret.
    Recognized {
        kind: String,
        payload: Value,
    },
    /// A frame kind the CLI schema does not name.
    Unknown {
        kind: String,
        payload: Value,
    },
}

/// Frame kinds the CLI schema names that carry nothing Harness normalizes.
const RECOGNIZED_KINDS: &[&str] = &[
    "keep_alive",
    "rate_limit_event",
    "tool_progress",
    "tool_use_summary",
    "auth_status",
    "prompt_suggestion",
    "streamlined_text",
    "streamlined_tool_use_summary",
    "transcript_mirror",
    "active_goal",
    "autocompact_state",
    "conversation_reset",
    "api_error",
    "api_metrics",
    "attachment",
    "compact_start",
    "compact_progress",
    "compact_end",
    "hint_clears",
    "hooks_start",
    "open_message_selector",
    "os_notification",
    "refusal_continuation",
    "response_length",
    "session_notice",
    "set_expanded_view",
    "set_in_progress_tool_use_ids",
    "stream_mode",
    "thinking_progress",
    "thinking_signature",
    "tombstone",
    "turn_duration",
];

/// Fail decoding of one line without exposing its content.
#[derive(Debug, thiserror::Error)]
pub(crate) enum FrameDecodeError {
    #[error("stdout line is not JSON")]
    NotJson,
    #[error("stdout frame has no type")]
    MissingType,
    #[error("stdout frame `{kind}` has an invalid payload")]
    InvalidPayload { kind: String },
}

impl StdoutFrame {
    /// Decode one complete stdout line.
    pub(crate) fn decode(line: &str) -> Result<Self, FrameDecodeError> {
        let value: Value = serde_json::from_str(line).map_err(|_| FrameDecodeError::NotJson)?;
        Self::from_value(value)
    }

    /// Decode one parsed stdout frame.
    pub(crate) fn from_value(value: Value) -> Result<Self, FrameDecodeError> {
        let kind = value
            .get("type")
            .and_then(Value::as_str)
            .ok_or(FrameDecodeError::MissingType)?
            .to_owned();
        let invalid = |_| FrameDecodeError::InvalidPayload { kind: kind.clone() };
        Ok(match kind.as_str() {
            "assistant" => Self::Assistant(serde_json::from_value(value).map_err(invalid)?),
            "user" => Self::User(serde_json::from_value(value).map_err(invalid)?),
            "result" => Self::Result(serde_json::from_value(value).map_err(invalid)?),
            "system" => Self::System(SystemFrame::from_value(value).map_err(invalid)?),
            "stream_event" => Self::StreamEvent(serde_json::from_value(value).map_err(invalid)?),
            "control_request" => {
                Self::ControlRequest(InboundControlRequest::from_value(value).map_err(invalid)?)
            }
            "control_response" => {
                Self::ControlResponse(ControlResponse::from_value(value).map_err(invalid)?)
            }
            "control_cancel_request" => Self::ControlCancelRequest {
                request_id: value
                    .get("request_id")
                    .and_then(Value::as_str)
                    .ok_or_else(|| FrameDecodeError::InvalidPayload { kind: kind.clone() })?
                    .to_owned(),
            },
            "command_lifecycle" => {
                Self::CommandLifecycle(serde_json::from_value(value).map_err(invalid)?)
            }
            known if RECOGNIZED_KINDS.contains(&known) || known.starts_with("ui_") => {
                Self::Recognized {
                    kind,
                    payload: value,
                }
            }
            _ => Self::Unknown {
                kind,
                payload: value,
            },
        })
    }
}

/// One completed assistant content block, which shares its API message id with its siblings.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct AssistantFrame {
    pub message: AssistantMessage,
    #[serde(default)]
    pub parent_tool_use_id: Option<String>,
    #[serde(default)]
    pub uuid: Option<String>,
    #[serde(default)]
    pub error: Option<String>,
}

/// A user message the CLI authored or echoed.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct UserFrame {
    pub message: UserMessage,
    #[serde(default)]
    pub parent_tool_use_id: Option<String>,
    #[serde(default)]
    pub uuid: Option<String>,
    #[serde(default, rename = "isReplay")]
    pub is_replay: bool,
    #[serde(default, rename = "isSynthetic")]
    pub is_synthetic: bool,
    #[serde(default)]
    pub tool_use_result: Option<Value>,
}

/// The end of one CLI turn, which begins when a queued user input starts.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct ResultFrame {
    pub subtype: String,
    #[serde(default)]
    pub is_error: bool,
    #[serde(default)]
    pub result: Option<String>,
    #[serde(default)]
    pub errors: Vec<String>,
    #[serde(default)]
    pub stop_reason: Option<String>,
    #[serde(default)]
    pub terminal_reason: Option<String>,
    #[serde(default)]
    pub num_turns: Option<u64>,
    #[serde(default)]
    pub duration_ms: Option<u64>,
    #[serde(default)]
    pub usage: Option<Usage>,
    #[serde(default)]
    pub local_command: Option<String>,
    #[serde(default)]
    pub api_error_status: Option<u64>,
    #[serde(default)]
    pub origin: Option<Value>,
    pub uuid: String,
    #[serde(default)]
    pub session_id: Option<String>,
}

impl ResultFrame {
    /// Report whether the turn ended because the client interrupted it.
    pub(crate) fn interrupted(&self) -> bool {
        self.subtype == "error_during_execution"
            && matches!(
                self.terminal_reason.as_deref(),
                Some("aborted_streaming" | "aborted_tools")
            )
    }

    /// Report whether the CLI started this turn on its own instead of for client input.
    pub(crate) fn unsolicited(&self) -> bool {
        self.origin
            .as_ref()
            .and_then(|origin| origin.get("kind"))
            .and_then(Value::as_str)
            .is_some_and(|kind| kind != "human")
    }
}

/// One raw Messages API stream event, forwarded with `--include-partial-messages`.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct StreamEventFrame {
    pub event: StreamEvent,
    #[serde(default)]
    pub parent_tool_use_id: Option<String>,
}

/// The stream events this adapter interprets; other events decode as `Other`.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum StreamEvent {
    MessageStart {
        message_id: String,
    },
    ContentBlockStart {
        index: u64,
        block_kind: String,
    },
    ContentBlockDelta {
        index: u64,
        delta: StreamDelta,
    },
    ContentBlockStop {
        index: u64,
    },
    MessageDelta {
        stop_reason: Option<String>,
        usage: Option<Usage>,
    },
    MessageStop,
    Other {
        kind: String,
    },
}

/// One content delta of a streamed block.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum StreamDelta {
    Text(String),
    Thinking(String),
    /// Signature, partial tool input, citation, or another delta with no displayed text.
    Other,
}

impl<'de> Deserialize<'de> for StreamEvent {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::Error;
        let value = Value::deserialize(deserializer)?;
        let kind = value
            .get("type")
            .and_then(Value::as_str)
            .ok_or_else(|| D::Error::custom("stream event has no type"))?;
        let index = || {
            value
                .get("index")
                .and_then(Value::as_u64)
                .ok_or_else(|| D::Error::custom("stream event has no block index"))
        };
        Ok(match kind {
            "message_start" => Self::MessageStart {
                message_id: value
                    .pointer("/message/id")
                    .and_then(Value::as_str)
                    .ok_or_else(|| D::Error::custom("message_start has no message id"))?
                    .to_owned(),
            },
            "content_block_start" => Self::ContentBlockStart {
                index: index()?,
                block_kind: value
                    .pointer("/content_block/type")
                    .and_then(Value::as_str)
                    .unwrap_or_default()
                    .to_owned(),
            },
            "content_block_delta" => {
                let delta = &value["delta"];
                Self::ContentBlockDelta {
                    index: index()?,
                    delta: match delta["type"].as_str() {
                        Some("text_delta") => {
                            StreamDelta::Text(delta["text"].as_str().unwrap_or_default().into())
                        }
                        Some("thinking_delta") => StreamDelta::Thinking(
                            delta["thinking"].as_str().unwrap_or_default().into(),
                        ),
                        _ => StreamDelta::Other,
                    },
                }
            }
            "content_block_stop" => Self::ContentBlockStop { index: index()? },
            "message_delta" => Self::MessageDelta {
                stop_reason: value
                    .pointer("/delta/stop_reason")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                usage: value
                    .get("usage")
                    .map(|usage| serde_json::from_value(usage.clone()))
                    .transpose()
                    .map_err(D::Error::custom)?,
            },
            "message_stop" => Self::MessageStop,
            other => Self::Other {
                kind: other.to_owned(),
            },
        })
    }
}

/// Delivery progress of one client user frame stamped with a `uuid`.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct CommandLifecycleFrame {
    pub command_uuid: String,
    pub state: CommandState,
}

/// Lifecycle state of a client command.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CommandState {
    /// The CLI received the command and queued it.
    Queued,
    /// The command entered the conversation, either starting a turn or joining the running one.
    Started,
    /// The turn that consumed the command finished.
    Completed,
    /// An interrupt discarded the command, either while it was still queued or with the turn that
    /// consumed it. The CLI prints a `result` only in the second case.
    Cancelled,
    #[serde(other)]
    Other,
}

/// A `system` frame, discriminated by `subtype`.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SystemFrame {
    Init(InitFrame),
    Status(StatusFrame),
    CompactBoundary(CompactBoundaryFrame),
    ThinkingTokens,
    TaskStarted(TaskFrame),
    TaskNotification(TaskFrame),
    SessionStateChanged {
        state: String,
    },
    ApiRetry(ApiRetryFrame),
    Informational(InformationalFrame),
    Notification(NotificationFrame),
    /// A subtype the CLI schema names that carries nothing Harness normalizes.
    Recognized {
        subtype: String,
        payload: Value,
    },
    /// A subtype the CLI schema does not name.
    Unknown {
        subtype: String,
        payload: Value,
    },
}

/// System subtypes the CLI schema names that carry nothing Harness normalizes.
const RECOGNIZED_SYSTEM_SUBTYPES: &[&str] = &[
    "control_request_progress",
    "model_refusal_fallback",
    "model_refusal_no_fallback",
    "local_command_output",
    "hook_started",
    "hook_progress",
    "hook_response",
    "plugin_install",
    "task_progress",
    "task_updated",
    "background_tasks_changed",
    "worker_shutting_down",
    "commands_changed",
    "files_persisted",
    "memory_recall",
    "elicitation_complete",
    "permission_denied",
    "mirror_error",
    "post_turn_summary",
    "task_summary",
    "session_metadata",
    "agents_killed",
    "away_summary",
    "cloud_session_delta",
    "code_change_published",
    "dev_intent",
    "feedback_draft_queued",
    "file_attachments_missing",
    "file_snapshot",
    "inherited_asks_settled",
    "instruction_size_warning",
    "memory_saved",
    "model_consent_fallback",
    "model_fallback",
    "peer_message_hold",
    "per_turn_effort_changed",
    "permission_check_status",
    "permission_retry",
    "scheduled_task_fire",
    "session_title_changed",
    "stop_hook_summary",
    "turn_handoff",
    "turn_handoff_available",
    "turn_preempted",
    "vcs_state_changed",
];

impl SystemFrame {
    fn from_value(value: Value) -> Result<Self, serde_json::Error> {
        let subtype = value
            .get("subtype")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
        Ok(match subtype.as_str() {
            "init" => Self::Init(serde_json::from_value(value)?),
            "status" => Self::Status(serde_json::from_value(value)?),
            "compact_boundary" => Self::CompactBoundary(serde_json::from_value(value)?),
            "thinking_tokens" => Self::ThinkingTokens,
            "task_started" => Self::TaskStarted(serde_json::from_value(value)?),
            "task_notification" => Self::TaskNotification(serde_json::from_value(value)?),
            "session_state_changed" => Self::SessionStateChanged {
                state: value
                    .get("state")
                    .and_then(Value::as_str)
                    .unwrap_or_default()
                    .to_owned(),
            },
            "api_retry" => Self::ApiRetry(serde_json::from_value(value)?),
            "informational" => Self::Informational(serde_json::from_value(value)?),
            "notification" => Self::Notification(serde_json::from_value(value)?),
            known if RECOGNIZED_SYSTEM_SUBTYPES.contains(&known) => Self::Recognized {
                subtype,
                payload: value,
            },
            _ => Self::Unknown {
                subtype,
                payload: value,
            },
        })
    }
}

/// Session metadata reported when a turn starts.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct InitFrame {
    pub session_id: String,
    #[serde(default)]
    pub model: Option<String>,
    #[serde(default, rename = "permissionMode")]
    pub permission_mode: Option<String>,
    #[serde(default)]
    pub claude_code_version: Option<String>,
    #[serde(default)]
    pub tools: Vec<String>,
    #[serde(default)]
    pub mcp_servers: Vec<InitMcpServer>,
    #[serde(default)]
    pub skills: Vec<Value>,
    #[serde(default)]
    pub capabilities: Vec<String>,
    #[serde(flatten)]
    pub extra: Map<String, Value>,
}

/// One MCP server's connection state at turn start.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct InitMcpServer {
    pub name: String,
    pub status: String,
}

/// Run status, including the compaction phase of `/compact`.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct StatusFrame {
    #[serde(default)]
    pub status: Option<String>,
    #[serde(default, rename = "permissionMode")]
    pub permission_mode: Option<String>,
    #[serde(default)]
    pub compact_result: Option<String>,
}

/// The boundary a completed compaction leaves in the transcript.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct CompactBoundaryFrame {
    pub compact_metadata: CompactMetadata,
    pub uuid: String,
}

/// Token accounting of one compaction.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct CompactMetadata {
    #[serde(default)]
    pub trigger: Option<String>,
    #[serde(default)]
    pub pre_tokens: Option<u64>,
    #[serde(default)]
    pub post_tokens: Option<u64>,
    #[serde(default)]
    pub duration_ms: Option<u64>,
}

/// A native task, which a foreground shell or subagent tool also reports.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct TaskFrame {
    pub task_id: String,
    #[serde(default)]
    pub tool_use_id: Option<String>,
    #[serde(default)]
    pub is_backgrounded: Option<bool>,
    #[serde(default)]
    pub status: Option<String>,
    #[serde(default)]
    pub task_type: Option<String>,
}

/// A retry the CLI schedules after an API failure.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct ApiRetryFrame {
    #[serde(default)]
    pub attempt: Option<u64>,
    #[serde(default)]
    pub max_retries: Option<u64>,
    #[serde(default)]
    pub error: Option<String>,
}

/// An informational notice attached to the conversation.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct InformationalFrame {
    #[serde(default)]
    pub content: String,
    #[serde(default)]
    pub level: Option<String>,
}

/// A user-facing notification.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct NotificationFrame {
    #[serde(default)]
    pub text: String,
    #[serde(default)]
    pub priority: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn frames_decode_by_tag_and_unnamed_kinds_stay_unknown() {
        let decode = |value: Value| StdoutFrame::from_value(value).unwrap();
        assert!(matches!(
            decode(json!({"type":"keep_alive"})),
            StdoutFrame::Recognized { .. }
        ));
        assert!(matches!(
            decode(json!({"type":"ui_banner"})),
            StdoutFrame::Recognized { .. }
        ));
        assert!(matches!(
            decode(json!({"type":"brand_new_frame"})),
            StdoutFrame::Unknown { kind, .. } if kind == "brand_new_frame"
        ));
        assert!(matches!(
            decode(json!({"type":"system","subtype":"hook_started"})),
            StdoutFrame::System(SystemFrame::Recognized { .. })
        ));
        assert!(matches!(
            decode(json!({"type":"system","subtype":"novel"})),
            StdoutFrame::System(SystemFrame::Unknown { .. })
        ));
        assert!(StdoutFrame::from_value(json!({"type":"result"})).is_err());
        assert!(StdoutFrame::decode("not json").is_err());
    }

    #[test]
    fn stream_events_decode_text_and_thinking_deltas() {
        let StdoutFrame::StreamEvent(frame) = StdoutFrame::from_value(json!({
            "type":"stream_event",
            "event":{"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"hi"}}
        }))
        .unwrap() else {
            panic!("stream event");
        };
        assert_eq!(
            frame.event,
            StreamEvent::ContentBlockDelta {
                index: 1,
                delta: StreamDelta::Text("hi".into())
            }
        );
        let StdoutFrame::StreamEvent(frame) = StdoutFrame::from_value(json!({
            "type":"stream_event",
            "event":{"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":3}}
        }))
        .unwrap() else {
            panic!("stream event");
        };
        assert!(matches!(
            frame.event,
            StreamEvent::MessageDelta { stop_reason: Some(reason), .. } if reason == "tool_use"
        ));
    }

    #[test]
    fn interrupted_results_are_distinguished_from_failures() {
        let StdoutFrame::Result(result) = StdoutFrame::from_value(json!({
            "type":"result","subtype":"error_during_execution","is_error":true,
            "terminal_reason":"aborted_streaming","uuid":"result-uuid"
        }))
        .unwrap() else {
            panic!("result");
        };
        assert!(result.interrupted());
        assert!(!result.unsolicited());
    }
}
