use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
/// Carries the timeline identity through a manually requested provider operation.
pub struct BackendCompactionRequest {
    /// Timeline operation identity shared by RPC results and native notifications.
    pub id: String,
    /// Provider session configuration and conversation identity.
    pub request: super::BackendRequest,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Distinguishes active compaction from provider-confirmed terminal outcomes.
pub enum CompactionState {
    /// The provider has begun replacing conversation context.
    Running,
    /// The provider has completed context replacement.
    Completed,
    /// The provider rejected or failed context replacement.
    Failed,
    /// Provider execution ended without confirming context replacement.
    Unconfirmed,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Describes the compaction evidence available through the provider protocol.
pub struct CompactionCapability {
    /// Publishes compaction start and completion notifications.
    pub lifecycle: bool,
    /// Compaction can continue after the assistant provider turn completes.
    pub background: bool,
    /// Returns the actual generated summary when the provider supplies one.
    pub summary: bool,
    /// Returns compaction token or message counts when available.
    pub metrics: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains reported token categories for the compaction model request.
pub struct CompactionUsage {
    /// Provider billing totals and token categories when included in the native event.
    #[serde(alias = "copilotUsage")]
    pub billing: Option<CompactionCost>,
    /// Submitted prompt tokens.
    #[serde(alias = "inputTokens")]
    pub input: Option<u64>,
    /// Generated tokens.
    #[serde(alias = "outputTokens")]
    pub output: Option<u64>,
    /// Prompt tokens served from cache.
    #[serde(alias = "cacheReadTokens")]
    pub cached_input: Option<u64>,
    /// Prompt tokens written to cache.
    pub cache_write_tokens: Option<u64>,
    /// Provider-measured compaction request duration.
    #[serde(alias = "duration")]
    pub duration_ms: Option<u64>,
    /// Model used for the compaction request.
    pub model: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains provider billing evidence in its reported AI-unit denomination.
pub struct CompactionCost {
    /// Provider-reported request cost in nano-AI units.
    pub total_nano_aiu: Option<f64>,
    /// Provider billing categories, absent when no itemized cost was reported.
    pub token_details: Option<Vec<CompactionTokenCost>>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains one billing category without deriving monetary prices.
pub struct CompactionTokenCost {
    /// Number of tokens in one billing batch.
    pub batch_size: Option<u64>,
    /// Reported cost of one batch.
    pub cost_per_batch: Option<u64>,
    /// Token count charged to the category.
    pub token_count: Option<u64>,
    /// Provider billing category name.
    pub token_type: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains reported context composition after compaction.
pub struct CompactionContext {
    /// Total retained tokens.
    pub current_tokens: Option<u64>,
    /// Model context capacity.
    pub token_limit: Option<u64>,
    /// Retained message count.
    pub messages_length: Option<u64>,
    /// Tokens belonging to conversation messages.
    pub conversation_tokens: Option<u64>,
    /// Tokens belonging to system instructions.
    pub system_tokens: Option<u64>,
    /// Tokens belonging to tool definitions.
    pub tool_definitions_tokens: Option<u64>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains provider-reported results without inventing absent counts or summary text.
pub struct CompactionResult {
    /// Compaction model reported at operation start.
    pub model: Option<String>,
    /// Provider-reported context composition at operation start.
    pub before_context: Option<CompactionContext>,
    /// User focus text supplied to provider compaction when reported.
    pub custom_instructions: Option<String>,
    /// Provider request correlation identity.
    pub request_id: Option<String>,
    /// Provider service correlation identity.
    pub service_request_id: Option<String>,
    /// Provider HTTP status accompanying a failed compaction request.
    pub status_code: Option<u16>,
    /// Generated context summary, distinct from the complete retained model context.
    pub summary_content: Option<String>,
    /// Failure details supplied by the provider.
    pub error: Option<String>,
    /// Conversation tokens before context replacement.
    pub pre_compaction_tokens: Option<u64>,
    /// Conversation tokens after context replacement.
    pub post_compaction_tokens: Option<u64>,
    /// Tokens removed by context replacement.
    pub tokens_removed: Option<u64>,
    /// Messages present before context replacement.
    pub pre_compaction_messages_length: Option<u64>,
    /// Messages removed by context replacement.
    pub messages_removed: Option<u64>,
    /// Provider checkpoint ordinal created by compaction.
    pub checkpoint_number: Option<u64>,
    /// Provider checkpoint location, retained as metadata rather than an editor target.
    pub checkpoint_path: Option<String>,
    /// Provider-reported input, output, and cached input for the compaction request.
    pub compaction_tokens_used: Option<CompactionUsage>,
    /// Provider-reported context composition after manual compaction.
    pub context_window: Option<CompactionContext>,
    /// Conversation tokens after automatic compaction.
    pub conversation_tokens: Option<u64>,
    /// System instruction tokens after automatic compaction.
    pub system_tokens: Option<u64>,
    /// Tool definition tokens after automatic compaction.
    pub tool_definitions_tokens: Option<u64>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns one compaction lifecycle at its original timeline position.
pub struct Compaction {
    /// Provider operation identity, shared by start and completion.
    pub id: String,
    /// Provider conversation whose context changed.
    pub thread_id: String,
    /// Current provider-confirmed lifecycle state.
    pub state: CompactionState,
    /// Identifies an explicitly requested operation without guessing automatic triggers.
    pub manual: bool,
    /// First observed start, absent when only completion was received.
    pub started_at_ms: Option<i64>,
    /// First observed completion.
    pub completed_at_ms: Option<i64>,
    /// Optional provider evidence accompanying completion.
    pub result: CompactionResult,
}

impl Compaction {
    /// Merge lifecycle replays without moving the row or reopening terminal history.
    pub(crate) fn observe(&mut self, mut update: Self, now_ms: i64) {
        if !matches!(
            self.state,
            CompactionState::Running | CompactionState::Unconfirmed
        ) || (self.state == CompactionState::Unconfirmed
            && update.state == CompactionState::Running)
        {
            if self.state == update.state || update.state == CompactionState::Running {
                self.result.retain_missing(update.result);
            }
            return;
        }
        update.started_at_ms = self.started_at_ms.or(update.started_at_ms);
        update.result.before_context = self
            .result
            .before_context
            .clone()
            .or(update.result.before_context);
        update.result.model = update.result.model.or_else(|| self.result.model.clone());
        update.manual |= self.manual;
        if update.state == CompactionState::Running {
            update.started_at_ms.get_or_insert(now_ms);
        } else {
            update.completed_at_ms.get_or_insert(now_ms);
        }
        *self = update;
    }

    /// Initialize timing from the first accepted notification.
    pub(crate) fn observed(mut self, now_ms: i64) -> Self {
        if self.state == CompactionState::Running {
            self.started_at_ms.get_or_insert(now_ms);
        } else {
            self.completed_at_ms.get_or_insert(now_ms);
        }
        self
    }
}

impl CompactionResult {
    /// Preserve confirmed evidence while filling fields supplied by later notifications.
    fn retain_missing(&mut self, update: Self) {
        fn merge_missing(existing: &mut serde_json::Value, update: serde_json::Value) {
            match (existing, update) {
                (serde_json::Value::Object(existing), serde_json::Value::Object(update)) => {
                    for (name, value) in update {
                        merge_missing(
                            existing.entry(name).or_insert(serde_json::Value::Null),
                            value,
                        );
                    }
                }
                (existing, update) if existing.is_null() => *existing = update,
                _ => {}
            }
        }
        let mut retained =
            serde_json::to_value(&*self).expect("compaction result is JSON serializable");
        merge_missing(
            &mut retained,
            serde_json::to_value(update).expect("compaction result is JSON serializable"),
        );
        *self = serde_json::from_value(retained)
            .expect("merged compaction result preserves field types");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compaction_late_confirmation_preserves_start_evidence_and_terminal_timing() {
        let mut start = Compaction {
            id: "operation".into(),
            thread_id: "thread".into(),
            state: CompactionState::Running,
            manual: false,
            started_at_ms: None,
            completed_at_ms: None,
            result: CompactionResult {
                model: Some("summary-model".into()),
                before_context: Some(CompactionContext {
                    conversation_tokens: Some(800),
                    ..Default::default()
                }),
                ..Default::default()
            },
        }
        .observed(100);
        let replay = start.clone();
        start.state = CompactionState::Unconfirmed;
        start.completed_at_ms = Some(200);
        start.observe(replay.clone(), 300);
        assert_eq!(start.state, CompactionState::Unconfirmed);
        let mut completed = replay;
        completed.state = CompactionState::Completed;
        completed.started_at_ms = None;
        completed.result = CompactionResult {
            summary_content: Some("Retained context".into()),
            ..Default::default()
        };
        start.observe(completed.clone(), 400);
        start.observe(completed, 500);
        assert_eq!(start.started_at_ms, Some(100));
        assert_eq!(start.completed_at_ms, Some(400));
        assert_eq!(start.result.model.as_deref(), Some("summary-model"));
        assert_eq!(
            start.result.before_context.as_ref().unwrap().conversation_tokens,
            Some(800)
        );
        let mut detail = start.clone();
        detail.result.tokens_removed = Some(0);
        detail.result.summary_content = Some("Replacement replay text".into());
        start.observe(detail, 600);
        assert_eq!(start.completed_at_ms, Some(400));
        assert_eq!(start.result.tokens_removed, Some(0));
        assert_eq!(start.result.summary_content.as_deref(), Some("Retained context"));
    }
}
