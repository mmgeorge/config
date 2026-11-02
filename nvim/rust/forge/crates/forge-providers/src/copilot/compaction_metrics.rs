use serde::{Deserialize, Serialize};
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains reported token categories for the compaction model request.
pub struct NativeCompactionUsage {
    /// Provider billing totals and token categories when included in the native event.
    #[serde(alias = "copilotUsage")]
    pub billing: Option<NativeCompactionCost>,
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
pub struct NativeCompactionCost {
    /// Provider-reported request cost in nano-AI units.
    pub total_nano_aiu: Option<f64>,
    /// Provider billing categories, absent when no itemized cost was reported.
    pub token_details: Option<Vec<NativeCompactionTokenCost>>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, rename_all = "camelCase")]
/// Retains one billing category without deriving monetary prices.
pub struct NativeCompactionTokenCost {
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
pub struct NativeCompactionContext {
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
/// Retains provider results and observed context usage without inventing absent values.
pub struct NativeCompactionResult {
    /// Context meter tokens captured when compaction starts.
    pub context_tokens_before: Option<u64>,
    /// First context meter tokens observed after confirmed completion.
    pub context_tokens_after: Option<u64>,
    /// Compaction model reported at operation start.
    pub model: Option<String>,
    /// Provider-reported context composition at operation start.
    pub before_context: Option<NativeCompactionContext>,
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
    pub compaction_tokens_used: Option<NativeCompactionUsage>,
    /// Provider-reported context composition after manual compaction.
    pub context_window: Option<NativeCompactionContext>,
    /// Conversation tokens after automatic compaction.
    pub conversation_tokens: Option<u64>,
    /// System instruction tokens after automatic compaction.
    pub system_tokens: Option<u64>,
    /// Tool definition tokens after automatic compaction.
    pub tool_definitions_tokens: Option<u64>,
}

impl From<NativeCompactionUsage> for forge_provider_api::compaction::CompactionUsage {
    fn from(native: NativeCompactionUsage) -> Self {
        let NativeCompactionUsage {
            billing,
            input,
            output,
            cached_input,
            cache_write_tokens,
            duration_ms,
            model,
        } = native;
        Self {
            billing: billing.map(Into::into),
            input,
            output,
            cached_input,
            cache_write_tokens,
            duration_ms,
            model,
        }
    }
}

impl From<NativeCompactionContext> for forge_provider_api::compaction::CompactionContext {
    fn from(native: NativeCompactionContext) -> Self {
        let NativeCompactionContext {
            current_tokens,
            token_limit,
            messages_length,
            conversation_tokens,
            system_tokens,
            tool_definitions_tokens,
        } = native;
        Self {
            current_tokens,
            token_limit,
            messages_length,
            conversation_tokens,
            system_tokens,
            tool_definitions_tokens,
        }
    }
}

impl From<NativeCompactionResult> for forge_provider_api::compaction::CompactionResult {
    fn from(native: NativeCompactionResult) -> Self {
        let NativeCompactionResult {
            context_tokens_before,
            context_tokens_after,
            model,
            before_context,
            custom_instructions,
            request_id,
            service_request_id,
            status_code,
            summary_content,
            error,
            pre_compaction_tokens,
            post_compaction_tokens,
            tokens_removed,
            pre_compaction_messages_length,
            messages_removed,
            checkpoint_number,
            checkpoint_path,
            compaction_tokens_used,
            context_window,
            conversation_tokens,
            system_tokens,
            tool_definitions_tokens,
        } = native;
        Self {
            context_tokens_before,
            context_tokens_after,
            model,
            before_context: before_context.map(Into::into),
            custom_instructions,
            request_id,
            service_request_id,
            status_code,
            summary_content,
            error,
            pre_compaction_tokens,
            post_compaction_tokens,
            tokens_removed,
            pre_compaction_messages_length,
            messages_removed,
            checkpoint_number,
            checkpoint_path,
            compaction_tokens_used: compaction_tokens_used.map(Into::into),
            context_window: context_window.map(Into::into),
            conversation_tokens,
            system_tokens,
            tool_definitions_tokens,
        }
    }
}

impl From<NativeCompactionCost> for forge_provider_api::compaction::CompactionCost {
    fn from(native: NativeCompactionCost) -> Self {
        let NativeCompactionCost {
            total_nano_aiu,
            token_details,
        } = native;
        use forge_provider_api::compaction::ReportedMeasurement;
        let mut measurements = Vec::new();
        if let Some(value) = total_nano_aiu {
            measurements.push(ReportedMeasurement {
                label: "Provider cost".into(),
                value: value.to_string(),
                unit: "nano-AI units".into(),
            });
        }
        for category in token_details.unwrap_or_default() {
            let name = category.token_type.as_deref().unwrap_or("unspecified");
            for (label, value, unit) in [
                (
                    format!("Billed {name} tokens"),
                    category.token_count,
                    "tokens",
                ),
                (
                    format!("Billing {name} batch size"),
                    category.batch_size,
                    "tokens",
                ),
                (
                    format!("Billing {name} cost per batch"),
                    category.cost_per_batch,
                    "nano-AI units",
                ),
            ] {
                if let Some(value) = value {
                    measurements.push(ReportedMeasurement {
                        label,
                        value: value.to_string(),
                        unit: unit.into(),
                    });
                }
            }
        }
        Self { measurements }
    }
}
