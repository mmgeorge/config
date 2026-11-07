//! Per-turn developer context delivered through native thread history.
//!
//! Codex keeps the developer instructions sent at thread start or resume, and its
//! collaboration-mode state prefers the model catalog's built-in mode text over the
//! `developer_instructions` a `turn/start` supplies. Per-turn context such as tool approval,
//! access, and turn purpose therefore never reaches the model through that channel when the
//! catalog defines the mode. The adapter appends the context as a developer message with
//! `thread/inject_items` whenever it differs from what the model last saw on that thread.

use super::rpc::{MethodReply, NativeRpc};
use forge_provider_api::{ProviderError, ProviderSessionId, error::ProviderOperation};
use serde_json::json;

/// Native request that appends raw Responses API items to model-visible thread history.
const INJECT_METHOD: &str = "thread/inject_items";

/// Opening line that tells the model the injected text supersedes earlier injections.
const CONTEXT_PREAMBLE: &str = "Current Forge Harness turn context. It replaces any earlier Forge Harness turn context in this conversation.";

/// Track which per-turn context the model can currently see on one binding's native thread.
#[derive(Debug, Default)]
pub(super) enum TurnContextDelivery {
    /// No per-turn context is known to be model-visible, such as after open or retraction.
    #[default]
    Unknown,
    /// The model saw `context` on `thread`, and `compaction_count` compactions had been observed.
    Delivered {
        thread: ProviderSessionId,
        context: String,
        compaction_count: u64,
    },
    /// The runtime predates `thread/inject_items`; turns rely on collaboration mode alone.
    Unsupported,
}

impl TurnContextDelivery {
    /// Append `context` to `thread` unless the model already sees it, then record the outcome.
    ///
    /// `before_write` runs once, immediately before the native write, so the caller can record
    /// that history may now exist. Empty context is never injected. A runtime without the method moves the tracker to
    /// `Unsupported` and succeeds, leaving the caller's collaboration-mode delivery in place.
    /// Any other failure leaves the tracker unchanged so the next turn retries.
    pub(super) async fn deliver(
        &mut self,
        rpc: &NativeRpc,
        thread: &ProviderSessionId,
        context: &str,
        compaction_count: u64,
        before_write: impl FnOnce(),
    ) -> Result<(), ProviderError> {
        if context.is_empty() || !self.requires(thread, context, compaction_count) {
            return Ok(());
        }
        before_write();
        let item = json!({
            "type":"message",
            "role":"developer",
            "content":[{"type":"input_text","text":format!("{CONTEXT_PREAMBLE}\n\n{context}")}]
        });
        let reply = rpc
            .method_request(
                INJECT_METHOD,
                json!({"threadId":thread.as_str(),"items":[item]}),
                ProviderOperation::TurnStart,
            )
            .await?;
        *self = match reply {
            MethodReply::Answered(_) => Self::Delivered {
                thread: thread.clone(),
                context: context.to_owned(),
                compaction_count,
            },
            MethodReply::UnknownMethod => Self::Unsupported,
        };
        Ok(())
    }

    /// Forget delivered context after a history mutation that may have removed it.
    pub(super) fn invalidate(&mut self) {
        if let Self::Delivered { .. } = self {
            *self = Self::Unknown;
        }
    }

    /// Decide whether the model may not see `context` on `thread` as the latest injection.
    fn requires(&self, thread: &ProviderSessionId, context: &str, compaction_count: u64) -> bool {
        match self {
            Self::Unknown => true,
            Self::Delivered {
                thread: delivered_thread,
                context: delivered_context,
                compaction_count: delivered_compactions,
            } => {
                delivered_thread != thread
                    || delivered_context != context
                    || *delivered_compactions != compaction_count
            }
            Self::Unsupported => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn thread(id: &str) -> ProviderSessionId {
        id.try_into().unwrap()
    }

    /// Repeat delivery only when the thread, the text, or compaction state changed.
    #[test]
    fn delivered_context_requires_resend_only_after_a_visible_change() {
        let delivery = TurnContextDelivery::Delivered {
            thread: thread("native"),
            context: "approval ask".into(),
            compaction_count: 1,
        };
        assert!(!delivery.requires(&thread("native"), "approval ask", 1));
        assert!(delivery.requires(&thread("native"), "approval yolo", 1));
        assert!(delivery.requires(&thread("native"), "approval ask", 2));
        assert!(delivery.requires(&thread("replacement"), "approval ask", 1));
        assert!(TurnContextDelivery::Unknown.requires(&thread("native"), "approval ask", 0));
        assert!(!TurnContextDelivery::Unsupported.requires(&thread("native"), "approval ask", 0));

        let mut invalidated = delivery;
        invalidated.invalidate();
        assert!(matches!(invalidated, TurnContextDelivery::Unknown));
        let mut unsupported = TurnContextDelivery::Unsupported;
        unsupported.invalidate();
        assert!(matches!(unsupported, TurnContextDelivery::Unsupported));
    }
}
