//! Per-turn developer context delivered through native thread history.
//!
//! Codex keeps the developer instructions sent at thread start or resume, and its
//! collaboration-mode state prefers the model catalog's built-in mode text over the
//! `developer_instructions` a `turn/start` supplies. Harness therefore sends only the context
//! sections that changed since the conversation last accepted them, together with the input's
//! note, as one `<forge>` block, and the adapter appends it as a developer message with
//! `thread/inject_items` before the turn starts. Harness owns which sections are pending; this
//! adapter owns only the native mechanics.
//!
//! The same channel carries the reason for a declined approval during a turn: Codex queues an
//! item injected into an active turn as that turn's pending input, and the decline reply itself
//! has no field for a reason.

use super::rpc::{MethodReply, NativeRpc};
use forge_provider_api::{ProviderError, ProviderSessionId, error::ProviderOperation};
use serde_json::json;

/// Native request that appends raw Responses API items to model-visible thread history.
const INJECT_METHOD: &str = "thread/inject_items";

/// Developer note appended after a rewind that removed turns.
///
/// `thread/revert` cuts history where the first removed turn starts, and it accepts no other
/// boundary, so the developer context injected just before that turn stays in model-visible
/// history. For a goal that context holds the rolled-back objective. Harness cannot remove the
/// item, so this note voids it; the next turn's own context states the current task. The note
/// follows the open-time convention for bracketed `[Forge:` history notes and opens with the same
/// statement as the Claude adapter's rewind note.
pub(super) const ROLLBACK_NOTE: &str = "[Forge: the user rolled the conversation back to this point; later turns were discarded. A <forge> developer message above this note that follows the last completed turn belongs to the rolled-back interaction: its <task> and <instructions> are void. Do not continue them, quote them as the user's instruction, or count them as messages.]";

/// Remember whether the native runtime accepts developer-message injection.
#[derive(Debug, Default)]
pub(super) enum TurnContextDelivery {
    /// The runtime accepted injection, or has not been asked yet.
    #[default]
    Supported,
    /// The runtime predates `thread/inject_items`; context must travel in the user input.
    Unsupported,
}

impl TurnContextDelivery {
    /// Append each text to `thread` as one developer message.
    ///
    /// `before_write` runs once, immediately before the native write, so the caller can record
    /// that history may now exist. Nothing is written when `texts` is empty. The result is
    /// `false` when the runtime lacks the method, in which case the caller must place the context
    /// in the user input instead. Any other failure is returned unchanged.
    pub(super) async fn deliver(
        &mut self,
        rpc: &NativeRpc,
        thread: &ProviderSessionId,
        texts: &[String],
        before_write: impl FnOnce(),
    ) -> Result<bool, ProviderError> {
        if texts.is_empty() {
            return Ok(true);
        }
        if matches!(self, Self::Unsupported) {
            return Ok(false);
        }
        before_write();
        let items = texts
            .iter()
            .map(|text| {
                json!({
                    "type":"message",
                    "role":"developer",
                    "content":[{"type":"input_text","text":text}]
                })
            })
            .collect::<Vec<_>>();
        let reply = rpc
            .method_request(
                INJECT_METHOD,
                json!({"threadId":thread.as_str(),"items":items}),
                ProviderOperation::TurnStart,
            )
            .await?;
        Ok(match reply {
            MethodReply::Answered(_) => true,
            MethodReply::UnknownMethod => {
                *self = Self::Unsupported;
                false
            }
        })
    }
}
