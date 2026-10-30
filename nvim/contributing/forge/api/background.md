# Background methods

The background service owns persisted generation conversations independently of the selected interactive session. These calls never accept an envelope `session_id` or start a provider.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `background.session.document`

Read or operate on a persisted background conversation.

- **Context:** Independent background service.
- **Input:** workspace, data_root, optional session_id/presentation, and optional auto_compaction with expected_revision.
- **Result and effects:** Returns the background snapshot, presentation result, or settings update.
- **Boundary:** Reading a background timeline does not require opening an interactive Harness session.
- **Inspected source:** [`BackgroundManager::timeline`](../../../rust/forge/crates/forge-harness/src/background/manager.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `none`.

| Parameter | Type and default | Contract |
| --- | --- | --- |
| `data_root` | Required path string | Root containing persisted background sessions and settings. |
| `workspace` | Required path string | Existing repository whose current branch selects the default background conversation. |
| `session_id` | Optional string, omitted/null means current branch | Selects a persisted background session within this workspace. This is a payload selector, not envelope context. An explicit unknown ID fails. |
| `presentation` | Optional presentation-operation object, omitted/null means no operation | Operates on the selected background conversation's document. Uses the same typed presentation operations as the session document service. |
| `auto_compaction` | Optional `{enabled: boolean, threshold_tokens: integer}` | Updates the selected conversation's policy. Its threshold must satisfy the current context policy. |
| `expected_revision` | Optional unsigned integer | Required when updating `auto_compaction`. Must match the current settings revision. |

When no conversation exists for the branch, the result is `{sessions: [], session: null}` or the workspace's other background sessions with `session: null`. No new conversation is created. A selected conversation returns `sessions`, `session`, its effective `settings`, retained `events`, and the nested presentation `result` or null. Presentation operations retain their document IDs, revisions, view IDs, and sequence requirements. A highlight operation returns an empty object after applying its result to the owned document.

There is no progress callback for this request. Reads inspect local Git and SQLite state. Policy updates write local settings with a revision check, so a stale update cannot overwrite a newer value. A nested presentation change can mutate the in-memory document view but does not execute model work. Retrying reads is safe. After a policy-write response is lost, read the settings before attempting another write. Keep the captured workspace/session selector and document generation when adopting asynchronous results.

An empty repository with no background conversation is covered by [the background host fixture](../../../../nvim/tests/forge/background_host.lua):

```json
{"id":10,"method":"background.session.document","params":{"data_root":"/tmp/forge","workspace":"/tmp/repository"}}
```

```json
{"id":10,"result":{"sessions":[],"session":null}}
```

The directories in the example stand for the fixture's isolated data root and initialized Git repository. Invalid repository paths, unavailable storage, unknown selected sessions, stale settings revisions, and invalid nested document operations return a request error without redirecting to the active Harness conversation.

## `background.settings`

Read or update persisted background-generation preferences.

- **Context:** Independent background service.
- **Input:** data_root, defaults, and optionally expected_revision and settings.
- **Result and effects:** Returns the settings document and revision.
- **Boundary:** Reject stale updates rather than overwriting newer preferences.
- **Inspected source:** [`commit_message/settings.rs`](../../../rust/forge/src/commit_message/settings.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `none`.

| Parameter | Type and default | Contract |
| --- | --- | --- |
| `data_root` | Required path string | Shared settings storage root. |
| `defaults` | Required preferences object | Fallback preferences, validated on every call. |
| `settings` | Optional preferences object, omitted/null means read | Complete preferences to persist. |
| `expected_revision` | Optional unsigned integer | Required with `settings`, ignored for reads. Must match the shared settings revision. |

A preferences object has two required fields: `auto_compaction: {enabled: boolean, threshold_tokens: integer}` and `models`, an object keyed by provider name. Each model value contains required nonblank `model`, optional/null `thinking`, and optional `service_tier`, defaulting to `"default"`. Thinking accepts `none`, `minimal`, `low`, `medium`, `high`, `xhigh`, or `max`. Service tier accepts `default`, `fast`, or `ultrafast`. Provider names must be nonblank and cannot be `gemini`. Unknown fields fail decoding. The auto-compaction policy validates its positive threshold.

The result is `{revision, settings}`. Saved auto-compaction replaces the fallback policy, and saved model selections override matching default provider keys. An untouched store reports revision zero. Updates atomically save the provider preferences and advance the settings revision. No progress or asynchronous provider events accompany the response. This method performs local storage I/O and never contacts a provider or edits an interactive task. Invalid preferences and stale revisions leave the previous settings usable. After an unknown write outcome, read the current revision before deciding whether to update again.

The default read shape is verified by `defaults_persist_and_reject_stale_updates` in [the settings source](../../../rust/forge/src/commit_message/settings.rs):

```json
{"id":11,"method":"background.settings","params":{"data_root":"/tmp/forge","defaults":{"auto_compaction":{"enabled":true,"threshold_tokens":128000},"models":{"codex":{"model":"gpt-6-luna","thinking":"low"}}}}}
```

```json
{"id":11,"result":{"revision":0,"settings":{"auto_compaction":{"enabled":true,"threshold_tokens":128000},"models":{"codex":{"model":"gpt-6-luna","thinking":"low","service_tier":"default"}}}}}
```

An editor must discard an obsolete response when its settings owner has closed or switched providers. The revision guards writes, not delivery to a replacement view.
