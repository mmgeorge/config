# Repository methods

The transport metadata comes from the shared contract. Each entry summarizes the inspected handler.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `repository.generation`

Collect generation context, preview a prompt, or generate requested repository text.

- **Context:** Repository resource; no session envelope.
- **Input:** operation, workspace, comparison, optional base/head/context allowance, ignored_paths, generation configuration, and prepared input where required.
- **Result and effects:** Returns state, message, optional session_id, and optional prompt preview plus source/diff counts.
- **Boundary:** preview/context do not invoke a provider; generation requires explicit provider configuration.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [GenerationRequest](native-schemas.md#generationrequest).

### Contract

Accepts [GenerationRequest](native-schemas.md#generationrequest). Required fields are `operation` (one of `preview`, `context`, `generate`, `pull_request`), `workspace` (path string), and `comparison` (`status`, `head`, `staged`, `latest_commit`, or `pull_request`). Optional `ignored_paths` defaults to an empty string array. `generation` is [TextGenerationConfig](native-schemas.md#textgenerationconfig). Optional `base`, `head`, `max_bytes`, and `input` are restricted by operation.

`status` selects staged changes if the captured index has any, otherwise unstaged tracked changes and non-ignored untracked files. It selects the scope before content exclusions, so excluded staged files never cause fallback to unrelated unstaged changes. `head` compares the complete worktree against HEAD. `staged` always compares HEAD against the index and returns `none` when that comparison is empty. About generation and the default prompt preview use `status`. Explicit commit-editor regeneration uses `staged`, while amendment uses `latest_commit`.

Cancellation instead accepts exactly `{ "operation": "cancel", "request_id": 123 }`. The request ID targets an admitted generation on this host connection. The host registers cancellation before dispatching generation work, so it also applies before provider startup or while waiting for Background admission. Repeated cancellation and cancellation after completion are no-ops. The cancellation call returns a `none` response immediately. The original generation settles separately after cooperative provider cancellation and retains its recorded exchange. Other requests and the shared host remain active.

`preview` and `generate` use a non-PR comparison and reject base/head/max_bytes. Preview rejects provider configuration and returns exact prompt input without invoking a provider. Generate requires provider configuration and invokes the background conversation. `context` requires PR comparison, base, head, and a positive max_bytes accepted by the current context builder, and rejects provider configuration. It collects compact PR evidence. `pull_request` requires PR comparison, captured head, provider configuration, and nonblank prepared input. It rejects base/max_bytes and verifies that HEAD still matches before generating the description.

Returns [GenerationResponse](native-schemas.md#generationresponse). Interpret `state` first: `ready` has usable prompt or generated text, `none` means there are no changes, `retry` means the captured repository generation was superseded, and `conflicts` means PR context collection detected a merge conflict. `message` is generated text or null, `prompt` is present for provider-free preview/context, `session_id` identifies the background conversation when generation ran, and counts report source requests and diff pairs. A successful callback is not permission to apply a stale result.

The preview/context branches read local Git and syntax data. Generation may launch a provider, contact its service, and persist background history. It does not commit or publish the generated text. Background generation events are correlated by request and background session identity. Invalid operation/comparison combinations, invalid provider configuration, unavailable repository state, changed HEAD, provider errors, and generation admission failure are explicit errors. Keep the old text until a current successful result is available.

## `repository.revision.list`

List revision-completion candidates for a repository.

- **Context:** Repository resource; no session envelope.
- **Input:** workspace; initial offset 0; later pages repeat revision, repository, and reference_digest.
- **Result and effects:** Returns a page of candidates from the captured revision snapshot.
- **Boundary:** Reject pages whose repository, revision, or reference digest no longer matches.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [RevisionRequest](native-schemas.md#revisionrequest).

### Contract

Requires `workspace` (local path string). `offset` is an optional nonnegative integer defaulting to zero. The initial request omits `revision` and must use offset zero. Continuation requests repeat the returned integer `revision`, base64 `repository` identity, and string `reference_digest` with the next byte offset.

Returns `revision`, `repository`, `reference_digest`, `offset`, `next_offset`, `total_bytes`, `count`, and base64 `data`. Decode and concatenate each page's bytes, then split the completed stream on NUL bytes. An individual candidate can cross page boundaries. Continue while next_offset is less than total_bytes. Empty inventories return count/total_bytes zero and empty data. The per-page transfer budget does not cap the total candidate inventory.

No provider, network request, mutation, or progress stream occurs. Missing repositories, invalidated generations, superseded snapshots, changed repository/reference identities, out-of-range offsets, and integer overflow fail explicitly. Discard all pages from the old snapshot before restarting at offset zero. Do not combine pages across a host replacement or a refreshed snapshot.

## `repository.source.document`

Open or operate on a versioned source document.

- **Context:** Repository resource; no session envelope.
- **Input:** operation selects open, demand, snapshot, close_view, or close; open requires document, workspace, revision, and path.
- **Result and effects:** Returns a source snapshot/delivery or a close result.
- **Boundary:** The source belongs to the requested revision rather than the live buffer's latest bytes.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

The five operations and all required fields are enumerated in [SourceRequest](native-schemas.md#sourcerequest) and the [operation tables](operation-schemas.md). `open.path` is standard base64 of the repository-relative path bytes, not a display path. `workspace` is a local path string and `revision` is the requested Git revision. Invalid base64, unavailable revisions, and absent source fail explicitly.

`open` returns [SourceOpen](repository-schemas.md#sourceopen). It binds the document to immutable source bytes and returns the initial snapshot, source state, `more` flag, title, object identity, resolved revision, and optional syntax diagnostic. `demand` takes a captured [DocumentInput](native-schemas.md#documentinput) and returns [SourceDelivery](repository-schemas.md#sourcedelivery), including an optional patch. Continue while `more` is true. `snapshot` returns the current buffer snapshot. `close_view` returns `{ "closed": true }` after releasing that view. `close` returns a boolean `closed` indicating whether the document was collected.

No provider prompt, repository write, network request, or request-progress stream occurs. Reads and syntax work can be cancelled by document closure. View sequences, revisions, and document ownership fence asynchronous results. Apply returned patches only to the matching replica, and close the owned document after its last view is released.

## `repository.status.context`

Resolve or act on context from a status document.

- **Context:** Repository resource; no session envelope.
- **Input:** A StatusContextRequest with the captured StatusInput and operation-specific arguments.
- **Result and effects:** Returns context choices, results, or the requested action outcome.
- **Boundary:** The captured document/view/revision determines which action is valid.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

The four operations are [StatusContextRequest](native-schemas.md#statuscontextrequest). `action` requires a captured [StatusInput](repository-schemas.md#statusinput) and returns the resolved context action. `issues` requires that input plus `text` (string), finishes the resulting Git write ticket, and returns the [write outcome](#repositorywrite). `commit_message` requires a destination `document` ID, local `workspace` path, and commit `oid` string. It returns [SourceOpen](repository-schemas.md#sourceopen) containing the committed message. `about_message` requires destination `document` and `text` strings and returns SourceOpen for that immutable text.

Target resolution checks the captured status identity and freshness. Unknown context roles, changed targets, unavailable commits, and Git/write failures fail explicitly. Source results belong to their new document and must be closed through the source-document API. These operations do not generate text with a provider or emit request progress. Only the explicit issue-text action writes repository state.

## `repository.status.document`

Open or operate on a native repository-status document.

- **Context:** Repository resource; no session envelope.
- **Input:** operation selects local, comparison, open, snapshot, refresh, demand, open_target, input, close_view, body_snapshot, or close; supply the matching typed fields.
- **Result and effects:** Returns document snapshots, patches, accepted actions, or write outcomes.
- **Boundary:** A status input action can mutate the repository; do not treat all operations as reads.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

The eleven operations and their fields are enumerated in [StatusRequest](native-schemas.md#statusrequest) and the [operation tables](operation-schemas.md). Inputs use [StatusInput](repository-schemas.md#statusinput), with optional [StatusSelection](repository-schemas.md#statusselection) for `input`. `local.filename` must be absolute. `comparison.path`, when supplied, is standard base64 of repository path bytes. Other paths are ordinary local path strings.

`open`, `local`, `comparison`, and `snapshot` return [StatusSnapshot](repository-schemas.md#statussnapshot). `refresh`, `demand`, and `close_view` return native inventory/body delivery. `body_snapshot` uses the document, file ID, and captured file generation to recover a diff body. `open_target` resolves the captured target. `input` returns an accepted action, a repository [write outcome](#repositorywrite), or `{ "success": true, "patch": … }`, according to the selected action. `close` returns `{ "closed": boolean }`.

`open` emits `repository.status.document.progress` with `phase = "open"`, native timing data, and router serialization time before the final response. The document can also receive status inventory/body updates on its document subscription. Request IDs correlate timing, while document IDs, inventory revisions, file generations, and body revisions correlate content. Completion of `open` does not mean all lazily requested bodies are loaded.

Inventory and body reads invoke local Git and syntax services. Explicit input actions can stage, unstage, discard, or otherwise write repository state. Their result must be checked for per-target rejection or an unknown outcome. Invalid revisions, closed views, stale file generations, absent targets, Git failures, and source-read failures remain explicit. Retain the last usable document on failure and recover a missing patch through its snapshot operation.

## `repository.status.navigate`

Resolve the adjacent status navigation target.

- **Context:** Repository resource; no session envelope.
- **Input:** input and forward.
- **Result and effects:** Returns the navigation result.
- **Boundary:** Reject stale or invalid captured input instead of choosing from a different view.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [StatusNavigationParams](native-schemas.md#statusnavigationparams).

### Contract

Requires `input` ([StatusInput](repository-schemas.md#statusinput)) and `forward` (boolean). The input identifies a current status document, view, sequence, and navigation origin. Returns [NavigationDelivery](repository-schemas.md#navigationdelivery): an optional cursor `effect`, optional diff `body`, and boolean `more`. A deferred destination returns body delivery with more=true so navigation can continue after applying the body. A resolved header returns a cursor effect with the captured ownership fields. End-of-range returns null effect/body with more=false. The service does not change repository state. Unknown documents, invalid view/sequence, or stale body/file identities fail. It emits no progress and invokes no provider. Apply the navigation result only if the original view still owns the cursor.

## `repository.walkthrough.document`

Open or operate on a walkthrough and its annotation/source documents.

- **Context:** Repository/document resource; no session envelope.
- **Input:** operation selects open, snapshot, view, close_view, change, annotation_source, annotation_snapshot, annotation_view, annotation_close_view, annotation_close, or close.
- **Result and effects:** Returns walkthrough/source/annotation snapshots, source-row information, or close results.
- **Boundary:** Captured source/review identities and view widths are operation-specific and must remain current.
- **Inspected source:** [`nvim/rust/forge/src/walkthrough_routes.rs`](../../../rust/forge/src/walkthrough_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

The eleven operations and all fields are enumerated in the [operation tables](operation-schemas.md). `open` requires a unique `document` and local `workspace`; optional `inventory` defaults true. It loads the workspace's walkthrough artifact and returns its `snapshot`, `inventory_state`, optional `inventory_diagnostic`, and `stale` flag. `snapshot` returns the current [BufferSnapshot](repository-schemas.md#buffersnapshot). `view` and `close_view` return an optional [BufferPatch](repository-schemas.md#bufferpatch). Width changes reflow the retained content without changing its logical identity.

`change` requires captured `input`, a destination source `document`, and `annotation_document`. Optional `review` defaults false. It resolves the captured source and returns [WalkthroughSource](repository-schemas.md#walkthroughsource), which flattens SourceOpen and adds annotation snapshot, zero-based `source_row`, `stale`, and `review`. Review projection reads the captured before/after source. Failure closes the newly admitted annotation. `annotation_source` returns zero-based `source_row` for the captured annotation target. `annotation_snapshot`, `annotation_view`, and `annotation_close_view` return the corresponding snapshot or optional patch. `annotation_close` and `close` release their owned documents.

The service performs local artifact, Git, syntax, and optional semantic-inventory reads. It does not mutate repository files, invoke a provider, or emit request progress. Missing or invalid artifacts, stale captured actions, unavailable source, and closed document/view identities fail explicitly. Collapsed content remains in the source model while its rendered rows are omitted. Expand the relevant node before capturing a visible action, and retain document/view/revision ownership across asynchronous source resolution.

## `repository.write`

Execute the repository-write lifecycle.

- **Context:** Repository resource; no session envelope.
- **Input:** operation selects prepare, submit, cancel, acknowledge, or trace; use workspace/action or the returned intent/operation identity as required.
- **Result and effects:** Returns preparation, write outcome, acknowledgement, or trace data.
- **Boundary:** A submitted write can require outcome reconciliation; do not replay blindly.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

The five operations and nested write actions are [WriteRequest](repository-schemas.md#writerequest) and [WriteAction](repository-schemas.md#writeaction). `prepare` requires local `workspace` and `action`, whose `kind` is `create_branch`, `commit_editor`, `push`, `publish_branch`, or `pull`. Its result contains an opaque host-lifetime `intent`, integer `operation_id`, and `timing_us` map. Preparation validates and captures repository state without submitting the write.

`submit` consumes that `intent` exactly once and returns a write outcome: integer `operation_id`, aggregate boolean `success`, per-target `target` entries, base64 `affected` paths, optional `settlement_diagnostic`, and boolean `requires_refresh`. Each target has optional base64 `path`, `completion` (`completed`, `rejected`, `outcome_unknown`, or `not_started`), optional diagnostic, and optional exit code. A successful transport response does not imply every target completed.

During submission, `repository.write.progress` events carry the request ID, `stream` (`stdout` or `stderr`), integer `sequence`, and base64 `bytes`. The terminal response follows completion of the submitted ticket. Retain the operation ID when the outcome is uncertain. `acknowledge` requires that integer operation ID and returns `{ "acknowledged": true }` only for an adoptable receipt. `cancel` requires exactly one of intent or operation ID and returns `{ "cancellation_requested": true }`. Cancellation is a request, not proof that no write happened. `trace` accepts optional integer `after` and returns `{ "host_id": string, "trace": […] }`.

Preparation tokens become invalid after host replacement or consumption. Writes execute through the shared Git mutation coordinator and can change local branches, index/worktree state, commits, or remote repositories according to the action. Git, editor, credential, stale-capture, cancellation, and settlement failures remain explicit. Do not repeat `prepare`/`submit` merely because the first callback was lost. Reconcile the retained receipt and refresh before further dependent actions.

