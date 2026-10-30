# Github methods

The transport metadata comes from the shared contract. Each entry summarizes the inspected handler.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `github.actor.get`

Read the authenticated GitHub actor for a repository.

- **Context:** GitHub resource; no session envelope.
- **Input:** directory and repository.
- **Result and effects:** Returns actor identity.
- **Boundary:** Authentication/network errors do not imply an unauthenticated empty actor.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `directory` (local command context) and `repository` ([GithubRepositoryId](github-schemas.md#githubrepositoryid)). Returns [RemoteActor](github-schemas.md#remoteactor), containing `login` and authenticated `node_id`. The remote identity belongs to the requested hostname. This can launch the configured GitHub CLI and perform a network read. It does not change GitHub state or emit request progress. Invalid repository identity, missing authentication, remote failure, and malformed actor responses fail explicitly.

## `github.comment`

Change or reconcile a selected GitHub comment.

- **Context:** GitHub/cache resource; no session envelope.
- **Input:** directory, target, and typed CommentOperation.
- **Result and effects:** Returns the comment-operation result.
- **Boundary:** Use the comment target and durable recovery identity for uncertain outcomes.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [CommentParams](native-schemas.md#commentparams).

### Contract

Requires `directory`, `target` ([CommentTarget](github-schemas.md#commenttarget)), and `request` ([CommentOperation](github-schemas.md#commentoperation)). `edit` supplies body text and a stable receipt, `delete` supplies a stable receipt, and `reconcile` reads the captured target. Returns [CommentResult](github-schemas.md#commentresult), discriminated by `outcome`: confirmed/reconciled results carry optional comment state, while rejected/unknown results carry a message. A null state can confirm deletion.

Mutations validate the PR/comment identities and authenticated ownership, serialize remote work, and preserve uncertain outcomes. A successful transport response with `outcome_unknown` is not permission to repeat the mutation. Reconcile the captured target and receipt first. It can perform network writes and durable recovery work but emits no request-progress stream. Invalid captures, conflicting ownership, authentication, remote rejection, and uncertain dispatch remain distinguishable.

## `github.issue.document`

Open or operate on a native issue document.

- **Context:** GitHub issue document; no session envelope.
- **Input:** operation selects open, snapshot, save, refresh, view, close_view, act, resolve, or close; open includes document, directory, database, repository, and number.
- **Result and effects:** Returns document delivery, save/action/recovery outcome, or collection result.
- **Boundary:** save/act/resolve can mutate remote or durable draft state and require captured identities.
- **Inspected source:** [`nvim/rust/forge/src/issue_routes.rs`](../../../rust/forge/src/issue_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

Uses the `operation` discriminator and exact [IssueRequest](operation-schemas.md) fields. `open` supplies a document ID, directory, database, repository, and number, acquires the issue, and returns [IssueOpen](review-schemas.md#issueopen). `snapshot` returns the retained IssueOpen without refetching. `save` supplies document and optional capture array and returns [IssueSave](review-schemas.md#issuesave). `refresh` returns [IssueUpdate](review-schemas.md#issueupdate), and `resolve` returns [IssueSave](review-schemas.md#issuesave); resolve requires the retained operation ID and [RecoveryResolution](github-schemas.md#recoveryresolution). `view` supplies document/view/width, and `close_view` supplies document/view; both return an optional buffer patch. `act` supplies [DocumentInput](repository-schemas.md#documentinput) and returns [IssueEffect](review-schemas.md#issueeffect). `close` returns `{ "collected": boolean }` after document collection.

Open/refresh read remote issue data and maintain the local cache. Save can publish edited fields, while resolve settles a captured uncertain mutation. Captured edits must belong to this document. Preserve newer local edits, conflict/uncertainty flags, and recovery identity until resolved. View operations change local layout only. Inputs use document/view/target/sequence ownership, and stale inputs fail. Closing does not cancel already admitted remote work. Invalid variants, unknown document ownership, conflicting remote operations, malformed content, and I/O failures remain explicit. There is no request-progress stream.

## `github.issue.get`

Fetch or refresh a selected issue's detail.

- **Context:** GitHub/cache resource; no session envelope.
- **Input:** database, directory, and typed IssueDetailRequest.
- **Result and effects:** Returns issue detail.
- **Boundary:** Remote/cache errors remain explicit.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [IssueDetailParams](native-schemas.md#issuedetailparams).

### Contract

Requires `database`, `directory`, and `request` ([IssueDetailRequest](github-schemas.md#issuedetailrequest)). The request identifies the repository and an issue number greater than zero and no larger than the signed 32-bit range. Returns [DetailRecord](github-schemas.md#detailrecord), whose `item` contains the complete [IssueDetail](github-schemas.md#issuedetail) and whose `fetched_at` is Unix time in seconds.

The service verifies repository/issue identity, performs the remote read, and commits identical content to the local detail cache before returning. It holds repository operation ownership against cache deletion. The remote read has a 120-second deadline. Invalid identity, a busy/deleting store, remote failure, and cache commit failure are explicit. It does not publish a remote mutation or emit request progress.

## `github.issue.list`

Execute a cached issue-store operation.

- **Context:** GitHub/cache resource; no session envelope.
- **Input:** database, repo, and typed IssueOperation.
- **Result and effects:** Returns the issue operation's result.
- **Boundary:** This dispatcher is broader than a simple remote issue-list call.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [IssueRequest](native-schemas.md#issuerequest).

### Contract

Requires `database`, `repo` (repository owner/name), and `request` ([IssueOperation](github-schemas.md#issueoperation)). The `operation` discriminator chooses the local store behavior. Returns the corresponding untagged [IssueResult](github-schemas.md#issueresult): `upsert_page` returns committed page/state metadata, `state` returns synchronization state, `detail` returns an explicit found/missing detail, `details` preserves requested order, `upsert_detail` returns the committed detail, `publish_snapshot` returns `issue_count`, `reconcile_snapshot` returns state/readiness/publication flags, and `delete_cache` returns `deleted`.

Only the selected local operation executes. Upserts change cached data, publication writes the supplied snapshot path, and deletion obtains exclusive repository ownership. Other operations cannot race an admitted deletion. This method does not fetch or mutate remote GitHub data and emits no request progress. Invalid repository/operation fields, unavailable ownership, malformed storage, and filesystem failures remain errors.

## `github.issue.sync`

Synchronize an issue store with GitHub.

- **Context:** GitHub/cache resource; no session envelope.
- **Input:** database, directory, typed SyncRequest, and progress boolean defaulting false.
- **Result and effects:** Returns synchronization outcome and optional request-correlated progress.
- **Boundary:** Progress is not final completion; sync writes the local issue cache.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [IssueSyncRequest](native-schemas.md#issuesyncrequest).

### Contract

Requires `database`, `directory`, and `request` ([SyncRequest](github-schemas.md#syncrequest)). `progress` is optional and defaults to false. The request supplies repository identity, scope, output snapshot path, and optional `manual` (false by default). Returns [SyncOutcome](github-schemas.md#syncoutcome): whether a refresh occurred and the fetched-record/page counts. A recent nonmanual request can skip refresh through the ten-minute freshness rule.

Synchronization reads remote pages, commits local issue/cache state, and publishes the requested completion snapshot. With `progress: true`, request events named `github.issue.sync.progress` carry [SyncProgress](github-schemas.md#syncprogress) and the originating `request_id`. Progress is a latest-state stream, so intermediate updates can coalesce. The terminal response establishes completion even if the final progress update was coalesced. Local ownership, storage/publication, authentication, rate, remote, and decoding failures remain explicit. This does not mutate remote issues.

## `github.notification.document`

Open or operate on a native notification document.

- **Context:** GitHub notification document; no session envelope.
- **Input:** operation selects open, snapshot, view, close_view, act, or close; open includes document, workspace, and hostname.
- **Result and effects:** Returns document delivery or action/close outcome.
- **Boundary:** Actions can change remote notification state; snapshots are reads.
- **Inspected source:** [`nvim/rust/forge/src/notification_routes.rs`](../../../rust/forge/src/notification_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

### Contract

Uses the `operation` discriminator and exact [NotificationRequest](operation-schemas.md) fields. `open` supplies document/workspace/hostname and returns [NotificationOpen](review-schemas.md#notificationopen); `snapshot` returns the same shape from retained state. `view` supplies document/view/width and `close_view` supplies document/view; both return an optional buffer patch. `act` supplies [DocumentInput](repository-schemas.md#documentinput) and returns [NotificationUpdate](review-schemas.md#notificationupdate). `close` returns `{ "closed": boolean }` after collection.

Act supports open, browse, save, unread, done, expand, count, more, and refresh. More/refresh fetch remote pages; expand can fetch detail; state actions can mutate the remote notification. Copy the returned effect identity before opening a target. The `more` flag preserves pagination, and diagnostics/recovery preserve partial or uncertain results. Notification recovery serializes `resource.number` as a decimal string. Inputs must match the document/view/target/sequence, and an in-progress action prevents overlapping mutation. Invalid ownership, action, host, remote response, and persistence failures remain explicit. The call emits no request-progress stream.

## `github.notification.list`

Read a page of GitHub notifications.

- **Context:** GitHub resource; no session envelope.
- **Input:** directory and the typed notification read request.
- **Result and effects:** Returns the requested notification page.
- **Boundary:** Pagination and remote read errors remain distinct from an empty page.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `directory` and `request` ([NotificationReadRequest](github-schemas.md#notificationreadrequest)). The request supplies a hostname and an optional positive page cursor. Omission starts at page 1. Returns [NotificationPage](github-schemas.md#notificationpage), including complete [NotificationRecord](github-schemas.md#notificationrecord) values, `next_cursor`, and `complete`. This is a participating-notification read across repositories on the selected hostname, not a repository-filtered issue query. Follow `next_cursor` until `complete` rather than treating one page as the entire collection. It can contact GitHub, changes no remote notification state, and emits no request progress. Invalid hosts/cursors, duplicate identities, malformed records, authentication, and network failures are errors.

## `github.pull_request`

Change or reconcile a pull request.

- **Context:** GitHub/cache resource; no session envelope.
- **Input:** directory and a PullRequestRequest selecting transition, edit, reconcile, or reconcile_edit for its target.
- **Result and effects:** Returns the pull-request operation outcome.
- **Boundary:** This is a mutation/reconciliation dispatcher, not a get-only endpoint.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [PullRequestParams](native-schemas.md#pullrequestparams).

### Contract

Requires `directory` and `request` ([PullRequestRequest](github-schemas.md#pullrequestrequest)). The request carries the PR target and selects transition, edit, or their reconciliation operations. The nested `request` uses [PullRequestOperation](github-schemas.md#pullrequestoperation) and captures the intended state/text. Returns [PullRequestResult](github-schemas.md#pullrequestresult), including `ok`, the outcome, and optional observed state or text.

The service serializes work for the remote PR, validates identity and current state, and retains uncertain delivery. A terminal transport response can report an unknown remote outcome. Reconciliation reads the captured remote target before another write is authorized. This can mutate GitHub and durable recovery state. It emits no request progress. Invalid identities, competing ownership, unavailable authentication, changed remote state, rejection, and transport uncertainty remain explicit.

## `github.pull_request.creation.context`

Collect the repository/branch context required for PR creation.

- **Context:** GitHub resource; no session envelope.
- **Input:** directory.
- **Result and effects:** Returns repository identity, branch, head commit, and creation metadata.
- **Boundary:** This reads creation context and does not create a PR.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `directory` (local repository path). Returns [CreationContext](github-schemas.md#creationcontext): validated remote repository identity, repository node ID, current branch, and head commit. The operation reads repository metadata through the configured GitHub CLI and reads the branch/commit through Git. It does not create a pull request, modify Git state, or emit request progress. Missing repository context, detached/invalid branch or commit data, authentication, remote, and decoding failures remain errors.

## `github.recovery.acknowledge`

Acknowledge a retained mutation outcome.

- **Context:** GitHub resource; no session envelope.
- **Input:** resource and operation_id.
- **Result and effects:** Returns acknowledged=true.
- **Boundary:** Acknowledge the matching captured operation, not an unrelated request ID.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `resource` and matching `operation_id` (string). Returns `{ "acknowledged": true }` after retiring an adoptable terminal record. This local mutation acquires the retained resource lease and validates the operation. Editable mutations with an `edit_sequence` require `github.recovery.draft.settle` instead. Unknown, mismatched, or nonterminal records fail. It does not contact GitHub, repeat the original write, or emit request progress.

## `github.recovery.draft.settle`

Persist a settled draft and acknowledge the matching mutation.

- **Context:** GitHub resource; no session envelope.
- **Input:** resource, operation_id, and draft.
- **Result and effects:** Returns acknowledged=true.
- **Boundary:** Draft settlement and acknowledgement follow the service's durable recovery boundary.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `resource`, matching `operation_id` (string), and `draft` (object) identifying the same `repo` (`owner/name` string) and integer `number`. Returns `{ "acknowledged": true }` after durable draft settlement and acknowledgement. Confirmed, rejected, user-linked, and user-closed-unknown phases can settle; an uncertain operation cannot.

The draft merge preserves newer creation sequences and immutable retired receipts. A creation draft must carry a positive integer sequence below 2^53. Reusing one sequence for different content fails. Repository creation settlement retains its creation receipt. Wrong resource identity, nonterminal recovery state, conflicting draft sequence, or storage failure is explicit. It performs local persistence, emits no request progress, and does not publish draft text to GitHub.

## `github.recovery.inspect`

Read retained recovery state for a resource.

- **Context:** GitHub resource; no session envelope.
- **Input:** resource.
- **Result and effects:** Returns the recovery record when present.
- **Boundary:** An absent record is distinct from proof that a remote operation failed.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `resource` ([RecoveryResource](github-schemas.md#recoveryresource)). Returns the retained [RecoveryRecord](github-schemas.md#recoveryrecord), or JSON null if this resource has no unsettled record. This is a local read under the configured durable recovery root. It does not contact GitHub, repeat a mutation, or emit request progress. Invalid resource identity, unavailable recovery configuration, malformed storage, and I/O failures are explicit. A non-null result blocks blind replay even after the repository cache or host has been replaced.

## `github.recovery.resolve`

Apply an explicit recovery resolution.

- **Context:** GitHub resource; no session envelope.
- **Input:** directory, resource, operation_id, and typed resolution.
- **Result and effects:** Returns the resolved recovery record.
- **Boundary:** Resolution can inspect or link remote outcomes and must preserve the captured operation identity.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `directory` (local GitHub command context), `resource`, `operation_id` (string matching the retained capture), and `resolution` ([RecoveryResolution](github-schemas.md#recoveryresolution)). `link` requires positive integer `remote_id` and reads/verifies that remote result. `close_unknown` records the user's decision to close uncertainty without asserting non-delivery. `not_dispatched` is valid only while the record is still prepared and cannot override possible dispatch.

Returns the updated [RecoveryRecord](github-schemas.md#recoveryrecord). Resolution obtains resource ownership and persists the outcome. Linking can contact GitHub; it does not execute the original write again. Changed operation/resource identities, failed remote verification, lost ownership, and invalid phase transitions fail. It emits no request progress. Settle any editable draft before acknowledging the terminal record.

## `github.repository.metadata`

Read cached or refreshed repository contributor metadata.

- **Context:** GitHub/cache resource; no session envelope.
- **Input:** cache_directory, directory, and MetadataRequest with repository and ttl_seconds.
- **Result and effects:** Returns repository metadata and contributor-read failure information where represented.
- **Boundary:** The TTL controls cache freshness rather than changing repository identity.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [MetadataParams](native-schemas.md#metadataparams).

### Contract

Requires `directory`, `cache_directory`, and `request` ([MetadataRequest](github-schemas.md#metadatarequest)). The cache directory must end in `<encoded-hostname>/repos/<owner>/<name>` matching the requested repository, case-insensitively. Hostname bytes outside ASCII letters, digits, dots, and hyphens use uppercase percent encoding. The request contains repository identity and optional `ttl_seconds`, defaulting to 600. Returns [RepositoryMetadata](github-schemas.md#repositorymetadata): repo/hostname identity, fetch time, contributors, and any per-source failures. The operation uses a valid fresh local cache or reads remote contributor sources and updates that cache. A partially successful result can include `failure` entries and must not be presented as complete contributor discovery. Invalid cache identity, unavailable ownership, total remote failure, and storage errors remain explicit. It performs no remote write and emits no request progress.

## `github.review.document.batch.begin`

Begin batched review mode.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document.
- **Result and effects:** Returns the review-mode delivery.
- **Boundary:** Entering mode does not submit the pending review.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `document` ([ReviewDocumentParams](native-schemas.md#reviewdocumentparams)); capture arrays are not adopted. Returns [ReviewBatchedDelivery](review-schemas.md#reviewbatcheddelivery): snapshot, mode, and viewed-file paths. This switches the local review to batched mode and persists its draft. It does not submit the review. Invalid document state or draft persistence failure is explicit.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.batch.recover`

Resolve a retained batched-review submission.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, operation_id, and resolution.
- **Result and effects:** Returns the recovered submission outcome.
- **Boundary:** Do not create a second review to resolve an unknown first outcome.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewBatchRecoverParams](native-schemas.md#reviewbatchrecoverparams).

### Contract

Requires `document`, the retained `operation_id`, and [RecoveryResolution](github-schemas.md#recoveryresolution) ([ReviewBatchRecoverParams](native-schemas.md#reviewbatchrecoverparams)). Returns [ReviewBatchedRecoveryDelivery](review-schemas.md#reviewbatchedrecoverydelivery), containing the recovered submission state and `fresh_required`. It verifies the captured operation/resource and settles the durable draft/recovery record. Resolution can read GitHub but does not repeat the original submission. Missing/mismatched records, invalid phase transitions, changed remote truth, and storage errors fail explicitly.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.batch.submit`

Submit a captured batched review.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, verdict, capture defaulting empty, and draft_comment defaulting empty.
- **Result and effects:** Returns the submission delivery/outcome.
- **Boundary:** Retain submission/recovery identities when the remote result is uncertain.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewBatchSubmitParams](native-schemas.md#reviewbatchsubmitparams).

### Contract

Requires `document` and `verdict` ([ReviewVerdict](review-schemas.md#reviewverdict): approve, request_changes, or comment), with optional capture/draft-comment arrays defaulting to empty ([ReviewBatchSubmitParams](native-schemas.md#reviewbatchsubmitparams)). The document must be in batched mode and contain at least one nonempty inline draft anchored to the retained PR head; drafts spanning different head revisions fail. Returns [ReviewBatchedSubmissionDelivery](review-schemas.md#reviewbatchedsubmissiondelivery), containing snapshot/mode/viewed paths, an outcome string, and optional operation ID. It captures draft text, persists intent, validates actor/PR state, and submits a remote review. A retained uncertain submission blocks a new one. Preserve the returned operation ID for recovery; transport success alone does not prove publication.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.close`

Close the native review document.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document.
- **Result and effects:** Returns closed=true after document closure.
- **Boundary:** Closing is not equivalent to submitting or discarding remote review state.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `document` ([ReviewDocumentParams](native-schemas.md#reviewdocumentparams)). Returns `{ "closed": true }` after removing the document from the host registry. Unknown/already removed documents fail. Admitted remote jobs retain their owner until settlement, so closing the document is not cancellation of a submitted mutation. Captures accepted by the shared schema are not adopted by close.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.comment`

Apply a review-comment command and captured edits.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, typed ReviewCommentCommand, capture, and draft_comment.
- **Result and effects:** Returns the comment-command delivery/outcome.
- **Boundary:** The command determines draft, edit, reply, or recovery semantics.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewCommentParams](native-schemas.md#reviewcommentparams).

### Contract

Requires `document` and `command` ([ReviewCommentCommand](review-schemas.md#reviewcommentcommand)); accepts `capture` and `draft_comment` arrays defaulting to empty ([ReviewCommentParams](native-schemas.md#reviewcommentparams)). Returns [ReviewCommentResult](review-schemas.md#reviewcommentresult): a comment snapshot, optional remote outcome, and optional patch. The `snapshot` command requires both capture arrays to be empty. Other commands adopt edits under the same document and can perform remote comment mutations/recovery. Preserve the returned comment identity, edit sequence, and uncertainty state. Invalid ownership, conflicting save/recovery, remote rejection, and uncertain dispatch remain explicit.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.file.get`

Read a selected review file.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, directory, and path.
- **Result and effects:** Returns file data/delivery.
- **Boundary:** The path must identify a file belonging to the document.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewFileParams](native-schemas.md#reviewfileparams).

### Contract

Requires `document`, captured `directory`, and repository-relative `path` ([ReviewFileParams](native-schemas.md#reviewfileparams)). Returns [ReviewFileDelivery](review-schemas.md#reviewfiledelivery), whose analysis fields are flattened alongside the optional buffer patch. The result captures base/head/merge-base and optional blob identities, an optional syntax diagnostic, and `more`. It acquires the file against the retained PR comparison and projects an initial portion. Continue through `file.page.get` while `more` is true. Missing files, changed comparison identity, analysis admission, and remote/Git failures are errors.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.file.page.get`

Read the next materialized portion of a review file.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document and path.
- **Result and effects:** Returns the additional file data/delivery.
- **Boundary:** Do not restart another file's pagination state.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewFilePageParams](native-schemas.md#reviewfilepageparams).

### Contract

Requires `document` and `path` ([ReviewFilePageParams](native-schemas.md#reviewfilepageparams)). Returns the next [ReviewFileDelivery](review-schemas.md#reviewfiledelivery) using the file's retained projection cursor, without accepting a caller-supplied cursor or directory. The file must already be acquired, have `more: true`, and have no acquisition in flight. It preserves the same comparison identity and appends local projection work. No continuation, stale identity, or unavailable analysis fails explicitly.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.file.viewed.set`

Set a reviewed file's viewed flag.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, path, and viewed.
- **Result and effects:** Returns the updated delivery/outcome.
- **Boundary:** This is a state change and may contact GitHub.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewSetViewedParams](native-schemas.md#reviewsetviewedparams).

### Contract

Requires `document`, `path`, and `viewed` (boolean). Returns [ReviewBatchedDelivery](review-schemas.md#reviewbatcheddelivery) after updating the locally retained viewed-file set and persisting the draft. This is not a GitHub mark-as-viewed request. The path and review mode must be valid for the retained document, and persistence failures remain errors.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.header.load`

Load or refresh the review header.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, directory, and optional initial snapshot.
- **Result and effects:** Returns header-load result and corresponding native updates.
- **Boundary:** Retain only an initial snapshot that belongs to this document's PR.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `document` and the captured `directory`, with optional `initial` PR JSON ([ReviewLoadParams](native-schemas.md#reviewloadparams)). The initial snapshot is retained against that owner before header loading. Returns `{ "ready": true, "timing": {...} }` after header acquisition, with microsecond timings. It can fetch remote PR metadata and update the local header. Invalid snapshots, mismatched directories, and header-read errors fail the request.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.input.apply`

Apply a captured review-document input action.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** input and directory.
- **Result and effects:** Returns the action's native delivery/outcome.
- **Boundary:** Validate document revision, view, and target before applying the action.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewInputParams](native-schemas.md#reviewinputparams).

### Contract

Requires `input` ([DocumentInput](repository-schemas.md#documentinput)) and the captured `directory`. Supported actions are `activate`, `open`, `browse`, `expand`, `lifecycle:draft`, `lifecycle:open`, and `lifecycle:closed`. Returns [ReviewActionDelivery](review-schemas.md#reviewactiondelivery): patches, optional effect/diagnostic/comment/choices, and a refresh flag. Input carries its document, view, target, and sequence, so stale or cross-view actions cannot be retargeted to another row. Expansion can fetch remote sections/files, while lifecycle actions can mutate the PR. Follow the returned effect using its captured identity, and apply only patches matching the current document revision.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.lifecycle.reconcile`

Reconcile the remote PR lifecycle with the native review.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document.
- **Result and effects:** Returns lifecycle delivery, including recovery state where present.
- **Boundary:** Reconciliation is not permission to repeat an uncertain mutation.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `document` ([ReviewDocumentParams](native-schemas.md#reviewdocumentparams)); accepted capture arrays are ignored. Returns [ReviewLifecycleDelivery](review-schemas.md#reviewlifecycledelivery) after reading current remote lifecycle and reconciling any retained mutation. It does not repeat the lifecycle write. Remote failures and changed identity remain errors, while unresolved state is represented by recovery/freshness fields.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.lifecycle.resolve`

Resolve an identified PR lifecycle recovery record.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, operation_id, and resolution.
- **Result and effects:** Returns the lifecycle resolution outcome.
- **Boundary:** The resolution must name the captured operation.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewLifecycleResolveParams](native-schemas.md#reviewlifecycleresolveparams).

### Contract

Requires `document`, the retained `operation_id`, and `resolution` ([RecoveryResolution](github-schemas.md#recoveryresolution)). Returns [ReviewLifecycleDelivery](review-schemas.md#reviewlifecycledelivery). The operation ID must match this document's lifecycle recovery record. A `close_unknown` resolution returns no assumed lifecycle and sets `fresh_required: true`; other successful resolutions reconcile remote truth. It changes durable recovery state and can read GitHub, but does not replay the original mutation. Missing/mismatched records and invalid phase transitions fail.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.load`

Load review sections from the remote resource.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, directory, and optional initial snapshot.
- **Result and effects:** Returns load diagnostics and timing information while document updates are published.
- **Boundary:** Loading does not mean unsent draft text may be discarded.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewLoadParams](native-schemas.md#reviewloadparams).

### Contract

Requires `document` and the captured `directory` ([ReviewLoadParams](native-schemas.md#reviewloadparams)). The shared schema accepts `initial`, but this operation does not consume it. It loads the document's remote sections and returns `{ "diagnostic": [...], "timing": {...} }`; timing values are microseconds. Section failures are retained as diagnostics so already usable sections remain available. A successful transport result does not imply every section loaded. Render/read the retained document to consume updates.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.open`

Open a native PR review document.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** directory and exactly one of captured target or repository plus number; optional initial snapshot.
- **Result and effects:** Returns the review snapshot.
- **Boundary:** Reject ambiguous/missing target combinations instead of guessing a PR.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewOpenParams](native-schemas.md#reviewopenparams).

### Contract

Accepts [ReviewOpenParams](native-schemas.md#reviewopenparams): `directory` and exactly one of `target` or the pair `repository`/`number`. The latter form accepts optional `initial` captured PR JSON. Returns [ReviewSnapshot](review-schemas.md#reviewsnapshot), including the newly allocated document ID, editable fields, and save/uncertainty state. It reads remote PR content unless a supplied initial snapshot can seed the open. A mixed or incomplete target form fails. Subsequent requests must retain the returned document ID and the same captured directory.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.reconcile`

Reconcile the native review with current remote state.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document.
- **Result and effects:** Returns the reconciled review snapshot.
- **Boundary:** Preserve current unsent/durable draft content according to the service's merge rules.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `document` ([ReviewDocumentParams](native-schemas.md#reviewdocumentparams)); accepted capture arrays are not adopted by this operation. Returns [ReviewSnapshot](review-schemas.md#reviewsnapshot) after observing remote fields and settling the retained save state. It reads remote truth and preserves newer local text. This does not blindly repeat an uncertain write. Unknown ownership, remote read failure, and incompatible retained state fail explicitly.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.render`

Render the current native review document for a width profile.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document and width.
- **Result and effects:** Returns the rendered document delivery.
- **Boundary:** Rendering must preserve the document's logical identity.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewRenderParams](native-schemas.md#reviewrenderparams).

### Contract

Requires `document` and `width` ([ReviewRenderParams](native-schemas.md#reviewrenderparams)). Returns [ReviewMaterialization](review-schemas.md#reviewmaterialization), with fields, comments, inline anchors, timing values in microseconds, complete buffer snapshot, and optional patch. This performs local diff/syntax projection. It does not fetch missing remote sections or save edits. Width must agree with the oldest live view when one owns the document's presentation. Invalid width, stale ownership, or unavailable analysis fails.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.save`

Save captured editable review content.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, capture, and draft_comment, with empty-list defaults.
- **Result and effects:** Returns the save result.
- **Boundary:** Captured edits retain revision/region validation.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `document` and accepts `capture`/`draft_comment` arrays defaulting to empty ([ReviewDocumentParams](native-schemas.md#reviewdocumentparams)). Returns [ReviewSaveResult](review-schemas.md#reviewsaveresult): the updated snapshot and optional [PullRequestResult](github-schemas.md#pullrequestresult). Every captured region must belong to this review. It adopts local draft edits, saves the submitted text, and reconciles against the captured remote baseline. Newer local edits remain separate from that submission. An unresolved prior save blocks another save. Inspect the returned remote outcome and snapshot uncertainty rather than treating transport success as confirmed publication.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.section.get`

Read a review section page.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, directory, section, and optional cursor.
- **Result and effects:** Returns the selected section page/delivery.
- **Boundary:** Use the cursor for that section and document.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewSectionParams](native-schemas.md#reviewsectionparams).

### Contract

Requires `document`, captured `directory`, and `section` ([ReviewSectionKind](review-schemas.md#reviewsectionkind)); `cursor` is optional/null. Returns [ReviewSectionState](review-schemas.md#reviewsectionstate), including accumulated items, next cursor, completeness/loading state, optional diagnostic, and timings. The section enum includes overview, commits, files, reviews, conversation, review_comments, threads, checks, and requested_reviewers. Continue with the returned cursor rather than inventing an offset. Remote acquisition updates the retained section. Ownership/cursor changes, remote failure, and decoding errors remain explicit.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.thread.get`

Read a review thread page.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, directory, thread_node_id, and optional cursor.
- **Result and effects:** Returns the selected thread page/delivery.
- **Boundary:** The thread and cursor belong to this PR review.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewThreadParams](native-schemas.md#reviewthreadparams).

### Contract

Requires `document`, captured `directory`, and `thread_node_id`; `cursor` is optional/null ([ReviewThreadParams](native-schemas.md#reviewthreadparams)). The thread must already belong to the retained threads section. A supplied cursor must equal its retained `next_cursor`. Returns [ReviewThreadDelivery](review-schemas.md#reviewthreaddelivery): a summary containing node ID, loaded/total comment counts, next cursor, and optional projection patch. It fetches and retains comment content for the document. Unknown threads, stale cursors, changed owner/section, unavailable analysis, and remote failures are errors.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.transition`

Change the reviewed PR lifecycle state.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document and desired pull-request state.
- **Result and effects:** Returns lifecycle delivery/outcome.
- **Boundary:** Remote acceptance may require explicit reconciliation.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewTransitionParams](native-schemas.md#reviewtransitionparams).

### Contract

Requires `document` and `desired` ([DesiredPullRequestState](github-schemas.md#desiredpullrequeststate)). Returns [ReviewLifecycleDelivery](review-schemas.md#reviewlifecycledelivery): optional observed lifecycle, optional durable recovery record, and `fresh_required`. The service captures actor/PR identity and persists mutation intent before dispatch. A nonterminal recovery record blocks another transition. It can mutate remote lifecycle; it does not save unrelated draft text. An unknown outcome must be reconciled or explicitly resolved before another transition.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.document.view.set`

Attach or update a review view's width.

- **Context:** Native GitHub review document; no session envelope.
- **Input:** document, view, and width.
- **Result and effects:** Returns view-specific rendered delivery.
- **Boundary:** A view identity is separate from the document identity.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [ReviewViewParams](native-schemas.md#reviewviewparams).

### Contract

Requires `document`, `view`, and `width` ([ReviewViewParams](native-schemas.md#reviewviewparams)). Registers/updates that view and returns [ReviewMaterialization](review-schemas.md#reviewmaterialization) when presentation changes, otherwise JSON null. The oldest live view owns the shared width. Registration also retains input sequencing for that view. This is a local presentation mutation, not a remote save.

These calls use host-owned document identities, not Harness session IDs. Unknown input fields fail decoding. They emit no request-progress stream. The terminal response completes this operation; it does not promise that all future document loading or previously admitted remote work has finished.

## `github.review.draft.get`

Read a locally persisted draft for a GitHub resource.

- **Context:** GitHub resource; no session envelope.
- **Input:** resource.
- **Result and effects:** Returns the stored draft value.
- **Boundary:** Reading a draft does not publish it to GitHub.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `resource` ([RecoveryResource](github-schemas.md#recoveryresource)). Returns the locally stored draft object or JSON null when absent. Draft fields are owned by the corresponding review workflow and are not a remote GitHub snapshot. The read requires the configured durable recovery root but no interactive session. It does not contact GitHub, mutate a draft, or emit request progress. Invalid resource/storage state fails explicitly.

## `github.review.draft.save`

Persist a draft for a GitHub resource.

- **Context:** GitHub resource; no session envelope.
- **Input:** resource and draft.
- **Result and effects:** Returns saved acknowledgement.
- **Boundary:** This is local persistence, distinct from remote publication.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `resource` and `draft` (object). The draft must include `repo` (`owner/name`) and integer `number` matching the resource. Returns `{ "saved": true }`. The local store merges supplied top-level draft fields, retains newer creation sequences, and prevents replacement of immutable retired-operation receipts. A creation draft requires a positive sequence below 2^53; an equal sequence must contain identical data. Wrong identity, conflicting sequence, malformed retained data, and I/O failure are explicit. This writes local draft state only, emits no progress, and does not publish to GitHub.

## `github.review.mutate`

Execute a captured GitHub review mutation.

- **Context:** GitHub resource; no session envelope.
- **Input:** directory and typed ReviewMutationRequest.
- **Result and effects:** Returns the mutation record/outcome.
- **Boundary:** Retain the operation identity when remote acceptance is uncertain.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `directory` (local GitHub command context) and `request` ([ReviewMutationRequest](github-schemas.md#reviewmutationrequest)). The request captures resource, stable operation ID, authenticated actor node ID, optional parent node ID, optional edit sequence/draft target, and exactly one tagged [ReviewMutation](github-schemas.md#reviewmutation). The linked declaration enumerates every accepted operation and its fields.

Returns a durable [RecoveryRecord](github-schemas.md#recoveryrecord), including captured intent, confirmed partial steps, and the resulting phase. The service obtains per-resource ownership, writes intent before possible dispatch, validates remote actor/context, and performs the selected GitHub mutation. A transport-successful response may still report rejected or unknown outcome. An existing unsettled record blocks another write for that resource. Do not change the operation ID to bypass it.

The call can perform network writes and local persistence. It emits no request-progress stream. Invalid actor/resource binding, stale remote state, concurrent resource ownership, network failure, partial completion, and uncertain dispatch remain explicit. Recovery survives host replacement and repository-cache deletion. Use inspect/resolve and draft settlement before acknowledging the completed operation.

## `github.review.submission.context`

Read current remote context before submitting a review.

- **Context:** GitHub resource; no session envelope.
- **Input:** directory, repository, number, optional pending_review_id, and expected comment/review items.
- **Result and effects:** Returns submission context for validating the pending review.
- **Boundary:** Changed remote state must be reconciled before submitting captured drafts.
- **Inspected source:** [`nvim/rust/forge/src/github_routes.rs`](../../../rust/forge/src/github_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

### Contract

Requires `directory`, `repository` ([GithubRepositoryId](github-schemas.md#githubrepositoryid)), `number` (positive integer through the signed 32-bit range), and `expected` (array of [PendingReviewExpected](github-schemas.md#pendingreviewexpected)). `pending_review_id` is optional/null, or a positive integer. Each expected member has a unique positive `id` and exact `body` text.

Returns [ReviewSubmissionContext](github-schemas.md#reviewsubmissioncontext): actor node ID, current PR head commit, optional pending review ID, and captured pending comments. It reads remote identity and review membership, verifies that the review remains pending and belongs to the actor, and compares exact member identities/text. Changed head identity, actor, state, or membership fails instead of authorizing a stale submission. The call does not submit the review or emit request progress.

