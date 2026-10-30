# GitHub storage and review

A remote GitHub write can succeed while its response is lost, and two writers to the same pull request can interleave. Forge therefore gives the shared host sole ownership of remote operations, durable GitHub data, submitted review state, and reconciliation. Lua owns unsent fields, local comment drafts, and explicit save captures until the host acknowledges their revisions. No GitHub request requires Harness initialization, and GitHub failures never invalidate Harness state.

Read [Buffers and rendering](buffers-and-rendering.md) for editable-region application and [Host and protocol](host-and-protocol.md) for request ownership.

## Pull request creation

The creation workflow pins two commits before generating anything: the selected base branch and the head commit captured by `github.pull_request.creation.context`. When `origin` is configured, it fetches only the base branch into its tracking ref. Without `origin`, it resolves the local base branch. Git reads and the fetch share a 30-second deadline, and a failed fetch stops creation instead of reusing a stale ref. The first head-only commit supplies the title.

The `context` operation on `repository.generation` compares the merge base of the pinned commits with the captured head and reads committed objects only, so base-only changes and uncommitted work never enter the prompt. It first runs `git merge-tree --write-tree --quiet` on the pinned base and head. Exit status 1 returns `conflicts`, and Lua stops with "Merge conflicts. Resolve them before creating the PR." before any model request, push, or PR creation. The check changes no branch, index, or working file, although Git can write unreferenced merge objects. Missing refs, unrelated histories, and other failures stop generation, and transient invalidation retries keep the same commits for at most three attempts.

PR descriptions use the commit-message compact formatter with the remainder of a 500,000-byte allowance after instructions and metadata. The `pull_request` operation sends the evidence to `HarnessService.background`, which shares the Background model, thinking settings, and persistent provider conversation with commit generation and serializes their requests. Generation rejects a HEAD that differs from the captured revision, and branch publication uses that same head. Lua never launches a provider CLI or calls a model API for PR generation.

A description leads with the concrete problem and resulting behavior, or with the new capability when the evidence shows no prior failure. One paragraph is the default. A second paragraph requires a separate compatibility impact, failure boundary, or supplied verification result, and test changes alone do not establish that checks ran.

## Native `gh` execution

Every GitHub read and write runs through one `GhClient` owned by `ForgeRuntime`. Its directory-bound `GhDirectory` handle exposes the issue, comment, and review capabilities without allocating another pool. At most four `gh` processes run concurrently across all checkouts, and each request passes explicit hostname routing and GraphQL variables through an owned temporary input file.

Each process keeps its complete stdout and stderr under a 120-second deadline. `process-wrap` contains the command in a Windows Job Object or a Unix process group, and cancellation or deadline expiry terminates that container, awaits exit, and drains both streams before releasing admission. Windows `KillOnDrop` also covers an unexpected host exit. A Unix process group has no equivalent guarantee after forced host death, and Git can leave a repository lock after forced termination.

Decoders preserve complete returned collections without count limits and treat an empty collection as success. Missing fields, malformed JSON, invalid pagination, GraphQL errors, and command failures remain errors, with categories for rate limits, missing repositories, authentication, and local Busy admission. Failure diagnostics keep at most 64 KiB of UTF-8 and mark truncation. That budget applies only to diagnostics, never to successful content.

## Repository storage and leases

Repository data lives under `stdpath("data")/forge/github/<host>/repos/<owner>/<repo>/`. `forge-github::RepositoryLease` coordinates processes through lock files that sit beside the repository cache directory, so deleting and recreating the directory cannot split cooperating hosts across different lock files. Ordinary operations take a shared lease, deletion takes an exclusive lease, and an incompatible request reports Busy immediately.

`GithubService` admits at most 8 blocking storage jobs and 16 asynchronous remote jobs. It reserves repository admission before submitting work, retains admitted jobs after a caller drops its receiver, and drains them at shutdown. Remote waits never hold a database handle, so storage transactions proceed while a sync waits on the network.

Deletion runs through the `delete_cache` operation of `github.issue.list`. Rust acquires exclusive ownership, rejects redirected paths, closes database handles, and removes the repository directory. A removal failure reports the error and can leave partial deletion without automatic retry. Lua's `repo_cache.delete_current` deletes the requested repository and any distinct mapped repository in sequence, stops at the first failure while preserving the cwd mapping, and reports the completed count. It invalidates a loaded snapshot only after success and only when the cache namespace still matches the deleted path.

## Issue synchronization and completion

`issue_index.sync_repo` sends one typed `github.issue.sync` request, and `GithubService` owns pagination, freshness, rate-limit delays, page persistence, and leases. The sync job holds a shared operation lease and an exclusive sync lease across remote reads, retry delays, page commits, and snapshot publication.

Initial history resumes the persisted cursor. Once history is complete, refresh starts at the newest page, includes closed issues in both open and all scopes, and stops at the high-water mark fixed before the refresh. Each page of at most 100 records commits its cursor, high-water state, and an exact revision in one transaction, then publishes an open completion snapshot. A failed later page preserves earlier progress, and a repeated cursor fails before commit. Rate-limit failures receive at most three retries with sixty-second timers, normal page spacing is 150 milliseconds, and each read has a 120-second deadline. A non-manual refresh skips remote reads only when its scope history is complete, its snapshot exists, and its last check is under ten minutes old.

A process can die between a page commit and snapshot publication. Every preload therefore first sends `reconcile_snapshot`, which compares the published repository, filter, revision, and row count against committed state while holding the database handle, and rebuilds a missing or mismatched file without contacting GitHub. A database without a committed page reports not ready rather than publishing an empty success. Lua adopts only the reconciled revision, retries concurrent file replacement at most three times, and keeps prior records on failure.

Lua coalesces sync requests by database path, rejects a resolved hostname that differs from `repo_cache.hostname()` before dispatch, and finishes a request only after the snapshot preload completes. Manual sync shows progress immediately, automatic sync shows it once the host reports work, and a freshness skip stays silent. Opt-in progress arrives as `RequestEvent` values carrying only the latest phase, so slow observers can miss intermediate phases, and the correlated final response stays authoritative. `#` completion filters the adopted snapshot synchronously and never issues a storage request, disk read, or GitHub search per keystroke.

## Issue details and repository metadata

`github.detail` reads one issue through `gh issue view`, persists it under the repository operation lease, and returns a normalized `DetailRecord`. The decoder preserves every comment, label, assignee, and project item and rejects mismatched repository or issue URLs. CLI issue comments export no update timestamp, so `updated_at` stays empty. Lua keys detail memory by database path, shares one pending fetch per issue, may show a cached detail while refreshing, and never repeats the remote read or its persistence.

The native issue buffer is named `forge://issue/` and uses `acwrite`, so `:write` produces an explicit immutable field capture through BufWriteCmd. Editing is enabled only inside native editable regions, typing sends no request, and closing a dirty buffer hides it so reopening the issue restores the unsaved text.

`github.metadata` merges contributors and collaborators from paginated `gh api` reads into a case-insensitive map of unique logins. One failed source still yields metadata with its diagnostic retained, while failure of both preserves the previous file. A metadata job holds the shared operation lease plus an exclusive refresh lease, honors a ten-minute freshness window that excludes future-dated records, and atomically replaces `metadata.json` through a flushed and synced temporary file. Invalid cached JSON or identity reports a failure instead of an empty result or a destructive repair. Lua reads the cached file through `repo_cache.ensure_metadata` and never writes it.

Completed responses that exceed one transport frame use request-correlated segmented delivery. Frame size bounds each chunk, never the total response.

## Remote mutations and uncertainty

`RemoteQueue` orders every write by normalized hostname, repository, and issue or PR number. It admits at most 64 operations and retains at most 64 resource identities, with FIFO order per resource and independent progress across resources. A running operation keeps ownership across multi-step transitions, such as reopen followed by draft. `GithubService.run_mutation` owns queue admission, task retention, and shutdown for every typed mutation adapter, and it keeps active writes after their response receivers drop.

A write whose outcome is unknown marks its resource uncertain under an evidence scope: the pull request, one comment node, or one creation operation. Any unresolved scope blocks later writes to that resource. Only a read-only reconciliation in the matching scope clears it, so a title observation cannot settle a comment write. Mutations echo a bounded `clientMutationId` that the response must return. This correlates the immediate response but provides no server-side idempotency. Only a proven pre-dispatch failure counts as a rejection, and every other failure, including malformed success output, is reported as unknown. No mutation retries automatically. Uncertainty is process-local, and the review service persists submitted captures through its recovery store.

## Pull request lifecycle and fields

`github.pull_request` binds each lifecycle transition to the normalized repository, number, and expected node identity. Rust reads current remote state before choosing at most two writes. Merged PRs reject lifecycle changes, an already satisfied target performs no write, and a proven second-step rejection returns the confirmed intermediate state. After an unknown result, Lua requests exactly one read-only reconciliation, reports the original failure, and shows the observed state when one is available.

Title and body edits travel as optional fields of the same route. Omitted fields stay unchanged, an empty body clears the description, and blank or multiline titles and NUL bytes are rejected. Matching remote text yields a confirmed no-op. Otherwise one `updatePullRequest` mutation must confirm identity and the submitted values, and reconciliation reports `matches_submission` without reposting.

## Review documents

`ReviewService` holds at most 64 open PR documents and admits at most 32 asynchronous jobs, using the `GithubService` queue for every remote operation. Each `ReviewDocument` owns an `EditStore` of accepted field text and saved baselines that is independent of buffer layout. A region edit carries document, region, revision, and sequence identities, and a stale or conflicting edit leaves the field unchanged. Document identities include a service UUID, so a request from an old host cannot address a replacement host's document.

Neovim owns unsaved typing, and no per-keystroke request exists. `<C-s>` or `:w` captures the exact native field text, including CR characters and trailing empty rows, as an immutable save. Confirmation advances only the submitted baseline, so text typed after the capture stays dirty. An unknown outcome holds later publication until `github.review.document.reconcile` reads remote truth, and a failed observation keeps the capture and exposes `gR` to retry recovery. Lua runs one active publication and keeps at most one queued explicit action, and a newer queued action supersedes the previous one.

Opening a PR keeps the current buffer until the title, metadata, and description are ready, then publishes them together with `Loading...` rows for pending sections. Files, checks, conversation, and commits load concurrently, and a failed section shows its diagnostic without hiding the others. Lua retains at most eight PR documents with their buffers, keyed by workspace, hostname, repository, and number. Reopening shows retained text immediately and refreshes clean fields while preserving local edits. Editable regions reject cross-boundary edits by restoring the retained text and extmarks on the next main-loop callback without sending the invalid edit to Rust.

Requested reviewers are an editable field whose additions and removals Rust applies after the title and body save. Rust validates reviewer names again before any field write and before the reviewer phase, so a self-review request cannot cause a partial save. GitHub exposes no reviewer-operation identity, so reviewer recovery closes the unknown operation after observing the current reviewer set.

## Comments

`CommentStore` gives each remote comment or local draft a stable `CommentId` and `EditStore` region, and gives each displayed copy a separate `CommentOccurrenceId`. Focus selects exactly one occurrence, and removing an occurrence never discards its body or reply draft. Cursor entry promotes only an inline viewer-authored comment to an editor, while an explicit open can expand a read-only or conversation occurrence.

A remote update that differs from local text keeps a conflict candidate beside the local text and baseline. KeepLocal adopts the remote baseline and preserves the draft, TakeRemote replaces the text, and typing the candidate also resolves the conflict. Pending saves reject refreshes, and unresolved conflicts reject saves.

A save captures create, edit, reply, or delete intent with the current body, and one pending operation per comment prevents repeated posting. Edit and delete read the remote owner first and require its exact node, database identity, and PR-scoped URL, and an already-matching body skips the write. Creation verifies the parent PR node before dispatch, and confirmation requires the echoed receipt, the expected subject, viewer ownership, and the exact body. A lost creation response stays unresolved, because neither an absent comment nor a matching body proves what happened. Confirmed deletion keeps the native text available for recovery. The `gR` picker offers remote observation, a linked confirmed comment ID, not-dispatched rejection, or close-unknown.

Browser actions resolve an explicit comment occurrence or source anchor. Adjacent code never inherits a nearby comment URL, and an unsaved draft has no remote URL.

## Batched review

A `ReviewDocument` is in `Overview` or `Batched` mode. Batched mode adds an editable review summary and a viewed-file set, both stored in the PR-scoped durable review draft and restored before publication. Lua changes mode, viewed state, and submission only through `github.review.document.batch.begin`, `github.review.document.file.viewed.set`, and `github.review.document.batch.submit`.

A submission captures the summary and every local inline draft with its revision, path, line range, and side, and sends them in one review request. The operation ID and complete capture reach durable storage before dispatch. An unknown outcome survives reopen, blocks another submission, and requires explicit recovery through `gB`. Recovery validates the retained capture against the recovery journal, and a missing journal cannot establish confirmation.

Commit detail reads resolve a full commit SHA to its first parent, and expansion compares those two commits. A root commit has no comparison base and shows an explicit unavailable state. `CommitDiffStore` keys expanded rows by commit SHA and path, separately from pull-request file state, and retains at most 64 records and 16 MiB for reuse. A larger diff still renders completely from its response.
