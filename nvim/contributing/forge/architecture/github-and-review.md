# GitHub storage and review

The shared host owns remote operations, durable GitHub data, submitted review state, and reconciliation. Lua owns unsent fields, local comment drafts, and explicit save captures until the host acknowledges their revisions.

Read [Buffers and rendering](buffers-and-rendering.md) for editable-region application and [Host and protocol](host-and-protocol.md) for request ownership.

## Pull request creation context

The creation workflow retains the selected base branch and the head commit captured
by `github.creation.context`. When `origin` is configured, it fetches only the
selected base branch into its tracking ref and pins that commit before collecting
the title and diff. Git reads and the fetch have a 30-second deadline. Fetch failures
stop creation without reusing stale refs. Without
`origin`, it resolves the local base branch. The first head-only commit supplies the title.

The `context` operation on `repository.generate` compares the merge base of those
two pinned commits with the captured head. It reads committed objects only, so
base-only changes and staged, unstaged, or untracked work do not enter the prompt.
Before reading diff metadata or source excerpts, the native comparison runs
`git merge-tree --write-tree --quiet` on the pinned base and head. Exit status 1
returns `conflicts` without prompt text, and Lua stops with "Merge conflicts.
Resolve them before creating the PR." No model request, branch push, or PR
creation follows. Other command failures remain errors. This check changes no
branch, index, or working file, though Git can write unreferenced merge objects.
Missing refs, unrelated histories, and context failures stop generation and publication.
Transient invalidation retries retain the same commits and stop after three attempts.

PR descriptions use the same native compact formatter as commit messages. Renames,
license changes, and lockfiles remain in the file list, and declaration excerpts
retain the shared context and omission rules. The formatter receives the remainder
of the 500,000-byte allowance after the PR instructions and comparison metadata.
The PR prompt uses its own description instructions without the commit-message header.

The `pull_request` operation on `repository.generate` sends the prepared evidence to
`HarnessService.background`. Lua reads the same saved Background model and thinking
settings used by commit generation. Both Codex and Copilot default to `gpt-6-luna`
with `low` thinking. PR descriptions and commits reuse the same persistent provider
conversation and compaction policy. Every exchange carries its own output requirements,
and generation rejects a HEAD that differs from the captured PR revision. The shared
Background owner serializes requests and retains provider cleanup independently of chat.
The request returns provider failures to the creation workflow before publication.
Lua never launches Copilot CLI or calls a model HTTP API for PR generation.

Descriptions lead with the concrete problem and resulting behavior, or the new
capability when the evidence establishes no prior failure. One paragraph is the
default. A second requires a separate compatibility impact, failure boundary, or
supplied verification result. Changes to tests do not establish that checks ran.
Branch publication retains the same captured head used for generation.

## Repository storage and deletion leases

`forge-github::RepositoryLease` uses shared filesystem locks for issue operations and an exclusive
lock for deletion. The lock file is a persistent sibling of the repository cache directory, so
removing and recreating that directory does not let cooperating hosts lock different files. Each
IssueStore operation retains its lease until the database and any publication temporary file close.
The lease API reports Busy immediately when incompatible ownership exists.

`GithubService` reserves repository admission before submitting a blocking job. Deletion admission
requires no earlier admitted operation for that directory and rejects new operations until its
worker finishes. The worker owns admission independently of the request receiver. Cross-process
ownership is enforced separately by the filesystem lease.

The typed `delete_cache` storage operation accepts the current `issues/issues.redb` layout. It
resolves the repository directory, rejects redirected issue/database paths and existing sync locks,
checks that redb can open, closes the database, and removes the resolved directory under its deletion
lease. A held legacy redb handle produces an error without archiving or deleting the database.
Removal failures report the error and can leave partial deletion. No automatic retry occurs.

Lua deletion commands have not switched to this operation. Lua remote sync does not yet retain a
host lease across remote requests, and the legacy sync-lock existence check does not atomically
exclude a concurrently starting legacy client. That lifecycle cutover remains required before the
user command can claim coordinated deletion. Repository hostname and canonical admission identity
also remain acceptance work.

## Owned issue synchronization

`GithubService::sync` retains one shared repository operation lease and one exclusive sync lease
across remote reads, retry delays, page commits, and snapshot publication. Both lease files remain
outside the repository cache directory. The service admits at most two async sync jobs separately
from its eight blocking storage jobs. Remote waits retain no redb handle, so state and detail
transactions can proceed while deletion and another cooperating sync remain excluded.

Initial history resumes the persisted cursor. Once history is complete, refresh starts at the newest
page and includes closed issues for both open and all scopes. The fixed pre-refresh high-water mark
stops incremental pagination. Each page commits its cursor and high-water state, then publishes an
open completion snapshot. A failed later page preserves earlier committed progress. A repeated
cursor fails before committing that page. A non-manual refresh skips remote reads only when its
scope history is complete, its snapshot exists, and its last check is less than ten minutes old.

The loop accepts at most 100 records per API page and follows nonempty, advancing cursors until
history completes or reaches the high-water mark. It imposes no total page or cursor-byte cap.
Rate-limit read failures receive at most three retries with sixty-second timers. Normal
page spacing is 150 milliseconds. Exhausted page rate budgets wait sixty seconds. Each remote read
has a 120-second deadline. Close notification takes priority over read or timer progression.

Dropping the caller leaves admitted sync work owned by the service. Shutdown closes admission,
interrupts remote waits and timers, and drains both task collections under the existing deadline.
Blocking commits keep independent operation ownership. A remote implementation must retain native
process cleanup independently of a dropped read future.

`GithubRepositoryId` validates and normalizes hostname, owner, and name without changing existing
cache paths. Sync validates issue identity, state, URL host/path, update presence and ordering, and
page cursor progression before commit. This is not complete remote schema or global allocation
validation. `GithubRemote` is currently an injectable issue-read contract. The concrete gh process
client, host sync route, Lua consumer cutover, progress events, and coordinated deletion command
remain pending. Production remote sync still uses the Lua implementation until those paths switch.

## Owned gh issue reads

`GhClient` supplies the concrete `GithubRemote` issue-page implementation. It invokes gh directly
with explicit hostname routing, inherited authentication, and JSON request variables stored in an
owned temporary input file. Initial open scope requests open issues. All scope and incremental
refresh request open and closed issues. The query preserves the existing hundred-issue page and
fifty-label limits and excludes issue bodies.

The client reuses forge-git's retained blocking read pool and command runner. At most four native
requests execute concurrently, and each retains complete request input, stdout, and stderr through
a 120-second execution deadline. Total content bytes do not control admission.
A blocking worker drives Tokio process I/O through the existing runtime. `process-wrap` contains
each command in a Windows Job Object or Unix process
group. Cancellation and deadlines terminate that job or group, await its exit, and collect both
streams before releasing admission. Windows `KillOnDrop` also terminates the job if the
Rust host exits unexpectedly. Unix process groups need a surviving owner to receive cancellation,
so forced host death has no equivalent guarantee. Git can leave a repository lock after forced
termination, which requires separate recovery.

GraphQL decoding preserves complete returned issue, label, and error collections. An empty
issue connection succeeds. Missing fields, malformed JSON, invalid pagination, GraphQL errors, and
command failures remain errors. Error categories preserve rate limits, missing repositories,
authentication failures, and local Busy admission. Diagnostics preserve UTF-8 within a 64 KiB limit
and mark truncation. This diagnostic retention budget does not reject or truncate successful content.

Native tests compile a local Rust gh fixture and execute the real client against it. They verify
arguments, request-file cleanup, Unicode records, cancellation and child reaping, admission overflow,
large output, stderr classification, and retained descendant pipes. The fixture performs no
network requests. Unit tests verify empty results, missing fields, API failures, complete collections,
and diagnostic truncation. The client is not yet registered with host runtime ownership or exposed
through a sync route, so production Lua consumers have not switched.

## Host sync dispatch and request progress

ForgeRuntime now retains the concrete GhClient and includes native gh ownership in shutdown
reporting. Checkout contexts share that single four-request pool instead of allocating independent
admission budgets. Context creation binds the working directory without launching a process.

The `github.sync` host method accepts a database path, checkout directory, typed SyncRequest, and
optional progress flag. It routes through GithubService without initializing Harness. Unknown
request fields and malformed remote identity fail decoding. The final response reports freshness,
fetched records, and published page count. Successful sync releases its repository ownership before
a later delete-cache request runs.

Opt-in progress uses RequestEvent with request_id, event, and payload fields. Sync reports reading,
indexing, publishing, rate-limit waits, completion, and freshness through a watch channel holding
only the latest value. Slow observers can miss intermediate phases. The correlated final response
remains authoritative, and no progress is emitted after it. Request events do not require or imply
a Harness session identity.

The Lua host client accepts an optional progress callback for request_host. It delivers matching
events only while that request remains pending. Unknown, mismatched, and late request events do not
reach Harness subscribers. Invalid request-event shapes fail decoding. GitHub host failures no
longer invalidate unrelated Harness state. Callback failures are reported without completing or
removing the pending request.

Executable tests use the actual Forge host and a local native gh fixture. They verify sync,
freshness, progress correlation, strict parameters, deletion after sync, and shutdown while a remote
read is blocked. The terminal shutdown response follows direct-child reaping. Library tests verify
that separate checkout contexts cannot exceed the shared native admission budget. Production
issue_index.sync_repo still uses its Lua orchestration pending the consumer cutover. Complete
descendant termination and global allocation accounting remain open.

## Repository cache deletion through the host

Repository deletion now uses the shared host's `github.issues` delete_cache operation. Rust acquires
exclusive repository ownership and closes database handles before removing repository data. Lua
retains the loaded issue snapshot while the request is pending or rejected. A successful host result
invalidates the corresponding local snapshot, including an already-absent repository result.

repo_cache.delete_current sequences the requested repository and any distinct mapped repository.
Its callback reports the completed deletion count and an optional failure. The first failure stops
the sequence and preserves the cwd mapping. The command displays success only after the callback.
If an earlier repository was already removed before a later failure, the returned count preserves
that partial completion. Cwd removal errors notify and return failure instead of reporting success.

The deletion sequence captures its cwd and cache namespace. Before advancing, it checks that the
namespace and cwd mapping still match. A changed context stops further deletion and preserves the
new mapping. Direct repository deletion invalidates the snapshot only if the current namespace
still matches the path sent to Rust. The stale-repository sync failure path also uses host deletion.

Lua fixtures verify delayed completion, Busy rejection, unchanged path routing, snapshot retention,
successful invalidation, and a changed cwd mapping. The issue-index fixture exercises the real host
against a real database. An active Lua sync lock rejects deletion without clearing local state.
After lock release, the same API removes repository data and the cwd mapping, invalidates completion
records, and shuts down cleanly. Production issue sync, metadata writers, and other GitHub consumers
have not yet all moved behind Rust ownership, so the complete GitHub migration gate remains open.

## Production issue sync consumer cutover

issue_index.sync_repo now submits one typed github.sync request to the shared host. Lua no longer
owns the issue-page query, GraphQL response parsing, pagination, high-water traversal, freshness
policy, rate-limit delays, page persistence, or filesystem sync locks. GithubService owns those
transitions and retains them through caller cancellation and runtime shutdown.

Lua keys pending sync context by the database path. Duplicate requests in that context do not reach
the host. Each context retains its checkout directory, repository, snapshot path, progress state,
and optional completion callback. The final callback accepts a successful SyncOutcome only after
the snapshot preload completes. A host error, malformed outcome, failed preload, or changed cache
context produces a failure result and notification. Late host responses and progress cannot finish
the same context twice. Test reset retires local context without claiming native cancellation.

Manual sync starts progress immediately. Automatic sync starts progress when the host reports work
or returns a refreshed outcome. A ten-minute freshness skip stays silent. Coalesced progress maps
native phases and counts to the existing notification UI. Invalid progress reports a diagnostic
without replacing the final host response. A failing notifier cannot prevent request completion.

Hostname resolution preserves the explicit issue-index override and Git remote lookup. Lookup
failure remains a failure. Before dispatch, the resolved host must equal repo_cache.hostname(),
which owns the current cache namespace. A mismatch fails before any remote request or database
write. This protects the current path contract while complete repository identity propagation
through other GitHub consumers remains unfinished.

The issue-index fixture runs the actual Forge host with a compiled local gh fixture in its child
search path. It verifies native issue sync, durable snapshot publication, synchronous completion
search after preload, automatic freshness, deletion ownership, and clean shutdown without Harness
initialization. Mocked boundaries cover scopes, duplicate admission, malformed and failed responses,
exceptions, missing repositories, progress phases, notifier failure, hostname lookup failure,
host/cache mismatch, and cache-context changes. Other UI fixtures now inject the host sync result
instead of recreating a Lua GraphQL backend. Detail fetching and metadata writers remain separate
Lua consumers awaiting their service cutovers.

## Native issue details and retained persistence

The native issue adapter gives its retained buffer a `forge://issue/` name and uses `acwrite` ownership.
Neovim therefore routes `:write` through BufWriteCmd into an explicit immutable field capture. Cursor
and insert transitions enable editing only inside a native editable region. Projection delivery
rechecks that boundary, so generated headings and comment presentation stay protected while issue
fields remain editable. Typing sends no host request. Dirty close hides the buffer without saving,
and reopening the same issue restores that buffer with its current unsaved text.

GithubRemote now exposes typed IssueDetailRequest and IssueDetail results. GhClient routes issue
detail reads through `gh issue view` with an explicit hostname-qualified repository. Detail and page
requests share one native command runner and the same four-request admission pool. Processes retain
complete output with a 120-second execution deadline. Failure diagnostics retain up to 64 KiB and
mark truncation independently of successful content. Cancellation
retains admission until child exit, pipe drainage, decoding, and result collection finish.

The detail decoder consumes the CLI's current exported schema and preserves every returned comment,
label, assignee, and project item without collection-count admission limits. Missing
required fields, mismatched repository or issue URLs, unsupported states, and invalid comment
anchors fail decoding. Empty or null CLI collections remain valid empty collections. Normalization
preserves bodies, metadata, project status, and comment source URLs. CLI issue comments do not
export an update timestamp, so the normalized updated_at remains empty, matching the former Lua
representation. Complete timestamp validation and global allocation accounting remain open.

GithubService.fetch_detail shares the sixteen-job asynchronous admission bound with sync and review reads. The owned job
acquires a repository operation lease before reading remotely and retains it through detail
persistence. Remote waits hold no database handle. Dropping a caller leaves admitted work owned.
Shutdown cancels pending remote reads and drains retained jobs. Service repository admission is
released before delivering the terminal result, allowing a subsequent deletion to observe completed
ownership.

The github.detail route works before Harness initialization. It returns a normalized DetailRecord
after persistence. The route shares the completed-storage response encoder with github.issues.
Section 78 defines segmented delivery for results above the transport frame limit.

Lua detail fetching now calls github.detail and does not repeat remote execution or persistence.
Cached detail display and stale refresh remain available. Memory keys include the database path to
isolate cache namespaces. Matching requests share one pending fetch. Invalid, failed, or stale
responses notify and fail their waiters. A throwing waiter does not prevent other callbacks from
completing. Successful repository deletion invalidates both snapshot and detail memory and retires
local pending detail callbacks without claiming cancellation of already-owned native work.

Tests cover normalized fields, empty and large collections, source anchors, native
argument routing, shared page/detail admission, cancellation and child reaping, retained service
ownership after caller drop, shutdown, persistence, and oversized host responses. The Lua native
fixture proves remote fetch, persisted reload after memory eviction, and detail invalidation after
deletion. Metadata writers, remote mutations, complete identity propagation, descendant process
termination, and the broader Forge migration gates remain unfinished.

## Repository user metadata ownership

The github.metadata route owns contributor and collaborator reads and metadata.json publication
without initializing Harness. It uses the same sixteen-job service admission bound as issue sync and
detail fetches. Native commands share the existing four-request pool. Explicit hostname arguments
select the API host independently of the checkout's default repository.

GhClient requests contributors and collaborators with per_page=100, --paginate, and --slurp. The
typed decoder preserves all returned pages and users. User logins must be nonempty and contain no
whitespace or controls. Logins and optional names have no byte-admission cap.
A sorted, case-insensitive map merges duplicate logins and fills a missing name from the other
source, retaining every unique user. Empty pages are successful empty results. Malformed records
remain failures. A successful source can supply metadata when the other fails, with the failed
source's diagnostic retained in the metadata failure list. Failure of both sources returns an error
and preserves previous metadata.
Serialization omits absent names so cached Lua completion retains its repository-text fallback.

MetadataStore validates the existing hostname/repos/owner/name path suffix and the record identity.
It does not rename or relocate legacy data. Legacy records without a hostname remain readable under
that validated path. A ten-minute cache hit returns without issuing a remote request. Future-dated
records are not fresh. Invalid cached JSON or identity returns a visible failure rather than an
empty success or an implicit destructive repair.

One retained metadata job owns both shared repository operation admission and an exclusive metadata
refresh lease. Another metadata refresh receives Busy, while issue sync and detail reads can use
their own leases. Deletion cannot proceed until remote work and publication release ownership.
Dropping a request receiver leaves admitted work owned. Shutdown cancels remote waits and drains
retained blocking publication. No database handle spans the remote wait.

Publication validates the result, streams complete JSON through a reusable 64 KiB buffer,
flushes and syncs the temporary file, and atomically replaces metadata.json. Serialization or flush
failure leaves the prior file unchanged and removes temporary output. The buffer controls write
batching, not total file size. The host uses segmented result delivery if the completed response
exceeds one transport frame.

Lua repo_users now adapts requests to the host. The two former gh contributor methods and Lua user
pagination, merge, and metadata-write implementations are removed. Status, issue, and notification
consumers use repo_cache.ensure_metadata. Cached reads retain their current interface and cwd
mapping behavior. Pending requests coalesce by cache directory. Callback validation rejects a
changed cache context or foreign identity, and repository deletion retires pending local metadata
context. Error paths release local admission and report refresh failure. Lua does not write the
host's metadata result back to disk.

Tests cover source merging, partial and total failures, invalid and large pages, retained
ownership after caller drop, shutdown, old-format cache reuse, identity rejection, publication
of complete large metadata, and temporary-file cleanup. The native Neovim fixture fetches merged
users through the actual host, observes persisted metadata before callback completion, and removes metadata through
coordinated repository deletion. Remote mutations, other feature services, process-tree cleanup,
global memory accounting, and the remaining migration gates are still open.

## Completion snapshot revision reconciliation

A process can exit after a page commits and before its completion snapshot is published. Each
page transaction now advances an exact wire integer revision with its issue and sync records.
Snapshot construction reads that revision and its rows in one read transaction. Detail-only
writes do not advance it. Revision exhaustion rejects the entire page transaction.

IssueStore.reconcile_snapshot retains the repository operation lease and database handle while
checking the published repository, filter, revision, and row count. A missing or mismatched file
is rebuilt from committed records. A database without a committed page reports ready=false and
does not publish an empty success. Fresh automatic sync repairs the file before freshness can
skip remote work. The shared publisher streams complete JSON through a reusable 64 KiB buffer,
flushes and syncs the temporary file, and replaces the destination atomically.

Production Lua preload and watcher reload reconcile inside the existing four-active-request
admission bound. Concurrent callers coalesce, and callback identity checks reject changed cache
contexts. Lua verifies the decoded revision before adopting records. Concurrent file replacement
uses the existing three-attempt bound. Failures preserve prior records and report failure to the
caller. Synchronous completion performs no storage request or disk read.

Lua retains reusable snapshots within a 32 MiB cache and a sixteen-repository cache budget.
A snapshot larger than that byte budget, or blocked from cache admission by pending readers,
remains available through one separately retained active snapshot. Eviction controls reuse,
not which complete issue content completion can consume.

The two-process fixture kills the writer after its database commit and observes repair by the
surviving host. A second reconciliation leaves the repaired file unchanged. The native Neovim
fixture verifies recovery without another gh request. Unit fixtures cover revision exhaustion,
invalid publication headers, invalidated callbacks, and revision changes during preload.

Database commit and file publication remain separate operations. Recovery establishes consistency
at its observation point, and a later writer can advance the database again. Header validation
counts rows without retaining their contents and does not validate every issue field. These
checks do not establish global allocation bounds, macOS behavior, or power-loss durability.

## PR lifecycle mutation ownership

Concurrent PR lifecycle requests can interleave reopen and draft conversion, and a failed response
can follow a successful remote write. The github.pull_request route owns both sequencing and
outcome classification. Each transition binds normalized hostname, owner, repository, number,
and expected node identity. Rust reads current remote truth before selecting at most two writes.
Merged PRs reject lifecycle changes, and an already satisfied target performs no write.

RemoteQueue admits at most 64 operations and retains at most 64 resource identities. Clones share
FIFO admission by hostname, repository, and issue/PR number. Distinct hosts and numbers proceed
independently. A running operation retains ownership across reopen and subsequent draft or ready.
Dropping a queued operation removes it without releasing the active owner. Dropping a write owner
after begin_mutation marks its resource uncertain. Unknown completion rejects waiting mutations
and retains the resource slot until a successful reconciliation. Existing uncertain resources can
still reconcile when all 64 resource slots are occupied.

GithubService retains mutation jobs independently of request receivers. Closure rejects waiting
work and drains active work without dropping a possible native write. Native reads and writes
share the four-job gh process pool. Each command retains the existing 120-second deadline and complete
process output. Formatted failure diagnostics retain at most 64 KiB. The worker collects process
termination and pipe drainage before the mutation future completes. Native admission rejection proves no write began.
Other native failures conservatively report OutcomeUnknown, including malformed successful output.
No state mutation retries automatically. A proven second-step rejection returns the last confirmed
remote state, while unknown results expose no claimed current state.

Lua github.pull_request adapts host results and guards callback delivery and hostname changes.
After an unknown result, host transport failure, or malformed response it requests exactly one
read-only reconciliation, reports the original
failure, and supplies the newly observed state when available. Reconciliation failure preserves
uncertainty and does not fabricate a state. A changed hostname prevents automatic reconciliation
against a different namespace. pr_edit checks that the buffer still owns the same
PR before applying the callback. The previous Lua lifecycle GraphQL helpers are removed. The
existing chooser and local edit queue remain presentation consumers.

Queue fixtures cover FIFO ordering, independent resource identities, cancellation, closure,
capacity recovery, and uncertain-resource retention. State-machine fixtures cover merged and
foreign records, no-op transitions, reopen ordering, and confirmed partial failure. Native gh
fixtures persist a changed PR before returning malformed output, demonstrate explicit recovery,
and retain a blocked writer after caller cancellation and shutdown. The stdio fixture exercises
this route without initializing Harness, and Lua fixtures preserve chooser, cache, notification,
and no-repost assertions.

Ordering is process-local. Other Lua remote writers and other Forge processes remain outside this
queue. Uncertainty is not persisted across host restart, although every lifecycle transition reads
remote truth before writing. Cross-process mutation receipts, comment/review submission ownership,
complete descendant termination, and global allocation acceptance remain open.

## PR text edits and submitted baselines

PR title and description writes share the Rust resource owner used by lifecycle transitions.
The github.pull_request edit operation captures optional title and body fields. Omitted fields
remain unchanged, and an empty body clears the description. PullRequestEdit rejects an empty
submission, blank or multiline titles, and NUL bytes. Valid title and body content has no total-byte
admission cap. Logical requests preserve their complete encoded content, while completed responses
can span multiple transport frames.

Rust reads PR identity and editable text before choosing whether a write is needed. Matching
submitted fields return a confirmed no-op. A changed field uses one updatePullRequest mutation,
and the response must confirm both identity and the submitted values. A mismatched or malformed
response is uncertain and never triggers another write. ReconcileEdit reads current text and
returns matches_submission without returning or reposting the body. General PR reconciliation
also reads editable text so it cannot release an uncertain text edit using lifecycle data alone.

RemoteQueue retains at most 64 concurrent operations and 64 resource identities. It accounts for
submitted string capacity through queue waiting and completion without rejecting requests by total
input bytes. The native pool independently limits execution to four processes and preserves complete
encoded queries and launch context. These concurrency bounds do not cap total field content or
establish a complete global allocation proof.

Lua github.pull_request.request_async copies the submitted request before dispatch and adapts
both lifecycle and edit operations. Uncertain edits request one read-only comparison against that
captured text. A matching reconciliation advances the submitted baseline while still reporting
the original delivery failure. A mismatching or failed reconciliation leaves the text unconfirmed.
The old Lua gh.update_pr_async helper and its subprocess construction are removed.

The PR editor updates only fields from the completed submission. It recomputes dirty markers and
the modified flag from the current native buffer before deciding whether to render. If a newer
title, description, reviewer, or milestone edit exists, the save completion preserves those rows.
A changed document identity rejects the callback. Failed remote operations still notify after
the originating buffer closes. The UI regression fixture submits one title, types a newer title
and description while the callback is held, and verifies that the older
completion updates its baseline without replacing either newer field. The next save commits
those fields and clears their markers.

Native tests verify Unicode, quotes, indentation, trailing newlines, omitted fields, empty-body
clearing, no-op writes, shared ordering with lifecycle changes, and text recovery after a write
persists but its response is malformed. The host fixture verifies the edit receipt over stdio.
These changes do not implement the planned Rust ReviewDocument or EditStore. Their document,
region-revision, comment, reviewer, and milestone ownership work remains open.

## Review edit store and save capture lifetimes

The forge-review crate owns accepted field text and saved baselines independently of generated
document layout. RegionEdit carries document identity, region identity, region revision, a positive
edit sequence, and exact UTF-8 text. Stale sequences, conflicting revisions, invalid text, exhausted
counters, and failed admission leave the field unchanged. The buffer integration test inserts
generated rows before accepting a Unicode multiline field edit and verifies matching region
acknowledgements from EditStore and BufferDocument despite their different layout revisions.

prepare_capture validates all fields and reserves their text without changing the store. Its proposed
snapshots let a document prepare its buffer projection before committing either state. Dropping the
prepared capture releases its reservations. Issue captures validate the proposed title and assignees
before projection preparation, so scalar validation and missing projection blocks cannot partially
adopt another field in the same capture.

begin_save retains immutable submitted text, revision, and sequence. Confirmation advances only
that submitted baseline, so newer local text remains dirty. A pending write followed by a local
revert still admits a compensating submission. Callers must execute writes in submission order.
Completion delivery may arrive out of order without replacing a newer confirmed baseline.
Rejected writes preserve the baseline. Uncertain writes retain their capture and prevent further
save admission until an explicit remote observation reconciles the field. Reconciliation preserves
local text, and a superseded observation cannot replace a newer confirmed baseline.

The default per-document save admission allows 64 pending saves. Fields and region lifetimes have
no arbitrary count limit, and field text has no byte-admission cap. Retired region identities cannot
be reused during the document lifetime. A shared EditBudget accounts for retained UTF-8 text bytes
across stores and submissions without enforcing a total-content budget.
Current text, baseline text, and captures share allocations when their content matches an existing
current or baseline value. A retained submission keeps its charge after its store closes. Admission
accounts for new text before changing field state, including the transient overlap with replaced text.
The accounting excludes allocator overhead and metadata and does not establish global memory acceptance.

Production ReviewDocument uses EditStore to retain explicitly captured text and confirmed baselines.
Neovim owns unsaved typing. Save, comment publication, and batched submission carry immutable captures
into the admitted service task. The router does not adopt captures separately, and rejected concurrent
operations leave the document unchanged. No per-keystroke acknowledgment participates in editing.

## Host-owned review field lifecycle

ForgeRuntime owns ReviewService alongside GithubService. The review service holds a registry of
at most 64 PR documents, admits at most 32 asynchronous jobs, and shares retained-text accounting
across fields without a total-byte admission limit. Open, save, and reconciliation use the existing
GithubService resource queue.
They do not create another GitHub process pool or mutation queue. Completed job handles remain
owned until collection. Runtime shutdown closes review admission and joins its jobs with the
remaining shared owners.

review.open_pr accepts a directory and validated PR target. It reads identity and editable text
through GitHub reconciliation before allocating a document identity. The initial title and body
baselines come from that remote observation. A failed or abandoned open releases its document
admission. Document identities do not repeat within a host lifetime. The current ReviewDocument
owns title and body fields and their save batch. PR rendering, comment ownership, and its planned
BufferDocument composition remain migration boundaries.

Explicit review saves carry immutable full-field captures with document identity, region revision,
and sequence. Typing remains in Neovim and sends no per-edit request. The host validates the capture
before submitting title and body in one updatePullRequest mutation. Omitted clean fields remain
unchanged. Confirmation advances captured baselines. A remote panic or delivery error retains
explicit uncertainty rather than implicitly replaying the mutation.

review.reconcile reads current remote text through the same resource owner and updates unresolved
baselines without replacing local text or reposting the write. A failed observation retains the
unresolved capture. If field validation or accounting fails partway through a multi-field
reconciliation, settled fields retain the observed baseline and remaining captures stay unresolved
for a later observation.
review.snapshot reports current text, baseline, revision, sequence, dirty flags, and operation state.
review.close removes the view's registry entry. An admitted job retains its document and permit
until it settles, so closing a buffer or dropping a request receiver cannot cancel an active write.

GitHub reconciliation receipts now include observed title and body. Completed review and GitHub
responses use the same chunked, request-correlated result transfer when encoded output exceeds a
single frame. Frame sizes control each delivery chunk, not the total completed content.
The host integration fixture transfers a 200 KiB body of quote characters with exact
current and baseline text. Review failures reach their Lua request callbacks without invalidating
Harness state. The existing PR editor still uses its previous rendering and baseline adapter until
the native review consumer adopts these host documents. No review UI parity or migration gate is
established by the host API alone.

## Native PR field consumer cutover

The PR view binds title and description regions through forge.review to one ReviewService document.
The initial host observation supplies saved baselines. If native text still matches the initial
rendered field, the view adopts current remote text. If the user typed while the read was pending,
the view preserves that text as a local draft. The adapter validates document identity,
field revisions, edit sequences, field structure, and dirty-state consistency before adopting results.

Text-change events retain native values and capture sequences locally. An explicit save captures
title, description, and reviewer text before dispatching review.save. A queued save retains that
capture while later typing continues. Settlement advances the captured region revisions and adopts
host baselines while preserving newer pending text. Lua compares native values with confirmed or
explicitly queued values for presentation. It does not publish newer typing implicitly.
github.pull_request.transition_async now handles lifecycle transitions only.

Failed saves observe the host snapshot and reconcile an uncertain result without reposting. Failure
notifications remain visible even after the originating buffer closes. A failed acknowledgement or
lost host preserves native text and stops operation dispatch against the obsolete identity. Reopening
the retained PR validates a replacement host snapshot and restores local fields and comment boxes
into the same buffer. Baseline or inline-revision conflicts retain the original document. Host
document identities include a service UUID, so an old document request cannot resolve to a new
host's document with the same ordinal. Closing during open releases the late document, and closing
before save dispatch completes the caller without sending a mutation.

Empty descriptions contain one empty editable row. The renderer preserves CR characters and
trailing empty rows instead of normalizing the source or inserting an editable placeholder. A
folded description has no native editable region and retains its existing field value. Generated
check rows below the folded section cannot become description text. Clean views remain interactive
while the initial host read runs. Dirty fields and pending save captures prevent replacement renders.

PR title labels occupy a native gutter, leaving the editable title at source column zero. Native
file headers own collapsed folds, semantic change counts, and opaque diff targets. Opening a fold
submits an `expand` action that Rust restricts to retained file diffs. Overview and batched review
modes rebind their command specifications without changing editable-region ownership. The overview
preserves metadata order, compact SHA labels, section spacing, and Checks before Changes. Activating
Status returns only the native lifecycle choices available for its retained observation. The shared
Lua chooser displays those choices and sends a fresh captured action, which Rust validates against
the same status target before admitting a transition. Full and incremental comment projection share
one label and fold constructor, so unchanged rematerialization publishes no patch.

The native UI fixtures cover held acknowledgements, newer typing, initial remote adoption, folded
descriptions, exact empty/trailing rows, invalid responses, late opens, close-before-save, and
read-only recovery. A real Forge executable plus native gh fixture verifies confirmed and uncertain
save flows through the Lua adapter with exactly two mutation attempts. PR Markdown layout, comment
ownership, reviewer and milestone mutation consumers, and the planned full ReviewDocument renderer
remain separate migration boundaries. No complete review parity or global allocation gate closes.

## Comment ownership and remote merge conflicts

CommentStore assigns one stable CommentId and EditStore region to each remote comment or local
draft. Conversation and inline presentations receive independent CommentOccurrenceId values.
Removing an occurrence releases presentation state without removing its body or reply draft.
Focus selects exactly one occurrence. Cursor promotion requires an inline, viewer-authored comment,
while an explicit open can expand a read-only or conversation occurrence. Overview mode retains
one inline reply draft per remote parent. Batched review mode rejects that editor.

ReviewDocument contains the comment index alongside its existing shared EditStore. Comment bodies
use the same retained-text accounting as title and description. The index validates document identity,
remote node and database identity, PR-scoped browser URLs, and revision-bound source anchors before
admitting text. Comment owners, simultaneous occurrences, and occurrence lifetimes have no arbitrary
count cap. Exact cross-language identity counters still enforce representability and never reuse
occurrence identities. Failed admission does not consume a comment identity.

A clean remote update advances the region revision. A remote value matching current local text
advances the baseline without changing the local revision or sequence. A divergent remote value
retains a conflict candidate alongside local text and the previous baseline. All three values share
the existing retained-text accounting. Accounting overflow preserves the previous candidate and body.
Pending saves reject refreshes before metadata changes, and unresolved conflicts reject new saves.

Conflict resolution validates the current region revision. KeepLocal adopts the observed remote
baseline and preserves the local draft. TakeRemote replaces current text, advances the revision,
and releases obsolete retained values. Typing the candidate value also resolves the conflict.
Counter exhaustion rejects a revision-changing resolution without losing any version. A remote
reversion to the saved baseline clears the candidate while retaining local edits.

Browser actions resolve an explicit comment occurrence or source-code anchor. Adjacent code does
not inherit a nearby comment URL, and an unsaved draft has no remote comment URL. These ownership
and merge contracts are tested directly in Rust. Remote comment ingestion, create/edit/reply/delete
receipts, host routes, Lua consumer adoption, and full ReviewDocument rendering remain open. This
foundation does not establish comment UI parity or close a migration gate.

## Comment mutation capture and settlement

CommentStore.save captures explicit create, edit, reply, or delete intent with the owner's current
body. One pending operation per comment prevents repeated posting while a receipt remains unsettled.
The receipt retains its shared EditStore text charge after the document closes. Deletes capture even
a clean body. Save rejects empty bodies, read-only owners, deleted owners, and unresolved conflicts.

Settlement checks receipt identity against the pending operation. Rejection releases admission without
advancing the baseline. Other outcomes first retain an uncertain capture. A matching confirmation
then reconciles the baseline while preserving newer native text. Invalid text, identity, anchor, or
resource evidence leaves the operation uncertain. Observation accounting failure also preserves its
capture, body, baseline, and retry prohibition. Edit and delete observations require the exact existing
remote node and database identity. Creation observations must refer to a new identity with the expected
source anchor and viewer ownership. The native caller must supply evidence correlated with the actual
creation operation. An arbitrary matching comment is insufficient evidence for that caller.

Unknown creation cannot be resolved by an absent result. The owner cannot prove that a lost mutation
response means no comment was created. Native routing must obtain positively correlated evidence or
keep that operation unresolved. No automatic mutation retry occurs in this lifecycle. A confirmed reply
adopts its remote identity and frees the parent's draft slot, allowing a subsequent distinct reply.

Confirmed deletion and observed absence of an existing remote target mark the owner deleted and
adopt an empty remote baseline. Retained native text remains available for recovery. Deleted owners
reject new mutations, stale refreshes, and new replies. An uncertain delete that observes the original
remote comment settles without reissuing deletion. Tombstone presentation and explicit draft recovery
remain part of the pending UI integration.

This implementation supplies in-memory operation ownership and settlement contracts. It does not yet
execute comment requests, persist receipts across host restarts, expose host comment routes, or replace
Lua mutation consumers. Remote fetching, correlated creation evidence, native cancellation/reaping,
and consumer cutover remain required before comment mutation parity is established.

## Scoped remote reconciliation and shared mutation execution

A PR title/body observation cannot prove the outcome of a comment mutation. RemoteQueue now keeps
an evidence scope alongside uncertainty while preserving one FIFO owner for the whole repository
resource number. PullRequest, Comment(node identity), and Creation(operation identity) scopes share
write ordering. Any unresolved scope blocks all subsequent writes to that resource. Reconciliation
clears uncertainty only when its scope matches. Unrelated observations remain read-only and leave
the original uncertain scope intact, including queued mutations admitted before the failure.
ReviewService leaves comment-only uncertainty with its comment owner. An explicitly closed unknown
PR operation adopts the subsequently observed title/body baseline while retaining local edits.
Confirmed recovery still requires exact equality with every captured field. A failure before draft
publication and remote dispatch releases its in-memory save capture without retrying closed storage,
and preserves `MutationNotStarted` evidence for the caller.

Scope identities contain at most 256 ASCII graphic bytes. Admission accounts for their text alongside
retained request input without a total-byte budget and rejects accounting overflow before acquiring
an operation slot. At most 64 uncertain resources retain one bounded scope each after request
completion. A read-only owner
cannot create or replace mutation uncertainty. Creation scopes identify individual operations, so an
observation for a different creation cannot enable reposting of the unresolved operation.

GithubService.run_mutation centralizes queue admission, task retention, shutdown selection, and result
delivery for typed mutation adapters. The existing pull_request path uses this executor with the PR
scope. The executor retains active operations independently of their response receivers and drains
them during shutdown. Native comment adapters must use this same executor with the matching evidence
scope. This change adds no alternate process pool and does not yet execute native comment requests.

Queue regressions verify cross-scope FIFO ordering, queued-write rejection after an unknown result,
unrelated observation isolation, exact-scope recovery, identity limits, and retained-byte accounting.
Service regressions verify the same uncertainty boundary and retention of an active comment-scoped operation
after caller cancellation through a shutdown deadline. Native comment request construction, correlated
creation receipts, host routing, and Lua consumer adoption remain open migration boundaries.

## Native existing-comment operations

forge-github defines typed existing-comment targets, observations, edit/delete requests, and result
outcomes. GithubService.comment validates requests before queue admission and reads the remote owner
before a write. Comment kind, node identity, database identity, and the PR-scoped URL must match.
Read-only owners and missing comments reject mutations. An already-matching body skips the write.
The service uses the existing comment evidence scope and shared mutation executor. A malformed or
mismatched write confirmation retains uncertainty until explicit read-only reconciliation.

GithubCommentRemote separates the comment capability from issue synchronization methods while GhClient
and its directory-bound view share the existing native process owner. Comment reads, edits, and deletes
use GraphQL variables encoded into temporary input files. Conversation edits select updateIssueComment,
and review-comment edits select updatePullRequestReviewComment with pullRequestReviewCommentId. Deletes
select the matching delete mutation. The client requests fullDatabaseId and accepts exact integer or
decimal-string representations, rejecting lossy values. The existing cross-language identity bound
remains 2^53 - 1.

Mutation requests include a bounded clientMutationId receipt. A response must echo that receipt before
the native client accepts its result. This correlation check does not provide server-side idempotency.
Only proven pre-execution admission failure is classified as rejected after mutation dispatch. Process
failure, GraphQL errors, malformed JSON, missing fields, or foreign receipts remain uncertain. Native
work is not reported complete until the shared process owner has collected it. Reads distinguish an
explicit null node from missing data and reject GraphQL errors before interpreting absence.

Deterministic native fixtures exercise both comment kinds, correct GraphQL identity input keys,
64-bit identities, raw Unicode and trailing rows, confirmed edits and deletion, lost responses, and
foreign receipts. Unknown results reconcile with one mutation attempt. Isolated service tests reject
foreign observations, read-only owners, invalid bodies, and mismatched confirmations. These tests do
not perform live GitHub writes. Schema references are GitHub's Issues and Pull requests GraphQL pages:
https://docs.github.com/en/graphql/reference/issues and https://docs.github.com/en/graphql/reference/pulls.

Native create/reply operations, remote comment-list loading and source-anchor reconstruction,
ReviewService receipt adoption, host routes, persistent receipts, and Lua consumer cutover remain
open. Existing Lua comment mutation consumers remain active until that integration is complete.

## Host routing for existing-comment operations

GhClient.for_directory now returns a concrete shared GhDirectory handle. The handle exposes both
GithubRemote and GithubCommentRemote without allocating another process pool. Existing callers
coerce the same handle to their required capability. Directory validation and native admission remain
centralized in the original client.

The github.comment host route accepts strict directory, target, and operation parameters and dispatches
through GithubService.comment. It runs independently of Harness initialization. Confirmed, rejected,
uncertain, and reconciled results retain their typed outcomes. Completed responses use the existing
chunked result-transfer mechanism when JSON exceeds one frame. Invalid requests and uncertain-resource
errors return through the caller's response channel. Lua does not invalidate Harness state for these
host comment failures.

Host integration tests connect the actual Forge process to a deterministic native gh fixture. They
verify checkout context, exact Unicode text, lost responses, prevention of repeated mutation, explicit
reconciliation, and unknown-field rejection. A separate 200 KiB quote-heavy comment observation crosses
multiple response frames and is assembled under the original request identity. The Lua transport fixture
verifies comment failure isolation. These fixtures issue no live GitHub writes.

The route exposes existing-comment operations. It does not replace the ReviewDocument saved-baseline
owner or complete the PR comment consumer cutover. ReviewService comment ownership integration,
creation/reply requests, comment-list loading, persistent receipts, and native UI adoption remain open.

## Review-owned comment baselines and native settlement

ReviewDocument now owns native comment targets alongside CommentStore and the shared EditStore.
ReviewService retains a combined ReviewRemote capability for PR and comment operations. Loading an
existing comment validates its target against the document's repository, PR number, and PR node
identity, then obtains the body and saved baseline from GithubService's native observation. Caller
anchor hints are validated as source coordinates but are not yet reconstructed from native comment
loading. Canonical comment-list and source-anchor loading remain separate migration work.

The review.comment command supports load, snapshot, save, reconcile, and conflict resolution. Snapshot
returns stable comment and region identities, region revision, edit sequence, current text, saved
baseline, conflict candidate, viewer ownership, deletion state, and save uncertainty. Native region
edits route through CommentStore's viewer-ownership check. Snapshot and region edits remain available
while a remote comment operation runs.

One document-level remote guard serializes PR and comment operations. Saving captures existing-comment
edit or delete intent from the shared EditStore and generates a UUID receipt for the native request.
Settlement advances only the submitted baseline, preserving newer native text. Unknown results retain
the pending capture and block subsequent saves. A PR reconciliation cannot settle a pending comment.
Comment reconciliation uses its matching native target and adopts observed truth without reposting.
Refreshed remote conflicts require a matching region revision before KeepLocal or TakeRemote resolution.

The guard retains an active native operation after its request receiver or document view closes. On
panic or an abandoned active save, it conservatively records uncertainty and releases the active flag.
The service retains submitted mutations and uncertain outcomes through the durable recovery store.
Replacement hosts restore those records and require explicit reconciliation before another publication.
Deleted comments retain native text for the pending recovery presentation.

The real Forge host fixture verifies native loading, captured text, confirmed baseline adoption,
lost responses, newer text, and reconciliation with exactly two mutation attempts. Isolated service
tests cover held writes, snapshot access during save, PR/comment exclusion, closure through shutdown,
remote panic, foreign document targets, and revision-checked refresh conflict resolution. Native create
and reply operations use client-owned draft definitions carried by explicit captures.

## Native conversation-comment creation

GithubService validates a conversation submission before admitting it to the shared PR resource queue.
The operation reads the addressed repository and PR number and verifies the supplied PR node before
beginning the mutation. A foreign parent observation therefore fails before any write. The creation
uses the existing GhClient or GhDirectory process owner and the common bounded native executor.

AddCommentInput carries the parent node, exact body, and caller receipt. Confirmation requires the
returned clientMutationId and subject node to match that submission. The returned node must be an
IssueComment with valid node and database identities, a URL bound to the target PR, viewer ownership,
and the exact submitted body. Unicode, indentation, and trailing rows retain their original bytes.
The input schema follows [GitHub's Issues GraphQL reference](https://docs.github.com/en/graphql/reference/issues).

A malformed, lost, or mismatched confirmation retains uncertainty under the creation receipt. The
resource queue blocks subsequent writes without retrying addComment. The receipt correlates the
immediate response and does not establish server-side idempotency. Creation recovery cannot use the
absence of a comment or an arbitrary body match as proof of failure or ownership. Persistent creation
receipts and positively correlated recovery remain unfinished.

The native fixture verifies successful creation, foreign receipt, foreign subject, changed body,
foreign author, wrong comment kind, response loss after persistence, and foreign-parent preflight.
Each dispatched case records exactly one mutation, and uncertain cases reject another attempt.
ReviewDocument draft adoption, host command exposure, inline creation, replies, pending reviews, and
Lua comment consumer migration remain separate unfinished boundaries.

## Review-owned conversation drafts and creation adoption

Neovim allocates a stable local comment identity and body region without contacting the host.
Explicit publication sends the draft definition and exact body text together. The admitted service
task adopts them into CommentStore and EditStore, generates a native receipt, and dispatches
GithubService creation through the document guard. Empty bodies fail before remote dispatch.

Confirmed creation adopts the returned remote identity into the same comment owner. Subsequent save
and delete commands use the existing-comment path. Saved-baseline settlement uses the captured body,
so newer local typing remains dirty against the submitted baseline. The comment and
region identities remain stable across creation and later mutations.

A failed PR identity preflight returns a proven rejection before mutation admission begins. A proven
rejection releases the capture and permits an explicit retry. Unknown creation retains the
capture and draft without adopting a target. PR reconciliation cannot clear this uncertainty, and
existing-comment reconciliation cannot substitute an arbitrary observed node. Creation recovery still
requires positively correlated evidence and persistent receipt storage. Active creation survives
caller cancellation and document removal through service shutdown. Replacement hosts restore uncertain
creation records without reposting the submitted body.

Five service tests cover creation followed by edit and deletion, newer text during a held creation,
rejection and failed preflight followed by explicit retry, uncertainty isolation, and caller/document closure during an
active write. A native Forge host fixture verifies draft capture, exact text, GraphQL
creation, receipt correlation, unchanged local identities, and exact saved-baseline adoption. These
tests issue no live GitHub mutations. Service tests also cover inline creation, replies, batched reviews,
and uncertain creation recovery after host replacement.

## PR buffer loading

Editable PR descriptions retain their exact Markdown source rows. Source-preserving Markdown metadata supplies links and structural formatting, and the shared Rust syntax engine supplies Markdown captures and fenced-language injections before publication. The review window conceals presentation markers outside the active cursor line. Accepted description edits request a fresh projection so removed fences and stale syntax decorations do not persist.

Opening a PR retains the current buffer until its title, metadata, and description are ready. Initial loading reads overview, editable fields, and requested reviewers concurrently by repository and PR number. `review.header` consumes the retained overview and reviewer responses without repeating either request. The first visible native presentation contains the complete header and description, with normal heading highlights and dark gray `Loading...` rows in pending sections. Leaving the originating buffer before presentation prevents a late response from replacing the newer buffer.

The Lua adapter retains up to eight PR documents and their native buffers for the Neovim session, keyed by workspace, hostname, repository, and PR number. Reopening a retained PR displays its existing text and loaded sections immediately, then refreshes header and secondary sections concurrently. Clean title, description, and requested-reviewer fields adopt remote changes. Local edits and pending saves remain intact. Explicit buffer deletion releases the retained document.

Title, description, and requested reviewers have native editable regions. Cursor entry unlocks the selected region and restores native editing bindings. Unsaved fields display `*`. Both `<C-s>` and `:w` immediately capture native values as a local save intent and clear their markers and the buffer's modified flag. Lua keeps this presentation baseline separate from the host's confirmed baseline. Typing after the local save restores the affected field's marker.

Explicit save and submission actions preserve the exact native field text, including CR characters, Unicode, internal line breaks, and trailing empty rows. Capture does not rewrite the buffer. Native anchors follow local edits, and immutable captures cross the host boundary only for explicit operations.

The PR and review `b` command remains available over editable fields in normal and visual mode. It sends a distinct `browse` action with the captured document, view, revision, and position. Rust preserves explicit browser targets and otherwise opens the PR page from its captured host, repository, and number. Browsing cannot activate lifecycle changes, file expansion, or comment editing. Issue browsing uses its source target or issue URL. Notification browsing uses the selected notification URL and reports missing URLs and launch failures.

PR file headers own working-file opening through the `open` action and diff loading through fold expansion. No separate working-file action row appears in the document. A stable empty trailing block bounds each file fold while incremental diff rows arrive. Status snapshots normalize an empty `headRepository.nameWithOwner` from the separately supplied head owner login and repository name, preserving fork identity for immutable source reads.

Editable regions protect their surrounding rows and headings independently of cursor-based `modifiable` settings. The shared Lua guard retains the last permitted buffer text, updating only affected rows for accepted edits and generated patches. A cross-boundary edit captures extmarks before Neovim moves them, suspends projection effects, and restores the retained text and marks on the next main-loop callback. Rejection leaves accepted field edits and native region anchors intact, sends no invalid edit to Rust, and keeps the fields editable. Only a failed restoration faults the document.

Lua retains one active publication operation and at most one queued explicit action across field saves, comment publication, batched submission, and lifecycle changes. Each action captures its input when invoked. A newer queued action replaces the previous queued action and reports supersession to its callback. Completion dispatches the retained action without recapturing later typing. Unresolved submission recovery holds publication until its outcome is settled. Rust saves title and description together, then applies reviewer additions and removals through the durable mutation queue. Completion adopts confirmed baselines without replacing native text. The queued capture continues to control markers while the earlier request settles. Presentation refresh waits while local drafts remain pending. A deferred refresh does not mark reconciliation as active, and a refresh started before a save completes cannot restore older field baselines.

Rejected and missing save results retain later explicit captures, restore markers against confirmed baselines, and notify the failure. Uncertain field saves start `review.reconcile` without reposting mutations and hold later publication until recovery settles. Rust reads current remote fields or the reviewer set before settling the durable capture. Matching title/body observations verify the submitted operation. Reviewer changes close the unknown operation after observing the current reviewer set because GitHub provides no reviewer-operation identity to link. Differing title/body observations also close the unknown operation. Recovery establishes the observed baseline without claiming that a closed-unknown request succeeded and preserves all locally owned text. Failed observations retain captures, keep native editing enabled, and expose `gR` to retry recovery. An explicit save during recovery retains its capture for later dispatch.

Uncertain comment snapshots hold later publication without replacing its queued capture. The `gR` recovery picker offers remote observation, a linked confirmed comment ID, not-dispatched rejection, and close-unknown resolution. Cancellation sends no request. Comment recovery bypasses only the comment-uncertainty guard and retains the queued publication action. Successful settlement resumes that action, while failed recovery keeps it held. The same picker presents batched submission resolution through `gB`.

Reviewer completion excludes the authenticated user only in the reviewer input. Ordinary `@` mentions still include that user. Lua validates manually entered reviewer names case-insensitively before dispatch. Rust repeats validation before any field write and again before the reviewer phase, so a self-review request cannot produce a partial title/body save or bypass validation by changing during an earlier write. The buffer enables the shared `@` username and `#` issue completion sources, with metadata refreshed at open and issue queries served from the local index.

After the first presentation, `review.load` starts file, check, conversation, and commit reads concurrently in Rust. Lua requests one presentation refresh after the batch settles. Successful sections survive failures in other sections, and failed sections display diagnostics. The shared GitHub service admits at most sixteen remote jobs, independently of storage-worker admission. Native `gh` execution runs at most four subprocesses concurrently. Additional admitted requests wait asynchronously for native job capacity, and shutdown wakes waiting requests with a closed-pool error. Section updates serialize against presentation capture without blocking editable-field acceptance during analysis.

## Native batched-review ownership

ReviewDocument owns an explicit `Overview` or `Batched` mode. Batched mode adds an independently editable review-summary region and a repository-relative viewed-file set. Both values are written into the PR-scoped durable review draft, then restored before the document is published. A failed draft write reports the request failure and prevents Lua from adopting the returned mode state.

A batched submission captures the summary and every local inline draft body with its immutable revision, path, line range, and side. The captured `ReviewMutation::ReviewSubmit` contains those comments in the single review endpoint request. The operation ID and complete mutation capture reach durable draft storage before dispatch. Confirmation advances the summary baseline and hides submitted local drafts. Rejection releases the capture without changing local text. An unknown outcome retains the operation ID and immutable capture across reopen, blocks another submission, and requires an explicit recovery action instead of automatic replay. Recovery validates the retained mutation against the recovery journal before it accepts a linked confirmed review ID, an explicit not-dispatched rejection, or close-unknown. A missing journal cannot establish confirmation.

Lua obtains mode, viewed state, and submission outcomes only through `review.begin_batched`, `review.set_viewed`, and `review.submit_batched`. It opens a compact comment fold before focusing the corresponding editable occurrence. Command specifications retain ownership of the configured review bindings.

### Commit-scoped diff expansion

Commit detail reads resolve one immutable full commit identity to its first parent and subject through `GithubReviewRemote`. Expansion compares that parent SHA to the selected commit SHA. A commit with no parents produces an explicit root-commit unavailable state because no comparison base exists.

`forge-review::commit_diff::CommitDiffStore` owns expanded commit rows separately from pull-request file state. Each record uses the full commit SHA and repository-relative path as its key and retains the subject target across expansion. The cache evicts records to retain at most 64 records and 16 MiB. A diff larger than the cache budget still renders completely from the response even when its record is not retained for reuse. Path-only pull-request `RetainedFile` entries never satisfy or replace a commit-scoped lookup.
