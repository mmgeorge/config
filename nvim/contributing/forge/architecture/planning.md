# Planning, review, and implementation

A saved plan describes a proposed interface and its review evidence. Harness validates captured internal references and retains proposal-only navigation. Existing project language servers resolve source-backed definition requests on demand.

Tree-sitter extraction and LSP setup are separate capabilities. See [Languages](languages.md) and [Adding a language](../adding-language.md).

## Local annotations and recovery

`forge.draft_comments` owns local compact and full-width comment presentation. PlanReview
uses it for creation, focus, collapse, raw body editing, and full-collection captures without
editing requests to Rust. Source extmarks retain semantic identities independently from
physical comment rows. Providers may supply sparse source identities. Read-only comments
reject focus promotion and deletion, while an owner callback admits editable source fields.
The renderer forces status-column layout before measuring rule width so line-number growth
cannot wrap the heading label. `forge.review_comments` uses the same renderer for inline PR
draft creation, focus, collapse, and local deletion. Native field extmarks retain other unsaved
text through those transitions. `forge.draft_source` caches source identities once per buffer
change or local render generation and maps navigation, syntax, and fold boundaries independently
from inserted comment rows. Materialization supplies immutable diff anchors for local creation.
Conversation and reply drafts use the same local creation path. Reply drafts retain the remote
parent identity and source anchor, reuse one unsent body per parent, and remain unavailable in
batched mode. Compact read-only comments expose their identity for reply selection without
admitting body edits. Existing conversation entries retain their explicit-open lifecycle.

Plan document recovery retains current local annotations and the latest queued explicit
operation independently. After replacement admission validates the source digest, the queued
operation keeps its original annotation capture. A queued submission receives the replacement
document, revision, view, and sequence identities before dispatch. Completion closes the current
review only when its physical buffer and plan identity match the originating submission.
An operation already dispatched to the obsolete host is not replayed by this recovery path.
Explicit PR comment saves capture only the selected body,
and batched review submission captures the summary and comments without accepting PR fields.
Generated patches defer while a local PR projection owns inserted comment rows. The adapter
coalesces those updates, detaches the clean local projection, and adopts a full host snapshot
before rebuilding source mappings. Pending native text prevents that replacement.
Review capture admission accepts client-owned comment region identities, immutable source
anchors, and optional reply parents with their bodies. `CommentStore` stages numeric identities
and validates reply ownership before `EditStore` reserves every field body. Both stores commit
only after the complete capture passes. New bodies retain an empty saved baseline. Repeated
captures preserve accepted identities and revisions. Comment save can resolve a captured region
to its numeric identity, and restoration retains that region when reopening saved draft state.
Materialization returns comment body snapshots and anchors for local presentation ownership.

## Captured declarations and saved-plan resolution

Declaration design records use the schema version declared by the current plan implementation. Older plan records are excluded from storage reads,
and older document schemas fail validation. Planning no longer authors entities, stages,
or implementation tasks.

Harness captures nonignored Git working-tree Rust, TypeScript, TSX, Lua, JSON/JSONC, TOML, YAML, and XML files. Tree-sitter
language adapters extract immutable declaration baselines, complete signatures, fields, visibility,
generics, imports, and documentation. They omit executable bodies and value initializers. Private
members remain visible when changed. Lua function headers omit the closing `end`. TypeScript arrow
bindings stop at `=>`. These overview files describe interfaces and do not claim compilation validity.
Configuration retains complete values. Unsupported extensions remain outside this design representation.

`declaration` resolves Rust and TypeScript names within the saved plan for submission validation
and proposal navigation. The shared Tree-sitter `DeclarationIndex` extracts scopes, symbols,
imports, exports, generics, and signature references without examining implementation bodies.
Proposed declarations replace their baseline files. Deleted lines use a separate baseline graph.
Uncaptured and removed files cannot enter this resolver through filesystem discovery.

## Source-backed LSP navigation

Source-backed definition jumps use the existing project LSP on demand. Rust projects the real
source path, captured digest, identifier position, and filetype through `workspace_definition`.
Lua loads that real file into a hidden buffer without triggering server autostart, selects an
already-running client for its language and project root, and requests `textDocument/definition`.
The request targets the referenced member token, including members of optional chains. Lua
normalizes `Location` and `LocationLink` responses into byte positions and returns the selected
target through the typed `plan_definition` presentation request. Rust validates the original
document, revision, view, input sequence, target, and source digest before mapping the definition
to a plan declaration or returning its source location. Modified target buffers open as source.

No generated source, stub project, private language server, or dependency download participates
in this path. Definition requests have a five-second deadline and discard superseded results.
Multiple destinations use a chooser. Missing clients and empty results remain distinct from
request failures. Lua releases only temporary buffers created by the request and never stops the
shared client. Neither Rust nor Lua caches jump results. Each jump issues a fresh request, while
the language server retains ownership of its own analysis state.

Tree-sitter still owns extraction, formatting, syntax presentation, and source-token mapping.
Captured references retain the exact source expression independently of their inferred name and
position. Unchanged captured expressions round-trip even when their receiver cannot be resolved.
Newly authored qualified targets retain structural validation. Proposed references without real
source evidence use the plan resolver and never query generated source.

External APIs, dependency versions, standard libraries, and uncaptured modules remain unchecked
during planning. Existing dependencies can be navigation targets when the running project server
resolves them. Plan capture, saving, and submission issue no LSP or dependency-acquisition requests.
Known invalid references within captured modules still fail validation. Internal incoming-use
requirements still apply to new internal declarations. These checks do not constitute compilation
or full typechecking of the proposed program.

## Validation, persistence, flows, and review questions

The submit tool runs reference validation before becoming terminal, so the provider can repair a
rejected design in the same turn. The broker repeats validation on the formatted snapshot before
freezing it. `DeclarationDesign.validation` stores generated location-bearing diagnostics and its
snapshot fingerprint. Patches invalidate that evidence. Review displays a collapsible Validation
section. Proven invalid or ambiguous references reject submission. Unverified references warn.

`.` resolves the selected token through saved Tree-sitter token coordinates. It jumps to a visible
plan declaration and opens containing folds, opens a declaration snapshot when the target is hidden
or outside the diff, or opens the exact existing project/dependency/standard-library source location.
Re-export navigation follows the defining declaration. Filtering remains unchanged. Native input
revision, view, and cursor guards discard stale responses. Lua owns key binding and presentation,
while Rust owns resolution and source coordinates.

`DeclarationDesign` persists baseline text, original source digests, proposed text, and explicit moves
inside Harness-owned `plans/<session>/<plan>/working.json`. New plans read `.forge.json` settings
and start with no captured source files. They perform no repository file discovery or extraction.
The provider receives the plan identity,
version, request, and path inventory. `plan_read` reads one virtual file or its baseline as
numbered plain text. Optional 1-based inclusive line bounds require a file path. The response retains
the active version, selected side, actual range, and total line count. An uncaptured-path read extracts
only that workspace file and returns its full-source digest without changing the saved design or version.
Inventory reads remain structured and list captured and proposed paths rather than workspace files.
`design_patch` applies familiar Add File, Update File, Delete File, Move to, and
context chunks atomically. The first Update, Delete, or Move captures that path's baseline and source
digest in the patch candidate. Add and Move reject occupied workspace destinations. Captured removals
and moves never reload their original source. The control runtime retains inspected digests within a
turn, and optional `source_digests` preserves read identities across turns. Provider transports retain
first-capture digests for broker replay, so a source change after acknowledgement also rejects the patch.
Invalid syntax, paths, bodies, stale versions, source identities, or unmatched context change nothing.
Every patch that changes the design increments the version. Patch confirmation includes
the applied declaration and overview deltas with three context lines, capped at 16 KiB with explicit
truncation. The agent can confirm focused edits without a second read. The baseline remains immutable.
Successful submission retains inconclusive reference warnings in the saved validation evidence and
returns only acceptance and version to the provider. Proven-invalid references still return actionable errors.
Reference validation reads relevant ancestor manifests, lockfiles, and untouched modules on demand
without adding those source files to the proposal. Lockfile evidence remains separate from editable files.
Files first captured in a later submitted revision compare against their captured source baseline,
and removed-row navigation opens that baseline in the current revision rather than a missing prior proposal.
`plan/prompts/system.md` defines stable model-facing Harness conventions. It separates permission
presets from Plan, Execute, and Goal tasks, preserves task identity across interruption, and explains
control transitions, questions, evidence, and failures. `backend/prompt.rs` composes current effective
permission and interaction purpose from the admitted request. Recorded-answer guidance comes from
control context rather than a phrase found in user text. Codex receives the shared instructions and
current context through `developerInstructions` on both thread start and resume, including native goal
resumption. Copilot appends shared instructions when creating or resuming a session and sends current
interaction context with every message, including messages to an already-live session.

Plan metadata stores data flows separately from the Design Markdown. `design_flows.rs` owns
the recursive `text`, optional incoming `via`, and `children` node schema, validates its size and
depth, and generates code-fenced diagrams. The planning prompt follows values, requests, events,
records, and artifacts through producers, transformations, stores, and consumers. Short labels name
one semantic role each. Paired JSON and rendered examples demonstrate transformations, shared
consumers, and alternative results. Scheduling rules and lifecycle guarantees stay in descriptions.
Short chains share a line, longer chains continue with arrows at the same
indentation, and only actual splits create indented branches. Branches retain their incoming
transfer or condition labels and continuation rails. The shared section
projection places Flows after Design and omits an empty inventory. Section reads, review output,
and revision diffs use the same generated Markdown without changing the stored JSON.

Plan review places Tests after proposed declaration changes and before Verification. Each section
retains its own fold boundary and navigation targets in both full and public-only views.
The blank row before Tests stays outside the Changes fold in both views.
Public filtering preserves that unhighlighted section spacer even after an empty diff row.

`plan/prompts/planning.md` owns the virtual file procedure, metadata roles, declaration syntax,
validation contracts, and patch examples. `PlanPrompt` embeds it in draft, feedback, revision, and
editable discussion requests. Execution prompts retain execution ID, plan ID, accepted revision,
phase, findings, and the accepted document. Start, continuation, and interruption resume have distinct
guidance. Resume requires checking current files, tool results, and background processes before
repeating work. Successful phase completion ends the model turn so Harness owns the next phase
transition. Execution revision submission retains the provider turn while the broker commits the
revision and returns the decision. Ordinary pending-question follow-ups do not claim to be Plan tasks.

`question_ask` pauses either backend on one to three structured decisions while
`plan_submit` alone creates a review artifact. `ControlToolRegistry` owns every Harness
tool schema once, then Codex projects it into app-server dynamic tools while Copilot projects it
into SDK `Tool` handlers. Ordinary prose remains an ordinary planning response until the agent
invokes a structured control tool.
`ControlToolRuntime` owns the provider-visible state machine for one bounded turn. The broker
captures the active plan state, canonical document, resolved question digests, elicitation,
execution, and goal state, then both adapters feed every control invocation through that Rust
runtime before forwarding it. Versioned patches atomically mutate virtual files and plan metadata,
so both providers share the same authoring and validation boundary. The runtime terminalizes question asks and plan
submissions, rejects controls outside their state, and treats an already-consumed question digest
as an idempotent success rather than creating another picker. The broker still replays accepted
operations against durable state with optimistic version checks, which keeps the session-scoped
broker document authoritative. Adapters export a control invocation into aggregate
`BackendOutput` only after its provider request returns success. Rejected invocations remain
visible as failed timeline activities but never enter durable replay, preventing a corrected plan
from being poisoned by an earlier validation failure. Started and completed lifecycle
notifications update the rendered tool activity only. They never authorize a control mutation,
because those notifications can precede validation or replay a rejected call after its response.
Copilot uses its native tool events for Harness control-call activity, preserving the native tool
and turn identities. The accepted-control channel carries replay data only and never adds a
second, addressless completion. Concurrent SDK handlers share one asynchronously locked control
runtime, so one call cannot make that runtime appear absent to another. Queued calls verify their
turn ownership after acquiring the runtime before invoking it.
A separate accepted-request identity set keeps successful provider request replays idempotent
without allowing rejected requests to reserve that identity.
The question tool also works during ordinary chat, goal, and execution turns. Those questions
persist on their owning `Exchange`, while planning questions remain on `PlanRecord`.
After submission, a normal prompt associated with the selected waiting Plan task creates a plan discussion exchange.
`PromptMode::PlanDiscussion` supplies the canonical document and instructions to answer questions
without editing or resubmitting. Discussion questions remain on that exchange, and clarification
and answer continuations retain its planning context. Reading leaves `AwaitingReview` unchanged.
A successful version-checked edit applies `ChangesRequested`, changes the exchange to
`PlanRevision`, and clears pending acceptance. The planning loop then requires submission or a
new question before completion. A rejected edit does not start a revision. Tool authorization
allows discussion reads and edits but rejects resubmission without an edit and rejects editing
while a question remains unresolved. These transitions retain the selected execution authorization.
`BrokerSnapshot.active_elicitation` projects either owner through one question UI contract, so
answer, skip, Ask, and continue reuse the shared bottom picker without creating a plan artifact.
Submitting the review page consumes that question set before the provider continuation starts.
The continuation prompt and control-tool descriptions therefore forbid `question_answer`
and `question_withdraw` for the embedded planning feedback. The Codex request responder
enforces the same boundary by returning an unsuccessful tool result and omitting the rejected call
from normalized control output. If Codex edits the plan but ends without submitting or asking a new
question, the backend starts one bounded corrective provider turn inside the same Harness
interaction and requires the missing terminal control action. Codex app-server can expose one
dynamic control invocation through both lifecycle and request messages. The JSON-RPC transport
normalizes that invocation once across the full Harness request, including its corrective turn.
This keeps one reviewed answer set attached to one planning interaction instead of reopening the
same questions or applying one plan edit twice.
Ordinary prose never counts as a submitted question set. Copilot `session.todos_changed` and Codex
`turn/plan/updated` events feed the generic task tracker without submitting a plan. Names printed
in prose or command output never count as control calls. Codex uses its app-server protocol
directly, including `model/list`, dynamic Harness control tools, `thread/goal/set`, and
`thread/fork`, but it does not set `collaborationMode` for Harness planning. Copilot uses the SDK
model catalog, native permission callbacks, immediate steering, cancellation, and streamed
subagent observation. Native fork enters the command set only after the backend advertises it.
No transcript-copy fallback exists.

## Check setup and plan elicitation

`/plan` validates project checks before admitting plan generation. Missing or invalid configuration
retains the original request in a `ConfiguringChecks` plan and a `checks_setup` task phase. A separate
setup exchange receives the request as tooling context and owns its questions. Configuration
publication requires an explicit user answer. Closing the picker preserves the pending setup,
while cancelling the plan cancels its setup exchange. Recovery retains the request and resumes
setup rather than generating a design. A matching persisted configuration-save record plus a valid
current snapshot permits the broker to transition to `Generating` and start one planning exchange
with the original request. An intentionally empty configuration bypasses setup. Submission still
validates the saved configuration in case it changed during planning.

`/plan` uses `Planning` and `Planned` summaries, while plan revision uses `Revising plan` during work. A
question-only turn saves `PlanElicitation` under the active `AwaitingInput` plan, renders its
choices in the timeline, and opens the shared picker across the bottom of the complete Harness
transcript-and-composer surface. The border shows the question title and `N/M` progress. The
picker maps provider choices and Other through configurable `picker.choice_keys`, reserves `a`
for Ask, and reserves `o` for Other. Provider choices
default to `n`, `e`, `i`, `l`, `u`, and `y`. Arrow keys plus `s`/`t` change the light-blue selected
row without submitting it. The compact footer exposes only question navigation and Tab feedback.
Enter records an ordinary choice or opens the
attached editor for Other and Ask. Tab opens that same editor for optional choice feedback.
Ctrl-s belongs only to the attached input window, where it records the selected answer plus text
and advances to the next unanswered question. The input renders as an independent borderless child
inside rows reserved by the parent picker and uses the shared window-local prompt gutter, so buffer
columns contain only submitted text. Reserving those rows keeps the parent top edge stable while
the input opens. Opening the picker focuses its read-only navigation buffer with the cursor hidden.
Opening an attached input restores the configured cursor and enters Insert mode. Tab returns to
the cursorless picker without closing the child, so its text
follows whichever option the user selects next. Tab or `go` can focus that existing child again,
while Ctrl-c clears its draft, closes it, shrinks the picker, and restores option focus.

After the final answer, the float becomes an explicit review page that lists every question,
selected answer, and additional input in question order. `y` closes the elicitation and resumes
the provider. `n` returns to the first question while preserving the broker-backed answer set,
including feedback and Other text, so each answer can be inspected or replaced before submission.
The main question float never maps Ctrl-s and cannot bypass this review boundary.

Answer and skip requests update only the durable elicitation record. Ask and subsequent ordinary
Harness prompts run read-only follow-up interactions against a mutable decision set. The provider
must preserve the set without a control call when it remains valid, may replace the complete set
through `question_ask`, may record only an explicit user answer through
`question_answer`, and may remove the boundary through `question_withdraw` only
when user direction or repository evidence proves that no material decision remains. A model
recommendation never qualifies as an answer.
Closing the float leaves a Timeline Status reading `Waiting for input (press oe)`. Clarification
chat can continue without reopening that unchanged question set, while `oe` or `/questions`
restores it explicitly. A provider replacement increments the elicitation revision, preserves only
answers that remain valid against the new schema, and presents the revised questions once. A
conversational answer also increments the elicitation revision so the next unresolved decision
reopens. Withdrawing an ordinary question clears its status after the response. Withdrawing a
planning question records `Question withdrawn: <reason>` in plan history and starts a new planning
turn automatically. Failed planning continuation restores the original elicitation and removes the
provisional withdrawal lifecycle, preserving the review boundary. The
status takes precedence over a concurrent subagent wait because user input blocks provider
continuation. The broker serializes missing answers as intentional best-judgment decisions only
when the user explicitly continues. A provisional `QuestionAnswered` lifecycle record is removed if that
continuation turn fails, so the timeline never claims feedback was consumed while the elicitation
has been restored.
Elicitation revisions prevent a restored session from repeatedly presenting the same picker, while
the transient Timeline Status exposes the pending decision after dismissal or restart.
Plan edit calls persist the structurally valid JSON draft without rendering its intermediate
cross-section state. `plan_submit` validates ownership coverage and renderability against
the complete staged document. A rejected submission returns those violations to the provider and
leaves the draft editable, while a successful submission atomically refreshes `working.md`, its
navigation index, and the immutable submitted revision.
Successful submission appends one collapsed artifact delta to the planning interaction that
produced it and increments the artifact count in the winbar. The delta uses the shared changed-file
tree while labeling the canonical plan as an artifact, so revision history stays attached to its
causal turn instead of creating a sibling `Plan revision created` event or dumping the complete plan
into the timeline. Harness never opens PlanReview automatically. `op` selects a session artifact
through the floating no-preview picker and opens its physical working copy through `plan.get`.
The picker selects the current task's plan first. Browsing another plan or revision never selects
a task. Each review document captures its session, plan, and revision digest independently.
`C` creates an inline annotation at the current source line. Cursor focus expands its editable body,
while leaving the annotation collapses it into a compact rendered box. Request changes records
line/body input, then Rust resolves every line to its exact semantic target before starting another
read-only revision turn. That revision interaction names the overall comment when present and otherwise stays
`Request plan changes`. The durable `ChangesRequested` record remains state-machine history but
does not create a duplicate timeline node. PlanReview admits only one acceptance or revision request
at a time. Once the reviewer commits `oY` or confirms an `oN` comment, it destroys the physical
Markdown buffer immediately and restores the originating Harness window while the broker request
continues. Failed revision requests preserve the Lua annotation cache so `op` can rebuild the review
surface. Acceptance creates durable `PlanAcceptance` input in Rust and reuses the Harness multi-page
question picker for provider-context reuse and execution authorization. Dismissing that picker
cancels pending acceptance and restores plan-review status. Rust attaches one collapsed
`Resolved N comments` node before the revision artifact delta only after the model submits the
replacement plan. Acceptance prevents an accepted write plan from inheriting Read authorization and
blocking its first file change. It then emits
`Plan accepted` inside the admitted execution exchange and creates the guarded
`Complete accepted plan: <title>` goal. Phase transitions, revision decisions, and suspension events
retain an `ExchangeAnchor` containing the owning exchange ID and a boundary in its append-only
source node sequence. `TimelineProjector` inserts their content as `ExchangeNode::PlanEvent`
inside that exchange. The timeline has no separate plan lifecycle, execution, or resolution
container. Live provider replacement preserves these derived nodes at their captured positions.
Question answers and withdrawals retain the matching question identity and place its resolved
details at the response boundary.
The collapsed clarification row retains the question, options, and response, without duplicating
the user input row. Review feedback retains annotations before revision execution, including
failed revisions. Direct review renames remain visible as exchange-owned revision events.
Records saved before anchors existed use a compatibility path scoped to the matching plan or
execution. That path uses clarification input boundaries when available, but old provider content
without item timestamps cannot recover every historical intra-turn position exactly. Cancellation
pauses execution without changing its phase. `/goal resume` uses the effective canonical design,
retained findings, and phase instructions while preserving completed workspace work.

## Harness representation and evidence boundaries

Submitted Codex turns own their request through a box, and plan-execution timeline interactions
own their interaction record through a box. Inactive enum variants no longer reserve storage for
the largest payload. Serialization continues to emit the underlying request and interaction data
without an additional wire object.

Plan mutation uses a `ResourceSchema` descriptor for collection identity, semantic keys, renaming,
and preparation. The descriptor keeps each resource's naming and identity rules together while
the mutation algorithm still validates and applies the same ordered edits. Semantic execution
accepts phase completion through its broker-owned control channel. Completion requires a fresh
semantic comparison and a verification assessment with retained tool evidence.

Wrapped plan lines accept their first-line and continuation prefixes as a pair. Entity rendering
derives the entity from its document index instead of accepting a second independently supplied
entity reference. Cargo source resolution names its package/version lock map without changing
lock ownership or retention.

## Plan implementation and conformance

Implementation cannot be interrupted by declaration reconciliation. Implement writes the accepted
behavior and tests, permits justified source deviations, and defers builds and checks to Harness Checking.
The tool catalog omits plan mutation controls in Implement and Verify. The control runtime and
broker independently reject those mutations, including calls from stale provider sessions.

Checking compares body-free declaration structure and configuration values against the current
accepted revision. Required declarations, signatures, ownership, and exposed API remain binding.
Additional internal helpers are permitted. Calls and Accesses record implementation evidence.
Planning uses their resolved internal relationships to enforce incoming-use requirements, while
implementation conformance does not treat every captured body reference as a completion gate. Checking collects declaration findings and executes accepted commands. Resolve fixes the
workspace or submits a consolidated contract revision, then returns to Checking. Plan revisions
retain the existing review-wait protocol and never replace the original accepted baseline.

Unique Rust callables within the caller's conditional scope can supply navigation and incoming-use
evidence without asserting that the current build enables their condition. This includes private
helpers called by tests inside `#[cfg(test)]` modules. An uncalled helper still fails validation,
and references to conditionally available types remain unverified.
Check tables preserve multiline command and diagnostic cells as physical rows before measuring
their display width, so a package-discovery error remains visible without preventing plan review.

At completion or a blocked result, the broker records an implementation-differences report against
the original accepted revision. Failure, cancellation, and continuation exhaustion retain an
incomplete report. Reports compare accepted paths and checkpoint changes, including removed files.
They distinguish unspecified reference categories from explicitly empty Calls or Accesses and show
internal additions, contract differences, approved revisions, and unavailable evidence.

The report is an immutable content-addressed object. The execution record and timeline node retain
its identity and counts, not its full text. The timeline shares cached report data and exposes its
file details through the existing collapsed-section loader. Reopening a session preserves the same
report. A missing report object shows unavailable content rather than silently dropping the summary.
Report publication checks source identities before committing a successful phase result. A changed
workspace rejects that result for a fresh verification attempt, rather than pairing stale evidence
with a newer report.

## Project checks and the Harness checking gate

Project check definitions live in the `checks` section of `.forge.json`. The LLM configures
them through `/checks setup`, preserving other repository settings. The categories are
`format`, `lint`, `check`, `test`, and `build`. Unit, integration, and screenshot suites are
named `test` definitions. No language detector generates commands. An absent checks section
blocks plan submission. An explicitly empty definition list is valid. Configuration version 2
requires each check to name its functional area and define its automatic trigger.

Each definition owns its identity, name, area, trigger, enabled default, scope, directory, commands,
and prerequisite identities. Every definition implements `all`; `files` and `cases` are
optional contracts. Shell-based scoped handlers receive versioned JSON on stdin and write
the actual resolved selection to a Harness-provided receipt path. Unsupported scopes, empty results
for nonempty requests, malformed receipts, and missing receipts are configuration errors.
Harness notifies the user and never silently broadens a selection. Managed Nushell helpers
live under `.forge/checks/`, and setup validates their syntax without running the checks.

Direct operations retain an executable and argument vector. `checks::invocation::InvocationResolver`
discovers Cargo, npm, or pnpm workspace members and expands file/package bindings before
authorization and execution. Identical invocations within a definition share one run and retain
all contributing files. Workspace-wide inputs require explicit full-workspace definitions.
Nested excluded packages cannot become accidental members of their parent package. Native
invocations retain manifest fingerprints and reject changed configuration before execution.
Every expanded prerequisite must pass before a dependent definition runs.

`enabled` permits execution and `auto` controls trigger-driven selection. Omitted
`auto` defaults to true. The plan's `checks` list explicitly selects enabled
checks, and `disabled_checks` suppresses checks for that plan even when their
automatic triggers match. The plan-specific `oc` picker and visual deletion
submit `plan.checks.update` through the same versioned review-edit path as test
deletion. Rust expands prerequisite and dependent choices, recomputes the preview,
retains annotations, and persists the new review digest. The repository check
configuration remains unchanged. Check rows carry semantic check identities so
visual selection remains valid across wrapping and repeated package invocations.

The `checks` list explicitly selects enabled
definitions without changing their repository defaults. The versioned
`plan_checks_add` tool adds these references through the normal design
patch lifecycle. Both previews and execution include required prerequisites.

`CheckEvidence` retains the source checkpoint and execution contract certified by
an attempt. Automatic gates reuse current evidence within the owning task,
including evidence from explicit tool runs. Package ownership bounds reuse to
that package and its metadata. Helpers without declared input boundaries use the
whole workspace. Partial selections cannot satisfy wider coverage, and changed
inputs or execution configuration invalidate reuse. Explicit runs always execute.

Ordinary completion and successful goal completion enter the Rust-owned automatic
gate. Intermediate goal turns, waiting questions, approvals, cancellation, and
active children do not. The gate runs applicable automatic checks and delivers
saved failures to a repair exchange retaining the original check owner.
`AutomaticChecks` persists pending validation and failure delivery. Repeated
failures without changed inputs stop repair with unresolved diagnostics.

`CheckPreview` uses the same resolver for the check catalog and submitted-plan review. A preview
is a proposal, not execution evidence. Plans changing package metadata retain pending validation
instead of resolving against stale manifests. Direct Cargo case operations validate the exact
case against the selected target's discovered inventory and require execution evidence, including
rejecting ignored cases and successful commands that ran zero matching tests. Other runners keep
the scoped-helper receipt contract. Argument vectors never pass through shell interpolation.

Changed-case defaults consume an accepted plan's added/modified case inventory. `new_cases`
consumes only new entries. Optional case target identities disambiguate Cargo libraries, binaries,
and integration targets without inferring test impact from production source files. An explicitly
empty inventory can select no cases. Standalone runs have no such inventory and report a
configuration error when an applicable case-scoped check cannot resolve coverage. Explicit
empty file or case selections also fail configuration. File mappings and explicit nonempty
case selections remain available outside plan execution.

Triggers select checks independently from execution scope. An `any_change` trigger requires at
least one task-relative changed path. A `paths` or `files_changed` trigger selects changes
matching an include and no exclude pattern. Patterns support `*` and `?` within components
and `**` across components.
A `manual` trigger requires an explicit request or a dependency from a selected check. Areas
are descriptive groups and may overlap through trigger paths. A full-workspace check therefore
does not run on changes outside its trigger. Required prerequisites enter the selected set even
when their own triggers do not match.

The runner compares the current checkpoint with the first checkpoint of the owning task,
preserving the baseline across continuations and host recovery. Existing dirty files do not
trigger checks unless that task changes them. Additions, deletions, and both rename paths remain
part of the change set. Before a task baseline exists, automatic applicability remains unknown
and standalone runs select no commands. An accepted plan without a checkpoint has unknown
changes rather than an empty change set. Its automatic triggers require verification, with
full-scope checks running normally and unresolved file or case scopes blocking completion.
The runner never infers a complete change set from declaration files alone or silently broadens
scoped coverage. Explicit IDs or the separate run-all action bypass triggers.
Package-manager and case-runner extensions follow
[Extending project checks](../extending-project-checks.md), with targeted verification in
[Building and testing](../building-and-testing.md#verify-project-checks).
Nushell receives `--stdin` before `-c` when scoped selectors are piped into a check. Shell
construction owns both argument order and stdin configuration.

Setup first discovers commands and then asks the user about areas, automatic suites, coverage,
and cost through the shared question flow. The setup exchange retains its decision state across
question continuations. The configuration tool rejects publication before a user answer.
Missing-configuration bootstrap during planning uses the same question requirement before
publishing checks. Explicit setup never runs configured checks or modifies active-plan questions.
Setup rejected before exchange admission returns `not_admitted` and preserves the existing task's
status and pending questions. An error after admission follows normal task failure handling.

The check-tool lease owns authorization and lifecycle event senders. When a provider turn ends,
the lease revokes tool access and releases those senders even if a persistent provider connection
retains its control context. Request completion can then drain the event stream and publish pending
questions to the UI. A retained context cannot keep the broker locked after the turn returns.

`/checks` shows ordered definitions, areas, enabled state, scope, prerequisites, result, duration,
and each check's applicability reason in its subtitle and details. Run applicable checks uses
current task changes. Run all enabled checks explicitly requests complete suites regardless of triggers.
Shared inline picker fields change task overrides without editing project defaults. Overrides
persist by task identity across resume and revisions. Ad hoc conversations use a separate
conversation identity. Saving defaults writes `.forge.json` explicitly. Details expose commands,
requested and resolved coverage, error reasons, and saved output. History retains completed
and interrupted runs. Read, status, cancel, and override requests remain responsive outside
the active provider or automatic-gate broker lock.

Checks use arrow keys for value editing on the definitions page and bracket keys for page
navigation. Refresh retains the selected page unless the owner supplies an explicit initial page.
History replaces definition actions with its own page mappings. Action rows use the available
width independently of table columns.
Expanded details wrap long output paths without discarding characters or crossing picker borders.
Missing configuration stays unconfigured. Invalid configuration and failed catalog requests retain
a visible repair row and disable execution until the catalog loads successfully.

Plans retain test inventory and manual `verification` actions, while configured checks are a
read-only projection. Submission captures the saved configuration. Acceptance rejects stale
configuration, refreshes the projection, and requires reopening the review. Implement and
Resolve write source without executing validation. Their completion persists Checking before
process admission. Manual Verify begins only after the declaration scan and enabled checks pass.

One Rust-owned runner serves the automatic gate and `checks_run`. Its provider-neutral
tools list configuration, save authorized setup captures, start runs, read status, and cancel
owned runs. Model-triggered commands follow the existing permission coordinator. The narrow
setup action writes only checks settings and managed helpers while retaining the active plan.
Disabled checks cannot be selected by the model. Source-writing phases and active children
exclude on-demand runs. Automatic checking does not admit a model exchange or report tokens.

Checks execute serially in explicit dependency order through `/config Shell`. Independent
checks continue after failures. Dependents of failed prerequisites are skipped with a reason.
An enabled check with a disabled prerequisite is a configuration error. Categories do not
imply dependencies. Formatting and lint errors therefore do not suppress independent tests.
Run snapshots retain configuration, helper hashes, overrides, selections, matched paths, selection
reasons, shell, source identity,
and environment digest. Environment values do not appear in the transcript. Each command owns
exit status, timestamps, stdout, stderr, combined output, and requested/resolved selections.

Each run owns stable structured events for start, individual check start and completion,
skips, configuration errors, interruption, resumption, and completion. Setup records a
configuration-saved event. The timeline keeps one expandable group per run with a visible
result-count and duration summary. Child rows retain lifecycle events and lazy output nodes.
Timer and output refreshes update the existing group without appending events. Reopening a
session reconstructs the same identities, ordering, and completed output. Running previews
retain at most four lines, and expanded nodes page output from saved files.

Recorded declaration or command failures enter Resolve. Configuration errors block completion
and notify the user to fix the definitions. Disabled checks and empty task selections remain
Not run, never Passed. Source changes during the gate invalidate its result. Interruption
terminates owned processes and preserves outputs. Recovery marks detached work interrupted.
Resume reuses completed results only when configuration, scope, environment, and workspace
identities still match. Passing gates enter manual Verify when actions exist, otherwise
complete the plan. Manual defects return to Resolve and manual blockers preserve Verify.
Checking updates the owning task phase before publishing the transition, so the header and
the next Resolve or Verify exchange show the same phase.
