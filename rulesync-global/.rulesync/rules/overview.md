---
root: true
targets:
  - antigravity-cli
  - claudecode
  - copilotcli
globs:
  - '**/*'
---
Keep communication direct, grounded, and concise.

- Always answer the immediate prompt directly. Do not include unsolicited
  background context, extra examples, or secondary elaboration before
  answering. The user will ask for expansion or clarification when needed.
- Apply the `technical-writing` skill to all architecture explanations, design
  reviews, implementation plans, code advice, and documentation. Omit it only
  for single-fact lookups, literal translations, syntax rewrites, or brief
  acknowledgments.
- Apply the skill's prose guidance to chat. Read a document profile only when
  the requested output matches that document form.
- Answer direct questions before elaborating. A question requests analysis, not
  implementation, unless the user also asks for a change.
- State uncertainty directly and distinguish verified facts, inferences,
  assumptions, and unresolved questions.
- Use concrete, established terminology. Do not invent unsupported frameworks,
  acronyms, or shorthand.
- Do not use semicolons in conversational or narrative prose.

# Programming

- Use semantic identifiers that state component role, ownership, and scope
  directly without requiring explanatory comments.
- Use the `technical-writing` skill's API Documentation profile for API docs,
  docstrings, and rustdoc. Use its Code Comments profile for source comments.
  Apply the active language or project skill for syntax and domain-specific
  contracts.
- **Fix the design at its source.** Refactor root abstractions, eliminate
  duplicated logic, and update all affected call sites across the codebase.
- **Use established patterns deliberately.** Make an implicit pattern explicit
  when it clarifies the design.
- **Preserve modular boundaries.** Keep concerns separate, interfaces narrow,
  and internals hidden.
- **Keep names semantic.** Never use single-letter names. Abbreviate only
  widely understood terms such as `url`, `id`, and `config`.
  - Name multi-value types with a singular collection role such as `TaskStore`,
    `TaskRegistry`, `TaskSet`, `TaskMap`, or `TaskList`.
  - Rename types at the source instead of adding aliases or compatibility
    wrappers.
- Keep functions focused. Remove wrapper functions that only forward arguments
  to an underlying call without adding parameter transformation, error
  handling, or validation.

# Local Search

- Start local code investigation with `sem`, not broad file reads, `rg`, or Git
  commands.
- Use `sem_entities` to map files and symbols, then `sem_context` to read the
  selected entity and its relationships. Read raw snippets only for omitted
  imports, glue, schemas, generated output, or exact line anchors.
- Use `sem_impact` for dependency and test effects. Use `sem_blame` and
  `sem_log` for ownership and history.
- Use bounded `rg` only when searching for exact string literals, comments,
  configuration keys, or unindexed raw text.
- Use `sem_diff` to inventory tracked changes from `filePath` and `oldFilePath`.
  Inspect raw diffs afterward for exact hunks, whitespace, and line-level proof.

# Remote Search

- For researching or finding new Rust crates, use `docs-mcp`.
- Check locally before fetching Rust or TypeScript source. Otherwise use the
  `github` MCP for source, issues, and pull requests.
- For CLI tools and APIs, prefer source code over secondary summaries.

# Shell

- **Never** read or set environment variables via shell commands. If one is required and unset, ask the user.
- Call commands directly without `cmd /c`. Prefer simple serial commands over
  complex chains.
- Set an explicit command timeout (defaulting to 30 seconds or less). Never
  exceed 120 seconds without user approval.
- Treat a timeout as diagnostic evidence. Narrow the target, add output, or
  change the command before increasing the limit.

# Testing

- Use unit tests with mocks to isolate module logic and integration tests with
  real modules to cover end-to-end boundaries.
- Verify the most targeted unit test, package, linter, or build artifact first.
  Run the full test suite only after targeted checks pass or when changing
  shared interfaces.
- For slow tests, use a targeted test filter, isolated unit test, or cached test
  fixture. If running a slow suite is unavoidable, execute it with an explicit
  timeout and report execution duration.
- Report the executed command, timeout duration, exit status, and specific error
  output or generated artifact.

# Planning

Use this section when asked to create, draft, write, review, or update an implementation plan, execution plan, refactor plan, test plan, or reviewer-readable code-flow walkthrough before making changes.

When DiffReview Harness exposes structured planning tools, treat its `PlanDocument` JSON as the canonical artifact and the rendered Markdown as a read-only reviewer view. Create the document with `harness_plan_create`, apply focused semantic operations with `harness_plan_edit`, request current state with `harness_plan_read`, and submit the exact validated ID and version with `harness_plan_submit`. Update complete definition member or enum variant arrays instead of issuing member-level operations.

During accepted-plan execution, work one complete task at a time. Use subtasks and code edits as progress and audit evidence rather than separate goals, report each task through `harness_plan_task_report`, and record divergences through `harness_plan_deviation`. Informational deviations preserve scope. Scope deviations follow the Harness review policy. Planning and plan approval never widen command permissions.

Plans follow the walkthrough artifact model. A plan is a reviewer-readable implementation walkthrough, not a generic proposal. Frame the plan around the domain object model and its ownership relationships, then use 1-3 separate code-flow diagrams to explain the major runtime or data flows that cross those boundaries. Do not force unrelated behavior into one linear flow. Connect every flow back to the objects and owners in the model. Do not start with generic Problem, Refactoring, or Design patterns sections.

When creating a plan, use this template and output each section in order:

1. **Overview** - 2-3 sentence context-and-outcome story with precise, accessible prose. Start with the feature, fix, or capability and the reviewer-visible outcome. Then explain the relevant limitation, unmet capability, or architectural motivation and the resulting role-level architecture. Use a before/now contrast only when the plan changes existing behavior. Name central constructs only when they clarify ownership or a review boundary.
2. **Usage** - Include this section immediately after Overview. When the plan adds or changes caller-facing behavior (such as a command, API, function, UI action, configuration, or text-producing workflow), show one concrete call or interaction and the expected result. For CLI tasks, include the full command and expected stdout, exit status, or artifact. Use fenced code for text-based inputs and outputs. For visual, audio, hardware, or other non-text results, write a compact text placeholder such as `<visual result: rendered preview updates with the selected theme>`. If no caller-facing usage applies, write `<Omitted>` as the only body text and keep the following section numbers unchanged.
3. **Diagrams** - Start with 1-2 sentences that summarize the ownership change and the role of every displayed type. Follow with a compact, unboxed UML-style diagram whose fixed columns are named `Contracts` and `Concrete`. Put traits, interfaces, and abstract base classes under `Contracts`. Put structs, classes, enums, configs, resources, and other instantiable or value types under `Concrete`. Write `<none>` when the plan has no relevant contract. Rows do not imply relationships between columns.
   - Start every declaration with its construct kind, such as `*trait Backend`, `*struct CodexBackend: Backend`, `*interface Focusable`, or `*class SceneEditor extends Widget: Focusable`. A colon declares contract conformance, while `extends` declares inheritance. Mark modified or new declarations with `*` and removed declarations with `~`.
   - Put the repository-relative path on the next indented line after the complete declaration and before its members. Repeat a shared path for every declaration it defines. Keep both column anchors fixed. Wrapped declarations, paths, fields, and operations must remain aligned within their own column.
   - Indent public operations with `+` and internal fields with `-`. Keep properties and return types on one line whenever possible. Omit `: void` and `: ()`. Add `: Type` only for a meaningful return value.
   - Declare shared operations once on their trait, interface, or abstract base. Do not repeat inherited operations under concrete implementations. Put concrete-only operations on an implementation only when they matter to the plan. Model implementation capability differences as fields, capability values, strategies, or explicit results instead of asymmetric subsets of contract operations.
   - Use an unqualified `Type` for owned state, `&Type` for a retained non-owning reference, and `@Type` for retained shared ownership. Use `Type?` for an optional value and `Type[]` for zero or more values. Parameters and return types express transient dependencies, so do not add `uses` relationships or a separate relationship column.
   - **Exclusive child types:** Keep reusable concrete types at the column's base indentation. When a type forms an intentional private implementation detail used exclusively by one parent, declare it immediately beneath that parent and indent it one additional level. Keep its path and members indented relative to its declaration. Do not infer exclusivity merely from having one current caller.
   - Name enum variants without a field marker. Indent named payload fields beneath their variant and prefix them with `+` because the variant exposes them as contract data. Reserve `-` for private struct or class state. When direct accessors belong in the design, use `noun()`, `noun_mut()`, and `set_noun(value)` instead of `get_noun()` variants. Omit `noun_mut()` when the language or API does not expose a distinct mutable-reference operation.

   Rust example:

   ```text
   Contracts                                 Concrete

   *trait PaymentGateway                     *struct StripeGateway: PaymentGateway
     [src/payment/gateway.rs]                  [src/payment/stripe_gateway.rs]
     + authorize(request): Authorization       - client: HttpClient
     + capture(id): Receipt                    - credentials: &SecretProvider
     + refund(id, amount): Refund

   *trait InventoryStore                     *struct PostgresInventoryStore: InventoryStore
     [src/inventory/store.rs]                  [src/inventory/postgres_store.rs]
     + reserve(request): Reservation           - connection_pool: @DatabasePool
     + release(reservation_id)
     + commit(reservation_id)

                                             *struct CheckoutService
                                               [src/checkout/service.rs]
                                               - sessions: CheckoutSessionStore
                                               - gateway: @PaymentGateway
                                               - inventory: @InventoryStore
                                               - catalog: &PricingCatalog
                                               + submit(session_id): CheckoutResult
                                               + cancel(session_id)

                                               *struct CheckoutSessionStore
                                                 [src/checkout/session_store.rs]
                                                 - sessions: CheckoutSession[]
                                                 + create(session)
                                                 + find(id): &CheckoutSession

                                             *struct CheckoutSession
                                               [src/checkout/session.rs]
                                               - authorization: Authorization?
                                               - adjustments: PriceAdjustment[]
                                               - state: CheckoutState
                                               + state(): &CheckoutState
                                               + state_mut(): &mut CheckoutState
                                               + set_state(state)

                                             *enum CheckoutState
                                               [src/checkout/session.rs]
                                               Draft
                                               PaymentAuthorized
                                                 + authorization_id: AuthorizationId
                                               Failed
                                                 + error: CheckoutError
   ```

   TypeScript example:

   ```text
   Contracts                                 Concrete

   *abstract class Widget                    *class SceneEditor extends Widget:
     [src/ui/widget.ts]                        Focusable, CommandTarget
                                               [src/editor/scene-editor.ts]
     + render(canvas)                          - viewport: SceneViewport
     + measure(bounds): Size                   - selection: SelectionModel
     + handle_event(event)                     - document: &SceneDocument
                                               - assets: @AssetCache
                                               - hovered_entity: EntityId?
                                               + selection(): SelectionModel
                                               + set_selection(selection)

                                               *class SelectionModel
                                                 [src/editor/selection-model.ts]
                                                 - selected_ids: EntityId[]
                                                 + selected_ids(): EntityId[]
                                                 + set_selected_ids(selected_ids)

   *interface Focusable                      *class SceneViewport extends Widget: Focusable
     [src/ui/focusable.ts]                     [src/editor/scene-viewport.ts]
     + focus()                                 - camera: Camera
     + blur()                                  - document: &SceneDocument
     + has_focus(): Boolean                    - renderer: @RenderDevice

   *interface CommandTarget                  *class AssetCache
     [src/command/command-target.ts]           [src/asset/asset-cache.ts]
     + can_execute(command): Boolean           - entries: AssetCacheEntry[]
     + execute(command): CommandResult         - decoder: @AssetDecoder
                                               + load(asset_id): Asset
                                               + clear()

                                             *enum EditorCommand
                                               [src/command/editor-command.ts]
                                               DeleteSelection
                                               RenameEntity
                                                 + entity_id: EntityId
                                                 + name: String
   ```

   Then include 1-3 labeled compact code-flow diagrams for the major runtime, data, request, event, persistence, recovery, or configuration flows affected by the plan. Precede each flow with 1-2 sentences that explain why it matters. Lay each flow out horizontally from its actual entry point to its observable consumer or effect. Put the action or state on the first line of each node and its repository-relative path or logical subsystem on the next line. Keep actions and locations visually paired. Use natural title-case labels such as `Capture`, `Sync`, and `Recovery`. Label arrows with the value, event, or result crossing each boundary. Keep every diagram line at 100 characters or fewer. Split long flows at meaningful boundaries. Reuse names from the UML diagram and keep independent flows separate.

4. **Tasks** - Numbered walkthrough tree organized by domain object and ownership responsibility. Each task is an active architectural review claim, not a file bucket or a forced stage in one code flow. Prefer the title shape `<Active verb> <domain object> <with|through|in|across> <architectural role>.`, but vary it when another concise active construction states the ownership change more precisely. Follow the title with 1-2 sentences that add new information about the architectural effect, motivation, constraint, or reviewer-visible consequence. Do not merely paraphrase the title. Use `now` only when contrasting changed existing behavior. For additions, state what the new construct owns, enables, or connects without inventing a previous behavior.
   - `Task` is a numbered architectural claim that advances the object model or clarifies an ownership boundary.
   - `Group` is a concrete source-file boundary. Start every group with `file` followed by the required repository-relative file path, such as `file src/draft_sync.rs`. Do not use `module`, `package`, `directory`, or a friendly file label in place of a path.
   - `Subtask` is a local design move under a group. Start with one of these verbs: `Expose`, `Encapsulate`, `Move`, `Centralize`, `Distribute`, `Extract`, `Inline`, `Split`, `Merge`, `Compose`, `Embed`, `Create`, `Destroy`, `Register`, `Unregister`, `Attach`, `Detach`, `Start`, `Stop`, `Route`, `Resolve`, `Defer`, `Configure`, `Relax`, `Enable`, `Disable`, `Reuse`, `Generalize`, or `Specialize`.
   - `Change` is a concrete construct edit. Start with one of these actions: `Add`, `Modify`, or `Remove`.
   - After the action, include a standalone colorizable type/kind term before the target. Use one of these kind terms when possible: `class`, `struct`, `enum`, `trait`, `interface`, `test`, `app`, `config`, `fn`, `method`, `constant`, or `field`. Use a code-proven role term such as `Resource`, `Cache`, or `Adapter` only when that role is clearer than the broad kind.
   - Keep the `file` group term and change kind/role term separate from the target so renderers can colorize words like `file`, `struct`, `fn`, `config`, and `Resource`.
   - Include every function, type, config, app, test, or field that will be added, modified, or deleted.
   - Order tasks by the object model and ownership relationships. Within each task, order concrete changes in the sequence that makes the ownership change easiest to review. Do not contort the task tree to mirror one code-flow diagram.
5. **Modularity, testability, and plan validation** - Validate the plan as one ownership model. Every changed construct must appear under a task, every group must name a repository-relative path, and every major flow must have its own described diagram. The UML must keep fixed `Contracts` and `Concrete` anchors, place paths after complete declarations, declare shared operations once, encode retained state through `Type`, `&Type`, `@Type`, `Type?`, and `Type[]`, and indent exclusive child types directly beneath their parent. Check that task claims, UML fields, code-flow values, and tests describe the same boundaries. Revise plans that scatter ownership across unrelated files, expose broad interfaces, hide capability differences in asymmetric implementations, imply accidental exclusivity, exceed 100 characters on a diagram line, or collapse failures and user-visible behavior into vague `handle`, `support`, `make`, or `update` wording.
6. **Test plan** - Specific tests tied to the ownership boundaries and relevant code flows:
   - **Unit tests**: What to test, what to mock, and what behavior each validates.
   - **Integration tests**: End-to-end workflows with real modules, covering key scenarios and edge cases.

Example:

# Overview

Add offline draft sync so editor changes survive closed buffers and failed saves. Before, unsaved edits lived only in the active editor session and retry workers had no durable source. Now, the editor writes draft changes into a cache and the sync worker drains that cache into retryable save requests.

# Usage

```rust
editor.apply_edit(DocumentId::from("doc-42"), Edit::insert("draft body"))
```

Expected result:

```rust
DraftChange { document_id: "doc-42", status: PendingSync }
```

CLI example:

```sh
draft-sync drain --document doc-42
```

Expected result:

```text
drained 1 pending draft for doc-42
```

# Diagrams

The diagrams establish the changed ownership model first, then separate the three lifecycle flows that use it.

## Object model and ownership

`DraftCache` owns durable `DraftChange` records independently from editor buffers. `DocumentEditor`, `SyncWorker`, and `EditorRecovery` retain shared access to that owner for capture, persistence, and recovery.

```text
Contracts                                 Concrete

<none>                                    *struct DocumentEditor
                                            [src/editor/session.rs]
                                            - draft_cache: @DraftCache
                                            + apply_edit(document_id, edit)

                                          *struct DraftCache
                                            [src/draft_sync.rs]
                                            - changes: DraftChange[]
                                            + store(change)
                                            + pending(): &DraftChange[]
                                            + mark_saved(draft_id)

                                          *struct DraftChange
                                            [src/draft_sync.rs]
                                            - document_id: DocumentId
                                            - status: DraftStatus

                                          *struct SyncWorker
                                            [src/sync/worker.rs]
                                            - draft_cache: @DraftCache
                                            + drain_cache()

                                          *struct EditorRecovery
                                            [src/editor/recovery.rs]
                                            - draft_cache: @DraftCache
                                            + restore_session()
```

## Code flow: edit capture

The editor persists each change before the active buffer can close.

```text
Capture   *apply_edit                       *store             *Pending
          [editor/session.rs] ─DraftChange▶ [draft_sync.rs] ─▶ [DraftCache]
```

## Code flow: background save retry

The sync worker turns cached drafts into save requests and clears only successful records.

```text
Sync      *drain_cache        *pending                      save                    *mark_saved
          [sync/worker.rs] ─▶ [draft_sync.rs] ─SaveRequest▶ [persistence] ─success▶ [draft_sync.rs]
```

## Code flow: session recovery

The recovery path rebuilds editor state from drafts that remain pending.

```text
Recovery  *restore                *pending                      buffer
          [editor/recovery.rs] ─▶ [draft_sync.rs] ─DraftChange▶ [editor session]
```

# Tasks

1. Own pending editor changes through durable draft state. DraftCache gives pending records a lifetime independent from editor buffers. Closing a buffer therefore preserves unsynced work.
   file src/draft_sync.rs
   └─ Create the durable owner for draft records and their lifecycle.
      ├─ Add struct DraftChange to describe pending editor edits
      ├─ Add Resource DraftCache to store pending draft records
      └─ Add fn DraftCache::mark_saved to clear records after persistence
2. Route editor mutation through the draft owner. DocumentEditor now persists each edit before buffer lifetime can end, making DraftCache the durable boundary.
   file src/editor/session.rs
   └─ Route draft changes out of the active buffer.
      └─ Modify fn DocumentEditor::apply_edit to write changes to DraftCache
3. Consume pending drafts through sync and recovery boundaries. A shared cache contract gives retries and reopened sessions one durable source of truth.
   file src/sync/worker.rs
   └─ Resolve cached drafts into save attempts.
      └─ Modify fn SyncWorker::drain_cache to build retry save requests
   file src/editor/recovery.rs
   └─ Reuse cached drafts for reopened sessions.
      └─ Modify fn restore_editor_session to load pending DraftChange records

# Modularity, testability, and plan validation

DraftCache owns durable draft state behind a narrow interface. The editor, sync worker, and recovery path depend on that owner without pretending their distinct lifecycle flows form one linear pipeline.

# Test plan

Tests verify the draft owner in isolation and each lifecycle flow across real boundaries.

* **Unit tests**: DraftCache stores pending DraftChange records, SyncWorker::drain_cache turns cached drafts into SaveRequest values, and DraftCache::mark_saved clears saved records.
* **Integration tests**: Apply an edit, close and reopen the editor, then verify the session restores the cached draft and the background sync drains it into a retry save request.
