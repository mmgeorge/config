# Planning and implementation

A plan that a provider can change after review, or that names declarations nobody can find, gives the reviewer nothing to hold the implementation to. Harness therefore owns the plan document, validates its references against captured source, freezes each submitted revision, and checks the finished work against the accepted revision.

This chapter follows a plan from authoring through acceptance and execution. [Plan review](plan-review.md) covers the review interface, and [Project checks](project-checks.md) covers the checks that run after implementation. Tree-sitter extraction is in [Languages](languages.md).

## Prompts

`plan/prompts/planning.md` introduces directory-based authoring, and `plan/prompts/authoring.md` defines the artifact and declaration grammar. Harness saves the latter as a read-only guide outside the editable directory. `plan/prompts/system.md` introduces the Harness environment, while `provider/prompt.rs` supplies the active authoring directory and current interaction authority on every planning turn.

## Plan documents

The planning agent edits one directory under `state/vN/plan-workspaces/<session>/<plan>/`, containing only `docs/` and `files/`. `docs/` holds the four Markdown artifacts; `files/` mirrors repository-relative paths with declaration-only source and complete configuration files. Normal file tools own edits, moves, and deletion; there is no operations manifest or origin marker.

Harness keeps the capture ledger, baseline copies, guide, snapshots, receipts, and rendered review files outside that directory under `state/vN/plans/<session>/<plan>/`. The ledger distinguishes captured files from additions: deleting a captured file proposes deletion, while deleting a new file withdraws the addition. Repeated capture preserves both edits and planned deletions, and restoring a readable baseline withdraws a modification.

| Tool | Behavior |
| --- | --- |
| `plan_status` | Reports the authoring root, dirty state, and proposal inventory without listing host bookkeeping directories. |
| `plan_capture_file` | Captures an existing repository-relative source at the corresponding `files/` path, never a caller-selected destination, and returns proposal and read-only baseline paths. |
| `plan_submit` | Captures the directory, validates the captured contents, and freezes an immutable revision without rewriting the draft. Planning supplies the plan ID without an expected edit version. |
| `plan_read` and `plan_edit` | Remain available for existing execution uses, including Resolve revisions and the Verify report, but are withheld during filesystem planning. |

Capture preserves original source digests, and submission rejects stale repository sources or occupied uncaptured destinations. Harness fingerprints raw draft contents and baseline evidence; submitted JSON, Markdown, and navigation are derived snapshots, not another editable authority. Later ordinary edits dirty the draft without changing a reviewed snapshot, and acceptance requires a current submission.

`PlanFileStore` journals reviewer file changes together with its existing artifact transaction and rejects intervening edits during recovery. Captures have their own recovery record so a copied file cannot silently become an addition after interruption. Forks copy draft files and capture evidence independently, and version leases protect both directory trees from history cleanup.

An explicitly new forked or replanned task captures current repository evidence while preserving its seeded proposals and deletions. This is separate from continuing an existing draft: repeated capture never silently refreshes that draft's source baseline.

Turn settlement fingerprints the editable tree and records the last observed input identity outside it. A changed directory counts as progress once, while an edited reviewable draft returns to revision work and requires submission. Repeated turns over the same unsubmitted files do not manufacture progress from repeated file-tool events.

### Artifacts

Planning artifacts are ordinary Markdown files in `docs/`, so the agent can use its existing readers and editors. The internal artifact model retains virtual names for parsing and existing execution tools; planning diagnostics identify the physical relative paths.

| Path | Contents |
| --- | --- |
| `docs/overview.md` | `# Title`, then the Objective, Usage, Requirements, Background, Decisions, Design, and Manual checks sections. |
| `docs/flows.md` | One `## Title` per independently triggered flow, a one-to-two-line description naming its trigger, and one `mermaid` block that opens with `sequenceDiagram`, `flowchart`, or `stateDiagram-v2`. |
| `docs/tests.md` | One `## path` per file with `- change \`name\` [target: t]: scenario` items. |
| `docs/checks.md` | One saved check ID per line, each selecting a check to run beyond the automatic ones. `parse_checks` appends every unlisted prerequisite after the listed IDs, and listing an automatic check is harmless. |
| `plan:findings.md` | Execution findings for Resolve; read-only. |
| `plan:evidence.md` | Completed tool results and passed checks the Verify report may cite, each with a short alias; read-only. |
| `plan:verification.md` | The Verify report: one result row per manual check, the defect findings, and a reuse reason. Editable only in Verify. |

Submission imports exact authored text into `DeclarationDesign::artifact_text` and `proposed_source`, then parses it into a snapshot's `DesignDocument` and declarations. `design_artifact::parse_authored` parses all four artifacts and collects their diagnostics with proposed-file structure failures. Dependent reference and authored-rule validation waits for successful parsing; malformed drafts remain editable.

Reviewer edits regenerate only the affected authored inputs through guarded persistence, preserving reviewer-owned disabled-check choices and annotations. Import reads the overview heading as the draft title. Prompts point to the existing directory rather than embedding a copy that goes stale on the next edit.

Evidence in `plan:evidence.md` carries a short alias derived from a hash of its full ID, so the alias stays fixed as later tool results join the list. The Verify report is an artifact rather than a `plan_phase_done` argument, because the argument was a nested array of objects whose labels had to repeat each accepted action verbatim. In Verify, `plan_edit` accepts only a patch of `plan:verification.md` and stores the text on `PlanExecutionRecord::verification_draft`. Each manual check gets an `N. passed|failed|blocked [IDs] note` row, and each defect a `path:line:column: message [IDs]` finding, so Resolve receives locations. `plan_phase_done` takes only the phase, revision, and summary. `verification_artifact::parse` reports every problem by line: pending or unknown statuses, unknown citations, findings whose file or line does not exist, and missing rows. It then builds the `VerificationReport`, labels each check from its accepted action, and derives the overall result: findings or a failed row fail it, a blocked row blocks it, and otherwise it passes. Every phase transition discards the draft, so a later Verify phase starts from the template.

Failed control calls return compiler-style text rather than JSON, because only the model reads them. `control_tool_failure_text` prints one `location: error[code]: message` line per problem, each shared correction once, and the retry instruction. The successful results of `plan_submit` and `plan_phase_done` are text too, and a successful submission lists unverified-reference warnings in the same format.

Harness captures nonignored Rust, TypeScript, TSX, Lua, JSON, JSONC, TOML, YAML, and XML files. Language adapters extract declarations with complete signatures, fields, visibility, generics, imports, and documentation, and omit executable bodies. Configuration keeps complete values. Capture stores declaration text only; it extracts no references from executable bodies. Harness stores proposed text as the agent wrote it and lays declarations out only when presenting them, through `calls::present` in reviews and diffs, so agents supply content and nesting without a required base indentation. `declaration_line_width` is a presentation preference, never a validation limit.

## Function behavior and references

`FunctionBehavior` is a tagged type whose authored block states the agent's classification: `Algorithm` means new behavior, `Change` means modified behavior, and no block claims unchanged behavior. Harness validates provided blocks without proving their historical classification, so rename provenance is not a prerequisite and an omitted Algorithm is no longer detected as a classification error.

Algorithm blocks use the repository's `algorithm` Tree-sitter grammar, and submission checks statement syntax, delimiters, indentation, control bodies, `otherwise` branches, and that `break` and `continue` sit inside loops, reporting the callable, line, and column. Every supplied Algorithm is checked, including one attached to a captured callable. Parse errors suppress the structure pass because an unparsed body has no reliable statement layout. The grammar accepts `<-` as an alias of `←`, `mod` as an alias of `%`, and `for item in items` without `each`.

Algorithm statements and expressions reject free-form prose. The grammar accepts assignments, calls, structured expressions, and control flow, with explanations confined to `#` comments. Lowercase abstract operations keep algorithms independent of implementation APIs. PlanReview gives no Algorithm invocation call coloring, so abstract operations and repository calls both render as identifiers. `calls::resolve_algorithm_reference` resolves an Algorithm identifier on demand and on a best-effort basis: a signature binding resolves to its parameter, while a qualified name, a capitalized type or constructor, or a member of a single-typed binding yields a name for the declaration resolver. Every unqualified lowercase call has no target, and an unresolved name produces neither a link nor a diagnostic.

Plans store no references. A `FunctionBody` holds only its owner and its `Algorithm` or `Change`, capture leaves `baseline_calls` empty, and `baseline_calls` holds behaviors only when a revision delta carries the previous revision's blocks forward. The editable text `calls::combined` produces and the review text `calls::present` produces show only signatures and behavior blocks, and `calls::parse` rejects a written `Calls`, `Constructs`, or `Accesses` section at its line. `calls::validate_behaviors` checks only behavior-block kinds and Algorithm syntax, so no rule requires an Algorithm to name only real declarations. Implementation reports compare declarations only, and a review rename rewrites signatures but not uses inside Algorithm text.

Submission reports authored-rule violations together. `DeclarationDesign::authored_diagnostics` builds one `PlanReferenceIndex` for comment wording and documentation rules. Agent-classified new behavior requires a callable documentation comment; absent or Change blocks do not establish that a callable was introduced. Other introduced declarations require documentation above attributes or decorators, except parameters, bindings, modules, and test or trait-contract entries. `DeclarationDesign::validated_revision` gathers those diagnostics with behavior diagnostics, then merges them with the resolver report into one `DeclarationValidationError`. Each `DeclarationDiagnosticKind` supplies the violation code and correction hint that `plan_submit` returns. `control_tool_failure_json` states a hint shared by every violation of a code once in `code_hint`, so a large rejection stays proportional to its distinct corrections. Structural design errors still fail first, because the resolver requires a valid design. Validation never resolves what an Algorithm names, type-checks expressions, or proves correctness.

`CallPresentation::section` records each Algorithm and Change heading with its exclusive end row, and `design_review::section_folds` turns each into an open `FoldRange` with `heading_marker: true`. `node_projection.lua` draws an inline `▾`/`▸` before a marked heading's text from the fold's current state, so the caret follows toggles without a host round trip.

A `PlanNavigationAnchor` can carry byte columns, and `design_review` gives each columned anchor its own column-ranged target, so go to definition, comments, and reverse usage resolve the entry under the cursor when one row holds several targets. No current producer sets columns; the field remains for links derived from Algorithm identifiers.

## Control tools

`ControlToolRegistry` defines every Harness tool schema once. Codex projects it into app-server dynamic tools, and Copilot into SDK `Tool` handlers. `ControlToolRuntime` owns the provider-visible state machine for one turn, and both adapters route every control call through it before forwarding. Submission keeps the runtime open for more edits and resubmission in the same turn, and only the last current submitted version reaches review.

Adapters add a control call to durable replay only after its provider request succeeds, so a rejected call stays visible as a failed activity without poisoning a later corrected plan. Lifecycle notifications update the rendered activity but never authorize a mutation, because they can arrive before validation. Names printed in prose or command output never count as control calls. Copilot's concurrent SDK handlers share one asynchronously locked runtime and verify turn ownership after acquiring it.

`question_ask` appends session-owned questions and returns generated IDs immediately by default. With `wait: true`, the tool remains pending and returns the resolved answers. Questions retain their originating exchange and optional task ID. If Codex edits a plan but ends without submitting or asking, the backend runs one bounded corrective turn inside the same interaction.

## Reference validation

`plan_submit` validates references before publishing a revision, so the provider can repair a rejected design in the same turn, and the broker repeats validation on the identified host-owned snapshot. `ParsedDeclarations::validation` stores diagnostics and the snapshot fingerprint. Reimporting a changed directory discards derived structure; rejection preserves the draft and previous submitted revision. Proven invalid or ambiguous references reject submission, and unverifiable references only warn.

The shared Tree-sitter `DeclarationIndex` resolves Rust and TypeScript names within the saved plan, with proposed declarations replacing their baseline files and deleted lines using a separate baseline graph. External APIs, dependency versions, and uncaptured modules stay unchecked, and capture, saving, and submission never make LSP or dependency requests.

## Flows

`DesignDocument::flows` stores `plan:flows.md` as the Markdown the agent wrote, so review, section reads, and revision diffs show the same text without a renderer of their own. Submission checks only the shape: `design_artifact::parse_flows` requires each `## Title` block to hold a description and exactly one closed `mermaid` block whose first line names an allowed diagram type, and reports each violation at its line. Harness does not parse Mermaid, so a diagram with a syntax error inside an allowed type passes submission, and PlanReview does not link names inside a diagram to declarations.

## Acceptance

Acceptance creates a durable `PlanAcceptance` and reuses the question picker for provider-context reuse and execution authorization, so an accepted write plan never inherits Read authorization. Dismissing the picker cancels acceptance. Once every question resolves, Harness emits `Plan accepted` inside the execution exchange and creates the guarded `Complete accepted plan: <title>` goal. An acceptance comment becomes reviewer context in execution prompts. Phase transitions and revision decisions carry an `ExchangeAnchor`, so `TimelineProjector` places them inside their owning exchange.

When acceptance includes inline comments, Harness sends changes-requested feedback to the planning agent in the same exchange without changing the task's automatic-acceptance flag. New Plan tasks start with this flag off; only `/plan accept on/off` changes it. Every complete submission checks that flag before starting or resuming execution. `/plan accept off` restores manual review without interrupting revision work; submitting does not reset the flag. [Plan review](plan-review.md#decisions) describes policy ownership and execution-setting retention.

## Implementation and conformance

Accepted execution begins in Implement, then enters the Harness-owned Check phase. Check findings enter Resolve, and passing checks enter Verify only when the plan contains manual verification actions; otherwise they complete execution. Implement attempts the accepted behavior and tests, and plan mutation tools are absent in Implement. Verify exposes `plan_edit` only for its report artifact, and withholds `plan_submit`. Successful phase completion ends the model turn, so Harness owns the next transition.

Execution admission persists the workspace baseline before starting its first provider turn. Baseline capture and executor setup are required operations rather than a Prepare phase. A missing baseline fails admission instead of allowing implementation to proceed without task-relative change attribution.

Repository workspaces use the existing Git checkpoint inventory. Ordinary folders use a complete content snapshot that excludes Harness's own data and never traverses symbolic links. Both forms preserve the original bytes and detect creation, modification, and deletion for checks, progress accounting, and implementation comparison.

Check compares body-free declaration structure and configuration values with the accepted revision, then runs the accepted checks. Required declarations, signatures, ownership, and exposed API are binding, and extra internal helpers are allowed. Actionable command and configuration findings enter Resolve rather than suspending the workflow in Check. Resolve fixes the workspace or submits a combined contract revision through the normal review wait, preserving Resolve while review is pending. Unexpected infrastructure failures retain their diagnostic and require explicit recovery.

Passing automated evidence admits Verify only while its accepted revision, workspace content, check configuration, task overrides, shell, and environment still match. The scheduler revalidates those inputs before another Verify turn, and phase completion checks them again. Resolve can reuse current passing evidence and return directly to Verify; changed inputs require Check. When the accepted plan has no manual actions, a current passing gate completes execution without a Verify phase.

Revision submission waits in Resolve and consumes its completed model turn before awaiting review. A user pause suspends that same phase, and accepting a revision cannot undo the pause. Explicit resume restores the preserved workflow and grants a fresh continuation allowance.

At completion or a blocked result, the broker records an implementation-differences report against the original accepted revision. The report is an immutable content-addressed object referenced from the execution record and timeline. Publication checks source identities first, so a changed workspace forces fresh verification instead of pairing stale evidence with a new report.
