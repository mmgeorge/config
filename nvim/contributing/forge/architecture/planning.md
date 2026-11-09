# Planning and implementation

A plan that a provider can change after review, or that names declarations nobody can find, gives the reviewer nothing to hold the implementation to. Harness therefore owns the plan document, validates its references against captured source, freezes each submitted revision, and checks the finished work against the accepted revision.

This chapter follows a plan from authoring through acceptance and execution. [Plan review](plan-review.md) covers the review interface, and [Project checks](project-checks.md) covers the checks that run after implementation. Tree-sitter extraction is in [Languages](languages.md).

## Prompts

Three files define what the model is told. `plan/prompts/planning.md` introduces semantic structure, the plan artifacts, and the authoring workflow; `plan_edit` describes the patch syntax and each artifact's grammar. `plan/prompts/system.md` introduces the Harness environment. `provider/prompt.rs` adds the current permission and interaction purpose for each request.

## Plan documents

`DeclarationDesign` persists each baseline's declarations, original source digest, and `DeclarationFile::editable_text`, the text `plan_read` shows for an unchanged file; `proposed_source`, the one editable text of each proposed file; `parsed`, the declaration structure and validation that the last successful submission derived from that text; explicit moves; and the `DesignDocument` overview in `plans/<session>/<plan>/working.json`. A new plan starts with no captured files. The provider receives one line naming the plan ID and version, because the plan tools read the live plan and a snapshot in the prompt goes stale at the first edit. It treats the whole plan as files it reads and patches:

| Tool | Behavior |
| --- | --- |
| `plan_status` | Lists the plan identity, phase, shared version, artifacts, and changed project files in one shape for planning and execution. Validation errors belong to `plan_submit`. |
| `plan_read` | Returns an artifact, a source file's semantic structure, or a complete configuration file as numbered text, optionally within line bounds. Reading an uncaptured path returns its digest without changing the design. |
| `plan_edit` | Applies Add, Update, Delete, Revert, Move, and context chunks atomically to artifacts and project files. The first edit of a path captures its baseline and digest. Artifact and source text is stored as written, even when it does not parse, and every effective edit removes `parsed`; configuration edits still reject invalid syntax. A file the user changes between edits needs no special handling, because a patch whose context no longer matches fails like any edit tool's patch against a changed file. Revert File withdraws a file's proposal, including a move, or returns an artifact to its last submitted text. Invalid patch syntax, paths, versions, or context change nothing, and the failure says so. |
| `plan_submit` | Parses every artifact and every proposed source file against its baseline evidence, classifies references once, stores the result in `parsed`, validates the complete document, and freezes an immutable revision without reformatting it. A rejection lists every artifact and declaration problem by line. |

Optional `source_digests` carry read identities across turns, so a patch is rejected if the source changed after the read. Artifact and file edits share one version. Every effective edit increments it, and a no-op leaves it unchanged. A successful edit returns the version and the changed paths rather than a diff, because the agent already holds the text it wrote.

### Artifacts

The overview would otherwise be a nested JSON argument, which provider schema compaction can strip of its guidance and which models often get wrong on the first attempt. The plan is therefore a set of Markdown artifacts under the reserved `plan:` prefix, which no workspace path can occupy.

| Path | Contents |
| --- | --- |
| `plan:overview.md` | `# Title`, then the Objective, Usage, Requirements, Background, Decisions, Design, and Manual checks sections. |
| `plan:flows.md` | One `## Title` per independently triggered flow, a one-to-two-line description naming its trigger, and one `mermaid` block that opens with `sequenceDiagram`, `flowchart`, or `stateDiagram-v2`. |
| `plan:tests.md` | One `## path` per file with `- change \`name\` [target: t]: scenario` items. |
| `plan:checks.md` | Every enabled saved check as a `- [ ] id` line; `[x]` selects a check, and its prerequisites must be marked too. |
| `plan:findings.md` | Execution findings for Resolve; read-only. |
| `plan:evidence.md` | Completed tool results and passed checks the Verify report may cite, each with a short alias; read-only. |
| `plan:verification.md` | The Verify report: one result row per manual check, the defect findings, and a reuse reason. Editable only in Verify. |

`DeclarationDesign::artifact_text` stores each edited authored artifact exactly as the agent wrote it, so every diagnostic names a line the agent can find. Submission is the only path from that text to `DesignDocument`: `design_artifact::parse_authored` parses all four artifacts and collects every problem as a `DeclarationDiagnostic` with an artifact kind, and `validated_revision` merges them with the declaration diagnostics into one rejection. A clean parse covers every rule `DesignDocument::validate_for_submission` enforces, so the parsed document always satisfies it. Parsers continue past a bad line rather than stopping at the first, and flow parsing builds nested steps iteratively, without count or depth caps.

An artifact without stored text renders from `DesignDocument`. That keeps Harness-side writers consistent with the text model: the review check toggle and test removal change the document and drop the artifact's stored text, and the next read renders the result in the grammar the agent edits. Every overview edit copies its `# Title` line into the plan title, so the title in the UI follows the draft. Provider prompts present the artifact text rather than the parsed document, so a prompt never shows a document older than the agent's latest edit.

Evidence in `plan:evidence.md` carries a short alias derived from a hash of its full ID, so the alias stays fixed as later tool results join the list. The Verify report is an artifact rather than a `plan_phase_done` argument, because the argument was a nested array of objects whose labels had to repeat each accepted action verbatim. In Verify, `plan_edit` accepts only a patch of `plan:verification.md` and stores the text on `PlanExecutionRecord::verification_draft`. Each manual check gets an `N. passed|failed|blocked [IDs] note` row, and each defect a `path:line:column: message [IDs]` finding, so Resolve receives locations. `plan_phase_done` takes only the phase, revision, and summary. `verification_artifact::parse` reports every problem by line: pending or unknown statuses, unknown citations, findings whose file or line does not exist, and missing rows. It then builds the `VerificationReport`, labels each check from its accepted action, and derives the overall result: findings or a failed row fail it, a blocked row blocks it, and otherwise it passes. Every phase transition discards the draft, so a later Verify phase starts from the template.

Failed control calls return compiler-style text rather than JSON, because only the model reads them. `control_tool_failure_text` prints one `location: error[code]: message` line per problem, each shared correction once, and the retry instruction. The successful results of `plan_submit` and `plan_phase_done` are text too, and a successful submission lists unverified-reference warnings in the same format.

Harness captures nonignored Rust, TypeScript, TSX, Lua, JSON, JSONC, TOML, YAML, and XML files. Language adapters extract declarations with complete signatures, fields, visibility, generics, imports, and documentation, and omit executable bodies. Configuration keeps complete values. Capture classifies baseline references against the baseline snapshot, so a later plan edit never changes the evidence of an existing file. Harness stores proposed text as the agent wrote it and lays declarations out only when presenting them, through `calls::present` in reviews and diffs, so agents supply content and nesting without a required base indentation. `declaration_line_width` is a presentation preference, never a validation limit.

## Function behavior and references

`FunctionBehavior` is a tagged type. New callables need a nonempty `Algorithm`, and existing callables use `Change`. Algorithm blocks use the repository's `algorithm` Tree-sitter grammar, and submission checks statement syntax, delimiters, indentation, control bodies, `otherwise` branches, and that `break` and `continue` sit inside loops, reporting the callable, line, and column. `validate_algorithm` returns every parse and structure diagnostic for a block, naming the text each ERROR node rejected. Parse errors suppress the structure pass because an unparsed body has no reliable statement layout. The grammar accepts `<-` as an alias of `←`, `mod` as an alias of `%`, and `for item in items` without `each`. Parse diagnostics keep the first error on each line, because tree-sitter recovery reports further errors that restate it.

Algorithm statements and expressions reject free-form prose. The grammar accepts assignments, calls, structured expressions, and control flow, with explanations confined to `#` comments. Lowercase abstract operations keep algorithms independent of implementation APIs. PlanReview keeps call highlighting only on Algorithm invocations that `calls::declared_algorithm_call` matches to a `Calls` or `Constructs` entry, so abstract operations render as identifiers. Unlisted unqualified operation calls have no declaration navigation target, while explicit reference metadata, qualified names, and typed receivers retain concrete navigation.

Function-body references use three sections: `Calls`, `Constructs`, and `Accesses`. `calls::validate_behaviors` applies these rules with the declaration resolver, so they bind only repository declarations. Every repository `Constructs` target needs a matching invocation in the Algorithm. Named-field types take `field = value` arguments, while tuple structs and variants, classified by `FieldShape` in the declaration index, take positional arguments. A field reference needs an `Accesses` entry for its deepest repository-owned member: `Velocity::vector::x` requires `Velocity::vector`. A reference rooted in a dependency, standard-library, or prelude type needs none.

Submission reports authored-rule violations together. `DeclarationDesign::authored_diagnostics` builds one `PlanReferenceIndex` for comment wording, documentation, and incoming-use rules. The documentation rule requires a `///` or `/** */` comment, above any attributes or decorators, on every introduced declaration except parameters, bindings, modules, and test or trait-contract entries. `DeclarationDesign::validated_revision` gathers those diagnostics with behavior diagnostics, then merges them with the resolver report into one `DeclarationValidationError`. Each `DeclarationDiagnosticKind` supplies the violation code and correction hint that `plan_submit` returns. `control_tool_failure_json` states a hint shared by every violation of a code once in `code_hint`, and `validate_behaviors` reports each missing Accesses field once per callable with every Algorithm line that uses it, so a large rejection stays proportional to its distinct corrections. Structural design errors still fail first, because the resolver requires a valid design. An omitted section means unknown, and an empty section means none. Validation resolves references and members, but it never type-checks expressions or proves correctness.

`CallPresentation::section` records each behavior and reference heading with its exclusive end row, and `design_review::section_folds` turns each into a `FoldRange` with `heading_marker: true`, closed for Constructs and Accesses. `node_projection.lua` draws an inline `▾`/`▸` before a marked heading's text from the fold's current state, so the caret follows toggles without a host round trip.

PlanReview groups Accesses rows by declaring type. `calls::insert` renders review presentations with one `Type { member, ... }` row per type, while the editable presentation keeps one entry per line, so patches and saved plans are unaffected. `CallPresentation::call_row` records every entry on a row with its byte columns, each navigation anchor carries those columns, and `design_review` gives each entry its own column-ranged target, so go to definition, comments, and reverse usage resolve the entry under the cursor.

## Control tools

`ControlToolRegistry` defines every Harness tool schema once. Codex projects it into app-server dynamic tools, and Copilot into SDK `Tool` handlers. `ControlToolRuntime` owns the provider-visible state machine for one turn, and both adapters route every control call through it before forwarding. Submission keeps the runtime open for more edits and resubmission in the same turn, and only the last current submitted version reaches review.

Adapters add a control call to durable replay only after its provider request succeeds, so a rejected call stays visible as a failed activity without poisoning a later corrected plan. Lifecycle notifications update the rendered activity but never authorize a mutation, because they can arrive before validation. Names printed in prose or command output never count as control calls. Copilot's concurrent SDK handlers share one asynchronously locked runtime and verify turn ownership after acquiring it.

`question_ask` appends session-owned questions and returns generated IDs immediately by default. With `wait: true`, the tool remains pending and returns the resolved answers. Questions retain their originating exchange and optional task ID. If Codex edits a plan but ends without submitting or asking, the backend runs one bounded corrective turn inside the same interaction.

## Reference validation

`plan_submit` validates references before accepting a version, so the provider can repair a rejected design in the same turn, and the broker repeats validation on the same stored snapshot. `ParsedDeclarations::validation` stores diagnostics and the snapshot fingerprint, and every effective plan edit removes `parsed`, including its validation. A rejected submission changes nothing. Proven invalid or ambiguous references reject submission, and unverifiable references only warn.

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
