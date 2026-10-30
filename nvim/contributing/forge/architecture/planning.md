# Planning and implementation

A plan that a provider can change after review, or that names declarations nobody can find, gives the reviewer nothing to hold the implementation to. Harness therefore owns the plan document, validates its references against captured source, freezes each submitted revision, and checks the finished work against the accepted revision.

This chapter follows a plan from authoring through acceptance and execution. [Plan review](plan-review.md) covers the review interface, and [Project checks](project-checks.md) covers the checks that run after implementation. Tree-sitter extraction is in [Languages](languages.md).

## Prompts

Three files define what the model is told. `plan/prompts/planning.md` owns the authoring procedure, metadata fields, declaration syntax, and patch examples. `plan/prompts/system.md` defines the stable Harness conventions. `backend/prompt.rs` adds the current permission and interaction purpose for each request. A test applies the patch examples in `planning.md` to real declarations, so keep them valid.

## Plan documents

`DeclarationDesign` persists baseline text, original source digests, proposed text, and explicit moves in `plans/<session>/<plan>/working.json`. A new plan starts with no captured files. The provider receives the plan identity, version, request, and path inventory, and edits the plan only through three control tools:

| Tool | Behavior |
| --- | --- |
| `plan_read` | Returns one virtual file or its baseline as numbered text, optionally within line bounds. Reading an uncaptured path returns its digest without changing the design. |
| `design_patch` | Applies Add, Update, Delete, Move, and context chunks atomically. The first edit of a path captures its baseline and digest. Invalid syntax, paths, versions, or context change nothing. |
| `plan_submit` | Validates the complete document and freezes an immutable revision. |

Optional `source_digests` carry read identities across turns, so a patch is rejected if the source changed after the read. Every effective patch increments the version.

Harness captures nonignored Rust, TypeScript, TSX, Lua, JSON, JSONC, TOML, YAML, and XML files. Language adapters extract declarations with complete signatures, fields, visibility, generics, imports, and documentation, and omit executable bodies. Configuration keeps complete values. Harness owns layout, so agents supply content and nesting. `declaration_line_width` is a preference, never a validation limit.

## Function behavior and references

`FunctionBehavior` is a tagged type. New callables need a nonempty `Algorithm`, and existing callables use `Change`. Algorithm blocks use the repository's `algorithm` Tree-sitter grammar, and submission checks statement syntax, delimiters, indentation, control bodies, `otherwise` branches, and that `break` and `continue` sit inside loops, reporting the callable, line, and column.

Algorithm statements and expressions reject free-form prose. The grammar accepts assignments, calls, structured expressions, and control flow, with explanations confined to `#` comments. Lowercase abstract operations keep algorithms independent of implementation APIs. Unlisted unqualified operation calls have no declaration navigation target, while explicit reference metadata, qualified names, and typed receivers retain concrete navigation.

Function-body references use three sections: `Calls`, `Constructs`, and `Accesses`. Every `Constructs` target needs a matching invocation in the Algorithm with `field = value` arguments, and qualified field references need matching `Accesses` entries. An omitted section means unknown, and an empty section means none. Validation resolves references and members, but it never type-checks expressions or proves correctness.

## Control tools

`ControlToolRegistry` defines every Harness tool schema once. Codex projects it into app-server dynamic tools, and Copilot into SDK `Tool` handlers. `ControlToolRuntime` owns the provider-visible state machine for one turn, and both adapters route every control call through it before forwarding. Submission keeps the runtime open for more edits and resubmission in the same turn, and only the last current submitted version reaches review.

Adapters add a control call to durable replay only after its provider request succeeds, so a rejected call stays visible as a failed activity without poisoning a later corrected plan. Lifecycle notifications update the rendered activity but never authorize a mutation, because they can arrive before validation. Names printed in prose or command output never count as control calls. Copilot's concurrent SDK handlers share one asynchronously locked runtime and verify turn ownership after acquiring it.

`question_ask` pauses on one to three structured decisions. In chat, goal, and execution turns, questions persist on the owning `Exchange` instead of `PlanRecord`. If Codex edits a plan but ends without submitting or asking, the backend runs one bounded corrective turn inside the same interaction.

## Reference validation

`plan_submit` validates references before accepting a version, so the provider can repair a rejected design in the same turn, and the broker repeats validation on the formatted snapshot. `DeclarationDesign.validation` stores diagnostics and the snapshot fingerprint, and any patch invalidates them. Proven invalid or ambiguous references reject submission, and unverifiable references only warn.

The shared Tree-sitter `DeclarationIndex` resolves Rust and TypeScript names within the saved plan, with proposed declarations replacing their baseline files and deleted lines using a separate baseline graph. External APIs, dependency versions, and uncaptured modules stay unchecked, and capture, saving, and submission never make LSP or dependency requests.

## Flows

Plan metadata stores execution flows separately from prose, typed by `design_flows.rs` as a `trigger` and ordered `steps`. Steps hold actions, terminal effects, and conditional branches. Rendering and validation traverse branches iteratively, without count or depth caps.

## Acceptance

Acceptance creates a durable `PlanAcceptance` and reuses the question picker for provider-context reuse and execution authorization, so an accepted write plan never inherits Read authorization. Dismissing the picker cancels acceptance. Once every question resolves, Harness emits `Plan accepted` inside the execution exchange and creates the guarded `Complete accepted plan: <title>` goal. An acceptance comment becomes reviewer context in execution prompts. Phase transitions and revision decisions carry an `ExchangeAnchor`, so `TimelineProjector` places them inside their owning exchange.

When acceptance includes inline comments, Harness first sends ordinary changes-requested feedback to the planning agent. A one-cycle approval retains the execution choices and accepts the next valid submitted revision before implementation starts. Existing execution revisions use the same one-cycle approval to resume Resolve. This approval is independent of the global revision-approval preference. [Plan review](plan-review.md#decisions) describes its persistence and invalidation boundaries.

## Implementation and conformance

Accepted execution runs Implement, Checking, Resolve, and optional Verify. Implement writes the accepted behavior and tests, and it may deviate from source with justification. Plan mutation tools are absent in Implement and Verify, and both the runtime and the broker reject them. Successful phase completion ends the model turn, so Harness owns the next transition.

Checking compares body-free declaration structure and configuration values with the accepted revision. Required declarations, signatures, ownership, and exposed API are binding, and extra internal helpers are allowed. It then runs the accepted checks. Failures enter Resolve, which fixes the workspace or submits a combined contract revision through the normal review wait, then returns to Checking. A passing gate enters manual Verify when verification actions exist, and otherwise completes the plan.

At completion or a blocked result, the broker records an implementation-differences report against the original accepted revision. The report is an immutable content-addressed object referenced from the execution record and timeline. Publication checks source identities first, so a changed workspace forces fresh verification instead of pairing stale evidence with a new report.
