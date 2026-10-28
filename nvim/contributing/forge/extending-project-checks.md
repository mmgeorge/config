# Extending project checks

Project checks execute configured commands without recognizing runners. Package
ownership, test filtering, test discovery, and runner-output interpretation belong
in repository callbacks, never in Harness. Adding a runner requires configuration
and repository code, not another branch in the host.

Read [Project checks](../../docs/forge/checks.md) for the input schema, argv
substitution, JSON callback request, receipts, and optional result reports.
[Planning](architecture/planning.md#project-checks-and-the-harness-checking-gate)
describes admission, persistence, and completion gates.

## Prerequisites

Use the repository's selected runtime and
[development toolchain](building-and-testing.md). Enable check editing before
publishing through `check_definition_edit`. Configuration publication does not execute
the configured command. Changes to callback code require their own targeted
coverage.

## Define a repository check

Keep selection and execution in the same repository-owned boundary.

1. Declare named inputs with types, descriptions, defaults, required flags, and
   explicit context sources. Names such as `package` and `test` have no built-in
   meaning. Automatic runs obtain context sources from task changes and the plan
   inventory. Explicit `check_run.inputs` values override those sources.
2. Choose a fixed argv command for direct scalar substitution or scalar-array
   expansion. Only a complete `{name}` argument substitutes a value. Substitution
   never produces shell source.
3. Configure `input:true` when the repository needs a callback. Read the versioned
   JSON request on stdin, validate repository-specific selection, and invoke the
   appropriate runner. Package mapping and optional discovery remain explicit
   callback behavior. An explicit package and test should not require searching
   unrelated executables.
4. Write a matching selection receipt after the requested work completes. Reject
   missing tests, ignored tests, or zero matches in the callback when that check
   promises exact-case coverage. Plain commands use their exit status.
5. Write an optional versioned report to `report_path` for file-level results.
   Keep stdout and stderr as diagnostics. Harness never infers a report by matching
   the executable, helper filename, or output text.
6. Publish the exact definition and any managed `.forge/checks/` helper captures
   through `check_definition_edit`. Preserve unrelated configuration. Managed helper
   ownership does not imply a language or trigger an implicit syntax command.

Categories determine default priority, not dependencies. Selected preparation
and its prerequisites run before unrelated verification. Other runnable checks
follow check, test, read-only format, lint, and build priority. Configure Rust
compiler checks and TypeScript type-check scripts as category `check`. Keep
source-changing formatting in a separate preparation action. Declare `after` only when a
consumer requires a successful prerequisite, such as a built host executable.

## Source ownership

Host paths are relative to `nvim/rust/forge/crates/forge-harness/src/`.

| Boundary | Source |
| --- | --- |
| Public check schema and operation validation | `checks/spec.rs`, `checks/config.rs` |
| Typed named values and context sources | `checks/inputs.rs` |
| Trigger matching and generic selection | `checks/applicability.rs`, `checks/selection.rs` |
| Literal argv substitution without subprocesses | `checks/invocation.rs` |
| Admission, execution, dependencies, and receipts | `checks/supervisor.rs` |
| Explicit result reports | `checks/report.rs` |
| Evidence identity and automatic reuse | `checks/evidence.rs`, `checks/automatic.rs` |
| History persistence and UI delivery | `checks/publication.rs` |
| Catalog, previews, and authorized configuration | `checks/tools.rs`, `checks/preview.rs` |

This dotfiles repository keeps runner-specific callbacks under
`nvim/tools/checks/`. The Rust callback maps changed files through Cargo metadata
only when no package is supplied. The Lua and Nushell callbacks adapt existing
repository helpers to the generic protocol.

## Host cutover

The generic-input host rejects the removed `package_manager` property. Rebuild
the development executable before restarting the host. A restart alone does not
compile source changes.

After restarting, `nu --no-config-file nvim/tools/checks/propose-config.nu`
prints the proposed checks object without modifying `.forge.json`. Publish that
object with the new `check_definition_edit` interface and the current revision from
`check_definition_list`. The proposal preserves unrelated checks, triggers, and settings.
Regenerate Rulesync guidance through its normal publication workflow.

## Verify the boundaries

Use the recorded check interface and the targeted commands in
[Building and testing](building-and-testing.md#verify-project-checks).
Host coverage must distinguish configuration and previews from actual execution.

Cover missing, unknown, and mistyped inputs, explicit source overrides, scalar
argument boundaries, array expansion, and literal embedded braces. Previewing a
command must never start discovery. Exercise callbacks through a fresh host with
valid and missing receipts, malformed reports, cancellation, prerequisite
failures, and identical versus changed input evidence.

Repository tests cover runner-specific ownership, exact-case guarantees, and
zero-match rejection. Completion-gate coverage separately exercises task
baselines, pending questions, cancelled work, explicit runs reused by automatic
checks, and failure delivery across repair turns.
