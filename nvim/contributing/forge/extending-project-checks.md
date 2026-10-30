# Extending project checks

Teaching the host about a test runner would mean a new host branch for every language, so project checks run configured commands without recognizing runners. Package ownership, test filtering, discovery, and output interpretation belong in repository callbacks. Adding a runner takes configuration and repository code.

[Project checks](../../docs/forge/checks.md) in the user docs defines the input schema, argv substitution, callback request, receipts, and reports. [Project checks](architecture/project-checks.md) in the architecture docs covers selection and the completion gate.

## Prerequisites

Enable check editing before publishing through `check_definition_edit`. Publishing a configuration does not run it, so callback code needs its own targeted coverage.

## Define a check

1. Declare named inputs with types, descriptions, defaults, required flags, and context sources. Names such as `package` and `test` have no built-in meaning. Explicit `check_run.inputs` values override context sources.
2. Choose a fixed argv command. Only a complete `{name}` argument substitutes a value, and substitution never produces shell source.
3. Set `input: true` when the repository needs a callback. Read the versioned JSON request on stdin, validate the selection, and invoke the runner. An explicit package and test should not require searching unrelated executables.
4. Write a matching selection receipt after the work completes. Reject missing or ignored tests and zero matches in the callback when the check promises exact-case coverage. Plain commands use their exit status.
5. Write an optional versioned report to `report_path` for file-level results. Harness never infers a report from output text.
6. Publish the definition and any managed `.forge/checks/` helpers through `check_definition_edit`, keeping unrelated configuration.

Categories set default priority, not dependencies. Configure Rust compiler checks and TypeScript type-check scripts as category `check`, and keep source-changing formatting in a separate action. Declare `after` only when a check needs a successful prerequisite, such as a built host executable.

## Source map

Host paths are relative to `nvim/rust/forge/crates/forge-harness/src/`.

| Boundary | Source |
| --- | --- |
| Check schema and validation | `checks/spec.rs`, `checks/config.rs` |
| Typed values and context sources | `checks/inputs.rs` |
| Triggers and selection | `checks/applicability.rs`, `checks/selection.rs` |
| Literal argv substitution | `checks/invocation.rs` |
| Admission, execution, and receipts | `checks/supervisor.rs` |
| Result reports | `checks/report.rs` |
| Evidence and automatic reuse | `checks/evidence.rs`, `checks/automatic.rs` |
| History and UI delivery | `checks/publication.rs` |
| Catalog and previews | `checks/tools.rs`, `checks/preview.rs` |

This repository keeps its runner callbacks under `nvim/tools/checks/`. The Rust callback maps changed files through Cargo metadata only when no package is supplied. `nu --no-config-file nvim/tools/checks/propose-config.nu` prints a proposed checks object without changing `.forge.json`.

## Verification

Use [Verification by area](verification-by-area.md#project-checks), and keep configuration and preview coverage separate from actual execution.

Cover missing, unknown, and mistyped inputs, explicit overrides, array expansion, and literal braces. Previewing a command must never start discovery. Exercise callbacks through a fresh host with valid and missing receipts, malformed reports, cancellation, prerequisite failures, and identical versus changed input evidence.
