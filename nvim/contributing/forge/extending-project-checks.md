# Extending project checks

Check adapters own package mapping, test selection, discovery, and translation of tool output. Harness owns process lifetime, protocol validation, coverage, and presentation. Read the compiled adapter catalog before adding a runner: use an applicable built-in adapter when one is available, or implement the script protocol for the category.

[Project checks](../../docs/forge/checks.md) in the user docs defines agent-facing selection and inspection. [Project checks](architecture/project-checks.md) in the architecture docs covers selection and the completion gate. The adapter types under `checks/adapter/` define the implemented protocol.

## Prerequisites

Enable check editing before publishing through `check_definition_edit`. Publishing a configuration does not run it, so adapter code needs its own targeted coverage. The catalog describes the categories supported by this build; do not assume that an adapter mentioned in a different build is available.

## Define a check

1. Read `check_definition_list` for the current revision and adapter catalog, and `check_definition_details` for definitions that must be preserved.
2. Declare the category and its required runner: `adapter` for typecheck, test, format, lint, and build; `command` for custom. Adapter checks reject `inputs`, `input`, and `prepare`. A custom command can use typed named inputs and fixed argv substitution.
3. For a test script adapter, implement static and exact discovery as supported by the runner. Static discovery must not build or invoke test binaries. Return opaque IDs that round-trip through selection, parent metadata, and available source locations.
4. For execution, emit a plan before case events, one terminal result per planned case, and a final completion event. Write machine-readable events only to the supplied event file and human output to stdout or stderr. The owned launcher collects the process tree on cancellation, deadline, and protocol failure.
5. Use one Nushell implementation for Windows and macOS. Its configured argv includes `--stdin`; the script receives one JSON request followed by EOF, and starts in the repository root. Honor the request's check directory and report repository-relative paths.
6. Publish the complete definition map and any managed `.forge/checks/` helpers through `check_definition_edit`, preserving unrelated configuration. Run the saved check through Harness after publication.

Categories set default priority, not dependencies: typecheck, test, format, lint, build, then custom. Every action declares its category; `check` has been renamed to `typecheck`. Declare `after` only when a check needs a successful prerequisite, such as a built host executable. Existing plain commands in contract categories must migrate through the revision-checked configuration workflow; loading configuration never rewrites them. Custom commands receive closed stdin. The removed callback mode, `input: true`, fails with a named migration error; use named argv inputs or an adapter.

## Rust nextest

The built-in `rust_nextest` adapter implements the test category. A definition can select a Cargo workspace below the repository root:

```json
{
  "trigger": {},
  "action": {
    "category": "test",
    "directory": "nvim/rust/forge",
    "adapter": {
      "rust_nextest": {
        "cargo_profile": "dev",
        "nextest_profile": "default",
        "locked": true
      }
    }
  }
}
```

Static discovery reads offline Cargo metadata and follows Rust module declarations with Tree-sitter. It does not build targets. It recognizes all Cargo library crate types and applies configured target selectors, including named-target overrides of `test = false`. `--tests` follows the manifest's `test` flags; `--benches` follows its `bench` flags, which require reading the package manifests. Generated tests appear only in exact discovery; conditional compilation and required features can leave static declarations absent from the runtime list. Exact discovery builds and lists the selected binaries, applies nextest's default filter, and supplies the plan used by execution. Both list and run use the same profiles, features, and selection. Execution disables retries and fail-fast.

Selections accept discovered package, binary, module, or case IDs. Affected files select their owning package and its reverse dependencies; workspace control files and files without a package owner widen to the whole workspace with a reason. Large explicit selections are resolved through bounded list requests. A selection covering every eligible runtime case in a binary compresses to that binary; a partial selection whose final filter exceeds 8 KiB fails without widening. Parent selections reuse their complete list inventory, while case and module selections need a second inventory pass. The adapter retains native output in the command log and translates reported case output, failures, durations, and panic locations. When different target kinds produce identical native event names, separate binary executions preserve their distinct case IDs.

Options also accept `features`, `all_features`, and `cargo_args`. Extra build arguments are restricted to target selectors (`--lib`, `--bins`, `--bin`, `--tests`, `--test`, `--examples`, `--example`, `--benches`, `--bench`, `--all-targets`), `--no-default-features`, `--offline`, `--frozen`, `--target`, `--target-dir`, and `--build-jobs` (also accepted as `--jobs`/`-j`). Target names support Cargo's Unix globs; invalid patterns are rejected when saving the definition. Flags that select packages, replace the filter, set profiles, enable retries, or change the output protocol are rejected. Cargo and nextest must already be installed. Offline metadata failure is reported without an online retry.

Exact discovery and execution require cargo-nextest **0.9.151** or later and verify the installed version before listing tests. The adapter pins `libtest-json-plus` format **0.1**; newer versions must still provide that protocol. Static discovery requires Cargo but does not launch nextest. Compilation and version-policy failures report the native exit code and up to 16 KiB of raw stderr, decoded safely with an explicit omitted-byte count; full output remains in the command log. Successful cases retain only the per-case output supplied by nextest's reporter.

## Source map

Host paths are relative to `nvim/rust/forge/crates/forge-harness/src/`.

| Boundary | Source |
| --- | --- |
| Check schema and validation | `checks/spec.rs`, `checks/config.rs` |
| Typed values and context sources | `checks/inputs.rs` |
| Triggers and selection | `checks/applicability.rs`, `checks/selection.rs` |
| Literal argv substitution | `checks/invocation.rs` |
| Adapter contracts and catalog | `checks/adapter/` |
| Discovery ownership and snapshot validation | `checks/discovery.rs`, `checks/owner.rs` |
| Admission, execution, and retained category results | `checks/supervisor.rs` |
| Evidence and automatic reuse | `checks/evidence.rs`, `checks/automatic.rs` |
| History and UI delivery | `checks/publication.rs` |
| Catalog and previews | `checks/tools.rs`, `checks/preview.rs` |

The Lua runner's adapter mode lives in `.forge/checks/lua-tests.nu`; its people-facing CLI remains available. Script adapters also support `typecheck`, `lint`, `build`, and `format_check` requests with a `select.paths` selection. Diagnostic events use one-based lines and UTF-8 byte columns, an optional target plan, and a final completion record. Build streams alone may report artifacts. Format checks report `formatted`, `unformatted`, or `failed` per file and may include proposed diffs. Omit the file plan when the formatter cannot enumerate its processed set. An unformatted result fails even when the process exits zero. Explicit format apply and lint fix operations follow the admission and required-verification rules described in the architecture document; repository-wide writer coordination remains incomplete.

## Verification

Use [Verification by area](verification-by-area.md#project-checks), and keep configuration and preview coverage separate from actual execution.

Cover invalid trees, unknown IDs, empty selections, missing completion, protocol errors, and preserved partial results. Previewing a command must never start discovery. Use the [real-host integration suite](building-and-testing.md#check-adapter-process-integration) to exercise stdin delivery, process collection, discovery invalidation, and deadlines on both supported platforms. Unit tests over recorded events supplement those runs.
