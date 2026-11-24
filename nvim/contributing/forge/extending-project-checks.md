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

Set `nextest_command` to a literal executable and argument prefix when the workspace prepares its own test environment, for example `["cargo", "tasks", "test"]`. The default is `["cargo", "nextest"]`. Forge appends the version probe, list, or run arguments and reads Nextest output from stdout. A wrapper must preserve arguments, stdout and the exit status. Send setup diagnostics to stderr. Static discovery always uses Cargo metadata directly.

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

Cover invalid trees, unknown IDs, empty selections, missing completion, protocol errors, and preserved partial results. Previewing a command must never start discovery. Use the [real-host integration suite](#check-adapter-process-integration) to exercise stdin delivery, process collection, discovery invalidation, and deadlines on both supported platforms. Unit tests over recorded events supplement those runs.

## Check adapter process integration

After building the current development executable, run the adapter protocol suite from the repository root:

```nu
nu --no-config-file .forge/checks/adapter-integration.nu
```

The suite starts an isolated real Forge host, configures script adapters and the built-in nextest adapter in a temporary repository, and submits `check_run`, `check_cancel`, and `check_discover` through the mock provider's normal tool-call interface. It reads the supervisor's recorded results through the session check API. Cases cover successful and failed results, empty or malformed streams, missing completion, oversized records, CRLF, split UTF-8 output, stdout/stderr drainage, cancellation, and deadlines. Cancellation and deadline cases start a child process and verify that both processes are gone while completed case results remain available. Discovery checks verify stale source, helper, and configuration rejection, cancellation, timeout, shutdown collection, and fresh admission after collection, without creating execution history. The suite removes its temporary workspace after success and prints the evidence directory on failure. Run it on Windows and macOS; success on one platform does not establish process cleanup on the other.

The collection fixture isolates a parent that exits while an unpiped child remains alive. Build the public-wrapper probe, then run both the direct probe and real-host scenarios:

```nu
do { cd nvim/rust/forge; cargo build -p forge-shell --example process_collection --locked }
nu --no-config-file .forge/checks/process-collection.nu
```

The direct probe repeatedly cancels and resumes its wait while the child is held. Both paths must retain collection until either the child is released or cancellation collects it. Readiness files and observed process exits establish the boundary; the fixture polls during a bounded interval before release to detect premature terminal publication. The fixture compiles its dependency-free child helper with `rustc`, creates a separate repository, and leaves evidence on failure. The two pure collection-state tests in the patched `process-wrap` workspace member run under nextest without launching processes. Windows job containment and Unix group containment differ; this fixture must pass on each platform before claiming both are covered.

On Windows the fixture also kills only the launcher after its root exits and checks that the held child exits without normal cancellation. The shell and Git launchers use creation-time job assignment; `.cargo/config.toml` enables Tokio's custom-spawn hook for the pinned nightly toolchain. This scenario does not inject a crash inside the native creation call or prove journal recovery.

On Windows, `.forge/checks/planned-integration.nu` enables fsmonitor in its unborn fixture repository. Planning must complete without a persistent monitor holding an owned Git command open, and the fixture verifies that saved fsmonitor configuration remains enabled afterward.

Use `nu --no-config-file .forge/checks/adapter-integration.nu --nextest-only` for the native nextest portion. It requires installed Cargo and nextest, creates dependency-free Cargo fixtures, and checks static discovery before a target directory exists, exact generated-test discovery, default-filter and ignored-test handling, exact case selection, affected-package selection, missing cases, duplicate native names across target kinds, Rust path escapes, and retained panic locations. A large fixture proves that a full runtime case list above 8 KiB compresses safely, while selecting only the statically declared cases fails without including its generated test. The full suite includes these checks.

The native fixture also compares static discovery, exact discovery, and execution across default, library, binary, test, example, bench, all-target, mixed, and glob target selections. Disabled `test`/`bench` flags exercise forced target selection and manifest-controlled groups; a library with both `rlib` and `cdylib` outputs verifies that crate types retain one test-binary identity. The default fixture passes a Cargo job-count alias to verify nextest argument normalization. A named fixture test exceeds its short nextest override and must produce a retained `timed_out` case result.

Native cancellation and command-deadline cases wait for one retained passing case and a PID published by another test, then verify that both the real nextest parent and test process exit. The unfinished case stays uncompleted, and a fresh selection runs after collection. This fixture does not launch subprocesses from Rust test code; nextest owns the test processes.

The suite also measures parent, explicit full-binary, and partial selections. Each selection uses a distinct, initially empty Cargo target directory for a cold build-cache run, followed by an unchanged warm run. The report records Harness command lifetime, native list-document count, and version-probe count. It does not control OS file caches or estimate production-workspace performance. To retain the report, add `--measurements <path>`; for example:

```nu
nu --no-config-file .forge/checks/adapter-integration.nu --nextest-only --measurements nextest-measurements.json
```

The recorded Windows sample is `.forge/checks/fixtures/nextest-latency.windows.json`. Latencies are observations, not test thresholds. Assertions verify cold compilation, warm build reuse, exact case counts, and the expected inventory reuse for parent versus partial selections. Native list output can lack trailing newlines, so document counts do not assume one physical log line per invocation.

The same suite introduces a repository version requirement above the installed nextest version and a Rust compilation error, then checks that discovery and run results retain the native diagnostic. Pure Rust tests translate the committed nextest recordings under `crates/forge-harness/src/checks/adapter/rust_nextest/fixtures`. Refresh those recordings with `nu --no-config-file .forge/checks/record-nextest.nu` from the repository root. The recorder creates a dependency-free temporary crate, captures list/run, format rejection, version-policy rejection, and compiler failure, and records tool versions, exact arguments, and exit codes. Only the temporary workspace path is normalized. Review changed recordings before accepting a new supported version.

Diagnostic cases cover error and warning outcomes for typecheck, lint, and build, source ranges, artifacts, and protocol text in the command log. They follow public detail cursors to retrieve separate target and artifact pages. Format cases cover an empty successful result with unknown file totals, a zero-exit unformatted failure with a retained diff, missing completion, and a forbidden write-mode file status. These cases assert the saved result, progress, script request, and public detail response. Focused response tests cover large target/artifact collections, UTF-8 text previews, missing-ID pages, whole-result counts, and cursor binding to the original run and command.

The Lua adapter also has a direct protocol suite:

```nu
nu --no-config-file -c 'source .forge/checks/lua-adapter.test.nu; run_adapter_tests'
```

That suite checks collection and selection against real mini.test shards, including computed names and deliberately failing fixtures. It supplements the supervisor suite rather than replacing it.
