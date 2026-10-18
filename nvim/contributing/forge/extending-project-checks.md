# Extending project checks

A check must preserve the requested coverage from selection through execution.
Package discovery identifies workspace membership, the invocation resolver expands
commands, and the runner records execution evidence. A new package manager or test
runner must use those boundaries so plan previews and Checking resolve the same
commands without silently widening coverage.

Automatic selection is independent of availability. `enabled` permits execution,
while `auto` selects matching checks for plans and completion gates. Explicit plan
IDs and explicit runs can select enabled nonautomatic checks. Keep the shared
agent instructions synchronized with these contracts: validation must use the
checks interface rather than unrecorded terminal commands.

Read [Planning](architecture/planning.md#project-checks-and-the-harness-checking-gate)
for lifecycle ownership and [Project checks](../../docs/forge/checks.md) for
configuration examples. Parser and LSP integration is separate and follows
[Adding a language](adding-language.md).

## Current contracts

Triggers determine whether a definition applies. `files_changed` and `paths`
match changed paths, while the definition's scope chooses all, changed-file,
changed-case, or new-case coverage. Package membership is an operation capability
selected by `package_manager`, not another trigger or an LLM-supplied mapping.

| Capability | Current behavior |
| --- | --- |
| Cargo ownership | `cargo metadata --format-version 1 --no-deps --offline --locked` supplies workspace members and target identities |
| npm ownership | `package.json` supplies workspace patterns, including the `workspaces.packages` form |
| pnpm ownership | `pnpm-workspace.yaml` supplies workspace patterns |
| Single-package projects | The root package is available without workspace member patterns |
| Changed files | The deepest discovered package owns the path, including a deleted file's path |
| Planned cases | `changed_cases` selects new and modified entries, while `new_cases` selects only new entries from the accepted plan |
| Native exact cases | Cargo standard-harness targets validate runtime names and prove the selected tests executed |
| Other case runners | Scoped shell helpers consume the versioned selection and return a validated receipt |

Cargo discovery preserves an operation's leading `+toolchain` selector. Its
request-local package cache includes that selector, so checks using different
toolchains cannot share metadata resolved under another toolchain.

Ownership does not establish dependent-package impact or map arbitrary changed
source files to individual unit tests. Root workspace manifests and lockfiles in
multi-package projects require explicit workspace-wide rules. Excluded or
independent nested packages cannot fall back to their parent's ownership.

## Source ownership

Rust paths in this table are relative to
`nvim/rust/forge/crates/forge-harness/src/`.

| Boundary | Source |
| --- | --- |
| Definition schema, operation validation, and scopes | `checks/config.rs`, `checks/selection.rs` |
| Trigger matching | `checks/applicability.rs` |
| Package inventories, manifests, and ownership | `checks/packages.rs` |
| Binding expansion and invocation deduplication | `checks/invocation.rs` |
| Preparation, dependencies, execution, and evidence | `checks/runner.rs` |
| Current coverage and repair delivery | `checks/evidence.rs`, `checks/automatic.rs` |
| Shared catalog and plan previews | `checks/preview.rs`, `checks/tools.rs` |
| Accepted-plan case inventory | `broker/checks.rs`, `plan/design_tests.rs` |
| Preview invalidation and plan rendering | `plan/design.rs`, `plan/design_review.rs` |
| Setup guidance | `checks/setup.md`, `plan/prompts/planning.md` |
| Editor presentation | `nvim/lua/forge/views/harness/checks.lua`, relative to the repository root |

## Integration sequence

Use the pinned Rust toolchain and development target described in
[Building and testing](building-and-testing.md). Discovery fixtures must be local
and must not install project dependencies.

1. **Define the capability.** Specify the manifest format, authoritative workspace
   membership, package identity, deleted-path behavior, and whether exact test
   identities are available. Package-level execution alone does not require a
   native case-runner adapter.
2. **Extend discovery.** Add the manager to `PackageManager` and discovery to
   `PackageCatalog`. Retain normalized workspace-relative identities, reject
   outside-workspace members, and capture every metadata input needed to detect
   changed manifests or lockfiles before execution.
3. **Extend resolution.** Reuse `InvocationResolver` for direct argument expansion
   and deduplication. Keep arguments separate from shell source. Merge identical
   program, argument, and working-directory tuples only within one definition,
   retaining all contributing files and cases.
4. **Integrate exact-case evidence when supported.** Validate the runtime case name
   and target, then require proof that execution ran the selected case. Preserve
   rejection of missing, ignored, ambiguous, or zero-match cases. Use the existing
   helper receipt contract when the runner cannot supply native evidence.
5. **Integrate previews.** Route catalog and plan presentation through the shared
   resolver. Extend plan preview invalidation for any new metadata filenames.
   Keep unresolved metadata pending and never record a preview as a completed run.
6. **Update configuration guidance.** Update setup prompts, schema coverage,
   user-facing configuration examples, and the affected architecture contracts.
   Preserve shell operations and existing scoped helpers.
7. **Verify the boundaries.** Add targeted Rust cases and a fresh-host fixture
   where the public service behavior changes. Follow the project-check commands
   in [Building and testing](building-and-testing.md#verify-project-checks).

## Required failure and resource boundaries

Discovery must reject unsupported membership syntax rather than guess. Node
workspace patterns currently support `*`, `?`, `**`, and leading `!` exclusions.
The scanner skips `node_modules`, `.git`, `target`, and `.forge`, and does not
traverse directory symlinks. It limits traversal to 32 levels, 8,192 directories,
1,024 manifest candidates, and 512 discovered packages. Individual metadata files
are limited to 1 MiB.

Owned discovery processes use no stdin, a 30-second metadata timeout, an 8 MiB
stdout limit, and a 1 MiB stderr limit. Cargo test discovery has a 120-second
timeout. One operation resolves to at most 4,096 invocations. A new adapter must preserve
bounded execution, process cleanup, and cancellation.

| Failure | Required behavior |
| --- | --- |
| Missing or changed metadata | Report pending preview validation or an execution configuration error and resolve again after correction |
| Ambiguous or missing package name | Reject name-based substitution and use a valid directory-based operation where supported |
| File outside discovered membership | Reject ownership instead of assigning an unrelated ancestor package |
| Missing plan inventory | Report unavailable case coverage, distinct from an accepted plan containing zero matching cases |
| Expanded prerequisite fails | Skip dependents until every invocation of the prerequisite passes |
| Native case does not execute | Reject successful process exit as insufficient evidence |

Fixtures must cover single-package and multi-package discovery, exclusions,
deleted paths, deduplication, metadata changes, exact-case failures, and
cancellation where applicable. Report the executed commands and observed results
without treating a preview or configuration save as a test run.

Completion-gate fixtures must distinguish provider-turn completion from actual
idleness. Include intermediate goals, pending questions, cancelled work, explicit
tool runs reused by automatic checks, and a repair ending without progress.
Coverage belongs to the original task or exchange across repair turns. Persist
failure delivery independently of the provider response to avoid repeated repair
requests after recovery.
