---
name: harness-checks
description: >-
  Configure Forge Harness project checks, automatic triggers, package-aware commands,
  and scoped test helpers through checks_configure. Use when adding, editing, or
  repairing .forge.json checks or .forge/checks/*.nu helpers. Do not use for merely
  running existing checks or changing task-specific check selection.
targets:
  - '*'
---

# Harness Checks

Configure checks only through the Harness check tools. Check-editing permission
must be enabled in the current interaction. If disabled, ask the user to enable
it with `/checks setup on`. Skill selection does not grant editing permission.

Inspect project manifests, scripts, CI, test runners, and repository instructions.
For an empty directory, use the user's request to establish tooling. During a
planning prerequisite, limit discovery to checks and retain the original feature
request for Harness to resume after publication.

Infer functional areas, automatic suites, and coverage cost from repository evidence
and the user's request. Preserve explicit choices already made. Ask questions only
when a material decision remains unresolved, not as a prerequisite to saving.
Read `checks_list` for the current revision, definitions, helper identities, and
live automatic-check settings. Preserve unrelated definitions and settings.
Submit through `checks_configure`. Every call presents the configuration and helper
changes for approval before saving in Read and Write modes, even with saved allow
rules. YOLO approves automatically. Rejection leaves the existing files unchanged.
Do not run checks during configuration or write `.forge.json` or helpers through
shell tools. A saved configuration and its command previews do not prove execution.

The checks configuration is a map of names to trigger and action:

```json
{
  "rust-tests": {
    "trigger": {"auto": true, "paths": ["rust/**/*.rs", "rust/**/Cargo.toml"], "exclude": ["**/target/**"]},
    "action": {"directory": "rust", "package_manager": "cargo", "command": ["cargo", "test", "--locked", "--package", "{package}", "--tests"]}
  },
  "web-tests": {
    "trigger": {"paths": ["web/**"], "exclude": ["**/node_modules/**"]},
    "action": {"directory": "web", "package_manager": "npm", "command": ["npm", "test", "--workspace", "{package}"]}
  }
}
```

An empty map is valid without automated checks. Do not add dummy commands.
There is no version, definitions array, kind, scope, or separate all/files/cases
configuration. trigger.auto defaults to false. Empty paths match any changed file.
Paths are project-relative and case-sensitive. `*` and `?` match within a component,
and a whole-component `**` matches zero or more components. Exclude removes matching
paths. Include manifests, lockfiles, generators, and shared inputs when relevant.

An action.command array starts with the executable. A string instead runs in the
configured shell. Only arrays support bindings. `{path}` expands once per selected
file. `{package}` expands to its owning package name. Identical invocations are
merged. A full request expands all discovered packages or existing matching files.

action.package_manager supports cargo, npm, and pnpm and is required for
`{package}`. Declare it per action, including in mixed-language repositories.
Cargo uses workspace metadata, npm uses package.json membership, and pnpm uses
its workspace manifest. action.directory selects the discovery and execution root.
Cargo's leading +toolchain is preserved. Reject unknown or ambiguous packages.
Workspace root manifests and lockfiles may affect all members and need separate
full actions. Package discovery does not infer dependent-package impact.

Optional action fields are label, category, prepare, input, and after. Categories
are format, lint, check, test, and build. Standard Cargo commands infer a category.
Use prepare:true for source-mutating formatters. Successful preparation establishes
the source baseline. action.after names required successful checks. Use it for
actual dependencies such as integration tests consuming a build. After dependencies
and preparation, category priority is format, lint/check, test, then build. Names
break priority ties.

Manual execution names a saved check: `{"check":"rust-tests"}`. Its trigger
selects current task changes. Outside a task, supply an explicit selector. Exactly
one optional override is supported: files, packages, tests, or all:true. Examples
are `{"check":"rust-tests","packages":["engine"]}` and
`{"check":"rust-tests","tests":["tests::example"]}`. Commands cannot be replaced.
Native named tests require Cargo and an unambiguous standard test target. Forge
lists the target and requires the exact test to execute. Unknown, ignored,
ambiguous, and zero-match tests do not count as passing coverage.

For other scoped runners, use input:true and a Nushell helper under .forge/checks/.
Helpers receive version 1 JSON on stdin with check_id, workspace, selection, and
receipt_path. Internal selections use `{kind:"files",files:[...]}`,
`{kind:"packages",packages:[...]}`, `{kind:"tests",tests:[...]}`, or `{kind:"all"}`.
Accepted plans may provide `{kind:"cases",cases:[{file,name,target}]}`. Write a
version 1 receipt with the actual selection. Missing, empty, and broader receipts
fail. Forward runner output and status. Use `def main []` and read `$in` inside
main. Launch as `["nu","--no-config-file","--stdin",".forge/checks/example.nu"]`.
Pass source and expected_digest to checks_configure, null for new helpers.

Keep the project's selected toolchain, profile, features, compiler flags, and
target directory. Prefer development artifacts unless release is required. Avoid
redundant builds, cargo clean, duplicate tests, and silent coverage expansion.
Concrete previews are proposed commands, never execution evidence.
