# Project checks

`/checks setup` configures the commands that Harness runs during Checking.
`/checks` shows their selection reasons, proposed commands, and saved results.
Definitions live in the `checks` section of `.forge.json`. Setup preserves other
repository settings and asks for your choices before saving.

## Tests for changed packages

One definition can run every affected package without naming packages in advance.
This Cargo definition selects each package containing a changed Rust file and runs
its unit and integration test targets:

```json
{
  "id": "package-tests",
  "name": "Package tests",
  "category": "test",
  "area": "Tests",
  "enabled": true,
  "trigger": { "kind": "files_changed", "include": ["**/*.rs"] },
  "scope": "changed_files",
  "all": { "command": "cargo test --workspace --tests" },
  "files": {
    "command": "cargo",
    "args": ["test", "--package", "{package.name}", "--tests"],
    "package_manager": "cargo"
  }
}
```

Add the toolchain, profile, feature, and lockfile flags required by your project.
Five changed files in one package produce one invocation. Changes in two packages
produce two invocations. The details retain every triggering file. Root lockfiles
and workspace manifests require a separate workspace-wide rule instead of being
silently attributed to one package.

The direct form keeps arguments separate from shell source. Available bindings are
`{file.path}`, `{package.name}`, `{package.directory}`, `{package.manifest}`, and
`{workspace.directory}`. File paths are workspace-relative. Directory and manifest
bindings are absolute. The operation's `package_manager` chooses Cargo, npm, or pnpm
workspace membership. Excluded or independent nested packages are not included in
their parent's checks.

For npm workspaces, use `npm run test --workspace={package.name}`. For pnpm
workspaces, use `pnpm --filter {package.name} test`. Directory-based commands also
work for single-package projects: `npm --prefix {package.directory} run test` and
`pnpm --dir {package.directory} test`. These command examples must be represented as
separate `command` and `args` values in direct operations.

## New tests from the plan

The `new_cases` scope selects only test entries marked `new` in the accepted plan.
The `changed_cases` scope selects both new and modified entries. These scopes use
the definition's `cases` operation. They include tests added to existing files.

A native Cargo cases operation selects exact runtime names:

```json
{
  "command": "cargo",
  "package_manager": "cargo",
  "args": [
    "test", "--package", "{package.name}", "{case.target_args}",
    "--", "{case.name}", "--exact"
  ]
}
```

The plan supplies the test file and full runtime name. It may also supply `target`
as `lib`, `bin:<name>`, or `test:<name>` when the file's target is ambiguous. Harness
confirms the name in the target's test inventory before running it. Missing tests,
ignored tests, and successful commands with no executed matching test do not pass.

An accepted plan with no new tests reports that no tests were selected. Outside
plan execution, provide explicit cases or choose a file/package check. Missing
plan inventory is different from an empty inventory.

npm and pnpm do not define individual test identities. Their case-scoped checks
continue to use a configured runner-specific helper and a selection receipt.
Existing Nushell helper checks remain supported.

## Preview and execution

Plan review and `/checks` show resolved commands and why they were selected.
Plans that change package manifests show pending validation until their metadata
exists on disk. Editing selection inputs invalidates an earlier plan preview.
Previews never count as execution evidence.

Execution checks the captured manifests again. A changed manifest requires a new
resolution. A dependent check runs only after every expanded prerequisite passes.
History records each concrete command, its output, and its selected package or
cases. Interruption retains completed output and stops owned processes.
