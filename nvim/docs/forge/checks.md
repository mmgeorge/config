# Project checks

`/checks setup` configures the commands that Harness runs during Checking.
`/checks` shows their selection reasons, proposed commands, and saved results.
Definitions live in the `checks` section of `.forge.json`. Setup preserves other
repository settings and asks for your choices before saving.

Follow-up messages and interrupted setup retain your recorded choices. Setup still
requires a user answer before saving. After configuration is saved, subsequent
messages return to ordinary chat. Cancellation, rollback, or starting another task
does not transfer the earlier setup's approval to that task.

## Automatic and explicit checks

While a check run is active, the status area shows executing checks and their latest
nonempty output line. The line updates as output arrives, and complete logs remain
available in the saved result.
The completion counter includes only selected checks and their required prerequisites.
Unselected catalog entries do not count as finished work or increase the total.
Selected checks that finish with a skip, failure, or configuration error still count
toward progress.
A check expanded across several packages counts once and finishes when all its
commands have settled.
Submitting a new message during checks stops the previous execution and collects
its running checks before starting the message. The composer retains newer drafts
while that interruption finishes.
Completed runs retain the full details. Result summaries omit configuration-error
counts and show a failed count only when more than one check failed.

`enabled` controls whether a check can run. `auto` controls automatic selection
and defaults to `true` when omitted. An enabled check with `auto: false` remains
available for explicit runs and explicit plan selection. Prerequisites run when
required by a selected check, including prerequisites with `auto: false`.

The Auto field in `/checks` changes the task override. Saving defaults writes it
to project configuration. Plan review shows automatic selection reasons and
checks explicitly requested by the plan.

While viewing a plan, press `oc` to open its checks picker. `●` marks an enabled
check and `○` marks a disabled check. Press Space or Enter to toggle the selected
check. These choices apply only to that plan. Enabling a check also enables its
prerequisites, and disabling a prerequisite disables its dependent checks.

To remove checks from the plan view, select their rows visually and press `j`,
the same action used to delete planned tests. Confirming disables those checks
for this plan. Re-enable them through `oc`. Historical and accepted plans remain
read-only.

Agents add existing checks with `plan_checks_add`, supplying `plan_id`,
`expected_version`, and `check_ids`. The tool updates the plan's `checks` list
without changing repository settings or executing commands. Repeated IDs do not
create repeated plan entries, and adding only already-selected checks leaves the
plan version unchanged. The tool is available during plan authoring and Resolve.
`checks_run` accepts multiple `ids`, or
`all: true` for every enabled check. Explicit runs execute even when earlier
evidence remains current.

Agents must run automated validation through the checks interface. Direct
terminal builds, tests, linters, formatters, and validation scripts do not record
coverage and must not replace check runs. During plan implementation and
resolution, agents finish the phase and let Harness Checking run validation.

## Completion and repair

Before ordinary work becomes idle, Harness runs applicable automatic checks.
Intermediate goal turns do not trigger this gate. Successful goal completion
does. Questions, pending approvals, cancellation, and paused work do not start
automatic checks.

Harness retains the inputs and coverage of completed runs. A current recorded
result is reused instead of executing the same check again. Package A's result
does not satisfy package B, and partial selections do not satisfy wider coverage.
Relevant edits, changed commands, or changed configuration require new evidence.

A failed result starts a repair turn with its saved diagnostics and output.
Harness also uses failures already recorded through the run tool without
repeating the unchanged command. If repair ends with the same failures and no
relevant changes, automatic repair stops with the failure unresolved.
Configuration errors require correction through the authorized setup workflow.

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

Agents can call `checks_status` with `wait_for_completion: true` to wait through
progress updates until the run finishes. The default wait is 60 seconds. An
optional `wait_ms` shortens that deadline, and a run still in progress at the
deadline returns its current state. Omitting this option preserves immediate
status reads or waits for the next update.

Execution checks the captured manifests again. A changed manifest requires a new
resolution. A dependent check runs only after every expanded prerequisite passes.
History records each concrete command, its output, and its selected package or
cases. Interruption retains completed output and stops owned processes.

Running checks appear below the main status, alongside ongoing work. The status area
shows the current check names, completion count, and elapsed time. It does not replace
Working or a waiting state. The timeline receives one expandable result when the run
finishes or is interrupted. Open that result for command details and saved output.

Submitting a prompt while checks run interrupts those checks and starts the prompt
after their processes stop. Explicitly queueing a prompt leaves checks running and
waits for completion. A directly submitted prompt takes precedence over queued
prompts, and edits made to the composer during cancellation remain unsent.


Cancelling a check stops its owned process tree. Automatic completion does not retry
the same cancelled checks against unchanged inputs. Run those checks explicitly to
retry, or change their relevant inputs before automatic verification resumes.
If cancellation precedes the first input snapshot, retry explicitly.
Active check status names the run trigger, and an otherwise idle session shows
Checking until the run settles. Result summaries include a single failure as well
as interrupted and blocked checks.
