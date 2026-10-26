# Project checks

`/checks setup` configures the commands that Harness runs during Checking.
`/checks` lists configured checks and their task-specific settings. Expand a check
with `i` to inspect selection reasons, proposed commands, and saved results.
Definitions live in the `checks` section of `.forge.json`. Setup preserves other
repository settings and asks for your choices before saving.

Saved results from an older check-history format remain on disk but are excluded from
the current session history. Plans containing the older check configuration must be recreated.

`/checks setup on` enables check-editing permission without sending a prompt.
`/checks setup off` disables it without sending a prompt, including during active work.
The permission starts off, applies to this Harness conversation, and survives reopening.
The winbar shows `check editing on` or `check editing off`. The same permission is
available in `/config` under **Checks → Setup**, toggled with Space. Guided `/checks setup`
also enables permission, but still asks for your choices before saving. Enabling
permission does not run checks or change which checks are enabled.

Follow-up messages and interrupted setup retain your recorded choices. Setup still
requires a user answer before saving. After configuration is saved, subsequent
messages return to ordinary chat. Cancellation, rollback, or starting another task
does not transfer the earlier setup's recorded answers to that task. Check-editing
permission remains a separate conversation-level setting until you disable it.

## Automatic and explicit checks

In `/config`, select **Checks → Automatic → Enabled** and press Space to disable
or enable completion checks. Tab switches between Enabled and Timing. The setting
applies immediately, including during active work. Disabling completion checks
leaves explicit check runs and plan Checking available.

While a check run is active, the status area shows executing checks and their latest
nonempty output line. The line updates as output arrives, and complete logs remain
available in the saved result.
For an exact test run, the check label includes the fully qualified test name,
including while its executable is compiling. Other queued tests are not shown as running.
Single-file checks show the selected path alongside the configured check name in
live progress and completed history.
Check status and history omit requester labels and show execution progress and results.
The completion counter includes only selected checks and their required prerequisites.
Unselected catalog entries do not count as finished work or increase the total.
Selected checks that finish with a skip, failure, or configuration error still count
toward progress.
A check expanded across several packages counts once and finishes when all its
commands have settled.
Submitting a new message during checks stops the previous execution and collects
its running checks before starting the message. The composer retains newer drafts
while that interruption finishes.
Completed runs retain the full details. Result summaries omit not-run counts while
preserving passed, failed, interrupted, stale, and other error counts.

The transcript groups completed results under **Checks**, then by category and
source file or package/application target. Repeated executions appear as separate
Run rows in their original order. Named Rust tests retain their test names. Lua
runner results describe whole test files, not individual assertions. The Checks
summary counts command executions even when one command reports several files.
Expand an execution to inspect its diagnostics. Its output action opens the full
command log. Invalid file reports keep the command result and show the report error.

`trigger.auto` controls automatic selection and defaults to `false`. Every configured
check remains available for explicit runs and explicit plan selection. Prerequisites
run when required by a selected check, including checks with automatic selection off.

In `/checks`, `●` marks an enabled check and `○` marks a disabled check. Space
toggles the selected check. The Auto field controls automatic selection with the left/right arrows. These changes affect task overrides. Enter does not run
checks. The History page retains previous runs. Use `/checks setup`
to edit project defaults. Plan review shows automatic selection reasons and checks
explicitly requested by the plan.

If the host returns an incompatible or malformed checks configuration, the picker
shows the error and retains its last valid rows with editing disabled. A schema
mismatch requires the current Forge executable and configuration. Rebuild and
restart Forge after changing the checks schema, then update `.forge.json`.

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
`checks_run` accepts `check` as a name or a list of names. Omit coverage overrides
to use each check's trigger and current changes. Supply exactly one of `files`,
`packages`, `tests`, or `all: true` to override coverage. Explicit runs execute
even when earlier evidence remains current.
Configured checks run without executable approval prompts, whether started by an
agent, the checks picker, or an automatic gate. Agents select saved check IDs and
coverage rather than supplying commands. Editing check definitions still requires
check-editing permission and write authorization.

Agents can stop a running check with `checks_cancel`, supplying its `run_id`
from `checks_run` or `checks_list`. Cancellation preserves output and check
history. It stops that run without disabling future automatic checks.

`checks_list` returns the live `automatic_checks` boolean and `check_timing`
(`after_queue` or `between_work`) alongside the catalog. Agents read these fields
before deciding whether to run checks manually. When enabled, they normally
rely on the completion gate and may call `checks_run` for earlier feedback. When
disabled, they explicitly run applicable checks if their changes need verification.
Direct
terminal builds, tests, linters, formatters, and validation scripts do not record
coverage and must not replace check runs. During plan implementation and
resolution, agents finish the phase and let Harness Checking run validation.

## Execution order

Harness runs selected source preparation and its transitive prerequisites before
unrelated verification. Among checks whose prerequisites have been ordered, the
default priority is **format → lint/check → test → build**. Lint and check share
one priority. Definition order breaks ties.

Explicit `action.after` prerequisites take precedence over category priority.
A host integration test remains a `test` check and depends on the `build` check
whose executable it launches. Independent unit tests run before that build.
Categories do not create success dependencies or select additional checks.
A failed independent test therefore does not suppress a build unless the build
explicitly depends on it.

Use `action.prepare: true` for source-changing commands and reserve `action.after`
for checks that require a successful prerequisite. Unselected preparation does
not promote its prerequisites ahead of selected verification. Preparation that
requires verification can still invalidate that prerequisite's evidence when it
changes source. Keep those dependencies only when the prerequisite is required.

## Completion and repair

In `/config` under **Checks → Automatic**, **Enabled** starts on. Space toggles
it using `●` for enabled and `○` for disabled. Turning it off suppresses automatic
completion checks. Explicit runs and plan Checking remain available. An already
running check finishes unless stopped explicitly.

The **Timing** field selects **After queue** by default and retains its value
when automatic checks are disabled.
Harness runs applicable automatic checks after the last queued work item finishes.
Select **Between queued work** to run them after each item before starting the next.
Both settings save global defaults and can change while work is running without
interrupting the model. Already running checks finish before queued work resumes.
Removing the remaining queued items also releases the pending completion check.
The final check includes changes from the entire queue, using its first change baseline.

When enabled, Harness runs applicable automatic checks before ordinary work becomes idle.
Intermediate goal turns do not trigger this gate. Successful goal completion
does. Questions, pending approvals, cancellation, and paused work do not start
automatic checks.

Harness retains the inputs and coverage of completed runs. A current recorded
result is reused instead of executing the same check again. Package A's result
does not satisfy package B, and partial selections do not satisfy wider coverage.
Relevant edits, changed commands, or changed configuration require new evidence.

A check run attaches to the exchange whose work it validates. Its saved results
remain with that exchange on reload. A failed result starts a repair turn within
the same exchange, retaining the original prompt, change baseline, and accumulated
provider usage. Repair does not create a separate conversation entry.
The model corrects the failure and finishes its turn so Harness checks again.
Harness also uses failures already recorded through the run tool without
repeating the unchanged command. If repair ends with the same failures and no
relevant changes, automatic repair stops with the failure unresolved.
Configuration errors require correction through the authorized setup workflow.

Actions verify their inputs by default. Use `action.prepare: true` for commands
that intentionally change source, such as a formatter in fix mode. A successful
preparation records its resulting source as the baseline. Dependent checks run
against that source. Source changes during verification produce `Stale`, and
automatic checking verifies the current inputs again without asking the model
to repair a successful command. Later preparation also invalidates earlier
verification when it changes those inputs.

Compiler failures during test discovery remain failed checks with their full
compiler output. Invalid definitions and selectors remain configuration errors.
Unavailable executables block execution, and storage or internal failures appear
as execution errors. Explicit cancellation suppresses automatic retries of
unchanged inputs with `Stopped`. An explicit run or relevant source changes
permit another attempt.

## Trigger and action configuration

The `checks` object maps check names to one trigger and one action. No version,
separate scope, or `all`/`files`/`cases` command templates are required.

```json
{
  "checks": {
    "rust-tests": {
      "trigger": {
        "auto": true,
        "paths": ["rust/**/*.rs"],
        "exclude": ["rust/**/target/**"]
      },
      "action": {
        "directory": "rust",
        "package_manager": "cargo",
        "command": ["cargo", "test", "--package", "{package}", "--tests"]
      }
    },
    "web-tests": {
      "trigger": {"paths": ["web/**/*.ts"]},
      "action": {
        "directory": "web",
        "package_manager": "npm",
        "command": ["npm", "test", "--workspace={package}"]
      }
    }
  }
}
```

`{path}` runs the action for each selected file. `{package}` runs it once per
owning package. Repeated files and packages are deduplicated. Without either
placeholder, the action runs once. Substitutions remain individual process
arguments rather than executable shell text. A command string is available for
commands that deliberately require the configured shell.

`action.package_manager` is required for package substitution or package
selection. Supported managers are Cargo, npm, and pnpm. Each check discovers
membership within its own action directory, so different package managers and
nested workspaces can coexist. Unknown managers, missing package context, unknown
package names, and ambiguous names are errors. Workspace-wide manifests and
lockfiles need a separate workspace action.

Triggers use repository-relative paths. The action directory is also relative
to the repository. File substitutions resolve correctly from that directory.
An omitted `trigger.auto` is false. An omitted path list imposes no path filter.

Optional action fields are `label` for display, `category` for custom command
ordering, `prepare` for source mutation, and `after` for prerequisites. Standard
Cargo commands infer their category. `input: true` sends a selection request to
stdin for a custom helper, which must return a matching selection receipt.

## Explicit coverage

A check name alone uses current changes:

```json
{"check": "rust-tests"}
```

Exactly one override can replace that selection:

```json
{"check": "rust-tests", "files": ["rust/src/lib.rs"]}
{"check": "rust-tests", "packages": ["engine"]}
{"check": "rust-tests", "tests": ["tests::updates_scene"]}
{"check": "rust-tests", "all": true}
```

Cargo test actions derive the package, executable target, exact filter, and
inventory command internally. Missing or ambiguous test names fail before
execution. A passing exit with zero matching tests or an ignored test does not
prove coverage. The configured toolchain, profile, features, and other build
flags remain in effect. npm and pnpm package checks run their configured command.
Named individual tests currently require Cargo's standard test harness.

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
`Ctrl-C` with an empty composer stops running checks and pauses work. `Ctrl-S`
submits the composer even while that pause is pending, after cleanup finishes.
Closing the runtime during checks retains interrupted results when the session reopens.
Checks do not restart automatically. If checkpoint saving was interrupted, the next prompt
finishes that save before starting new work. A save failure retains the previous exchange
and reports the underlying error.


Cancelling a check stops its owned process tree. Automatic completion does not retry
the same cancelled checks against unchanged inputs. Run those checks explicitly to
retry, or change their relevant inputs before automatic verification resumes.
If cancellation precedes the first input snapshot, retry explicitly.
Active check status names the run trigger, and an otherwise idle session shows
Checking until the run settles. Result summaries include a single failure as well
as interrupted and blocked checks.
