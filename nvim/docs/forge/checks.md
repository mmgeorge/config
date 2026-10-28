# Project checks

`/checks` lists the commands Harness runs during Checking and their task-specific settings. Expand a check
with `i` to inspect selection reasons, proposed commands, and saved results.
Definitions live in the `checks` section of `.forge.json`. Setup preserves other
repository settings and presents the proposed configuration and helper changes
for approval before saving.

Saved results from an older check-history format remain on disk but are excluded from
the current session history. Plans containing the older check configuration must be recreated.

`/checks edit on` enables check-editing permission without sending a prompt.
`/checks edit off` disables it without sending a prompt, including during active work.
The permission starts off, applies to this Harness conversation, and survives reopening.
The winbar appends gray `ce` to the permissions code while enabled, such as
`rxwxce` or `rxwxaxce`. The same permission is
available in `/config` under **Checks → Setup**, toggled with Space. Enabling
permission does not run checks, send a prompt, or change which checks are enabled.
Request configuration changes in ordinary chat, such as "Configure targeted checks
for this repository." The `harness-checks` skill supplies discovery, schema, and
scoped-runner guidance through the shared Rulesync skill catalog. Bare
`/checks edit` and `/checks edit <check-id>` are not commands.

`check_definition_edit` follows the selected approval mode after saved rules and
filesystem access checks. Ask opens a popup for requests that require approval,
Deny rejects them, and Allow approves them once without saving a grant. The popup
shows the configuration and managed helper changes. Approve publishes that exact
update, while rejection, cancellation, or timeout leaves the existing files unchanged.
Write-off rejects publication in every approval mode.
Setup questions are optional and do not authorize publication.

Planning still configures missing or invalid checks before generating the plan.
That prerequisite uses the same skill and publication approval, then resumes the
original planning request. Check-editing permission remains a separate
conversation-level setting until you disable it.

## Automatic and explicit checks

Compiler/type checks run before tests, followed by read-only formatting, linting,
and build. Configure `cargo check` and TypeScript's project type-check command in
category `check`. Source-changing preparation and explicit prerequisites run
before checks that require their results. Formatting validation does not modify
the source that was tested.

In `/config`, select **Checks → Automatic → Enabled** and press Space to disable
or enable completion checks. Tab switches between Enabled and Timing. The setting
applies immediately, including during active work. Disabling completion checks
leaves explicit check runs and plan Checking available.

While a check run is active, the status area shows executing checks and their latest
nonempty output line. The line updates as output arrives, and complete logs remain
available in the saved result.
For planned-case context, the check label includes the selected test name.
Explicit inputs appear in the frozen invocation and expanded details.
Single-file checks show the selected path alongside the configured check name in
live progress and completed history.
Check status and history omit requester labels and show execution progress and results.
The completion counter includes only selected checks and their required prerequisites.
Unselected catalog entries do not count as finished work or increase the total.
Selected checks that finish with a skip, failure, or configuration error still count
toward progress.
A callback can run several packages, but Harness records its configured invocation
once and waits for that process to settle.
Submitting a new message during checks stops the previous execution and collects
its running checks before starting the message. The composer retains newer drafts
while that interruption finishes.
Completed runs retain the full details. Result summaries omit not-run counts while
preserving passed, failed, interrupted, stale, and other error counts.

The transcript groups completed results under **Checks**, then by category and
source file or configured definition. Repeated executions appear as separate
Run rows in their original order. Repository callbacks can report individual
files through the generic result protocol. The Checks
summary counts command executions even when one command reports several files.
Expand an execution to inspect its diagnostics. Its output action opens the full
command log. Invalid file reports keep the command result and show the report error.

`trigger.auto` controls automatic selection and defaults to `false`. Every configured
check remains available for explicit runs and explicit plan selection. Prerequisites
run when required by a selected check, including checks with automatic selection off.

In `/checks`, `●` marks an enabled check and `○` marks a disabled check. Space
toggles the selected check. The Auto field controls automatic selection with the left/right arrows. These changes affect task overrides. Enter does not run
checks. The History page retains previous runs. Enable check editing and request
changes in chat to edit project defaults. Plan review shows automatic selection reasons and checks
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
`check_run` accepts `check` as a name or a list of names. Supply `inputs` as an
object matching each check's declared input contract, or omit it to use defaults
and automatic context bindings. `all: true` requests full context and cannot be
combined with `inputs`. Explicit runs execute even when earlier evidence remains current.
Configured checks run without executable approval prompts, whether started by an
agent, the checks picker, or an automatic gate. Agents select saved check IDs and
coverage rather than supplying commands. Editing check definitions still requires
check-editing permission and write authorization.

Agents can stop a running check with `check_cancel`, supplying its `run_id`
from `check_run` or `check_list`. Cancellation preserves output and check
history. It stops that run without disabling future automatic checks.

`check_definition_list` returns the live `automatic_checks` boolean and `check_timing`
(`after_queue` or `between_work`) alongside the catalog. Agents read these fields
before deciding whether to run checks manually. When enabled, they normally
rely on the completion gate and may call `check_run` for earlier feedback. When
disabled, they explicitly run applicable checks if their changes need verification.
Direct
terminal builds, tests, linters, formatters, and validation scripts do not record
coverage and must not replace check runs. During plan implementation and
resolution, agents finish the phase and let Harness Checking run validation.

## Execution order

Harness runs selected source preparation and its transitive prerequisites before
unrelated verification. Among checks whose prerequisites have been ordered, the
default priority is **check → test → format → lint → build**. Compiler/type
checks belong to `check`, and formatting validation is read-only. Definition
order breaks ties within each category.

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
result is reused instead of executing the same check again. Different input maps
and selections require different evidence. Any workspace edit, changed command,
or changed configuration requires new evidence.

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

Nonzero command exits remain failed checks with their complete output.
Invalid definitions and input values remain configuration errors.
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
        "auto": false,
        "paths": ["rust/**/*.rs"],
        "exclude": ["rust/**/target/**"]
      },
      "action": {
        "directory": "rust",
        "category": "test",
        "command": ["cargo", "test", "--package", "{package}", "--lib", "--", "{test}", "--exact"],
        "inputs": {
          "package": {"type": "string", "description": "Package name", "required": true},
          "test": {"type": "string", "description": "Exact test name", "required": true}
        }
      }
    },
    "web-tests": {
      "trigger": {"paths": ["web/**/*.ts"]},
      "action": {
        "directory": "web",
        "category": "test",
        "command": ["npm", "test", "--workspace", "{package}"],
        "inputs": {
          "package": {"type": "string", "description": "Workspace package", "required": true}
        }
      }
    }
  }
}
```

Each action runs once. An argv element equal to `{name}` substitutes a declared
scalar input or expands an array into separate scalar arguments. Embedded text
such as `prefix-{name}` is literal. Object inputs are available to callbacks,
not argv templates. Input names have no reserved language or runner meaning.
The executable remains fixed, and input values never become interpolated shell
source. Shell-string actions with declared inputs must use `input: true`.

Input declarations require `type` and `description`. Supported types are `string`,
`integer`, `number`, `boolean`, `array`, and `object`. `required` defaults to false.
Optional `default` values must match the declared type. Optional `source` binds
automatic context through `changed_files`, `planned_cases`, or `workspace`.
Explicit values override context bindings and defaults. Missing required inputs,
unknown names, and incompatible types fail before starting the command.

Harness treats every command as opaque. It does not recognize package managers,
discover tests, infer arguments, or interpret runner stdout. Repository callbacks
own file-to-package mapping, executable selection, and zero-match detection.

Triggers use repository-relative paths. The action directory is also relative
to the repository. Substitution preserves input values unchanged. The configured
command or callback resolves repository-relative paths from its working directory.
An omitted `trigger.auto` is false. An omitted path list imposes no path filter.

Optional action fields are `label` for display, `category` for custom command
ordering, `prepare` for source mutation, and `after` for prerequisites. The default
category is `check` for every executable. `input: true` sends a JSON request to
stdin for a callback, which must return a matching selection receipt on success.

## Explicit coverage

A check name alone uses current changes:

```json
{"check": "rust-tests"}
```

Named values can be supplied together according to the check's declaration:

```json
{"check": "rust-tests", "inputs": {"package": "engine", "test": "tests::updates_scene"}}
{"check": "rust-tests", "all": true}
```

The example template still needs its required values when `all` is selected.
Full coverage is not an alternative command synthesized by Harness. Use a
callback or a separate configured action when full and targeted commands differ.

This repository uses one `lua-test` definition for ordinary Lua coverage.
Omit inputs for changed-file mapping, supply `files` for explicit paths, select
a complete feature `suite`, or name one `fixture`:

```json
{"check":"lua-test","inputs":{"fixture":"nvim/tests/forge/plan_check_selection.lua"}}
{"check":"lua-test","inputs":{"suite":"walkthrough"}}
```

`suite` defaults to `affected` and `fixture` defaults to an empty string.
An exact fixture cannot be combined with a named or full suite. Existing feature
names remain in the repository runner, not separate saved definitions.
`lua-test-host` and `lua-benchmark` remain explicit checks with a
`rust-build` prerequisite. The ordinary callback rejects those suite names and
build-dependent fixtures rather than bypassing that prerequisite.
Supply explicit `files` to `lua-test-host` or `lua-benchmark` to select individual
fixtures while retaining the build prerequisite. Omitting inputs keeps the full
saved suite.

Check IDs use language/tool-first names, while display labels name the action.
`rulesync-local-preview` and `rulesync-global-preview` only preview generation.
Their manual `rulesync-local-update` and `rulesync-global-update` counterparts
write generated files and declare source preparation. Each update requires its
matching preview to pass. Updates never run automatically.

Definition lists group check, test, format, lint, and build categories, with IDs
breaking ties. Execution retains prerequisite ordering and runs selected source
preparation before unrelated verification. A required build can therefore run
before its host-integration test.

## Callback request and result

Callbacks receive `version: 1`, `check_id`, `workspace`, resolved `inputs`,
`changed_files`, `selection`, `receipt_path`, and `report_path` on stdin.
`selection` records full, changed-file, planned-case, or explicit-input context.
On success, write `{"version":1,"selection":<request.selection>}` to `receipt_path`.
Helpers may narrow changed-file coverage to planned cases under the existing
receipt contract, but cannot claim broader coverage or change explicit inputs.

An optional generic report at `report_path` has `version: 1` and a `result` array.
Each result contains `file` (repository-relative), terminal `state`, optional
`duration_ms`, `command`, and complete `output`. Harness reads this explicit report,
not stdout. Malformed reports fail, and a callback cannot report failures while
claiming an overall successful result. Ordinary command output remains preserved.

The Rust callback streams Cargo stdout and stderr while retaining both for the
final report. A running check displays the latest nonempty output line beneath
its title, including compiler progress before the test executable starts. Earlier
output remains in the log rather than accumulating in the status area.

## Preview and execution

Plan review and `/checks` show resolved commands and why they were selected.
Missing required inputs show unresolved previews. Previewing never starts the
configured command or a discovery subprocess. Editing inputs invalidates a prior preview.
Previews never count as execution evidence.

Agent tools separate saved definitions from execution. The source contracts live
in `checks/response.rs` and `control_tools/mod.rs` in `forge-harness`.

| Tool | Request and response |
| --- | --- |
| `check_definition_list` | No arguments. Get revision, automatic policy, and minimal definitions with input descriptions, without history or previews. |
| `check_definition_details` | Supply `check_id`. Get one complete saved trigger/action definition and its revision. |
| `check_definition_edit` | Supply `expected_revision`, complete `config`, and optional managed `helpers`. Get revision, changed check/helper names, and `checks_executed:false`. |
| `check_run` | Supply `check` and optional `inputs` or `all`. Every admitted run immediately gets only `run_id`, even if execution finishes immediately. |
| `check_list` | No arguments. Get `runs`, containing compact status for all currently running IDs in this session. Completed history is excluded. |
| `check_status` | Supply `run_id` and optional `wait_ms` from 0 to 60000, default 0. Get compact status, never logs or saved configuration. |
| `check_detail` | Supply `run_id`, optional `stream` (`combined`, `stdout`, or `stderr`), and optional `cursor`. Get completed-command metadata and paginated output. |
| `check_cancel` | Supply `run_id`. Get `cancellation_requested:true`, not a claim that process cleanup has finished. |

Use `check_definition_list` to choose a saved check and
`check_definition_details` to inspect its caller contract. Normal execution uses
`check_run` followed by `check_status`. Use `check_list` to recover running IDs,
`check_cancel` to request cancellation, and `check_detail` for completed logs.
Definition `check_id` values identify saved configuration. Execution `run_id`
values identify admitted jobs and are not interchangeable with definition IDs.

`check_status` includes `state`, `completed`, `total`, `elapsed_ms`, `exit_code`,
`latest_output`, `error`, and `executed`/`reused` counts. `latest_output` is one
nonempty line with terminal escapes removed, or null before output arrives.
The aggregate exit code is null for multiple selected commands. Waiting ignores
progress updates and ends on completion or the original deadline. A deadline
returns the current summary without cancelling work.

Definition details are available before execution. Execution details remain
unavailable until the run finishes, including failed, interrupted, and blocked
runs. A running detail request returns `Still running. Use check_status to wait.`
without opening log files. Unknown run IDs fail explicitly.

Completed detail pages contain a command's executable and argv, working directory,
resolved inputs, exit status, duration, and reuse origin. The returned `text` uses
the requested stream. Pages use a 16 KiB read budget and preserve UTF-8 boundaries.
Follow `next_cursor` with the same `run_id` and `stream` until null. The cursor's
`command_index` identifies a selected command in the frozen run, and `offset`
counts bytes within that command's log. Pagination traverses every selected
command and preserves the complete log without imposing a total-content limit.
Missing logs for started commands, invalid cursors, and decoding failures report
errors. Unstarted commands with no log return empty text.

The editor's internal `checks.list` and `checks.status` service routes retain
their full catalog and run shapes. The agent tools do not serialize those
snapshots during polling.

Evidence includes the definition, resolved input values, context, helper content,
and workspace snapshot. Different inputs cannot reuse each other's evidence.
A dependent check runs only after its prerequisites pass. History records the
concrete command and complete output. Interruption stops owned processes.

Running checks appear below the main status, alongside ongoing work. The status area
shows the current check names, completion count, and elapsed time. It does not replace
Working or a waiting state. The timeline receives one expandable result when the run
finishes or is interrupted. Open that result for command details and saved output.

Submitting a prompt while checks run interrupts those checks and starts the prompt
after their processes stop. Explicitly queueing a prompt leaves checks running and
waits for completion. A directly submitted prompt takes precedence over queued
prompts, and edits made to the composer during cancellation remain unsent.
`Ctrl-C` stops running checks and pauses work without submitting the composer or
queued messages. `Ctrl-S`
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
