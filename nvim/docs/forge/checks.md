# Project checks

`/checks` lists the commands Harness runs during Checking and their task-specific settings. Expand a check
with `i` to inspect selection reasons, proposed commands, and saved results.
Definitions live in the `checks` section of `.forge.json`. Setup preserves other
repository settings and presents the proposed configuration and helper changes
for approval before saving.

Saved results from an older check-history format remain on disk but are excluded from
the current session history. Preserve authored plans when updating unsupported check
configuration; disposable execution records do not migrate between state versions.

`/checks edit on` enables check-editing permission without sending a prompt.
`/checks edit off` disables it without sending a prompt, including during active work.
The permission starts off, applies to this Harness conversation, and survives reopening.
The winbar appends gray `ce` to the permissions code while enabled, such as
`rxwxce` or `rxwxaxce`. The same permission is
available in `/config` under **Workflow → Checks → Setup**, toggled with Space. Enabling
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
category `typecheck`. Every action declares its category; the old `check` spelling
is rejected. Typecheck, test, format, lint, and build require adapters. Only
`custom` accepts a plain command; it rejects adapters. Invalid combinations name
the check and point to the adapter catalog in `check_definition_list`.
Source-changing preparation and explicit prerequisites run
before checks that require their results. Formatting validation does not modify
the source that was tested.

In `/config`, select **Workflow → Checks → Automatic → Enabled** and press Space to disable
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
A custom command can run several packages, but Harness records its configured invocation
once and waits for that process to settle.
Submitting a new message during checks stops the previous execution and collects
its running checks before starting the message. The composer retains newer drafts
while that interruption finishes.
Completed runs retain the full details. Result summaries omit not-run counts while
preserving passed, failed, interrupted, stale, and other error counts.

The transcript groups completed results under **Checks**, then by category and
source file or configured definition. Repeated executions appear as separate
Run rows in their original order. Adapters report category-specific results.
The Checks summary counts command executions independently of their result rows.
Expand an execution to inspect its diagnostics. Its output action opens the full
command log.

Test checks configured with an adapter report individual cases. Their live status
shows the finished and planned case counts, failed cases, and the current phase.
Completed cases are grouped by the parent labels supplied by the adapter; Harness
does not interpret test IDs as paths. Failures, not-found items, and cases that did
not finish appear before a collapsed **Passed and skipped** group. Cases retain
their own status even when a later runner error fails the whole check.

A case's output action opens only that case's captured output. Failure locations
open the reported source line, and truncated output shows the omitted byte count.
If a retained output file is no longer available, the case remains visible and
its output explains the read failure. Runner errors have a separate result row
whose output action opens the complete command log.

`enabled` beside `trigger` and `action` defaults to `true`. Set it to `false` to
disable a check by default; a task can explicitly enable it through `/checks`.
`trigger.auto` controls automatic selection and defaults to `false`. Enabled
checks remain available for explicit runs and explicit plan selection. Prerequisites
run when required by a selected check, including checks with automatic selection off.

In `/checks`, `●` marks an enabled check and `○` marks a disabled check. Space
toggles the selected check. The Auto field controls automatic selection with the left/right arrows. These changes affect task overrides. Enter does not run
checks. Press `i` to expand configuration fields beneath the selected check.
Field names follow the saved configuration (`adapter`, `command`, `after`,
`paths`, and `exclude`), with adapter options indented and list entries aligned
in the value column. Long values wrap without truncation. Empty lists and the
default nextest profile and disabled `all_features` option are omitted. Directory
and automatic selection remain in the main row. The expansion contains no run
history, selection reasons, coverage, or output paths.
The History page retains previous runs. Enable check editing and request
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
For test checks, use the left and right arrows to switch the Tests field between
Planned and Whole suite. Planned runs only the required cases owned by that
check; Whole suite runs all its tests. The choice is saved with this plan and
does not enable a disabled check. Press `?` for picker controls.

To remove checks from the plan view, select their rows visually and press `j`,
the same action used to delete planned tests. Confirming disables those checks
for this plan. Re-enable them through `oc`. Historical and accepted plans remain
read-only.

During execution, Check resolves the plan's new, modified, and reused tests to
exact case IDs and runs those cases. Every required case must pass; a skipped
case does not satisfy the plan even if its check passes. Removed tests require
exact discovery to confirm their absence. Missing or ambiguous tests enter
Resolve. With Planned selected, a test check owning no required cases is excluded
after resolution. Whole suite still runs an enabled check with no planned cases,
and every required planned case must pass even when the whole suite runs.
Each Check retry resolves the tests again and records a new attempt. Results from
an interrupted attempt remain in history and do not satisfy the retry.

When exact discovery omits a source location, Check may supplement it through
static discovery. It matches the same item ID within the same check, preserves
exact locations, and never adds static-only cases to the executable inventory.
Unavailable static locations leave ownership unverified and retain a diagnostic.
The execution report in plan review shows each resolved check and case, its
source provenance, and its result. Verified removals display “Removal verified.”

During planning, agents select existing checks by listing their IDs, one per line,
in the planning directory's `docs/checks.md` with their file-editing tool. A listed ID adds that check to the
plan without changing repository settings or executing commands, and Harness
adds each listed check's prerequisites itself. Non-test checks also follow their
automatic triggers; test checks follow the planned inventory or Whole suite.
`plan_submit` reports a listed ID that names no configured check at its line.
An empty artifact clears additions while automatic non-test checks still apply.
Editing is available during plan authoring and
Resolve; Resolve retains `plan:checks.md` and `plan_edit`.

`plan_submit` uses static discovery to check modified, reused, and removed test
entries against enabled test adapters. Warnings identify the line in
`plan:tests.md`, including ambiguous names across checks, missing tests, unsupported
target selectors, and unavailable discovery. New tests wait for implementation.
Static discovery does not verify removal or create passing test evidence; these
warnings permit submission and do not replace execution-time checking.

The Tests section retains these warnings when you reopen the review. Each warning
keeps its submitted file, test name, change kind, and `docs/tests.md` line. Opening
the review compares the captured source, configuration, helper content, task
preferences, and reviewer choices. Changed inputs mark the warnings stale; a
comparison failure is shown without discarding them. The comparison describes
the inputs when that review opened. Resubmission replaces the warnings, and an
empty test inventory clears them.

`check_run` accepts `check` as a name or a list of names. Supply `inputs` as an
object matching each check's declared input contract, or omit it to use defaults
and automatic context bindings. Test adapters accept `tests` with item IDs or
affected paths. Typecheck, lint, and build adapters accept `paths`, a list of
repository-relative paths. `all: true` requests full context; `inputs`, `tests`,
`paths`, and `all` are mutually exclusive. Explicit runs execute even when earlier evidence remains current.
Configured checks run without executable approval prompts, whether started by an
agent, the checks picker, or an automatic gate. Agents select saved check IDs and
coverage rather than supplying commands. Editing check definitions still requires
check-editing permission and write authorization.

Agents can stop a running check with `check_cancel`, supplying its `run_id`
from `check_run` or `check_list`. Cancellation preserves output and check
history. It stops that run without disabling future automatic checks.

`check_discover` lists a test adapter's item IDs without executing test cases.
It honors task enable/disable choices, including enabling a definition whose
configured default is disabled. Changing task preferences during discovery
invalidates its result.
Static discovery is the default; exact discovery may build and list test binaries
and requires the same execution authority as verification. Discovery occupies the
session's check-operation slot until all its processes are collected, including
on cancellation or timeout. It creates no check-run history or passing evidence.
If source files, check configuration, or managed helpers change during discovery,
the result is rejected and the request must be repeated against the new workspace.

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

Planned Check runs share one tool timeout from admission through discovery,
prerequisites, execution, and final validation. Expiration fails the run and stops
further launches. Harness collects running processes before releasing ownership
and keeps completed case results; cleanup may extend beyond the timeout.
Other check runs use a separate timeout for each command.

Harness runs selected source preparation and its transitive prerequisites before
unrelated verification. Among checks whose prerequisites have been ordered, the
default priority is **typecheck → test → format → lint → build → custom**. Compiler/type
checks belong to `typecheck`, and formatting validation is read-only. Definition
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

In `/config` under **Workflow → Checks → Automatic**, **Enabled** starts off. Space toggles
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
command result is reused instead of executing the same check again. Script-adapter
results are not reused across runs until downstream tool identities can be verified;
they still count for the workspace snapshot they checked. Different input maps
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

Actions verify their inputs by default. Use `action.prepare: true` for custom commands
that intentionally change source, such as configuration generation. Format and
lint adapters use their explicit apply operation. A successful
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
    "artifact-preview": {
      "trigger": {
        "auto": false,
        "paths": ["rust/**/*.rs"],
        "exclude": ["rust/**/target/**"]
      },
      "action": {
        "directory": "rust",
        "category": "custom",
        "command": ["cargo", "package", "--list", "--package", "{package}"],
        "inputs": {
          "package": {"type": "string", "description": "Package name", "required": true}
        }
      }
    },
    "web-package-preview": {
      "trigger": {"paths": ["web/**/*.ts"]},
      "action": {
        "directory": "web",
        "category": "custom",
        "command": ["npm", "pack", "--dry-run", "--workspace", "{package}"],
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
such as `prefix-{name}` is literal. Arguments accept scalar values or arrays of
scalars; objects cannot be bound to argv. Input names have no reserved language or runner meaning.
The executable remains fixed, and input values never become interpolated shell
source. Shell-string actions cannot declare named inputs; use an argv command
when inputs are needed. Custom commands receive closed stdin.

Input declarations require `type` and `description`. Supported types are `string`,
`integer`, `number`, `boolean`, `array`, and `object`. `required` defaults to false.
Optional `default` values must match the declared type. Optional `source` binds
automatic context through `changed_files` or `workspace`.
Explicit values override context bindings and defaults. Missing required inputs,
unknown names, and incompatible types fail before starting the command.

The removed `planned_cases` source reports a migration error. Planned tests use
test adapter IDs; custom commands cannot consume the plan's test inventory.

Harness treats every command as opaque. It does not recognize package managers,
discover tests, infer arguments, or interpret runner stdout. A custom command's
exit status determines its result; structured results require an adapter.

Triggers use repository-relative paths. The action directory is also relative
to the repository. Substitution preserves input values unchanged. The configured
command resolves repository-relative paths from its working directory.
An omitted `trigger.auto` is false. An omitted path list imposes no path filter.

Every action requires `category`. Optional fields are `label` for display,
`prepare` for custom source mutation, and `after` for prerequisites.
The removed `input: true` callback mode fails with a named migration error.
Custom commands do not produce selection receipts or generic file reports.

## Explicit coverage

A check name alone uses current changes:

```json
{"check": "artifact-preview"}
```

Named values can be supplied together according to the check's declaration:

```json
{"check": "artifact-preview", "inputs": {"package": "engine"}}
```

The example template needs its required `package` value for every invocation.
Custom inputs follow the saved declaration; they do not acquire the adapter's
test-selection semantics.

The repository provides two Lua checks using the same test-selection contract.
`lua-test` runs cases that do not launch Forge, while `lua-host-test` runs cases
marked as requiring the Forge executable. Mock-only integration tests belong to
`lua-test` alongside unit tests.

```json
{"check":"lua-test"}
{"check":"lua-test","tests":{"ids":["nvim/lua/forge/tests/unit/views/plan_review/checks.test.lua"]}}
{"check":"lua-test","tests":{"affected":["nvim/lua/forge/views/plan_review/checks.lua"]}}
{"check":"lua-host-test","tests":{"ids":["nvim/lua/forge/tests/integration/client/client.test.lua::receive_initialize"]}}
```

Only `lua-host-test` declares `after: ["rust-build"]`, so Harness builds the
Forge host before those cases and skips them when the build fails. `lua-test`
has no build prerequisite. Select both checks to cover both partitions of an
affected source area. The manual runner without a partition still runs both.

If Harness skips the implicit build because no changed Rust paths match, select
both checks explicitly: `{"check":["rust-build","lua-host-test"],"all":true}`.
This runs the complete host suite; a skipped dependency is not a passing build.

Check IDs use language/tool-first names, while display labels name the action.
`rulesync-local-preview` and `rulesync-global-preview` only preview generation.
Their manual `rulesync-local-update` and `rulesync-global-update` counterparts
write generated files and declare source preparation. Each update requires its
matching preview to pass. Updates never run automatically.

Definition lists group typecheck, test, format, lint, build, and custom categories, with IDs
breaking ties. Execution retains prerequisite ordering and runs selected source
preparation before unrelated verification. A required build can therefore run
before its host-integration test.

## Preview and execution

Plan review and `/checks` show resolved commands and why they were selected.
Missing required inputs show unresolved previews. Previewing never starts the
configured command or a discovery subprocess. Editing inputs invalidates a prior preview.
Previews never count as execution evidence.

Agent tools separate saved definitions from execution. The source contracts live
in `tools/checks/`, with shared execution and result models in `checks/`.

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

For format and lint adapters, `check_run` accepts `apply: true` to run the tool's
fix operation. Name each check explicitly; `all: true` selects all source work
for those checks. A batch containing any other category is rejected before launch.
Only named checks run fixes; adapter prerequisites run read-only. A failed
prerequisite or fix stops later operations, retaining any partial edits and the
results already recorded. Apply is available in ordinary chat and plan Resolve;
plan Verify permits read-only checks. Apply records preparation rather than
verification, and creates a whole-check verification requirement that survives
restart and remains mandatory when automatic checks are disabled.
Configured or task disabling leaves that requirement unresolved. Re-enable the
check and run it again with whole-check coverage. Changes to automatic selection,
scope, trigger, or display labels do not change the required tool identity;
changes to the ID, category, directory, runner, prerequisite IDs, or
helpers require restoring the original execution definition before verification
can satisfy the obligation.
Each admitted command records its own purpose. The fix prepares source; only a
successful read-only verification command can satisfy its requirement. A custom
command's configured `prepare` flag belongs to its runner and is retained across
configuration and history round trips.
An unsupported fix still reports an error. When Harness proves it left source
unchanged, it removes that operation's provisional verification requirement;
earlier verification keeps its original source identity.

`check_status` includes `state`, `completed`, `total`, `elapsed_ms`, `exit_code`,
`latest_output`, `error`, and `executed`/`reused` counts. `latest_output` is one
nonempty line with terminal escapes removed, or null before output arrives.
Adapter `progress` includes a `category`: `tests` reports case counts and running
IDs; `diagnostics` reports finished targets and error/warning counts; `format`
reports file outcomes and unformatted counts. Target and file totals are absent
when the tool did not declare its processed set. A formatter that lists only
changed files reports outcomes, not a count of all files checked.
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
Use `results: true` to page test cases, diagnostics, or formatting files instead of log text.
Each page names its `collection` and includes at most 100 entries. Missing test
IDs, diagnostic targets, and build artifacts have their own pages; they are not
repeated on every case or diagnostic page. `counts` reports whole-result totals;
`counts.diagnostics` includes diagnostics omitted by retention limits, while
`counts.retained_diagnostics` counts the available diagnostic entries.
Free-form text previews share a 64 KiB budget per response;
`preview_omitted_bytes` distinguishes preview shortening from retained-result
truncation. Identifiers and source paths remain intact. The timeline groups diagnostics by
severity and file; located entries navigate to their source line. Format results
retain per-file status, messages, and proposed diffs, including completed file
outcomes from interrupted runs. Read-only format streams reject `changed` results.
The **Load diagnostics into quickfix** row loads that command's retained,
file-located diagnostics and opens Neovim's quickfix window. Activate it with
the configured timeline-open key (Enter or `.` by default). Errors use `E`,
warnings `W`, and information and hints `I`. Known line and byte-column values
are preserved; a file-only diagnostic leaves its position unspecified. Source
links also preserve byte columns and clamp stale positions to the current file.
During plan execution, failed typecheck, lint, and build checks also add their
located error diagnostics to `plan:findings.md` for Resolve. The command's failure
summary retains its log path, errors without complete locations, and diagnostic
truncation counts. Warnings remain available in the result without becoming
repair findings.
Follow `next_cursor` unchanged with the same `run_id`, `results`, `case`, and
`stream` until null. Result cursors bind the run, command, collection, and entry
offset; using one with another run or command fails. For log cursors,
`command_index` identifies a selected command in the frozen run, and `offset`
counts bytes within that command's log. Log pagination traverses every selected
command and preserves the complete log without imposing a total-content limit.
Missing logs for started commands, invalid cursors, and decoding failures report
errors. Unstarted commands with no log return empty text.

The editor's internal `session.check.list` and `session.check.status` service routes retain
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
