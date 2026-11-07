# Project checks

A check result that no longer matches the code, or a gate that passes because nothing ran, gives false confidence that a change works. Harness therefore records what ran against which inputs, reuses a result only when those inputs are unchanged, and reports skipped checks as Not run rather than Passed.

[Extending project checks](../extending-project-checks.md) covers adding inputs and callbacks, and [Timeline](timeline.md#check-results) covers how results appear.

## Definitions

Checks live in the `checks` section of `.forge.json` and are edited through `check_definition_edit` with the `harness-checks` skill. Each definition has an identity, an automatic `trigger`, a category (`format`, `lint`, `check`, `test`, or `build`), a directory, an opaque command, typed named inputs, and prerequisites. No language detector generates commands. An absent section blocks plan submission, and an explicitly empty one is valid.

Callbacks under `.forge/checks/` receive resolved inputs and change context as JSON on stdin, and must write a matching selection receipt. `checks::invocation::resolve` substitutes declared values into complete argv elements, never through shell interpolation. Harness never builds runner commands, and repository callbacks own test filtering and coverage guarantees.

## Setup before planning

`/plan` validates project checks before generating a plan. Missing or invalid configuration keeps the request in a `ConfiguringChecks` plan, run as a separate setup exchange that needs an explicit user answer before publishing configuration. A matching saved configuration then starts one planning exchange with the original request. An intentionally empty configuration skips setup, and submission still validates the saved configuration.

The setup exchange runs in Chat mode, so its admission still captures a `PlanningContext`, marked `configures_checks` while the plan waits in `configuring_checks`. Owner finalization of a normally completed setup turn calls `advance_configured_plan_now` before observing the response. That call advances the plan only when the check runner recorded a `ConfigurationSaved` event for the current configuration digest after the plan was created. A saved configuration therefore reserves the planning continuation as soon as setup settles. The observation counts progress as exactly that advance. A turn that leaves the plan in `configuring_checks` spends one setup turn of the plan's continuation budget, even if the workspace file changed without a recorded save. A cancelled, refused, or limit-terminated setup turn neither advances nor counts.

## Selection

Triggers select checks independently of execution scope:

| Trigger | Selects a check when |
| --- | --- |
| `any_change` | The task made a change. |
| `paths` or `files_changed` | Changed files match the include and exclude globs. |
| `manual` | A user requests it, or a dependent needs it. |

Required prerequisites always join the selection. The change set compares the current checkpoint with the owning task's first checkpoint, so files that were already dirty never trigger checks. Without a baseline, applicability is unknown, and scoped coverage blocks completion instead of being guessed. `enabled` permits execution, and `auto` controls trigger selection. A plan's `checks` list adds checks, and `disabled_checks` suppresses them for that plan without changing repository defaults.

## Execution

One Rust runner serves the automatic gate and `check_run`. Ordinary completion and successful goal completion enter the gate, while intermediate goal turns, questions, approvals, cancellation, and active children do not. The gate defaults to `after_queue`, which checks the entire queued batch, and `/config` can select `between_work`. Automatic completion checks are off until the user enables them.

Checks run serially through the shell selected in `/config`. `CheckConfig::order_selected` runs selected preparation and its prerequisites first, then orders check, test, read-only format, lint, and build. Explicit dependencies take precedence. Independent checks continue after a failure, and dependents of a failed prerequisite are skipped with a reason.

`CheckEvidence` lets the gate reuse a result only when context, resolved inputs, execution contract, helper content, and workspace are all unchanged. Explicit runs always execute.

## Failure and repair

Ordinary completion-check failures go to a repair exchange owned by the original request. Repeated failures with unchanged inputs stop repair, and cancelled work with unchanged inputs becomes Blocked instead of retrying. Source changes during the gate invalidate it. A direct prompt interrupts active checks and waits for process collection, and queued prompts wait for completion.

Accepted plan execution owns a separate Check phase. Verify instructions derive from `PlanExecutionRecord::passed_check_commands`, the selected commands of the revision's passing run, which is also the source of citable `Harness check` evidence. When no configured check runs automatically, as with manual-only triggers, that list is empty: the prompt says that Harness ran no automated check commands and directs the model to `check_definition_list` and `check_run`, and the completed run reads `No automatic checks ran` instead of a vacuous `0/0 passed`. Verify is the only execution phase whose model may call `check_run`, because it forbids source edits and so cannot race a writer; `checks::model_may_run_checks` refuses the tool during Implement, Check, Resolve, check setup, and while delegated agents run. Actionable command, configuration, and execution errors retain their individual diagnostics and enter Resolve; they do not leave an otherwise runnable task suspended in Check. Its durable local reservation survives newer chat exchanges, and explicit resume in Check schedules local work without a provider prompt. Goal retains only its Work phase; this execution state machine adds no Goal check phase.

## Agent tools

`CheckSupervisor` owns publication. A `check_run` admitted by the session owner outlives the provider turn that requested it, so the owner replaces that turn's update sink with an owner-lifetime publication stream before launch. Progress, results, and configuration errors therefore reach Neovim after the turn ends. The check request and the turn's observation travel on separate channels, so admission accepts a request whose scope matches either the foreground transaction's dispatched invocation or the observed steering scope.

Agents use `check_status` (with `wait_ms` up to 60 seconds), `check_list`, paged `check_detail`, `check_definition_list`, and `check_definition_details`. Task-level overrides from `/checks` persist per task without editing project defaults.

Check history and plan documents load only at their current versions, `CHECK_HISTORY_VERSION` in `checks/supervisor.rs` and `PLAN_SCHEMA_VERSION` in `plan/document.rs`.
