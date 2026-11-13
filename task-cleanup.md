# Task lifecycle cleanup

This implementation plan prevents successful task-control operations from interrupting a provider turn or committing completion before the turn's outcome is known. It covers Forge Harness Plan, Execute, and Goal workflows, assumes familiarity with the Rust host/provider boundary, and proposes changes rather than documenting implemented behavior.

The governing rule is: record a terminal request during the turn, let the provider finish naturally, collect its work, and let Harness commit the task transition and authorize the next work. Plan acceptance follows that boundary when it moves Plan into Execute; submitting an artifact for review does not.

## Problem and verified behavior

Harness currently mixes tool acknowledgement, task mutation, native interruption, and workflow scheduling. These operations have different owners and failure conditions, but several paths treat one as evidence that another may proceed.

The reported Bevy session reached a successful `plan_phase_done` call for Implement, which committed Checking before Copilot finished its turn. Copilot then aborted the native response, normalized its internal handoff as interruption, and Harness reported cancellation instead of completing the intended workflow.

- **Execute phase completion commits live and triggers Copilot's abort.** [`tools/plan/phase_done.rs`](nvim/rust/forge/crates/forge-harness/src/tools/plan/phase_done.rs) sends `ExecutionOperation::Phase`, waits for the phase mutation, and sets the runtime completion marker. [`copilot/prompt.rs`](nvim/rust/forge/crates/forge-providers/src/copilot/prompt.rs) consults `TurnControl::tool_finished` after a root tool completes and calls `session.abort()`. Its `HostHandoff` outcome becomes `TurnOutcome::Interrupted`.
- **Shared finalization treats interruption as cancellation.** [`tools/runtime.rs`](nvim/rust/forge/crates/forge-harness/src/tools/runtime.rs) converts native interruption into `ProviderError::Cancelled`. [`broker/foreground_finalization.rs`](nvim/rust/forge/crates/forge-harness/src/broker/foreground_finalization.rs) uses that result for cancellation presentation and to skip automatic checks. A completion request must not rewrite native interruption into success as a workaround.
- **Goal completion and blocking also commit live.** [`HostInvocation::publication`](nvim/rust/forge/crates/forge-harness/src/provider/host_invocation.rs) waits for live effect publication, and [`commit_live_goal_effect`](nvim/rust/forge/crates/forge-harness/src/broker/live_goal_effect.rs) completes or blocks the task while the invocation is running. The recorded effect receipt prevents duplicate application; it does not defer the application. This path does not set the Copilot phase-abort marker.
- **Planning submission publishes live, which is not itself a terminal decision.** [`commit_plan_submission`](nvim/rust/forge/crates/forge-harness/src/broker/foreground_plan_effect.rs) records the submitted version and review state before the tool returns. Keep this capability so the user can inspect the artifact while the conversation continues.
- **Plan acceptance currently enters a stopping path.** [`TaskCommandRun::prepare`](nvim/rust/forge/crates/forge-harness/src/exchange/task_command.rs) routes ready `ExecutePlan` selections and execution-bearing workflow continuations to `begin_collection`, which starts `ExchangeStop` with `StopIntent::Reconfigure`. [`recover_automatic_plan_acceptance`](nvim/rust/forge/crates/forge-harness/src/service/input_delivery.rs) promotes eligible submissions through the task-command machinery. Acceptance needs its own non-interrupting boundary, not a different label for this stop.
- **Resolve revision review is an interactive operation.** [`tools/plan/submit.rs`](nvim/rust/forge/crates/forge-harness/src/tools/plan/submit.rs) sends `ExecutionOperation::Submit`, waits for review, updates the execution generation and canonical document, and returns feedback within the same turn. Preserve that behavior; do not make its reply depend on the turn ending.

The audit found the successful-phase-tool abort in Copilot, not the other production adapters. Early task mutation and acceptance-driven stopping belong to shared Harness code, so the repair is not a Copilot-only patch. The Bevy failure is confirmed; other race failures below are regression scenarios to establish, not claims that additional sessions reproduced them.

## Scope and acceptance criteria

The cleanup changes who commits terminal decisions and when, without changing what an accepted plan requires or who may approve execution. It must preserve native outcome evidence, user control, current review interactions, and the distinction between task checks and optional session-level automatic checks.

- A successful `goal_complete`, `goal_blocked`, or `plan_phase_done` call records a request and returns without interrupting the provider or advertising a committed terminal state.
- Manual acceptance and auto-accept record approval of one exact submitted revision, but neither stops a running planning turn merely to start Execute.
- An agent-requested terminal transition requires the matching native result, successful provider collection, reconciled host effects, current task authority, and valid workflow evidence.
- A successful task transition creates at most one dependent continuation or check reservation. Recovering a lost acknowledgement does not rerun tools or create another reservation.
- A failed, interrupted, disconnected, or uncertain turn cannot become successful because it recorded a terminal request. A normally completed `goal_blocked` request still produces a blocked task, not a provider failure.
- Explicit cancellation, pause, replacement, shutdown, and existing safety failures retain their stop mechanisms and accurate reasons. Approval is not cancellation, and an unexplained native interruption is not proof of user cancellation.
- Artifact edits, ordinary planning submission, questions, and same-turn Resolve revision review remain live operations where their callers require live results.

Do not redesign all provider execution, introduce an alternative scheduler, or add task-specific provider capabilities. Do not change check selection, semantic conformance rules, authorization defaults, turn budgets, retry policy, or user settings except where the new boundary requires an explicit contract adjustment.

## Ownership and lifecycle

Harness must own the completion request, its validation, and its eventual disposition; adapters must own only native execution facts and provider resource collection. Reuse the existing task controller, retained host effects, settlement receipts, and continuation journal rather than adding a parallel handoff system.

### Record terminal requests without committing them

Introduce a typed host-owned request for Goal completion, Goal blocking, and Execute phase completion, with variant-specific evidence. Keep it separate from the committed task workflow so recording a request cannot make the task Complete, Blocked, Checking, or Resolve.

Bind each request to the host-issued task identity, control revision, dispatched entry, and exact exchange/turn invocation. Include the execution phase and accepted revision for Execute, and identify the submitted verification report and workspace evidence when applicable. Reuse existing identity types: callback/effect identity detects replay within its original invocation, while task and execution revisions prevent a request from applying to a replacement or later phase.

The request lifecycle has explicit pending, committed, and rejected or superseded outcomes. Pending means Harness recorded intent, not that the task succeeded. Storage uncertainty remains an error requiring reconciliation, not a guessed disposition.

Validation before acknowledgement must reject malformed requests, wrong phase or revision, unresolved required questions, and invalid verification reports. For evidence that can change, record its identity and revalidate it at commit; successful validation at tool time does not authorize an unconditional later transition.

Use responses such as "Phase completion requested. Finish this turn; Harness will evaluate the transition after it ends." Goal responses use the same distinction. Do not return next-phase instructions or claim "Phase finished" before the host commits that phase.

### Close mutation admission without stopping the provider

Recording terminal intent must prevent new mutations of the completed scope while still allowing the response to end. This admission state belongs to the current host invocation; it must not revoke the very authority needed to return the tool result, collect callbacks, or finalize the request.

Use a serialized admission boundary that records pending intent and closes new task-scoped mutation admission together. Retain exact retries and their original acknowledgements; reject conflicting terminal requests with an actionable error. Keep read-only inspection, final response delivery, collection, and explicit user cancellation available, but reject new state-changing task tools, check launches, child launches, and additional mutating work from that invocation.

Route mediated native file and shell permissions through the same invocation admission decision instead of relying solely on `ControlToolRuntime::terminal`. Revalidate permission requests after approval waits so a permission captured before sealing cannot admit a new mutation afterward. Audit direct tools, MCP callbacks, permission caches, and managed children for bypasses.

Already-admitted operations remain owned and must finish or be collected before evidence is authoritative. Their final effects must participate in snapshot validation; completion cannot certify a workspace that was inspected before an outstanding writer finished. Reuse existing child ownership and write-admission rules, and prevent a successor phase from overlapping an unfinished task-scoped writer.

This is not a filesystem lock against external editors or previously authorized native processes. If a provider surface cannot enforce the admission boundary, document that limitation and require final evidence validation to reject a changed workspace; do not claim that all writes are impossible after a request or add a hidden abort fallback.

### Commit after natural completion and collection

The host finalizer consumes the matching collected native result and pending request through one coordinated path. Neither a final-answer string nor a tool-success event establishes completion, and a pending request's acknowledgement must not wait on this finalizer or it would prevent the provider from reaching its natural terminal event.

1. Record and acknowledge the validated request while retaining the current workflow and invocation.
2. Allow the provider to deliver its remaining response and report its native terminal result normally.
3. Collect provider-owned work and reconcile the original callback/effect set, including writes admitted before sealing.
4. Check the actual native outcome, pending user controls, required questions, task authority, accepted revision, and final evidence.
5. Commit the workflow transition and the dependent scheduling decision under the same task fence. Reuse retained effect identities and the existing continuation/check reservation journal to recover partial publication without repeating domain mutations.
6. Finish required host settlement and publication before releasing the reserved next work for dispatch.

Use the existing normally completed outcome as the eligibility condition for agent-requested completion. Non-normal completion reasons retain their existing explicit-continuation policy. If task mutation and reservation cannot share one storage transaction, retain a recoverable intermediate receipt and prove that reconciliation reserves the missing work once without reapplying the transition.

Do not move every host callback into post-turn finalization. Live artifact publication, question answers, approval replies, and nonterminal progress remain live; only terminal requests use the deferred path. Reconciliation must distinguish a recorded request from an already-applied effect so the old live Goal receipt cannot masquerade as a new terminal commit.

## Workflow-specific changes

All three task kinds use the same completion boundary, but they do not have the same triggering event. Keep their domain transitions explicit rather than treating any review or control message as task completion.

### Goal completion and blocking

Replace the terminal mutation in `commit_live_goal_effect` with request recording and acknowledgement. Finalization applies Complete or Blocked only after the matching normally completed turn has been collected and its authority remains current.

Keep `goal_status` nonterminal. Preserve ordinary Goal progress accounting and continuation for turns without a terminal request, and count the finishing turn only once. A rejected terminal request must not fall through into ordinary automatic continuation as though no completion was attempted; surface its disposition and require the appropriate recovery.

Required checks retain their current authority over completion. If checks or other required evidence are not yet satisfied, represent the outstanding obligation without presenting the goal as complete. Task blocking remains distinct from transport failure or a cancelled exchange.

### Execute phase completion

Replace the immediate phase mutation behind `ExecutionOperation::Phase` with validated request recording. Keep Verify's report validation and final conformance checks, but defer task mutation, evidence publication, and scheduling until settlement.

Preserve the existing domain outcomes in [`PlanExecutionRecord::finish_phase`](nvim/rust/forge/crates/forge-harness/src/plan/execution.rs) and the scheduler: Implement and Resolve hand off to Harness-owned checking or reuse eligible current evidence; Verify can complete, return findings to Resolve, or block while preserving Verify. Passing manual claims must never bypass semantic conformance or stale automated-evidence checks.

Keep the Checking phase host-owned. Passing checks may finish a plan with no manual actions, failing checks may admit Resolve, and current passing evidence may admit Verify; these transitions consume check results rather than wait for a nonexistent provider turn. Schedule task-required checks independently of optional automatic-check policy.

Keep Resolve's `plan_submit` review wait within the same turn. An accepted revision changes the contract for continued Resolve work, not the phase's terminal status. Pause, review rejection, withdrawal, and cancellation must release or suspend that wait through existing explicit control paths.

### Plan acceptance into Execute

Separate recording approval from replacing Plan with Execute. Both manual acceptance and auto-accept must target a specific plan, submitted version, review digest, selected execution settings, and completed authorization choices.

Once those prerequisites hold, record acceptance and show that execution is waiting for the planning turn, if one is running. Do not start `ExchangeStop` with `StopIntent::Reconfigure` for acceptance, cancel question delivery merely to force advancement, or open an execution invocation before the planning invocation settles. If no planning turn is running, use the same owner commit path without inventing a provider turn to wait for.

Acceptance closes new mutations against the accepted planning scope without blocking provider completion. Writes already admitted before acceptance, concurrent reviewer edits, or external changes can still invalidate the selection; revalidate its exact digest and source guards at settlement and reject stale acceptance instead of silently approving the newer draft. Preserve the reviewed snapshot and feedback for resubmission.

Pending acceptance must not occupy the command owner in a way that prevents it from processing the native completion or a later explicit cancellation. Retain the decision as owner state, continue draining events, and let the same settlement boundary release the transition. Explicit pause or replacement before commit supersedes the pending advancement and cannot be undone by a late acceptance callback.

If the planning turn fails or is interrupted after approval, retain the approval evidence but do not automatically start Execute. Surface that advancement did not occur and require explicit recovery with fresh validation. A repeated acceptance of the same revision observes the original decision; acceptance of a different revision must not reuse that decision.

Leave ordinary `plan_submit` live and nonterminal when no acceptance is pending. Preserve `/plan accept on/off` as policy rather than task completion: disabling it prevents future automatic acceptance, while an already-recorded acceptance has a separate disposition. Keep execution authorization and provider-context selection independent of that policy.

## Provider contract and stop provenance

Remove task completion as a reason for an adapter to interrupt native work. Preserve native interruption as evidence, and represent the host's reason for requesting a stop separately so presentation and scheduling do not infer user intent from a transport outcome.

Remove `TurnControl::tool_finished`, its `execution_finished` propagation, Copilot's successful-tool abort branch, and the task-driven `IdleOutcome::HostHandoff` case. Remove `HostTurnStatus::control_handoff` if its usage audit confirms it has no independent native responsibility. Update the Copilot callback multiplexer, all provider fixtures, and isolated text-generation controls along with the API.

Keep runtime replay information only for host request idempotency; it must no longer mean "the provider must stop." Do not replace the removed Boolean with a task-phase enum, provider-specific stop hint, alternate normal-completion label, or adapter-visible handoff receipt.

Reuse or extend host stop provenance for explicit user cancellation, pause/replacement, configuration restart, shutdown, and safety failure. If native interruption arrives without a matching host stop request, report interruption without "by user." A normal native completion racing an explicit stop remains normal native evidence, but the owner still honors the user's prohibition on new work.

Integrate with the existing in-progress owner-cancellation and presentation changes rather than replacing them. Preserve callback ownership, collection watermarks, checkpoint ordering, and interruption routing to the original invocation; a late stop must never attach to a successor.

## Implementation sequence

Implement the host lifecycle first and remove the adapter workaround as part of the same integrated change. The sequence below identifies code ownership and dependencies, not permission to leave a partially converted workflow enabled.

1. **Pin the regression and boundary contracts.** Add controllable provider fixtures that can hold the native terminal event and collection independently after a tool returns. Establish the current phase-abort regression, live Goal mutation, and acceptance stop behavior. Reconcile concurrent owner-control changes before modifying shared files.
2. **Add host-owned pending requests and admission.** Update `tools/runtime.rs`, `tools/executor.rs`, `provider/host_invocation.rs`, the task authority/permission boundaries, and existing settlement storage. Implement typed variants, invocation-bound replay, sealing, and explicit rejected/superseded dispositions. Avoid separate copies of the admission rule.
3. **Centralize terminal settlement.** Update `broker/foreground_finalization.rs`, Goal effect handling, `broker/execution.rs`, and `task/controller.rs`. Make Goal and Execute tools record requests, then use the collected-result path to validate and commit them. Reuse `reserve_continuation` and `reserve_checks` with the existing task-turn identity.
4. **Separate approval from stopping.** Update `exchange/task_command.rs`, `broker/acceptance.rs`, `service/input_delivery.rs`, and task acceptance state. Route both `ExecutePlan` and execution-bearing workflow continuations through non-interrupting pending acceptance; preserve intentional stop behavior for pause, clear, replacement, and configuration changes.
5. **Remove the provider coupling.** Update `forge-provider-api/src/turn_control.rs`, Copilot prompt/turn/control code, Harness callback implementations, isolated text-generation controls, and all provider mocks. Delete obsolete handoff branches and tests instead of keeping unused compatibility hooks.
6. **Update presentation and contracts.** Change tool descriptions and result text, task prompt sources, and pending/committed UI events together. Update [Harness architecture](nvim/contributing/forge/architecture/harness.md), [Providers](nvim/contributing/forge/architecture/providers.md), [Planning](nvim/contributing/forge/architecture/planning.md), [Plan review](nvim/contributing/forge/architecture/plan-review.md), and relevant user command documentation. Correct the planning documentation's claim that revision submission consumes a completed turn if the preserved same-turn review contract remains as implemented.
7. **Validate and retire incompatible disposable state.** Finish the regression matrix, run the saved checks, and apply the versioned-state rollout below. Do not retain an old successful-tool abort path behind a feature flag.

These steps change canonical Rust sources and their documentation. If provider instructions or generated assets need adjustment, edit their authoritative sources and regenerate through the repository's existing process; do not patch generated provider files directly.

## Regression coverage and verification

Tests must observe the ordering itself, not merely a final successful task label. Use explicit barriers for request acknowledgement, native completion, collection, host commit, and dependent dispatch so the tests do not depend on sleeps.

Extend the existing provider bridge tests, service task-state tests, task-controller tests, execution-state tests, and Copilot's native fixtures. In particular, strengthen `goal_completion_settles_final_response_and_accepts_next_input` in [`service/tests/task_state.rs`](nvim/rust/forge/crates/forge-harness/src/service/tests/task_state.rs) with assertions before native completion rather than treating its final response as proof of correct timing.

- **Goal timing:** Test Complete and Blocked separately. Hold the native turn after the tool acknowledgement and assert that the goal is still nonterminal, no interruption was requested, and no successor started. Release native completion but hold collection and assert the same. Then release collection and assert the exact final task state and one event/receipt.
- **Execute outcomes:** Test Implement, Resolve, and each Verify assessment, including a claimed pass with conformance findings. Assert the old phase until settlement, then the correct phase or terminal result, with exactly one required check or provider reservation. Cover both optional automatic-check settings and plans with and without manual actions.
- **Bevy cancellation regression:** Deliver a successful phase tool, then a final assistant response and normal native completion. Assert zero adapter abort calls, preserved final content, no cancellation label, and Checking admission after collection. No Bevy dependency is needed to reproduce this lifecycle ordering.
- **Acceptance:** Cover manual approval and auto-accept with running, finalizing, and idle planning turns. Assert no reconfiguration stop, no early execution, exact accepted revision/settings, and one Execute dispatch. Include execution-bearing workflow continuations and acceptance that still needs authorization answers.
- **Interactive controls:** Prove that ordinary plan submission remains editable, questions can resolve during the turn, and Resolve revision acceptance returns feedback to the same invocation without sealing it or waiting for native completion.
- **Admission and writers:** Try conflicting terminal requests, exact retries, post-request mutations, permission approvals that resume after sealing, pre-admitted writes, and unfinished task-scoped child writers. Reads and final responses remain available; mutations reject visibly or invalidate final evidence without certifying stale content.
- **Failure and provenance:** Inject native failure, native interruption without a user request, non-normal completion, disconnect, collection failure, and storage failure after request recording. None may commit terminal success or automatically dispatch the next phase; presentation must identify the observed cause.
- **Races with user control:** Deliver pause, cancellation, replacement, new required input, or review edits before commit and again after commit but before dispatch. Assert explicit ordering, no resurrection of cancelled work, no stale-digest execution, and no interruption of a successor.
- **Replay and recovery:** Lose the tool reply, replay the same callback, duplicate native terminal delivery, and fail publication after task commit. Recover the recorded request or committed decision without rerunning the mutation, spending another turn, or reserving duplicate work. An uncertain native send is never resent to recover task state.
- **Provider parity:** Exercise the normalized host path independently of provider wire formats, then assert native adapter behavior for Copilot, Codex, Claude, and ACP. No adapter may learn task phases or stop a turn because a task tool succeeded.
- **State isolation:** Run the new state version beside a live old owner, verify fresh workflows, skip and report live old versions during cleanup, remove stale versions after their owners exit, and prove persistent settings remain unchanged.

Use saved Harness checks exclusively. Discover their current definitions before execution; the inspected repository provides `rust-check`, `rust-test`, `rust-format`, `rust-lint`, `rust-build`, and `git-whitespace`. Select the affected packages and named regression cases using their declared inputs, compile before tests, and expand to workspace coverage where the shared provider API change requires it. Use `lua-test` only if Lua presentation changes; do not add check tooling for this cleanup.

Run one live-provider smoke test after deterministic coverage: accept a plan while its planning turn is finishing, finish Implement, observe the final assistant response and subsequent checks, and separately exercise explicit cancellation. Provider fixture coverage proves ordering; the smoke test verifies that the native event sequence and UI expose that ordering without an accidental abort.

## Rollout, alternatives, and remaining risks

Pending terminal requests and acceptance decisions change disposable task/session state semantics. Start a new explicit state compatibility directory rather than interpreting older live-completion receipts as deferred requests or migrating incompatible sessions.

The inspected implementation uses [`storage/state.rs`](nvim/rust/forge/crates/forge-harness/src/storage/state.rs) version 19. Allocate the next unused version at implementation time, keep old hosts on their own directory, and reuse the existing leased-version cleanup. Never delete a live version or its dependent files. Persistent preferences, permission settings, user-authored repository files, and external authored artifacts are not part of this reset; report unsupported settings or migration failures rather than replacing them with defaults.

Simply removing Copilot's abort would repair the immediate interruption but leave live Goal/phase mutation and acceptance-driven stopping. Relabeling `HostHandoff` as normal completion would hide native evidence. Adding a host-handoff protocol would retain the unnecessary coupling; neither alternative meets the ownership rule.

Waiting for natural completion increases the interval between a terminal tool call and the next phase. Keep the invocation visibly finishing and preserve explicit cancellation and existing watchdogs; do not add an arbitrary timeout, output cap, or silent forced-stop fallback.

The implementation must establish how every provider's mediated tools enforce sealing, how existing owner commands remain serviceable while acceptance waits, and how task mutation and reservation recover across storage failure. If a surface cannot meet those requirements, report the precise limitation and leave advancement uncommitted rather than claiming success. Workspace evidence changed by an external writer, unresolved required input, lost collection, or uncertain storage must remain an explicit error boundary.
