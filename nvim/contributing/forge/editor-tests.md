# Editor tests

A Rust test cannot show that a keystroke reaches the right buffer, and a Lua mock cannot show that the real host keeps its queue. Editor fixtures close that gap by driving real Neovim, and host fixtures add the real Rust host. This guide explains which suite to use, how to run a fixture, and how to control provider timing without sleeping.

Host fixtures need a built executable. See [Building and testing](building-and-testing.md).

## Suites

| Check | Runs | Provider |
| --- | --- | --- |
| `lua-test` | Ordinary module fixtures | None |
| `lua-test-mock-integration` | Host fixtures | `MockProvider` or a simulated process, with no model requests |
| `lua-test-live-integration` | Fixtures that use installed provider CLIs | Real, and may make model requests |
| `lua-benchmark` | Benchmarks | Varies |

The `lua-test` check runs the ordinary suite, which excludes both integration suites and benchmarks; its default `affected` selection maps changed source paths to the feature suites that cover them. The registry rejects a live fixture submitted to the mock check. Run the live check only after checking credentials and each fixture's prerequisites. Its fixtures include `forge-providers/tests/fixtures/skill_reads_host.lua`, `forge-providers/tests/fixtures/activity_host.lua`, and `harness_provider_switch_host.lua`.

## Run a fixture

`.forge/checks/lua-tests.nu` is the only way to run Lua fixtures, both inside Harness, where every Lua check calls it, and outside. Each fixture runs in its own fresh Neovim, `nvim --headless -i NONE -u NONE --cmd "set rtp^=./nvim" --cmd "lua vim.loader.enable(false)" -l <fixture>`, from the repository root. The runner runs fixtures in parallel, by default on a sixth of the logical CPUs, because host fixtures contend for process launches and `git`, and kills the whole process tree of any fixture still running after five seconds, including the Forge host it launched. A killed fixture fails as a timeout, and a fixture that prints success but stays alive therefore fails too.

Inside Harness, select a fixture through its check:

```text
check_run({"check":"lua-test","inputs":{"fixture":"nvim/tests/forge/plan_check_selection.lua"}})
check_run({"check":"lua-test-mock-integration","inputs":{"files":["nvim/tests/forge/harness_host.lua"]}})
```

Use `inputs:{"suite":"walkthrough"}` only when a whole feature needs coverage. Omitting `files` from an integration check selects its whole suite.

Outside Harness, call the runner directly. Without paths it runs every fixture except the live-provider suite:

```nu
nu .forge/checks/lua-tests.nu nvim/tests/forge/plan_check_selection.lua
nu .forge/checks/lua-tests.nu --suite mock-integration --jobs 4
nu .forge/checks/lua-tests.nu --affected nvim/lua/forge/walkthrough.lua
nu .forge/checks/lua-tests.nu
```

The runner prints each fixture's result and duration as it finishes, then the slowest fixtures and a pass and fail count, and exits nonzero when any fixture fails. `--list` prints the selection without running it. Every fixture must finish within the five-second limit, and there are no per-fixture exceptions. A fixture that exceeds it because it waits on the real clock or hangs must be fixed with a deterministic signal or removed. The runner's `disabled_fixture_list` names the fixtures excluded from default selections because launch and sequencing overhead alone exceeds the limit, and [disabled slow tests](../../tests/forge/plans/disabled-slow-tests.md) records each one's measured duration. A fixture that takes a scenario name through `arg[1]`, such as `plan_review_question_response_host.lua`, is listed once per scenario as `<path>::<scenario>`; the runner discovers the scenarios from the fixture's `{ name = "...", run = ... }` rows and passes the name as `arg[1]`.

Parameterized fixtures for languages and providers need the setup in [Verification by area](verification-by-area.md), and the runner rejects them without it. Pass that setup as Lua with `--lua`, which each fixture runs before it loads:

```nu
nu .forge/checks/lua-tests.nu --lua "vim.g.forge_test_plan_behavior = true" nvim/tests/forge/plan_review_calls_host.lua
```

## Launch the host executable

A host fixture launches the built Forge executable through `nvim/tests/forge/support/host_executable.lua`. Do not copy the executable into the fixture directory. Windows scans every new image before its first launch, and a scan of the 182 MB development build routinely outlasts a fixture's startup deadline. The helper acquires a lease from the builder's content-addressed executable cache under `stdpath("cache")`. That cache keeps one pinned copy per build, so repeated runs of the same build launch an image Windows has already scanned:

```lua
local binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
```

The first fixture run after a rebuild still pays for one copy and scan of the new image.

## Place fixture workspaces

A fixture never creates its workspace, data directory, or retained evidence inside the checkout. Git reports a nested repository as a single untracked entry, so a fixture workspace left in the checkout shows up in every later status, checkpoint, and Harness session of that checkout. Use `vim.fn.tempname()` for state that may disappear with the Neovim process, which deletes that directory on exit.

A fixture that keeps diagnostics after a failure takes its directory from `nvim/tests/forge/support/evidence_directory.lua`. The helper creates a unique directory under `vim.uv.os_tmpdir()`, inside its `forge` subdirectory: `%TEMP%\forge` on Windows and `$TMPDIR/forge` on macOS. Resolve the helper at file scope, before the fixture changes directory:

```lua
local fixture_evidence = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/evidence_directory.lua")("harness-admission")
local workspace, data = fixture_evidence.path .. "/workspace", fixture_evidence.path .. "/data"
```

Call `fixture_evidence.release(passed)` only after the host process exits, because a running host holds files in the data directory open. A passing run removes the directory. A failing run keeps it, and the fixture reports its `failure.txt` path. A failure to remove the directory fails the fixture.

## Control provider timing

An interaction test often has to act after the provider receives a prompt but before it responds. Elapsed-time sleeps make these tests flaky, so `nvim/tests/forge/support/mock_provider.lua` holds the real host's `MockProvider` through `session.provider.mock_response` instead. Create the hold before submitting, then wait for the prompt to arrive:

```lua
local mock = dofile("nvim/tests/forge/support/mock_provider.lua")
local hold = mock.hold(state.session.id, "chat")
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Test response timing" })
controller.submit()
local pending = hold:wait()
assert(pending.prompt == "Test response timing")
-- Perform the interaction under test here.
hold:release()
```

This fragment assumes an initialized mock session and the existing Harness `state` and `controller` objects. The mode selects which prompt to hold, such as `"chat"` or `"plan_question"`, and omitting it holds the session's next prompt of any mode.

`wait()` returns the held request's ID, session, mode, and full prompt. `release()` resumes only that request and does not wait for rendering, so afterward wait for the specific buffer or state change under test. `cancel()` discards an unused hold, or fails a response waiting on it. Unknown IDs, early release, and IDs from another session fail explicitly. The helper's ten-second deadline detects a stuck test, and cancellation or provider shutdown clears holds.

Approval timeouts follow the same rule. Configure an approval timeout long enough that the real clock never reaches it, wait for the approval to appear, then call `mock.expire_approval(session_id)`. The host resolves the pending approval through its deadline path, so the fixture observes the same `Timeout` resolution, rejection feedback, and continuation as an elapsed timer.

## Unscripted mock turns

A fixture that does not script `session.provider.mock_response` receives the mock provider's default turn. Planning fixtures depend on that default reaching a reviewable plan, so the mock acts as a minimal planner instead of only echoing its prompt. Scripted turns skip all of the behavior below.

1. A Plan turn, or a Chat turn whose prompt names the `harness-checks` skill, first reads `check_definition_list`. When checks are unconfigured or invalid, the mock resubmits the listed definitions through `check_definition_edit`, so the edit follows the session's approval mode. Harness issues that Chat turn while a plan waits in `configuring_checks`, and starts the original planning request after the configuration is saved.
2. A Plan turn whose instructions or prompt contain the declaration JSON then fills the plan overview, adds the `src/change.rs` declaration file, and calls `plan_submit`, leaving the plan in review. A revision carries the original design in its instructions and the current design in its input. The mock therefore revises the higher version, and it skips the file edit when that design already declares `src/change.rs`. The mock cannot read the workspace, so a fixture whose workspace already contains `src/change.rs` gets a rejected add and never reaches review.
3. Every unscripted turn ends with a `Mock response:` echo of its prompt.

Fixtures answer the resulting approvals and review questions through shared helpers rather than private copies of the request shapes. `support/approval.lua` answers each pending approval request once through `session.approval.resolve`, and accepts a callback that inspects each request before it is answered. A resolution failure arrives in an asynchronous callback, where raising it would only log the error, so the responder retains the first failure and raises it from the next `answer_pending` call inside the fixture's wait loop. `support/plan_acceptance.lua` answers the execution tool-approval question that `session.plan.accept` raises before execution starts.

## Observe state and activity

Read session state through `support/state.lua` in any fixture that opens the Harness view. `session.state.get` is the reconnect primitive, not a passive read. Its response carries a retained `owner_publication` page, and the host records that page as delivered once the response reaches output. A fixture that calls the method directly and discards the page consumes live events such as `activity_changed`, and the presentation then waits for an idle transition it will never receive. The helper calls `controller.synchronize_state`, which sends the client's publication cursor and replays the page through `snapshot.apply`:

```lua
local state_snapshot = dofile(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h") .. "/support/state.lua")
local snapshot = state_snapshot.get()
```

Resolve the helper path at file scope, before the fixture changes directory, because `debug.getinfo` reports the path as `nvim -l` received it. Fixtures that test the method's own wire behavior, such as `request_independence_host.lua`, still issue it directly.

Observe execution through `require("forge.views.harness.activity").busy(state)`, which reads the host-published `state.activity` phase. `session.prompt.submit` responds when the host admits the input, not when the turn finishes, so await a turn through activity or the concrete plan, question, or transcript state it produces. Lua unit fixtures without a host set activity through `support/activity.lua`, whose `set_busy` advances the activity revision the way a host publication does.

## Submission races

`harness_host.lua` runs `support/submission_races.lua` against the real Rust host, SQLite storage, Lua input reconciliation, and transcript presentation. Only the provider response and selected transport callbacks are controlled.

The module asserts the split input model. Host-confirmed messages live in `state.input.confirmed` with a `delivery` state, while editor drafts that the host has not confirmed live in `state.input.drafts` with a `draft_state` of `pending`, `rejected`, or `unknown`. A lost submit acknowledgement is recovered through `session.command.status.get` inside `forge.command.dispatch`, never by resubmitting, so the lossy-transport case asserts exactly one submission. A rejected attachment draft stays recoverable without blocking later submissions, and `input_state.discard_rejected` removes it. A host replacement restores durably queued input as the same confirmed message and does not deliver it on startup.

Add a race case to that module with an explicit provider or callback barrier, and assert durable exchange identity, queue contents, and preservation of newer composer text. Do not replace the Rust queue with a Lua mock, or depend on sleeps to order events. Every wait needs a diagnostic deadline, and teardown must restore intercepted callbacks and collect the isolated host when an assertion fails.
