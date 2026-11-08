# Editor tests

A Rust test cannot show that a keystroke reaches the right buffer, and a Lua mock cannot show that the real host keeps its queue. Editor fixtures close that gap by driving real Neovim, and host fixtures add the real Rust host. This guide explains which suite to use, how to run a fixture, and how to control provider timing without sleeping.

Tests that launch the Forge host assume a current executable. Run `rust-build` after Rust changes and before the Lua tests; see [Building and testing](building-and-testing.md).

## Test layout

Each plugin keeps its tests in `nvim/lua/<plugin>/tests/`, split by how much of the system a test exercises. Both halves mirror the plugin's source tree, so a source path names where its tests live. Every test file ends in `.test.lua`, and the runner finds tests by that suffix alone.

| Kind | Location | Form |
| --- | --- | --- |
| Unit | `tests/unit/` at the source file's mirrored path: `nvim/lua/forge/views/status/status_context.lua` → `nvim/lua/forge/tests/unit/views/status/status_context.test.lua`, and `init.lua` → `init.test.lua` | One row per test in the file's `scenarios` table |
| Integration | `tests/integration/` in the mirrored directory of the source area it drives, in the file named after that area: `views/status/` → `nvim/lua/forge/tests/integration/views/status/status.test.lua`, and a top-level module's area is a directory named after it, such as `integration/client/client.test.lua` for `client.lua` | One row per test in the area file's `scenarios` table |
| Support | `tests/support/` for shared helpers, `tests/fixtures/` for data and shared setup scripts, `tests/plans/` for notes | Plain `.lua` or Markdown; never run as tests |

Forge's integration areas are `buffer`, `client`, `git`, `lsp`, `review_document`, `text`, `walkthrough`, and, under `views/`, `harness`, `picker`, `plan_review`, and `status`. Each area name is also a suite. An integration test stays a standalone file in its area directory only when a row cannot hold it: when it reloads its own file in a child Neovim, such as `harness_markdown_layout.test.lua`, or dispatches its own scenarios, such as `plan_review_question_response.test.lua`. Load helpers by repository path, such as `dofile("nvim/lua/forge/tests/support/state.lua")`, at file scope.

A test file declares its tests as rows at two-space indentation, which is how the runner finds them, and dispatches on `arg[1]`:

```lua
local scenarios = {
  { name = "admission_queue", run = function()
    -- arrange, act, and assert against forge.client
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
```

Add a new test as a row in the existing unit or area file rather than as a new file, and move a unit test file whenever its source file moves.

## Suites

The `lua-test` check runs every Lua test through the runner. Its default `affected` selection maps changed paths to tests. A changed test file runs itself; a changed Lua source file runs its mirrored unit test and its integration area, the nearest mirrored integration directory of the file or an ancestor; a source file with no area runs its plugin's whole test directory; and any other change, including a change under `tests/` or to the runner, runs every test. Its `suite` input accepts `all`, an area name such as `status`, which selects that area's unit and integration mirrors, or a directory under `nvim/lua`.

## Run a fixture

`.forge/checks/lua-tests.nu` is the only way to run Lua tests, both inside Harness, where the `lua-test` check calls it, and outside. Each scenario row, or each standalone file, runs in its own fresh Neovim, `nvim --headless -i NONE -u NONE --cmd "set rtp^=./nvim" --cmd "lua vim.loader.enable(false)" -l <file> [scenario]`, from the repository root. The runner runs tests in parallel, by default on a sixth of the logical CPUs, because host tests contend for process launches and `git`, and kills the whole process tree of any test still running after five seconds, including the Forge host it launched. A killed test fails as a timeout, and a test that prints success but stays alive therefore fails too.

Inside Harness, select tests through the check:

```text
check_run({"check":"lua-test","inputs":{"fixture":"nvim/lua/forge/tests/unit/views/plan_review/checks.test.lua"}})
check_run({"check":"lua-test","inputs":{"suite":"harness"}})
```

Use a `suite` only when a whole area needs coverage.

Outside Harness, call the runner directly. Without arguments it runs every test. Each argument is a test file, a source file (which selects its unit test), a directory, a test name (the file name without `.test.lua`, which selects every file with that name), or any of these followed by `::scenario`. A source directory selects both of its mirrors:

```nu
nu .forge/checks/lua-tests.nu client                                 # unit/client.test.lua and integration/client/client.test.lua
nu .forge/checks/lua-tests.nu client::admission_queue                # one scenario
nu .forge/checks/lua-tests.nu nvim/lua/forge/tests/unit/views/plan_review/checks.test.lua
nu .forge/checks/lua-tests.nu nvim/lua/forge/lsp                     # unit and integration tests for lsp/
nu .forge/checks/lua-tests.nu --suite status --jobs 4                # a source area as the suite
nu .forge/checks/lua-tests.nu --affected nvim/lua/forge/walkthrough.lua
nu .forge/checks/lua-tests.nu
```

The runner prints each test's result and duration as it finishes, then the slowest tests and a pass and fail count, and exits nonzero when any test fails. `--list` prints the selection without running it. Every test must finish within the five-second limit, and there are no per-test exceptions. A test that exceeds it because it waits on the real clock or hangs must be fixed with a deterministic signal or removed.

`--lua` runs the given Lua before each selected test loads, for ad hoc setup such as `vim.g` values.

## Launch the host executable

A host fixture launches the built Forge executable through `nvim/lua/forge/tests/support/host_executable.lua`. Do not copy the executable into the fixture directory. Windows scans every new image before its first launch, and a scan of the 182 MB development build routinely outlasts a fixture's startup deadline. The helper acquires a lease from the builder's content-addressed executable cache under `stdpath("cache")`. That cache keeps one pinned copy per build, so repeated runs of the same build launch an image Windows has already scanned:

```lua
local binary = dofile("nvim/lua/forge/tests/support/host_executable.lua")()
```

The first fixture run after a rebuild still pays for one copy and scan of the new image.

## Place fixture workspaces

A fixture never creates its workspace, data directory, or retained evidence inside the checkout. Git reports a nested repository as a single untracked entry, so a fixture workspace left in the checkout shows up in every later status, checkpoint, and Harness session of that checkout. Use `vim.fn.tempname()` for state that may disappear with the Neovim process, which deletes that directory on exit.

A fixture that keeps diagnostics after a failure takes its directory from `nvim/lua/forge/tests/support/evidence_directory.lua`. The helper creates a unique directory under `vim.uv.os_tmpdir()`, inside its `forge` subdirectory: `%TEMP%\forge` on Windows and `$TMPDIR/forge` on macOS. Resolve the helper at file scope, before the fixture changes directory:

```lua
local fixture_evidence = dofile("nvim/lua/forge/tests/support/evidence_directory.lua")("harness-admission")
local workspace, data = fixture_evidence.path .. "/workspace", fixture_evidence.path .. "/data"
```

Call `fixture_evidence.release(passed)` only after the host process exits, because a running host holds files in the data directory open. A passing run removes the directory. A failing run keeps it, and the fixture reports its `failure.txt` path. A failure to remove the directory fails the fixture.

## Control provider timing

An interaction test often has to act after the provider receives a prompt but before it responds. Elapsed-time sleeps make these tests flaky, so `nvim/lua/forge/tests/support/mock_provider.lua` holds the real host's `MockProvider` through `session.provider.mock_response` instead. Create the hold before submitting, then wait for the prompt to arrive:

```lua
local mock = dofile("nvim/lua/forge/tests/support/mock_provider.lua")
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
local state_snapshot = dofile("nvim/lua/forge/tests/support/state.lua")
local snapshot = state_snapshot.get()
```

Load the helper at file scope by its repository path, before the fixture changes directory, because the runner starts every test from the repository root. Fixtures that test the method's own wire behavior still issue it directly.

Observe execution through `require("forge.views.harness.activity").busy(state)`, which reads the host-published `state.activity` phase. `session.prompt.submit` responds when the host admits the input, not when the turn finishes, so await a turn through activity or the concrete plan, question, or transcript state it produces. Lua unit fixtures without a host set activity through `support/activity.lua`, whose `set_busy` advances the activity revision the way a host publication does.
