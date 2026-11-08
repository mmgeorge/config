# Editor tests

A Rust test cannot show that a keystroke reaches the right buffer, and a Lua mock cannot show that the real host keeps its queue. Editor fixtures close that gap by driving real Neovim, and host fixtures add the real Rust host. This guide explains which suite to use, how to run a fixture, and how to control provider timing without sleeping.

Tests that launch the Forge host assume a current executable. Run `rust-build` after Rust changes and before the Lua tests; see [Building and testing](building-and-testing.md).

## Test layout

Each plugin keeps its tests in `nvim/lua/<plugin>/tests/`, split by how much of the system a test exercises. Both halves mirror the plugin's source tree, so a source path names where its tests live. Every test file ends in `.test.lua` and is a [mini.test](https://github.com/nvim-mini/mini.test) set, and the runner finds test files by that suffix alone.

| Kind | Location | Process |
| --- | --- | --- |
| Unit | `tests/unit/` at the source file's mirrored path: `nvim/lua/forge/views/status/status_context.lua` → `nvim/lua/forge/tests/unit/views/status/status_context.test.lua`, and `init.lua` → `init.test.lua` | One shard per file, whose cases run in sequence in one Neovim |
| Integration | `tests/integration/` in the mirrored directory of the source area it drives, in the file named after that area: `views/status/` → `nvim/lua/forge/tests/integration/views/status/status.test.lua`, and a top-level module's area is a directory named after it, such as `integration/client/client.test.lua` for `client.lua` | One Neovim per top-level case |
| Support | `tests/support/` for shared helpers, `tests/fixtures/` for data and shared setup scripts, `tests/plans/` for notes | Plain `.lua` or Markdown; never run as tests |

Forge's integration areas are `buffer`, `client`, `git`, `lsp`, `review_document`, `text`, `walkthrough`, and, under `views/`, `harness`, `picker`, `plan_review`, and `status`. Each area name is also a suite. An integration test stays a standalone file in its area directory only when it reloads its own file in a child Neovim, such as `harness_markdown_layout.test.lua`, or shares a heavy `pre_once` setup across its cases, such as `plan_review_question_response.test.lua`. Load helpers by repository path, such as `dofile("nvim/lua/forge/tests/support/state.lua")`, at file scope.

A test file returns a `MiniTest.new_set()` whose hooks come from `support/isolation.lua`. It declares each test as a top-level `T["name"] = function() ... end` at column zero, which is how the runner finds cases:

```lua
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks({
  post_case = function() require("forge.lsp.startup").close() end,
}) })

T["close_without_start"] = function()
  MiniTest.expect.no_error(require("forge.lsp.startup").close)
end

return T
```

Add a new test as a case in the existing unit or area file rather than as a new file, and move a unit test file whenever its source file moves. Compare values with `MiniTest.expect.equality` and check failures with `MiniTest.expect.error` rather than hand-written assertion helpers.

### Isolation

Cases in a unit shard share one Neovim, so a case must leave nothing behind. `isolation.hooks()` captures the editor before each case and restores it afterward, whether or not the case failed. It first runs the caller's `post_case`, which stops work the case started, and then drains the `vim.schedule` queue so that no callback fires in the next case. It then deletes the case's autocommands and timers. It restores replaced `vim.*` functions, global options, `vim.g`, Lua globals, the working directory, global keymaps, user commands, and the starting window's options. It closes the case's tabpages and windows, wipes its buffers, and unloads every `forge`, `github`, `git`, and `rust_sidecar` module. Require plugin modules inside cases or hooks, never at file scope, because the next case receives fresh module singletons.

Cleanup that a case owns goes through `MiniTest.finally`. mini.test runs those callbacks without protection, so an error raised in one escapes the case; wrap fallible cleanup in `pcall`. A callback runs when the function that registered it returns, and a hook is such a function, so call a helper that registers a `finally` from the case body, never from a hook.

### Child Neovim

A file that must reload itself in a fresh editor starts a child with `MiniTest.new_child_neovim()`, attaches the grid size it needs through `child.api.nvim_ui_attach`, and reads the screen with `child.get_screenshot()`. A self-relaunching file checks a `vim.g` child flag at the very top, before `require("mini.test")`, runs its child body, and returns; otherwise it builds and returns `T`. The child's dispatch never reads `arg`, which the shard bootstrap owns. On Windows the bootstrap gives each child pipe a process-unique name, because mini.test derives the name from a `tempname()` counter that restarts in every process, and concurrent tests would otherwise connect to each other's children.

### Shared helpers

`tests/support/` holds the code that cases would otherwise repeat. Each helper below takes effect for the rest of the current case and undoes itself through `MiniTest.finally`, except `wait.lua`, `buffer.lua`, and `keymap.lua`, which change nothing that outlives the call.

| Helper | Use |
| --- | --- |
| `wait.lua` | `wait_until(condition, message, { timeout_ms, errors, context })` polls every 5 ms for at most 1000 ms by default and fails with `message`. It fails fast when the `errors` list is non-empty and appends `context()` to the failure. Keep `timeout_ms` at or below 4000, because each case is killed at five seconds. |
| `buffer.lua` | It reads and writes whole buffers through `lines`, `text`, and `set`, reads one-based rows through `line`, creates `scratch` buffers, and finds the row containing some text through `find_row`. Files with a local `buffer` variable load it as `buffer_support`. |
| `keymap.lua` | `invoke` runs a mapping's Lua callback in the current window, resolving a buffer-local mapping in `buffer`. `get` asserts that a mapping exists, `find` returns nil when it does not, and `feed` types keys through `vim.keycode`. |
| `temp.lua` | `directory(label)` and `file(label, lines)` create temporary paths that are deleted when the case ends. |
| `stub.lua` | `stub(target, key, value)` replaces one field and returns the original. |
| `notify.lua` | `capture({ min_level, notifications })` records `vim.notify` messages and, optionally, `forge.infra.notifications` reports, and offers `messages()`, `clear()`, and `assert_none()`. |
| `mock_client.lua` | `record()` keeps every `forge.client.request`, and `respond(responses)` answers by method. Both accept `wrap_callback` and `copy_params`. |
| `host.lua` | `start({ data, config, schedule_ensure })` launches the real host with temporary data and the pinned executable, and `stop()` returns whether its process was collected. |
| `git_repository.lua` | `create({ path, branch, bare, files, message })` builds a fixture repository with a fixed identity, and `run(arguments, { text })` fails the case on a nonzero exit. |

## Suites

The `lua-test` check runs every Lua test through the runner. Its default `affected` selection maps changed paths to tests. A changed test file runs itself; a changed Lua source file runs its mirrored unit test and its integration area, the nearest mirrored integration directory of the file or an ancestor; a source file with no area runs its plugin's whole test directory; and any other change, including a change under `tests/` or to the runner, runs every test. Its `suite` input accepts `all`, an area name such as `status`, which selects that area's unit and integration mirrors, or a directory under `nvim/lua`.

## Run a fixture

`.forge/checks/lua-tests.nu` is the only way to run Lua tests, both inside Harness, where the `lua-test` check calls it, and outside. The runner clones the pinned mini.test commit once into a commit-named directory beside the Nushell cache. It runs every unit shard and every integration case in its own fresh Neovim from the repository root: `nvim --headless -i NONE -u NONE --cmd "set rtp^=./nvim" --cmd "lua vim.loader.enable(false)" --cmd "set rtp^=<mini.test>" -l nvim/lua/forge/tests/support/minitest_shard.lua <file> [case]`. The shard bootstrap arms a five-second deadline for each case as it starts and exits with status 124 naming the hung case, so a timeout ends the shard and later cases in a unit file do not run. The runner kills the whole process tree, including any Forge host, of a single-case process still running one second past that deadline, and of a unit shard still running after one minute. It runs processes in parallel, by default on a sixth of the logical CPUs, because host tests contend for process launches and `git`.

Inside Harness, select tests through the check:

```text
check_run({"check":"lua-test","inputs":{"fixture":"nvim/lua/forge/tests/unit/views/plan_review/checks.test.lua"}})
check_run({"check":"lua-test","inputs":{"suite":"harness"}})
```

Use a `suite` only when a whole area needs coverage.

Outside Harness, call the runner directly. Without arguments it runs every test. Each argument is a test file, a source file (which selects its unit test), a directory, a test name (the file name without `.test.lua`, which selects every file with that name), or any of these followed by `::case`, which names one top-level case. A source directory selects both of its mirrors:

```nu
nu .forge/checks/lua-tests.nu client                                 # unit/client.test.lua and integration/client/client.test.lua
nu .forge/checks/lua-tests.nu client::admission_queue                # one case
nu .forge/checks/lua-tests.nu nvim/lua/forge/tests/unit/views/plan_review/checks.test.lua
nu .forge/checks/lua-tests.nu nvim/lua/forge/lsp                     # unit and integration tests for lsp/
nu .forge/checks/lua-tests.nu --suite status --jobs 4                # a source area as the suite
nu .forge/checks/lua-tests.nu --affected nvim/lua/forge/walkthrough.lua
nu .forge/checks/lua-tests.nu
```

The runner prints each test's result and duration as it finishes, then the slowest tests and a pass and fail count, and exits nonzero when any test fails. `--list` prints the selection without running it. Every case must finish within the five-second limit, and there are no per-test exceptions. A test that exceeds it because it waits on the real clock or hangs must be fixed with a deterministic signal or removed.

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
