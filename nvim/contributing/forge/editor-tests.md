# Editor tests

A Rust test cannot show that a keystroke reaches the right buffer, and a Lua mock cannot show that the real host keeps its queue. Editor fixtures close that gap by driving real Neovim, and host fixtures add the real Rust host. This guide explains which suite to use, how to run a fixture, and how to control provider timing without sleeping.

Host fixtures need a built executable. See [Building and testing](building-and-testing.md).

## Suites

| Check | Runs | Provider |
| --- | --- | --- |
| `lua-test` | Ordinary module fixtures | None |
| `lua-test-mock-integration` | Host fixtures | `MockBackend` or a simulated process, with no model requests |
| `lua-test-live-integration` | Fixtures that use installed provider CLIs | Real, and may make model requests |
| `lua-benchmark` | Benchmarks | Varies |

Ordinary `lua-test` and its `all` selection exclude both integration suites and benchmarks. The registry rejects a live fixture submitted to the mock check. Run the live check only after checking credentials and each fixture's prerequisites. Its fixtures include `harness_skill_reads_host.lua`, `harness_codex_activity.lua`, `harness_native_import_host.lua`, and `harness_provider_switch_host.lua`.

## Run a fixture

Inside Harness, select a fixture through its check:

```text
check_run({"check":"lua-test","inputs":{"fixture":"nvim/tests/forge/plan_check_selection.lua"}})
check_run({"check":"lua-test-mock-integration","inputs":{"files":["nvim/tests/forge/harness_host.lua"]}})
```

Use `inputs:{"suite":"walkthrough"}` only when a whole feature needs coverage. Omitting `files` from an integration check selects its whole suite.

Outside Harness, `nvim/tools/run_tests.nu` runs each fixture in a child process and enforces a deadline, so a fixture that prints success but stays alive fails:

```nu
nu nvim/tools/run_tests.nu --minimal plan_check_selection
nu nvim/tools/run_tests.nu --minimal --timeout 60sec harness_settings
```

To run a fixture by hand, start a fresh Neovim and enforce your own deadline:

```nu
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_check_selection.lua
```

Parameterized fixtures for languages and providers need the setup in [Verification by area](verification-by-area.md), and fail without it.

## Control provider timing

An interaction test often has to act after the provider receives a prompt but before it responds. Elapsed-time sleeps make these tests flaky, so `nvim/tests/forge/support/mock_provider.lua` holds the real host's `MockBackend` through `session.provider.mock_response` instead. Create the hold before submitting, then wait for the prompt to arrive:

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

## Submission races

`harness_host.lua` runs `support/submission_races.lua` against the real Rust host, SQLite storage, Lua input reconciliation, and transcript presentation. Only the provider response and selected transport callbacks are controlled.

Add a race case to that module with an explicit provider or callback barrier, and assert durable exchange identity, queue contents, and preservation of newer composer text. Do not replace the Rust queue with a Lua mock, or depend on sleeps to order events. Every wait needs a diagnostic deadline, and teardown must restore intercepted callbacks and collect the isolated host when an assertion fails.
