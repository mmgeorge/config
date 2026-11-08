---
name: mini-test
description: >-
  Write, run, and debug Neovim plugin tests with mini.test (MiniTest.new_set, MiniTest.expect,
  hooks, parametrize, MiniTest.new_child_neovim, child.type_keys, reference screenshots, reporters,
  collect and execute options). Use when creating or editing mini.test test files, setting up a
  project to run mini.test headlessly or in CI, testing editor UI and keystrokes through a child
  Neovim, running mini.test files in parallel shards, or diagnosing failing, hanging, or flaky
  mini.test cases. Do not use for plenary.nvim or busted test suites.
targets:
  - '*'
---

# mini.test

mini.test is the testing module of mini.nvim, also distributed standalone as `nvim-mini/mini.test`. It sources test files, flattens their test sets into an array of cases, and executes the cases one after another inside the Neovim process that called it. Three consequences shape every test suite built on it:

- **State leaks between cases.** All cases of a run share one Lua state, editor, and event loop. mini.test provides no mocks or stubs; the intended isolation is a child Neovim process restarted before each case (see [child-neovim.md](references/child-neovim.md)), with hooks restoring anything the parent process changed.
- **No parallelism.** The help lists parallel execution as unsupported "due to idea of limiting implementation complexity", and mini.nvim runs its own suite sequentially. Parallel runs need an outer scheduler that shards files or cases across processes (see [parallel-runs.md](references/parallel-runs.md)).
- **No per-case timeout.** A case that never returns blocks the run forever; only an outer process deadline stops it.

The authoritative sources are `:h mini.test` (`doc/mini-test.txt`) and `TESTING.md` in mini.nvim. When behavior here is in doubt, read `lua/mini/test.lua` of the pinned checkout.

## Project setup

1. Pin the dependency. Clone mini.test (or mini.nvim) into an ignored dependency directory at a release tag or commit, such as `git clone --filter=blob:none --depth=1 --branch v0.18.0 https://github.com/nvim-mini/mini.test deps/mini.test`, from a Makefile target named after that directory so it runs once. Many projects clone `main` unpinned; that lets reporter output and helper behavior change under the suite between runs.
2. Write a minimal init, conventionally `scripts/minimal_init.lua`, that puts the plugin and the dependency on `'runtimepath'` and sets mini.test up only when headless:

   ```lua
   vim.cmd([[let &rtp.=','.getcwd()]])
   if #vim.api.nvim_list_uis() == 0 then
     vim.cmd("set rtp+=deps/mini.test")
     require("mini.test").setup()
   end
   ```

3. Run headlessly from the project root:

   ```sh
   nvim --headless --noplugin -u ./scripts/minimal_init.lua -c "lua MiniTest.run()"
   nvim --headless --noplugin -u ./scripts/minimal_init.lua -c "lua MiniTest.run_file('tests/test_feature.lua')"
   ```

   The same init serves child processes (`child.restart({ "-u", "scripts/minimal_init.lua" })`), so it is also where a suite pins everything a test depends on: a fixed colorscheme, `statusline`, and options that screenshots capture, a replacement for network clients that raises on any real request, and a `vim.notify` filter that keeps expected warnings out of the output.

   The headless default reporter, `MiniTest.gen_reporter.stdout()`, ends with `cquit 1` when any case failed and `cquit 0` otherwise, so the shell exit status reports the result.

By default `MiniTest.run()` first sources `scripts/minitest.lua` (`config.script_path`) and stops there if that script succeeds; otherwise it collects `vim.fn.globpath("tests", "**/test_*.lua", true, true)`. Pass `script_path = ""` to `setup()` when a run must ignore a project script. `vim.g.minitest_disable` or `vim.b.minitest_disable` turns runs into no-ops, and `vim.b.minitest_config` overrides config per buffer.

## Test file shape

A test file returns one test set. Build it with `MiniTest.new_set()` and add entries with `T["name"] = ...`, which preserves insertion order; entries passed through the second argument of `new_set` have no order guarantee.

```lua
local MiniTest = require("mini.test")
local new_set, expect, eq = MiniTest.new_set, MiniTest.expect, MiniTest.expect.equality

local T = new_set()

T["compute()"] = new_set()

T["compute()"]["joins lines"] = function()
  eq(require("my_plugin").compute({ "a", "b" }), "a\nb")
end

T["compute()"]["rejects nil"] = function()
  expect.error(function() require("my_plugin").compute(nil) end, "lines must be a table")
end

return T
```

- A callable entry is a case; a nested set groups cases and inherits its parent's options; any other value is an ignored helper.
- A case's description, `case.desc`, is the file path followed by each key on the way down, such as `{ "tests/test_feature.lua", "compute()", "joins lines" }`. Filters, reporters, and default screenshot names all use it.
- Keep names short and distinct ignoring case. Default screenshot files are named from the description, and case-insensitive file systems and path-length limits break long or case-colliding names.
- busted-style `describe`/`it` works through `collect.emulate_busted` (on by default), but it lacks parametrization and set data. Write test sets.

## Sets: hooks, parametrize, data, retry

`MiniTest.new_set(opts)` accepts:

- `hooks`: `pre_once` before the first filtered case of the set, `pre_case` before each case including nested ones, `post_case` after each case, and `post_once` after the last filtered case. For one case, `pre_case` hooks run from the outermost set inward and `post_case` hooks from the innermost outward. An error in any hook fails the case. After a failing `pre_case`, mini.test skips the case body with the note `Skip case due to error(s) in hooks.` and still runs every `post_case` hook, so `post_case` must tolerate partially created state.
- `parametrize`: an array of argument arrays, such as `{ { 1 }, { 2 } }`. Every case of the set runs once per inner array, with its elements appended to the arguments from enclosing sets; nested parametrizations multiply. `parametrize = {}` collects zero cases; omit the option for none.
- `data`: a table merged down to cases as `case.data`, for filtering.
- `n_retry`: rerun a failing case, with its `*_case` hooks, up to this many times. A retry hides a flake instead of fixing it; prefer making the case deterministic.

Hooks are the reliable cleanup point because they run whether or not the case failed. Within one callable, `MiniTest.finally(fn)` registers cleanup that runs after that callable ends, on success or error; use it next to the code that created a temporary directory, buffer, timer, or autocommand.

## Expectations and case helpers

- `MiniTest.expect.equality(left, right)` (aliased `eq`) compares with `vim.deep_equal` and reports both values plus the first differing key branch, such as `2->1`. Write `eq(observed, expected)`.
- `expect.no_equality(left, right)` is the negation.
- `expect.error(fn, pattern)` passes when `fn` raises; a non-empty `pattern` must match the message as a Lua pattern, so escape literal text with `vim.pesc`. `expect.no_error(fn)` passes when `fn` returns.
- `expect.reference_screenshot(screenshot, path, opts)` compares a child screenshot with a stored file (see [child-neovim.md](references/child-neovim.md)).
- Every expectation takes `{ fail_reason = string | function(...) }`.
- `MiniTest.new_expectation(subject, predicate, fail_context)` builds a reusable expectation with the same reporting; define repeated domain checks once at file scope.
- `MiniTest.skip(message)` ends the case as passed with a note; inside `pre_case` it skips the case body. Reserve it for environments the case cannot run in.
- `MiniTest.add_note(message)` attaches context; a case with notes reports `Pass with notes` or `Fail with notes`, and notes never fail a case.
- `MiniTest.current.case` is the running case and `MiniTest.current.all_cases` the executed array; `#MiniTest.current.case.exec.fails > 0` inside a `finally` callback detects failure for diagnostics.

## Asynchronous code

mini.test executes its queue through `vim.schedule`, but each case is synchronous: it ends when its function returns, and an error raised later in a callback is not attributed to it. Drive asynchronous work to completion inside the case and assert afterward:

```lua
T["fetch() delivers lines"] = function()
  local result, failure
  require("my_plugin").fetch(function(error, lines) failure, result = error, lines end)
  assert(vim.wait(2000, function() return result ~= nil or failure ~= nil end, 5), "fetch never completed")
  eq(failure, nil)
  eq(result, { "line" })
end
```

Capture callback errors into locals instead of asserting inside the callback. Treat the `vim.wait` timeout as a hang guard, never as a sleep: a test that waits for a real timer or deadline to elapse is slow and fails under load. Inject the clock or call the timeout path directly.

Before the fix for nvim-mini/mini.nvim#2423 (landed on `main` in May 2026), a `vim.wait` in the test process could let mini.test's scheduled queue run ahead, so `reporter.finish` fired before the case ended. Pin a commit that contains the fix when cases wait in the test process; otherwise run the asynchronous work in a child.

The maintainer's recommended pattern for asynchronous code puts it in a child: start the work there, record results in a child global such as `_G.log`, and read them back once they exist. [child-neovim.md](references/child-neovim.md) covers the polling helpers.

## Selecting and customizing runs

`MiniTest.run(opts)` takes the shape of `MiniTest.config`:

- `collect.find_files()` returns the files to source. `MiniTest.run_file(path)` replaces it with one file.
- `collect.filter_cases(case)` keeps cases for which it returns true; filter on `case.desc` or `case.data`, such as `function(case) return case.desc[2] == "compute()" end`. Filtering runs before `*_once` hooks are attached, so `pre_once` and `post_once` still run around the filtered subset.
- `MiniTest.run_at_location({ file = path, line = n })` runs the cases whose function spans that line; without arguments it uses the cursor position in the current buffer. mini.test has no `only` marker; the maintainer's answer for running one case from a shell is this function:

  ```make
  test_case: deps/mini.test
  	nvim --headless --noplugin -u ./scripts/minimal_init.lua \
  		-c "lua MiniTest.run_at_location({ file = '$(FILE)', line = $(LINE) })"
  ```
- `execute.reporter` takes a table with `start(cases)`, `update(case_number)`, and `finish()`. `gen_reporter.stdout({ group_depth, quit_on_finish })` is the headless default; `gen_reporter.buffer()` is the interactive default with a floating window, where `q` or `<Esc>` stops a run.
- `execute.stop_on_error = true` stops after the first failing case.
- `MiniTest.collect(opts)` and `MiniTest.execute(cases, opts)` run the two stages separately, for scripts that inspect or wrap cases before execution.

Interactive runs (`:lua MiniTest.run_file()`) execute cases inside the editing session, so any case that changes editor state changes the session. Run interactively only when every case confines its effects to a child Neovim.

## Shared helpers module

Suites larger than a file keep shared code in `tests/helpers.lua`, which test files load with `dofile("tests/helpers.lua")` from the project root. mini.nvim's own helpers set the conventions most projects copy:

- `Helpers.expect` is `vim.deepcopy(MiniTest.expect)` extended through `MiniTest.new_expectation` with `match(str, pattern)`, `no_match`, and `equality_partial_tbl` (compare only the keys present in the expected table).
- `Helpers.new_child_neovim()` wraps `MiniTest.new_child_neovim()` and adds `child.setup()`, which restarts the child with the project's minimal init and sets `child.bo.readonly = false`, plus buffer helpers such as `child.set_lines(lines)`, `child.get_lines()`, `child.set_cursor(row, col)`, and `child.set_size(lines, columns)`. Each helper refuses to run against a blocked child.
- Module lifecycle helpers load a plugin in the child, and unload it by clearing `package.loaded`, its global, and its augroup, so each case starts from a clean module.
- Platform predicates (`is_windows()`, `is_ci()`) back inline `MiniTest.skip` calls for cases a platform cannot run.

mini.nvim also scales sleep durations and `n_retry` counts per operating system on CI, because its timing tests are inherently timing-dependent. A suite whose cases avoid real-time waits needs neither.

## Isolation checklist for parent-process cases

Cases that run directly in the test process, without a child, must leave it as they found it:

- Capture each replaced function, option, or global in `pre_case` and restore it in `post_case`.
- Stop timers, jobs, servers, and module state the case started, before restoring the functions those modules call.
- Reset `package.loaded[name]` for any module a case replaced or reloaded.
- Delete buffers, windows, autocommand groups, and temporary files through `MiniTest.finally` or `post_case`.
- Prove independence by running each case alone (filter on its description) and the whole file in order; a case that passes alone but fails in sequence depends on a leak.

When a case needs a pristine editor rather than restored state, move it into a child Neovim.

## Diagnosing failures

- `Failed expectation for equality` prints `Left`, `Right`, and the differing key branch; the traceback's first project line is the failing expectation.
- `Sourcing <file> resulted into following error` means the file raised at load time, before any case ran; `<file> does not define a test set` means it returned nothing.
- A headless run that never exits usually has a hung case, or, on mini.test versions before the fix for nvim-mini/mini.nvim#2538, a filter that matched no cases.
- `Cannot convert given lua type`, `Can not use child.<method> because child process is blocked`, and `Failed to make connection to child Neovim` are child-process errors; read [child-neovim.md](references/child-neovim.md).
- A case that fails only after another case is a state leak; apply the isolation checklist.

## References

- Read [child-neovim.md](references/child-neovim.md) when a case starts a child Neovim, types keys, inspects the screen, or uses reference screenshots.
- Read [parallel-runs.md](references/parallel-runs.md) when running mini.test files concurrently, sharding a suite across processes, or adding per-case deadlines.
