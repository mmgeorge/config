# Child Neovim and screenshots

*When to read this:* When a mini.test case runs in a child Neovim, types keys, waits for asynchronous child work, inspects the screen, or uses reference screenshots through `MiniTest.new_child_neovim()`, `child.type_keys()`, `child.get_screenshot()`, or `MiniTest.expect.reference_screenshot()`.

## Contents

- When to use a child
- Lifecycle
- Running code in the child
- Mocks and callbacks
- Waiting for asynchronous work
- Typing keys and modes
- Screenshots
- Pitfalls

## When to use a child

mini.test runs every case in the test process, so a child Neovim is the only way to give a case a pristine editor. Use one for anything that touches editor state beyond a pure function: buffers, windows, options, autocommands, keymaps, rendering, startup, and plugin setup. A child also keeps the test process's own state, including mini.test's reporter, out of the code under test. Pure Lua functions with no editor effects can run directly in the test process.

## Lifecycle

Restart the child before every case and stop it after the last one:

```lua
local MiniTest = require("mini.test")
local child = MiniTest.new_child_neovim()

local T = MiniTest.new_set({
  hooks = {
    pre_case = function()
      child.restart({ "-u", "scripts/minimal_init.lua" })
      child.bo.readonly = false
      child.lua([[M = require("my_plugin")]])
    end,
    post_once = child.stop,
  },
})
```

- `child.start(args, opts)` launches `opts.nvim_executable` (default `v:progpath`) with `--clean -n --listen <address> --headless --cmd "set lines=24 columns=80"` followed by `args`, through `jobstart`. On Windows the address is a `\\.\pipe\mininvim<name>` named pipe, where `<name>` is the tail of `vim.fn.tempname()`, a counter that restarts in every Neovim process. Concurrent test processes therefore choose the same pipe, and a case can connect to another process's child. When running test processes in parallel on Windows, wrap `child.start` so that `vim.fn.tempname` returns a process-unique name, for example by appending `vim.fn.getpid()`, while the child starts. It retries `sockconnect` every 10 ms until `opts.connection_timeout` (default 5000 ms), then raises `Failed to make connection to child Neovim`.
- `child.restart(args, opts)` stops a running child and starts another, reusing the previous `args` and `opts` when omitted.
- `child.stop()` sends `0cquit`, waits up to one second for exit, and closes the channels. mini.test registers every child and stops them all on `MiniTest.stop()`.
- The child inherits the test process's working directory, so a minimal init that adds `getcwd()` to `'runtimepath'` makes the plugin loadable. `--clean` alone loads no plugin code.
- Each restart costs one Neovim startup, tens of milliseconds, per case.

## Running code in the child

Every call is an RPC; call fields with `.`, never `:`.

| Call | Use |
| --- | --- |
| `child.lua(code, args)` | Run a Lua chunk; `...` receives `args`; returns the chunk's value. |
| `child.lua_get(expression, args)` | Return an expression's value. |
| `child.lua_func(fn, ...)` | Run a test-process function in the child through `string.dump`. It loses its upvalues, so pass every input as an argument. |
| `child.lua_notify(code, args)` | Run without waiting for a result, for calls that block, such as opening a picker that waits for a choice. |
| `child.api.*`, `child.api_notify.*` | `vim.api` in the child, waiting or not. |
| `child.fn`, `child.lsp`, `child.treesitter`, `child.diagnostic`, `child.fs`, `child.uv`, `child.json`, `child.hl` | Redirection tables for the matching `vim.*` namespace. |
| `child.o`, `child.bo`, `child.wo`, `child.go`, `child.g`, `child.b`, `child.w`, `child.t`, `child.v`, `child.env` | Read or assign options and variables, such as `child.o.lines = 10`. |
| `child.cmd(command)`, `child.cmd_capture(command)` | Run Ex commands, optionally returning output. |
| `child.is_running()`, `child.is_blocked()` | Process state; `is_blocked` reads `nvim_get_mode().blocking`. |

A small forwarding helper keeps call sites readable: `local get_state = function(...) return child.lua_get("M.get_state(...)", { ... }) end`.

## Mocks and callbacks

Functions and userdata cannot cross RPC in either direction; the error is `Cannot convert given lua type`. Keep callbacks and mocks on the child side:

- Record callback results in a child global and read them back: `child.lua([[_G.log = {}; M.on_change(function(value) table.insert(_G.log, value) end)]])`, then `eq(child.lua_get("_G.log"), { "a" })`. Child globals vanish on the next restart.
- Put mocks in a Lua file and load it into the child, such as `child.lua([[dofile("tests/mocks/spawn.lua")]])` replacing `vim.uv.spawn` with a version fed from a child global. mini.nvim mocks external processes this way.
- Send configuration that contains functions as Lua source evaluated in the child, not as a table argument.

## Waiting for asynchronous work

mini.test has no event-loop helper; mini.nvim's `tests/helpers.lua` adds `child.poke_eventloop = function() child.api.nvim_eval("1") end`, one synchronous round trip that makes the child process pending events. Its tests follow a notify-poke-assert sequence: `child.lua_notify(...)`, `child.poke_eventloop()`, then the assertion or screenshot.

For work that takes several event-loop turns, poll a condition rather than sleeping a fixed time:

```lua
local function wait_for_child(condition_expression, timeout_ms)
  local deadline = vim.uv.now() + (timeout_ms or 2000)
  while not child.lua_get(condition_expression) do
    if vim.uv.now() > deadline then
      error(("Timed out waiting for %s\n%s"):format(condition_expression, tostring(child.get_screenshot())))
    end
    child.cmd("sleep 10m")
  end
end
```

Attaching a screenshot to the timeout error, as grug-far.nvim does, shows the state the child was stuck in. A child-side `vim.wait` also works: `child.lua_get("vim.wait(2000, function() return _G.done end, 5)")`.

## Typing keys and modes

`child.type_keys(wait, ...)` feeds each entry to `nvim_input` in the child and raises if the keys set `v:errmsg`. Entries are strings or nested arrays: `child.type_keys("i", "text", "<Esc>")`. A leading number waits that many milliseconds after each entry. Pass a lone `<` as `"<"`, not `"<LT>"`. `child.ensure_normal_mode()` sends `<C-\><C-n>` to leave insert, visual, terminal, or operator-pending mode. `type_keys` does not check for a blocked child.

## Screenshots

`child.get_screenshot({ redraw = true })` returns `text` and `attr` grids for every cell of the child's screen, or `nil` when no reasonable screenshot exists. `tostring(screenshot)` renders both grids with a column ruler and line numbers. `attr` symbols only say whether two cells look the same, not which highlight they use; past 94 distinct attributes the symbols repeat.

`MiniTest.expect.reference_screenshot(screenshot, path, opts)`:

- Without `path`, it writes to `opts.directory` (default `tests/screenshots`) under a name built from the full case description, including the test file path and any parametrized arguments, with forbidden characters replaced by `-`. A second screenshot in the same case gets a `-002` suffix. mini.nvim keeps thousands of such files flat in `tests/screenshots/`. Keep case names short: case-insensitive file systems collide names that differ only in case, and long descriptions exceed path limits (260 bytes in some Git-on-Windows setups).
- A missing file is created and the case passes with the note `Created reference screenshot at path …`. Review every created file before committing it, since the first run proves nothing.
- `opts.force = true` rewrites the reference; delete the file and rerun to get the same effect without a code change. Regenerate a whole suite by deleting `tests/screenshots/*` and running it, then review the diff.
- `opts.ignore_text` and `opts.ignore_attr` take `true` or an array of line numbers, for rows that legitimately vary. mini.nvim's helper masks the command-line rows this way.
- Stabilize what the screen shows before rendering: set a small `child.o.lines` and `child.o.columns`, and pin the colorscheme, `statusline`, `ruler`, and highlight links in the minimal init. Give UI elements distinct highlights (such as `child.cmd("hi MyPluginBorder guifg=Red")`) so the attribute grid distinguishes them.
- Rendering changes between Neovim releases invalidate references; suites that test several Neovim versions update references per version.

## Pitfalls

- A child waiting at a hit-enter prompt or in operator-pending mode is blocked. `lua`, `lua_get`, `lua_func`, and `get_screenshot` raise `Can not use child.<method> because child process is blocked`; `child.api` and `type_keys` do not check and hang the run. Raise `cmdheight`, avoid multi-line messages, call `child.ensure_normal_mode()` after partial key sequences, and use `api_notify` or `lua_notify` for calls that legitimately block.
- The first buffer of a fresh child can be `readonly`, which slows edits; set `child.bo.readonly = false` after each restart.
- To debug a hanging child, replay the case's steps in an interactive `nvim --clean -u scripts/minimal_init.lua`; the maintainer declined to add a debugging proxy (nvim-mini/mini.nvim#914).
- Windows child connections timed out before mini.nvim PR #1101 (August 2024); use a newer mini.test on Windows.
