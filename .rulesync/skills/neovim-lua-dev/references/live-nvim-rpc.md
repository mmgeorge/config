# Driving Live Neovim Instances over RPC

This reference establishes procedures for querying live editor state, inspecting buffers, evaluating Lua expressions, reloading modified plugin modules, and verifying runtime behavior via Neovim msgpack-RPC.

## Identify the Target Before Input

A remote UI shares the target editor's process, buffers, mappings, and active tasks. Attaching Terminal MCP to the editor hosting the agent gives that agent control over its own host. Treat a discovered socket as an existing user editor unless the current test created its process.

1. Start a separate Neovim process with a unique `--listen` address and isolated session storage for interactive reproduction.
2. Record the launched process ID and query `vim.fn.getpid()`, `vim.v.servername`, and `vim.fn.getcwd()` through that socket to verify the target.
3. Attach Terminal MCP only after confirming the process identity. Inspect mode and buffer-local mappings before sending input.
4. Capture terminal output and the test process's exit status. Terminate only that test process during cleanup.

For inspection of an existing user editor, prefer read-only RPC. If a remote UI is needed to observe rendering, detach that UI with `:detach` when finished. Do not send quit commands or close its terminal as a substitute for detaching. If process identity cannot be established, do not send input that changes editor state.

## RPC Socket Detection

When executed inside a Neovim terminal session, `$NVIM` contains the server socket address (a Unix domain socket on Unix platforms or a named pipe such as `\\.\pipe\nvim.*` on Windows).

```powershell
# PowerShell socket verification
$env:NVIM
```

```bash
# Bash socket verification
echo "$NVIM"
```

If `$NVIM` is unset, the shell is not executing within an attached Neovim terminal. Report that the RPC socket is unavailable rather than assuming active server state.

## Output Filtering for NVIM_APPNAME

When `NVIM_APPNAME` is active, `nvim --server` prepends diagnostic warnings (`Warning: Using NVIM_APPNAME=...`) to stdout, corrupting structured JSON responses. Filter stdout before parsing:

```powershell
$result = nvim --server $env:NVIM --remote-expr 'EXPR'
$clean = $result | Where-Object { $_ -notmatch '^Warning: Using NVIM_APPNAME=' }
```

```bash
result=$(nvim --server "$NVIM" --remote-expr 'EXPR') && echo "$result" | grep -v '^Warning: Using NVIM_APPNAME='
```

Use command substitution (`$(...)`) rather than piping directly to ensure proper variable expansion on all platforms.

## Evaluating Expressions (`--remote-expr`)

`--remote-expr` executes read-only queries against the live instance:

- **Single Lua Expressions:**
  ```powershell
  nvim --server $env:NVIM --remote-expr 'luaeval("vim.api.nvim_buf_get_name(0)")'
  ```
- **Multi-Statement Blocks (IIFE Pattern):**
  ```powershell
  nvim --server $env:NVIM --remote-expr 'luaeval("(function() return vim.api.nvim_win_get_number(0) end)()")'
  ```
- **Complex Data Structures (JSON Encoding):**
  ```powershell
  nvim --server $env:NVIM --remote-expr 'luaeval("vim.json.encode(vim.api.nvim_list_bufs())")'
  ```

Wrap multi-line or stateful Lua evaluations in immediately invoked function expressions (IIFE) and encode tables with `vim.json.encode`.

## Executing Ex Commands (`execute()`)

Use `execute()` within `--remote-expr` to trigger plugin commands and capture output:

```powershell
nvim --server $env:NVIM --remote-expr 'execute("ForgeStatus")'
nvim --server $env:NVIM --remote-expr 'execute("LspRestart")'
```

## Keystroke Simulation and File Opening

Before sending keys, query `vim.api.nvim_get_mode()` and `vim.fn.maparg(key, mode, false, true)` in the target buffer through `nvim --headless --server <socket> --remote-expr`. Inspect callback mappings at their source. Recheck after changing buffers or modes. In this configuration, Normal-mode Ctrl+W invokes `:qa!`, and `i` and `k` do not have their default Vim meanings. Never send Ctrl+W for window navigation.

Select windows with `vim.api.nvim_list_wins()` and `vim.api.nvim_set_current_win(window_id)` over RPC, then inspect the rendered result through Terminal MCP. Enter prompt text only after `:startinsert` and a confirmed Insert mode. Inspect the target buffer's submission mapping before using it.

`nvim --remote-ui --server <socket>` attaches to the existing process. A separate terminal does not make that editor disposable. Finish observation with `:detach` to detach only the observing UI. Quit only test editors created for the current verification, never the user's editor or the editor hosting the agent.

- **`--remote-send`**: Dispatches raw keystrokes into the active editor buffer. Reserve for workflows that require end-to-end keymap validation.
- **`--remote` / `--remote-tab`**: Opens specified files in existing or new tabs within the user session.

```powershell
nvim --server $env:NVIM --remote-send ':echo "hello"<CR>'
nvim --server $env:NVIM --remote nvim/lua/forge/init.lua
```

## Common Diagnostic Queries

- **Active Buffer Path:**
  ```lua
  luaeval("vim.api.nvim_buf_get_name(0)")
  ```
- **Current Working Directory:**
  ```lua
  luaeval("vim.fn.getcwd()")
  ```
- **Window Cursor Coordinates:**
  ```lua
  luaeval("vim.json.encode(vim.api.nvim_win_get_cursor(0))")
  ```
- **Attached LSP Clients:**
  ```lua
  luaeval("vim.json.encode(vim.tbl_map(function(c) return c.name end, vim.lsp.get_clients({ bufnr = 0 })))")
  ```
- **Buffer Diagnostics:**
  ```lua
  luaeval("vim.json.encode(vim.diagnostic.get(0))")
  ```
- **Extmark Highlighting and Inspection:**
  ```lua
  luaeval("vim.json.encode(vim.api.nvim__inspect_cell(0, ROW, COL))")
  ```

Use `nvim__inspect_cell` to inspect the composite highlight stack for a specific screen cell when debugging highlight precedence conflicts.

## Runtime Path and Plugin Resolution

Query runtime paths dynamically instead of opening interactive help splits:

```powershell
# Locate plugin installation directories
nvim --server $env:NVIM --remote-expr 'luaeval("require(\"lazy.core.config\").plugins[\"trouble.nvim\"].dir")'

# Locate runtime files
nvim --server $env:NVIM --remote-expr 'luaeval("vim.json.encode(vim.api.nvim_get_runtime_file(\"lua/forge/**\", true))")'
```

## Hot-Reloading Plugin Modules

To verify plugin modifications in a running session, clear the package cache for each affected module, invoke `setup()`, and test command execution:

```powershell
nvim --server $env:NVIM --remote-expr 'execute("lua package.loaded[\"forge\"] = nil; package.loaded[\"forge.views.status.state\"] = nil")'
nvim --server $env:NVIM --remote-expr 'execute("lua require(\"forge\").setup({})")'
nvim --server $env:NVIM --remote-expr 'execute("ForgeStatus")'
```

`package.loaded` caches every submodule separately. Clear all modified submodule paths explicitly before reloading the parent package.

## Operational Safety Rules

1. Do not dispatch destructive commands (`:q`, `:qa`, `:bd`, `:bw`) without explicit user direction.
2. Do not mutate buffer text directly over RPC (`nvim_buf_set_lines`, `:%d`) on active user files.
3. Prefer `--remote-expr` over `--remote-send` for verifiable read operations.
4. Filter stdout for `NVIM_APPNAME` warnings on all parsed RPC calls.
5. Notify the user before triggering module reloads in an active editing session.
