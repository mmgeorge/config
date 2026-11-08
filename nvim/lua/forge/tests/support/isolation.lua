-- Per-case isolation for mini.test files whose cases share one Neovim. `.forge/checks/lua-tests.nu`
-- runs each unit test file as one shard, so a stub, option, buffer, or module singleton left behind
-- by one case would change every later case in the file. `isolation.hooks()` returns `pre_case` and
-- `post_case` hooks that capture the editor before each case and return it to that state afterward,
-- whether or not the case failed:
--
--   local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
--   local T = MiniTest.new_set({ hooks = isolation.hooks({
--     post_case = function() require("forge.lsp.startup").close() end,
--   }) })
--
-- The caller's `post_case` runs first, while the case's stubs and modules are still in place, so it
-- can stop the work the case started, and the case's pending `vim.schedule` callbacks run out
-- before anything is restored. Isolation then deletes the case's autocommands and timers,
-- restores replaced `vim.*` functions, global options, global variables, Lua globals, and the
-- working directory, closes the case's tabpages and windows, wipes its buffers, restores global
-- keymaps, user commands, and the starting window's local options, and unloads every
-- plugin module, so the next case requires fresh singletons such as `forge.client`. Cases must
-- therefore require plugin modules inside the case or a hook, never at file scope.
local isolation = {}

-- The `vim.*` tables whose functions tests replace. Indexing the lazily loaded ones here loads them
-- before the capture, so a replacement made during a case is always detected.
local function stubbable_tables()
  return {
    vim = vim, api = vim.api, fn = vim.fn, uv = vim.uv, ui = vim.ui, fs = vim.fs, json = vim.json,
    lsp = vim.lsp, lsp_buf = vim.lsp.buf, lsp_rpc = vim.lsp.rpc, lsp_util = vim.lsp.util,
    diagnostic = vim.diagnostic, filetype = vim.filetype, treesitter = vim.treesitter,
  }
end

local restored_options = {
  "columns", "lines", "virtualedit", "guicursor", "timeout", "timeoutlen", "cmdheight", "backspace",
}

-- Options local to the window a case starts in, which outlives the case and so carries settings
-- into the next one.
local restored_window_options = {
  "number", "relativenumber", "statuscolumn", "signcolumn", "foldcolumn", "winbar", "winhighlight",
  "conceallevel", "concealcursor", "wrap", "linebreak", "list", "cursorline", "spell", "scrolloff",
}

local keymap_modes = { "n", "i", "x", "s", "o", "c", "t" }

local function global_keymaps()
  local mappings = {}
  for _, mode in ipairs(keymap_modes) do
    for _, listed in ipairs(vim.api.nvim_get_keymap(mode)) do
      mappings[mode .. "\0" .. listed.lhs] = vim.fn.maparg(listed.lhs, mode, false, true)
    end
  end
  return mappings
end

-- Lua modules owned by this repository's plugins. Unloading them gives each case new module state.
local function is_plugin_module(name)
  return name == "forge" or name:find("^forge%.") ~= nil
    or name == "github" or name:find("^github%.") ~= nil
    or name == "git" or name:find("^git%.") ~= nil
    or name == "rust_sidecar" or name:find("^rust_sidecar%.") ~= nil
end

local function function_fields(target)
  local fields = {}
  for key, value in pairs(target) do
    if type(value) == "function" then fields[key] = value end
  end
  return fields
end

local function key_set(list)
  local set = {}
  for _, key in ipairs(list) do set[key] = true end
  return set
end

local function shallow_copy(source)
  local copy = {}
  for key, value in pairs(source) do copy[key] = value end
  return copy
end

local function capture()
  local tables = stubbable_tables()
  local functions = {}
  for name, target in pairs(tables) do functions[name] = function_fields(target) end
  local options = {}
  for _, option in ipairs(restored_options) do options[option] = vim.go[option] end
  local timers = {}
  vim.uv.walk(function(handle) if handle:get_type() == "timer" then timers[handle] = true end end)
  local autocmds = {}
  for _, autocmd in ipairs(vim.api.nvim_get_autocmds({})) do
    if autocmd.id then autocmds[autocmd.id] = true end
  end
  local current_window = vim.api.nvim_get_current_win()
  local window_options = {}
  for _, option in ipairs(restored_window_options) do window_options[option] = vim.wo[current_window][option] end
  return {
    window_options = window_options,
    keymaps = global_keymaps(),
    commands = vim.api.nvim_get_commands({}),
    tables = tables,
    functions = functions,
    options = options,
    globals = shallow_copy(_G),
    variables = vim.fn.eval("g:"),
    cwd = vim.fn.getcwd(),
    timers = timers,
    autocmds = autocmds,
    tabpages = key_set(vim.api.nvim_list_tabpages()),
    windows = key_set(vim.api.nvim_list_wins()),
    buffers = key_set(vim.api.nvim_list_bufs()),
    current_window = current_window,
    current_buffer = vim.api.nvim_win_get_buf(current_window),
    modules = shallow_copy(package.loaded),
  }
end

local function restore_functions(snapshot)
  for name, target in pairs(snapshot.tables) do
    local original = snapshot.functions[name]
    for key, value in pairs(function_fields(target)) do
      if original[key] == nil then
        rawset(target, key, nil)
      elseif original[key] ~= value then
        target[key] = original[key]
      end
    end
    for key, value in pairs(original) do
      if rawget(target, key) == nil and target[key] ~= value then target[key] = value end
    end
  end
end

local function restore_variables(snapshot)
  for name in pairs(vim.fn.eval("g:")) do
    if snapshot.variables[name] == nil then vim.g[name] = nil end
  end
  for name, value in pairs(snapshot.variables) do
    if not vim.deep_equal(vim.g[name], value) then vim.g[name] = value end
  end
end

local function restore_globals(snapshot)
  for name in pairs(shallow_copy(_G)) do
    if snapshot.globals[name] == nil then _G[name] = nil end
  end
  for name, value in pairs(snapshot.globals) do
    if rawget(_G, name) ~= value then rawset(_G, name, value) end
  end
end

local function close_layout(snapshot)
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    if not snapshot.tabpages[tabpage] and #vim.api.nvim_list_tabpages() > 1 then
      vim.api.nvim_set_current_tabpage(tabpage)
      vim.cmd("tabclose!")
    end
  end
  if vim.api.nvim_win_is_valid(snapshot.current_window) then
    vim.api.nvim_set_current_win(snapshot.current_window)
  end
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    if not snapshot.windows[window] then pcall(vim.api.nvim_win_close, window, true) end
  end
  if vim.api.nvim_win_is_valid(snapshot.current_window) and vim.api.nvim_buf_is_valid(snapshot.current_buffer) then
    vim.api.nvim_win_set_buf(snapshot.current_window, snapshot.current_buffer)
  end
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if not snapshot.buffers[buffer] then vim.api.nvim_buf_delete(buffer, { force = true }) end
  end
end

local function restore_modules(snapshot)
  for name in pairs(shallow_copy(package.loaded)) do
    if snapshot.modules[name] == nil or is_plugin_module(name) then package.loaded[name] = nil end
  end
  for name, module in pairs(snapshot.modules) do
    if not is_plugin_module(name) and package.loaded[name] ~= module then package.loaded[name] = module end
  end
end

-- Runs the callbacks the case left on the `vim.schedule` queue while its stubs and modules are
-- still in place, so they cannot fire in the next case against that case's state. The queue is
-- first in, first out, so every callback queued before the sentinel runs before it does. The
-- second round runs work those callbacks queued in turn.
local drain_rounds = 2

local function drain_scheduled_work()
  for _ = 1, drain_rounds do
    local reached = false
    vim.schedule(function() reached = true end)
    vim.wait(1000, function() return reached end, 1)
  end
end

local function restore(snapshot)
  local failures = {}
  local function step(label, action)
    local ok, failure = pcall(action)
    if not ok then failures[#failures + 1] = ("%s: %s"):format(label, failure) end
  end
  step("autocommands", function()
    for _, autocmd in ipairs(vim.api.nvim_get_autocmds({})) do
      if autocmd.id and not snapshot.autocmds[autocmd.id] then pcall(vim.api.nvim_del_autocmd, autocmd.id) end
    end
  end)
  step("timers", function()
    vim.uv.walk(function(handle)
      if handle:get_type() == "timer" and not snapshot.timers[handle] and not handle:is_closing() then
        handle:stop()
        handle:close()
      end
    end)
  end)
  step("functions", function() restore_functions(snapshot) end)
  step("options", function()
    for option, value in pairs(snapshot.options) do
      if vim.go[option] ~= value then vim.go[option] = value end
    end
  end)
  step("variables", function() restore_variables(snapshot) end)
  step("globals", function() restore_globals(snapshot) end)
  step("cwd", function()
    if vim.fn.getcwd() ~= snapshot.cwd then vim.fn.chdir(snapshot.cwd) end
  end)
  step("layout", function() close_layout(snapshot) end)
  step("keymaps", function()
    for key, mapping in pairs(global_keymaps()) do
      if snapshot.keymaps[key] == nil then pcall(vim.keymap.del, key:sub(1, 1), mapping.lhs) end
    end
    for key, mapping in pairs(snapshot.keymaps) do
      local current = vim.fn.maparg(mapping.lhs, key:sub(1, 1), false, true)
      if current.rhs ~= mapping.rhs or current.callback ~= mapping.callback then
        vim.fn.mapset(key:sub(1, 1), false, mapping)
      end
    end
  end)
  step("commands", function()
    for name in pairs(vim.api.nvim_get_commands({})) do
      if snapshot.commands[name] == nil then pcall(vim.api.nvim_del_user_command, name) end
    end
  end)
  step("window options", function()
    if vim.api.nvim_win_is_valid(snapshot.current_window) then
      for option, value in pairs(snapshot.window_options) do
        if vim.wo[snapshot.current_window][option] ~= value then vim.wo[snapshot.current_window][option] = value end
      end
    end
  end)
  step("modules", function() restore_modules(snapshot) end)
  if #failures > 0 then error("isolation could not restore the editor:\n" .. table.concat(failures, "\n"), 0) end
end

-- Hooks for `MiniTest.new_set({ hooks = ... })`. `options.pre_case` runs after the capture and
-- `options.post_case` before the restore; both are optional.
function isolation.hooks(options)
  options = options or {}
  local snapshot
  return {
    pre_case = function()
      snapshot = capture()
      if options.pre_case then options.pre_case() end
    end,
    post_case = function()
      local ok, failure = true, nil
      if options.post_case then ok, failure = pcall(options.post_case) end
      pcall(drain_scheduled_work)
      if snapshot then restore(snapshot) end
      snapshot = nil
      if not ok then error(failure, 0) end
    end,
  }
end

return isolation
