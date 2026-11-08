-- Unit tests for `forge.views.harness.workspace` (workspace.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["workspace"] = function()
  local workspace = require("forge.views.harness.workspace")
  local layout = require("forge.views.harness.layout")
  local controller = require("forge.views.harness.controller")
  local state = require("forge.session").harness
  local client = require("forge.client")
  client.session_events_subscribe = function() return function() end end

  vim.keymap.set("n", "gks", "<Cmd>close!<CR>")
  vim.keymap.set("n", "gkb", "<Cmd>bd<CR>")
  local function open()
    state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
      layout.open(tostring(vim.uv.hrtime()))
    state.session = { id = "workspace-test" }
    controller.attach()
  end
  local function intact()
    assert(vim.api.nvim_win_is_valid(state.transcript_win), "timeline window closed")
    assert(vim.api.nvim_win_is_valid(state.composer_win), "input window closed")
    eq(vim.api.nvim_win_get_buf(state.transcript_win), state.transcript_buf)
    eq(vim.api.nvim_win_get_buf(state.composer_win), state.composer_buf)
  end
  ---@return integer[]
  local function foreign_harness_windows()
    local foreign = {}
    for _, window in ipairs(vim.api.nvim_list_wins()) do
      local window_buffer = vim.api.nvim_win_get_buf(window)
      if (window_buffer == state.transcript_buf or window_buffer == state.composer_buf)
        and window ~= state.transcript_win and window ~= state.composer_win then
        foreign[#foreign + 1] = window
      end
    end
    return foreign
  end
  -- A script run with -l never idles in the main loop, so fire the idle event the lock observes.
  local function drain() vim.api.nvim_exec_autocmds("SafeState", {}) end

  for _, pane in ipairs({ "transcript_win", "composer_win" }) do
    open()
    vim.api.nvim_set_current_win(state[pane])
    assert(not vim.bo[state.transcript_buf].modifiable)
    assert(vim.bo[state.composer_buf].modifiable)
    assert(not pcall(vim.cmd, "enew"), "pinned pane allowed replacement")
    for _, keys in ipairs({ "gks", "gkb", ":close<CR>", ":only<CR>", ":bd<CR>" }) do
      keymap.feed(keys)
      intact()
    end
    -- Neovim's :windo splits a 'winfixbuf' window before visiting windows, and netrw runs :windo at VimEnter.
    vim.api.nvim_set_current_win(state[pane])
    vim.cmd("windo echo")
    drain()
    local foreign = foreign_harness_windows()
    assert(#foreign == 0, ":windo left unowned Harness windows " .. vim.inspect(foreign))
    assert(vim.api.nvim_get_current_win() == state[pane], "rejecting the :windo split lost the focused pane")
    intact()
    vim.cmd("new")
    local scratch_window = vim.api.nvim_get_current_win()
    drain()
    assert(vim.api.nvim_win_is_valid(scratch_window), "the pane lock closed a split showing a foreign buffer")
    vim.api.nvim_win_close(scratch_window, true)
    vim.api.nvim_set_current_win(state[pane])
    local preview = buffer.scratch()
    workspace.set_buffer(state.transcript_win, preview)
    assert(vim.wo[state.transcript_win].winfixbuf)
    workspace.set_buffer(state.transcript_win, state.transcript_buf)
    intact()
    local tab = state.timeline_tab
    keymap.feed("q")
    assert(not vim.api.nvim_tabpage_is_valid(tab), "q did not close the complete Harness tab")
  end

  open()
  state.presentation = { close = function() return false end }
  keymap.feed("q")
  intact()
  assert(vim.wo[state.composer_win].winfixbuf, "deferred exit lost protection")
  state.presentation = nil
  workspace.release(state)
  local restored = 0
  vim.keymap.set("n", "gks", function() restored = restored + 1 end, { buffer = state.composer_buf })
  workspace.attach(state)
  workspace.release(state)
  keymap.feed("gks")
  assert(restored == 1, "release did not restore a buffer-local mapping")
  workspace.attach(state)
  keymap.feed("q")
  open()
  vim.cmd("tabonly")
  keymap.feed("q")
  assert(#vim.api.nvim_list_wins() == 1, "exiting the final tab left an input split")
  assert(vim.bo.filetype ~= "ForgeHarnessInput", "exiting the final tab left the input buffer")
end

return T
