vim.opt.runtimepath:prepend("nvim")

local workspace = require("forge.views.harness.workspace")
local layout = require("forge.views.harness.layout")
local controller = require("forge.views.harness.controller")
local state = require("forge.session").harness
local client = require("forge.client")
client.subscribe = function() return function() end end

vim.keymap.set("n", "gks", "<Cmd>close!<CR>")
vim.keymap.set("n", "gkb", "<Cmd>bd<CR>")
local function input(keys)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), "xt", false)
end
local function open()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    layout.open(tostring(vim.uv.hrtime()))
  state.session = { id = "workspace-test" }
  controller.attach()
end
local function intact()
  assert(vim.api.nvim_win_is_valid(state.transcript_win), "timeline window closed")
  assert(vim.api.nvim_win_is_valid(state.composer_win), "input window closed")
  assert(vim.api.nvim_win_get_buf(state.transcript_win) == state.transcript_buf)
  assert(vim.api.nvim_win_get_buf(state.composer_win) == state.composer_buf)
end

for _, pane in ipairs({ "transcript_win", "composer_win" }) do
  open()
  vim.api.nvim_set_current_win(state[pane])
  assert(not vim.bo[state.transcript_buf].modifiable)
  assert(vim.bo[state.composer_buf].modifiable)
  assert(not pcall(vim.cmd, "enew"), "pinned pane allowed replacement")
  for _, keys in ipairs({ "gks", "gkb", ":close<CR>", ":only<CR>", ":bd<CR>" }) do
    input(keys)
    intact()
  end
  local preview = vim.api.nvim_create_buf(false, true)
  workspace.set_buffer(state.transcript_win, preview)
  assert(vim.wo[state.transcript_win].winfixbuf)
  workspace.set_buffer(state.transcript_win, state.transcript_buf)
  intact()
  local tab = state.timeline_tab
  input("q")
  assert(not vim.api.nvim_tabpage_is_valid(tab), "q did not close the complete Harness tab")
end

open()
state.presentation = { close = function() return false end }
input("q")
intact()
assert(vim.wo[state.composer_win].winfixbuf, "deferred exit lost protection")
state.presentation = nil
workspace.release(state)
local restored = 0
vim.keymap.set("n", "gks", function() restored = restored + 1 end, { buffer = state.composer_buf })
workspace.attach(state)
workspace.release(state)
input("gks")
assert(restored == 1, "release did not restore a buffer-local mapping")
workspace.attach(state)
input("q")
open()
vim.cmd("tabonly")
input("q")
assert(#vim.api.nvim_list_wins() == 1, "exiting the final tab left an input split")
assert(vim.bo.filetype ~= "ForgeHarnessInput", "exiting the final tab left the input buffer")
print("harness_workspace: passed")
vim.cmd("qa!")
