local root = vim.fn.getcwd()
dofile(root .. "/nvim/tests/forge/fixtures/commit_reuse_manual.lua")
local client = require("forge.client")
local ai_commit = require("forge.integrations.ai_commit")
local commit = require("forge.integrations.commit")
local state = forge_reuse.state
local request = ai_commit._backend.request_async
local editor = commit.editor
local held = {}
local generation_calls = 0
local comparison_calls = 0
local hold_regeneration = false
local first_editor_line
ai_commit._backend.request_async = function(params, callback)
  if params.operation == "generate" then generation_calls = generation_calls + 1 end
  if params.operation == "fingerprint" then comparison_calls = comparison_calls + 1 end
  request(params, function(result, failure)
    if hold_regeneration and params.comparison == "staged" then
      held[#held + 1] = function() callback(result, failure) end
    else callback(result, failure) end
  end)
end
commit.editor = function(target, address)
  editor(target, address)
  first_editor_line = vim.api.nvim_buf_get_lines(0, 0, 1, false)[1]
end
local ok, failure = xpcall(function()
  assert(vim.wait(15000, function()
    return state.replica.status == "Applied" and forge_reuse.model_calls == 1
      and not state.request_active and next(client._client.pending) == nil
  end, 10), "Status About did not finish")
  local rendered = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n")
  assert(rendered:find("About:  " .. forge_reuse.message, 1, true), "Status did not display generated About")
  assert(generation_calls == 1, "initial Status generated more than once")
  assert(comparison_calls == 0, "cold About requested an unnecessary comparison")
  local selected
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == "tracked.txt" then selected = file end
  end
  assert(selected, "fixture tracked file missing")
  local _, row = state.replica.sequence:position(string.format("file:%.0f", selected.id))
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  vim.api.nvim_feedkeys("S", "xt", false)
  assert(vim.wait(15000, function()
    if state.request_active or next(client._client.pending) ~= nil then return false end
    local staged, remaining = false, false
    for _, file in ipairs(state.replica.inventory.file) do
      staged = staged or (file.path == "tracked.txt" and file.section == "staged")
      remaining = remaining or file.section == "unstaged"
    end
    return staged and remaining and #(state.replica.inventory.pending or {}) == 0
  end, 10), "partial staging did not settle with unstaged changes remaining")
  assert(generation_calls == 1, "staging generated another About message")
  hold_regeneration = true
  vim.api.nvim_feedkeys("cc", "xt", false)
  assert(vim.wait(15000, function() return first_editor_line ~= nil end, 10), "real commit editor did not open")
  assert(vim.b.forge_commit_buffer and vim.api.nvim_buf_get_name(0):match("COMMIT_EDITMSG$"), "cc did not open the real Git editor")
  assert(first_editor_line == forge_reuse.message, "first editor presentation omitted cached About")
  assert(generation_calls == 1 and comparison_calls == 0 and #held == 0, "opening editor performed generation or validation")
  assert(vim.wo.winbar:find("<C-a>", 1, true) and vim.wo.winbar:find("regenerate staged", 1, true), "regeneration help is missing")
  for _, mode in ipairs({ "n", "i" }) do
    assert(type(vim.fn.maparg("<C-a>", mode, false, true).callback) == "function", "Ctrl-A is missing in " .. mode)
  end
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-a>", true, false, true), "xt", false)
  assert(vim.wait(10000, function() return #held == 1 end, 10), "Ctrl-A did not generate staged context")
  assert(generation_calls == 2 and comparison_calls == 0, "Ctrl-A did not issue exactly one generation")
  assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] == forge_reuse.message, "regeneration cleared the draft while waiting")
  held[1]()
  assert(vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] == "refactor: describe staged fixture", "Ctrl-A did not replace the message")
  local comments = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(comments:find("#", 1, true), "regeneration removed Git comments")
  print("Status -> cc reuses About without requests; Ctrl-A generates staged text and preserves comments")
end, debug.traceback)
commit.editor = editor
if commit._active then
  local abort = vim.fn.maparg("q", "n", false, true)
  if type(abort.callback) == "function" then
    vim.defer_fn(abort.callback, 100)
    assert(vim.wait(10000, function() return commit._active == nil end, 10), "fixture commit did not abort")
  end
end
require("forge.status").close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "fixture host did not stop")
vim.fn.chdir(root)
vim.fn.delete(forge_reuse.fixture, "rf")
assert(ok, failure)
