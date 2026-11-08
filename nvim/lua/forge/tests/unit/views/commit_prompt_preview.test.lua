-- Unit tests for `forge.views.commit_prompt_preview` (commit_prompt_preview.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["commit_prompt_preview"] = function()
  local deferred = {}
  package.loaded["forge"] = { setup = function() end }
  package.loaded["forge.revisions"] = { setup = function() end, values = function() return {} end }
  local pending = mock_client.record()
  local notices = notify.capture()
  vim.defer_fn = function(callback) deferred[#deferred + 1] = callback end
  local specification = dofile("nvim/lua/plugins/forge.lua")[1]
  assert(vim.tbl_contains(specification.cmd, "ForgeDebugCommitPromptPreview"))
  specification.config(nil, {})
  local response = { state = "ready", diff_pairs = 1, source_requests = 2,
    prompt = { system = "System instructions", user = "Changed files:\n  source.py (+1 -1)\n", input_bytes = 62, limit_bytes = 500000 } }
  vim.b.forge_workspace = "fixture-workspace"
  vim.cmd("ForgeDebugCommitPromptPreview")
  local first = vim.api.nvim_get_current_buf()
  assert(pending[1].spec.method == "repository.generation" and pending[1].spec.params.operation == "preview")
  assert(pending[1].spec.params.workspace == "fixture-workspace" and pending[1].spec.params.comparison == "status")
  assert(pending[1].spec.params.model == nil and pending[1].spec.params.generation == nil)
  assert(buffer.text(first):find("Preparing prompt", 1, true))
  pending[1].callback(response)
  assert(buffer.text(first):find("SYSTEM\n\nSystem instructions\n\nUSER\n\nChanged files:", 1, true))
  assert(not vim.bo[first].modifiable and vim.bo[first].readonly and vim.bo[first].buftype == "nofile")
  assert(vim.b[first].forge_commit_prompt_input.user == response.prompt.user)

  vim.cmd("ForgeDebugCommitPromptPreview staged")
  local second = vim.api.nvim_get_current_buf()
  eq(pending[2].spec.params.comparison, "staged")
  assert(not vim.api.nvim_buf_is_valid(first))
  pending[1].callback(nil, "late failure")
  eq(#notices, 0)
  local ordinary = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_win_set_buf(0, ordinary)
  pending[2].callback(response)
  assert(vim.api.nvim_get_current_buf() == ordinary, "late preview stole the window")

  vim.cmd("ForgeDebugCommitPromptPreview latest_commit")
  local empty = vim.api.nvim_get_current_buf()
  eq(pending[3].spec.params.comparison, "latest_commit")
  pending[3].callback({ state = "none" })
  assert(buffer.text(empty):find("No changes for this comparison.", 1, true) and #notices == 0)

  vim.cmd("ForgeDebugCommitPromptPreview")
  local failed = vim.api.nvim_get_current_buf()
  pending[4].callback(nil, "repository unavailable")
  assert(buffer.text(failed):find("Preview failed: repository unavailable", 1, true))
  eq(notices[1].message, "repository unavailable")

  vim.cmd("ForgeDebugCommitPromptPreview")
  pending[5].callback({ state = "retry" })
  deferred[1]()
  pending[6].callback({ state = "retry" })
  deferred[2]()
  pending[7].callback({ state = "retry" })
  assert(#deferred == 2 and #notices == 2, "superseded preview retries were not bounded")
  local count = #pending
  vim.cmd("ForgeDebugCommitPromptPreview invalid")
  assert(#pending == count and #notices == 3)
end

return T
