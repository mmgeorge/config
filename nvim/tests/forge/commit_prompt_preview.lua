vim.loader.enable(false)
local pending, notices, deferred = {}, {}, {}
package.loaded["forge"] = { setup = function() end }
package.loaded["forge.revisions"] = { setup = function() end, values = function() return {} end }
package.loaded["forge.client"] = { request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  pending[#pending + 1] = { method = method, params = params, callback = callback }
end }
local original_notify, original_defer = vim.notify, vim.defer_fn
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
vim.defer_fn = function(callback) deferred[#deferred + 1] = callback end
local specification = dofile("nvim/lua/plugins/forge.lua")[1]
assert(vim.tbl_contains(specification.cmd, "ForgeDebugCommitPromptPreview"))
specification.config(nil, {})
local function text(buffer) return table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n") end
local response = { state = "ready", diff_pairs = 1, source_requests = 2,
  prompt = { system = "System instructions", user = "Changed files:\n  source.py (+1 -1)\n", input_bytes = 62, limit_bytes = 500000 } }
local success, failure = xpcall(function()
  vim.b.forge_workspace = "fixture-workspace"
  vim.cmd("ForgeDebugCommitPromptPreview")
  local first = vim.api.nvim_get_current_buf()
  assert(pending[1].method == "repository.generation" and pending[1].params.operation == "preview")
  assert(pending[1].params.workspace == "fixture-workspace" and pending[1].params.comparison == "head")
  assert(pending[1].params.model == nil and pending[1].params.generation == nil)
  assert(text(first):find("Preparing prompt", 1, true))
  pending[1].callback(response)
  assert(text(first):find("SYSTEM\n\nSystem instructions\n\nUSER\n\nChanged files:", 1, true))
  assert(not vim.bo[first].modifiable and vim.bo[first].readonly and vim.bo[first].buftype == "nofile")
  assert(vim.b[first].forge_commit_prompt_input.user == response.prompt.user)

  vim.cmd("ForgeDebugCommitPromptPreview staged")
  local second = vim.api.nvim_get_current_buf()
  assert(pending[2].params.comparison == "staged")
  assert(not vim.api.nvim_buf_is_valid(first))
  pending[1].callback(nil, "late failure")
  assert(#notices == 0)
  local ordinary = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_win_set_buf(0, ordinary)
  pending[2].callback(response)
  assert(vim.api.nvim_get_current_buf() == ordinary, "late preview stole the window")

  vim.cmd("ForgeDebugCommitPromptPreview latest_commit")
  local empty = vim.api.nvim_get_current_buf()
  assert(pending[3].params.comparison == "latest_commit")
  pending[3].callback({ state = "none" })
  assert(text(empty):find("No changes for this comparison.", 1, true) and #notices == 0)

  vim.cmd("ForgeDebugCommitPromptPreview")
  local failed = vim.api.nvim_get_current_buf()
  pending[4].callback(nil, "repository unavailable")
  assert(text(failed):find("Preview failed: repository unavailable", 1, true))
  assert(notices[1] == "repository unavailable")

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
end, debug.traceback)
vim.notify, vim.defer_fn = original_notify, original_defer
assert(success, failure)
print("commit prompt command: exact input, scopes, failure, stale response, and retry bounds passed")
