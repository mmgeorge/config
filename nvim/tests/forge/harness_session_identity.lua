local workspace = vim.fn.getcwd()
vim.opt.runtimepath:prepend(workspace .. "/nvim")
require("forge.infra.config").setup({})
local session = require("forge.session")
local original_controller = package.loaded["forge.views.harness.controller"]
package.loaded["forge.views.harness.controller"] = {
  activate_snapshot = function(result) session.harness.session = result.session end,
  attach = function() end,
  attach_transcript = function() end,
  refresh_winbar = function() end,
}
local navigation = require("forge.views.harness.session_navigation")

local succeeded, failure = xpcall(function()
  local pending = navigation.begin_new("Identity fixture")
  local state = pending.state
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Unsent draft" })
  local durable_id = "durable-identity-fixture"
  navigation.complete_pending(pending, { session = { id = durable_id } })
  local settings = require("forge.infra.config").options.harness
  assert(vim.api.nvim_buf_get_name(state.transcript_buf) == settings.buffer_name .. "://" .. durable_id)
  assert(vim.api.nvim_buf_get_name(state.composer_buf) == settings.composer_name .. "://" .. durable_id,
    "composer retained temporary session identity")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, true)[1] == "Unsent draft")
  assert(session.harness_by_id[durable_id] == state)
  assert(vim.api.nvim_get_current_buf() == state.composer_buf)
end, debug.traceback)
package.loaded["forge.views.harness.controller"] = original_controller
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("Harness session identity passed")
vim.cmd("qa!")
