local workspace = vim.fn.getcwd()
vim.opt.runtimepath:prepend(workspace .. "/nvim")
require("forge.infra.config").setup({})
local session = require("forge.session")
local client = require("forge.client")
local original_client = client._client
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
  local source_pending = navigation.begin_new("Switch source")
  local source = source_pending.state
  navigation.complete_pending(source_pending, { session = { id = "source-identity-fixture" } })
  local source_tab = source.timeline_tab
  local previous_target_tab = state.timeline_tab
  local tab_count = #vim.api.nvim_list_tabpages()
  navigation.activate({ session = { id = durable_id } }, { state = source, open_mode = "current" })
  assert(session.harness == state and state.timeline_tab == source_tab)
  assert(vim.api.nvim_get_current_tabpage() == source_tab and not vim.api.nvim_tabpage_is_valid(previous_target_tab))
  assert(#vim.api.nvim_list_tabpages() == tab_count - 1)
  assert(vim.api.nvim_win_get_buf(state.composer_win) == state.composer_buf)
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, true)[1] == "Unsent draft")
  assert(source.timeline_tab == nil and source.composer_win == nil)
  navigation.activate({ session = { id = "source-identity-fixture" } }, { state = state, open_mode = "current" })
  assert(session.harness == source and source.timeline_tab == source_tab)
  assert(vim.api.nvim_get_current_tabpage() == source_tab)
  navigation.activate({ session = { id = durable_id } }, { state = source, open_mode = "current" })
  assert(session.harness == state and state.timeline_tab == source_tab)
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, true)[1] == "Unsent draft")
  local retained_transcript, retained_composer = state.transcript_buf, state.composer_buf
  vim.cmd("tabclose")
  assert(not vim.api.nvim_tabpage_is_valid(state.timeline_tab))
  navigation.activate({ session = { id = "source-identity-fixture" } })
  assert(session.harness == source, "source session was not active before resuming")
  client.host_get_generation()
  local transport = client._client
  local replied = false
  transport.ready, transport.runtime_ready = true, true
  transport.provider_registry_generation = transport.generation
  transport.process = { write = function(_, encoded)
    local request = vim.json.decode(encoded)
    assert(request.method == "session.resume" and request.session_id == durable_id)
    local pending = assert(transport.pending[request.id])
    transport.pending[request.id] = nil
    pending.callback({ session = { id = durable_id } }, nil)
    replied = true
  end }
  navigation.resume(durable_id)
  assert(vim.wait(3000, function() return replied end), "resume callback was not delivered")
  assert(session.harness == state, "resume replaced the retained session owner")
  assert(state.transcript_buf == retained_transcript and state.composer_buf == retained_composer,
    "resume replaced retained buffers or collided with their durable names")
  assert(vim.api.nvim_tabpage_is_valid(state.timeline_tab))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, true)[1] == "Unsent draft")
end, debug.traceback)
client._client = original_client
package.loaded["forge.views.harness.controller"] = original_controller
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("Harness session identity passed")
vim.cmd("qa!")
