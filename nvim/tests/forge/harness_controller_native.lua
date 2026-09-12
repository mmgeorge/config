vim.loader.enable(false)
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local original_request, original_subscribe, original_generation = client.request_for, client.subscribe, client.host_generation
local original_accepting = client.host_accepting
client.host_accepting = function() return true end
local state = session.harness
local generation, submitted, receive, delayed_edit, navigated, sync_count = 1, nil, nil, nil, nil, 0
local initial = {}
local function snapshot(document, text, composer)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = composer and { { id = "composer", revision = 0,
      range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } or {} } } } }
end
client.host_generation = function() return generation end
client.subscribe = function(callback) receive = callback return function() end end
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "native-controller")
  if method == "prompt.submit" then submitted = params return end
  if method == "history.record" then vim.schedule(function() callback({}) end) return end
  assert(method == "harness.document")
  if params.operation == "open" then
    initial[#initial + 1] = params.initial[1]
    vim.schedule(function() callback({ transcript = snapshot(params.document, "Native transcript"), composer = snapshot(params.composer, params.initial[1], true) }) end)
  elseif params.operation == "edit_composer" then delayed_edit = callback
  elseif params.operation == "navigate_prompt" then
    navigated = params.input
    vim.schedule(function() callback({ anchor = { block = "body", position = { row = 0, column = 0 } } }) end)
  elseif params.operation == "sync" then
    sync_count = sync_count + 1
    vim.schedule(function() callback({ patch = {} }) end)
  else vim.schedule(function() callback({ patch = {} }) end) end
end
local success, failure = xpcall(function()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab = require("forge.views.harness.layout").open("native-controller")
  state.session = { id = "native-controller", backend = "mock", model = "mock", execution_mode = "read" }
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "native draft" })
  controller.attach()
  controller.render()
  assert(vim.wait(1000, function() return state.presentation and state.presentation.ready end))
  assert(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)[1] == "Native transcript")
  local original_window = state.transcript_win
  vim.api.nvim_set_current_win(original_window)
  vim.cmd("vsplit")
  local secondary = vim.api.nvim_get_current_win()
  state.presentation.refresh_views()
  assert(vim.wait(1000, function() return state.presentation.views[secondary] ~= nil end))
  vim.api.nvim_win_set_cursor(original_window, { 1, 3 })
  vim.api.nvim_win_set_cursor(secondary, { 1, 5 })
  controller.jump_prompt(1)
  assert(vim.wait(1000, function() return navigated ~= nil and vim.api.nvim_win_get_cursor(secondary)[2] == 0 end))
  assert(navigated.view == state.presentation.views[secondary].id)
  assert(vim.api.nvim_win_get_cursor(original_window)[2] == 3, "secondary navigation moved the initiating window")
  vim.api.nvim_win_close(original_window, true)
  assert(vim.wait(1000, function() return state.presentation.views[original_window] == nil end))
  assert(not state.presentation.closed and state.presentation.views[secondary])
  local sync_before_submission = sync_count
  controller.submit()
  assert(submitted and submitted.composer and not submitted.text)
  assert(vim.wait(2500, function() return sync_count > sync_before_submission end, 20),
    "busy Harness did not refresh the native transcript status")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "native draft", "controller cleared before native admission")
  local metadata = snapshot(state.presentation.composer_id, "", true).block[1].metadata
  metadata.editable_region[1].revision = 1
  receive("backend_event", { kind = "composer_patch", data = { document = state.presentation.composer_id, base = 0, next = 1,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 0, removed_rows = 1, text = { "" } } },
    metadata_edit = { { block = "body", row_count = 1, metadata = metadata } },
  } }, "native-controller")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "draft after host collection" })
  require("forge.editable").flush(state.presentation.composer.editable)
  assert(delayed_edit)
  generation = 2
  controller.render()
  assert(vim.wait(1000, function() return state.presentation and state.presentation.ready and state.presentation.host_generation == 2 end))
  delayed_edit({ accepted = true })
  assert(initial[2] == "draft after host collection")
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "draft after host collection")
  state.presentation.close()
end, debug.traceback)
if state.working_timer then state.working_timer:stop() state.working_timer:close() state.working_timer = nil end
client.request_for, client.subscribe, client.host_generation = original_request, original_subscribe, original_generation
client.host_accepting = original_accepting
assert(success, failure)
print("harness_controller_native: passed")
