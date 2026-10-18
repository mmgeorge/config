vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local history = require("forge.views.harness.prompt_history")
local notifications = require("forge.infra.notifications")
local requests, warnings = {}, {}
client.request = function(method, params, callback)
  if method == "backend.model_pins" then callback({}) return end
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
history.record = function() end
notifications.info = function() end
notifications.error = function(message) error(message) end
notifications.warn = function(message) warnings[#warnings + 1] = message end
controller.render = function() end
controller.refresh_winbar = function() end
state.session = { id = "queue-edit", backend = "mock", current_task_id = "task-1" }
state.capability = { model_selection = true, effort_selection = true, native_steer = true }
state.model_list = {}
state.busy = true
state.queue, state.queue_edit, state.pending_steer = {}, nil, {}
state.composer_buf = vim.api.nvim_create_buf(false, true)

local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end

local function composer_text()
  return table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n")
end

local success, failure = xpcall(function()
  local config_metadata = { model = "retained" }
  local prior_entry = { text = "prior prompt", task_id = "task-1" }
  local second_entry = { text = "second original", task_id = "task-1", config = config_metadata }
  local third_entry = { text = "third original", task_id = "task-1" }
  state.queue = { prior_entry, second_entry, third_entry }
  compose("original composer draft")

  controller.edit_last_queued()
  assert(state.queue_edit.entry == third_entry and state.queue_edit.barrier == third_entry
    and #state.queue == 3 and state.queue[1] == prior_entry and state.queue[2] == second_entry
    and state.queue[3] == third_entry, "editing the tail changed queue order or chose the wrong barrier")
  compose("/model rejected impossible prompt")
  controller.edit_last_queued()
  assert(state.queue_edit.entry == second_entry and state.queue_edit.drafts[third_entry] == "/model rejected impossible prompt"
    and state.queue_edit.barrier == second_entry, "moving backward did not retain the third-entry draft")
  compose("second edited")
  controller.submit()
  assert(#warnings == 1 and warnings[1]:find("Queued message 3", 1, true), "invalid later draft was not identified by queue position")
  assert(state.queue_edit.entry == third_entry and state.queue_edit.barrier == second_entry
    and state.queue[1] == prior_entry and state.queue[2] == second_entry and state.queue[3] == third_entry
    and prior_entry.text == "prior prompt" and second_entry.text == "second original"
    and third_entry.text == "third original", "failed validation mutated or reordered the reserved queue")

  state.busy = false
  controller.drain()
  assert(requests[1].method == "prompt.submit" and requests[1].params.text == "prior prompt"
    and #state.queue == 2 and state.queue[1] == second_entry and state.queue[2] == third_entry,
    "the earlier prompt did not drain ahead of the edit barrier")
  state.busy = false
  controller.drain()
  assert(#requests == 1 and #state.queue == 2 and state.queue_edit.barrier == second_entry,
    "drain crossed the earliest unfinished edit after the earlier prompt")

  compose("third corrected")
  state.busy = true
  controller.submit()
  assert(not state.queue_edit and #state.queue == 2 and state.queue[1] == second_entry and state.queue[2] == third_entry
    and second_entry.text == "second edited" and third_entry.text == "third corrected"
    and second_entry.config == config_metadata and composer_text() == "original composer draft"
    and #requests == 1 and #state.pending_steer == 0,
    "commit changed entry identity, metadata, position, steered while busy, or lost the original composer draft")
  state.busy = false
  controller.drain()
  assert(requests[2].method == "prompt.submit" and requests[2].params.text == "second edited"
    and #state.queue == 1 and state.queue[1] == third_entry, "the committed second entry did not drain first")
  state.busy = false
  controller.drain()
  assert(requests[3].method == "prompt.submit" and requests[3].params.text == "third corrected"
    and #state.queue == 0, "the corrected third entry did not drain second")

  local deleted_entry = { text = "delete me", task_id = "task-1" }
  local retained_entry = { text = "retain me", task_id = "task-1" }
  state.queue, state.busy = { deleted_entry, retained_entry }, true
  compose("another composer draft")
  controller.edit_last_queued()
  compose("retained draft")
  controller.edit_last_queued()
  assert(state.queue_edit.entry == deleted_entry and state.queue_edit.drafts[retained_entry] == "retained draft"
    and state.queue[1] == deleted_entry and state.queue[2] == retained_entry,
    "moving from the third entry to the second lost its draft or changed queue order")
  compose("")
  controller.submit()
  assert(not state.queue_edit and #state.queue == 1 and state.queue[1] == retained_entry
    and retained_entry.text == "retained draft" and composer_text() == "another composer draft"
    and #requests == 3, "blank submit did not delete only the selected entry or restore the composer draft")
  state.busy = false
  controller.drain()
  assert(requests[4].method == "prompt.submit" and requests[4].params.text == "retained draft"
    and #state.queue == 0, "deleting the second entry did not release the third entry")
end, debug.traceback)

if state.working_timer then state.working_timer:stop() state.working_timer:close() state.working_timer = nil end
if vim.api.nvim_buf_is_valid(state.composer_buf) then vim.api.nvim_buf_delete(state.composer_buf, { force = true }) end
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_queue_edit: passed")
vim.cmd("qa!")
