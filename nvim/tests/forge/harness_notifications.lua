vim.opt.runtimepath:prepend("nvim")

local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local notifications = require("forge.infra.notifications")
local state = require("forge.session").harness

local notice_list = {}
notifications.info = function(message) notice_list[#notice_list + 1] = { "info", message } end
notifications.warn = function(message) notice_list[#notice_list + 1] = { "warn", message } end
notifications.error = function(message) notice_list[#notice_list + 1] = { "error", message } end

state.session = { id = "notification-test", execution_mode = "read" }
state.busy = false
state.no_checkpoint = false
state.command_set = nil
client.request = function(method, params, callback)
  if method == "session.execution_mode" then
    callback({ id = state.session.id, mode = params.mode, execution_mode = params.mode })
  elseif method == "session.mode" then
    callback(nil, "mode rejected")
  else
    error("unexpected request: " .. method)
  end
end

controller.toggle_mode()
assert(state.session.execution_mode == "write", "execution mode should change")
assert(#notice_list == 0, "routine execution mode changes should not notify")

controller.toggle_mode()
assert(state.session.mode == "read", "Shift-Tab should return from Write to Read")
for _, mode in ipairs({ "full", "yolo", "plan" }) do
  state.session.mode = mode
  state.session.execution_mode = mode == "plan" and "write" or mode
  controller.toggle_mode()
  assert(state.session.mode == "read", "Shift-Tab should select Read from " .. mode)
end
assert(#notice_list == 0, "routine mode shortcuts should not notify")

local picker = require("forge.views.picker")
local mode_spec
picker.open = function(spec) mode_spec = spec end
state.transcript_win = vim.api.nvim_get_current_win()
state.composer_win = state.transcript_win
controller.select_mode()
local mode_options = mode_spec.page_list[1].option_list
assert(#mode_options == 5, "the /mode picker should retain all modes")
for _, option in ipairs(mode_options) do
  assert(option.highlight_group == require("forge.infra.highlights").harness_mode(option.value),
    "mode option should use the same color as the Harness status")
end

controller.set_mode("plan")
assert(#notice_list == 1 and notice_list[1][1] == "error"
  and notice_list[1][2]:find("mode rejected", 1, true),
  "failed mode changes should notify with the error")

print("harness_notifications: passed")
vim.cmd("qa!")
