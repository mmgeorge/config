vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local keymaps = require("forge.shared.keymaps")
local transition, errors = {}, {}
local header = ""
local header_segments = {}
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  if method == "session.provider.model.list" then
    callback({ { id = "test", reasoning = { "minimal", "low", "medium", "high" } } })
    return
  end
  error("unexpected request: " .. method)
end
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  if method == "session.state.get" then
    callback({ session = state.session, capability = state.capability })
  else
    assert(method == "session.configure", method)
    transition[#transition + 1] = { params = params, callback = callback }
  end
end
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
require("forge.shared.keymaps").apply_view_winbar = function(_, _, _, _, segments)
  header_segments = segments
  header = table.concat(vim.tbl_map(function(segment) return segment.text end, segments))
end
controller.render, controller.attach_transcript = function() controller.refresh_winbar() end, function() end
state.command_set = controller.command_set()
state.transcript_win = vim.api.nvim_get_current_win()
state.session = { id = "effort-test", provider = "fixture_alpha", model = "test", resolved_model = "test", effort = "medium" }
state.capability = { effort_selection = true, fast_mode = true, ultrafast_mode = true }
for _, tier in ipairs({ "default", "fast", "ultrafast" }) do
  state.session.service_tier = tier
  controller.refresh_winbar()
  local marker = ({ fast = "󱐌", ultrafast = "󱐋" })[tier]
  assert(header:find((marker and marker .. " " or "") .. "test medium", 1, true), header)
  for _, segment in ipairs(header_segments) do
    if segment.text == marker then
      assert(segment.group == (tier == "ultrafast" and "ForgeHarnessUltrafast" or "ForgeStatusLabel"))
    end
  end
  assert(not header:find("+", 1, true), "legacy speed marker remains")
  assert((header:find("󱐌", 1, true) ~= nil) == (tier == "fast"))
  assert((header:find("󱐋", 1, true) ~= nil) == (tier == "ultrafast"))
  assert(not header:find("unverified", 1, true) and not header:find("unsupported", 1, true), header)
end
-- Settings changes go straight to the host through `session.configure`; the header shows only
-- the host-applied settings, so it changes when the session publishes the new values.
dofile("nvim/tests/forge/support/activity.lua").set_busy(state, false)
state.session.model, state.session.resolved_model, state.session.effort = "test", "test", "medium"
state.session.service_tier = nil
state.model_provider, state.model_list = "fixture_alpha", { { id = "test", reasoning = { "minimal", "low", "medium", "high" }, service_tiers = { "fast", "ultrafast" } } }
local function acknowledge(index)
  local request = transition[index]
  state.session = vim.tbl_extend("force", state.session, request.params)
  request.callback({})
end
local composer = vim.api.nvim_create_buf(false, true)
keymaps.setup_view_keymaps(composer, "harness", state.command_set)
local function press_effort(command_id, mode)
  vim.api.nvim_buf_call(composer, function()
    local key = keymaps.view_keys_for("harness", command_id)[1]
    local mapping = vim.fn.maparg(key, mode, false, true)
    assert(mapping.buffer == 1 and type(mapping.callback) == "function",
      command_id .. " must be mapped in " .. mode .. " mode")
    mapping.callback()
  end)
end
for _, mode in ipairs({ "i", "n" }) do
  press_effort("effort_up", mode)
  assert(#transition == 1 and transition[1].params.effort == "high", vim.inspect(transition))
  acknowledge(1)
  press_effort("effort_down", mode)
  assert(#transition == 2 and transition[2].params.effort == "medium", vim.inspect(transition))
  acknowledge(2)
  transition = {}
end
press_effort("effort_down", "n")
assert(#transition == 1 and transition[1].params.effort == "low")
assert(header:find("test medium", 1, true), "header showed an unapplied effort: " .. header)
acknowledge(1)
controller.refresh_winbar()
assert(header:find("test low", 1, true) and not header:find("*", 1, true), header)
transition = {}

controller.toggle_service_tier("fast")
assert(#transition == 1 and transition[1].params.service_tier == "fast", vim.inspect(transition))
acknowledge(1)
controller.refresh_winbar()
assert(header:find("󱐌 test low", 1, true), header)
controller.toggle_service_tier("fast")
assert(#transition == 2 and transition[2].params.service_tier == "default", "toggling the active tier must restore default")
acknowledge(2)
transition = {}

local count = #transition
state.capability.ultrafast_mode = false
controller.toggle_service_tier("ultrafast")
assert(state.configuration_error:find("does not support ultrafast", 1, true))
assert(#transition == count, "an unsupported tier reached the host")
assert(#errors == 0, table.concat(errors, "\n"))
local applied
controller.configure({ effort = "unavailable" }, function(value) applied = value end)
transition[#transition].callback(nil, "reasoning effort unavailable")
assert(applied == false and state.session.effort == "low", "failed configuration replaced the applied setting")
assert(state.configuration_error, "failed configuration was not reported")
state.capability.ultrafast_mode = true
state.model_list = { { id = "test", service_tiers = { "fast" } } }
state.session.service_tier = "default"
local before_invalid = #transition
controller.configure_service_tier("ultrafast")
assert(#transition == before_invalid and state.session.service_tier == "default")
assert(errors[#errors]:find("does not support ultrafast", 1, true))
state.session.service_tier = "ultrafast"
controller.refresh_winbar()
assert(header:find("󱐋 test low", 1, true) and not header:find("unsupported ultrafast", 1, true), header)
vim.api.nvim_buf_delete(composer, { force = true })
print("harness_effort_selection: passed")
vim.cmd("qa!")
