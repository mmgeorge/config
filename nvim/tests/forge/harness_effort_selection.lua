vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local keymaps = require("forge.shared.keymaps")
local transition, errors = {}, {}
local header = ""
local header_segments = {}
client.request = function(method, params, callback)
  if method == "backend.models" then
    callback({ { id = "test", reasoning = { "minimal", "low", "medium", "high" } } })
    return
  end
  error("unexpected request: " .. method)
end
client.request_for = function(_, method, params, callback)
  if method == "state.get" then
    callback({ session = state.session, task = state.task, capability = state.capability })
  else
    assert(method == "task.transition", method)
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
state.session = { id = "effort-test", backend = "codex", model = "test", resolved_model = "test", effort = "medium" }
state.capability = { effort_selection = true, fast_mode = true, ultrafast_mode = true }
for _, tier in ipairs({ "default", "fast", "ultrafast" }) do
  state.session.service_tier = tier
  controller.refresh_winbar()
  local marker = ({ fast = "󱐌", ultrafast = "󱐋" })[tier]
  assert(header:find((marker and marker .. " " or "") .. "test medium ·", 1, true), header)
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
for _, owner in ipairs({ "configuring_config", "task_config" }) do
  for _, pending in ipairs({
    { model = "other" }, { effort = "high" }, { service_tier = "fast" },
    { model = "other", effort = "high" }, { model = "other", service_tier = "ultrafast" },
    { effort = "high", service_tier = "fast" },
    { model = "other", effort = "high", service_tier = "ultrafast" },
  }) do
    for _, busy in ipairs({ false, true }) do
      state.session.model, state.session.resolved_model = "test", "test"
      state.session.effort, state.session.service_tier = "medium", "default"
      state[owner], state.busy = pending, busy
      state.configuration_completion = { deferred = busy }
      controller.refresh_winbar()
      local _, count = header:gsub("%*", "")
      assert(count == (busy and 1 or 0), "only deferred properties need a marker: " .. header)
      assert(header:find((pending.model or "test") .. " " .. (pending.effort or "medium") .. (busy and "*" or "") .. " ·", 1, true), header)
      state.session.model = pending.model or "test"
      state.session.resolved_model = state.session.model
      controller.refresh_winbar()
      assert((header:find("*", 1, true) ~= nil) == (busy and (pending.effort ~= nil or pending.service_tier ~= nil)),
        "model confirmation cleared another pending property: " .. header)
      state.session.effort = pending.effort or "medium"
      controller.refresh_winbar()
      assert((header:find("*", 1, true) ~= nil) == (busy and pending.service_tier ~= nil),
        "thinking confirmation cleared pending speed: " .. header)
      state.session.service_tier = pending.service_tier or "default"
      controller.refresh_winbar()
      assert(not header:find("*", 1, true), "confirmed properties retained a marker: " .. header)
      state[owner] = nil
      state.configuration_completion = nil
    end
  end
end
state.busy = false
state.session.model, state.session.resolved_model, state.session.effort = "test", "test", "medium"
state.session.service_tier = nil
state.model_backend, state.model_list = "codex", { { id = "test", reasoning = { "minimal", "low", "medium", "high" }, service_tiers = { "fast", "ultrafast" } } }
state.queue, state.pending_steer = {}, {}
local function settled()
  assert(#transition > 0, "configuration did not reach Rust immediately")
end
local function acknowledge(index)
  local request = transition[index]
  state.session = vim.tbl_extend("force", state.session, request.params.config)
  request.callback({ state = "completed" })
end
state.busy = false
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
press_effort("effort_up", "i")
press_effort("effort_down", "i")
settled()
assert(#transition == 2, "idle settings were withheld by Lua")
acknowledge(1)
acknowledge(2)
transition = {}
press_effort("effort_up", "n")
press_effort("effort_down", "n")
settled()
assert(#transition == 2, "normal-mode settings were withheld by Lua")
acknowledge(1)
acknowledge(2)
transition = {}
press_effort("effort_down", "n")
settled()
assert(#transition == 1 and transition[1].params.config.effort == "low")
assert(header:find("test low", 1, true) and not header:find("*", 1, true), header)
assert(state.configuration_completion.deferred == false, "idle configuration was classified as deferred")
press_effort("effort_down", "i")
settled()
assert(#transition == 2, "new selection did not supersede the pending transition")
assert(transition[2].params.config.effort == "minimal")
assert(state.configuration_completion.deferred == false and not header:find("*", 1, true),
  "superseding idle configuration gained a pending marker")
transition[1].callback({ state = "completed" })
assert(state.task_config.effort == "minimal" and state.configuring,
  "an older completion cleared the latest desired settings")
acknowledge(2)
transition[1].callback({ state = "completed" })
assert(state.session.effort == "minimal", "late configuration response overwrote current selection")
transition = {}

state.busy = true
state.session.current_task_id = "goal"
state.task = { { id = "goal", kind = "goal", phase = "continue", status = "running" } }
controller.change_effort(1)
controller.change_effort(1)
controller.configure_service_tier("fast")
assert(#transition == 3 and state.task_config.effort == "medium" and state.task_config.service_tier == "fast",
  vim.inspect({ transition = transition, config = state.task_config }))
assert(state.configuration_completion.deferred == true and state.task_operation.configuration_deferred == true)
assert(header:find("󱐌 test medium*", 1, true), header)
local _, markers = header:gsub("%*", "")
assert(markers == 1, "model properties produced separate pending markers: " .. header)
controller.toggle_service_tier("ultrafast")
assert(state.task_config.service_tier == "ultrafast" and header:find("󱐋 test medium*", 1, true), header)
controller.toggle_service_tier("fast")
assert(state.task_config.service_tier == "fast", "fast did not replace ultrafast")
controller.toggle_service_tier("ultrafast")
controller.toggle_service_tier("ultrafast")
local latest = transition[#transition]
assert(state.task_config.effort == "medium" and latest.params.config.service_tier == "default")
assert(header:find("test medium*", 1, true) and not header:find("standard", 1, true), header)
task.receive(state, { id = transition[1].params.operation_id, state = "superseded" })
assert(state.task_config and state.busy, "old operation cleared the latest desired settings")
latest.callback({ state = "accepted" })
for index = 2, #transition - 1 do
  task.receive(state, { id = transition[index].params.operation_id, state = "completed" })
end
state.session.effort, state.session.service_tier = "medium", "default"
state.task[1].status = "paused"
task.receive(state, { id = latest.params.operation_id, state = "completed" })
assert(not state.task_config and not state.busy and state.session.effort == "medium")
assert(not header:find("fast", 1, true))
assert(not header:find("*", 1, true), "applied model properties retained a pending marker")
state.model_list[#state.model_list + 1] = { id = "other-model", service_tiers = { "fast", "ultrafast" } }
state.task_config = { model = "other-model", effort = "high", service_tier = "fast" }
controller.refresh_winbar()
assert(header:find("󱐌 other-model high", 1, true), header)
local _, idle_markers = header:gsub("%*", "")
assert(idle_markers == 0, "idle model changes flashed a pending marker: " .. header)
state.busy = true
state.task_config.service_tier = "ultrafast"
controller.refresh_winbar()
assert(header:find("󱐋 other-model high", 1, true), header)
local _, paused_markers = header:gsub("%*", "")
assert(paused_markers == 0, "paused model changes flashed a pending marker: " .. header)
state.busy, state.task_config = false, nil
local count = #transition
state.capability.ultrafast_mode = false
controller.toggle_service_tier("ultrafast")
assert(state.configuration_error:find("does not support ultrafast", 1, true))
assert(#transition == count and not state.task_config)
assert(#errors == 0, table.concat(errors, "\n"))
local applied
controller.configure({ effort = "unavailable" }, true, function(value) applied = value end)
transition[#transition].callback({ state = "failed", error = "reasoning effort unavailable" })
assert(applied == false and #errors == 1 and errors[1] == "reasoning effort unavailable")
assert(state.session.effort == "medium" and not state.task_config and not state.configuring,
  "failed configuration replaced the applied setting or retained pending state")
state.capability.ultrafast_mode = true
state.model_list = { { id = "test", service_tiers = { "fast" } } }
state.session.service_tier = "default"
local before_invalid = #transition
controller.configure_service_tier("ultrafast")
assert(#transition == before_invalid and state.session.service_tier == "default")
assert(errors[#errors]:find("does not support ultrafast", 1, true))
state.session.service_tier = "ultrafast"
controller.refresh_winbar()
assert(header:find("󱐋 test medium", 1, true) and not header:find("unsupported ultrafast", 1, true), header)
vim.api.nvim_buf_delete(composer, { force = true })
print("harness_effort_selection: passed")
vim.cmd("qa!")
