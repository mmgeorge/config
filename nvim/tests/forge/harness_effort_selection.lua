vim.loader.enable(false)
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local task = require("forge.views.harness.task")
local requests, transition, errors = {}, {}, {}
local header = ""
client.request = function(method, params, callback)
  assert(method == "session.configure", method)
  requests[#requests + 1] = { params = params, callback = callback }
end
client.request_for = function(_, method, params, callback)
  if method == "state.get" then
    callback({ session = state.session, task = state.task })
  else
    assert(method == "task.transition", method)
    transition[#transition + 1] = { params = params, callback = callback }
  end
end
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
require("forge.shared.keymaps").apply_view_winbar = function(_, _, _, _, segments)
  header = table.concat(vim.tbl_map(function(segment) return segment.text end, segments))
end
controller.render, controller.attach_transcript = function() controller.refresh_winbar() end, function() end
state.command_set = controller.command_set()
state.session = { id = "effort-test", backend = "codex", model = "test", resolved_model = "test", effort = "medium" }
state.capability = { effort_selection = true, fast_mode = true }
state.queue, state.pending_steer = {}, {}
local function settled()
  assert(vim.wait(1000, function() return not state.configuration_debounce end, 5))
end
local function acknowledge(index)
  requests[index].callback(vim.tbl_extend("force", state.session, requests[index].params))
end
state.busy = false
controller.change_effort(1)
controller.change_effort(-1)
settled()
assert(#requests == 0 and state.pending_config == nil, "cancelled idle selection reached provider")
controller.change_effort(-1)
settled()
assert(#requests == 1 and requests[1].params.effort == "low")
controller.change_effort(-1)
settled()
assert(#requests == 1, "idle configuration overlapped another request")
acknowledge(1)
assert(vim.wait(1000, function() return #requests == 2 end, 5))
assert(requests[2].params.effort == "minimal")
acknowledge(2)
acknowledge(1)
assert(state.session.effort == "minimal", "late configuration response overwrote current selection")

state.busy = true
state.session.current_task_id = "goal"
state.task = { { id = "goal", kind = "goal", phase = "continue", status = "running" } }
controller.change_effort(1)
controller.change_effort(1)
controller.configure_fast_mode(true)
assert(#transition == 3 and state.task_config.effort == "medium" and state.task_config.fast_mode == true)
assert(header:find("medium* fast*", 1, true), header)
controller.configure_fast_mode(false)
local latest = transition[#transition]
assert(latest.params.config.effort == "medium" and latest.params.config.fast_mode == false)
task.receive(state, { id = transition[1].params.operation_id, state = "superseded" })
assert(state.task_config and state.busy, "old operation cleared the latest desired settings")
latest.callback({ state = "accepted" })
state.session.effort, state.session.fast_mode = "medium", false
state.task[1].status = "paused"
task.receive(state, { id = latest.params.operation_id, state = "completed" })
assert(not state.task_config and not state.busy and state.session.effort == "medium")
assert(not header:find("fast", 1, true))
assert(#errors == 0, table.concat(errors, "\n"))
print("harness_effort_selection: passed")
vim.cmd("qa!")
