vim.loader.enable(false)
local state = require("forge.session").harness
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
state.session = { id = "rollback-choice" }
state.busy, state.no_checkpoint = false, true
local history = {
  { id = "first", ordinal = 1, prompt = "first prompt", state = "complete", disposition = "current" },
  { id = "second", ordinal = 2, prompt = "second prompt", state = "complete", disposition = "current" },
}
local recovery = { pending = false }
local requests, shown = {}, nil
client.request_for = function(_, method, params, callback)
  requests[#requests + 1] = { method = method, params = params }
  if method == "exchange.recovery" and not params.action then callback(recovery)
  elseif method == "exchange.list" then callback(history) end
end
require("forge.views.picker").open = function(options) shown = options end
local function choose(value)
  for _, option in ipairs(shown.page_list[1].option_list) do
    if option.value == value then shown.on_confirm({ option = option }) return end
  end
  error("missing rollback option: " .. tostring(value))
end
controller.open_undo_picker()
assert(shown.page_list[1].title == "Select Exchange", "completed restores must not add an initial prompt")
assert(shown.page_list[1].option_list[1].value == history[2], "history must list newest first")
choose(history[2])
assert(shown.page_list[1].title == "Rollback interaction")
assert(#shown.page_list[1].option_list == 2, "without a checkpoint only cancel and conversation are available")
local count = #requests
choose("cancel")
assert(#requests == count, "cancelling sent a rollback request")
controller.open_undo_picker()
choose(history[2])
choose("conversation")
assert(requests[#requests].method == "task.transition", "conversation rollback must collect active execution")
assert(requests[#requests].params.action == "undo")
assert(requests[#requests].params.rollback.scope == "conversation")
assert(requests[#requests].params.rollback.exchange_id == "second"
  and requests[#requests].params.rollback.latest_exchange_id == "second")
state.restore_applying = nil
state.busy, state.task_operation = false, nil
state.no_checkpoint = false
history[2].checkpoint_before = "checkpoint"
controller.open_undo_picker()
choose(history[2])
assert(#shown.page_list[1].option_list == 3, "checkpointed interactions must offer both rollback scopes")
choose("workspace")
assert(requests[#requests].method == "exchange.rollback.prepare", "file rollback must retain its restore preview")
recovery = { pending = true }
controller.open_undo_picker()
assert(shown.page_list[1].title == "Harness restore", "unfinished file restoration must require recovery")
print("harness_rollback_choice: passed")
