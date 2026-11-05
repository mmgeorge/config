vim.loader.enable(false)
local replica = require("forge.replica").new({ count = 0 }, 0, function(value, intent) value.count = value.count + intent.amount end)
replica.begin("first", { amount = 1 })
replica.begin("second", { amount = 2 })
assert(replica.view().count == 3)
replica.remove("first")
assert(replica.view().count == 2, "rejecting one operation restored an old application snapshot")
assert(not replica.patch(1, 2, function(value) value.count = 10 end))
assert(not replica.patch(0, 1, function(value) value.count = 10 end), "a delta advanced a replica awaiting recovery")
assert(replica.view().count == 2)
replica.replace({ count = 5 }, 2)
assert(replica.view().count == 7 and not replica.needs_snapshot())
local exposed = replica.view()
exposed.count = 100
assert(replica.view().count == 7, "view mutation changed authoritative state")

require("forge").setup({ walkthrough_inventory = "sem", harness = { provider = "mock" } })
local view = require("forge.views.harness.plan_question")
local pending, continued = {}, 0
local initial = { revision = 1, answer_revision = 0, answer = {}, current_index = 0,
  question_set = { id = "delayed", questions = {
    { id = "first", header = "First", question = "First?", options = { { label = "Yes", description = "First choice" } }, allow_freeform = true },
    { id = "second", header = "Second", question = "Second?", options = { { label = "Yes", description = "Second choice" } }, allow_freeform = true },
  } } }
local host = { transcript_win = vim.api.nvim_get_current_win(),
  answer = function(params, callback) pending[#pending + 1] = { params = params, callback = callback } end,
  continue = function() continued = continued + 1 end }
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback, "missing picker mapping " .. key)
  mapping.callback()
end
view.open(initial, host)
invoke("<CR>")
assert(#pending == 1 and view._state_for_test().state.page_index == 2, "answer waited for its acknowledgement before advancing")
invoke("<CR>")
assert(#pending == 2)
invoke("y")
assert(not view.is_open() and continued == 0, "continuation ran before answers committed")
local first = vim.deepcopy(initial)
first.answer_revision, first.current_index = 1, 1
first.answer = { { question_id = "first", response = pending[1].params.response } }
local second = vim.deepcopy(first)
second.answer_revision, second.current_index = 2, 2
second.answer[#second.answer + 1] = { question_id = "second", response = pending[2].params.response }
pending[2].callback(second)
assert(continued == 0, "a newer acknowledgement discarded another pending operation")
pending[1].callback(first)
assert(continued == 1, "confirmed answers did not resume exactly once")
pending[1].callback(first)
assert(continued == 1, "duplicate acknowledgement resumed twice")
print("command_reconciliation: passed")

local client = require("forge.client")
local command = require("forge.command")
local original_generation, original_subscribe, original_defer = client.host_get_generation, client.session_events_subscribe, vim.defer_fn
local generation, requests, outcome, subscribers = 1, {}, {}, {}
client.host_get_generation = function() return generation end
client.session_events_subscribe = function(callback)
  subscribers[callback] = true
  return function() subscribers[callback] = nil end
end
vim.defer_fn = function() error("command recovery installed a polling timer") end
local function emit(event, payload)
  local copied = {}
  for callback in pairs(subscribers) do copied[#copied+1] = callback end
  for _, callback in ipairs(copied) do callback(event, payload, "session") end
end
local function transport(_, method, params, callback, sending)
  if sending then sending(generation) end
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
command.dispatch(transport, "session", "session.rename", { name = "New name" },
  function(result, failure) outcome[#outcome + 1] = { result = result, failure = failure } end)
requests[1].callback(nil, "lost acknowledgement")
assert(requests[2].method == "session.command.status.get", "unknown delivery replayed the mutation")
requests[2].callback({ state = "active" })
assert(#requests == 2 and #outcome == 0, "running work was polled or marked complete")
emit("command_completed", { id = requests[1].params._command.id, revision = 3 })
assert(#requests == 3 and requests[3].method == "session.command.status.get")
requests[3].callback({ state = "completed", response = { result = { name = "New name" } } })
assert(#outcome == 1 and outcome[1].result.name == "New name")
requests[1].callback({ name = "New name" })
emit("command_completed", { id = requests[1].params._command.id, revision = 3 })
assert(#outcome == 1 and #requests == 3 and next(subscribers) == nil, "completed command retained a waiter or replayed")
command.dispatch(transport, "session", "session.rename", { name = "Uncertain" },
  function(_, failure) outcome[#outcome + 1] = { failure = failure } end)
generation = 2
requests[4].callback(nil, "host stopped")
assert(#requests == 4 and outcome[2].failure:find("Host restarted", 1, true), "host replacement replayed a mutation")
command.dispatch(transport, "session", "session.question.answer", {
  command = { operation_id = "question-intent", question_id = "question", revision = 1, action = { kind = "skip" } },
}, function() end)
assert(requests[#requests].params._command.id == "question-intent", "question command changed recovery identity")
requests[#requests].callback({})
assert(next(subscribers) == nil)

-- A recovered plan mutation may need one readback through its original review route.
local offset = #requests
local reviewed = {}
command.dispatch(transport, "session", "session.plan.entity.rename", {
  review_document = { id = "review" }, plan_id = "plan", entity_id = "entity", name = "Renamed",
}, function(result, failure) reviewed[#reviewed + 1] = { result = result, failure = failure } end, true)
requests[offset + 1].callback(nil, "lost response")
requests[offset + 2].callback({ state = "completed", response = { result = { review_reload_required = true } } })
assert(#requests == offset + 3, "recovered review was not read back")
requests[offset + 3].callback(nil, "lost review response")
requests[offset + 4].callback({ state = "completed", response = { result = { review_reload_required = true } } })
assert(#requests == offset + 4 and #reviewed == 1, "review recovery repeated its mutation indefinitely")
assert(next(subscribers) == nil, "review recovery retained its subscription")

-- Completion arriving during the status read must wake the retained waiter.
offset = #requests
local raced = 0
command.dispatch(transport, "session", "session.rename", { name = "Raced" }, function() raced = raced + 1 end)
requests[offset + 1].callback(nil, "lost response")
emit("command_completed", { id = requests[offset + 1].params._command.id, revision = 2 })
requests[offset + 2].callback({ state = "active" })
assert(#requests == offset + 3, "completion raced past the retained waiter")
requests[offset + 3].callback({ state = "completed", response = { result = {} } })
assert(raced == 1 and next(subscribers) == nil)
client.host_get_generation, client.session_events_subscribe, vim.defer_fn = original_generation, original_subscribe, original_defer
print("command_reconciliation: completion stream passed")
vim.cmd("qa!")
