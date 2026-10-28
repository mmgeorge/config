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

require("forge").setup({ walkthrough_inventory = "sem", harness = { backend = "mock" } })
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
local original_generation, original_defer = client.host_generation, vim.defer_fn
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
local generation, timers, requests, outcome = 1, {}, {}, {}
client.host_generation = function() return generation end
vim.defer_fn = function(callback) timers[#timers + 1] = callback end
notifications.error = function() end
local function transport(_, method, params, callback, sending)
  if sending then sending(generation) end
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
command.dispatch(transport, "session", "session.rename", { name = "New name" },
  function(result, failure) outcome[#outcome + 1] = { result = result, failure = failure } end)
requests[1].callback(nil, "lost acknowledgement")
timers[1]()
assert(requests[2].method == "command.status", "unknown delivery replayed the mutation")
requests[2].callback({ state = "completed", response = { result = { name = "New name" } } })
assert(#outcome == 1 and outcome[1].result.name == "New name")
requests[1].callback({ name = "New name" })
assert(#outcome == 1, "late transport response completed twice")
command.dispatch(transport, "session", "session.rename", { name = "Uncertain" },
  function(_, failure) outcome[#outcome + 1] = { failure = failure } end)
generation = 2
requests[3].callback(nil, "host stopped")
assert(#requests == 3 and outcome[2].failure:find("Host restarted", 1, true),
  "host replacement replayed an unresolved mutation")
client.host_generation, vim.defer_fn, notifications.error = original_generation, original_defer, original_error
