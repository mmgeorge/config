vim.loader.enable(false)
local client = require("forge.client")
local input = require("forge.views.harness.input_state")
local submission = require("forge.views.harness.prompt_submission")
local requests, errors = {}, {}
client.request_for = function(session_id, method, parameters, callback)
  assert(session_id == "optimistic-input")
  requests[#requests + 1] = { method = method, parameters = parameters, callback = callback }
end
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local state = { session = { id = "optimistic-input", active_task = { revision = "task", kind = "goal", goal = { objective = "Fixture goal", state = "active" } } }, queue = {} }
local renders = 0
input.attach(state, function() renders = renders + 1 end)
assert(requests[1].method == "input.get")
requests[1].callback({ revision = 0, suspended = false, execution_busy = true, messages = {} })

state.composer_buf = vim.api.nvim_create_buf(false, true)
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
end
local function text()
  return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1]
end
local context = { document = "composer", buffer = state.composer_buf, is_current = function() return true end }
compose("first message")
submission.start(state, { text = "first message", context = context }, function() end)
assert(text() == "" and #state.queue == 0 and #state.pending_steer == 1,
  "active submission did not enter the separate steering presentation immediately")
local first = requests[2]
assert(first.method == "input.apply" and first.parameters.action.kind == "submit")
assert(first.parameters.action.message.purpose.kind == "user"
  and first.parameters.action.message.task_id == nil, "user input retained workflow ownership")
local first_id = first.parameters.action.message.id
compose("newer unsent text")
local second_id = input.submit(state, "queued message", {}, "queue")
assert(#state.queue == 1 and #state.pending_steer == 1 and #requests == 2,
  "explicit queue and pending steering were not separated or ordered")
first.callback({ operation_id = first.parameters.id, revision = 1, suspended = false,
  upsert = {}, removed = {}, error = "message rejected" })
assert(text() == "newer unsent text", "rollback overwrote newer typing")
assert(#errors == 1 and #state.input.rejected == 1)
local second = requests[3]
local accepted = vim.deepcopy(second.parameters.action.message)
accepted.revision = 1
second.callback({ operation_id = second.parameters.id, revision = 2, suspended = false,
  upsert = { accepted }, removed = {} })
assert(#state.input.pending == 0)
assert(state.queue[1].id == second_id and state.queue[2].id == first_id)
local identity = state.queue[1]
input.perform(state, { kind = "reserve", id = second_id, revision = 1, reservation = "edit" })
assert(state.queue[1] == identity and identity.reservation == "edit")
local reservation = requests[4]
accepted = vim.deepcopy(accepted)
accepted.revision, accepted.reservation = 2, "edit"
reservation.callback({ operation_id = reservation.parameters.id, revision = 3, suspended = false,
  upsert = { accepted }, removed = {} })
input.perform(state, { kind = "commit", reservation = "edit", edits = {
  { id = second_id, revision = 2, text = "changed", attachments = {} },
} })
assert(identity.text == "changed", "edit commit waited for a host reply")
local commit = requests[5]
input.submit(state, "later operation", {}, "queue")
commit.callback({ operation_id = commit.parameters.id, revision = 4, suspended = false,
  upsert = {}, removed = {}, error = "edit conflict" })
assert(identity.text == "queued message", "failed edit was not rolled back")
assert(state.queue[2].text == "later operation", "rollback discarded a later operation")
assert(text() == "newer unsent text")
input.receive(state, { revision = 3, suspended = false, upsert = {}, removed = { second_id } })
assert(state.queue[1] == identity, "stale publication removed a current message")
assert(renders > 0)
local accepted_message = vim.deepcopy(identity)
accepted_message.delivery = "accepted"
input.receive(state, { revision = 5, suspended = false, execution_busy = true,
  upsert = { accepted_message }, removed = {} })
assert(state.busy and state.execution_busy, "acceptance prematurely completed execution")
for _, queued in ipairs(state.queue) do
  assert(queued.id ~= second_id, "accepted message remained labelled as queued or sending")
end
accepted_message.delivery = "delivering"
input.receive(state, { revision = 6, suspended = false, execution_busy = true,
  upsert = { accepted_message }, removed = {} })
for _, queued in ipairs(state.queue) do
  assert(queued.id ~= second_id, "dispatching message remained visible as queued input")
end
local original_defer = vim.defer_fn
local retry
vim.defer_fn = function(callback) retry = callback end
local pending = requests[#requests]
local error_count = #errors
pending.callback(nil, "Input storage is busy", { code = "not_admitted" })
assert(retry and #errors == error_count, "temporary storage contention became a failed message")
local pending_id = pending.parameters.id
retry()
assert(requests[#requests].parameters.id == pending_id, "storage retry changed the input operation identity")
vim.defer_fn = original_defer
state.runtime_epoch = "replacement-host"
input.refresh(state)
local replacement = requests[#requests]
assert(replacement.method == "input.get")
replacement.callback({ revision = 0, suspended = false, execution_busy = false, messages = {} })
assert(#state.input.pending == 0 and #state.queue > 0,
  "host restart replayed pending operations or discarded recoverable text")
for _, entry in ipairs(state.queue) do
  assert(entry.delivery == "unknown" and entry.local_only, "recovery draft became runnable")
end
assert(#state.pending_steer == 0 and #state.pending_submit == 0)
state.input.rejected = {}
state.queue = {}

compose("idle direct prompt")
submission.start(state, { text = "idle direct prompt", context = context }, function() end)
local direct = requests[#requests]
assert(text() == "" and #state.pending_submit == 1 and #state.queue == 0 and #state.pending_steer == 0,
  "idle submission flashed in a queue")
local direct_message = vim.deepcopy(direct.parameters.action.message)
direct_message.revision = 1
direct.callback({ operation_id = direct.parameters.id, revision = 1, suspended = false,
  execution_busy = true, upsert = { direct_message }, removed = {} })
assert(#state.pending_submit == 1 and #state.queue == 0 and #state.pending_steer == 0,
  "the acknowledgement reclassified an idle submission as steering")
direct_message.delivery = "delivering"
input.receive(state, { revision = 2, suspended = false, execution_busy = true,
  upsert = { direct_message }, removed = {} })
assert(#state.pending_submit == 1 and #state.queue == 0 and #state.pending_steer == 0)
direct_message.delivery = "accepted"
input.receive(state, { revision = 3, suspended = false, execution_busy = true,
  upsert = { direct_message }, removed = {} })
assert(#state.pending_submit == 0, "accepted direct submission retained its pending presentation")
input.receive(state, { revision = 4, suspended = false, execution_busy = false,
  upsert = {}, removed = { direct_message.id } })
assert(next(state.input.direct_submission) == nil, "settled direct presentation identity leaked")

compose("rejected direct prompt")
submission.start(state, { text = "rejected direct prompt", context = context }, function() end)
local rejected_direct = requests[#requests]
compose("newer draft")
rejected_direct.callback({ operation_id = rejected_direct.parameters.id, revision = 5, suspended = false,
  upsert = {}, removed = {}, error = "direct submission rejected" })
assert(text() == "newer draft" and #state.pending_submit == 0 and #state.queue == 1,
  "rejected direct submission lost its recovery entry or overwrote newer typing")
assert(state.queue[1].text == "rejected direct prompt" and state.queue[1].delivery == "failed")
local failed_id = state.queue[1].id
input.retry(state, state.queue[1])
local resubmit = requests[#requests]
assert(resubmit.parameters.action.message.id == failed_id, "retry replaced the message identity")
resubmit.callback({ operation_id = resubmit.parameters.id, revision = 9,
  snapshot = { revision = 9, suspended = false, messages = {}, control_revision = 0 },
  execution_busy = false, upsert = {}, removed = {} })
assert(state.input.revision == 9 and #state.queue == 0, "complete snapshot failed to bridge revision gap")
input.receive(state, { revision = 8, execution_busy = true, upsert = {}, removed = {} })
assert(not state.execution_busy, "stale input update changed execution state")
input.receive(state, { revision = 12, upsert = {}, removed = {} })
assert(state.input.revision == 9, "incomplete delta advanced the confirmed baseline")
assert(requests[#requests].method == "input.get")
local recovery = requests[#requests]
input.receive(state, { revision = 10, execution_busy = true, upsert = {}, removed = {} })
input.receive(state, { revision = 13, execution_busy = true, upsert = {}, removed = {} })
assert(state.input.revision == 9 and not state.execution_busy, "delta advanced a replica with a missing baseline")
recovery.callback({ revision = 13, suspended = false, execution_busy = false, messages = {} })
assert(state.input.revision == 13 and not state.input.required_revision, "snapshot did not repair the revision gap")
print("harness_input_state: passed")
vim.cmd("qa!")
