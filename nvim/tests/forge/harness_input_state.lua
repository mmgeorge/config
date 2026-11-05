vim.loader.enable(false)
local client = require("forge.client")
local input = require("forge.views.harness.input_state")
local submission = require("forge.views.harness.prompt_submission")
local activity = require("forge.views.harness.activity")
local requests, errors = {}, {}
client.request = function(spec, callback)
  requests[#requests + 1] = { method = spec.method, parameters = spec.params, callback = callback }
end
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local generation = 1
client.host_get_generation = function() return generation end
local state = { session = { id = "input-projection" }, runtime_epoch = "first",
  activity = { revision = 1, phase = "working" }, queue = {} }
local renders = 0
input.attach(state, function() renders = renders + 1 end)
requests[1].callback({ revision = 0, suspended = false, messages = {} })
state.composer_buf = vim.api.nvim_create_buf(false, true)
local context = { document = "composer", buffer = state.composer_buf, is_current = function() return true end }
local function compose(text) vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text }) end
local function text() return vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] end
local function update(revision, messages, removed)
  input.receive(state, { revision = revision, suspended = false, upsert = messages or {}, removed = removed or {},
    execution_busy = false })
end
local function queued(request)
  local message = vim.deepcopy(request.parameters.action.message)
  assert(message.delivery == nil and message.submission_id == nil and message.route == nil,
    "caller manufactured host delivery metadata")
  message.revision, message.delivery = 1, "queued"
  return message
end

compose("first prompt")
local completed = 0
submission.start(state, { context = context }, function() completed = completed + 1 end)
local first = requests[#requests]
assert(text() == "" and #state.pending_submit == 1 and #state.pending_steer == 0,
  "unacknowledged editor draft was interpreted as steering")
input.submit(state, "second prompt", {}, "queue")
local second = requests[#requests]
assert(#requests == 3, "Lua held an independent input command queue")
local a, b = queued(first), queued(second)
update(1, { a })
update(2, { b })
assert(#state.queue == 2 and #state.pending_submit == 0 and #state.pending_steer == 0)
second.callback({ revision = 2, operation_id = second.parameters.id, upsert = {}, removed = {} })
local identity = state.queue[1]
a.delivery, a.route, a.revision = "delivering", "steering", 2
update(3, { a })
assert(#state.pending_steer == 1 and state.pending_steer[1] == identity,
  "host routing did not preserve the queue editor's entry identity")
a.delivery, a.revision = "accepted", 3
update(4, { a })
update(5, {}, { a.id })
first.callback({ revision = 1, operation_id = first.parameters.id, upsert = { queued(first) }, removed = {} })
first.callback({ revision = 1, operation_id = first.parameters.id, upsert = { queued(first) }, removed = {} })
assert(completed == 1 and #state.queue == 1 and state.queue[1].id == b.id,
  "late or duplicate command reply resurrected accepted input")
assert(activity.busy(state), "input projection changed authoritative activity")

input.perform(state, { kind = "reserve", id = b.id, revision = 1, reservation = "edit" })
local reserve = requests[#requests]
assert(state.queue[1].reservation == nil, "Lua granted an edit reservation")
b.reservation, b.revision = "edit", 2
reserve.callback({ revision = 6, operation_id = reserve.parameters.id, upsert = { b }, removed = {} })
input.perform(state, { kind = "commit", reservation = "edit", edits = {
  { id = b.id, revision = 2, text = "edited prompt", attachments = {} },
} })
local commit = requests[#requests]
assert(state.queue[1].text == "second prompt", "Lua committed input before host acknowledgement")
commit.callback({ revision = 7, operation_id = commit.parameters.id, upsert = {}, removed = {}, error = "edit conflict" })
assert(state.queue[1].text == "second prompt" and #errors == 1)

compose("retain this rejected draft")
submission.start(state, { context = context }, function() end)
local rejected = requests[#requests]
compose("newer typing")
local before = #requests
rejected.callback(nil, "not admitted", { code = "not_admitted" })
assert(#requests == before, "feature-level recovery replayed a rejected command")
assert(text() == "newer typing", "failure overwrote newer editor text")
local draft = state.queue[#state.queue]
assert(draft.local_only and draft.draft_state == "rejected" and draft.delivery == nil)
input.retry(state, draft)
local retried = requests[#requests]
local recovered = queued(retried)
assert(recovered.id == rejected.parameters.action.message.id)
retried.callback({ revision = 8, operation_id = retried.parameters.id, upsert = { recovered }, removed = {} })

local original_defer = vim.defer_fn
vim.defer_fn = function() error("input projection installed a retry timer") end
update(11)
assert(state.input.revision == 8 and requests[#requests].method == "session.input.get",
  "a delta with a missing baseline was committed")
local refresh = requests[#requests]
update(12)
assert(requests[#requests] == refresh, "missing baseline started competing refreshes")
refresh.callback({ revision = 12, suspended = false, messages = { b, recovered } })
assert(state.input.revision == 12 and state.input.required_revision == nil)
update(9, {}, { b.id })
assert(#state.queue == 2, "stale input event changed the current projection")
vim.defer_fn = original_defer

input.submit(state, "unconfirmed", {}, "queue")
local uncertain = requests[#requests]
before = #requests
uncertain.callback(nil, "outcome unconfirmed", { code = "outcome_unknown" })
local unknown = state.queue[#state.queue]
assert(unknown.local_only and unknown.draft_state == "unknown" and #requests == before)
input.retry(state, unknown)
assert(#requests == before, "uncertain submission was blindly resent")
input.submit(state, "pending at restart", {}, "queue")
local pending = requests[#requests]
generation, state.runtime_epoch = 2, "second"
input.refresh(state)
requests[#requests].callback({ revision = 0, suspended = true, messages = {} })
pending.callback(nil, "old host response", { code = "outcome_unknown" })
assert(#state.pending_submit == 0 and #state.pending_steer == 0)
for _, entry in ipairs(state.queue) do
  assert(entry.local_only and entry.draft_state == "unknown", "host replacement replayed a draft")
end
assert(renders > 0)
print("harness_input_state: passed")
vim.cmd("qa!")
