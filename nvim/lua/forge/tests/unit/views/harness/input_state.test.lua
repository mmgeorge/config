-- Unit tests for `forge.views.harness.input_state` (input_state.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["input_state"] = function()
  local client = require("forge.client")
  local input = require("forge.views.harness.input_state")
  local submission = require("forge.views.harness.prompt_submission")
  local activity = require("forge.views.harness.activity")
  local activity_fixture = dofile("nvim/lua/forge/tests/support/activity.lua")
  local requests = mock_client.record()
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local generation = 1
  client.host_get_generation = function() return generation end
  local state = { session = { id = "input-projection" }, runtime_epoch = "first", queue = {} }
  activity_fixture.set_busy(state, true)
  local renders = 0
  input.attach(state, function() renders = renders + 1 end)
  requests[1].callback({ revision = 0, suspended = false, messages = {} })
  state.composer_buf = buffer.scratch()
  local context = { document = "composer", buffer = state.composer_buf, is_current = function() return true end }
  local function update(revision, messages, removed)
    input.receive(state, { revision = revision, suspended = false, upsert = messages or {}, removed = removed or {},
      execution_busy = false })
  end
  local function queued(request)
    local message = vim.deepcopy(request.spec.params.action.message)
    assert(message.delivery == nil and message.submission_id == nil and message.route == nil,
      "caller manufactured host delivery metadata")
    message.revision, message.delivery = 1, "queued"
    return message
  end

  buffer.set(state.composer_buf, { "first prompt" })
  local completed = 0
  submission.start(state, { context = context }, function() completed = completed + 1 end)
  local first = requests[#requests]
  assert(buffer.line(state.composer_buf, 1) == "" and #state.pending_submit == 1 and #state.pending_steer == 0,
    "unacknowledged editor draft was interpreted as steering")
  input.submit(state, "second prompt", {}, "queue")
  local second = requests[#requests]
  assert(#requests == 3, "Lua held an independent input command queue")
  local a, b = queued(first), queued(second)
  update(1, { a })
  update(2, { b })
  assert(#state.queue == 2 and #state.pending_submit == 0 and #state.pending_steer == 0)
  second.callback({ revision = 2, operation_id = second.spec.params.id, upsert = {}, removed = {} })
  local identity = state.queue[1]
  a.delivery, a.route, a.revision = "delivering", "steering", 2
  update(3, { a })
  assert(#state.pending_steer == 1 and state.pending_steer[1] == identity,
    "host routing did not preserve the queue editor's entry identity")
  a.delivery, a.revision = "accepted", 3
  update(4, { a })
  update(5, {}, { a.id })
  first.callback({ revision = 1, operation_id = first.spec.params.id, upsert = { queued(first) }, removed = {} })
  first.callback({ revision = 1, operation_id = first.spec.params.id, upsert = { queued(first) }, removed = {} })
  assert(completed == 1 and #state.queue == 1 and state.queue[1].id == b.id,
    "late or duplicate command reply resurrected accepted input")
  assert(activity.busy(state), "input projection changed authoritative activity")

  input.perform(state, { kind = "reserve", id = b.id, revision = 1, reservation = "edit" })
  local reserve = requests[#requests]
  assert(state.queue[1].reservation == nil, "Lua granted an edit reservation")
  b.reservation, b.revision = "edit", 2
  reserve.callback({ revision = 6, operation_id = reserve.spec.params.id, upsert = { b }, removed = {} })
  input.perform(state, { kind = "commit", reservation = "edit", edits = {
    { id = b.id, revision = 2, text = "edited prompt", attachments = {} },
  } })
  local commit = requests[#requests]
  assert(state.queue[1].text == "second prompt", "Lua committed input before host acknowledgement")
  commit.callback({ revision = 7, operation_id = commit.spec.params.id, upsert = {}, removed = {}, error = "edit conflict" })
  assert(state.queue[1].text == "second prompt" and #errors == 1)

  buffer.set(state.composer_buf, { "retain this rejected draft" })
  submission.start(state, { context = context }, function() end)
  local rejected = requests[#requests]
  buffer.set(state.composer_buf, { "newer typing" })
  local before = #requests
  rejected.callback(nil, "not admitted", { code = "not_admitted" })
  assert(#requests == before, "feature-level recovery replayed a rejected command")
  assert(buffer.line(state.composer_buf, 1) == "newer typing", "failure overwrote newer editor text")
  local draft = state.queue[#state.queue]
  assert(draft.local_only and draft.draft_state == "rejected" and draft.delivery == nil)
  input.retry(state, draft)
  local retried = requests[#requests]
  local recovered = queued(retried)
  eq(recovered.id, rejected.spec.params.action.message.id)
  retried.callback({ revision = 8, operation_id = retried.spec.params.id, upsert = { recovered }, removed = {} })

  local original_defer = vim.defer_fn
  vim.defer_fn = function() error("input projection installed a retry timer") end
  update(11)
  assert(state.input.revision == 8 and requests[#requests].spec.method == "session.input.get",
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
end

T["retracted_input"] = function()
  local input = require("forge.views.harness.input_state")
  local submission = require("forge.views.harness.prompt_submission")
  local requests = mock_client.record()
  local state = { session = { id = "cancelled-input" }, queue = {} }
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  input.attach(state, function() end)
  requests[1].callback({ revision = 0, suspended = false, messages = {} })
  state.composer_buf = buffer.scratch()
  local context = { document = "draft", buffer = state.composer_buf, is_current = function() return true end }
  for _, scenario in ipairs({ { newer = false }, { newer = true }, { newer = false, late = true }, { newer = true, late = true }, { newer = false, claimed = true } }) do
    local newer_draft = scenario.newer
    buffer.set(state.composer_buf, { "Cancel this prompt" })
    submission.start(state, { context = context }, function() end)
    local request = requests[#requests]
    local message = vim.deepcopy(request.spec.params.action.message)
    message.revision = 1
    local revision = state.input.revision + 1
    request.callback({ operation_id = request.spec.params.id, revision = revision,
      suspended = false, upsert = { message }, removed = {} })
    if newer_draft then buffer.set(state.composer_buf, { "Newer unsent draft" }) end
    if scenario.claimed then
      -- The foreground claims and accepts the submission before the user cancels it.
      local accepted = vim.tbl_extend("force", message, { delivery = "accepted" })
      input.receive(state, { revision = revision + 1, suspended = false, upsert = { accepted }, removed = {} })
      input.receive(state, { revision = revision + 2, suspended = false, upsert = {}, removed = { message.id } })
      revision = revision + 2
    end
    local count = #requests
    local cancellation = { revision = revision + 1, suspended = true,
      upsert = {}, removed = scenario.claimed and {} or { message.id }, retracted = message.id }
    if scenario.late then
      input.receive(state, { revision = revision + 2,
        snapshot = { revision = revision + 2, control_revision = 9, suspended = true, messages = {} } })
    end
    input.receive(state, cancellation)
    input.receive(state, cancellation)
    assert(#requests == count, "rendering a cancelled input issued another queue mutation")
    assert(#state.input.confirmed == 0, "cancelled input remained in the queue")
    if scenario.late then
      assert(state.input.revision == revision + 2 and state.input.control_revision == 9,
        "late retraction regressed the authoritative input projection")
    end
    assert(buffer.line(state.composer_buf, 1)
      == (newer_draft and "Newer unsent draft" or "Cancel this prompt"), "cancellation overwrote or lost the draft")
    -- A retraction the composer could not take stays recoverable as an editor-only draft.
    local recovered = vim.tbl_filter(function(entry)
      return entry.local_only and entry.text == "Cancel this prompt"
    end, state.queue)
    eq(#recovered, newer_draft and 1 or 0)
    if newer_draft then
      assert(recovered[1].draft_state == "rejected" and recovered[1].id ~= message.id,
        "recovered retraction reused the retired host identity")
      eq(#requests, count)
      input.discard_rejected(state, recovered[1].id)
    end
  end
end

T["retraction_into_queue_edit"] = function()
  local input = require("forge.views.harness.input_state")
  local submission = require("forge.views.harness.prompt_submission")
  local requests = mock_client.record()
  local state = { session = { id = "queue-edit-retraction" }, queue = {} }
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  input.attach(state, function() end)
  requests[1].callback({ revision = 0, suspended = false, messages = {} })
  state.composer_buf = buffer.scratch()
  local context = { document = "draft", buffer = state.composer_buf, is_current = function() return true end }
  buffer.set(state.composer_buf, { "Retract me" })
  submission.start(state, { context = context }, function() end)
  local request = requests[#requests]
  local message = vim.deepcopy(request.spec.params.action.message)
  message.revision = 1
  request.callback({ operation_id = request.spec.params.id, revision = 1, suspended = false, upsert = { message }, removed = {} })
  -- An empty queue edit owns the composer; the retraction must not write into it.
  state.queue_edit = { drafts = {}, entry = {} }
  input.receive(state, { revision = 2, suspended = true, upsert = {}, removed = { message.id }, retracted = message.id })
  eq(buffer.line(state.composer_buf, 1), "")
  local recovered = vim.tbl_filter(function(entry) return entry.local_only and entry.text == "Retract me" end, state.queue)
  eq(#recovered, 1)
end

T["perform_settles_after_ownership_change"] = function()
  local input = require("forge.views.harness.input_state")
  local requests = mock_client.record()
  notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local state = { session = { id = "settled-owner" }, queue = {} }
  input.attach(state, function() end)
  requests[1].callback({ revision = 0, suspended = false, messages = {} })
  local settled
  input.submit(state, "explain", {}, "auto", nil, function(failure) settled = failure end)
  local request = requests[#requests]
  state.session = { id = "another-session" }
  request.callback({ operation_id = request.spec.params.id, revision = 1, suspended = false, upsert = {}, removed = {} })
  assert(type(settled) == "string", "a command settled after the view changed owner never answered its caller")
end

return T
