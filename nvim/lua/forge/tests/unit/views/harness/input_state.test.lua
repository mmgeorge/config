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
  -- Only the explicitly queued message waits visibly; the automatic one awaits the host's claim.
  assert(#state.queue == 1 and state.queue[1].id == b.id and #state.pending_submit == 1 and #state.pending_steer == 0)
  second.callback({ revision = 2, operation_id = second.spec.params.id, upsert = {}, removed = {} })
  local identity = state.pending_submit[1]
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
  local scenarios = { { newer = false }, { newer = true }, { newer = false, late = true }, { newer = true, late = true },
    { newer = false, claimed = true } }
  for index, scenario in ipairs(scenarios) do
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
      local accepted = vim.tbl_extend("force", message, { delivery = "accepted", revision = 2 })
      input.receive(state, { revision = revision + 1, suspended = false, upsert = { accepted }, removed = {} })
      revision = revision + 1
    end
    -- The host retires the retracted identity and keeps the text as an unsent follow-up, so the
    -- prompt survives an editor that exits before it restores the composer.
    local unsent = { id = "retracted:" .. index, revision = 1, text = "Cancel this prompt", attachments = {},
      purpose = { kind = "user" }, intent = "queue", delivery = "failed",
      error = "Interrupted before the model replied. Retry to send it again." }
    local count = #requests
    local cancellation = { revision = revision + 1, suspended = true,
      upsert = { unsent }, removed = { message.id }, retracted = message.id }
    if scenario.late then
      input.receive(state, { revision = revision + 2,
        snapshot = { revision = revision + 2, control_revision = 9, suspended = true, messages = { unsent } } })
    end
    input.receive(state, cancellation)
    input.receive(state, cancellation)
    if scenario.late then
      assert(state.input.revision == revision + 2 and state.input.control_revision == 9,
        "late retraction regressed the authoritative input projection")
    end
    assert(buffer.line(state.composer_buf, 1)
      == (newer_draft and "Newer unsent draft" or "Cancel this prompt"), "cancellation overwrote or lost the draft")
    eq(#vim.tbl_filter(function(entry) return entry.local_only end, state.queue), 0)
    local function held()
      return vim.tbl_filter(function(entry) return entry.id == unsent.id end, state.queue)
    end
    eq(#vim.tbl_filter(function(entry) return entry.id == message.id end, state.queue), 0)
    if newer_draft then
      -- The composer could not take the text, so the host's unsent entry stays for retry or edit.
      eq(#requests, count)
      eq(#held(), 1)
      eq(held()[1].delivery, "failed")
      input.receive(state, { revision = state.input.revision + 1, suspended = true, upsert = {}, removed = { unsent.id } })
    else
      -- The composer holds the text again, so the editor removes the host's copy exactly once.
      eq(#requests, count + 1)
      local removal = requests[#requests]
      eq(removal.spec.method, "session.input.apply")
      eq(removal.spec.params.action, { kind = "remove", id = unsent.id, revision = unsent.revision })
      removal.callback({ operation_id = removal.spec.params.id, revision = state.input.revision + 1,
        suspended = true, upsert = {}, removed = { unsent.id } })
    end
    eq(#held(), 0)
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
  local unsent = { id = "retracted:queue-edit", revision = 1, text = "Retract me", attachments = {},
    purpose = { kind = "user" }, intent = "queue", delivery = "failed", error = "Interrupted before the model replied." }
  local count = #requests
  input.receive(state, { revision = 2, suspended = true, upsert = { unsent }, removed = { message.id }, retracted = message.id })
  eq(buffer.line(state.composer_buf, 1), "")
  -- The host's unsent entry is the only copy; the editor neither removes nor duplicates it.
  eq(#requests, count)
  eq(vim.tbl_map(function(entry) return { entry.id, entry.local_only } end, state.queue), { { unsent.id } })
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

-- A session fixture whose every input render is recorded, so a case can assert on a placement
-- that the coalesced status render would only show some of the time.
local function placement_fixture(session_id, status_kind)
  local input = require("forge.views.harness.input_state")
  local requests = mock_client.record()
  local state = { session = { id = session_id }, queue = {}, status = { kind = status_kind } }
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, status_kind ~= "idle")
  local fixture = { input = input, state = state, requests = requests, renders = {} }
  input.attach(state, function()
    fixture.renders[#fixture.renders + 1] = { queue = #state.queue, handoff = #(state.pending_handoff or {}),
      steering = #state.pending_steer }
  end)
  requests[1].callback({ revision = 0, suspended = false, messages = {} })
  ---Answer the newest submission as the host does and return the committed message.
  function fixture.commit()
    local request = requests[#requests]
    local message = vim.deepcopy(request.spec.params.action.message)
    message.revision, message.delivery = 1, "queued"
    request.callback({ operation_id = request.spec.params.id, revision = state.input.revision + 1,
      suspended = false, upsert = { vim.deepcopy(message) }, removed = {} })
    return message
  end
  ---Publish the next delivery state of `message`.
  function fixture.deliver(message, delivery, route)
    message.delivery, message.route, message.revision = delivery, route, message.revision + 1
    input.receive(state, { revision = state.input.revision + 1, suspended = false,
      upsert = { vim.deepcopy(message) }, removed = {} })
  end
  return fixture
end

T["idle_submission_never_renders_a_queue_row"] = function()
  local fixture = placement_fixture("idle-submission", "idle")
  fixture.input.submit(fixture.state, "Explain the module", {}, "auto")
  local message = fixture.commit()
  -- The host's hand-off phase can report working before the editor hears of the claim.
  fixture.state.status = { kind = "working" }
  fixture.deliver(message, "queued")
  fixture.deliver(message, "delivering", "turn")
  fixture.deliver(message, "accepted", "turn")
  for _, render in ipairs(fixture.renders) do
    eq(render, { queue = 0, handoff = 0, steering = 0 })
  end
  eq(#fixture.state.pending_submit, 0)
end

T["follow_up_stays_staged_until_its_turn_is_accepted"] = function()
  local fixture = placement_fixture("follow-up-hand-off", "working")
  local state = fixture.state
  fixture.input.submit(state, "Then summarize it", {}, "queue")
  local message = fixture.commit()
  eq(#state.queue, 1)
  local identity = state.queue[1]
  -- The turn ended and the owner claimed the follow-up; its prompt is not in the transcript yet.
  state.status = { kind = "idle" }
  fixture.deliver(message, "delivering", "turn")
  eq(#state.queue, 0)
  eq(state.pending_handoff, { identity })
  eq(#state.pending_submit, 0)
  fixture.deliver(message, "accepted", "turn")
  eq(#state.pending_handoff, 0)
  eq(state.input.follow_up, {})
end

T["unanswered_submission_surfaces_after_the_notice_delay"] = function()
  local deferred = dofile("nvim/lua/forge/tests/support/deferred.lua")
  local fixture = placement_fixture("unanswered-submission", "idle")
  local state, input = fixture.state, fixture.input
  local timers = deferred.install({ input.submission_notice_delay_ms })
  local staging = require("forge.views.harness.queue_editor").staging

  -- A submission the host answers within the delay never gains a row, even after the delay.
  input.submit(state, "Answered promptly", {}, "auto")
  local prompt = fixture.commit()
  fixture.deliver(prompt, "delivering", "turn")
  fixture.deliver(prompt, "accepted", "turn")
  local settled = #fixture.renders
  timers.fire(input.submission_notice_delay_ms)
  eq(#fixture.renders, settled)
  for _, render in ipairs(fixture.renders) do eq(render.queue + render.handoff, 0) end

  -- An owner holding commands leaves the request unanswered; the prompt must not stay hidden.
  input.submit(state, "Held by a stop", {}, "auto")
  eq(#state.queue, 0)
  eq(#state.pending_submit, 1)
  eq(timers.fire(input.submission_notice_delay_ms), 1)
  eq(vim.tbl_map(function(staged) return staged.text end, staging(state, nil, state.queue)),
    { "[submitting] Held by a stop" })
  eq(#state.pending_submit, 0)
  -- The row then follows the prompt until its turn shows it in the transcript.
  local held = fixture.commit()
  eq(vim.tbl_map(function(staged) return staged.text end, staging(state, nil, state.queue)), { "Held by a stop" })
  fixture.deliver(held, "delivering", "turn")
  eq(#state.queue, 0)
  eq(#state.pending_handoff, 1)
  fixture.deliver(held, "accepted", "turn")
  eq(#state.pending_handoff, 0)

  -- A committed prompt that no owner claims is as hidden as an unanswered one.
  input.submit(state, "Committed but unclaimed", {}, "auto")
  fixture.commit()
  eq(#state.queue, 0)
  timers.fire(input.submission_notice_delay_ms)
  eq(#state.queue, 1)
end

-- The host produces a task's planning and continuation input itself, and an idle owner claims it
-- at once; drawing it as a queued follow-up flashed a row for the moment before that claim. Such
-- input waits out the notice delay like this view's own submissions, while input another view
-- submitted, and everything in a paused queue, still shows at once.
T["host_produced_input_surfaces_only_after_the_notice_delay"] = function()
  local deferred = dofile("nvim/lua/forge/tests/support/deferred.lua")
  local fixture = placement_fixture("host-produced-input", "idle")
  local state, input = fixture.state, fixture.input
  local timers = deferred.install({ input.submission_notice_delay_ms })
  local revision = 0
  local function publish(message, suspended)
    revision = revision + 1
    input.receive(state, { revision = revision, suspended = suspended or false,
      upsert = { vim.deepcopy(message) }, removed = {} })
  end
  local planning = { id = "task-input:plan", text = "Add a function moneyAllocate", revision = 1, delivery = "queued",
    intent = "auto", purpose = { kind = "task_entry", revision = "plan", id = "plan" } }

  -- Claimed within the delay, the task's input never gains a row.
  publish(planning)
  eq(#state.queue, 0)
  planning.delivery, planning.route = "delivering", "turn"
  publish(planning)
  planning.delivery = "accepted"
  publish(planning)
  timers.fire(input.submission_notice_delay_ms)
  for _, render in ipairs(fixture.renders) do eq(render.queue + render.handoff, 0) end

  -- Unclaimed past the delay, it shows as waiting.
  local continuation = { id = "task-input:continue", text = "Continue the plan", revision = 1, delivery = "queued",
    intent = "auto", purpose = { kind = "task_continuation", revision = "plan", id = "continue",
      completed_turn_id = "turn", reason = "budget" } }
  publish(continuation)
  eq(#state.queue, 0)
  eq(timers.fire(input.submission_notice_delay_ms), 1)
  eq(#state.queue, 1)

  -- Another view's submission and a paused queue's task input both show at once.
  local foreign = { id = "other-view", text = "From another tab", revision = 1, delivery = "queued",
    intent = "auto", purpose = { kind = "user" } }
  publish(foreign)
  eq(#state.queue, 2)
  local paused = { id = "task-input:paused", text = "Resume the plan", revision = 1, delivery = "queued",
    intent = "auto", purpose = { kind = "task_continuation", revision = "plan", id = "paused",
      completed_turn_id = "turn", reason = "paused" } }
  publish(paused, true)
  eq(#state.queue, 3)
end

T["submission_after_a_cancellation_never_renders_a_queue_row"] = function()
  local fixture = placement_fixture("after-cancellation", "idle")
  local state, input = fixture.state, fixture.input
  -- A cancellation leaves the stopped prompt as a failed follow-up and the status not yet idle.
  local stopped = { id = "retracted:stopped", revision = 1, text = "Stopped prompt", attachments = {},
    purpose = { kind = "user" }, intent = "queue", delivery = "failed", error = "Interrupted before the model replied." }
  input.receive(state, { revision = 1, suspended = false, upsert = { stopped }, removed = {} })
  state.status = { kind = "stopping" }
  local before = #fixture.renders
  input.submit(state, "Start over", {}, "auto")
  local message = fixture.commit()
  state.status = { kind = "working" }
  fixture.deliver(message, "delivering", "turn")
  fixture.deliver(message, "accepted", "turn")
  for index = before + 1, #fixture.renders do
    eq(fixture.renders[index], { queue = 1, handoff = 0, steering = 0 })
  end
end

T["steering_never_renders_as_a_queued_follow_up"] = function()
  local fixture = placement_fixture("steering-placement", "working")
  local state, input = fixture.state, fixture.input
  input.submit(state, "Steer the essay", {}, "auto")
  local message = fixture.commit()
  fixture.deliver(message, "queued")
  fixture.deliver(message, "delivering", "steering")
  eq(#state.pending_steer, 1)
  for _, render in ipairs(fixture.renders) do eq(render.queue, 0) end
end

-- While the provider is still starting its turn the host keeps a steer unclaimed, so the notice
-- delay used to draw it under "Queued follow-up inputs". A steer the running turn will take waits
-- in the "Steering active turn" section instead; an approval, a wait for children, and a paused
-- queue hold input, so those still list it as a queued follow-up.
T["unclaimed_steer_waits_under_steering_after_the_notice_delay"] = function()
  local deferred = dofile("nvim/lua/forge/tests/support/deferred.lua")
  local fixture = placement_fixture("waiting-steer", "working")
  local state, input = fixture.state, fixture.input
  local timers = deferred.install({ input.submission_notice_delay_ms })
  local staging = require("forge.views.harness.queue_editor").staging

  input.submit(state, "Reply with alpha", {}, "auto")
  local steer = fixture.commit()
  eq(timers.fire(input.submission_notice_delay_ms), 1)
  eq(#state.queue, 0)
  eq(vim.tbl_map(function(staged) return staged.text end, staging(state, nil, state.pending_steer)),
    { "[waiting] Reply with alpha" })
  local identity = state.pending_steer[1]
  fixture.deliver(steer, "delivering", "steering")
  eq(state.pending_steer, { identity })
  eq(vim.tbl_map(function(staged) return staged.text end, staging(state, nil, state.pending_steer)),
    { "Reply with alpha" })
  fixture.deliver(steer, "accepted", "steering")
  eq(state.pending_steer, { identity })
  for _, render in ipairs(fixture.renders) do eq(render.queue + render.handoff, 0) end

  -- A turn that ended before the steer reached it starts the steer as its own turn, keeping its row.
  input.submit(state, "Then summarize", {}, "auto")
  local late = fixture.commit()
  timers.fire(input.submission_notice_delay_ms)
  local late_identity = state.pending_steer[#state.pending_steer]
  eq(late_identity.id, late.id)
  state.status = { kind = "idle" }
  fixture.deliver(late, "delivering", "turn")
  eq(state.pending_handoff, { late_identity })

  for _, holding in ipairs({ { kind = "awaiting_approval" }, { kind = "waiting_for_agent", agent_count = 2 } }) do
    state.status = holding
    input.submit(state, "Held prompt", {}, "auto")
    local held = fixture.commit()
    timers.fire(input.submission_notice_delay_ms)
    eq(state.queue[#state.queue].id, held.id)
    for _, staged in ipairs(state.pending_steer) do assert(staged.id ~= held.id, "a held prompt was drawn as steering") end
  end

  -- An interrupt that pauses the queue moves a waiting steer into the paused queue.
  state.status = { kind = "working" }
  input.submit(state, "Paused prompt", {}, "auto")
  local paused = fixture.commit()
  timers.fire(input.submission_notice_delay_ms)
  eq(state.pending_steer[#state.pending_steer].id, paused.id)
  input.receive(state, { revision = state.input.revision + 1, suspended = true, upsert = {}, removed = {} })
  eq(state.queue[#state.queue].id, paused.id)
  for _, staged in ipairs(state.pending_steer) do assert(staged.id ~= paused.id, "a paused prompt stayed in steering") end
end

-- A stop holds every unclaimed submission, so a steer still waiting when the user stops the turn
-- moves into the queue at once and stays there, unsent, after the stop settles.
T["waiting_steer_moves_to_the_queue_when_the_turn_stops"] = function()
  local deferred = dofile("nvim/lua/forge/tests/support/deferred.lua")
  local fixture = placement_fixture("stopped-steer", "working")
  local state, input = fixture.state, fixture.input
  local timers = deferred.install({ input.submission_notice_delay_ms })
  input.submit(state, "Also mention the word anchor once.", {}, "auto")
  local steer = fixture.commit()
  timers.fire(input.submission_notice_delay_ms)
  eq(state.pending_steer[1].id, steer.id)
  state.status = { kind = "stopping" }
  input.status_changed(state)
  eq(vim.tbl_map(function(entry) return entry.id end, state.queue), { steer.id })
  eq(#state.pending_steer, 0)
  state.status = { kind = "idle" }
  input.receive(state, { revision = state.input.revision + 1, suspended = true, upsert = {}, removed = {} })
  eq(vim.tbl_map(function(entry) return entry.id end, state.queue), { steer.id })
end

-- The host reports a steer accepted before its exchange holds the acknowledged copy, so dropping
-- the staged row at acceptance left the "Steering active turn" section without it for a frame.
T["accepted_steer_stays_staged_until_the_turn_that_took_it_ends"] = function()
  local fixture = placement_fixture("accepted-steer", "working")
  local state, input = fixture.state, fixture.input
  input.submit(state, "Steer the essay", {}, "auto")
  local message = fixture.commit()
  fixture.deliver(message, "delivering", "steering")
  local first = #fixture.renders
  fixture.deliver(message, "accepted", "steering")
  for index = first, #fixture.renders do eq(fixture.renders[index].steering, 1) end
  local activity = require("forge.views.harness.activity")
  activity.receive(state, { phase = "idle", revision = state.activity.revision + 1 }, state.runtime_epoch)
  eq(#state.pending_steer, 0)
end

T["withdrawn_entries_leave_the_queue_before_the_host_answers"] = function()
  local fixture = placement_fixture("withdrawn-entries", "working")
  local state, input, requests = fixture.state, fixture.input, fixture.requests
  local queued = { id = "queued-entry", revision = 2, text = "Delete me", attachments = {},
    purpose = { kind = "user" }, intent = "queue", delivery = "queued", reservation = "edit" }
  input.receive(state, { revision = 1, suspended = false, upsert = { queued }, removed = {} })
  eq(#state.queue, 1)
  -- An empty edit deletes the message; the row must not return while the commit is in flight.
  input.perform(state, { kind = "commit", reservation = "edit", edits = {
    { id = queued.id, revision = 2, text = "", attachments = {} } } })
  eq(#state.queue, 0)
  local commit = requests[#requests]
  notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  commit.callback(nil, "edit conflict")
  eq(vim.tbl_map(function(entry) return entry.id end, state.queue), { queued.id })

  -- A retraction the composer took back is removed by the editor; its host copy never renders.
  state.composer_buf = buffer.scratch()
  local shown = false
  state.input.changed = function()
    for _, entry in ipairs(state.queue) do if entry.id == "retracted:taken" then shown = true end end
  end
  input.submit(state, "Take me back", {}, "auto", nil, nil, function()
    buffer.set(state.composer_buf, { "Take me back" })
    return true
  end)
  local message = fixture.commit()
  local unsent = { id = "retracted:taken", revision = 1, text = "Take me back", attachments = {},
    purpose = { kind = "user" }, intent = "queue", delivery = "failed", error = "Interrupted before the model replied." }
  input.receive(state, { revision = state.input.revision + 1, suspended = true, upsert = { unsent },
    removed = { message.id }, retracted = message.id })
  eq(requests[#requests].spec.params.action, { kind = "remove", id = unsent.id, revision = unsent.revision })
  eq(shown, false)
end

T["failed_submission_returns_to_the_composer_or_its_row"] = function()
  local input = require("forge.views.harness.input_state")
  local submission = require("forge.views.harness.prompt_submission")
  local requests = mock_client.record()
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  local state = { session = { id = "failed-submission" }, queue = {} }
  dofile("nvim/lua/forge/tests/support/activity.lua").set_busy(state, false)
  input.attach(state, function() end)
  requests[1].callback({ revision = 0, suspended = false, messages = {} })
  state.composer_buf = buffer.scratch()
  local context = { document = "composer", buffer = state.composer_buf, is_current = function() return true end }

  -- The composer is still free, so the text returns there and no failed row duplicates it.
  buffer.set(state.composer_buf, { "Restart failed" })
  submission.start(state, { context = context }, function() end)
  requests[#requests].callback(nil, "provider restart failed")
  eq(buffer.line(state.composer_buf, 1), "Restart failed")
  eq(#state.queue, 0)
  eq(#errors, 1)

  -- Newer typing keeps the composer; the text stays in a failed row with the host's reason.
  submission.start(state, { context = context }, function() end)
  local second = requests[#requests]
  buffer.set(state.composer_buf, { "newer typing" })
  second.callback({ revision = 1, upsert = {}, removed = {}, error = "Session unavailable" })
  eq(buffer.line(state.composer_buf, 1), "newer typing")
  eq(vim.tbl_map(function(entry) return { entry.text, entry.error } end, state.queue),
    { { "Restart failed", "Session unavailable" } })

  -- A session that became unavailable while the request was in flight still keeps the text.
  input.discard_rejected(state, state.queue[1].id)
  buffer.set(state.composer_buf, { "Lost in the restart" })
  submission.start(state, { context = context }, function() end)
  local third = requests[#requests]
  buffer.set(state.composer_buf, { "typed meanwhile" })
  state.host_error = "Provider restart failed"
  third.callback(nil, "Provider restart failed")
  eq(vim.tbl_map(function(entry) return { entry.text, entry.draft_state } end, state.queue),
    { { "Lost in the restart", "rejected" } })
end

return T
