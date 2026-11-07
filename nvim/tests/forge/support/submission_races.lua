return function(context)
  local state, controller, client = context.state, context.controller, context.client
  local input = require("forge.views.harness.input_state")
  local original_request, original_receive = client.request, input.receive
  local provider_errors = {}
  local unsubscribe = client.session_events_subscribe(function(event, payload)
    if event == "provider_event" and payload.kind == "error" then
      provider_errors[#provider_errors + 1] = payload.text
    end
  end)
  local command = require("forge.command")
  local original_dispatch = command.dispatch
  -- Editor drafts the host has not yet acknowledged; acknowledged input lives in `confirmed`.
  local function unacknowledged()
    local count = 0
    for _, draft in ipairs(state.input.drafts) do
      if draft.draft_state == "pending" then count = count + 1 end
    end
    return count
  end
  local function await(predicate, label)
    assert(vim.wait(10000, predicate, 10), label .. ": " .. vim.inspect({
      drafts = state.input.drafts, confirmed = state.input.confirmed,
      busy = require("forge.views.harness.activity").busy(state), errors = context.errors,
    }))
  end
  local function settled()
    await(function()
      return not require("forge.views.harness.activity").busy(state) and unacknowledged() == 0
    end, "submission did not settle")
  end
  local function exchanges()
    return context.request("session.exchange.list", {})
  end
  local function assert_exchange_count(prompt, expected)
    local count = 0
    for _, exchange in ipairs(exchanges()) do
      if exchange.prompt == prompt then count = count + 1 end
    end
    assert(count == expected, prompt .. " appeared in " .. count .. " exchanges, expected " .. expected)
  end
  local function assert_once(prompt) assert_exchange_count(prompt, 1) end
  local function find_message(id)
    for _, message in ipairs(state.input.confirmed) do
      if message.id == id then return message end
    end
  end
  local function find_draft(id)
    for _, draft in ipairs(state.input.drafts) do
      if draft.id == id then return draft end
    end
  end
  -- Admission rejection reaches the user through the Harness message notification, not a provider event.
  local function expect_attachment_failure(previous_errors)
    await(function() return #context.errors > previous_errors end, "missing attachment failure was not reported")
    assert(#provider_errors == 0, "admission rejection was reported as a provider error")
    while #context.errors > previous_errors do
      local diagnostic = table.remove(context.errors)
      assert(diagnostic:find("attachment is unavailable", 1, true), diagnostic)
    end
  end
  local success, failure = xpcall(function()
    for _, outcome in ipairs({ "active", "complete", "cancel" }) do
      local prompt = "Late acknowledgement after " .. outcome
      local held = context.mock.hold(state.session.id, "chat")
      local delayed, operation
      client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local parameters = request_specification.params or {}

        if method == "session.input.apply" and parameters.action.kind == "submit" and not operation then
          operation = vim.deepcopy(parameters)
          return original_request(request_specification, function(...)
            local arguments = { n = select("#", ...), ... }
            delayed = function() callback(unpack(arguments, 1, arguments.n)) end
          end)
        end
        return original_request(request_specification, callback)
      end
      context.compose_and_press(prompt, "<C-s>")
      assert(held:wait().prompt == prompt)
      await(function() return delayed ~= nil end, "admission acknowledgement did not arrive")
      local message_id = operation.action.message.id
      await(function()
        local message = find_message(message_id)
        return message and message.delivery == "accepted"
      end, "provider acceptance was not projected")
      vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Newer unsent draft " .. outcome })
      if outcome == "complete" then held:release() settled()
      elseif outcome == "cancel" then controller.cancel_turn() settled() end
      delayed()
      delayed()
      client.request = original_request
      context.request("session.input.apply", operation)
      -- Cancelling an output-free turn retracts its exchange; the late replay must not revive it.
      assert_exchange_count(prompt, outcome == "cancel" and 0 or 1)
      assert(#state.queue == 0 and #state.pending_submit == 0 and #state.pending_steer == 0,
        "late or duplicate admission revived an accepted message")
      assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "Newer unsent draft " .. outcome,
        "late acknowledgement overwrote newer typing")
      if outcome == "active" then held:release() settled() end
    end

    -- Lose the host's submit response beneath the shared command reconciler, so recovery must read
    -- the command journal instead of resending the mutation.
    local lost_hold = context.mock.hold(state.session.id, "chat")
    local lost_operation, submissions, lookups = nil, 0, 0
    local prior_errors = #context.errors
    input.receive = function(owner, update)
      if owner == state and update.operation_id == lost_operation and lookups == 0 then return end
      return original_receive(owner, update)
    end
    command.dispatch = function(transport, session_id, method, params, callback, owned_command)
      local function lossy(target_session, target_method, request, receive, sending)
        if target_method == "session.input.apply" and request.action.kind == "submit" then
          submissions = submissions + 1
          lost_operation = request.id
          return transport(target_session, target_method, request, function()
            receive(nil, "Injected acknowledgement loss", { code = "outcome_unknown" })
          end, sending)
        elseif target_method == "session.command.status.get" then
          lookups = lookups + 1
        end
        return transport(target_session, target_method, request, receive, sending)
      end
      return original_dispatch(lossy, session_id, method, params, callback, owned_command)
    end
    context.compose_and_press("Recover a lost admission acknowledgement", "<C-s>")
    lost_hold:wait()
    await(function() return lookups >= 1 and unacknowledged() == 0 end,
      "missing acknowledgement was not reconciled through the command journal")
    command.dispatch, input.receive = original_dispatch, original_receive
    assert(submissions == 1, "unknown delivery was resubmitted instead of reconciled")
    assert(#context.errors == prior_errors, "a reconciled acknowledgement was reported as a failure: "
      .. table.concat(context.errors, "\n"))
    lost_hold:release()
    settled()
    assert_once("Recover a lost admission acknowledgement")

    local held = context.mock.hold(state.session.id, "chat")
    local dropped, delayed_snapshot
    input.receive = function(owner, update)
      if owner == state and not dropped and update.upsert and update.upsert[1]
        and update.upsert[1].delivery == "delivering" then
        dropped = vim.deepcopy(update)
        return
      end
      return original_receive(owner, update)
    end
    client.request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local parameters = request_specification.params or {}

      if method == "session.input.get" and not delayed_snapshot then
        return original_request(request_specification, function(...)
          local arguments = { n = select("#", ...), ... }
          delayed_snapshot = function() callback(unpack(arguments, 1, arguments.n)) end
        end)
      end
      return original_request(request_specification, callback)
    end
    context.compose_and_press("Recover a missing input delta", "<C-s>")
    assert(held:wait().prompt == "Recover a missing input delta")
    await(function()
      return dropped and unacknowledged() == 0 and #state.pending_submit == 0
    end, "complete publication did not recover the missing input event")
    input.refresh(state)
    await(function() return delayed_snapshot ~= nil end, "explicit snapshot did not arrive")
    held:release()
    await(function() return not require("forge.views.harness.activity").busy(state) end, "provider did not finish during snapshot recovery")
    await(function() return #state.input.confirmed == 0 end, "completed input did not leave the queue")
    local completed_revision = state.input.revision
    client.request, input.receive = original_request, original_receive
    delayed_snapshot()
    assert(not state.input.refreshing and state.input.revision == completed_revision,
      "stale snapshot replaced a newer publication")
    settled()
    original_receive(state, dropped)
    assert(#state.queue == 0 and #state.pending_submit == 0 and #state.pending_steer == 0,
      "reordered input delta revived a delivered prompt")
    assert_once("Recover a missing input delta")

    -- The host validates retained attachments before admission, so an unavailable attachment never
    -- enters the host queue. Its text stays recoverable as an editor-owned rejected draft, which is
    -- never eligible for delivery and therefore cannot hold later input behind it.
    local previous_errors, before = #context.errors, #exchanges()
    local failed_id = input.submit(state, "Unavailable attachment [File #1]", {
      { id = string.rep("0", 64), name = "missing.txt", media_type = "text/plain",
        label = "[File #1]", start = 23, ["end"] = 32 },
    }, "auto")
    await(function() local draft = find_draft(failed_id) return draft and draft.draft_state == "rejected" end,
      "attachment rejection did not remain recoverable")
    expect_attachment_failure(previous_errors)
    assert(not find_message(failed_id) and #exchanges() == before, "an unavailable attachment was admitted")
    local follower = context.mock.hold(state.session.id, "chat")
    context.compose_and_press("Proceeds past a rejected draft", "<C-s>")
    assert(follower:wait().prompt == "Proceeds past a rejected draft")
    follower:release()
    settled()
    assert_once("Proceeds past a rejected draft")
    assert(find_draft(failed_id).draft_state == "rejected", "later input discarded the rejected draft")
    input.retry(state, find_draft(failed_id))
    await(function()
      local draft = find_draft(failed_id)
      return draft and draft.draft_state == "rejected" and unacknowledged() == 0
    end, "retry did not settle on the same rejected draft")
    expect_attachment_failure(previous_errors)
    assert(#exchanges() == before + 1, "a rejected retry dispatched input")
    input.discard_rejected(state, failed_id)
    assert(not find_draft(failed_id) and #state.queue == 0, "discarding the rejected draft left it queued")

    local active = context.mock.hold(state.session.id, "chat")
    context.compose_and_press("Finish while the next prompt is reserved", "<C-s>")
    active:wait()
    context.compose_and_press("Reserved follow-up", "<C-q>")
    await(function() return unacknowledged() == 0 and #state.queue == 1 end, "queued edit target missing")
    local reserved_id = state.queue[1].id
    controller.edit_queued(-1)
    await(function() return unacknowledged() == 0 and find_message(reserved_id).reservation end,
      "edit reservation was not acknowledged")
    local reserved = vim.deepcopy(find_message(reserved_id))
    active:release()
    await(function() return not require("forge.views.harness.activity").busy(state) end, "active response did not finish")
    assert(find_message(reserved_id).reservation == reserved.reservation,
      "turn completion consumed a reserved prompt")
    local edited = context.mock.hold(state.session.id, "chat")
    vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "Edited after turn completion" })
    controller.submit()
    assert(edited:wait().prompt == "Edited after turn completion")
    local stale = context.request("session.input.apply", { id = "stale-reservation-commit", action = {
      kind = "commit", reservation = reserved.reservation, edits = {
        { id = reserved.id, revision = reserved.revision, text = "Must never replace accepted text", attachments = {} },
      },
    } })
    assert(stale.error and stale.error ~= vim.NIL, "stale reservation commit was accepted")
    edited:release()
    settled()
    assert_once("Edited after turn completion")
    local repeated_active = context.mock.hold(state.session.id, "chat")
    context.compose_and_press("Hold while submitting identical text twice", "<C-s>")
    repeated_active:wait()
    for _ = 1, 2 do context.compose_and_press("Intentionally repeated prompt", "<C-q>") end
    await(function() return unacknowledged() == 0 and #state.queue == 2 end,
      "identical text was deduplicated before delivery")
    local first_id, second_id = state.queue[1].id, state.queue[2].id
    assert(first_id ~= second_id, "separate submissions reused a message identity")
    local first_repeat = context.mock.hold(state.session.id, "chat")
    local second_repeat = context.mock.hold(state.session.id, "chat")
    repeated_active:release()
    assert(first_repeat:wait().prompt == "Intentionally repeated prompt")
    first_repeat:release()
    assert(second_repeat:wait().prompt == "Intentionally repeated prompt")
    second_repeat:release()
    settled()
    local repeated_ids = {}
    for _, exchange in ipairs(exchanges()) do
      if exchange.prompt == "Intentionally repeated prompt" then repeated_ids[#repeated_ids + 1] = exchange.input_message_id end
    end
    assert(vim.deep_equal(repeated_ids, { first_id, second_id }),
      "identical text was deduplicated or delivered out of order")
    local primary = context.mock.hold(state.session.id, "chat")
    context.compose_and_press("Primary session remains active", "<C-s>")
    primary:wait()
    local secondary_id = context.request("session.new", { name = "Independent submission lane" }).session.id
    local secondary = context.mock.hold(secondary_id, "chat")
    local secondary_finished, secondary_error
    local secondary_deadline = vim.uv.hrtime() + 10e9
    local function submit_secondary()
      original_request({ session_id = secondary_id, method = "session.prompt.submit", params = { text = "Independent secondary request" } }, function(_, diagnostic, detail)
        if detail and detail.code == "not_admitted" and vim.uv.hrtime() < secondary_deadline then
          vim.defer_fn(submit_secondary, 10)
          return
        end
        secondary_finished, secondary_error = true, diagnostic
      end)
    end
    submit_secondary()
    assert(secondary:wait().prompt == "Independent secondary request")
    secondary:release()
    await(function() return secondary_finished end, "another session waited for the primary response")
    assert(not secondary_error, secondary_error)
    assert(require("forge.views.harness.activity").busy(state), "secondary completion cleared the primary session's activity: "
      .. vim.inspect({ status = state.status, session = state.session and state.session.id }))
    assert(primary:wait().prompt == "Primary session remains active",
      "secondary completion changed primary execution ownership")
    for _, exchange in ipairs(exchanges()) do
      assert(exchange.prompt ~= "Independent secondary request", "secondary transcript leaked into the primary session")
    end
    primary:release()
    settled()
    assert(#context.errors == previous_errors, table.concat(context.errors, "\n"))
    print("submission_races: late/duplicate/lost acknowledgements, cancellation, missing events, stale snapshots, failed retries, blocking, reservations, independent sessions passed")
  end, debug.traceback)
  client.request, input.receive, command.dispatch = original_request, original_receive, original_dispatch
  unsubscribe()
  assert(success, failure)
end
