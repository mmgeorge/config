local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["publication_replay"] = function()
  local client = require("forge.client")
  local snapshot = require("forge.views.harness.snapshot")
  local builder = require("forge.builder")
  local received = {}
  local emit
  builder.ensure = function(callback) callback({ ok = true, path = "publication-fixture" }) end
  local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  MiniTest.finally(function() pcall(client._host_reset_for_test) end)
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
        elseif request.method == "host.trace.configure" then
          emit({ id = request.id, result = { level = "none" } })
        elseif request.method == "host.shutdown" then
          vim.schedule(function() on_exit({ code = 0 }) end)
        end
      end,
      kill = function() end,
    }
  end)
  local function event(sequence, name, payload, session_id)
    payload = payload or { revision = sequence }
    payload._publication = { sequence = sequence }
    return { session_id = session_id or "replay", event = name or "input_changed", payload = payload }
  end
  local function page(state, revision, high_water, events)
    local entries = {}
    for _, message in ipairs(events or {}) do
      entries[#entries + 1] = { sequence = message.payload._publication.sequence, event = message }
    end
    return { session = state.session, runtime_epoch = state.runtime_epoch, snapshot_revision = revision,
      owner_publication = { receipt_id = "page-" .. revision, high_water = high_water,
        after = state.publication_cursor, through = high_water, more = false, events = entries } }
  end
  local ready = false
  client.host_start(function(_, error) assert(not error) ready = true end)
  wait_until(function() return ready end, "mock host did not finish starting")
  client.session_events_subscribe(function(name, payload, session_id)
    received[#received + 1] = { name = name, sequence = payload._publication and payload._publication.sequence,
      session_id = session_id }
  end)
  local state = { session = { id = "replay" }, runtime_epoch = "first", publication_cursor = 3 }
  local historical_error = event(4, "background_error", { error = "historical failure" })
  local initial_page = page(state, 1, 5, { historical_error })
  initial_page.owner_publication.more, initial_page.owner_publication.through = true, 4
  assert(snapshot.apply(state, initial_page))
  assert(#errors == 0 and #received == 0, "initial attachment replayed historical one-shot effects")
  assert(state.publication_cursor == 3, "consumption advanced the durable acknowledgement cursor")
  emit(event(6))
  wait_until(function() return #received == 1 end, "live publication 6 was not delivered")
  assert(snapshot.apply(state, page(state, 2, 5, { event(5) })))
  assert(#received == 1, "later initial page replayed history or hid a new live event")
  local completion = event(7, "command_completed", { id = "missed-command", revision = 1 })
  assert(snapshot.apply(state, page(state, 3, 7, { event(6), completion })))
  assert(#received == 2 and received[2].sequence == 7 and received[2].name == "command_completed",
    "live/page duplication or missed command completion recovery failed")
  emit(completion)
  emit(event(9))
  wait_until(function() return #received == 3 end, "live publication 9 was not delivered")
  assert(snapshot.apply(state, page(state, 4, 9, { event(8), event(9) })))
  assert(#received == 4 and received[4].sequence == 8, "out-of-order live event swallowed a missing publication")
  -- Runtime changes preserve the session-wide sequence; final canonical state wins over replay.
  local final_state_seen
  client.session_events_subscribe(function(name, payload)
    if name == "input_changed" and payload.revision == 10 then
      final_state_seen = state.runtime_epoch
      state.status = { kind = "replayed" }
    end
  end)
  local replacement = page(state, 1, 10, { event(9), event(10) })
  replacement.runtime_epoch, replacement.status = "replacement", { kind = "idle" }
  assert(snapshot.apply(state, replacement))
  assert(#received == 5 and final_state_seen == "first" and state.status.kind == "idle"
    and state.runtime_epoch == "replacement", "runtime replay did not precede its final canonical snapshot")
  -- A host generation change starts a fresh baseline without historical notifications.
  client._client.generation = client._client.generation + 1
  emit = function(message) client._client.receiver.options.dispatch(message) end
  assert(snapshot.apply(state, page(state, 2, 12, { event(11, "background_error", { error = "old host failure" }), event(12) })))
  assert(#received == 5 and #errors == 0, "new host replayed historical notifications")
  emit(event(13))
  wait_until(function() return #received == 6 end, "replacement host publication 13 was not delivered")
  assert(received[6].sequence == 13)
  local other = { session = { id = "other" }, runtime_epoch = "other" }
  assert(snapshot.apply(other, page(other, 1, 2, { event(2, nil, nil, "other") })))
  emit(event(3, nil, nil, "other"))
  wait_until(function() return #received == 7 end, "other session's publication 3 was not delivered")
  assert(received[7].session_id == "other", "sessions shared a publication sequence gate")
end

T["command_reconciliation"] = function()
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
  view.open(initial, host)
  keymap.invoke("<CR>")
  assert(#pending == 1 and view._state_for_test().state.page_index == 2, "answer waited for its acknowledgement before advancing")
  keymap.invoke("<CR>")
  assert(#pending == 2)
  keymap.invoke("y")
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

  local client = require("forge.client")
  local command = require("forge.command")
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
end

T["receive_initialize"] = function()
  local executable = require("forge.builder").binary_path()
  assert(vim.fn.executable(executable) == 1, "build the Forge host before running this fixture")

  local protocol = require("forge.protocol")
  local receive = require("forge.receive")
  local data_root = temp.directory("data")

  local function initialize_host(attempt)
    local process, initialized, exited = nil, false, nil
    local consumed_bytes, consumed_frames = 0, 0
    local receiver = receive.new({
      decode = protocol.decode_message,
      dispatch = function(message)
        if message.id == 1 then
          assert(not message.error, vim.inspect(message.error))
          initialized = true
          process:write(protocol.encode_request(2, "host.shutdown", {}))
          process:write(nil)
        end
      end,
      consumed = function(bytes, frames)
        consumed_bytes, consumed_frames = consumed_bytes + bytes, consumed_frames + frames
        process:write(protocol.encode_request(0, "host.transport.consumed", {
          bytes = consumed_bytes,
          frames = consumed_frames,
        }))
      end,
      finished = function(result) exited = result end,
    })
    process = vim.system({ executable }, {
      text = true,
      stdin = true,
      stdout = function(_, chunk)
        if chunk then receive.push(receiver, chunk) end
      end,
      stderr = function() end,
    }, function(result)
      receive.finish(receiver, result)
    end)
    process:write(protocol.encode_request(1, "host.initialize", {
      protocol_version = protocol.VERSION,
      recovery_directory = vim.fs.joinpath(data_root, "forge", "recovery", "github", "v1"),
      status_ignored_directory = vim.fs.joinpath(data_root, "forge", "status-ignored"),
    }))
    wait_until(function() return initialized and exited ~= nil end,
      "receive lifecycle did not finish on attempt " .. attempt, { timeout_ms = 3000 })
    assert(exited.code == 0, "receive lifecycle exited unexpectedly: " .. vim.inspect(exited))
  end

  for attempt = 1, 10 do initialize_host(attempt) end
end

return T
