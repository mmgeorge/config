vim.loader.enable(false)
local client = require("forge.client")
local snapshot = require("forge.views.harness.snapshot")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local original_ensure, original_error = builder.ensure, notifications.error
local errors, received = {}, {}
local emit
builder.ensure = function(callback) callback({ ok = true, path = "publication-fixture" }) end
notifications.error = function(message) errors[#errors + 1] = message end
client._host_reset_for_test()
client._host_set_launcher_for_test(function(_, options, on_exit)
  emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.initialize" then
        emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
      elseif request.method == "host.trace.configure" then
        emit({ id = request.id, result = { enabled = false } })
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
local function await(predicate)
  assert(vim.wait(1000, predicate, 5), "publication fixture timed out")
end
local succeeded, failure = xpcall(function()
  local ready = false
  client.host_start(function(_, error) assert(not error) ready = true end)
  await(function() return ready end)
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
  await(function() return #received == 1 end)
  assert(snapshot.apply(state, page(state, 2, 5, { event(5) })))
  assert(#received == 1, "later initial page replayed history or hid a new live event")
  local completion = event(7, "command_completed", { id = "missed-command", revision = 1 })
  assert(snapshot.apply(state, page(state, 3, 7, { event(6), completion })))
  assert(#received == 2 and received[2].sequence == 7 and received[2].name == "command_completed",
    "live/page duplication or missed command completion recovery failed")
  emit(completion)
  emit(event(9))
  await(function() return #received == 3 end)
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
  await(function() return #received == 6 end)
  assert(received[6].sequence == 13)
  local other = { session = { id = "other" }, runtime_epoch = "other" }
  assert(snapshot.apply(other, page(other, 1, 2, { event(2, nil, nil, "other") })))
  emit(event(3, nil, nil, "other"))
  await(function() return #received == 7 end)
  assert(received[7].session_id == "other", "sessions shared a publication sequence gate")
end, debug.traceback)
client._host_reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_publication_replay: passed")
vim.cmd("qa!")
