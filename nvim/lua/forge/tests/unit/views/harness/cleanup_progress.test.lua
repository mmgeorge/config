local MiniTest = require("mini.test")
local T = MiniTest.new_set({ hooks = dofile("nvim/lua/forge/tests/support/isolation.lua").hooks() })
local eq = MiniTest.expect.equality

T["request_lifetime_is_independent_of_timeout_notice"] = function()
  local progress = require("forge.views.harness.cleanup_progress")
  local state = { runtime_epoch = "host-one" }
  progress.begin(state, "cancel-one")
  eq(progress.pending(state), true)
  eq(progress.respond(state, "cancel-one", "host-one"), true)
  progress.timeout(state, "cancel-one")
  eq(progress.pending(state), false)
  eq(state.cleanup_operation_id, "cancel-one")
  eq(state.cleanup_notice, "Exceeded cancellation timeout")
  progress.begin(state, "cancel-two")
  progress.receive(state, { operation_id = "cancel-one", state = "pending", message = "old progress" })
  eq(state.cleanup_operation_id, "cancel-two")
  progress.settled(state, "cancel-one")
  eq(state.cleanup_operation_id, "cancel-two")
end

T["old_host_callbacks_cannot_retire_new_host_request"] = function()
  local progress = require("forge.views.harness.cleanup_progress")
  local state = { runtime_epoch = "host-one" }
  progress.begin(state, "cancel-one")
  state.runtime_epoch = "host-two"
  eq(progress.pending(state), false)
  progress.begin(state, "cancel-two")
  eq(progress.respond(state, "cancel-one", "host-one"), false)
  eq(progress.pending(state), true)
  progress.settled(state)
  eq(progress.pending(state), false)
end

T["authoritative_progress_survives_timeout_and_ignores_older_revisions"] = function()
  local progress = require("forge.views.harness.cleanup_progress")
  local state = { runtime_epoch = "host-one" }
  progress.begin(state, "cancel-one")
  local recording = vim.json.decode(table.concat(vim.fn.readfile(
    "nvim/lua/forge/tests/fixtures/protocol/cancellation-progress.json"), "\n"))
  progress.apply(state, recording)
  eq(state.cleanup_notice, "Cancellation pending - draining host work (response timed out)")
  progress.apply(state, { operation_id = "cancel-one", revision = 1, phase = "collecting" })
  eq(state.cancellation.phase, "draining")
  progress.receive(state, { operation_id = "cancel-one", state = "pending", message = "Cancellation still pending" })
  eq(state.cleanup_notice, "Cancellation pending - draining host work (response timed out)")
  progress.apply(state, { operation_id = "cancel-one", revision = 4, phase = "settled" })
  eq(state.cleanup_notice, nil)
end

T["prompt_cancellation_adds_no_notice_until_it_is_reported_slow"] = function()
  local progress = require("forge.views.harness.cleanup_progress")
  local state = { runtime_epoch = "host-one" }
  progress.begin(state, "cancel-one")
  -- The status row already reads `Stopping`; a notice per phase showed four texts in one second.
  for revision, phase in ipairs({ "acquiring", "collecting", "draining", "settling" }) do
    progress.apply(state, { operation_id = "cancel-one", revision = revision, phase = phase })
    eq(state.cancellation.phase, phase)
    eq(state.cleanup_notice, nil)
  end
  -- Once the owner reports the stop as slow, the notice names the boundary still open.
  progress.receive(state, { operation_id = "cancel-one", state = "pending", message = "Cancellation still pending" })
  eq(state.cleanup_notice, "Cancellation pending - committing settlement")
  progress.apply(state, { operation_id = "cancel-one", revision = 5, phase = "recovering" })
  eq(state.cleanup_notice, "Cancellation pending - recovering provider")
  progress.apply(state, { operation_id = "cancel-one", revision = 6, phase = "settled" })
  eq(state.cleanup_notice, nil)
  -- The report belonged to the settled operation, so the next stop starts without a notice.
  progress.apply(state, { operation_id = "cancel-two", revision = 7, phase = "collecting" })
  eq(state.cleanup_notice, nil)
end

return T
