vim.loader.enable(false)
local buffer = require("forge.buffer")
local hint = require("forge.views.harness.status_hint")
local health = require("forge.views.harness.health")
local spinner = require("forge.render.harness.timeline_status")
local commands = require("forge.shared.view_command_set").new()
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusHint")
local transcript = buffer.open("timeline-status-transitions", {})
vim.api.nvim_set_current_buf(transcript.buffer)
local revision = 0
local function publish(text, animated, context)
  revision = revision + 1
  local block = { id = "status", text = { text }, metadata = {
    target = {}, decoration = {}, editable_region = {},
    status = { row = 0, animated = animated, hint = context },
  } }
  assert(buffer.apply_snapshot(transcript, {
    document = transcript.document, revision = revision, block = { block },
  }).kind == "Applied")
  hint.render(transcript, commands, 100)
end
local function mark()
  return vim.api.nvim_buf_get_extmark_by_id(transcript.buffer, namespace, 1, { details = true })
end
assert(spinner.frame_at(0) == "⠋" and spinner.frame_at(120) == "⠙")
for _, activity in ipairs({ "Working", "Planning", "Revising plan", "Implementing", "Verifying", "Resolving", "Saving exchange", "Waiting for 2 agents" }) do
  publish(activity, true)
  assert(mark()[3].sign_text, activity .. " omitted its spinner")
end
for _, activity in ipairs({ "Paused", "Waiting for your answer", "Waiting for plan review", "Planning stopped", "Saving exchange failed" }) do
  publish(activity, false)
  assert(#mark() == 0, activity .. " retained a spinner")
end
publish("Planning", true, "working")
local state = { busy = true, status = { kind = "working" }, approval = { {} } }
transcript.status_notice = health.notice(state)
hint.render(transcript, commands, 100)
assert(mark()[3].virt_text[1][1]:find("Waiting for your approval", 1, true))
assert(not mark()[3].sign_text)
state.approval = {}
state.wait_notice = "Waiting for provider or tool · 30s without update"
transcript.status_notice = health.notice(state)
hint.render(transcript, commands, 100)
assert(mark()[3].sign_text and mark()[3].virt_text[1][1]:find("30s without update", 1, true))
state.execution_notice = "Stopped · lost connection"
transcript.status_notice = health.notice(state)
hint.render(transcript, commands, 100)
assert(not mark()[3].sign_text and mark()[3].virt_text[1][2] == "ForgeHarnessToolFailure")
state.execution_notice, state.wait_notice, transcript.status_notice = nil, nil, nil
publish("Saving exchange", true)
local before_text = vim.api.nvim_buf_get_lines(transcript.buffer, 0, -1, false)
local before_cursor = vim.api.nvim_win_get_cursor(0)
local before_view = vim.fn.winsaveview()
local first_frame = mark()[3].sign_text
local calls = { clear = 0, read = 0, scan = 0 }
local original_clear = vim.api.nvim_buf_clear_namespace
local original_read = vim.api.nvim_buf_get_lines
local original_at = transcript.sequence.at
vim.api.nvim_buf_clear_namespace = function(...) calls.clear = calls.clear + 1 return original_clear(...) end
vim.api.nvim_buf_get_lines = function(...) calls.read = calls.read + 1 return original_read(...) end
transcript.sequence.at = function(...) calls.scan = calls.scan + 1 return original_at(...) end
local advanced = vim.wait(600, function() return mark()[3].sign_text ~= first_frame end, 10)
vim.api.nvim_buf_clear_namespace, vim.api.nvim_buf_get_lines = original_clear, original_read
transcript.sequence.at = original_at
assert(advanced, "spinner stopped advancing")
assert(calls.clear == 0 and calls.read == 0 and calls.scan == 0, "spinner tick rescanned or cleared presentation")
assert(vim.deep_equal(before_text, vim.api.nvim_buf_get_lines(transcript.buffer, 0, -1, false)))
assert(vim.deep_equal(before_cursor, vim.api.nvim_win_get_cursor(0)))
assert(vim.deep_equal(before_view, vim.fn.winsaveview()))
vim.bo[transcript.buffer].modifiable = true
vim.api.nvim_buf_set_lines(transcript.buffer, 0, 0, true, { "Local insertion" })
vim.bo[transcript.buffer].modifiable = false
vim.wait(260, function() return false end, 20)
assert(mark()[1] == 1, "animation moved the status back to its old row")
publish("Idle", false)
assert(#mark() == 0)
local before = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
vim.wait(260, function() return false end, 20)
assert(vim.deep_equal(before, vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })))
-- Reject malformed status coordinates before committing any text.
local outcome = buffer.apply_snapshot(transcript, {
  document = transcript.document, revision = revision + 1, block = {
    { id = "bad", text = { "bad" }, metadata = { target = {}, decoration = {}, editable_region = {},
      status = { row = 2, animated = true } } },
  },
})
assert(outcome.kind ~= "Applied")
assert(vim.api.nvim_buf_get_lines(transcript.buffer, 0, 1, false)[1] == "Idle")
hint.clear(transcript.buffer)
buffer.close(transcript)
print("timeline_status: passed")
vim.cmd("qa!")
