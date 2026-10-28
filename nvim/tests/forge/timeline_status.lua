vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local buffer = require("forge.buffer")
local health = require("forge.views.harness.health")
local spinner = require("forge.render.harness.timeline_status")
local commands = require("forge.shared.view_command_set").new()
local transcript_buffer, transcript_window, composer_buffer, composer_window = require("forge.views.harness.layout").open("status-transitions")
local transcript = buffer.open("status-transitions", { buffer = transcript_buffer, generated = true,
  expected_changedtick = vim.api.nvim_buf_get_changedtick(transcript_buffer), preserve_view = true })
assert(buffer.apply_snapshot(transcript, { document = transcript.document, revision = 0, block = {} }).kind == "Applied")
local notices = {}
local status = require("forge.views.harness.status_view").new({ transcript = transcript,
  transcript_window = transcript_window, composer_window = composer_window, alive = function() return true end,
  notice = function(message) notices[#notices + 1] = message end, recover = function() end })
status:render({}, commands)
local revision = 0
local function publish(text, animated)
  revision = revision + 1
  local result
  status:apply({ document = transcript.document .. ":status", revision = revision, block = {
    { id = "status", text = { text }, metadata = { target = {}, decoration = {}, editable_region = {},
      status = { row = 0, animated = animated } } },
  } }, function(ok) result = ok end)
  assert(vim.wait(3000, function() return result ~= nil end, 1) and result)
end
local function mark()
  return vim.api.nvim_buf_get_extmark_by_id(status.buffer, status.animation_namespace, 1, { details = true })
end
assert(spinner.frame_at(0) == "⠋" and spinner.frame_at(120) == "⠙")
for _, phase in ipairs({ "Working", "Planning", "Revising plan", "Implementing", "Verifying", "Resolving", "Saving exchange", "Waiting for 2 agents" }) do
  publish(phase, true)
  assert(status.timer and mark()[3].virt_text[1][1] ~= "◷")
end
for _, phase in ipairs({ "Paused", "Waiting for your answer", "Waiting for plan review", "Planning stopped", "Saving exchange failed" }) do
  publish(phase, false)
  assert(status.timer == nil and mark()[3].virt_text[1][1] == "◷")
end
publish("Working", true)
local state = { status = { kind = "working" } }
status:render({ status_notice = { text = "Waiting for your approval", waiting = true, animated = false } }, commands)
assert(status.timer == nil and mark()[3].virt_text[1][1] == "◷")
assert(table.concat(status.content.lines):find("Waiting for your approval", 1, true))
state.connection_error = "Waiting for provider or tool · 30s without update"
status:render({ status_notice = health.notice(state) }, commands)
assert(table.concat(status.content.lines):find("30s without update", 1, true))
state.host_error = "Stopped · lost connection"
status:render({ status_notice = health.notice(state) }, commands)
assert(status.timer == nil)
status:render({}, commands)
local first = mark()[3].virt_text[1][1]
local text = vim.api.nvim_buf_get_lines(status.buffer, 0, -1, false)
local calls = { clear = 0, read = 0 }
local clear, read = vim.api.nvim_buf_clear_namespace, vim.api.nvim_buf_get_lines
vim.api.nvim_buf_clear_namespace = function(...) calls.clear = calls.clear + 1 return clear(...) end
vim.api.nvim_buf_get_lines = function(...) calls.read = calls.read + 1 return read(...) end
local advanced = vim.wait(600, function() return mark()[3].virt_text[1][1] ~= first end, 10)
vim.api.nvim_buf_clear_namespace, vim.api.nvim_buf_get_lines = clear, read
assert(advanced and calls.clear == 0 and calls.read == 0)
assert(vim.deep_equal(text, read(status.buffer, 0, -1, false)))
local result
status:apply({ document = transcript.document .. ":status", revision = revision + 1, block = {
  { id = "bad", text = { "bad" }, metadata = { target = {}, decoration = {}, editable_region = {}, status = { row = 2, animated = true } } },
} }, function(ok) result = ok end)
assert(vim.wait(3000, function() return result ~= nil end, 1) and not result)
assert(vim.deep_equal(text, read(status.buffer, 0, -1, false)))
status:close()
buffer.close(transcript)
vim.api.nvim_buf_delete(composer_buffer, { force = true })
print("timeline_status: passed")
