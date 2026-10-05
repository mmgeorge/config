vim.loader.enable(false)
local client = require("forge.client")
local pending = {}
local original_request = client.request_for
local original_generation = client.host_generation
local original_accepting = client.host_accepting
client.host_generation = function() return 1 end
client.host_accepting = function() return true end
client.request_for = function(_, method, params, callback)
  assert(method == "harness.document" or method == "plan.request_changes")
  pending[#pending + 1] = { method = method, params = vim.deepcopy(params), callback = callback }
end
local native_buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_name(native_buffer, vim.fn.tempname() .. ".md")
vim.api.nvim_win_set_buf(0, native_buffer)
vim.bo[native_buffer].bufhidden = "hide"
local owner
local ok, failure = xpcall(function()
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = native_buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
    notice = error,
  }, function(value, error_message) assert(not error_message, error_message) owner = value end)
  local source = {}
  for row = 1, 105 do
    source[row] = { id = "source:" .. row, target = "source:" .. row, text = "Source row " .. row, source_line = row,
      block = "plan:source", position = { row = row - 1, column = 0 }, metadata = {} }
  end
  local text = {}
  for _, row in ipairs(source) do text[#text + 1] = row.text end
  pending[1].callback({ saved_source_digest = "saved", annotation = {}, source_row = source,
    snapshot = { document = owner.document, revision = 0, block = {
      { id = "plan:source", text = text, metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } })
  vim.api.nvim_win_set_cursor(0, { 44, 0 })
  owner.action("comment", function() end)
  vim.wait(20)
  vim.cmd("stopinsert")
  assert(#pending == 1, "creating a local plan annotation sent a request")
  local cursor = vim.api.nvim_win_get_cursor(0)
  local body_row = cursor[1] - 1
  local heading = vim.api.nvim_buf_get_lines(native_buffer, body_row - 1, body_row, false)[1]
  local usable = vim.api.nvim_win_get_width(0) - vim.fn.getwininfo(vim.api.nvim_get_current_win())[1].textoff
  assert(vim.fn.strdisplaywidth(heading) < usable, "plan annotation heading exceeds the final gutter width")
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "first", "", "λ\r", "" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  local comments = require("forge.draft_comments")
  assert(comments.capture(native_buffer)[1].source.body == "first\n\nλ\r\n", "capture changed raw draft bytes")
  vim.cmd("write")
  assert(#pending == 2 and pending[2].params.operation == "plan_save_annotations")
  assert(pending[2].params.annotation[1].source.body == "first\n\nλ\r\n")
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 4, false, { "explicit second" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  vim.cmd("write")
  assert(#pending == 2, "concurrent save bypassed the explicit operation queue")
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "newer unsaved" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  pending[2].callback({ saved = true })
  assert(#pending == 3)
  assert(pending[3].params.annotation[1].source.body == "explicit second", "queued save recaptured later unsaved typing")
  pending[3].callback({ saved = true })
  assert(vim.bo[native_buffer].modified, "save completion cleared newer unsaved typing")
  assert(comments.capture(native_buffer)[1].source.body == "newer unsaved")
  vim.cmd("write")
  assert(#pending == 4)
  local submitted
  owner.submit("plan.request_changes", { comment = "revise" }, function(result, error_message)
    assert(not error_message, error_message)
    submitted = result
  end)
  assert(#pending == 4, "submission bypassed an active save")
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "typing after submit" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  pending[4].callback({ saved = true })
  assert(#pending == 5 and pending[5].method == "plan.request_changes")
  assert(pending[5].params.draft_annotation[1].source.body == "newer unsaved", "queued submission recaptured typing")
  pending[5].callback({ submitted = true })
  assert(submitted.submitted and vim.bo[native_buffer].modified)
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
  assert(#pending == 5, "collapse sent a request")
  local compact_row
  for row, line in ipairs(vim.api.nvim_buf_get_lines(native_buffer, 0, -1, false)) do
    if line:find("typing after submit", 1, true) then compact_row = row break end
  end
  vim.api.nvim_win_set_cursor(0, { assert(compact_row), 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
  assert(#pending == 5, "focus sent a request")
  assert(comments.capture(native_buffer)[1].source.body == "typing after submit")
  assert(owner.close() and not owner.closed, "closing collected an unsaved annotation")
  local alternate = vim.api.nvim_create_buf(false, true)
  vim.wo.statuscolumn = "%s"
  vim.api.nvim_win_set_buf(0, alternate)
  vim.api.nvim_win_set_buf(0, native_buffer)
  assert(vim.wo.number and vim.wo.statuscolumn == "", "reopening the draft inherited a status column without line numbers")
  assert(comments.capture(native_buffer)[1].source.body == "typing after submit", "hide and reopen lost the draft")
  owner.submit("plan.request_changes", { comment = "reentrant completion" }, function(result, error_message)
    assert(result and not error_message, error_message)
    vim.cmd("write")
  end)
  assert(#pending == 6)
  vim.cmd("write")
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "callback capture" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  pending[6].callback({ submitted = true })
  assert(#pending == 7, "completion callback dispatched overlapping explicit operations")
  assert(pending[7].params.annotation[1].source.body == "callback capture",
    "completion callback did not replace the older queued capture")
  pending[7].callback({ saved = true })
  assert(not vim.bo[native_buffer].modified)
end, debug.traceback)
client.request_for = original_request
client.host_generation = original_generation
client.host_accepting = original_accepting
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("plan_review_drafts: local creation/focus, exact bytes, final gutter, queued captures, newer typing, hide/reopen passed")
