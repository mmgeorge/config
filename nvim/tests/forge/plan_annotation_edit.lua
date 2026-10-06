vim.loader.enable(false)
local client = require("forge.client")
local original_request, original_accepting = client.request_for, client.host_accepting
client.host_accepting = function() return true end
local requests = {}
client.request_for = function(_, method, params, callback)
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "# Canonical plan" }, path)
vim.cmd("edit " .. vim.fn.fnameescape(path))
local native_buffer = vim.api.nvim_get_current_buf()
local owner
local success, failure = xpcall(function()
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = native_buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
  }, function(_, error_message) assert(not error_message, error_message) end)
  local id = requests[1].params.document
  requests[1].callback({ path = path, version = 1, saved_source_digest = "saved", annotation = {},
    source_row = { { id = "canonical", target = "canonical", text = "# Canonical plan", source_line = 1,
      block = "canonical", position = { row = 0, column = 0 }, metadata = {} } },
    snapshot = { document = id, revision = 0,
      block = { { id = "canonical", text = { "# Canonical plan" }, metadata = { target = {}, decoration = {}, editable_region = {} } } } },
  })
  owner.action("comment", function(result, error_message) assert(result and not error_message, error_message) end)
  vim.cmd("stopinsert")
  local body_row = vim.api.nvim_win_get_cursor(0)[1] - 1
  local literal = "literal ** comment"
  vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { literal, "second line" })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
  owner.submit("plan.request_changes", { comment = "Review" }, function() end)
  assert(#requests == 2 and requests[2].method == "plan.request_changes", "submission did not capture local annotations")
  local annotation = requests[2].params.draft_annotation
  assert(#annotation == 1 and annotation[1].source.body == literal .. "\nsecond line",
    "review submission changed annotation bytes")
  local tick = vim.api.nvim_buf_get_changedtick(native_buffer)
  requests[2].callback({ submitted = true })
  assert(vim.api.nvim_buf_get_changedtick(native_buffer) == tick, "submission acknowledgement rewrote local typing")
  vim.cmd("write")
  assert(#requests == 3 and requests[3].params.operation == "plan_save_annotations")
  assert(requests[3].params.annotation[1].source.body == literal .. "\nsecond line")
  requests[3].callback({ saved = true })
  assert(not vim.bo[native_buffer].modified)
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Canonical plan" }), "saving annotations overwrote canonical Markdown")
  assert(owner.close())
end, debug.traceback)
if owner then owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
vim.fn.delete(path)
assert(success, failure)
print("plan_annotation_edit: passed")
