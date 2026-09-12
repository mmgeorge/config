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
  local function metadata(revision, length)
    return { target = {}, decoration = {}, editable_region = { { id = "note", revision = revision,
      range = { start = { row = 1, column = 0 }, ["end"] = { row = 1, column = length } } } } }
  end
  requests[1].callback({ path = path, version = 1, saved_source_digest = "saved", snapshot = { document = id, revision = 0,
    block = { { id = "note", text = { "Comment", "" }, metadata = metadata(0, 0) } } } })
  vim.api.nvim_win_set_cursor(0, { 2, 0 })
  local literal = "literal ** comment"
  vim.api.nvim_buf_set_text(native_buffer, 1, 0, 1, 0, { literal })
  owner.submit("plan.request_changes", { comment = "Review" }, function() end)
  assert(#requests == 2 and requests[2].params.operation == "plan_edit", "submission bypassed saved annotation acknowledgement")
  local edit = requests[2].params.edit
  local tick = vim.api.nvim_buf_get_changedtick(native_buffer)
  requests[2].callback({ accepted = true, acknowledgement = { document = id, region = "note", sequence = edit.sequence, revision = 1 },
    patch = { document = id, base = 0, next = 1, base_rows = 2, next_rows = 2, base_blocks = 1, next_blocks = 1,
      block_edit = {}, removed_block = {}, text_edit = { { start_row = 0, removed_rows = 2, text = { "Comment", literal } } },
      metadata_edit = { { block = "note", row_count = 2, metadata = metadata(1, #literal) } } } })
  assert(vim.api.nvim_buf_get_changedtick(native_buffer) == tick, "annotation acknowledgement rewrote local typing")
  assert(#requests == 3 and requests[3].method == "plan.request_changes")
  assert(requests[3].params.review.revision == 1 and requests[3].params.annotations == nil,
    "review submission copied comment bodies through Lua")
  vim.cmd("write")
  assert(not vim.bo[native_buffer].modified)
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Canonical plan" }), "saving annotations overwrote canonical Markdown")
  assert(owner.close())
end, debug.traceback)
if owner then owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
vim.fn.delete(path)
assert(success, failure)
print("plan_annotation_edit: passed")
