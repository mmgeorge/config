vim.loader.enable(false)
local source = require("forge.source_document")
local closed = false
local captured = { document = "transcript", sequence = 2, revision = 4 }
source._set_runner_for_test(function(method, params, callback)
  if params.operation == "open_diff" then
    assert(method == "harness.document" and params.input == captured and not params.text)
    callback({ title = "Saved changes", object = "exact-source", revision = "saved", more = false,
      snapshot = { document = params.document, revision = 0, block = { { id = "diff", text = { "--- a/path", "+++ b/path" },
        metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
  else
    assert(method == "source.document")
    if params.operation == "close" then closed = true end
    callback({})
  end
end)
local owner
local success, failure = xpcall(function()
  owner = source.open_harness_diff({ session_id = "session", input = captured,
    window = vim.api.nvim_get_current_win(), is_current = function() return true end, on_error = error })
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == owner.replica.buffer end))
  assert(vim.api.nvim_buf_get_lines(owner.replica.buffer, 0, -1, false)[2] == "+++ b/path")
  source.close(owner)
  assert(vim.wait(1000, function() return closed end))
end, debug.traceback)
if owner and owner.active then source.close(owner) end
source._set_runner_for_test(nil)
assert(success, failure)
print("harness_saved_diff: passed")
