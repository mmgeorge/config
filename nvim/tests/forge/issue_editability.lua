vim.opt.runtimepath:append("nvim")
local adapter = require("github.issue_document")
local editable = require("forge.editable")
local body, revision, sequence = "initial", 0, 0
local saved_capture
local function metadata(region)
  return { target = {}, decoration = {}, visible_decoration = {}, fold = {}, gutter = {},
    editable_region = region or {} }
end
local function snapshot(document)
  return { document = document, revision = revision, block = {
    { id = "heading", text = { "Generated heading" }, metadata = metadata() },
    { id = "region:body", text = { body }, metadata = metadata({
      { id = "body", revision = revision, sequence = sequence,
        range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #body } } },
    }) },
    { id = "footer", text = { "Generated footer" }, metadata = metadata() },
  } }
end
adapter._set_runner_for_test(function(method, params, callback)
  assert(method == "issue.document")
  if params.operation == "open" then
    callback({ snapshot = snapshot(params.document), fields = {
      { region = "body", revision = revision, sequence = sequence, baseline = body },
    } })
  elseif params.operation == "view" or params.operation == "close_view" then callback(vim.NIL)
  elseif params.operation == "save" then
    assert(not saved_capture, "typing sent more than the explicit save")
    saved_capture = vim.deepcopy(params.capture)
    body, sequence = saved_capture[1].text, saved_capture[1].sequence
    revision = revision + 1
    callback({ fields = {
      { region = "body", revision = revision, sequence = sequence, baseline = body, dirty = false },
    } })
  elseif params.operation == "snapshot" then callback(snapshot(params.document))
  elseif params.operation == "close" then callback({ collected = true })
  else error("unexpected issue operation: " .. params.operation) end
end)
local state = adapter.open({
  repository = { hostname = "github.com", owner = "owner", name = "repo" }, number = 7,
  on_error = error,
})
assert(vim.wait(1000, function() return state.shown end))
local native_buffer = state.replica.buffer
assert(not vim.bo[native_buffer].modifiable, "generated heading became editable")
vim.api.nvim_win_set_cursor(0, { 2, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
assert(vim.bo[native_buffer].modifiable, "moving onto the issue body did not enable editing")
vim.cmd("normal! ISp")
assert(vim.api.nvim_buf_get_lines(native_buffer, 1, 2, false)[1] == "Spinitial")
assert(not saved_capture, "native issue typing dispatched a request")
assert(editable.capture_draft(state.replica.editable)[1].text == "Spinitial")
vim.api.nvim_win_set_cursor(0, { 1, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
assert(not vim.bo[native_buffer].modifiable, "leaving the issue body did not protect generated rows")
vim.cmd("write")
assert(vim.wait(1000, function() return saved_capture and not state.saving end))
assert(saved_capture[1].text == "Spinitial")
assert(vim.api.nvim_buf_get_lines(native_buffer, 0, 1, false)[1] == "Generated heading")
assert(vim.api.nvim_buf_get_lines(native_buffer, 2, 3, false)[1] == "Generated footer")
assert(not vim.bo[native_buffer].modifiable, "save projection enabled editing on a generated row")
vim.api.nvim_win_set_cursor(0, { 2, 0 })
vim.api.nvim_exec_autocmds("CursorMoved", { buffer = native_buffer })
assert(vim.bo[native_buffer].modifiable, "save projection left the issue body read-only")
adapter.close(state)
adapter._set_runner_for_test(nil)
print("issue_editability: cursor-owned editing, protected generated rows, and native writes passed")
