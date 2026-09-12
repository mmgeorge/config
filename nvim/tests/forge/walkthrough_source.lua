vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local source = require("forge.source_document")
local pending, closed, annotation = {}, {}, 0
source._set_runner_for_test(function(method, params, callback)
  if params.operation == "change" then assert(method == "walkthrough") pending[#pending + 1] = { params = params, callback = callback }
  else if params.operation == "close" then closed[params.document] = true end callback({}) end
end)
local origin = vim.api.nvim_get_current_buf()
local alive = true
local options = { workspace = vim.fn.getcwd(), input = { document = "walk", target = "change" }, annotation_document = "annotation",
  is_current = function() return alive end, on_annotation = function() annotation = annotation + 1 end }
local function deliver()
  local item = table.remove(pending, 1)
  item.callback({ title = "captured", object = "hash", revision = "captured", source_row = 1, annotation = {}, more = false,
    state = { state = "ready" }, snapshot = { document = item.params.document, revision = 1, block = {
      { id = "source", text = { "first", "second" }, metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
  return item.params.document
end
local ok, failure = xpcall(function()
  local first = source.open_walkthrough(options)
  alive = false
  local cancelled = deliver()
  assert(vim.wait(1000, function() return closed[cancelled] end))
  assert(annotation == 0 and vim.api.nvim_get_current_buf() == origin and not first.active)
  alive = true
  local second = source.open_walkthrough(options)
  deliver()
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == second.replica.buffer end))
  assert(annotation == 1 and vim.api.nvim_win_get_cursor(0)[1] == 2)
  assert(vim.bo[second.replica.buffer].readonly)
  source.close(second)
end, debug.traceback)
source._set_runner_for_test(nil)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("walkthrough_source OK")
vim.cmd("qa!")
