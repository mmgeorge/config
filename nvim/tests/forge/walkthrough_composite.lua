vim.loader.enable(false)
local source = require("forge.source_document")
local notices, deferred = {}, nil
local state
local function snapshot(document, rows, target)
  return { document = document, revision = 1, block = {
    { id = "body", text = rows, metadata = { decoration = {}, editable_region = {}, target = target and {
      { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = #rows, column = 0 } } },
    } or {} } },
  } }
end
package.loaded["forge.client"] = { request_host = function(_, params, callback)
  if params.operation == "open" then
    callback({ inventory_state = "disabled", snapshot = snapshot(params.document, { "Annotated change" }, "change") })
  elseif params.operation == "annotation_source" then
    deferred = callback
  else callback(vim.NIL) end
end }
source._set_runner_for_test(function(_, params, callback)
  if params.operation == "change" then
    assert(params.review == true)
    callback({ title = "source.rs", object = "captured", revision = "captured", source_row = 1, more = false, review = true,
      state = { state = "ready" }, snapshot = snapshot(params.document, { "first source line", "second source line" }),
      annotation = snapshot(params.annotation_document, { "changed source", "╭──────────╮", "│ annotation │", "╰──────────╯" }, "source-coordinate") })
  else callback({}) end
end)
local walkthrough = require("forge.walkthrough")
local function settled()
  assert(vim.wait(1000, function() return not state.opening and not state.pending and #state.queue == 0 end))
end
local function key(value)
  local mapping = vim.fn.maparg(value, "n", false, true)
  assert(mapping.callback, "missing command " .. value)
  mapping.callback()
end
local success, failure = xpcall(function()
  state = walkthrough.open({ on_error = function(message) notices[#notices + 1] = message end })
  settled()
  walkthrough.open_change(state)
  settled()
  local owner = state.source[1]
  assert(owner.review and owner.source.active and owner.annotation)
  assert(#vim.api.nvim_list_wins() == 2, "composite opened extra source/annotation panes")
  assert(vim.api.nvim_get_current_buf() == owner.annotation.buffer)
  assert(#vim.fn.win_findbuf(owner.source.replica.buffer) == 0, "exact source remained visible beside composite")
  key("o")
  assert(deferred)
  deferred({ source_row = 1 })
  deferred = nil
  settled()
  assert(vim.api.nvim_get_current_buf() == owner.source.replica.buffer)
  assert(vim.api.nvim_win_get_cursor(0)[1] == 2, "source coordinate was reconstructed from generated rows")
  key("q")
  settled()
  assert(vim.api.nvim_get_current_buf() == owner.annotation.buffer and owner.source.active)
  key("o")
  assert(deferred)
  vim.cmd("enew")
  local unrelated = vim.api.nvim_get_current_buf()
  deferred({ source_row = 0 })
  deferred = nil
  settled()
  assert(vim.api.nvim_get_current_buf() == unrelated, "late source action stole unrelated buffer")
  vim.api.nvim_win_set_buf(owner.window, owner.annotation.buffer)
  settled()
  key("q")
  settled()
  assert(not owner.active and not owner.source.active)
  assert(#vim.api.nvim_list_wins() == 1)
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)
if state then walkthrough.close(state) end
source._set_runner_for_test(nil)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("walkthrough_composite OK")
vim.cmd("qa!")
