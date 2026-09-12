vim.loader.enable(false)
local replica = require("forge.buffer")
local editable = require("forge.editable")
local session

local function metadata()
  return { target = {}, decoration = {}, editable_region = {} }
end

local ok, failure = xpcall(function()
  session = replica.open("large", { editable = { send = function() return true end } })
  local blocks = {}
  for index = 1, 10000 do
    blocks[index] = { id = tostring(index), text = { "body" }, metadata = metadata() }
  end
  blocks[10000].metadata.editable_region = {
    { id = "editor", revision = 0, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 4 } } },
  }
  assert(replica.apply_snapshot(session, { document = "large", revision = 0, block = blocks }).kind == "Applied")
  local native = session.editable.native
  local patch = {
    document = "large", base = 0, next = 1, base_rows = 10000, next_rows = 10001,
    base_blocks = 10000, next_blocks = 10000,
    text_edit = { { start_row = 0, removed_rows = 1, text = { "body", "added" } } },
    block_edit = {}, removed_block = {},
    metadata_edit = { { block = "1", row_count = 2, metadata = metadata() } },
  }
  session.sequence.visits = 0
  local calls, original = 0, vim.api.nvim_buf_get_lines
  vim.api.nvim_buf_get_lines = function(...)
    calls = calls + 1
    return original(...)
  end
  local result = replica.apply_patch(session, patch)
  vim.api.nvim_buf_get_lines = original
  assert(result.kind == "Applied", result.diagnostic)
  assert(session.sequence.visits < 150, "patch traversed unrelated block order")
  assert(calls < 10, "patch read unrelated document text")
  assert(session.editable.native == native, "patch reattached native editing")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 10000, 0, 10000, 4, { "typed" })
  assert(not session.editable.fault, session.editable.fault)
  assert(vim.deep_equal(editable.recoverable_text(session.editable, "editor"), { "typed" }))
  editable.detach(session.editable)
  session.editable = editable.new(session.document)
  replica.close(session)

  session = replica.open("invalid")
  local source = { id = "body", text = { "text" }, metadata = metadata() }
  assert(replica.apply_snapshot(session, { document = "invalid", revision = 0, block = { source } }).kind == "Applied")
  local invalid = {
    document = "invalid", base = 0, next = 1, base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
    text_edit = {}, block_edit = {}, removed_block = {},
    metadata_edit = { { block = "body", row_count = 2, metadata = metadata() } },
  }
  assert(not pcall(replica.preflight, session, invalid))
  assert(session.sequence:rows() == 1 and session.sequence.node.body.entry.row_count == 1,
    "failed validation changed live block ownership")
  assert(session.revision == 0 and vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true)[1] == "text")
end, debug.traceback)

if session then
  editable.detach(session.editable)
  session.editable = editable.new(session.document)
  replica.close(session)
end
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
