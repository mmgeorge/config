vim.loader.enable(false)
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local function metadata(text, revision)
  return { target = {}, decoration = {}, editable_region = {
    { id = "title", revision = revision, range = { start = { row = 0, column = 0 },
      ["end"] = { row = #text - 1, column = #text[#text] } } },
  } }
end
local session = buffer.open("mixed-capture", { editable = {} })
local block = { { id = "title", text = { "Original" }, metadata = metadata({ "Original" }, 0) } }
for index = 1, 10000 do
  block[#block + 1] = { id = "source:" .. index, text = { "untouched" },
    metadata = { target = {}, decoration = {}, editable_region = {} } }
end
assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = block }).kind == "Applied")
vim.bo[session.buffer].modifiable = true
vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 8, { "Captured", "title" })
local captured = editable.capture_draft(session.editable)
vim.api.nvim_buf_set_text(session.buffer, 1, 5, 1, 5, { " newer" })
local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
local patch = { document = session.document, base = 0, next = 1, base_rows = 10001, next_rows = 10001,
  base_blocks = 10001, next_blocks = 10001, block_edit = {}, removed_block = {},
  text_edit = { { start_row = 5000, removed_rows = 1, text = { "generated" } } }, metadata_edit = {} }
for index = 1, 300 do
  local deferred = vim.deepcopy(patch)
  deferred.base, deferred.next = index - 1, index
  assert(buffer.apply_patch(session, deferred).kind == "Deferred")
end
editable.saved_capture(session.editable, captured)
assert(buffer.apply_patch(session, patch).kind == "Deferred")
assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick)
assert(captured[1].text == "Captured\ntitle")
local latest = editable.capture_draft(session.editable)
assert(latest[1].text == "Captured\ntitle newer")
editable.saved_capture(session.editable, latest)
block[1] = { id = "title", text = { "Captured", "title newer" }, metadata = metadata({ "Captured", "title newer" }, 2) }
assert(buffer.apply_snapshot(session, { document = session.document, revision = 2, block = block }).kind == "Applied")
patch.base, patch.next, patch.base_rows, patch.next_rows = 2, 3, 10002, 10002
patch.text_edit[1].start_row = 5001
local untouched = session.block["source:9000"]
local attachment = session.editable.native
local get_lines, broad_read = vim.api.nvim_buf_get_lines, false
vim.api.nvim_buf_get_lines = function(buf, start, finish, strict)
  if buf == session.buffer and (finish == -1 or finish - start > 1024) then broad_read = true end
  return get_lines(buf, start, finish, strict)
end
local result = buffer.apply_patch(session, patch)
vim.api.nvim_buf_get_lines = get_lines
assert(result.kind == "Applied", vim.inspect(result))
assert(not broad_read, "generated patch copied unrelated source rows")
assert(session.block["source:9000"] == untouched and session.editable.native == attachment)
local malformed = vim.deepcopy(patch)
malformed.base, malformed.next = 3, 4
malformed.text_edit[1].start_row = 99999
local before = get_lines(session.buffer, 0, -1, true)
assert(buffer.apply_patch(session, malformed).kind == "Desynchronized")
assert(vim.deep_equal(get_lines(session.buffer, 0, -1, true), before))
buffer.close(session)
print("mixed_edit_patch: draft deferral, exact captures, bounded clean updates, and malformed patch isolation passed")
