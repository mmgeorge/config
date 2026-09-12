vim.opt.runtimepath:append("nvim")
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local function metadata(id, revision, sequence, text)
  return { target = {}, decoration = {}, editable_region = id and { { id = id, revision = revision,
    sequence = sequence, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } or {} }
end
for _, acknowledgement_first in ipairs({ false, true }) do
  local session = buffer.open("restored-region", { editable = { delay = 1000, max_delay = 1000, send = function() return true end } })
  assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = {
    { id = "title", text = { "old" }, metadata = metadata("title", 0, 4, "old") },
    { id = "label", text = { "Comments" }, metadata = metadata() },
  } }).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 3, { "new" })
  local edit = assert(editable.take_pending(session.editable, "title"))
  assert(edit.sequence > 4)
  local generated = { document = session.document, base = 0, next = 1, base_rows = 2, next_rows = 3,
    base_blocks = 2, next_blocks = 3, removed_block = {},
    block_edit = { { start_block = 2, removed_blocks = 0, inserted = { "comment" } } },
    text_edit = { { start_row = 2, removed_rows = 0, text = { "restored" } } },
    metadata_edit = { { block = "comment", row_count = 1, metadata = metadata("comment", 8, 100, "restored") } } }
  local patch = { document = session.document, base = 1, next = 2, base_rows = 3, next_rows = 3,
    base_blocks = 3, next_blocks = 3, removed_block = {}, block_edit = {},
    text_edit = { { start_row = 0, removed_rows = 1, text = { "new" } } },
    metadata_edit = { { block = "title", row_count = 1, metadata = metadata("title", 1, edit.sequence, "new") } } }
  local acknowledgement = { document = session.document, region = "title", sequence = edit.sequence, revision = 1 }
  local result
  if acknowledgement_first then
    assert(buffer.acknowledge_edit(session, acknowledgement, patch).kind == "Deferred")
    result = buffer.apply_patch(session, generated)
  else
    assert(buffer.apply_patch(session, generated).kind == "Deferred")
    result = buffer.acknowledge_edit(session, acknowledgement, patch)
  end
  assert(result.kind == "Applied", vim.inspect(result))
  assert(session.editable.sequence >= 100)
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 2, 0, 2, 8, { "new reply" })
  local reply = assert(editable.take_pending(session.editable, "comment"))
  assert(reply.sequence > 100 and reply.base == 8)
  buffer.close(session)
end
print("restored_region_sequence: deferred region insertion seeds native sequence before further typing")
