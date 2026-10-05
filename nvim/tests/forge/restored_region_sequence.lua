vim.loader.enable(false)
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local function metadata(id, revision, sequence, text)
  return { target = {}, decoration = {}, editable_region = id and { { id = id, revision = revision,
    sequence = sequence, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #text } } } } or {} }
end
local session = buffer.open("restored-region", { editable = {} })
assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = {
  { id = "title", text = { "old" }, metadata = metadata("title", 0, 4, "old") },
  { id = "label", text = { "Comments" }, metadata = metadata() },
} }).kind == "Applied")
vim.bo[session.buffer].modifiable = true
vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 3, { "new" })
local captured = editable.capture_draft(session.editable)
assert(captured[1].sequence > 4)
local restored = { document = session.document, revision = 1, block = {
  { id = "title", text = { "new" }, metadata = metadata("title", 1, captured[1].sequence, "new") },
  { id = "label", text = { "Comments" }, metadata = metadata() },
  { id = "comment", text = { "restored" }, metadata = metadata("comment", 8, 100, "restored") },
} }
assert(buffer.apply_snapshot(session, restored).kind == "Deferred")
editable.saved_capture(session.editable, captured)
assert(buffer.apply_snapshot(session, restored).kind == "Applied")
assert(session.editable.sequence >= 100)
vim.bo[session.buffer].modifiable = true
vim.api.nvim_buf_set_text(session.buffer, 2, 0, 2, 8, { "new reply" })
local reply = editable.capture_draft(session.editable)
assert(#reply == 1 and reply[1].sequence > 100 and reply[1].base == 8)
assert(reply[1].text == "new reply")
editable.saved_capture(session.editable, reply)
buffer.close(session)
print("restored_region_sequence: restored regions seed capture sequences before typing")
