vim.loader.enable(false)
local replica = require("forge.buffer")
local editable = require("forge.editable")
local sent = {}
local session = replica.open("edit-capture", { editable = {
  send = function(request) sent[#sent + 1] = request end,
} })
local function metadata(region, revision, text)
  return { target = {}, decoration = {}, editable_region = region and {
    { id = region, revision = revision, range = { start = { row = 0, column = 0 },
      ["end"] = { row = #text - 1, column = #text[#text] } } },
  } or {} }
end
local function snapshot(revision, body, other)
  return { document = session.document, revision = revision, block = {
    { id = "header", text = { "read only" }, metadata = metadata() },
    { id = "body", text = body, metadata = metadata("body", revision, body) },
    { id = "other", text = other, metadata = metadata("other", revision, other) },
    { id = "footer", text = { "tail" }, metadata = metadata() },
  } }
end
assert(replica.apply_snapshot(session, snapshot(0, { "body" }, { "other" })).kind == "Applied")
vim.bo[session.buffer].modifiable = true
vim.api.nvim_buf_set_text(session.buffer, 1, 0, 1, 4, { "first", "line" })
local first = editable.capture_draft(session.editable)
vim.api.nvim_buf_set_text(session.buffer, 2, 4, 2, 4, { " newer" })
vim.api.nvim_buf_set_text(session.buffer, 3, 0, 3, 5, { "other final" })
local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
editable.saved_capture(session.editable, first)
assert(first[1].text == "first\nline")
assert(replica.apply_snapshot(session, snapshot(1, { "first", "line" }, { "other" })).kind == "Deferred")
assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick)
local body = editable.capture_draft(session.editable, { body = true })
assert(body[1].base == 1 and body[1].text == "first\nline newer")
editable.saved_capture(session.editable, body)
assert(editable.suspend_generated_text(session.editable), "body save cleared another field")
local other = editable.capture_draft(session.editable, { other = true })
assert(other[1].base == 0 and other[1].text == "other final")
editable.saved_capture(session.editable, other)
assert(not editable.suspend_generated_text(session.editable))
assert(replica.apply_snapshot(session, snapshot(2, { "first", "line newer" }, { "other final" })).kind == "Applied")
vim.bo[session.buffer].modifiable = true
vim.api.nvim_buf_set_text(session.buffer, 2, 10, 2, 10, { "!" })
local latest = editable.capture_draft(session.editable)
assert(#latest == 1 and latest[1].base == 2 and latest[1].text == "first\nline newer!")
assert(replica.apply_snapshot(session, snapshot(3, { "stale" }, { "other final" })).kind == "Deferred")
assert(vim.api.nvim_buf_get_lines(session.buffer, 2, 3, true)[1] == "line newer!")
assert(#sent == 0, "typing emitted an edit request")
editable.saved_capture(session.editable, latest)
replica.close(session)
print("local_edit_patch: independent explicit captures preserve native drafts")
