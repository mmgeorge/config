vim.loader.enable(false)
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local function snapshot(revision, text)
  return { document = "composer", revision = revision, block = {
    { id = "body", text = text, metadata = { target = {}, decoration = {}, editable_region = {
      { id = "composer", revision = revision, range = { start = { row = 0, column = 0 },
        ["end"] = { row = #text - 1, column = #text[#text] } } },
    } } },
  } }
end
for _, completion_first in ipairs({ false, true }) do
  local session = buffer.open("composer", { editable = {} })
  assert(buffer.apply_snapshot(session, snapshot(0, { "submitted" })).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 9, { "accepted" })
  local captured = editable.capture_draft(session.editable)
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 8, { "newer", "draft" })
  local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
  if completion_first then editable.saved_capture(session.editable, captured) end
  assert(buffer.apply_snapshot(session, snapshot(1, { "" })).kind == "Deferred")
  if not completion_first then editable.saved_capture(session.editable, captured) end
  assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "newer", "draft" }))
  assert(captured[1].text == "accepted")
  assert(editable.capture_draft(session.editable)[1].base == 1)
  editable.saved_capture(session.editable, editable.capture_draft(session.editable))
  assert(buffer.apply_snapshot(session, snapshot(2, { "newer", "draft" })).kind == "Applied")
  assert(buffer.apply_snapshot(session, snapshot(3, { "" })).kind == "Applied")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "" }))
  buffer.close(session)
end
print("composer_interleave: completion and generated refresh preserve later typing")
