vim.loader.enable(false)
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local session
local function metadata(revision, text)
  return { target = {}, decoration = {}, editable_region = { { id = "composer", revision = revision,
    range = { start = { row = 0, column = 0 }, ["end"] = { row = #text - 1, column = #text[#text] } } } } }
end
local function open()
  session = buffer.open("composer", { editable = { delay = 1000, max_delay = 1000, send = function() return true end } })
  assert(buffer.apply_snapshot(session, { document = "composer", revision = 0, block = {
    { id = "body", text = { "submitted" }, metadata = metadata(0, { "submitted" }) },
  } }).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
end
local function patch(base, region_revision, old_rows, text)
  return { document = "composer", base = base, next = base + 1, base_rows = old_rows, next_rows = #text,
    base_blocks = 1, next_blocks = 1, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 0, removed_rows = old_rows, text = text } },
    metadata_edit = { { block = "body", row_count = #text, metadata = metadata(region_revision, text) } } }
end
local function ack(request, revision, update)
  return buffer.acknowledge_edit(session, { document = "composer", region = "composer",
    sequence = request.sequence, revision = revision }, update)
end
local ok, failure = xpcall(function()
  for _, acknowledgement_first in ipairs({ false, true }) do
    open()
    vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 9, { "newer", "draft" })
    local request = editable.take_pending(session.editable, "composer")
    local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
    local accepted = patch(1, 2, 1, { "newer", "draft" })
    local cleared = patch(0, 1, 1, { "" })
    local result
    if acknowledgement_first then
      assert(ack(request, 2, accepted).kind == "Deferred")
      result = buffer.apply_patch(session, cleared)
    else
      assert(buffer.apply_patch(session, cleared).kind == "Deferred")
      result = ack(request, 2, accepted)
    end
    assert(result.kind == "Applied", vim.inspect(result))
    assert(session.revision == 2 and session.editable.region.composer.revision == 2)
    assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick, "older clear rewrote newer draft")
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "newer", "draft" }))
    buffer.close(session)
  end
  open()
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 9, { "accepted" })
  local request = editable.take_pending(session.editable, "composer")
  assert(buffer.apply_patch(session, patch(1, 2, 1, { "" })).kind == "Deferred")
  local result = ack(request, 1, patch(0, 1, 1, { "accepted" }))
  assert(result.kind == "Applied", vim.inspect(result))
  assert(session.revision == 2 and session.editable.region.composer.revision == 2)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "" }),
    "clear admitted after exact local edit was suppressed")
  buffer.close(session)
  open()
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 9, { "unsent draft" })
  assert(buffer.apply_patch(session, patch(0, 1, 1, { "" })).kind == "Deferred")
  request = editable.take_pending(session.editable, "composer")
  assert(request.base == 1, "unsent draft retained pre-clear base")
  result = ack(request, 2, patch(1, 2, 1, { "unsent draft" }))
  assert(result.kind == "Applied", vim.inspect(result))
  buffer.close(session)
  open()
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 9, { "newer than rollback" })
  request = editable.take_pending(session.editable, "composer")
  local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
  assert(ack(request, 3, patch(2, 3, 1, { "newer than rollback" })).kind == "Deferred")
  assert(buffer.apply_patch(session, patch(1, 2, 1, { "submitted" })).kind == "Deferred")
  result = buffer.apply_patch(session, patch(0, 1, 1, { "" }))
  assert(result.kind == "Applied", vim.inspect(result))
  assert(session.revision == 3 and session.editable.region.composer.revision == 3)
  assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick, "rollback erased later typing")
end, debug.traceback)
if session then buffer.close(session) end
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("composer_interleave OK")
vim.cmd("qa!")
