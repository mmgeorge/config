vim.loader.enable(false)
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local existing = vim.api.nvim_create_buf(false, true)
local owner, replacement
local function snapshot(document, text)
  return { document = document, revision = 0, block = {
    { id = "body", text = text, metadata = { target = {}, decoration = {}, editable_region = {
      { id = "draft", revision = 0, range = { start = { row = 0, column = 0 },
        ["end"] = { row = #text - 1, column = #text[#text] } } },
    } } },
  } }
end
local function open(document)
  return buffer.open(document, { buffer = existing, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(existing),
    editable = { delay = 1000, max_delay = 1000, send = function() return true end } })
end
local ok, failure = xpcall(function()
  owner = open("old-generation")
  assert(buffer.apply_snapshot(owner, snapshot(owner.document, { "draft" })).kind == "Applied")
  vim.bo[existing].modifiable = true
  vim.api.nvim_buf_set_name(existing, "ForgeTransferFixture")
  local name = vim.api.nvim_buf_get_name(existing)
  local tick = vim.api.nvim_buf_get_changedtick(existing)
  buffer.close(owner, { preserve_buffer = true })
  assert(owner.status == "Closed" and vim.api.nvim_buf_is_valid(existing))
  assert(vim.api.nvim_buf_get_changedtick(existing) == tick and vim.api.nvim_buf_get_name(existing) == name)
  assert(vim.bo[existing].modifiable and not vim.b[existing].forge_native_document)
  owner = open("collected-generation")
  assert(buffer.apply_snapshot(owner, snapshot(owner.document, { "draft" })).kind == "Applied")
  vim.bo[existing].modifiable = true
  vim.api.nvim_buf_set_text(existing, 0, 0, 0, 5, { "newer", "🙂draft" })
  local pending = editable.take_pending(owner.editable, "draft")
  assert(pending, owner.editable.fault or "native edit did not enter admission")
  tick = vim.api.nvim_buf_get_changedtick(existing)
  assert(buffer.close(owner, { preserve_buffer = true }).kind == "Deferred")
  assert(owner.editable.native, "ordinary close detached unacknowledged source")
  buffer.invalidate(owner)
  assert(owner.status == "Closed" and owner.editable.native == nil)
  assert(vim.api.nvim_buf_get_changedtick(existing) == tick)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(existing, 0, -1, true), { "newer", "🙂draft" }))
  replacement = open("replacement-generation")
  assert(buffer.apply_snapshot(replacement, snapshot(replacement.document, { "newer", "🙂draft" })).kind == "Applied")
  local replacement_tick = vim.api.nvim_buf_get_changedtick(existing)
  assert(buffer.apply_snapshot(owner, snapshot(owner.document, { "stale" })).kind == "Closed")
  assert(buffer.apply_snapshot_part(owner, {}).kind == "Closed")
  assert(buffer.apply_patch(owner, {}).kind == "Closed")
  assert(buffer.acknowledge_edit(owner, { sequence = pending.sequence }, {}).kind == "Closed")
  buffer.invalidate(owner)
  assert(vim.b[existing].forge_native_document and replacement.editable.native)
  assert(vim.api.nvim_buf_get_changedtick(existing) == replacement_tick, "old owner mutated replacement source")
end, debug.traceback)
if replacement then buffer.invalidate(replacement) end
if owner then buffer.invalidate(owner) end
if vim.api.nvim_buf_is_valid(existing) then vim.api.nvim_buf_delete(existing, { force = true }) end
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("buffer_transfer OK")
vim.cmd("qa!")
