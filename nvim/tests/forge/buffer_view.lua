vim.loader.enable(false)
local buffer = require("forge.buffer")
local session
local original_window = vim.api.nvim_get_current_win()
local original_buffer = vim.api.nvim_get_current_buf()
local second_window

local function metadata() return { target = {}, decoration = {}, editable_region = {}, fold = {} } end
local function block(id, text) return { id = id, text = { text }, metadata = metadata() } end
local function cursor_at(id, column, window)
  local _, row = session.sequence:position(id)
  vim.api.nvim_win_set_cursor(window or original_window, { row + 1, column or 0 })
end
local function assert_cursor(id, column, window)
  local cursor = vim.api.nvim_win_get_cursor(window or original_window)
  assert(session.sequence:locate(cursor[1] - 1).id == id, "cursor changed declaration identity")
  assert(cursor[2] == column, "cursor changed byte column")
end
local function patch(blocks)
  local inserted, text, changed, retained, retired = {}, {}, {}, {}, {}
  for _, value in ipairs(blocks) do
    inserted[#inserted + 1], retained[value.id] = value.id, true
    vim.list_extend(text, value.text)
    changed[#changed + 1] = { block = value.id, row_count = #value.text, metadata = value.metadata }
  end
  for id in pairs(session.block) do if not retained[id] then retired[#retired + 1] = id end end
  local result = buffer.apply_patch(session, {
    document = session.document, base = session.revision, next = session.revision + 1,
    base_rows = session.row_count, next_rows = #text, base_blocks = session.sequence:count(), next_blocks = #blocks,
    text_edit = { { start_row = 0, removed_rows = session.row_count, text = text } },
    block_edit = { { start_block = 0, removed_blocks = session.sequence:count(), inserted = inserted } },
    metadata_edit = changed, removed_block = retired,
  })
  assert(result.kind == "Applied", result.diagnostic)
end

local success, failure = xpcall(function()
  session = buffer.open("cursor", { preserve_view = true })
  local full, public = {}, {}
  for index = 1, 90 do
    local value = block(tostring(index), index == 60 and "pub struct Foo;" or "identical declaration text")
    full[#full + 1] = value
    if index < 20 or index > 22 then public[#public + 1] = value end
  end
  assert(buffer.apply_snapshot(session, { document = "cursor", revision = 0, block = full }).kind == "Applied")
  vim.api.nvim_win_set_buf(original_window, session.buffer)
  cursor_at("60", 6)
  vim.fn.winrestview({ lnum = 60, col = 6, topline = 53 })
  local saved = vim.fn.winsaveview()
  vim.cmd("split")
  second_window = vim.api.nvim_get_current_win()
  cursor_at("80", 3, second_window)
  vim.api.nvim_set_current_win(original_window)
  saved = vim.fn.winsaveview()
  patch(public)
  assert_cursor("60", 6)
  assert_cursor("80", 3, second_window)
  local projected = vim.fn.winsaveview()
  assert(projected.lnum - projected.topline == saved.lnum - saved.topline, "cursor moved inside viewport")
  patch(full)
  assert_cursor("60", 6)
  assert_cursor("80", 3, second_window)

  cursor_at("21", 2)
  patch(public)
  assert_cursor("19", 2)
  patch(full)
  cursor_at("22", 2)
  patch(public)
  assert_cursor("23", 2)

  cursor_at("60", 6)
  local replacement = { block("intro", "New header") }
  vim.list_extend(replacement, public)
  assert(buffer.apply_snapshot(session, {
    document = "cursor", revision = session.revision + 1, block = replacement,
  }).kind == "Applied")
  assert_cursor("60", 6)
  assert_cursor("80", 3, second_window)
  for _, value in ipairs(replacement) do if value.id == "60" then value.text = { "é" } end end
  patch(replacement)
  assert_cursor("60", 0)
  assert(buffer.apply_snapshot(session, {
    document = "cursor", revision = session.revision + 1, block = {},
  }).kind == "Applied")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(original_window), { 1, 0 }))
end, debug.traceback)

if second_window and vim.api.nvim_win_is_valid(second_window) then vim.api.nvim_win_close(second_window, true) end
if vim.api.nvim_win_is_valid(original_window) then
  vim.api.nvim_win_set_buf(original_window, original_buffer)
end
if session then buffer.close(session) end
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("buffer_view: passed")
vim.cmd("qa!")
