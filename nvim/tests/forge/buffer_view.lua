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
local function patch(blocks, text_edit)
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
    text_edit = text_edit or { { start_row = 0, removed_rows = session.row_count, text = text } },
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

  local function status_rows()
    local value = block("status", "Working · 447s")
    value.text = { "Working · 447s", "Adding slow LSP requirement", "", "Queued follow-up inputs", "• queued input" }
    return value
  end
  local escape = vim.api.nvim_replace_termcodes("<Esc>", true, false, true)
  local block_mode = vim.api.nvim_replace_termcodes("<C-v>", true, false, true)
  local block_select = vim.api.nvim_replace_termcodes("<C-s>", true, false, true)
  local function select_range(mode, first, last)
    vim.cmd.normal({ args = { escape }, bang = true })
    vim.api.nvim_win_set_cursor(original_window, first)
    local visual = ({ s = "v", S = "V", [block_select] = block_mode })[mode] or mode
    vim.cmd.normal({ args = { visual }, bang = true })
    vim.api.nvim_win_set_cursor(original_window, last)
    if visual ~= mode then vim.cmd.normal({ args = { "\7" }, bang = true }) end
  end
  local function assert_selection(mode, first, last)
    assert(vim.fn.mode(1) == mode, "selection mode changed")
    local anchor = vim.fn.getpos("v")
    assert(vim.deep_equal({ anchor[2], anchor[3] - 1 }, first),
      "selection anchor moved: " .. vim.inspect(anchor))
    assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0), last), "selection cursor moved")
  end
  for _, mode in ipairs({ "v", "V", block_mode, "s", "S", block_select }) do
    for _, reverse in ipairs({ false, true }) do
      patch({ status_rows() })
      local first, last = { 1, 0 }, { 1, 6 }
      if reverse then first, last = last, first end
      select_range(mode, first, last)
      patch({ status_rows() })
      assert_selection(mode, first, last)
      assert(buffer.apply_snapshot(session, {
        document = "cursor", revision = session.revision + 1,
        block = { block("prefix", "Earlier output"), status_rows() },
      }).kind == "Applied")
      assert_selection(mode, { 2, first[2] }, { 2, last[2] })
      local growing = status_rows()
      growing.text[#growing.text + 1] = "• another queued input"
      patch({ block("prefix", "Earlier output"), growing }, {
        { start_row = 1, removed_rows = 5, text = growing.text },
      })
      assert_selection(mode, { 2, first[2] }, { 2, last[2] })
      if mode == "v" or mode == "s" then
        assert(vim.deep_equal(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = "v" }),
          { "Working" }), "queued rows entered the selected text")
      end
      vim.cmd.normal({ args = { escape }, bang = true })
    end
  end
  local selection = vim.o.selection
  vim.o.selection = "exclusive"
  patch({ block("status", "Working") })
  select_range("v", { 1, 0 }, { 1, 7 })
  patch({ block("status", "Working") })
  assert_selection("v", { 1, 0 }, { 1, 7 })
  vim.cmd.normal({ args = { escape }, bang = true })
  vim.o.selection = selection
  patch({ status_rows() })
  select_range("v", { 1, 0 }, { 1, 6 })
  session.before_commit = function()
    select_range("v", { 2, 3 }, { 2, 8 })
    session.before_commit = nil
  end
  patch({ status_rows() })
  assert_selection("v", { 2, 3 }, { 2, 8 })
  vim.cmd.normal({ args = { escape }, bang = true })
  local tabs = block("status", "\tWorking")
  tabs.text[2] = "\tQueued"
  patch({ tabs })
  local virtualedit = vim.wo.virtualedit
  vim.wo.virtualedit = "all"
  vim.fn.setpos(".", { 0, 1, 1, 3 })
  vim.cmd.normal({ args = { block_mode }, bang = true })
  vim.fn.setpos(".", { 0, 2, 1, 5 })
  local anchor_before, cursor_before = vim.fn.getpos("v"), vim.fn.getpos(".")
  patch({ tabs })
  assert(vim.deep_equal(vim.fn.getpos("v"), anchor_before), "block anchor virtual column changed")
  assert(vim.deep_equal(vim.fn.getpos("."), cursor_before), "block cursor virtual column changed")
  vim.cmd.normal({ args = { escape }, bang = true })
  vim.wo.virtualedit = virtualedit
  patch({ status_rows() })
  select_range("v", { 2, 20 }, { 5, 10 })
  local shortened = block("status", "é")
  patch({ shortened })
  assert_selection("v", { 1, 0 }, { 1, 0 })
  assert(buffer.apply_snapshot(session, {
    document = "cursor", revision = session.revision + 1, block = {},
  }).kind == "Applied")
  assert_selection("v", { 1, 0 }, { 1, 0 })
  patch({ status_rows() })
  assert_selection("v", { 1, 0 }, { 1, 0 })
  vim.cmd.normal({ args = { escape }, bang = true })
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
