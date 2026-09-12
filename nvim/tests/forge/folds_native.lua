vim.loader.enable(false)
local replica = require("forge.buffer")
local input = require("forge.input")
local session = replica.open("folds")
local function block(id, text, fold)
  return { id = id, text = text, metadata = { target = {}, decoration = {}, editable_region = {}, fold = fold or {} } }
end
local function fold(id, endpoint)
  return { id = id, start = { row = 0, column = 0 },
    ["end"] = { block = endpoint, position = { row = 2, column = 0 } }, closed = true }
end
local source = { document = "folds", revision = 0, block = {
  block("first", { "first", "body" }, { fold("first-fold", "first") }),
  block("second", { "second", "tail" }, { fold("second-fold", "second") }),
} }
source.block[1].metadata.gutter = { { position = { row = 1, column = 0 },
  chunk = { { text = "1 │ ", capture = "LineNr" } }, priority = 100 } }
local ok, failure = xpcall(function()
  assert(replica.apply_snapshot(session, source).kind == "Applied")
  vim.api.nvim_set_current_buf(session.buffer)
  vim.wo.foldmethod = "manual"
  vim.wo.winhighlight = "Normal:NormalFloat,Folded:ErrorMsg"
  local view = input.open(session, 0)
  assert(vim.wo.winhighlight == "Normal:NormalFloat,Folded:Normal", "fold highlight differs from Normal")
  assert(vim.fn.foldclosed(1) == 1 and vim.fn.foldclosedend(1) == 2, "first native fold differs")
  assert(vim.fn.foldtextresult(1) == "first", "fold summary added count or filler text")
  assert(vim.fn.foldclosed(3) == 3 and vim.fn.foldclosedend(3) == 4, "adjacent native folds merged")
  local marks = vim.api.nvim_buf_get_extmarks(session.buffer, session.namespace, 0, -1, { details = true })
  local found = false
  for _, mark in ipairs(marks) do
    if mark[4].virt_text then
      found = mark[2] == 1 and mark[4].virt_text_pos == "inline" and mark[4].virt_text[1][1] == "1 │ "
    end
  end
  assert(found, "native gutter missing")
  local replacement = vim.deepcopy(source.block[1].metadata)
  replacement.fold[1]["end"].position.row = 3
  local patch = { document = session.document, base = 0, next = 1, base_rows = 4, next_rows = 5,
    base_blocks = 2, next_blocks = 2, block_edit = {}, removed_block = {},
    text_edit = { { start_row = 0, removed_rows = 2, text = { "first", "body", "inserted" } } },
    metadata_edit = { { block = "first", row_count = 3, metadata = replacement } },
  }
  assert(replica.apply_patch(session, patch).kind == "Applied")
  assert(vim.fn.foldclosed(4) == 4 and vim.fn.foldclosedend(4) == 5, "patch lost shifted native fold intent")
  local first_metadata = vim.deepcopy(replacement)
  first_metadata.fold[1]["end"].position.row = 4
  local second_metadata = vim.deepcopy(source.block[2].metadata)
  second_metadata.fold[1]["end"].position.row = 3
  local multiple = { document = session.document, base = 1, next = 2, base_rows = 5, next_rows = 7,
    base_blocks = 2, next_blocks = 2, block_edit = {}, removed_block = {},
    text_edit = {
      { start_row = 3, removed_rows = 2, text = { "second", "tail", "added tail" } },
      { start_row = 0, removed_rows = 3, text = { "first", "body", "inserted", "another" } },
    },
    metadata_edit = { { block = "first", row_count = 4, metadata = first_metadata },
      { block = "second", row_count = 3, metadata = second_metadata } },
  }
  assert(replica.apply_patch(session, multiple).kind == "Applied")
  assert(vim.fn.foldclosed(5) == 5 and vim.fn.foldclosedend(5) == 7, "disjoint patch lost native fold intent")
  vim.cmd("normal! zR")
  vim.api.nvim_win_set_cursor(0, { 5, 1 })
  local event = assert(input.capture(session, view, "toggle"))
  assert(event.block == "second" and event.position.row == 0 and event.position.column == 1)
  local invalid = vim.deepcopy(source)
  invalid.revision = 3
  invalid.block[1].metadata.fold[1]["end"].block = "missing"
  assert(replica.apply_snapshot(session, invalid).kind == "Desynchronized")
  assert(vim.api.nvim_buf_get_lines(session.buffer, 0, 1, true)[1] == "first", "invalid fold changed text")
  source.revision = 3
  assert(replica.apply_snapshot(session, source).kind == "Applied")
  vim.cmd("normal! zM")
  assert(vim.fn.foldclosed(1) == 1 and vim.fn.foldclosedend(1) == 2, "recovered first fold differs")
  assert(vim.fn.foldclosed(3) == 3 and vim.fn.foldclosedend(3) == 4, "recovered second fold differs")
  input.close(view)
  assert(vim.wo.foldmethod == "manual", "fold ownership did not restore window options")
  assert(vim.wo.winhighlight == "Normal:NormalFloat,Folded:ErrorMsg", "fold ownership did not restore highlights")

  local late = replica.open("late-folds")
  assert(replica.apply_snapshot(late, { document = "late-folds", revision = 0,
    block = { block("late", { "file.rs", "loading" }) } }).kind == "Applied")
  vim.api.nvim_set_current_buf(late.buffer)
  local late_view = input.open(late, 0)
  local metadata = block("late", {}, { fold("late-fold", "late") }).metadata
  local late_patch = { document = "late-folds", base = 0, next = 1, base_rows = 2, next_rows = 2,
    base_blocks = 1, next_blocks = 1, block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = "late", row_count = 2, metadata = metadata } } }
  assert(replica.apply_patch(late, late_patch).kind == "Applied")
  assert(vim.fn.foldclosed(1) == 1, "asynchronous fold ignored its default collapsed state")
  vim.cmd("normal! zo")
  late_patch.base, late_patch.next = 1, 2
  late_patch.text_edit = { { start_row = 1, removed_rows = 1, text = { "source" } } }
  assert(replica.apply_patch(late, late_patch).kind == "Applied")
  assert(vim.fn.foldclosed(1) == -1, "body update overwrote the user's expanded fold intent")
  input.close(late_view)
  replica.close(late)
end, debug.traceback)
replica.close(session)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
