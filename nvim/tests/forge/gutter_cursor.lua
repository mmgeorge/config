vim.loader.enable(false)
local buffer = require("forge.buffer")
local commands = require("forge.document_commands")
local session = buffer.open("gutter-cursor")
local owner
local ok, failure = xpcall(function()
  assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = {
    { id = "source", text = { "", "return 2", "🙂" }, metadata = {
      decoration = {}, target = {}, editable_region = {}, gutter = {
        { position = { row = 0, column = 0 }, chunk = { { text = "1 │ ", capture = "LineNr" } }, priority = 100 },
        { position = { row = 1, column = 0 }, chunk = { { text = "2 │ ", capture = "LineNr" } }, priority = 100 },
        { position = { row = 2, column = 0 }, chunk = { { text = "3 │ ", capture = "LineNr" } }, priority = 100 },
      },
    } },
  } }).kind == "Applied")
  vim.api.nvim_set_current_buf(session.buffer)
  vim.wo.virtualedit = "all"
  owner = commands.attach(session, { view = "diff" })
  local tick = vim.api.nvim_buf_get_changedtick(session.buffer)
  for row = 1, 3 do
    vim.api.nvim_win_set_cursor(0, { row, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = session.buffer })
    local position = vim.fn.getcurpos()
    assert(position[2] == row and position[3] == 1 and position[4] == 4, vim.inspect(position))
  end
  vim.fn.setpos(".", { 0, 2, 40, 20 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = session.buffer })
  local position = vim.fn.getcurpos()
  assert(position[3] == 8 and position[4] == 0, "EOL retained virtual text: " .. vim.inspect(position))
  vim.fn.setpos(".", { 0, 3, 40, 20 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = session.buffer })
  position = vim.fn.getcurpos()
  assert(position[3] == 1, "UTF-8 cursor stopped in a continuation byte")
  vim.api.nvim_win_set_cursor(0, { 2, 0 })
  vim.fn.maparg("W", "n", false, true).callback()
  position = vim.fn.getcurpos()
  assert(position[3] == 1 and position[4] == 0, "gutter selection retained cursor offset")
  assert(session.gutter_selection.first == 2 and session.gutter_selection.last == 2)
  vim.api.nvim_win_set_cursor(0, { 3, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = session.buffer })
  assert(session.gutter_selection.first == 2 and session.gutter_selection.last == 3,
    "visible gutter selection did not follow the cursor")
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  vim.api.nvim_exec_autocmds("ModeChanged", {})
  position = vim.fn.getcurpos()
  assert(session.gutter_selection == nil, "gutter highlight ownership survived selection exit")
  assert(position[3] == 1 and position[4] == 4, "normal gutter restriction did not resume")
  assert(vim.api.nvim_buf_get_changedtick(session.buffer) == tick, "cursor normalization changed source")
end, debug.traceback)
if owner then owner.close() end
buffer.close(session)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("gutter_cursor OK")
vim.cmd("qa!")
