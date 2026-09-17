vim.opt.runtimepath:append("nvim")
local buffer = require("forge.buffer")
local commands = require("forge.document_commands")
local session = buffer.open("review-cursor")
assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = {
  { id = "comments", text = { "Comments (4):", "mmgeorge July 10, 2026  Next value of sorts.", "short", "A longer comment description" },
    metadata = { target = {}, decoration = {}, editable_region = {}, gutter = {
      { position = { row = 1, column = 0 }, chunk = { { text = "  ", capture = "Comment" } }, priority = 100 },
      { position = { row = 2, column = 0 }, chunk = { { text = "  ", capture = "Comment" } }, priority = 100 },
      { position = { row = 3, column = 0 }, chunk = { { text = "  ", capture = "Comment" } }, priority = 100 },
    } },
  },
} }).kind == "Applied")
vim.api.nvim_set_current_buf(session.buffer)
vim.wo.virtualedit = "all"
local function unchanged(event, view)
  local position, wanted = vim.fn.getcurpos(), vim.fn.winsaveview().curswant
  vim.api.nvim_exec_autocmds(event, { buffer = session.buffer })
  assert(vim.deep_equal(position, vim.fn.getcurpos()), view .. " " .. event .. " moved cursor: " .. vim.inspect(vim.fn.getcurpos()))
  assert(vim.fn.winsaveview().curswant == wanted, view .. " changed preferred column")
end
for _, view in ipairs({ "pr", "review" }) do
  local owner = commands.attach(session, { view = view })
  vim.fn.setpos(".", { 0, 2, 1, 0 })
  for _, event in ipairs({ "CursorMoved", "BufEnter", "WinEnter" }) do unchanged(event, view) end
  vim.api.nvim_win_set_cursor(0, { 1, 7 })
  for _, motion in ipairs({ "j", "j", "j", "k", "k", "$", "j", "j", "k" }) do
    vim.cmd("normal! " .. motion)
    unchanged("CursorMoved", view)
  end
  vim.fn.setpos(".", { 0, 3, 40, 20 })
  unchanged("CursorMoved", view)
  owner.close()
end
buffer.close(session)
print("review_cursor: PR and review preserve native cursor position and preferred column")
