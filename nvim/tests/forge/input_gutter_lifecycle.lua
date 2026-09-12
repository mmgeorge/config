local workspace = vim.fn.getcwd()
vim.opt.runtimepath:prepend(workspace .. "/nvim")
local gutter = require("forge.shared.input_gutter")

local succeeded, failure = xpcall(function()
  local first_window = vim.api.nvim_get_current_win()
  local ordinary = vim.api.nvim_get_current_buf()
  local input = vim.api.nvim_create_buf(false, true)
  vim.bo[input].bufhidden = "hide"
  vim.wo[first_window].number = true
  vim.wo[first_window].relativenumber = true
  vim.wo[first_window].signcolumn = "yes"
  vim.wo[first_window].foldcolumn = "1"
  vim.wo[first_window].statuscolumn = "original"
  vim.api.nvim_win_set_buf(first_window, input)
  gutter.apply(first_window)
  assert(vim.wo[first_window].statuscolumn:find("❯", 1, true))
  vim.api.nvim_win_set_buf(first_window, ordinary)
  assert(vim.wo[first_window].statuscolumn == "original", "input gutter leaked into ordinary buffer: " .. vim.inspect(vim.wo[first_window].statuscolumn))
  assert(vim.wo[first_window].number and vim.wo[first_window].relativenumber)
  assert(vim.wo[first_window].signcolumn == "yes" and vim.wo[first_window].foldcolumn == "1")
  vim.api.nvim_win_set_buf(first_window, input)
  assert(vim.wo[first_window].statuscolumn:find("❯", 1, true), "input gutter did not reapply")
  local newly_opened = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_win_set_buf(first_window, newly_opened)
  assert(not vim.wo[first_window].statuscolumn:find("❯", 1, true), "new log buffer inherited input gutter")
  vim.api.nvim_win_set_buf(first_window, input)

  vim.cmd("vsplit")
  local second_window = vim.api.nvim_get_current_win()
  assert(vim.wo[second_window].statuscolumn:find("❯", 1, true))
  vim.api.nvim_win_set_buf(second_window, ordinary)
  assert(not vim.wo[second_window].statuscolumn:find("❯", 1, true), "split retained copied input gutter")
  assert(vim.wo[first_window].statuscolumn:find("❯", 1, true), "leaving split detached original input")
  vim.wo[second_window].statuscolumn = "other"
  vim.api.nvim_win_set_buf(second_window, input)
  vim.wo[second_window].signcolumn = "auto:2"
  vim.api.nvim_exec_autocmds("BufWinLeave", { buffer = input })
  assert(vim.wo[second_window].signcolumn == "auto:2", "release overwrote unrelated option change")
  vim.api.nvim_win_set_buf(second_window, ordinary)
  assert(vim.wo[second_window].statuscolumn == "other")
  vim.api.nvim_buf_delete(input, { force = true })
  assert(not vim.wo[second_window].statuscolumn:find("❯", 1, true))
end, debug.traceback)
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("input gutter lifecycle passed")
vim.cmd("qa!")
