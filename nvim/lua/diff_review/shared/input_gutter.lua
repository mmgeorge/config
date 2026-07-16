local M = {}

---@param win integer
function M.apply(win)
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].foldcolumn = "2"
  vim.wo[win].statuscolumn = "%#DiffReviewHarnessPrompt#%{v:lnum == 1 ? '❯ ' : '  '}%*"
end

return M
