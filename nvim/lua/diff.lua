local M = {}

M.show_diff = function (str1, str2)
  local diff = vim.diff(str1, str2, {
    result_type = "unified",
    algorithm = "myers",
    ctxlen = 3,
  })

  -- Create a scratch buffer
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
  vim.api.nvim_set_option_value("filetype", "diff", { buf = buf })

  -- Set diff content
  local lines = vim.split(diff, "\n", { trimempty = true })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  -- Open in a split
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, buf)
end

return M
