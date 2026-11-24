local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")

return function(lines, key)
  local buf = vim.api.nvim_get_current_buf()
  assert(vim.bo[buf].filetype == "ForgePlanCommentInput", "Plan Review did not open its comment input")
  vim.cmd("stopinsert")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  keymap.invoke(key or "<CR>", { buffer = buf })
end
