local M = {}

---@param window integer
---@return table
function M.capture(window)
  local buffer = vim.api.nvim_win_get_buf(window)
  local info = vim.fn.getwininfo(window)[1]
  local options = vim.wo[window]
  local textoff = info and info.textoff or 0
  if not options.number and not options.relativenumber and options.signcolumn == "no"
      and options.foldcolumn == "0" and not options.statuscolumn:find("%%") then
    textoff = vim.fn.strdisplaywidth(options.statuscolumn)
  end
  local variable = {}
  for value in vim.bo[buffer].vartabstop:gmatch("%d+") do variable[#variable + 1] = tonumber(value) end
  local custom = {}
  for _, range in ipairs(vim.fn.getcellwidths()) do
    custom[#custom + 1] = { first = range[1], last = range[2], cells = range[3] }
  end
  return {
    columns = math.max(1, vim.api.nvim_win_get_width(window) - textoff),
    tabstop = vim.bo[buffer].tabstop,
    variable_tabstop = variable,
    ambiguous = vim.o.ambiwidth,
    cell_width = custom,
  }
end

return M
