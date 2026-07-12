local M = {}

local failed = false

---@param row integer
---@param range_list { first0: integer, after0: integer }[]
---@return boolean
local function row_in_range(row, range_list)
  for _, range in ipairs(range_list) do
    if row >= range.first0 and row < range.after0 then return true end
  end
  return false
end

---@param range_list { first0: integer, after0: integer }[]
---@return table[]
local function parser_region_list(range_list)
  local region_list = {}
  for _, range in ipairs(range_list) do
    if range.after0 > range.first0 then
      region_list[#region_list + 1] = { range.first0, 0, range.after0, 0 }
    end
  end
  return #region_list > 0 and { region_list } or {}
end

---@param buf integer
---@param range_list { first0: integer, after0: integer }[]
local function prune_extmarks(buf, range_list)
  local ok, ui = pcall(require, "render-markdown.core.ui")
  if not ok or not ui.ns then return end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ui.ns, 0, -1, {})) do
    if not row_in_range(mark[2], range_list) then pcall(vim.api.nvim_buf_del_extmark, buf, ui.ns, mark[1]) end
  end
end

---@param buf integer
---@param win integer?
---@param range_list { first0: integer, after0: integer }[]
function M.render(buf, win, range_list)
  if #range_list == 0 or not (win and vim.api.nvim_win_is_valid(win)) then return end
  local ok, render_markdown = pcall(require, "render-markdown")
  if not ok or type(render_markdown.render) ~= "function" then return end
  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
  if not parser_ok then return end
  pcall(parser.set_included_regions, parser, parser_region_list(range_list))
  if type(parser.invalidate) == "function" then pcall(parser.invalidate, parser, true) end
  local highlight_ok, highlight_error = pcall(vim.treesitter.start, buf, "markdown")
  if not highlight_ok then
    if not failed then
      failed = true
      vim.notify("Harness markdown highlighting failed: " .. tostring(highlight_error), vim.log.levels.WARN, { title = "Harness" })
    end
    return
  end
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = win })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = win })
  local render_ok, render_error = pcall(render_markdown.render, {
    buf = buf,
    win = win,
    config = {
      enabled = true,
      render_modes = true,
      debounce = 0,
      completions = { lsp = { enabled = false } },
      sign = { enabled = false },
      win_options = {
        conceallevel = { default = conceallevel, rendered = 3 },
        concealcursor = { default = concealcursor, rendered = concealcursor },
      },
      on = { render = function() prune_extmarks(buf, range_list) end },
    },
  })
  if render_ok then
    prune_extmarks(buf, range_list)
  elseif not failed then
    failed = true
    vim.notify("Harness markdown rendering failed: " .. tostring(render_error), vim.log.levels.WARN, { title = "Harness" })
  end
end

return M
