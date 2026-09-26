local M = {}

local previous_tabline = nil
local label_key = "forge_harness_label"

local function escape(text)
  return text:gsub("%%", "%%%%")
end

local function label_for(tab)
  local ok, label = pcall(vim.api.nvim_tabpage_get_var, tab, label_key)
  if ok then return label end
  local win = vim.api.nvim_tabpage_get_win(tab)
  local buf = vim.api.nvim_win_get_buf(win)
  local name = vim.api.nvim_buf_get_name(buf)
  return name == "" and "[No Name]" or vim.fn.fnamemodify(name, ":t")
end

---@return string
function M.render()
  local parts = {}
  local current = vim.api.nvim_get_current_tabpage()
  for index, tab in ipairs(vim.api.nvim_list_tabpages()) do
    local group = tab == current and "TabLineSel" or "TabLine"
    parts[#parts + 1] = ("%%#%s#%%%dT %s %%T%%%dXx%%X "):format(group, index, escape(label_for(tab)), index)
  end
  parts[#parts + 1] = "%#TabLineFill#%T"
  return table.concat(parts)
end

---@param tab integer
---@param name? string
function M.set_session_name(tab, name)
  if not tab or not vim.api.nvim_tabpage_is_valid(tab) then return end
  if previous_tabline == nil then
    previous_tabline = vim.o.tabline
    vim.o.tabline = "%!v:lua.require'forge.views.harness.tabline'.render()"
  end
  local label = name and name ~= "" and name:gsub("%s+", " ") or "[unnamed]"
  vim.api.nvim_tabpage_set_var(tab, label_key, vim.fn.strcharpart(label, 0, 30))
  vim.cmd.redrawtabline()
end

---@param tab? integer
function M.clear(tab)
  if tab and vim.api.nvim_tabpage_is_valid(tab) then
    pcall(vim.api.nvim_tabpage_del_var, tab, label_key)
  end
  for _, other in ipairs(vim.api.nvim_list_tabpages()) do
    local ok = pcall(vim.api.nvim_tabpage_get_var, other, label_key)
    if ok then return end
  end
  if previous_tabline ~= nil then
    vim.o.tabline = previous_tabline
    previous_tabline = nil
  end
end

return M
