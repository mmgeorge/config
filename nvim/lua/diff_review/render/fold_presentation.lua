local M = {}

---@type table<integer, table<integer, any>>
local fold_text_by_buf = {}

---@param value string?
---@param key string
---@param replacement string
---@return string
local function option_with_pair(value, key, replacement)
  local value_by_key = {}
  local key_order = {}
  for option in tostring(value or ""):gmatch("[^,]+") do
    local option_key, option_value = option:match("^([^:]+):(.*)$")
    if option_key and option_key ~= "" then
      if value_by_key[option_key] == nil then key_order[#key_order + 1] = option_key end
      value_by_key[option_key] = option_value
    end
  end
  if value_by_key[key] == nil then key_order[#key_order + 1] = key end
  value_by_key[key] = replacement

  local option_list = {}
  for _, option_key in ipairs(key_order) do
    option_list[#option_list + 1] = option_key .. ":" .. value_by_key[option_key]
  end
  return table.concat(option_list, ",")
end

---Resolve one fold label into text accepted by Neovim's foldtext callback.
---@param value any
---@return any
function M.resolve(value)
  if type(value) == "function" then
    local ok, text = pcall(value)
    if ok then return M.resolve(text) end
  elseif type(value) == "table" then
    return value
  elseif value ~= nil then
    return tostring(value)
  end
  local fold_start = tonumber(vim.v.foldstart) or vim.fn.line(".")
  return vim.fn.getline(fold_start)
end

---Replace the fold labels owned by one rendered buffer.
---@param buf integer
---@param text_by_start_line table<integer, any>?
function M.replace(buf, text_by_start_line)
  fold_text_by_buf[buf] = text_by_start_line or {}
end

---Release fold labels owned by one rendered buffer.
---@param buf integer
function M.clear(buf)
  fold_text_by_buf[buf] = nil
end

---Apply the shared status-style fold chrome to one window.
---@param win integer?
function M.apply_window(win)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  vim.wo[win].foldtext = "v:lua.diff_review_foldtext()"
  vim.wo[win].fillchars = option_with_pair(vim.wo[win].fillchars, "fold", " ")
  vim.wo[win].winhighlight = option_with_pair(vim.wo[win].winhighlight, "Folded", "Normal")
end

function _G.diff_review_foldtext()
  local buf = vim.api.nvim_get_current_buf()
  local fold_start = tonumber(vim.v.foldstart) or vim.fn.line(".")
  local value = fold_text_by_buf[buf] and fold_text_by_buf[buf][fold_start] or nil
  return M.resolve(value)
end

return M
