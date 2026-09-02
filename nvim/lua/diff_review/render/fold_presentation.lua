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

--- Resolves a fold label value into text or segment tables consumable by Neovim's `foldtext`.
--- Falls back to the line text at `foldstart` when unresolved.
---@param value any String, table of segments, function, or nil.
---@return any text Resolved fold label string or segments table.
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

--- Replaces the starting-line fold text lookup table for a buffer.
---@param buf integer Target buffer handle.
---@param text_by_start_line table<integer, any>? Mapping from 1-based start line to fold text or resolver.
function M.replace(buf, text_by_start_line)
  fold_text_by_buf[buf] = text_by_start_line or {}
end

--- Clears the registered fold text lookup table for a buffer.
---@param buf integer Target buffer handle.
function M.clear(buf)
  fold_text_by_buf[buf] = nil
end

--- Configures window-local options to apply clean fold rendering and highlight styles.
---@param win integer? Target window handle.
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
