local M = {}
local window_owner = {}
local option = { "number", "relativenumber", "signcolumn", "foldcolumn", "statuscolumn",
  "foldmethod", "foldexpr", "foldenable", "foldlevel", "foldtext", "fillchars", "virtualedit",
  "wrap", "linebreak", "breakindent", "breakindentopt", "conceallevel", "concealcursor" }

---@param window integer
---@return table<string, string|boolean|number>
function M.capture(window)
  local retained = window_owner[window]
  if retained then return vim.deepcopy(retained.baseline) end
  local buffer = vim.api.nvim_win_get_buf(window)
  for attached, owner in pairs(window_owner) do
    if vim.api.nvim_win_is_valid(attached) and vim.api.nvim_win_get_buf(attached) == buffer then
      return vim.deepcopy(owner.baseline)
    end
  end
  local baseline = {}
  for _, name in ipairs(option) do baseline[name] = vim.wo[window][name] end
  return baseline
end

---@param window integer
---@param owner table
---@param baseline table<string, string|boolean|number>
---@param applied? table<string, string|boolean|number> Owned values to restore when unchanged.
function M.retain(window, owner, baseline, applied)
  window_owner[window] = { owner = owner, baseline = baseline, applied = applied }
end

---@param buffer integer Readonly buffer whose visible windows receive these options.
---@param applied table<string, string|boolean|number> Window options restored on leaving the buffer.
function M.attach(buffer, applied)
  local owner = {}
  local attached = {}
  local function enter()
    local window = vim.api.nvim_get_current_win()
    if vim.api.nvim_win_get_buf(window) ~= buffer then return end
    local baseline = M.capture(window)
    for name, value in pairs(applied) do vim.wo[window][name] = value end
    M.retain(window, owner, baseline, applied)
    attached[window] = true
  end
  vim.api.nvim_create_autocmd("BufWinEnter", { buffer = buffer, callback = enter })
  vim.api.nvim_create_autocmd("BufWinLeave", { buffer = buffer, callback = function()
    local window = vim.api.nvim_get_current_win()
    if attached[window] then M.release(window, owner) attached[window] = nil end
  end })
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = buffer, once = true, callback = function()
    for window in pairs(attached) do M.release(window, owner) end
  end })
  if vim.api.nvim_get_current_buf() == buffer then enter() end
end

---@param window integer
---@param owner table
---@param before_restore? fun() Release subordinate window state only while ownership matches.
---@return boolean owned
function M.release(window, owner, before_restore)
  local retained = window_owner[window]
  if not retained or retained.owner ~= owner then return false end
  window_owner[window] = nil
  local current = {}
  if vim.api.nvim_win_is_valid(window) then
    for name in pairs(retained.applied or {}) do current[name] = vim.wo[window][name] end
  end
  if before_restore then before_restore() end
  if vim.api.nvim_win_is_valid(window) then
    for name, value in pairs(retained.applied or {}) do
      if current[name] == value then vim.wo[window][name] = retained.baseline[name]
      else vim.wo[window][name] = current[name] end
    end
  end
  return true
end

vim.api.nvim_create_autocmd("WinClosed", { callback = function(event)
  window_owner[tonumber(event.match)] = nil
end })

return M
