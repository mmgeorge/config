--- Owns the status/diff window option overrides (line numbers, folds, conceal, wrap) for the
--- DiffReview views, saving each window's prior values so they restore exactly on teardown.
---
--- Maintains one saved-value record per window id, keyed in saved_by_win, until restore clears it.

---@class DiffReviewWindowOptionState
---@field number boolean
---@field relativenumber boolean
---@field signcolumn string
---@field foldcolumn string
---@field foldenable boolean
---@field foldlevel integer
---@field foldmethod string
---@field foldtext string
---@field fillchars string
---@field winhighlight string
---@field virtualedit string
---@field wrap boolean
---@field linebreak boolean
---@field breakindent boolean
---@field conceallevel integer
---@field concealcursor string

---@class DiffReviewWindowOptionsModule
local M = {}

--- Saved prior window options per window id, populated on hide and cleared on restore.
---@type table<integer, DiffReviewWindowOptionState>
M.saved_by_win = {}

--- Merge a `key:value` pair into a comma-list option (fillchars/winhighlight), preserving order
--- and replacing an existing entry for the same key.
---@param value string?
---@param key string
---@param replacement string
---@return string
function M.option_with_pair(value, key, replacement)
  local option_by_key = {}
  local option_order = {}
  for option in tostring(value or ""):gmatch("[^,]+") do
    local option_key, option_value = option:match("^([^:]+):(.*)$")
    if option_key and option_key ~= "" then
      if option_by_key[option_key] == nil then option_order[#option_order + 1] = option_key end
      option_by_key[option_key] = option_value
    end
  end
  if option_by_key[key] == nil then option_order[#option_order + 1] = key end
  option_by_key[key] = replacement
  local parts = {}
  for _, option_key in ipairs(option_order) do
    parts[#parts + 1] = option_key .. ":" .. option_by_key[option_key]
  end
  return table.concat(parts, ",")
end

--- Hide line numbers and signs for the window, saving the prior values once so restore is exact.
---@param win integer?
function M.hide_line_numbers(win)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  if not M.saved_by_win[win] then
    M.saved_by_win[win] = {
      number = vim.wo[win].number,
      relativenumber = vim.wo[win].relativenumber,
      signcolumn = vim.wo[win].signcolumn,
      foldcolumn = vim.wo[win].foldcolumn,
      foldenable = vim.wo[win].foldenable,
      foldlevel = vim.wo[win].foldlevel,
      foldmethod = vim.wo[win].foldmethod,
      foldtext = vim.wo[win].foldtext,
      fillchars = vim.wo[win].fillchars,
      winhighlight = vim.wo[win].winhighlight,
      virtualedit = vim.wo[win].virtualedit,
      wrap = vim.wo[win].wrap,
      linebreak = vim.wo[win].linebreak,
      breakindent = vim.wo[win].breakindent,
      conceallevel = vim.wo[win].conceallevel,
      concealcursor = vim.wo[win].concealcursor,
    }
  end
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].foldcolumn = "0"
  vim.wo[win].virtualedit = "all"
end

--- Apply the full status-window option set (hidden numbers, manual folds, soft word wrap, no conceal).
---@param win integer?
---@param _state? table
function M.apply(win, _state)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  M.hide_line_numbers(win)
  -- Soft-wrap mixed prose/diff content at word boundaries (linebreak). Keep breakindent
  -- OFF: a wrapped-continuation indent is virtual whitespace the diff background cannot
  -- paint, so an indented continuation shows an unpainted notch under the gutter on +/-
  -- rows. Landing continuations at column 0 keeps the hl_eol band gap-free. Diff rows
  -- carry their old/new/sign gutter as inline virt_text on the first display row only.
  vim.wo[win].wrap = true
  vim.wo[win].linebreak = true
  vim.wo[win].breakindent = false
  vim.wo[win].conceallevel = 0
  vim.wo[win].concealcursor = ""
  vim.wo[win].foldenable = true
  vim.wo[win].foldlevel = 99
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldtext = "v:lua.diff_review_status_foldtext()"
  vim.wo[win].fillchars = M.option_with_pair(vim.wo[win].fillchars, "fold", " ")
  vim.wo[win].winhighlight = M.option_with_pair(vim.wo[win].winhighlight, "Folded", "Normal")
end

--- Restore the window's saved options and drop its record.
---@param win integer?
function M.restore(win)
  if not (win and vim.api.nvim_win_is_valid(win) and M.saved_by_win[win]) then return end
  local saved = M.saved_by_win[win]
  vim.wo[win].number = saved.number
  vim.wo[win].relativenumber = saved.relativenumber
  vim.wo[win].signcolumn = saved.signcolumn or "auto"
  vim.wo[win].foldcolumn = saved.foldcolumn or "0"
  vim.wo[win].foldenable = saved.foldenable
  vim.wo[win].foldlevel = saved.foldlevel or 0
  vim.wo[win].foldmethod = saved.foldmethod or "manual"
  vim.wo[win].foldtext = saved.foldtext or "foldtext()"
  vim.wo[win].fillchars = saved.fillchars or ""
  vim.wo[win].winhighlight = saved.winhighlight or ""
  vim.wo[win].virtualedit = saved.virtualedit or ""
  vim.wo[win].wrap = saved.wrap
  vim.wo[win].linebreak = saved.linebreak
  vim.wo[win].breakindent = saved.breakindent
  vim.wo[win].conceallevel = saved.conceallevel or 0
  vim.wo[win].concealcursor = saved.concealcursor or ""
  M.saved_by_win[win] = nil
end

--- Drop every saved record without restoring (cleanup path that wipes the windows anyway).
function M.reset()
  M.saved_by_win = {}
end

return M
