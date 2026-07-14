local M = {}

local view_group = vim.api.nvim_create_augroup("DiffReviewHarnessPopupView", { clear = false })

---@param win integer
---@param buf integer
function M.clamp_view(win, buf)
  if not (vim.api.nvim_win_is_valid(win) and vim.api.nvim_buf_is_valid(buf)) then return end
  local line_count = vim.api.nvim_buf_line_count(buf)
  local height = vim.api.nvim_win_get_height(win)
  local maximum_topline = math.max(1, line_count - height + 1)
  local view = vim.api.nvim_win_call(win, vim.fn.winsaveview)
  if view.topline <= maximum_topline then return end
  view.topline = maximum_topline
  vim.api.nvim_win_call(win, function() vim.fn.winrestview(view) end)
end

---@param parent_win integer?
---@param width integer
---@param height integer
---@return integer, integer
function M.center(parent_win, width, height)
  if parent_win then
    local parent_width = vim.api.nvim_win_get_width(parent_win)
    local parent_height = vim.api.nvim_win_get_height(parent_win)
    return math.max(0, math.floor((parent_height - height) / 2)),
      math.max(0, math.floor((parent_width - width) / 2))
  end
  return math.max(0, math.floor((vim.o.lines - height) / 2)),
    math.max(0, math.floor((vim.o.columns - width) / 2))
end

---@class DiffReviewHarnessPopupWindowOptions
---@field parent_win? integer
---@field width integer
---@field height integer
---@field title string
---@field filetype string
---@field focusable? boolean
---@field cursorline? boolean
---@field wrap? boolean
---@field zindex? integer

---@param options DiffReviewHarnessPopupWindowOptions
---@return integer, integer
function M.open(options)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = options.filetype
  local row, col = M.center(options.parent_win, options.width, options.height)
  local window_config = {
    relative = options.parent_win and "win" or "editor",
    row = row,
    col = col,
    width = options.width,
    height = options.height,
    style = "minimal",
    border = "rounded",
    title = " " .. options.title .. " ",
    title_pos = "center",
    focusable = options.focusable ~= false,
    zindex = options.zindex or 80,
  }
  if options.parent_win then window_config.win = options.parent_win end
  local win = vim.api.nvim_open_win(buf, true, window_config)
  vim.wo[win].cursorline = options.cursorline == true
  vim.wo[win].wrap = options.wrap == true
  vim.wo[win].number = false
  vim.wo[win].relativenumber = false
  vim.wo[win].signcolumn = "no"
  vim.wo[win].scrolloff = 0
  vim.wo[win].sidescrolloff = 0
  vim.wo[win].winbar = ""
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled" }, {
    group = view_group,
    buffer = buf,
    callback = function() M.clamp_view(win, buf) end,
    desc = "Keep Harness popup content inside its rendered lines",
  })
  return buf, win
end

---@param win integer
---@param parent_win integer?
---@param width integer
---@param height integer
function M.resize(win, parent_win, width, height)
  local row, col = M.center(parent_win, width, height)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.row = row
  window_config.col = col
  window_config.width = width
  window_config.height = height
  vim.api.nvim_win_set_config(win, window_config)
end

---@param win integer
---@param hidden boolean
function M.set_cursor_hidden(win, hidden)
  vim.wo[win].winhighlight = hidden and "Cursor:DiffReviewHarnessHiddenCursor" or ""
end

---@param win integer
---@param focusable boolean
function M.set_focusable(win, focusable)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.focusable = focusable
  vim.api.nvim_win_set_config(win, window_config)
end

---@param win integer
---@param title string
function M.set_title(win, title)
  local window_config = vim.api.nvim_win_get_config(win)
  window_config.title = " " .. title .. " "
  vim.api.nvim_win_set_config(win, window_config)
end

return M
