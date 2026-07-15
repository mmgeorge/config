local M = {}

local view_group = vim.api.nvim_create_augroup("DiffReviewPopupView", { clear = false })

---@class DiffReviewPopupOrigin
---@field win integer
---@field mode string

---@class DiffReviewPopupWindowOptions
---@field parent_win? integer
---@field relative? "editor"|"win"|"cursor"
---@field win? integer
---@field row? integer
---@field col? integer
---@field width integer
---@field height integer
---@field title string
---@field filetype? string
---@field focusable? boolean
---@field cursorline? boolean
---@field wrap? boolean
---@field linebreak? boolean
---@field zindex? integer

---@type table<integer, DiffReviewPopupOrigin>
local origin_by_window = {}

---@return DiffReviewPopupOrigin
function M.capture_origin()
  local origin = {
    win = vim.api.nvim_get_current_win(),
    mode = vim.fn.mode(1),
  }
  vim.cmd("stopinsert")
  return origin
end

---@param origin DiffReviewPopupOrigin?
function M.restore_origin(origin)
  if not origin then return end
  vim.cmd("stopinsert")
  if not vim.api.nvim_win_is_valid(origin.win) then return end
  vim.api.nvim_set_current_win(origin.win)
  if origin.mode:sub(1, 1) == "i" then
    vim.cmd("startinsert")
  elseif origin.mode:sub(1, 1) == "R" then
    vim.cmd("startreplace")
  elseif origin.mode == "v" or origin.mode == "V" or origin.mode == "\22" then
    vim.cmd("normal! gv")
  end
end

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

---@param options DiffReviewPopupWindowOptions
---@return integer, integer
function M.open(options)
  local origin = M.capture_origin()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = options.filetype or "DiffReviewPopup"
  local centered_row, centered_col = M.center(options.parent_win, options.width, options.height)
  local relative = options.relative or (options.parent_win and "win" or "editor")
  local window_config = {
    relative = relative,
    row = options.row or (relative == "cursor" and 1 or centered_row),
    col = options.col or (relative == "cursor" and 0 or centered_col),
    width = options.width,
    height = options.height,
    style = "minimal",
    border = "rounded",
    title = " " .. options.title .. " ",
    title_pos = "center",
    focusable = options.focusable ~= false,
    zindex = options.zindex or 80,
  }
  if relative == "win" then window_config.win = options.win or options.parent_win end
  local popup_win = vim.api.nvim_open_win(buf, true, window_config)
  origin_by_window[popup_win] = origin
  vim.wo[popup_win].cursorline = options.cursorline == true
  vim.wo[popup_win].wrap = options.wrap == true
  vim.wo[popup_win].linebreak = options.linebreak == true
  vim.wo[popup_win].number = false
  vim.wo[popup_win].relativenumber = false
  vim.wo[popup_win].signcolumn = "no"
  vim.wo[popup_win].scrolloff = 0
  vim.wo[popup_win].sidescrolloff = 0
  vim.wo[popup_win].winbar = ""
  vim.api.nvim_create_autocmd({ "CursorMoved", "WinScrolled" }, {
    group = view_group,
    buffer = buf,
    callback = function() M.clamp_view(popup_win, buf) end,
    desc = "Keep DiffReview popup content inside its rendered lines",
  })
  vim.api.nvim_create_autocmd("WinClosed", {
    group = view_group,
    pattern = tostring(popup_win),
    once = true,
    callback = function()
      local closed_origin = origin_by_window[popup_win]
      origin_by_window[popup_win] = nil
      if closed_origin then vim.schedule(function() M.restore_origin(closed_origin) end) end
    end,
    desc = "Restore the window and mode that opened a DiffReview popup",
  })
  return buf, popup_win
end

---@param win integer?
---@param restore? boolean
function M.close(win, restore)
  if not win then return end
  local origin = origin_by_window[win]
  origin_by_window[win] = nil
  if vim.api.nvim_win_is_valid(win) then vim.api.nvim_win_close(win, true) end
  if restore ~= false then M.restore_origin(origin) end
end

---@generic T
---@param item_list T[]
---@param options table
---@param callback fun(item: T?, index: integer?)
function M.select(item_list, options, callback)
  local origin = M.capture_origin()
  vim.ui.select(item_list, options, function(item, index)
    M.restore_origin(origin)
    callback(item, index)
  end)
end

---@param options table
---@param callback fun(value: string?)
function M.input(options, callback)
  local origin = M.capture_origin()
  vim.ui.input(options, function(value)
    M.restore_origin(origin)
    callback(value)
  end)
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
