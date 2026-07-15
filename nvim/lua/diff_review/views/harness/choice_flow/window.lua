local M = {}

local popup_window = require("diff_review.infra.popup_window")

function M.open(parent_win, width, height, title, filetype, zindex)
  return popup_window.open({
    parent_win = parent_win,
    width = width,
    height = height,
    title = title,
    filetype = filetype or "DiffReviewChoiceFlow",
    zindex = zindex or 80,
  })
end

function M.resize(win, parent_win, width, height)
  popup_window.resize(win, parent_win, width, height)
end

function M.set_cursor_hidden(win, hidden)
  popup_window.set_cursor_hidden(win, hidden)
end

function M.set_focusable(win, focusable)
  popup_window.set_focusable(win, focusable)
end

function M.set_title(win, title)
  popup_window.set_title(win, title)
end

function M.clamp_view(win, buf)
  popup_window.clamp_view(win, buf)
end

---@param win integer?
---@param restore? boolean
function M.close(win, restore)
  popup_window.close(win, restore)
end

return M
