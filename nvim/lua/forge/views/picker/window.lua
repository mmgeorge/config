local PickerWindow = {}

local popup_window = require("forge.infra.popup_window")
local picker_layout = require("forge.views.picker.layout")

local function position(bounds, width, height, placement)
  if placement == "center" then
    return {
      row = bounds.top + math.max(0, math.floor((bounds.height - height - 2) / 2)),
      col = bounds.left + math.max(0, math.floor((bounds.width - width - 2) / 2)),
    }
  end
  if placement == "bottom" then
    return {
      row = math.max(bounds.top, bounds.bottom - height - 2),
      col = bounds.left,
    }
  end
  return { row = math.max(bounds.top, bounds.bottom - height - 2), col = bounds.left }
end

---@param host table
---@param width integer
---@param height integer
---@param title string|table
---@param origin ForgePopupOrigin
---@param placement? "center"|"bottom"
---@return integer, integer
function PickerWindow.open(host, width, height, title, origin, placement)
  local bounds = picker_layout.host_bounds(host.window_list)
  local anchor = position(bounds, width, height, placement)
  return popup_window.open({
    relative = "editor",
    row = anchor.row,
    col = anchor.col,
    width = width,
    height = height,
    title = title,
    title_pos = "left",
    filetype = "ForgePicker",
    winfixbuf = true,
    focusable = true,
    enter = false,
    origin = origin,
    wrap = false,
    zindex = 90,
  })
end

---@param win integer
---@param host table
---@param width integer
---@param height integer
---@param title string|table
---@param footer string?
---@param placement? "center"|"bottom"
function PickerWindow.resize(win, host, width, height, title, footer, placement)
  if not vim.api.nvim_win_is_valid(win) then return end
  local bounds = picker_layout.host_bounds(host.window_list)
  local anchor = position(bounds, width, height, placement)
  local config = vim.api.nvim_win_get_config(win)
  config.row = anchor.row
  config.col = anchor.col
  config.width = width
  config.height = height
  config.title = title
  config.title_pos = "left"
  config.footer = footer and { { " " .. footer .. " ", "ForgePickerHint" } } or ""
  config.footer_pos = "left"
  vim.api.nvim_win_set_config(win, config)
end

return PickerWindow
