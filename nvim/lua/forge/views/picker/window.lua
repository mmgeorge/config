local PickerWindow = {}

local popup_window = require("forge.infra.popup_window")
local picker_layout = require("forge.views.picker.layout")

---@param host table
---@param width integer
---@param height integer
---@param title string|table
---@param origin ForgePopupOrigin
---@return integer, integer
function PickerWindow.open(host, width, height, title, origin)
  local bounds = picker_layout.host_bounds(host.window_list)
  return popup_window.open({
    relative = "editor",
    row = math.max(bounds.top, bounds.bottom - height - 2),
    col = bounds.left,
    width = width,
    height = height,
    title = title,
    title_pos = "left",
    filetype = "ForgePicker",
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
function PickerWindow.resize(win, host, width, height, title, footer)
  if not vim.api.nvim_win_is_valid(win) then return end
  local bounds = picker_layout.host_bounds(host.window_list)
  local config = vim.api.nvim_win_get_config(win)
  config.row = math.max(bounds.top, bounds.bottom - height - 2)
  config.col = bounds.left
  config.width = width
  config.height = height
  config.title = title
  config.title_pos = "left"
  config.footer = footer and { { " " .. footer .. " ", "ForgePickerHint" } } or ""
  config.footer_pos = "left"
  vim.api.nvim_win_set_config(win, config)
end

return PickerWindow
