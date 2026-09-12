--- Owns keymaps, hints, winbars, and help for native Forge views.
--- Status, PR, and review command dispatch belongs to their native document owners.
---@class ForgeKeymapsModule
local M = {}

local config = require("forge.infra.config")
local popup_window = require("forge.infra.popup_window")
local command_specs = require("forge.shared.command_specs")

---@param group string
---@param command_id string
---@return string[]
function M.view_keys_for(group, command_id)
  local configured_keymaps = config.options.keymaps and config.options.keymaps[group] or {}
  local key = configured_keymaps and configured_keymaps[command_id]
  if key == false or key == nil then return {} end
  if type(key) == "table" then return vim.deepcopy(key) end
  return { key }
end

---@param buf integer
---@param group string
---@param command_set ForgeViewCommandSet
---@param context? table
function M.setup_view_keymaps(buf, group, command_set, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    if action and spec and (not action.enabled or action.enabled(context)) then
      local modes = type(spec.modes) == "table" and spec.modes or { spec.modes or "n" }
      for _, key in ipairs(M.view_keys_for(group, command_id)) do
        vim.keymap.set(modes, key, function()
          require("forge.shared.view_command_set").dispatch(command_set, command_id, context)
        end, { buffer = buf, silent = true, nowait = true, desc = spec.desc })
      end
    end
  end
end

---@param group string
---@param command_set ForgeViewCommandSet
---@param context? table
---@return string
function M.view_hint(group, command_set, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  local segments = {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    local key = M.view_keys_for(group, command_id)[1]
    if action and spec and spec.pinned and key and (not action.enabled or action.enabled(context)) then
      segments[#segments + 1] = key .. " " .. spec.label
    end
  end
  return table.concat(segments, " | ")
end

---@param win integer
---@param title string
---@param group string
---@param command_set ForgeViewCommandSet
---@param status? string|{ text: string, group: string }[]
---@param context? table
---@param right_status? { text: string, group: string }[]
function M.apply_view_winbar(win, title, group, command_set, status, context, right_status)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  local title_text = tostring(title or "")
  local status_segments = type(status) == "table" and status or nil
  local status_text = status_segments and table.concat(vim.tbl_map(function(segment) return segment.text end, status_segments))
    or tostring(status or "")
  local status_rendered = status_segments and table.concat(vim.tbl_map(function(segment)
    return ("%%#%s#%s%%*"):format(segment.group, segment.text:gsub("%%", "%%%%"))
  end, status_segments)) or nil
  local hint_text = M.view_hint(group, command_set, context)
  local right_status_text = table.concat(vim.tbl_map(function(segment) return segment.text end, right_status or {}))
  local right_status_rendered = table.concat(vim.tbl_map(function(segment)
    return ("%%#%s#%s%%*"):format(segment.group, segment.text:gsub("%%", "%%%%"))
  end, right_status or {}))
  local full_width = vim.fn.strdisplaywidth(title_text) + vim.fn.strdisplaywidth(status_text)
    + vim.fn.strdisplaywidth(right_status_text) + vim.fn.strdisplaywidth(hint_text) + 7
  local left_text = title_text ~= "" and title_text or status_text
  local center_text = title_text ~= "" and status_text or ""
  local left = left_text:gsub("%%", "%%%%")
  local center = center_text:gsub("%%", "%%%%")
  if full_width <= vim.api.nvim_win_get_width(win) then
    local hint = hint_text:gsub("%%", "%%%%")
    if status_rendered then
      local rendered_left = title_text ~= "" and ("%%#ForgeStatusLabel#%s%%*"):format(left) or status_rendered
      local rendered_center = title_text ~= "" and status_rendered or ""
      local rendered_right = right_status_rendered
      if rendered_right ~= "" and hint ~= "" then rendered_right = rendered_right .. " • " end
      vim.wo[win].winbar = ("%s%%=%s%%=%s%%#ForgeStatusHint#%s%%*"):format(rendered_left, rendered_center, rendered_right, hint)
      return
    end
    local rendered_right = right_status_rendered
    if rendered_right ~= "" and hint ~= "" then rendered_right = rendered_right .. " • " end
    vim.wo[win].winbar = ("%%#ForgeStatusLabel#%s%%*%%=%%#ForgeStatusHint#%s%%*%%=%s%%#ForgeStatusHint#%s%%*")
      :format(left, center, rendered_right, hint)
    return
  end
  local help_action = command_set.action_by_id.help
  local help_key = help_action and M.view_keys_for(group, "help")[1] or nil
  local help_hint = help_key and (help_key .. " help"):gsub("%%", "%%%%") or ""
  local compact_right = right_status_rendered ~= "" and ("%%=%s"):format(right_status_rendered)
    or (help_hint ~= "" and ("%%=%%#ForgeStatusHint#%s%%*"):format(help_hint) or "")
  if status_rendered then
    local rendered_left = title_text ~= "" and ("%%#ForgeStatusLabel#%s%%*"):format(left) or status_rendered
    local rendered_center = title_text ~= "" and status_rendered or ""
    vim.wo[win].winbar = ("%s  %s%s"):format(rendered_left, rendered_center, compact_right)
    return
  end
  vim.wo[win].winbar = ("%%#ForgeStatusLabel#%s%%*  %%#ForgeStatusHint#%s%%*%s"):format(left, center, compact_right)
end

---@param group string
---@param command_set ForgeViewCommandSet
---@param title string
---@param context? table
function M.show_view_help(group, command_set, title, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  local lines = { title, "" }
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    local keys = M.view_keys_for(group, command_id)
    if action and spec and #keys > 0 and (not action.enabled or action.enabled(context)) then
      lines[#lines + 1] = ("  %-12s %s"):format(table.concat(keys, ", "), spec.desc)
    end
  end
  local width = math.min(80, math.max(40, vim.o.columns - 8))
  local height = math.min(#lines + 2, math.max(6, vim.o.lines - 6))
  local buf, win = popup_window.open({ relative = "editor", width = width, height = height, title = title, filetype = "ForgeHelp" })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.keymap.set("n", "q", function() popup_window.close(win) end,
    { buffer = buf, silent = true, nowait = true, desc = "Close help" })
end

return M
