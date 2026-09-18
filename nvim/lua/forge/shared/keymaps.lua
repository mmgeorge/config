--- Owns keymaps, hints, winbars, and help for native Forge views.
--- Status, PR, and review command dispatch belongs to their native document owners.
---@class ForgeKeymapsModule
local M = {}

local config = require("forge.infra.config")
local popup_window = require("forge.infra.popup_window")
local command_specs = require("forge.shared.command_specs")

---@param key string
---@param bindings string[]
---@return boolean
function M.is_prefix(key, bindings)
  local prefix = vim.api.nvim_replace_termcodes(key, true, true, true)
  for _, binding in ipairs(bindings) do
    local candidate = vim.api.nvim_replace_termcodes(binding, true, true, true)
    if #candidate > #prefix and candidate:sub(1, #prefix) == prefix then return true end
  end
  return false
end

---@param group string
---@param command_id string
---@param overrides? table<string, string|string[]|boolean>
---@return string[]
function M.view_keys_for(group, command_id, overrides)
  local options = config.options or config.defaults
  local configured_keymaps = options.keymaps and options.keymaps[group] or {}
  local key = configured_keymaps[command_id]
  if key == nil then key = (config.defaults.keymaps[group] or {})[command_id] end
  if overrides and overrides[command_id] ~= nil then key = overrides[command_id] end
  if key == false or key == nil then return {} end
  if type(key) == "table" then return vim.deepcopy(key) end
  return { key }
end

---@class ForgeCommandBinding
---@field spec ForgeStatusCommandSpec
---@field keys string[]

---@class ForgeCommandMapping
---@field key string
---@field mode string
---@field callback function
---@field prior table
---@field command string
---@field active boolean
---@field desc string
---@field nowait boolean

---@class ForgeCommandBindings
---@field binding ForgeCommandBinding[]
---@field mapping ForgeCommandMapping[]

---Install configured commands and retain mapping ownership for editable document lifecycles.
---@param buf integer
---@param group string
---@param specs ForgeStatusCommandSpec[]
---@param handlers table<string, function>
---@param options? {view?: string, keymaps?: table}
---@return ForgeCommandBindings bindings
function M.bind_commands(buf, group, specs, handlers, options)
  options = options or {}
  local owner = { binding = {}, mapping = {} }
  local normal_keys = {}
  for _, spec in ipairs(specs) do
    if handlers[spec.id] and (not options.view or not spec.views or spec.views[options.view]) then
      local keys = M.view_keys_for(spec.keymap or group, spec.id, options.keymaps)
      if #keys > 0 then
        owner.binding[#owner.binding + 1] = { spec = spec, keys = keys }
        local modes = type(spec.modes) == "table" and spec.modes or { spec.modes or "n" }
        if vim.tbl_contains(modes, "n") then vim.list_extend(normal_keys, keys) end
      end
    end
  end
  for _, binding in ipairs(owner.binding) do
    local spec = binding.spec
    local modes = type(spec.modes) == "table" and spec.modes or { spec.modes or "n" }
    for _, key in ipairs(binding.keys) do
      for _, mode in ipairs(modes) do
        local prior = vim.api.nvim_buf_call(buf, function() return vim.fn.maparg(key, mode, false, true) end)
        local mapping = { key = key, mode = mode, callback = handlers[spec.id], prior = prior,
          command = spec.id, active = true, desc = spec.desc, nowait = mode == "n" and not M.is_prefix(key, normal_keys) }
        owner.mapping[#owner.mapping + 1] = mapping
        vim.keymap.set(mode, key, mapping.callback, { buffer = buf, silent = true,
          nowait = mapping.nowait, desc = spec.desc })
      end
    end
  end
  local clue = package.loaded["mini.clue"]
  if clue and clue.ensure_buf_triggers then clue.ensure_buf_triggers(buf) end
  return owner
end

---@param buf integer
---@param group string
---@param command_set ForgeViewCommandSet
---@param context? table
function M.setup_view_keymaps(buf, group, command_set, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  local specs, handlers = {}, {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action, spec = command_set.action_by_id[command_id], spec_by_id[command_id]
    if action and spec and (not action.enabled or action.enabled(context)) then
      specs[#specs + 1] = spec
      handlers[command_id] = function()
        require("forge.shared.view_command_set").dispatch(command_set, command_id, context)
      end
    end
  end
  M.bind_commands(buf, group, specs, handlers)
end

---Render the shared Forge command hints, preserving help and close in narrow windows.
---@param hint {id: string, key: string, label: string}[]
---@param width integer
---@param options? {title?: string, narrow_title?: string, inline?: boolean}
---@return string
function M.render_hintbar(hint, width, options)
  options = options or {}
  local title = options.title or ""
  local total = vim.fn.strdisplaywidth(title) + 2
  for index, entry in ipairs(hint) do
    total = total + vim.fn.strdisplaywidth(entry.key .. " " .. entry.label) + (index > 1 and 3 or 0)
  end
  local narrow = total > width and type(options.narrow_title) == "string"
  if total > width then title = options.narrow_title or "" end
  local available = width - vim.fn.strdisplaywidth(title) - (title ~= "" and not narrow and 2 or 0)
  local reserved = 0
  for _, entry in ipairs(hint) do
    if entry.id == "help" or entry.id == "close" then reserved = reserved + vim.fn.strdisplaywidth(entry.key .. " " .. entry.label) + 3 end
  end
  local parts = options.inline and {} or { "%#ForgeStatusLabel#", title:gsub("%%", "%%%%"), narrow and "%*" or "%*%<%=" }
  local selected = 0
  for _, entry in ipairs(hint) do
    local entry_width = vim.fn.strdisplaywidth(entry.key .. " " .. entry.label)
    local essential = entry.id == "help" or entry.id == "close"
    if essential then reserved = reserved - entry_width - 3 end
    if essential or entry_width + (selected > 0 and 3 or 0) + reserved <= available then
      if selected > 0 then parts[#parts + 1] = "%#ForgeStatusHint# | %*" available = available - 3 end
      parts[#parts + 1] = ("%%#ForgeStatusHintKey#%s%%*%%#ForgeStatusHint# %s%%*")
        :format(entry.key:gsub("%%", "%%%%"), entry.label:gsub("%%", "%%%%"))
      available, selected = available - entry_width, selected + 1
    end
  end
  return table.concat(parts)
end

---@param group string
---@param command_set ForgeViewCommandSet
---@param context? table
---@return string
function M.view_hint(group, command_set, context)
  return table.concat(vim.tbl_map(function(entry) return entry.key .. " " .. entry.label end,
    M.view_hint_entries(group, command_set, context)), " | ")
end

---Resolve hints from the same registered command specifications and configured keys as bindings.
---@param group string
---@param command_set ForgeViewCommandSet
---@param context? table
---@param surface? string
---@return {id: string, key: string, label: string}[]
function M.view_hint_entries(group, command_set, context, surface)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  local segments = {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    local key = M.view_keys_for(group, command_id)[1]
    if action and spec and key and (not action.enabled or action.enabled(context)) then
      local label
      if surface then
        if spec.hints and spec.hints[surface] then label = spec.hints[surface](context) end
      elseif spec.pinned then label = spec.label end
      if label then segments[#segments + 1] = { id = command_id, key = key, label = label } end
    end
  end
  return segments
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
    local hint = M.render_hintbar(M.view_hint_entries(group, command_set, context),
      vim.api.nvim_win_get_width(win), { inline = true })
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
  local help_hint = help_key and M.render_hintbar({ { id = "help", key = help_key, label = "help" } },
    vim.api.nvim_win_get_width(win), { inline = true }) or ""
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
  local bindings = {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    local keys = M.view_keys_for(group, command_id)
    if action and spec and #keys > 0 and (not action.enabled or action.enabled(context)) then
      bindings[#bindings + 1] = { keys = keys, spec = spec }
    end
  end
  M.show_bindings_help(bindings, title)
end

---Show registered bindings with the shared Forge help layout.
---@param bindings {keys: string[], spec: {desc: string}}[]
---@param title string
function M.show_bindings_help(bindings, title)
  local lines, key_width, width = {}, 0, 1
  for _, binding in ipairs(bindings) do
    key_width = math.max(key_width, vim.fn.strdisplaywidth(table.concat(binding.keys, ", ")))
  end
  for _, binding in ipairs(bindings) do
    local keys = table.concat(binding.keys, ", ")
    local padding = string.rep(" ", key_width - vim.fn.strdisplaywidth(keys) + 2)
    lines[#lines + 1] = "  " .. keys .. padding .. binding.spec.desc
    width = math.max(width, vim.fn.strdisplaywidth(lines[#lines]) + 2)
  end
  local buf, win = popup_window.open({ title = title, relative = "editor",
    width = math.max(1, math.min(width, vim.o.columns - 4)), height = math.max(1, math.min(#lines, vim.o.lines - 4)),
    filetype = "ForgeHelp" })
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, lines)
  vim.bo[buf].modifiable = false
  vim.keymap.set("n", "q", function() popup_window.close(win) end,
    { buffer = buf, silent = true, nowait = true, desc = "Close help" })
end

return M
