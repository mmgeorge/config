local config = require("forge.infra.config")
local specs = require("forge.shared.command_specs")
local popup = require("forge.infra.popup_window")
local buffer = require("forge.buffer")
local gutter = require("forge.gutter")

local M = {}
local window_owner = {}

local function keys_for(spec, overrides)
  local group = spec.keymap or "status"
  local options = config.options or config.defaults
  local configured = options.keymaps and options.keymaps[group] or {}
  local value = configured[spec.id]
  if value == nil then value = config.defaults.keymaps[group][spec.id] end
  if overrides and overrides[spec.id] ~= nil then value = overrides[spec.id] end
  if value == false or value == nil then return {} end
  return type(value) == "table" and value or { value }
end

local function selected_rows(session)
  assert(session.status == "Applied" and not session.applying, "document changed during selection")
  assert(not require("forge.editable").suspend_generated_text(session.editable), "local edits require acknowledgement before copying gutters")
  local anchor, cursor = vim.fn.getpos("v")[2], vim.api.nvim_win_get_cursor(0)[1]
  local first, last = math.min(anchor, cursor), math.max(anchor, cursor)
  local source_bytes = vim.api.nvim_buf_get_offset(session.buffer, last)
    - vim.api.nvim_buf_get_offset(session.buffer, first - 1)
  assert(source_bytes >= 0 and source_bytes <= 16 * 1024 * 1024, "selected source exceeds 16 MiB")
  local rows = vim.api.nvim_buf_get_lines(session.buffer, first - 1, last, true)
  local metadata, bytes = {}, 0
  for index, text in ipairs(rows) do
    local location = buffer.locate(session, first + index - 2, 0)
    if location then
      local gutter = metadata[location.block]
      if not gutter then
        gutter = {}
        for _, entry in ipairs(session.block[location.block].metadata.gutter or {}) do
          gutter[entry.position.row] = gutter[entry.position.row] or {}
          gutter[entry.position.row][#gutter[entry.position.row] + 1] = entry
        end
        metadata[location.block] = gutter
      end
      local fragment, column = {}, 0
      local row_gutter = vim.list_extend({}, gutter[location.position.row] or {})
      table.sort(row_gutter, function(left, right) return left.position.column < right.position.column end)
      for _, entry in ipairs(row_gutter) do
        fragment[#fragment + 1] = text:sub(column + 1, entry.position.column)
        for _, chunk in ipairs(entry.chunk) do fragment[#fragment + 1] = chunk.text end
        column = entry.position.column
      end
      fragment[#fragment + 1] = text:sub(column + 1)
      rows[index] = table.concat(fragment)
    end
    bytes = bytes + #rows[index] + 1
    assert(bytes <= 16 * 1024 * 1024, "selected source and gutters exceed 16 MiB")
  end
  return rows
end

function M.attach(session, options)
  local handler = vim.tbl_extend("force", {
    toggle = function()
      if vim.fn.foldlevel(".") > 0 then vim.cmd("normal! za") end
      if options.changed then options.changed() end
    end,
    collapse_parent = function()
      if vim.fn.foldlevel(".") > 0 then vim.cmd("normal! zc") end
      if options.changed then options.changed() end
    end,
  }, options.handler or {})
  local owner = { binding = {}, mapping = {}, selection = false, window = {} }
  local group = vim.api.nvim_create_augroup("ForgeDocumentCommands" .. session.buffer, { clear = true })
  local prior_clipboard, clipboard_callback
  local function clear_selection()
    if not owner.selection then return end
    owner.selection = false
    session.gutter_selection = nil
    if not vim.api.nvim_buf_is_valid(session.buffer) then return end
    vim.api.nvim_buf_call(session.buffer, function()
      local current = vim.fn.maparg("<Space>l", "x", false, true)
      if current.buffer == 1 and current.callback == clipboard_callback then
        pcall(vim.api.nvim_buf_del_keymap, session.buffer, "x", "<Space>l")
        if prior_clipboard and prior_clipboard.buffer == 1 then vim.fn.mapset("x", false, prior_clipboard) end
      end
    end)
    prior_clipboard, clipboard_callback = nil, nil
    gutter.normalize(session, false)
  end
  handler.visual_line_with_gutter = handler.visual_line_with_gutter or function()
    clear_selection()
    prior_clipboard = vim.fn.maparg("<Space>l", "x", false, true)
    vim.cmd("normal! V")
    owner.selection = true
    gutter.normalize(session, true)
    clipboard_callback = function()
      local ok, result = pcall(selected_rows, session)
      if not ok then vim.notify(result, vim.log.levels.ERROR) return end
      vim.fn.setreg("+", result, "V")
      vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
      clear_selection()
    end
    vim.keymap.set("x", "<Space>l", clipboard_callback,
      { buffer = session.buffer, silent = true, desc = "Copy selected source with gutters" })
  end
  handler.help = handler.help or function()
    local line, key_width, width = {}, 0, 1
    for _, binding in ipairs(owner.binding) do
      key_width = math.max(key_width, vim.fn.strdisplaywidth(table.concat(binding.keys, ", ")))
    end
    for _, binding in ipairs(owner.binding) do
      local keys = table.concat(binding.keys, ", ")
      local padding = string.rep(" ", key_width - vim.fn.strdisplaywidth(keys) + 2)
      line[#line + 1] = "  " .. keys .. padding .. binding.spec.desc
      width = math.max(width, vim.fn.strdisplaywidth(line[#line]) + 2)
    end
    local help_buffer, window = popup.open({ title = "Forge Commands", relative = "editor",
      width = math.max(1, math.min(width, vim.o.columns - 4)), height = math.max(1, math.min(#line, vim.o.lines - 4)),
      filetype = "ForgeHelp" })
    vim.api.nvim_buf_set_lines(help_buffer, 0, -1, true, line)
    vim.bo[help_buffer].modifiable = false
    vim.keymap.set("n", "q", function() popup.close(window) end, { buffer = help_buffer, silent = true, desc = "Close help" })
  end
  for _, spec in ipairs(specs.specs) do
    if (not spec.views or spec.views[options.view]) and handler[spec.id] then
      local keys = keys_for(spec, options.keymaps)
      if #keys > 0 then
        owner.binding[#owner.binding + 1] = { spec = spec, keys = keys }
        for _, key in ipairs(keys) do
          local mapped = function()
            if spec.id ~= "close" and spec.id ~= "help"
              and (session.status ~= "Applied" or session.applying) then return end
            local mode = vim.api.nvim_get_mode().mode
            handler[spec.id](mode == "v" or mode == "V" or mode == "\22")
          end
          local modes = spec.modes
          if type(modes) == "string" then modes = { modes } end
          for _, mode in ipairs(modes) do
            local prior = vim.api.nvim_buf_call(session.buffer, function() return vim.fn.maparg(key, mode, false, true) end)
            owner.mapping[#owner.mapping + 1] = { key = key, mode = mode, callback = mapped, prior = prior }
          end
          vim.keymap.set(modes, key, mapped, { buffer = session.buffer, silent = true, nowait = true, desc = spec.desc })
        end
      end
    end
  end
  local hint = {}
  for _, id in ipairs(specs.hint_command_ids_by_view[options.view] or {}) do
    for _, binding in ipairs(owner.binding) do
      if binding.spec.id == id and binding.spec.pinned then
        hint[#hint + 1] = { id = id, key = binding.keys[1], label = binding.spec.label }
        break
      end
    end
  end
  function owner.release_window(window)
    local previous = owner.window[window]
    if previous and vim.api.nvim_win_is_valid(window) and vim.wo[window].winbar == previous.value then
      vim.wo[window].winbar = previous.winbar
    end
    owner.window[window] = nil
    if window_owner[window] == owner then window_owner[window] = nil end
  end
  local function hint_winbar(window)
    local title = options.title or "Forge"
    local width = vim.api.nvim_win_get_width(window)
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
    local parts = { "%#ForgeStatusLabel#", title:gsub("%%", "%%%%"), narrow and "%*" or "%*%<%=" }
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
  local function update_winbar()
    if owner.closed or options.winbar == false or #hint == 0 then return end
    for window in pairs(owner.window) do
      if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then owner.release_window(window) end
    end
    for _, window in ipairs(vim.fn.win_findbuf(session.buffer)) do
      if window_owner[window] and window_owner[window] ~= owner then window_owner[window].release_window(window) end
      local previous = owner.window[window]
      if not previous then
        local prior = vim.wo[window].winbar
        for _, attached in pairs(owner.window) do
          if prior == attached.value then prior = attached.winbar break end
        end
        previous = { winbar = prior, value = vim.wo[window].winbar }
        owner.window[window], window_owner[window] = previous, owner
      end
      if vim.wo[window].winbar == previous.value then
        previous.value = hint_winbar(window)
        vim.wo[window].winbar = previous.value
      end
    end
  end
  vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter", "WinEnter", "WinResized" }, { group = group, callback = update_winbar })
  vim.api.nvim_create_autocmd("BufWinLeave", { group = group, buffer = session.buffer,
    callback = function() owner.release_window(vim.api.nvim_get_current_win()) end })
  vim.api.nvim_create_autocmd("WinClosed", { group = group,
    callback = function(event) owner.release_window(tonumber(event.match)) end })
  update_winbar()
  vim.api.nvim_create_autocmd("ModeChanged", { group = group, callback = function()
    local mode = vim.api.nvim_get_mode().mode
    if mode ~= "v" and mode ~= "V" and mode ~= "\22" then clear_selection() end
  end })
  vim.api.nvim_create_autocmd({ "CursorMoved", "BufEnter", "WinEnter" }, {
    group = group, buffer = session.buffer, callback = function() gutter.normalize(session, owner.selection) end,
  })
  vim.api.nvim_create_autocmd("BufLeave", { group = group, buffer = session.buffer, callback = clear_selection })
  vim.api.nvim_create_autocmd("BufWipeout", { group = group, buffer = session.buffer, callback = function() owner.close() end })
  function owner.close()
    owner.closed = true
    for window in pairs(owner.window) do owner.release_window(window) end
    clear_selection()
    pcall(vim.api.nvim_del_augroup_by_id, group)
    if vim.api.nvim_buf_is_valid(session.buffer) then
      vim.api.nvim_buf_call(session.buffer, function()
        for _, mapping in ipairs(owner.mapping) do
          local current = vim.fn.maparg(mapping.key, mapping.mode, false, true)
          if current.buffer == 1 and current.callback == mapping.callback then
            pcall(vim.api.nvim_buf_del_keymap, session.buffer, mapping.mode, mapping.key)
            if mapping.prior.buffer == 1 then vim.fn.mapset(mapping.mode, false, mapping.prior) end
          end
        end
      end)
    end
    owner.mapping = {}
  end
  return owner
end

return M
