local keymaps = require("forge.shared.keymaps")
local specs = require("forge.shared.command_specs")
local buffer = require("forge.buffer")
local gutter = require("forge.gutter")

local M = {}
local window_owner = {}

local function selected_rows(session)
  assert(session.status == "Applied" and not session.applying, "document changed during selection")
  assert(not require("forge.editable").suspend_generated_text(session.editable), "local edits require acknowledgement before copying gutters")
  local anchor, cursor = vim.fn.getpos("v")[2], vim.api.nvim_win_get_cursor(0)[1]
  local first, last = math.min(anchor, cursor), math.max(anchor, cursor)
  local rows = vim.api.nvim_buf_get_lines(session.buffer, first - 1, last, true)
  for index, text in ipairs(rows) do
    local location = session.decoration_location and session.decoration_location(first + index - 2)
      or session.sequence and buffer.locate(session, first + index - 2, 0)
    text = text:sub((location and location.display_indent or 0) + 1)
    rows[index] = text
    local node = location and session.sequence.node[location.block]
    local concealed = {}
    if location and node and vim.wo.conceallevel > 0 and vim.wo.concealcursor:find("v", 1, true) then
      for _, conceal in ipairs(node.entry.metadata.conceal or {}) do
        if conceal.range.start.row == location.position.row and conceal.range["end"].row == location.position.row then
          concealed[#concealed + 1] = conceal
        end
      end
    end
    table.sort(concealed, function(left, right) return left.range.start.column < right.range.start.column end)
    local function visible_text(start_column, end_column)
      local fragment, column = {}, start_column
      for _, conceal in ipairs(concealed) do
        local start = math.max(column, conceal.range.start.column)
        local finish = math.min(end_column, conceal.range["end"].column)
        if start < finish then
          fragment[#fragment + 1] = text:sub(column + 1, start)
          if start == conceal.range.start.column then fragment[#fragment + 1] = conceal.replacement end
          column = finish
        end
      end
      fragment[#fragment + 1] = text:sub(column + 1, end_column)
      return table.concat(fragment)
    end
    local layout = gutter.bounds(session, first + index - 1)
    if layout then
      local fragment, column = {}, 0
      for _, entry in ipairs(layout.insertion) do
        fragment[#fragment + 1] = visible_text(column, entry.position.column)
        for _, chunk in ipairs(entry.chunk) do fragment[#fragment + 1] = chunk.text end
        column = entry.position.column
      end
      fragment[#fragment + 1] = visible_text(column, #text)
      rows[index] = table.concat(fragment)
    else
      rows[index] = visible_text(0, #text)
    end
    local content = location and node and location.position.row == 0 and node.entry.metadata.layout
    local marker = content and node and require("forge.content_layout").marker(node.entry)
    if content and marker and marker ~= vim.NIL and content.indent > 2 then
      local column = content.indent - 4
      rows[index] = rows[index]:sub(1, column) .. marker.text
        .. rows[index]:sub(column + vim.fn.strdisplaywidth(marker.text) + 1)
    end
    if vim.wo.statuscolumn ~= "" then
      local column = vim.api.nvim_eval_statusline(vim.wo.statuscolumn, {
        winid = vim.api.nvim_get_current_win(), use_statuscol_lnum = first + index - 1,
      }).str
      rows[index] = column .. rows[index]
    end
  end
  return rows
end

---Own visual selection and copying of native diff gutters for any document view.
---@param session table
---@param options { normalize: boolean }
---@return table
function M.attach_selection(session, options)
  local normalize_gutter = options.normalize
  local owner = { selection = false }
  local group = vim.api.nvim_create_augroup("ForgeGutterSelection" .. session.buffer, { clear = true })
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
    if normalize_gutter then gutter.normalize(session, false) end
  end
  function owner.start()
    if owner.closed or session.status ~= "Applied" or session.applying then return end
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
  vim.api.nvim_create_autocmd("ModeChanged", { group = group, callback = function()
    local mode = vim.api.nvim_get_mode().mode
    if mode ~= "v" and mode ~= "V" and mode ~= "\22" then clear_selection() end
  end })
  vim.api.nvim_create_autocmd({ "CursorMoved", "BufEnter", "WinEnter" }, {
    group = group, buffer = session.buffer, callback = function()
      if normalize_gutter or owner.selection then gutter.normalize(session, owner.selection) end
    end,
  })
  vim.api.nvim_create_autocmd("BufLeave", { group = group, buffer = session.buffer, callback = clear_selection })
  function owner.close()
    if owner.closed then return end
    owner.closed = true
    clear_selection()
    pcall(vim.api.nvim_del_augroup_by_id, group)
  end
  return owner
end

function M.attach(session, options)
  local normalize_gutter = options.view == "diff" or options.view == "status"
  local handler = vim.tbl_extend("force", {
    toggle = function()
      require("forge.nodes").toggle_heading(session, vim.api.nvim_get_current_win())
      if options.changed then options.changed() end
    end,
    collapse_parent = function()
      require("forge.nodes").collapse_parent(session, vim.api.nvim_get_current_win())
      if options.changed then options.changed() end
    end,
  }, options.handler or {})
  local owner = { binding = {}, mapping = {}, window = {} }
  local group = vim.api.nvim_create_augroup("ForgeDocumentCommands" .. session.buffer, { clear = true })
  local selection = M.attach_selection(session, { normalize = normalize_gutter })
  handler.visual_line_with_gutter = handler.visual_line_with_gutter or selection.start
  handler.help = handler.help or function()
    keymaps.show_bindings_help(owner.binding, "Forge Commands")
  end
  local callbacks = {}
  for _, spec in ipairs(specs.specs) do
    if handler[spec.id] then
      callbacks[spec.id] = function()
        if spec.id ~= "close" and spec.id ~= "help"
          and (session.status ~= "Applied" or session.applying) then return end
        local mode = vim.api.nvim_get_mode().mode
        handler[spec.id](mode == "v" or mode == "V" or mode == "\22")
      end
    end
  end
  local bindings = keymaps.bind_commands(session.buffer, "status", specs.specs, callbacks, options)
  owner.binding, owner.mapping = bindings.binding, bindings.mapping
  function owner.sync_editing()
    if not options.editable or owner.closed then return end
    local editing = options.editable()
    local keep = { browse = true, sync = true, toggle = true, collapse_parent = true, close = true, help = true }
    for _, mapping in ipairs(owner.mapping) do
      local active = not editing or keep[mapping.command] == true
      if mapping.active ~= active then
        if active then
          vim.keymap.set(mapping.mode, mapping.key, mapping.callback, { buffer = session.buffer,
            silent = true, nowait = mapping.nowait, desc = mapping.desc })
        else
          local current = vim.fn.maparg(mapping.key, mapping.mode, false, true)
          if current.buffer == 1 and current.callback == mapping.callback then
            vim.api.nvim_buf_del_keymap(session.buffer, mapping.mode, mapping.key)
            if mapping.prior.buffer == 1 then vim.fn.mapset(mapping.mode, false, mapping.prior) end
          end
        end
        mapping.active = active
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
        previous.value = keymaps.render_hintbar(hint, vim.api.nvim_win_get_width(window),
          { title = options.title or "Forge", narrow_title = options.narrow_title })
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
  vim.api.nvim_create_autocmd("BufWipeout", { group = group, buffer = session.buffer, callback = function() owner.close() end })
  function owner.close()
    owner.closed = true
    for window in pairs(owner.window) do owner.release_window(window) end
    selection.close()
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
  owner.sync_editing()
  return owner
end

return M
