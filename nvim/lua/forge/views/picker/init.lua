local Picker = {}

local command_set = require("forge.shared.view_command_set")
local config = require("forge.infra.config")
local input = require("forge.views.picker.input")
local keymaps = require("forge.shared.keymaps")
local layout = require("forge.views.picker.layout")
local notifications = require("forge.infra.notifications")
local popup_window = require("forge.infra.popup_window")
local render = require("forge.views.picker.render")
local search_input = require("forge.views.picker.search_input")
local picker_state = require("forge.views.picker.state")
local picker_window = require("forge.views.picker.window")
local page_navigation = require("forge.views.picker.page_navigation")
local field_input = require("forge.views.picker.field_input")

local active = nil
local install_action_keymaps
local resize_group = vim.api.nvim_create_augroup("ForgePickerLayout", { clear = true })

local function valid_window(win)
  return win and vim.api.nvim_win_is_valid(win)
end

local function picker_keys(instance, command)
  return keymaps.view_keys_for("picker", command, instance.spec.keymaps)
end

local function close_field_editor(instance)
  local editor = instance.field_editor
  if not editor then return end
  instance.field_editor = nil
  field_input.close(editor)
end

local function fixed_dimension(size, ratio)
  assert(type(ratio) == "number" and ratio > 0 and ratio <= 1,
    "picker size ratio must be greater than zero and at most one")
  return math.max(1, math.min(size - 2, math.floor(size * ratio) - 2))
end

local function window_width(spec, host_width)
  if spec.width_ratio then return fixed_dimension(host_width, spec.width_ratio) end
  return spec.size_ratio and fixed_dimension(host_width, spec.size_ratio) or math.max(30, host_width - 2)
end

---@param spec { size_ratio?: number, height_ratio?: number, fit_content?: boolean }
---@param content_height integer
---@param host_height integer
---@return integer
local function window_height(spec, content_height, host_height)
  if spec.size_ratio then return fixed_dimension(host_height, spec.size_ratio) end
  local available = math.max(4, host_height - 2)
  if spec.height_ratio ~= nil then
    assert(type(spec.height_ratio) == "number" and spec.height_ratio > 0 and spec.height_ratio <= 1,
      "picker height ratio must be greater than zero and at most one")
    available = math.min(available, math.max(4, math.floor(host_height * spec.height_ratio)))
  end
  return math.min(content_height, spec.fit_content and available or config.options.picker.max_height, available)
end

local function title_chunks(instance, width)
  local page = picker_state.page(instance.state, instance.spec)
  local title = instance.spec.page_navigation and instance.spec.title or page.title or instance.spec.title or "Select"
  local counter = not instance.spec.page_navigation and #instance.spec.page_list > 1
      and (" %d/%d "):format(instance.state.page_index, #instance.spec.page_list)
    or ""
  local chunks = {
    { "  " .. title .. " ", "ForgePickerSection" },
    { counter, "ForgePickerHint" },
  }
  local right = page.header_right or instance.spec.header_right
  local deadline = instance.spec.deadline
  if type(deadline) == "table" and type(deadline.expires_at_ms) == "number" then
    if instance.deadline_ms ~= deadline.expires_at_ms then
      instance.deadline_ms = deadline.expires_at_ms
      instance.deadline_origin = vim.uv.hrtime()
      local seconds, microseconds = vim.uv.gettimeofday()
      instance.deadline_remaining = math.max(0, deadline.expires_at_ms - seconds * 1000 - microseconds / 1000)
    end
    local seconds = math.max(0, math.ceil((instance.deadline_remaining - (vim.uv.hrtime() - instance.deadline_origin) / 1000000) / 1000))
    if seconds >= 3600 then
      right = ("%d:%02d:%02d"):format(math.floor(seconds / 3600), math.floor(seconds / 60) % 60, seconds % 60)
    else
      right = ("%02d:%02d"):format(math.floor(seconds / 60), seconds % 60)
    end
  end
  if right and right ~= "" then
    local left_width = vim.fn.strdisplaywidth("  " .. title .. " " .. counter)
    local available = math.max(0, width - left_width - 3)
    if available > 0 then
      if vim.fn.strdisplaywidth(right) > available then
        local count = vim.fn.strchars(right)
        repeat count = count - 1 until count == 0 or vim.fn.strdisplaywidth(vim.fn.strcharpart(right, 0, count)) <= available - 1
        right = vim.fn.strcharpart(right, 0, count) .. "…"
      end
      chunks[#chunks + 1] = { string.rep("─", math.max(1, width - left_width - vim.fn.strdisplaywidth(right) - 2)), "FloatBorder" }
      chunks[#chunks + 1] = { " " .. right .. " ", "ForgePickerHint" }
    end
  end
  return chunks
end

local function update_deadline_timer(instance)
  if type(instance.spec.deadline) ~= "table" then
    if instance.timer then instance.timer:stop() instance.timer:close() instance.timer = nil end
    instance.deadline_ms = nil
    return
  end
  if instance.timer then return end
  instance.timer = vim.uv.new_timer()
  vim.uv.update_time()
  instance.timer:start(1000, 1000, vim.schedule_wrap(function()
    if active == instance and valid_window(instance.win) then
      local width = vim.api.nvim_win_get_width(instance.win)
      vim.api.nvim_win_set_config(instance.win, { title = title_chunks(instance, width), title_pos = "left" })
    end
  end))
end

local function action_list(instance, input_only)
  local actions = vim.list_extend(vim.deepcopy(instance.spec.action_list or {}),
    vim.deepcopy(picker_state.page(instance.state, instance.spec).action_list or {}))
  vim.list_extend(actions, vim.deepcopy(instance.spec.host.action_list or {}))
  return input_only and vim.tbl_filter(function(action) return action.input == true end, actions) or actions
end

local function input_footer(instance)
  local segment_list = {}
  local input_actions = action_list(instance, true)
  local overridden = {}
  for _, action in ipairs(input_actions) do
    for _, key in ipairs(action.command and picker_keys(instance, action.command) or { action.key }) do
      overridden[key] = true
    end
  end
  for _, command in ipairs({
    { id = "submit_input", label = "submit" },
    { id = "feedback", label = "options" },
    { id = "clear_input", label = "clear" },
  }) do
    local key = picker_keys(instance, command.id)[1]
    if key and not overridden[key] then segment_list[#segment_list + 1] = key .. " " .. command.label end
  end
  for _, action in ipairs(input_actions) do
    local key = action.command and picker_keys(instance, action.command)[1] or action.key
    if key then segment_list[#segment_list + 1] = key .. " " .. (action.label or action.desc or action.id or "action") end
  end
  return table.concat(segment_list, "  ")
end

local function clear_modal_keymaps(instance)
  for _, mapping in ipairs(instance.mapping_list) do
    if vim.api.nvim_buf_is_valid(mapping.buf) then
      pcall(vim.keymap.del, "n", mapping.key, { buffer = mapping.buf })
      if mapping.previous and next(mapping.previous) then
        vim.api.nvim_buf_call(mapping.buf, function() vim.fn.mapset("n", false, mapping.previous) end)
      end
    end
  end
  instance.mapping_list = {}
end

local function preserve_mapping(instance, buf, key)
  if vim.iter(instance.mapping_list):any(function(mapping) return mapping.key == key end) then return end
  local previous = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  instance.mapping_list[#instance.mapping_list + 1] = {
    buf = buf,
    key = key,
    previous = previous,
  }
end

local function close_input(instance)
  if instance.input_buf and vim.api.nvim_buf_is_valid(instance.input_buf) then
    local page = picker_state.page(instance.state, instance.spec)
    instance.state.draft_by_page[page.id] = input.text(instance.input_buf)
    if instance.spec.save_input_draft then instance.spec.save_input_draft(page.id, instance.input_kind or "feedback", input.text(instance.input_buf)) end
  end
  popup_window.close(instance.input_win, false)
  instance.input_buf = nil
  instance.input_win = nil
  instance.input_kind = nil
  instance.input_visible = false
end

local function close_search(instance)
  popup_window.close(instance.search_win, false)
  instance.search_buf = nil
  instance.search_win = nil
end

---@class ForgePickerSelectionContext
---@field page table
---@field option? table
---@field field? ForgePickerField
---@field query string
---@field preview_height integer Visible host rows above the picker.

---@param instance table
---@return ForgePickerSelectionContext
local function selection_context(instance)
  local bounds = layout.host_bounds(instance.spec.host.window_list)
  return {
    page = picker_state.page(instance.state, instance.spec),
    option = picker_state.selected_option(instance.state, instance.spec),
    field = picker_state.selected_field(instance.state, instance.spec),
    query = picker_state.query(instance.state, instance.spec),
    preview_height = math.max(1, bounds.height - vim.api.nvim_win_get_height(instance.win) - 2),
  }
end

---@param instance table
---@param force? boolean
local function notify_selection(instance, force)
  if not instance.spec.on_change then return end
  local context = selection_context(instance)
  local identity = context.page.id .. ":" .. (context.option and tostring(context.option.id or context.option.value or context.option.label) or "")
    .. (context.field and (":" .. context.field.id) or "")
  if not force and instance.notified_identity == identity
    and instance.notified_preview_height == context.preview_height then return end
  instance.notified_identity = identity
  instance.notified_preview_height = context.preview_height
  instance.spec.on_change(context, Picker)
end

--- Resolve category keys without shadowing row, field, or close commands.
function Picker._navigation_keys(instance)
  local reserved = {}
  local result = vim.tbl_extend("force", {}, instance.spec.keymaps or {}, { page_previous = {}, page_next = {} })
  local page = picker_state.page(instance.state, instance.spec)
  if page.search then reserved["/"] = true end
  local command_list = { "previous", "next", "select", "scroll_previous", "scroll_next", "focus_input", "close", "help" }
  if not page.field_navigation then command_list[#command_list + 1] = "feedback" end
  if page.field_navigation then
    vim.list_extend(command_list, { "next_field", "previous_field", "previous_value", "next_value", "toggle_field" })
  end
  for _, command in ipairs(command_list) do
    for _, key in ipairs(picker_keys(instance, command)) do
      reserved[vim.api.nvim_replace_termcodes(key, true, true, true)] = true
    end
  end
  for _, command in ipairs({ "page_previous", "page_next" }) do
    for _, key in ipairs(picker_keys(instance, command)) do
      local encoded = vim.api.nvim_replace_termcodes(key, true, true, true)
      if not reserved[encoded] then
        result[command][#result[command] + 1] = key
        reserved[encoded] = true
      end
    end
  end
  return result
end

--- Build configuration hints from the same effective bindings used by the commands.
function Picker._navigation_footer(instance)
  local category = Picker._navigation_keys(instance)
  local parts = {}
  local category_keys = vim.list_extend(vim.deepcopy(category.page_previous), category.page_next)
  if #category_keys > 0 then parts[#parts + 1] = table.concat(category_keys, "/") .. " category" end
  local page = picker_state.page(instance.state, instance.spec)
  local multiple_fields = false
  for _, option in ipairs(page.option_list) do
    if #(option.fields or {}) > 1 then multiple_fields = true break end
  end
  for _, entry in ipairs({
    { "previous", "row" }, { "next_field", "field" }, { "next_value", "value" },
    { "toggle_field", "toggle" }, { "select", page.toggle_on_confirm and "toggle/edit" or "edit/open" }, { "close", "close" },
  }) do
    local key = picker_keys(instance, entry[1])[1]
    if key and (entry[1] ~= "next_field" or multiple_fields)
      and (entry[1] ~= "toggle_field" or not page.toggle_on_confirm) then
      parts[#parts + 1] = key .. " " .. entry[2]
    end
  end
  return table.concat(parts, "  ")
end

--- Show the effective Normal-mode bindings, including aliases and caller actions.
local function show_help(instance)
  if valid_window(instance.help_win) then
    vim.api.nvim_set_current_win(instance.help_win)
    return
  end
  local page = picker_state.page(instance.state, instance.spec)
  local bindings, by_description = {}, {}
  local confirm_keys = picker_keys(instance, "select")
  for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(instance.buf, "n")) do
    local description = mapping.desc
    if page.toggle_on_confirm and vim.tbl_contains(confirm_keys, mapping.lhs) then
      description = "Toggle, edit, or open the selected setting"
    end
    if description and description ~= "" then
      local binding = by_description[description]
      if not binding then
        binding = { keys = {}, spec = { desc = description } }
        by_description[description] = binding
        bindings[#bindings + 1] = binding
      end
      binding.keys[#binding.keys + 1] = mapping.lhs == " " and "<Space>" or mapping.lhs
    end
  end
  table.sort(bindings, function(first, second) return first.spec.desc < second.spec.desc end)
  for _, binding in ipairs(bindings) do table.sort(binding.keys) end
  instance.help_win = keymaps.show_bindings_help(bindings, (instance.spec.title or page.title or "Picker") .. " help",
    { zindex = 100, wrap = true })
end

local function render_view(instance)
  if not (valid_window(instance.win) and vim.api.nvim_buf_is_valid(instance.buf)) then return end
  local page = picker_state.page(instance.state, instance.spec)
  local bounds = layout.host_bounds(instance.spec.host.window_list)
  local width = window_width(instance.spec, bounds.width)
  local selected = picker_state.selected_index(instance.state, instance.spec)
  if instance.field_editor then
    local option = picker_state.selected_option(instance.state, instance.spec)
    if instance.field_editor.page_id ~= page.id or not option or option.id ~= instance.field_editor.option_id then
      close_field_editor(instance)
    end
  end
  local input_visible = instance.input_visible == true
  local help_only = instance.spec.hint_mode == "help"
  local footer = help_only and (table.concat(picker_keys(instance, "help"), "/") .. " help")
    or instance.spec.page_navigation and Picker._navigation_footer(instance) or page.footer
  if not input_visible then
    local footer_actions = help_only and action_list(instance) or (instance.spec.host.action_list or {})
    for _, action in ipairs(footer_actions) do
      local key = action.command and picker_keys(instance, action.command)[1] or action.key
      if key and action.label and (not help_only or action.show_in_help_footer) then
        footer = (footer or "") .. "  " .. key .. " " .. action.label
      end
    end
  end
  local available_height = window_height(instance.spec, math.huge, bounds.height)
  local geometry = page_navigation.measure(instance.spec.page_list, instance.spec.page_navigation, width, available_height)
  local wrap_footer = instance.spec.page_navigation
    and vim.fn.strdisplaywidth(footer or "") > geometry.content_width - 4
  local frame = layout.build(page, selected, geometry.content_width, {
    input_visible = input_visible,
    search_visible = (not page.search or not page.search.start_in_normal
      or instance.search_visible or valid_window(instance.search_win)) == true,
    footer = input_visible and input_footer(instance) or footer,
    wrap_footer = wrap_footer,
    field_by_option = instance.state.field_by_page[page.id] or {},
    field_message = instance.field_editor and (instance.field_editor.error
      or instance.field_editor.pending and "Saving setting…") or nil,
  })
  frame.chosen_index_set = {}
  frame.option_highlight_by_index = {}
  local selected_set = instance.state.selected_set_by_page[page.id] or {}
  for index, option in ipairs(page.option_list) do
    local option_id = tostring(option.id or option.value or index)
    if selected_set[option_id] then frame.chosen_index_set[index] = true end
    if option.highlight_group then
      frame.option_highlight_by_index[index] = {
        group = option.highlight_group,
        text = option.highlight_text or option.label,
      }
    end
  end
  frame.input_height = page.input_height or config.options.picker.input_height
  local height = window_height(instance.spec, math.max(#frame.lines + geometry.header_height, geometry.minimum_height), bounds.height)
  local border_footer
  if not wrap_footer and (#frame.lines > height - geometry.header_height or instance.spec.page_navigation) then
    border_footer = vim.trim(table.remove(frame.lines, frame.footer_line))
    frame.footer_line = nil
  end
  local previous = instance.frame and instance.frame.page_id == page.id and instance.frame or nil
  local option_offset = previous and previous.selected_index == selected
    and (instance.option_offset or previous.option_offset) or 0
  frame = layout.viewport(frame, math.max(1, height - geometry.header_height), previous and previous.viewport_top,
    instance.content_offset or (previous and previous.content_offset),
    option_offset, instance.spec.size_ratio ~= nil)
  frame = page_navigation.compose(frame, geometry, instance.spec.page_list, instance.state.page_index)
  frame.page_id = page.id
  instance.content_offset = nil
  instance.option_offset = nil
  instance.frame = frame
  if frame.option_scroll_max > 0 and not help_only then
    local scroll_keys = vim.list_extend(picker_keys(instance, "scroll_previous"),
      picker_keys(instance, "scroll_next"))
    border_footer = table.concat(scroll_keys, "/") .. " scroll  " .. (border_footer or footer or "")
  end
  picker_window.resize(instance.win, instance.spec.host, width, height, title_chunks(instance, width), border_footer,
    instance.spec.placement or (instance.spec.size_ratio and "center" or nil))
  render.apply(instance.buf, frame)
  if instance.field_editor and not field_input.resize(instance.field_editor, instance.win, frame) then
    close_field_editor(instance)
  end
  if valid_window(instance.search_win) then search_input.resize(instance.search_win, instance.win, frame) end
  if valid_window(instance.input_win) then input.resize(instance.input_win, instance.win, frame) end
  popup_window.set_cursor_hidden(instance.win, true)
  popup_window.set_focusable(instance.win, true)
  local selected_range = frame.option_range[selected]
  if selected_range then
    vim.api.nvim_win_set_cursor(instance.win, { selected_range.first,
      frame.body_start_by_line and frame.body_start_by_line[selected_range.first] or 0 })
    vim.api.nvim_win_call(instance.win, function() vim.fn.winrestview({ topline = 1 }) end)
  else
    vim.api.nvim_win_set_cursor(instance.win, { 1, 0 })
    vim.api.nvim_win_call(instance.win, function()
      vim.fn.winrestview({ topline = 1, lnum = 1, col = 0, curswant = 0 })
    end)
  end
  notify_selection(instance)
end

---@param pages integer
function Picker.scroll_content(pages)
  if not active then return end
  if active.frame.scroll_range then
    active.content_offset = active.frame.content_offset + pages * active.frame.content_page_size
  elseif active.frame.option_scroll_max > 0 then
    active.option_offset = active.frame.option_offset + pages * active.frame.option_page_size
  else
    return
  end
  render_view(active)
end

local function finish(instance, result)
  if instance ~= active then return end
  local callback = instance.spec.on_confirm
  local should_close = nil
  if callback then should_close = callback(result, Picker) end
  if should_close ~= false and instance == active then Picker.close(false) end
end

local function submit_input(instance)
  local page = picker_state.page(instance.state, instance.spec)
  local option = picker_state.selected_option(instance.state, instance.spec)
  local text = input.text(instance.input_buf)
  if (instance.input_kind == "other" or instance.input_kind == "ask") and text == "" then
    notifications.warn("Enter a value before submitting this option.", "Picker")
    return
  end
  instance.state.draft_by_page[page.id] = text
  if instance.spec.save_input_draft then instance.spec.save_input_draft(page.id, instance.input_kind or "feedback", text) end
  finish(instance, { page = page, option = option, text = text, input_kind = instance.input_kind })
end

local function set_picker_cursor_hidden(instance, hidden)
  local visible_guicursor = instance.guicursor ~= "" and instance.guicursor or "a:block-Cursor"
  vim.o.guicursor = hidden and "n:block-ForgeHiddenCursor" or visible_guicursor
end

local function focus_picker(instance)
  if not valid_window(instance.win) then return end
  vim.cmd("stopinsert")
  set_picker_cursor_hidden(instance, true)
  vim.api.nvim_set_current_win(instance.win)
end

local function after_input_mapping(instance, callback)
  if vim.api.nvim_get_mode().mode:sub(1, 1) ~= "i" then
    callback()
    return
  end
  vim.schedule(function()
    if active == instance then callback() end
  end)
end

local function enter_insert_mode(instance, target_win)
  vim.cmd("startinsert")
  vim.defer_fn(function()
    if active == instance and valid_window(target_win) and vim.api.nvim_get_current_win() == target_win then
      vim.cmd("startinsert")
    end
  end, 10)
end

local function focus_input(instance, enter_insert)
  if not valid_window(instance.input_win) then return end
  vim.cmd("stopinsert")
  set_picker_cursor_hidden(instance, false)
  vim.api.nvim_set_current_win(instance.input_win)
  if enter_insert then enter_insert_mode(instance, instance.input_win) end
end

local function clear_input(instance)
  if instance.input_buf and vim.api.nvim_buf_is_valid(instance.input_buf) then
    vim.api.nvim_buf_set_lines(instance.input_buf, 0, -1, false, { "" })
  end
  close_input(instance)
  render_view(instance)
  focus_picker(instance)
end

local function open_input(instance, kind)
  local page = picker_state.page(instance.state, instance.spec)
  if not page.allow_input then return end
  if valid_window(instance.input_win) then
    if instance.input_kind ~= kind and instance.spec.read_input_draft then
      if instance.spec.save_input_draft then instance.spec.save_input_draft(page.id, instance.input_kind or "feedback", input.text(instance.input_buf)) end
      vim.api.nvim_buf_set_lines(instance.input_buf, 0, -1, false, vim.split(instance.spec.read_input_draft(page.id, kind or "feedback") or "", "\n", { plain = true }))
    end
    instance.input_kind = kind
    focus_input(instance, true)
    return
  end
  instance.input_kind = kind
  instance.input_visible = true
  render_view(instance)
  instance.input_buf, instance.input_win = input.open(
    instance.win,
    instance.frame,
    instance.spec.read_input_draft and instance.spec.read_input_draft(page.id, kind or "feedback") or instance.state.draft_by_page[page.id] or "",
    instance.origin
  )
  for _, key in ipairs(picker_keys(instance, "submit_input")) do
    vim.keymap.set({ "n", "i" }, key, function() submit_input(instance) end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Submit picker input",
    })
  end
  for _, key in ipairs(picker_keys(instance, "focus_input")) do
    vim.keymap.set("n", key, function() focus_picker(instance) end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Return to picker options",
    })
  end
  for _, key in ipairs(picker_keys(instance, "feedback")) do
    vim.keymap.set({ "n", "i" }, key, function()
      after_input_mapping(instance, function() focus_picker(instance) end)
    end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Return to picker options",
    })
  end
  for _, key in ipairs(picker_keys(instance, "clear_input")) do
    vim.keymap.set({ "n", "i" }, key, function()
      after_input_mapping(instance, function() clear_input(instance) end)
    end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Clear and close picker input",
    })
  end
  install_action_keymaps(instance, instance.input_buf, false, true)
  focus_input(instance, true)
end

local function start_field_edit(instance, context)
  if instance.field_editor then field_input.focus(instance.field_editor) return end
  local function cancel()
    if instance ~= active or not instance.field_editor or instance.field_editor.pending then return end
    close_field_editor(instance)
    render_view(instance)
    focus_picker(instance)
  end
  local function submit()
    local editor = instance.field_editor
    if instance ~= active or not editor or editor.pending then return end
    local selected = selection_context(instance)
    if not selected.field or selected.field.id ~= editor.field_id or selected.page.id ~= editor.page_id
      or selected.option.id ~= editor.option_id then cancel() return end
    local parsed = field_input.parse(editor, selected.field)
    editor.error = parsed.error or selected.field.disabled_reason
    if editor.error then render_view(instance) field_input.focus(editor) return end
    editor.pending = true
    vim.cmd("stopinsert")
    vim.bo[editor.buffer].modifiable = false
    render_view(instance)
    local settled = false
    local function complete(failure)
      if settled or instance ~= active or instance.field_editor ~= editor then return end
      settled = true
      if failure then
        editor.pending, editor.error = false, failure
        vim.bo[editor.buffer].modifiable = not editor.choice
        render_view(instance)
        field_input.focus(editor)
      else
        close_field_editor(instance)
        render_view(instance)
        focus_picker(instance)
      end
    end
    local success, failure = pcall(instance.spec.on_field_submit, selected, parsed.value, complete)
    if not success then complete(tostring(failure)) end
  end
  instance.field_editor = field_input.open(instance.win, instance.frame, context, { submit = submit, cancel = cancel })
end

local function field_action(instance, action, direction)
  local context = selection_context(instance)
  local field, option = context.field, context.option
  if not field or not option then return false end
  if action == "toggle" and field.kind ~= "toggle" then return true end
  if action == "change" and field.kind ~= "choice" and field.kind ~= "input" then return true end
  if action == "activate" and field.kind == "toggle" then
    if not context.page.toggle_on_confirm then return true end
    action = "toggle"
  end
  local failure = field.disabled_reason or option.disabled_reason
  if failure then notifications.warn(failure, "Picker") return true end
  if action == "activate" and field.kind == "input" and instance.spec.on_field_submit then
    start_field_edit(instance, context)
    return true
  end
  if instance.spec.on_field_action then instance.spec.on_field_action(context, action, direction or 1) end
  return true
end

local function confirm(instance, with_feedback)
  if field_action(instance, "activate") then return end
  local page = picker_state.page(instance.state, instance.spec)
  local option = picker_state.selected_option(instance.state, instance.spec)
  if not option then return end
  if option.disabled_reason then
    notifications.warn(option.disabled_reason, "Picker")
    return
  end
  if page.selection_mode == "multiple" and option.action ~= "submit" then
    picker_state.toggle_selected(instance.state, instance.spec)
    render_view(instance)
  elseif option.input_kind then
    open_input(instance, option.input_kind)
  elseif with_feedback then
    open_input(instance, "feedback")
  else
    -- Ask and Other drafts keep their own submit action when a regular option is chosen.
    local feedback = instance.input_kind == "feedback" and valid_window(instance.input_win)
      and input.text(instance.input_buf) or ""
    finish(instance, {
      page = page,
      option = option,
      selected_option_list = picker_state.selected_option_list(instance.state, instance.spec),
      text = feedback ~= "" and feedback or nil,
      input_kind = feedback ~= "" and instance.input_kind or nil,
    })
  end
end

local install_keymaps
local open_search

local function move_page(instance, delta)
  local search_focused = valid_window(instance.search_win) and vim.api.nvim_get_current_win() == instance.search_win
  if valid_window(instance.search_win) then
    local query = search_input.text(instance.search_buf)
    if query ~= picker_state.query(instance.state, instance.spec) then
      picker_state.set_query(instance.state, instance.spec, query)
    end
  end
  close_input(instance)
  close_search(instance)
  clear_modal_keymaps(instance)
  picker_state.move_page(instance.state, instance.spec, delta)
  instance.search_visible = false
  render_view(instance)
  install_keymaps(instance)
  local page = picker_state.page(instance.state, instance.spec)
  if page.search and (not page.search.start_in_normal or search_focused) then
    instance.search_visible = true
    render_view(instance)
    open_search(instance, search_focused or not page.search.start_in_normal)
  else
    focus_picker(instance)
  end
end

local function move_selection(instance, delta)
  picker_state.move(instance.state, instance.spec, delta)
  render_view(instance)
end

---@param instance table
---@param action table
local function run_action(instance, action)
  if action.callback then action.callback(selection_context(instance), Picker) end
end

---@param instance table
---@param buf integer
---@param preserve boolean
local function install_option_keymaps(instance, buf, preserve)
  local key_set = {}
  for _, page in ipairs(instance.spec.page_list) do
    for _, option in ipairs(page.option_list) do if option.key then key_set[option.key] = true end end
    for _, key in ipairs((page.search and page.search.choice_keys) or {}) do key_set[key] = true end
  end
  for key in pairs(key_set) do
    if not (instance.page_key_set and instance.page_key_set[vim.api.nvim_replace_termcodes(key, true, true, true)]) then
      if preserve then preserve_mapping(instance, buf, key) end
      vim.keymap.set("n", key, function()
        local page = picker_state.page(instance.state, instance.spec)
        for option_index, option in ipairs(page.option_list) do
          if option.key == key then
            instance.state.selected_index_by_page[page.id] = option_index
            render_view(instance)
            if option.confirm_on_key then confirm(instance, false) end
            return
          end
        end
      end, {
        buffer = buf,
        silent = true,
        nowait = true,
        desc = "Select picker option",
      })
    end
  end
end

---@class ForgePickerAction
---@field id? string
---@field key? string
---@field command? string Configured picker command used instead of a literal key.
---@field modes? string|string[]
---@field desc? string
---@field input? boolean Apply this action inside the freeform editor, overriding matching input bindings.
---@field label? string Short label used in picker footers.
---@field show_in_help_footer? boolean Show this action beside help in the compact Normal-mode footer.
---@field callback? fun(context: ForgePickerSelectionContext, picker: table)

---@param instance table
---@param buf integer
---@param preserve boolean
---@param input_only? boolean
install_action_keymaps = function(instance, buf, preserve, input_only)
  for _, action in ipairs(action_list(instance, input_only)) do
    local keys = action.command and picker_keys(instance, action.command) or { action.key }
    for _, key in ipairs(keys) do
      if input_only or not (instance.page_key_set and instance.page_key_set[vim.api.nvim_replace_termcodes(key, true, true, true)]) then
        if preserve then preserve_mapping(instance, buf, key) end
        vim.keymap.set(action.modes or "n", key, function()
          if input_only then after_input_mapping(instance, function() run_action(instance, action) end)
          else run_action(instance, action) end
        end, {
          buffer = buf,
          silent = true,
          nowait = true,
          desc = action.desc or action.id or "Picker action",
        })
      end
    end
  end
end

install_keymaps = function(instance)
  local set = command_set.new()
  command_set.register(set, "help", function() show_help(instance) end)
  command_set.register(set, "previous", function()
    move_selection(instance, -1)
  end)
  command_set.register(set, "next", function()
    move_selection(instance, 1)
  end)
  command_set.register(set, "select", function() confirm(instance, false) end)
  command_set.register(set, "feedback", function() confirm(instance, true) end)
  command_set.register(set, "page_previous", function() move_page(instance, -1) end)
  command_set.register(set, "page_next", function() move_page(instance, 1) end)
  command_set.register(set, "scroll_previous", function() Picker.scroll_content(-1) end)
  command_set.register(set, "scroll_next", function() Picker.scroll_content(1) end)
  command_set.register(set, "focus_input", function() focus_input(instance) end)
  command_set.register(set, "close", function() Picker.close(true) end)
  if picker_state.page(instance.state, instance.spec).field_navigation then
    for _, entry in ipairs({ { "next_field", 1 }, { "previous_field", -1 } }) do
      command_set.register(set, entry[1], function()
        picker_state.move_field(instance.state, instance.spec, entry[2])
        render_view(instance)
      end)
    end
    command_set.register(set, "previous_value", function() field_action(instance, "change", -1) end)
    command_set.register(set, "next_value", function() field_action(instance, "change", 1) end)
    command_set.register(set, "toggle_field", function() field_action(instance, "toggle") end)
  end
  local navigation_keys = instance.spec.page_navigation and Picker._navigation_keys(instance) or instance.spec.keymaps
  for _, command_id in ipairs(set.order) do
    for _, key in ipairs(keymaps.view_keys_for("picker", command_id, navigation_keys)) do
      preserve_mapping(instance, instance.buf, key)
    end
  end
  keymaps.setup_view_keymaps(instance.buf, "picker", set, { keymaps = navigation_keys })
  instance.page_key_set = {}
  for _, key in ipairs(picker_keys(instance, "help")) do
    instance.page_key_set[vim.api.nvim_replace_termcodes(key, true, true, true)] = true
  end
  if navigation_keys then
    for _, command in ipairs({ "page_previous", "page_next" }) do
      local keys = keymaps.view_keys_for("picker", command, navigation_keys)
      for _, key in ipairs(keys) do
        instance.page_key_set[vim.api.nvim_replace_termcodes(key, true, true, true)] = true
      end
    end
  end
  if picker_state.page(instance.state, instance.spec).search then
    preserve_mapping(instance, instance.buf, "/")
    vim.keymap.set("n", "/", function()
      if not valid_window(instance.search_win) then
        instance.search_visible = true
        render_view(instance)
        open_search(instance, true)
        return
      end
      vim.api.nvim_set_current_win(instance.search_win)
      enter_insert_mode(instance, instance.search_win)
    end, { buffer = instance.buf, silent = true, nowait = true, desc = "Search picker options" })
  end
  install_option_keymaps(instance, instance.buf, true)
  install_action_keymaps(instance, instance.buf, true)
end

---@param instance table
local function install_search_keymaps(instance)
  local buf = instance.search_buf
  for _, key in ipairs(picker_keys(instance, "previous")) do
    vim.keymap.set("n", key, function() move_selection(instance, -1) end,
      { buffer = buf, silent = true, nowait = true, desc = "Select previous picker option" })
    if key == "<Up>" then
      vim.keymap.set("i", key, function() move_selection(instance, -1) end,
        { buffer = buf, silent = true, nowait = true, desc = "Select previous picker option" })
    end
  end
  for _, key in ipairs(picker_keys(instance, "next")) do
    vim.keymap.set("n", key, function() move_selection(instance, 1) end,
      { buffer = buf, silent = true, nowait = true, desc = "Select next picker option" })
    if key == "<Down>" then
      vim.keymap.set("i", key, function() move_selection(instance, 1) end,
        { buffer = buf, silent = true, nowait = true, desc = "Select next picker option" })
    end
  end
  for _, key in ipairs(picker_keys(instance, "select")) do
    vim.keymap.set({ "n", "i" }, key, function() confirm(instance, false) end,
      { buffer = buf, silent = true, nowait = true, desc = "Confirm picker option" })
  end
  for _, key in ipairs(picker_keys(instance, "close")) do
    vim.keymap.set("n", key, function() Picker.close(true) end,
      { buffer = buf, silent = true, nowait = true, desc = "Close picker" })
  end
  install_option_keymaps(instance, buf, false)
  install_action_keymaps(instance, buf, false)
end

---@param instance table
open_search = function(instance, focus)
  local page = picker_state.page(instance.state, instance.spec)
  if not page.search then return end
  instance.search_buf, instance.search_win = search_input.open(
    instance.win,
    instance.frame,
    picker_state.query(instance.state, instance.spec),
    instance.origin,
    function(query)
      if instance ~= active then return end
      picker_state.set_query(instance.state, instance.spec, query)
      instance.notified_identity = nil
      render_view(instance)
    end
  )
  if page.search.start_in_normal then
    local search_buf = instance.search_buf
    vim.api.nvim_create_autocmd("InsertLeave", {
      buffer = search_buf,
      callback = function()
        vim.schedule(function()
          if active ~= instance or instance.search_buf ~= search_buf then return end
          if search_input.text(search_buf) ~= "" then return end
          focus_picker(instance)
          close_search(instance)
          instance.search_visible = false
          render_view(instance)
        end)
      end,
      desc = "Hide empty picker search after leaving insert mode",
    })
  end
  install_search_keymaps(instance)
  if focus ~= false then
    set_picker_cursor_hidden(instance, false)
    vim.api.nvim_set_current_win(instance.search_win)
    enter_insert_mode(instance, instance.search_win)
  else
    focus_picker(instance)
  end
end

---@param spec ForgePickerSpec
function Picker.open(spec)
  Picker.close(true)
  spec.host = spec.host or {}
  spec.host.window_list = spec.host.window_list or { spec.host.control_win or vim.api.nvim_get_current_win() }
  local origin = popup_window.capture_origin()
  local control_win = spec.host.control_win or origin.win
  assert(vim.api.nvim_win_is_valid(control_win), "picker control window must be valid")
  local state = picker_state.new(spec)
  local bounds = layout.host_bounds(spec.host.window_list)
  local width = window_width(spec, bounds.width)
  local initial_page = picker_state.page(state, spec)
  local geometry = page_navigation.measure(spec.page_list, spec.page_navigation, width, window_height(spec, math.huge, bounds.height))
  local initial_frame = layout.build(initial_page, picker_state.selected_index(state, spec), geometry.content_width,
    { search_visible = not initial_page.search or not initial_page.search.start_in_normal })
  local height = window_height(spec, math.max(#initial_frame.lines + geometry.header_height, geometry.minimum_height), bounds.height)
  local instance = {
    spec = spec,
    state = state,
    origin = origin,
    guicursor = vim.o.guicursor,
    mapping_list = {},
    input_visible = false,
  }
  instance.buf, instance.win = picker_window.open(spec.host, width, height, title_chunks(instance, width), origin,
    spec.placement or (spec.size_ratio and "center" or nil))
  vim.b[instance.buf].miniclue_disable = true
  active = instance
  update_deadline_timer(instance)
  render_view(instance)
  install_keymaps(instance)
  vim.api.nvim_clear_autocmds({ group = resize_group })
  vim.api.nvim_create_autocmd({ "VimResized", "WinResized" }, {
    group = resize_group,
    callback = function()
      if active == instance then vim.schedule(function() if active == instance then render_view(instance) end end) end
    end,
    desc = "Keep the picker anchored to its host surface",
  })
  focus_picker(instance)
  if picker_state.page(instance.state, instance.spec).search then
    if not picker_state.page(instance.state, instance.spec).search.start_in_normal then
      open_search(instance, true)
    end
  elseif spec.initial_input_kind then
    open_input(instance, spec.initial_input_kind)
  end
  return instance.buf, instance.win
end

---@param spec ForgePickerSpec
function Picker.update(spec)
  if not active then return end
  local search_focused = valid_window(active.search_win) and vim.api.nvim_get_current_win() == active.search_win
  local help_focused = valid_window(active.help_win) and vim.api.nvim_get_current_win() == active.help_win
  local editor_focused = active.field_editor and valid_window(active.field_editor.window)
    and vim.api.nvim_get_current_win() == active.field_editor.window
  if active.field_editor and spec.owner ~= active.spec.owner then close_field_editor(active) end
  local previous_page_id = picker_state.page(active.state, active.spec).id
  local snapshot = assert(Picker.capture_state(), "active picker must provide navigation state")
  local next_state = picker_state.new(spec)
  picker_state.restore(next_state, spec, snapshot)
  local next_page = picker_state.page(next_state, spec)
  local next_option = picker_state.selected_option(next_state, spec)
  local next_option_id = next_option and (next_option.id or next_option.value)
  local retained_input = spec.preserve_input and valid_window(active.input_win)
    and spec.owner == active.spec.owner and next_page.id == previous_page_id and next_page.allow_input
    and next_option_id ~= nil and tostring(next_option_id) == snapshot.selected_option_id_by_page[previous_page_id]
  if not retained_input then close_input(active) end
  clear_modal_keymaps(active)
  active.spec = spec
  update_deadline_timer(active)
  active.state = next_state
  if picker_state.page(next_state, spec).id ~= previous_page_id then close_search(active) end
  active.notified_identity = nil
  render_view(active)
  install_keymaps(active)
  if picker_state.page(active.state, active.spec).search then
    if valid_window(active.search_win) then
      install_search_keymaps(active)
      if not picker_state.page(active.state, active.spec).search.start_in_normal or search_focused then
        vim.api.nvim_set_current_win(active.search_win)
      else
        focus_picker(active)
      end
    else
      if not picker_state.page(active.state, active.spec).search.start_in_normal then
        open_search(active, true)
      end
    end
  else
    close_search(active)
    if not retained_input then
      focus_picker(active)
      if spec.initial_input_kind then open_input(active, spec.initial_input_kind) end
    end
  end
  if help_focused and valid_window(active.help_win) then vim.api.nvim_set_current_win(active.help_win) end
  if editor_focused and active.field_editor then field_input.focus(active.field_editor) end
end

---@param notify? boolean
function Picker.close(notify)
  local instance = active
  if not instance then return end
  local resume = Picker.capture_state()
  if instance.timer then instance.timer:stop() instance.timer:close() instance.timer = nil end
  set_picker_cursor_hidden(instance, false)
  active = nil
  vim.api.nvim_clear_autocmds({ group = resize_group })
  clear_modal_keymaps(instance)
  close_field_editor(instance)
  popup_window.close(instance.help_win, false)
  close_input(instance)
  close_search(instance)
  popup_window.close(instance.win)
  if notify ~= false and instance.spec.on_close then instance.spec.on_close(resume) end
end

---@param owner? string
function Picker.is_open(owner)
  return active ~= nil and valid_window(active.win) and (not owner or active.spec.owner == owner)
end

--- Get detached navigation state for a caller that is about to leave this picker.
---@return ForgePickerResumeState?
function Picker.capture_state()
  if not active then return nil end
  local snapshot = picker_state.capture(active.state, active.spec)
  local page = picker_state.page(active.state, active.spec)
  if valid_window(active.input_win) then
    snapshot.draft_by_page[page.id] = input.text(active.input_buf)
    if active.spec.save_input_draft then active.spec.save_input_draft(page.id, active.input_kind or "feedback", input.text(active.input_buf)) end
  end
  if valid_window(active.search_win) then snapshot.query_by_page[page.id] = search_input.text(active.search_buf) end
  return snapshot
end

function Picker._state_for_test()
  return active
end

return Picker
