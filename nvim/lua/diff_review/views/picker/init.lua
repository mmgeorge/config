local Picker = {}

local command_set = require("diff_review.shared.view_command_set")
local config = require("diff_review.infra.config")
local input = require("diff_review.views.picker.input")
local keymaps = require("diff_review.shared.keymaps")
local layout = require("diff_review.views.picker.layout")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local render = require("diff_review.views.picker.render")
local search_input = require("diff_review.views.picker.search_input")
local picker_state = require("diff_review.views.picker.state")
local picker_window = require("diff_review.views.picker.window")

local active = nil
local resize_group = vim.api.nvim_create_augroup("DiffReviewPickerLayout", { clear = true })

local function valid_window(win)
  return win and vim.api.nvim_win_is_valid(win)
end

local function title_chunks(instance)
  local page = picker_state.page(instance.state, instance.spec)
  local title = page.title or instance.spec.title or "Select"
  local counter = #instance.spec.page_list > 1
      and (" %d/%d "):format(instance.state.page_index, #instance.spec.page_list)
    or ""
  return {
    { " " .. title .. " ", "DiffReviewPickerSection" },
    { counter, "DiffReviewPickerHint" },
  }
end

local function input_footer()
  local segment_list = {}
  for _, command in ipairs({
    { id = "submit_input", label = "submit" },
    { id = "feedback", label = "options" },
    { id = "clear_input", label = "clear" },
  }) do
    local key = keymaps.view_keys_for("picker", command.id)[1]
    if key then segment_list[#segment_list + 1] = key .. " " .. command.label end
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

---@param instance table
---@return table
local function selection_context(instance)
  return {
    page = picker_state.page(instance.state, instance.spec),
    option = picker_state.selected_option(instance.state, instance.spec),
    query = picker_state.query(instance.state, instance.spec),
  }
end

---@param instance table
---@param force? boolean
local function notify_selection(instance, force)
  if not instance.spec.on_change then return end
  local context = selection_context(instance)
  local identity = context.option and tostring(context.option.id or context.option.value or context.option.label) or ""
  if not force and instance.notified_identity == identity then return end
  instance.notified_identity = identity
  instance.spec.on_change(context, Picker)
end

local function render_view(instance)
  if not (valid_window(instance.win) and vim.api.nvim_buf_is_valid(instance.buf)) then return end
  local page = picker_state.page(instance.state, instance.spec)
  local bounds = layout.host_bounds(instance.spec.host.window_list)
  local width = math.max(30, bounds.width - 2)
  local selected = picker_state.selected_index(instance.state, instance.spec)
  local input_visible = instance.input_visible == true
  local frame = layout.build(page, selected, width, {
    input_visible = input_visible,
    footer = input_visible and input_footer() or nil,
  })
  frame.chosen_index_set = {}
  local selected_set = instance.state.selected_set_by_page[page.id] or {}
  for index, option in ipairs(page.option_list) do
    local option_id = tostring(option.id or option.value or index)
    if selected_set[option_id] then frame.chosen_index_set[index] = true end
  end
  frame.input_height = page.input_height or config.options.picker.input_height
  local height = math.min(#frame.lines, config.options.picker.max_height, math.max(4, bounds.height - 2))
  instance.frame = frame
  picker_window.resize(instance.win, instance.spec.host, width, height, title_chunks(instance))
  render.apply(instance.buf, frame)
  if valid_window(instance.search_win) then search_input.resize(instance.search_win, instance.win, frame) end
  if valid_window(instance.input_win) then input.resize(instance.input_win, instance.win, frame) end
  popup_window.set_cursor_hidden(instance.win, true)
  popup_window.set_focusable(instance.win, true)
  local selected_range = frame.option_range[selected]
  if selected_range then
    vim.api.nvim_win_set_cursor(instance.win, { selected_range.first, 0 })
    popup_window.clamp_view(instance.win, instance.buf)
  else
    vim.api.nvim_win_set_cursor(instance.win, { 1, 0 })
    vim.api.nvim_win_call(instance.win, function()
      vim.fn.winrestview({ topline = 1, lnum = 1, col = 0, curswant = 0 })
    end)
  end
  notify_selection(instance)
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
  finish(instance, { page = page, option = option, text = text, input_kind = instance.input_kind })
end

local function set_picker_cursor_hidden(instance, hidden)
  local visible_guicursor = instance.guicursor ~= "" and instance.guicursor or "a:block-Cursor"
  vim.o.guicursor = hidden and "n:block-DiffReviewHiddenCursor" or visible_guicursor
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
    instance.state.draft_by_page[page.id] or "",
    instance.origin
  )
  for _, key in ipairs(keymaps.view_keys_for("picker", "submit_input")) do
    vim.keymap.set({ "n", "i" }, key, function() submit_input(instance) end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Submit picker input",
    })
  end
  for _, key in ipairs(keymaps.view_keys_for("picker", "focus_input")) do
    vim.keymap.set("n", key, function() focus_picker(instance) end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Return to picker options",
    })
  end
  for _, key in ipairs(keymaps.view_keys_for("picker", "feedback")) do
    vim.keymap.set({ "n", "i" }, key, function()
      after_input_mapping(instance, function() focus_picker(instance) end)
    end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Return to picker options",
    })
  end
  for _, key in ipairs(keymaps.view_keys_for("picker", "clear_input")) do
    vim.keymap.set({ "n", "i" }, key, function()
      after_input_mapping(instance, function() clear_input(instance) end)
    end, {
      buffer = instance.input_buf,
      silent = true,
      nowait = true,
      desc = "Clear and close picker input",
    })
  end
  focus_input(instance, true)
end

local function confirm(instance, with_feedback)
  local page = picker_state.page(instance.state, instance.spec)
  local option = picker_state.selected_option(instance.state, instance.spec)
  if not option then return end
  if page.selection_mode == "multiple" and option.action ~= "submit" then
    picker_state.toggle_selected(instance.state, instance.spec)
    render_view(instance)
  elseif option.input_kind then
    open_input(instance, option.input_kind)
  elseif with_feedback then
    open_input(instance, "feedback")
  else
    local feedback = valid_window(instance.input_win) and input.text(instance.input_buf) or ""
    finish(instance, {
      page = page,
      option = option,
      selected_option_list = picker_state.selected_option_list(instance.state, instance.spec),
      text = feedback ~= "" and feedback or nil,
      input_kind = feedback ~= "" and instance.input_kind or nil,
    })
  end
end

local function move_page(instance, delta)
  close_input(instance)
  picker_state.move_page(instance.state, instance.spec, delta)
  render_view(instance)
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

---@param instance table
---@param buf integer
---@param preserve boolean
local function install_action_keymaps(instance, buf, preserve)
  local action_list = vim.list_extend(vim.deepcopy(instance.spec.action_list or {}),
    vim.deepcopy(picker_state.page(instance.state, instance.spec).action_list or {}))
  for _, action in ipairs(action_list) do
    if preserve then preserve_mapping(instance, buf, action.key) end
    vim.keymap.set(action.modes or "n", action.key, function() run_action(instance, action) end, {
      buffer = buf,
      silent = true,
      nowait = true,
      desc = action.desc or action.id or "Picker action",
    })
  end
end

local function install_keymaps(instance)
  local set = command_set.new()
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
  command_set.register(set, "focus_input", function() focus_input(instance) end)
  command_set.register(set, "close", function() Picker.close(true) end)
  for _, command_id in ipairs(set.order) do
    for _, key in ipairs(keymaps.view_keys_for("picker", command_id)) do
      preserve_mapping(instance, instance.buf, key)
    end
  end
  keymaps.setup_view_keymaps(instance.buf, "picker", set)
  install_option_keymaps(instance, instance.buf, true)
  install_action_keymaps(instance, instance.buf, true)
end

---@param instance table
local function install_search_keymaps(instance)
  local buf = instance.search_buf
  for _, key in ipairs(keymaps.view_keys_for("picker", "previous")) do
    vim.keymap.set("n", key, function() move_selection(instance, -1) end,
      { buffer = buf, silent = true, nowait = true, desc = "Select previous picker option" })
    if key == "<Up>" then
      vim.keymap.set("i", key, function() move_selection(instance, -1) end,
        { buffer = buf, silent = true, nowait = true, desc = "Select previous picker option" })
    end
  end
  for _, key in ipairs(keymaps.view_keys_for("picker", "next")) do
    vim.keymap.set("n", key, function() move_selection(instance, 1) end,
      { buffer = buf, silent = true, nowait = true, desc = "Select next picker option" })
    if key == "<Down>" then
      vim.keymap.set("i", key, function() move_selection(instance, 1) end,
        { buffer = buf, silent = true, nowait = true, desc = "Select next picker option" })
    end
  end
  for _, key in ipairs(keymaps.view_keys_for("picker", "select")) do
    vim.keymap.set({ "n", "i" }, key, function() confirm(instance, false) end,
      { buffer = buf, silent = true, nowait = true, desc = "Confirm picker option" })
  end
  for _, key in ipairs(keymaps.view_keys_for("picker", "close")) do
    vim.keymap.set("n", key, function() Picker.close(true) end,
      { buffer = buf, silent = true, nowait = true, desc = "Close picker" })
  end
  install_option_keymaps(instance, buf, false)
  install_action_keymaps(instance, buf, false)
end

---@param instance table
local function open_search(instance)
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
  install_search_keymaps(instance)
  set_picker_cursor_hidden(instance, false)
  vim.api.nvim_set_current_win(instance.search_win)
  enter_insert_mode(instance, instance.search_win)
end

---@param spec table
function Picker.open(spec)
  Picker.close(true)
  spec.host = spec.host or {}
  spec.host.window_list = spec.host.window_list or { spec.host.control_win or vim.api.nvim_get_current_win() }
  local origin = popup_window.capture_origin()
  local control_win = spec.host.control_win or origin.win
  assert(vim.api.nvim_win_is_valid(control_win), "picker control window must be valid")
  local state = picker_state.new(spec)
  local bounds = layout.host_bounds(spec.host.window_list)
  local width = math.max(30, bounds.width - 2)
  local initial_frame = layout.build(picker_state.page(state, spec), picker_state.selected_index(state, spec), width)
  local height = math.min(#initial_frame.lines, config.options.picker.max_height, math.max(4, bounds.height - 2))
  local instance = {
    spec = spec,
    state = state,
    origin = origin,
    guicursor = vim.o.guicursor,
    mapping_list = {},
    input_visible = false,
  }
  instance.buf, instance.win = picker_window.open(spec.host, width, height, title_chunks(instance), origin)
  active = instance
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
    open_search(instance)
  elseif spec.initial_input_kind then
    open_input(instance, spec.initial_input_kind)
  end
  return instance.buf, instance.win
end

---@param spec table
function Picker.update(spec)
  if not active then return end
  local previous_page = picker_state.page(active.state, active.spec)
  local previous_option = picker_state.selected_option(active.state, active.spec)
  local previous_option_id = previous_option and tostring(previous_option.id or previous_option.value or "") or nil
  close_input(active)
  clear_modal_keymaps(active)
  local previous_state = active.state
  active.spec = spec
  local next_state = picker_state.new(spec)
  next_state.selected_index_by_page = previous_state.selected_index_by_page
  next_state.selected_set_by_page = previous_state.selected_set_by_page
  for page_id, draft in pairs(previous_state.draft_by_page) do next_state.draft_by_page[page_id] = draft end
  for page_id, query in pairs(previous_state.query_by_page) do next_state.query_by_page[page_id] = query end
  if previous_option_id and previous_page then
    local next_page = picker_state.page(next_state, spec)
    if next_page.id == previous_page.id then
      for option_index, option in ipairs(next_page.option_list) do
        if tostring(option.id or option.value or "") == previous_option_id then
          next_state.selected_index_by_page[next_page.id] = option_index
          break
        end
      end
    end
  end
  active.state = next_state
  active.notified_identity = nil
  render_view(active)
  install_keymaps(active)
  if picker_state.page(active.state, active.spec).search then
    if valid_window(active.search_win) then
      install_search_keymaps(active)
      vim.api.nvim_set_current_win(active.search_win)
    else
      open_search(active)
    end
  else
    close_search(active)
    focus_picker(active)
    if spec.initial_input_kind then open_input(active, spec.initial_input_kind) end
  end
end

---@param notify? boolean
function Picker.close(notify)
  local instance = active
  if not instance then return end
  set_picker_cursor_hidden(instance, false)
  active = nil
  vim.api.nvim_clear_autocmds({ group = resize_group })
  clear_modal_keymaps(instance)
  close_input(instance)
  close_search(instance)
  popup_window.close(instance.win)
  if notify ~= false and instance.spec.on_close then instance.spec.on_close() end
end

---@param owner? string
function Picker.is_open(owner)
  return active ~= nil and valid_window(active.win) and (not owner or active.spec.owner == owner)
end

function Picker._state_for_test()
  return active
end

return Picker
