vim.loader.enable(false)
local original_columns, original_lines = vim.o.columns, vim.o.lines
vim.o.columns, vim.o.lines = 110, 38
require("forge").setup({ harness = { provider = "mock" } })

local picker = require("forge.views.picker")
local picker_state = require("forge.views.picker.state")
local layout = require("forge.views.picker.layout")
local navigation = require("forge.views.picker.page_navigation")
local popup_window = require("forge.infra.popup_window")
local config = require("forge.infra.config")
local original_keymaps = vim.deepcopy(config.options.keymaps.picker)
local owned_window = {}

--- Fail with the values that violate an interaction invariant.
local function assert_equal(actual, expected, message)
  assert(vim.deep_equal(actual, expected), (message or "values differ")
    .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual))
end

--- Invoke the effective Normal-mode mapping in the current picker buffer.
local function press(key)
  local instance = assert(picker._state_for_test(), "picker is closed")
  local mapping = vim.api.nvim_buf_call(instance.buf, function() return vim.fn.maparg(key, "n", false, true) end)
  assert(mapping.callback, "missing mapping: " .. key)
  mapping.callback()
end

--- Select a field by its identity without depending on page or row positions.
local function select_field(id)
  local instance = picker._state_for_test()
  for page_index, page in ipairs(instance.spec.page_list) do
    for index, option in ipairs(page.option_list) do
      for _, field in ipairs(option.fields or {}) do
        if field.id == id then
          instance.state.page_index = page_index
          instance.state.selected_index_by_page[page.id] = index
          instance.state.field_by_page[page.id] = { [option.id] = id }
          picker.update(instance.spec)
          return
        end
      end
    end
  end
  error("missing field: " .. id)
end

--- Build two real field pages with a callback supplied by the scenario.
local function field_spec(callback)
  return {
    owner = "navigation-fixture", title = "Configuration", page_navigation = "tabs", fit_content = true,
    page_list = {
      { id = "models", title = "模型", field_navigation = true, option_list = {
        { id = "main", label = "Main", fields = { { id = "main-model", kind = "choice", value = "first" } } },
      } },
      { id = "tools", title = "Tools", field_navigation = true, option_list = {
        { id = "tool", label = "Tool", key = "]", confirm_on_key = true, fields = {
          { id = "tool-value", kind = "choice", value = "first" },
          { id = "tool-enabled", kind = "toggle", value = false },
        } },
      } },
    },
    on_field_action = callback,
  }
end

--- Verify tabs shift row anchors without consuming columns or changing source spans.
local function tabs_preserve_page_highlights()
  local spec = field_spec(function() end)
  local page = spec.page_list[1]
  page.search, page.allow_input = { start_in_normal = true }, true
  page.option_list[1].section = "Models"
  page.option_list[1].child_line_list = { "Child detail" }
  page.option_list[1].fields[1].value = string.rep("界", 95) .. "tail"
  local geometry = navigation.measure(spec.page_list, spec.page_navigation, 100, 16)
  local logical = layout.build(page, 1, geometry.content_width, { input_visible = true, search_visible = true })
  local visible = layout.viewport(logical, 16 - geometry.header_height)
  local original = vim.deepcopy(visible)
  local composed = navigation.compose(visible, geometry, spec.page_list, 1)
  assert_equal(visible, original, "composition mutated its source viewport")
  assert_equal(geometry.mode, "tabs")
  assert_equal(geometry.content_width, 100)
  assert_equal(composed.lines[1], "  [模型]  Tools")
  for line, content in ipairs(original.lines) do
    assert_equal(composed.lines[line + geometry.header_height], content)
  end
  for _, name in ipairs({ "option_range", "primary_range", "child_range", "content_range" }) do
    for index, range in pairs(original[name]) do
      assert_equal(composed[name][index].first, range.first + geometry.header_height)
      assert_equal(composed[name][index].last, range.last + geometry.header_height)
      assert_equal(composed[name][index].spans, range.spans)
    end
  end
  assert_equal(composed.input_start, original.input_start + geometry.header_height)
  assert_equal(composed.search_start, original.search_start + geometry.header_height)
  picker.open(spec)
  local instance = picker._state_for_test()
  require("forge.views.picker.render").apply(instance.buf, composed)
  local active_tab = false
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(instance.buf, -1, 0, -1, { details = true })) do
    if mark[2] == 0 and mark[4].hl_group == "ForgePickerSelected" then
      assert_equal(composed.lines[1]:sub(mark[3] + 1, mark[4].end_col), "[模型]")
      active_tab = true
    end
  end
  assert(active_tab, "active Unicode tab is not highlighted")
  local parent = vim.api.nvim_win_get_config(instance.win)
  for _, module in ipairs({ "forge.views.picker.input", "forge.views.picker.search_input" }) do
    local _, window = require(module).open(instance.win, composed, "draft", instance.origin, function() end)
    owned_window[#owned_window + 1] = window
    local position = vim.api.nvim_win_get_config(window)
    assert_equal(position.col, parent.col + 2)
    assert_equal(position.width, geometry.content_width - 4)
    popup_window.close(window, false)
  end
  picker.close(false)
end

--- Every page remains reachable when its neighbours no longer fit in the tab strip.
local function overflowing_tabs_keep_active_page_visible()
  local pages = {}
  for _, title in ipairs({ "Models", "Workflow", "Access", "Tools", "System", string.rep("界", 40) }) do
    pages[#pages + 1] = { title = title }
  end
  local frame = layout.build({ option_list = {} }, 1, 100)
  for _, width in ipairs({ 20, 32, 50, 100 }) do
    for selected, page in ipairs(pages) do
      local geometry = navigation.measure(pages, "tabs", width, 20)
      local composed = navigation.compose(frame, geometry, pages, selected)
      local strip = composed.lines[1]
      assert(vim.fn.strdisplaywidth(strip) <= width, "tab strip overflowed")
      local span = composed.content_span_list[#composed.content_span_list]
      local active = strip:sub(span.first + 1, span.last)
      assert(active:match("^%[.*%]$"), "active tab lost its brackets")
      if vim.fn.strdisplaywidth(page.title) < width - 10 then
        assert_equal(active, "[" .. page.title .. "]")
      end
      assert_equal(composed.content_width, width)
      assert_equal(composed.content_column, 0)
    end
  end
  assert_equal(navigation.measure({ pages[1] }, "tabs", 40, 20).mode, "none")
end

--- Suppress field help without changing disabled controls or adding blank rows.
local function field_help_can_be_hidden()
  local page = { field_navigation = true, field_descriptions = false, option_list = {
    { id = "first", label = "Model", fields = {
      { id = "model", kind = "choice", value = "model", description = "Model description.",
        disabled_reason = "Unavailable for this provider." },
    } },
    { id = "second", label = "Thinking", fields = {
      { id = "thinking", kind = "choice", value = "High", description = "Thinking description." },
    } },
  } }
  for selected = 1, #page.option_list do
    local frame = layout.build(page, selected, 60)
    local text = table.concat(frame.lines, " ")
    assert(not text:find("description", 1, true))
    assert(not text:find("Unavailable", 1, true))
    assert_equal(frame.option_range[2].first, frame.option_range[1].last + 1)
  end
  page.field_descriptions = nil
  local frame = layout.build(page, 1, 60)
  assert(table.concat(frame.lines, " "):find("Unavailable for this provider.", 1, true),
    "ordinary field pickers lost disabled help")
  page.option_list[1].section = "First section"
  page.option_list[2].section = "Second section"
  frame = layout.build(page, 1, 60)
  local second_heading = frame.section_line[2]
  local visible = layout.viewport(frame, second_heading + #frame.lines - frame.body_end)
  assert(not table.concat(visible.lines, " "):find("Second section", 1, true), "viewport orphaned a section heading")
end

--- Keep two help rows pinned below the list for short, long, absent, and disabled help.
local function fixed_help_does_not_resize_the_list()
  local page = { field_navigation = true, field_description_height = 2, footer = "q close", option_list = {
    { id = "short", label = "Short", fields = {
      { id = "short", kind = "choice", value = "One", description = "Short help." },
    } },
    { id = "long", label = "Long", fields = {
      { id = "long", kind = "choice", value = "Two", description = string.rep("界", 100) },
    } },
    { id = "empty", label = "Empty", fields = {
      { id = "empty", kind = "choice", value = "Three" },
    } },
    { id = "disabled", label = "Disabled", fields = {
      { id = "disabled", kind = "choice", value = "Four", description = "Description.", disabled_reason = "Unavailable." },
    } },
  } }
  for _, width in ipairs({ 32, 60, 100 }) do
    local first_frame = layout.build(page, 1, width)
    for selected = 1, #page.option_list do
      local frame = layout.build(page, selected, width)
      local help = frame.content_range[1]
      assert_equal(help.last - help.first + 1, 2)
      assert_equal(frame.footer_line - help.last, 1)
      assert_equal(#frame.lines, #first_frame.lines, "help resized the picker")
      assert_equal(frame.option_range, first_frame.option_range, "help displaced setting rows")
      for _, line in ipairs(frame.lines) do assert(vim.fn.strdisplaywidth(line) <= width) end
      if selected == 2 then assert(frame.lines[help.last]:find("…", 1, true), "overflow is not indicated") end
      if selected == 3 then
        assert(vim.trim(frame.lines[help.first]) == "" and frame.lines[help.last] == "")
      end
      if selected == 4 then assert(frame.lines[help.first]:find("Unavailable.", 1, true)) end
      local visible = layout.viewport(frame, 6)
      local visible_help = visible.content_range[1]
      assert_equal(visible_help.last - visible_help.first + 1, 2)
      assert_equal(visible.footer_line - visible_help.last, 1)
      assert_equal(#visible.lines, 6)
    end
  end
end

--- Verify resizing keeps a long selected value reachable in both layouts.
local function tab_navigation_survives_resize()
  local spec = field_spec(function() end)
  spec.page_list[1].option_list[1].fields[1].value = string.rep("界", 600) .. "tail"
  -- Headless Neovim without an attached UI ignores 'columns' and 'lines' for window layout, and the
  -- picker sizes itself from its host windows. Resize the host window itself; a neighboring split
  -- absorbs the columns it releases.
  local host = vim.api.nvim_get_current_win()
  local host_width, host_height = vim.api.nvim_win_get_width(host) - 2, vim.api.nvim_win_get_height(host)
  vim.cmd("rightbelow vnew")
  local neighbor = vim.api.nvim_get_current_win()
  vim.api.nvim_set_current_win(host)
  vim.api.nvim_win_set_width(host, host_width)
  picker.open(spec)
  vim.api.nvim_win_set_width(host, 52)
  vim.api.nvim_win_set_height(host, 14)
  picker.update(spec)
  local instance = picker._state_for_test()
  assert_equal(instance.frame.content_column, 0)
  assert(instance.frame.lines[1]:find("[模型]", 1, true), "active category tab is missing")
  local footer = table.concat(vim.list_slice(instance.frame.lines, assert(instance.frame.footer_line)), " ")
  assert_equal(footer:gsub("%s+", " "):gsub("^ ", ""),
    picker._navigation_footer(instance):gsub("%s+", " "), "narrow layout clipped its navigation hints")
  assert(#instance.frame.lines <= vim.api.nvim_win_get_height(instance.win), "footer displaced visible content outside the popup")
  assert(instance.frame.option_scroll_max > 0, "long field became unreachable")
  press("<PageDown>")
  assert(picker._state_for_test().frame.option_offset > 0, "long field did not scroll")
  vim.api.nvim_win_set_width(host, host_width)
  vim.api.nvim_win_set_height(host, host_height)
  picker.update(spec)
  instance = picker._state_for_test()
  assert_equal(instance.frame.content_column, 0)
  assert_equal(picker_state.selected_field(instance.state, instance.spec).id, "main-model")
  for _, line in ipairs(instance.frame.lines) do
    assert(vim.fn.strdisplaywidth(line) <= vim.api.nvim_win_get_width(instance.win), "resized row overflow")
  end
  picker.close(false)
  vim.api.nvim_win_close(neighbor, true)
end

--- Keep form columns stable across focus changes without splitting model names or row labels.
local function section_fields_keep_their_columns()
  local page = { field_navigation = true, option_list = {
    { id = "main", section = "Models", label = "Main", fields = {
      { id = "main-model", kind = "choice", value = "gpt-6.1-sol" },
      { id = "main-thinking", kind = "choice", label = "Thinking", value = "high" },
      { id = "main-speed", kind = "choice", label = "Speed", value = "ultrafast" },
    } },
    { id = "ask", section = "Models", label = "Ask", fields = {
      { id = "ask-model", kind = "choice", value = "Same as planning" },
      { id = "ask-thinking", kind = "choice", label = "Thinking", value = "Default" },
      { id = "ask-speed", kind = "choice", label = "Speed", value = "default" },
      { id = "ask-inherit", kind = "action", value = "Use planning" },
    } },
    { id = "background", section = "Models", label = "Background", fields = {
      { id = "background-model", kind = "choice", value = "gpt-6.1-terra" },
      { id = "background-thinking", kind = "choice", label = "Thinking", value = "low" },
      { id = "background-speed", kind = "choice", label = "Speed", value = "default" },
    } },
    { id = "compact", section = "Compaction", label = "Main", fields = {
      { id = "enabled", kind = "toggle", label = "Automatic", value = true },
      { id = "threshold", kind = "input", label = "Threshold", value = "128000" },
    } },
  } }
  for _, width in ipairs({ 48, 78, 108 }) do
    local first = layout.build(page, 1, width)
    for selected = 1, 3 do
      local frame = layout.build(page, selected, width)
      local text = table.concat(frame.lines, "\n")
      for _, content in ipairs({ "Background", "gpt-6.1-sol", "gpt-6.1-terra", "Same as planning",
        "Thinking: Default", "Speed: ultrafast", "Threshold: 128000" }) do
        assert(text:find(content, 1, true), "responsive fields split " .. content .. " at width " .. width)
      end
      local columns = {}
      for index = 1, 3 do
        local line = frame.lines[frame.option_range[index].first]
        local value = ({ "gpt-6.1-sol", "Same as planning", "gpt-6.1-terra" })[index]
        local value_start = assert(line:find(value, 1, true))
        columns[index] = vim.fn.strdisplaywidth(line:sub(1, value_start - 1))
      end
      assert_equal(columns, { columns[1], columns[1], columns[1] }, "model values do not share a column")
      for index, line in ipairs(frame.lines) do
        assert(vim.fn.strdisplaywidth(line) <= width, "responsive form overflowed")
        assert_equal(line:gsub("←", " "):gsub("→", " "):gsub("%s+$", ""),
          first.lines[index]:gsub("←", " "):gsub("→", " "):gsub("%s+$", ""), "focus moved form columns")
      end
    end
  end
end

--- Verify page commands cannot invoke a field callback or a colliding option shortcut.
local function category_navigation_does_not_edit_values()
  local changes = {}
  local spec = field_spec(function(context, action)
    changes[#changes + 1] = { context.field.id, action }
  end)
  picker.open(spec)
  press("]")
  assert_equal(picker._state_for_test().state.page_index, 2)
  assert_equal(changes, {})
  press("]")
  assert_equal(changes, {}, "category boundary activated the colliding option")
  press("<Right>")
  assert_equal(changes, { { "tool-value", "change" } })
  assert_equal(picker._state_for_test().state.page_index, 2)
  config.options.keymaps.picker.page_previous = "{"
  config.options.keymaps.picker.page_next = "}"
  picker.update(spec)
  press("{")
  assert_equal(picker._state_for_test().state.page_index, 1)
  local footer = vim.inspect(vim.api.nvim_win_get_config(picker._state_for_test().win).footer)
  assert(footer:find("{/} category", 1, true), "category hint ignored remapped commands")
  config.options.keymaps.picker = vim.deepcopy(original_keymaps)
  picker.close(false)
end

--- Verify detached state restores every page by identity and clamps removed rows.
local function resume_state_preserves_surviving_ids()
  local spec = field_spec(function() end)
  spec.page_list[1].option_list[2] = { id = "other", label = "Other", fields = {
    { id = "other-choice", kind = "choice", value = "second" },
  } }
  spec.page_list[2].search = { start_in_normal = true }
  picker.open(spec)
  select_field("other-choice")
  select_field("tool-enabled")
  local instance = picker._state_for_test()
  instance.state.query_by_page.tools = "Tool"
  instance.state.draft_by_page.models = "unsent"
  local snapshot = picker.capture_state()
  local updated = vim.deepcopy(spec)
  updated.page_list[1].option_list = { updated.page_list[1].option_list[2], updated.page_list[1].option_list[1] }
  picker.update(updated)
  instance = picker._state_for_test()
  assert_equal(instance.state.selected_index_by_page.models, 1, "inactive page lost its row identity")
  instance.state.field_by_page.tools.tool = "tool-value"
  assert_equal(snapshot.field_by_page.tools.tool, "tool-enabled", "snapshot retained mutable state")
  picker.close(false)
  updated.resume_state = snapshot
  picker.open(updated)
  instance = picker._state_for_test()
  assert_equal(instance.state.page_index, 2)
  assert_equal(picker_state.selected_field(instance.state, instance.spec).id, "tool-enabled")
  assert_equal(instance.state.query_by_page.tools, "Tool")
  assert_equal(instance.state.draft_by_page.models, "unsent")
  picker.close(false)
  updated.page_list[1].option_list = { updated.page_list[1].option_list[2] }
  updated.initial_page = 1
  local restored = picker_state.new(updated)
  assert_equal(restored.selected_index_by_page.models, 1, "removed selection did not clamp")
  assert_equal(restored.page_index, 1, "explicit initial page did not win")
  updated.owner = "different-owner"
  local isolated = picker_state.new(updated)
  assert_equal(isolated.draft_by_page.models, nil, "resume state crossed picker owners")
  local initial = field_spec(function() end)
  local initial_state = picker_state.new(initial)
  local initial_snapshot = picker_state.capture(initial_state, initial)
  table.insert(initial.page_list[1].option_list[1].fields, 1, { id = "inserted", kind = "choice", value = "new" })
  initial.resume_state = initial_snapshot
  local shifted = picker_state.new(initial)
  assert_equal(picker_state.selected_field(shifted, initial).id, "main-model", "default field lost its identity")
end

--- Verify real configuration child editors restore their origin and reject stale responses.
local function child_picker_return_preserves_configuration_location()
  local client = require("forge.client")
dofile("nvim/tests/forge/support/provider_catalog.lua").install()
  local controller = require("forge.views.harness.controller")
  local settings = require("forge.views.harness.settings")
  local background = require("forge.integrations.background_settings")
  local original_request, original_configure, original_background = client.request, controller.configure, background.request
  local state = { session = {
    id = "navigation-session", provider = "fixture_alpha", model = "first", effort = "low", workspace = vim.fn.getcwd(),
  }, capability = { model_selection = true, effort_selection = true, native_compact = true, os_sandbox = true } }
  local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
  local pending_response, pending_background
  local preferences = { auto_compaction = { enabled = true, threshold_tokens = 128000 },
    models = { fixture_alpha = { model = "first", thinking = "low" } } }
  client.request = function(request, callback)
    if request.method == "host.trace.status" then callback({ enabled = false, path = vim.fn.tempname() })
    elseif request.method == "session.provider.model.list" then callback({
      { id = "first", reasoning = { "low", "high" }, default_reasoning = "low" },
      { id = "second", reasoning = { "low", "high" }, default_reasoning = "low" },
    })
    elseif request.method == "session.configure" then
      state.ask_model = vim.deepcopy(request.params.ask_model)
      callback({ ask_model = state.ask_model })
    else error("unexpected request: " .. request.method) end
  end
  controller.configure = function(params, completed)
    local session_id = state.session.id
    pending_response = function()
      if state.session.id ~= session_id then completed(false) return end
      state.session = vim.tbl_extend("force", state.session, params)
      completed(true)
    end
  end
  background.request = function(_, changed, callback)
    if changed then
      pending_background = function()
        preferences = changed
        callback({ revision = 1, settings = preferences })
      end
    else callback({ revision = 0, settings = preferences }) end
  end
  local success, failure = xpcall(function()
    settings.open(state, host)
    select_field("main-model")
    press("<CR>")
    assert(not picker.is_open("harness-config"), "main selector did not open")
    press("<CR>")
    assert(pending_response and not picker.is_open(), "model completion was not deferred")
    pending_response()
    local instance = picker._state_for_test()
    assert_equal(instance.spec.page_list[instance.state.page_index].id, "models")
    assert_equal(picker_state.selected_field(instance.state, instance.spec).id, "main-model")
    select_field("ask-override")
    press("<CR>")
    select_field("ask-model")
    press("<CR>")
    picker.close(true)
    instance = picker._state_for_test()
    assert_equal(picker_state.selected_field(instance.state, instance.spec).id, "ask-model")
    select_field("background-model")
    press("<CR>")
    press("<CR>")
    assert(pending_background and not picker.is_open())
    pending_background()
    instance = picker._state_for_test()
    assert_equal(picker_state.selected_field(instance.state, instance.spec).id, "background-model")
    select_field("readable-directories")
    press("<CR>")
    assert_equal(vim.bo.buftype, "acwrite", "directories did not open an editable buffer")
    local close_editor = vim.fn.maparg("q", "n", false, true)
    assert(close_editor.callback, "directory editor has no close binding")
    close_editor.callback()
    instance = picker._state_for_test()
    assert_equal(instance.spec.page_list[instance.state.page_index].id, "permissions")
    assert_equal(picker_state.selected_field(instance.state, instance.spec).id, "readable-directories")
    select_field("main-thinking")
    press("<Right>")
    press("]")
    pending_response()
    instance = picker._state_for_test()
    assert_equal(instance.spec.page_list[instance.state.page_index].id, "workflow", "late redraw restored old navigation")
    select_field("main-model")
    press("<CR>")
    press("<CR>")
    state.session = { id = "new-session", provider = "fixture_alpha", model = "first" }
    picker.open({ owner = "new-owner", page_list = { { id = "new", option_list = {} } } })
    pending_response()
    assert(picker.is_open("new-owner"), "old session callback replaced the new picker")
  end, debug.traceback)
  picker.close(false)
  client.request, controller.configure, background.request = original_request, original_configure, original_background
  if not success then error(failure) end
end

--- Keep help above the picker without changing navigation or ignoring local bindings.
local function help_preserves_navigation_and_uses_overrides()
  local spec = field_spec(function() end)
  spec.hint_mode = "help"
  spec.keymaps = { help = "g?", page_previous = "{", page_next = "}" }
  spec.page_list[1].option_list[1].fields[1].value = string.rep("value ", 100)
  picker.open(spec)
  picker.update(spec)
  local instance = picker._state_for_test()
  local footer = vim.api.nvim_win_get_config(instance.win).footer
  assert_equal(vim.trim(footer[1][1]), "g? help", "long values exposed inline commands")
  local before = picker.capture_state()
  press("g?")
  local help_window = vim.api.nvim_get_current_win()
  assert_equal(vim.bo.filetype, "ForgeHelp")
  local text = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  assert(text:find("}", 1, true) and text:find("{", 1, true), "help ignored category overrides")
  assert(text:find("g?", 1, true), "help ignored its own override")
  picker.update(spec)
  assert_equal(vim.api.nvim_get_current_win(), help_window, "redraw stole focus from help")
  assert_equal(picker.capture_state(), before, "help changed navigation state")
  picker.close(false)
  assert(not vim.api.nvim_win_is_valid(help_window), "closing the picker leaked its help window")
end

--- Verify ordinary picker callers retain their full-width layout and input lifecycle.
local function ordinary_picker_keeps_existing_behavior()
  local spec = { owner = "ordinary-fixture", page_list = {
    { id = "first", title = "First", allow_input = true, search = { start_in_normal = true },
      option_list = { { id = "one", label = "One" } } },
    { id = "second", title = "Second", option_list = { { id = "two", label = "Two" } } },
  } }
  local origin = vim.api.nvim_get_current_win()
  local cursor = vim.o.guicursor
  picker.open(spec)
  local instance = picker._state_for_test()
  assert_equal(instance.frame.content_column, nil)
  assert_equal(instance.frame.body_start_by_line, nil)
  press("]")
  assert_equal(picker._state_for_test().state.page_index, 2)
  press("[")
  press("<Tab>")
  instance = picker._state_for_test()
  assert(instance.input_win and vim.api.nvim_win_is_valid(instance.input_win))
  local position = vim.api.nvim_win_get_config(instance.input_win)
  assert_equal(position.width, vim.api.nvim_win_get_width(instance.win) - 4)
  picker.close(false)
  assert_equal(vim.api.nvim_get_current_win(), origin)
  assert_equal(vim.o.guicursor, cursor)
end

--- Run scenarios in one isolated editor and release owned UI resources on failure.
function run_picker_navigation_tests()
  tabs_preserve_page_highlights()
  overflowing_tabs_keep_active_page_visible()
  field_help_can_be_hidden()
  fixed_help_does_not_resize_the_list()
  tab_navigation_survives_resize()
  section_fields_keep_their_columns()
  category_navigation_does_not_edit_values()
  resume_state_preserves_surviving_ids()
  child_picker_return_preserves_configuration_location()
  help_preserves_navigation_and_uses_overrides()
  ordinary_picker_keeps_existing_behavior()
end

local success, failure = xpcall(run_picker_navigation_tests, debug.traceback)
picker.close(false)
for _, window in ipairs(owned_window) do
  if vim.api.nvim_win_is_valid(window) then popup_window.close(window, false) end
end
config.options.keymaps.picker = original_keymaps
vim.o.columns, vim.o.lines = original_columns, original_lines
if not success then error(failure) end
print("picker_navigation: passed")
vim.cmd("qa!")
