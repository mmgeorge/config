local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["picker"] = function()
  local forge = require("forge")

  do
    forge.setup({ walkthrough_inventory = "sem", harness = { provider = "mock" } })
    local layout = require("forge.views.picker.layout")
    local picker_input = require("forge.views.picker.input")
    local picker_state = require("forge.views.picker.state")
    local picker = require("forge.views.picker")
    local styled_rows = {}
    for index = 1, 20 do
      styled_rows[index] = { label = "method " .. index, columns = { "foo.rs", "Client::send" },
        column_segments = { { { "foo.rs", "ForgeFileName" } },
          { { "Client", "@type" }, { "::" }, { "send", "@function" } } } }
    end
    local styled_page = { column_headers = { "Location", "Caller" }, option_list = styled_rows,
      highlight_selected_line = true, highlight_selected_text = false }
    local styled_frame = layout.build(styled_page, 20, 100)
    eq(styled_frame.lines[1]:find("Location", 1, true), 3, { fail_reason = "unkeyed columns must align with the title inset" })
    eq(styled_frame.lines[2]:find("foo.rs", 1, true), 3, { fail_reason = "unkeyed values must align with their headings" })
    eq(styled_frame.footer_line, styled_frame.body_end + 1, { fail_reason = "footer must not add an empty result row" })
    styled_frame = layout.viewport(styled_frame, 8)
    local styled_buf = buffer.scratch()
    require("forge.views.picker.render").apply(styled_buf, styled_frame)
    local styled_mark = {}
    local selected_line_count = 0
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(styled_buf, -1, 0, -1, { details = true })) do
      local capture = mark[4].hl_group
      if mark[4].line_hl_group then
        selected_line_count = selected_line_count + 1
        eq(mark[4].line_hl_group, "ForgePickerSelectedLine")
        eq(mark[2], styled_frame.primary_range[20].first - 1,
          { fail_reason = "selection background must follow the projected selected row" })
        eq(mark[3], 0, { fail_reason = "selection must include the leading gutter" })
        eq(mark[4].end_row, nil, { fail_reason = "line background must not extend into the next row" })
      end
      assert(capture ~= "ForgePickerKey", "unkeyed rows must not highlight a synthetic hotkey")
      assert(capture ~= "ForgePickerSelected", "background selection must not recolor the selected text")
      if capture and mark[2] == styled_frame.primary_range[20].first - 1 then
        styled_mark[capture] = styled_frame.lines[mark[2] + 1]:sub(mark[3] + 1, mark[4].end_col)
      end
    end
    eq(styled_mark.ForgeFileName, "foo.rs", { fail_reason = "filename style must survive scrolling on selected rows" })
    eq(selected_line_count, 1, { fail_reason = "only the selected row may receive a full-line background" })
    assert(vim.api.nvim_get_hl(0, { name = "ForgePickerSelectedLine", link = false }).bg ~= nil,
      "selected line highlight must supply a background")
    eq(styled_mark["@type"], "Client", { fail_reason = "caller type style must survive viewport remapping" })
    eq(styled_mark["@function"], "send", { fail_reason = "caller method style must cover only its identifier" })
    styled_page.highlight_selected_line, styled_page.highlight_selected_text = nil, nil
    local default_frame = layout.viewport(layout.build(styled_page, 19, 100), 8)
    require("forge.views.picker.render").apply(styled_buf, default_frame)
    local selected_text_count = 0
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(styled_buf, -1, 0, -1, { details = true })) do
      eq(mark[4].line_hl_group, nil, { fail_reason = "default selection must clear any prior line background" })
      if mark[4].hl_group == "ForgePickerSelected" then
        selected_text_count = selected_text_count + 1
        eq(mark[2], default_frame.primary_range[19].first - 1, { fail_reason = "default text highlight must follow selection" })
      end
    end
    eq(selected_text_count, 1, { fail_reason = "text selection must remain enabled by default" })
    vim.api.nvim_buf_delete(styled_buf, { force = true })
    local keyed_frame = layout.build({ option_list = { { key = "a", label = "One" }, { label = "Two" } } }, 1, 100)
    eq(keyed_frame.lines[1]:find("One", 1, true), keyed_frame.lines[2]:find("Two", 1, true),
      { fail_reason = "mixed keyed rows must retain aligned values" })
    eq(keyed_frame.primary_range[1].key_end, 3)
    eq(keyed_frame.primary_range[2].key_end, nil)
    local clipped_frame = layout.build({ option_list = { { label = "unicode", columns = { "界界界界界界界界界界Client" },
      column_segments = { { { "界界界界界界界界界界", "@type" }, { "Client", "@function" } } } } } }, 1, 24)
    local clipped_span = clipped_frame.primary_range[1].spans
    eq(#clipped_span, 1, { fail_reason = "truncated identifiers must not retain hidden highlight spans" })
    assert(clipped_span[1].last < #clipped_frame.lines[1], "truncation ellipsis must not inherit identifier styling")
    local text_page = { column_headers = { "Location", "Caller", "Text" }, flexible_column = 3,
      option_list = { { label = "code", columns = { "src/arena.rs:30", "RoundPlugin::new", "pub fn new(config: ArenaConfig) -> Self {" },
        column_spans = { [3] = { { first = 0, last = 3, group = "@keyword", priority = 100 },
          { first = 0, last = 3, group = "@keyword.modifier", priority = 120 },
          { first = 11, last = 29, group = "@type", priority = 100 } } } } } }
    local text_frame = layout.build(text_page, 1, 58)
    assert(text_frame.lines[2]:find("src/arena.rs:30", 1, true) ~= nil and text_frame.lines[2]:find("RoundPlugin::new", 1, true) ~= nil,
      "long code must clip before its location and caller")
    assert(text_frame.lines[2]:find("…", 1, true) ~= nil, "long text did not clip its end")
    local text_buf = buffer.scratch()
    require("forge.views.picker.render").apply(text_buf, text_frame)
    local priority_by_capture = {}
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(text_buf, -1, 0, -1, { details = true })) do
      if mark[4].hl_group and mark[4].hl_group:find("@keyword", 1, true) then priority_by_capture[mark[4].hl_group] = mark[4].priority end
    end
    assert(priority_by_capture["@keyword.modifier"] > priority_by_capture["@keyword"],
      "overlapping text captures lost their query priorities")
    local widened = layout.build(text_page, 1, 140)
    assert(widened.lines[2]:find(text_page.option_list[1].columns[3], 1, true) ~= nil,
      "resizing did not restore the full source line")
    vim.api.nvim_buf_delete(text_buf, { force = true })
    local rows = {}
    for index = 1, 40 do rows[index] = { label = "Session " .. index, detail = "fixture_alpha" } end
    local scrolling_page = { subtitle = "Search current repository. Open in current tab.",
      search = {}, column_headers = { "Session", "Harness" }, option_list = rows }
    local first_frame = layout.viewport(layout.build(scrolling_page, 1, 100), 12)
    local last_frame = layout.viewport(layout.build(scrolling_page, 40, 100), 12, first_frame.viewport_top)
    eq(#last_frame.lines, 12, { fail_reason = "option viewport must fit the window" })
    for row = 1, first_frame.header_height do
      eq(last_frame.lines[row], first_frame.lines[row], { fail_reason = "picker header must remain fixed" })
    end
    eq(last_frame.search_start, first_frame.search_start, { fail_reason = "search must remain fixed" })
    assert(last_frame.option_range[40] ~= nil and last_frame.option_range[1] == nil,
      "only visible options should receive projected row ranges")
    assert(last_frame.lines[last_frame.option_range[40].first]:find("Session 40", 1, true) ~= nil)

    local spec = {
      page_list = {
        { id = "one", option_list = { { label = "One" }, { label = "Two" } } },
        { id = "two", option_list = { { label = "Three" } } },
      },
    }
    local pure_state = picker_state.new(spec)
    local duplicate_ok = pcall(picker_state.new, {
      page_list = { { id = "same" }, { id = "same" } },
    })
    assert(not duplicate_ok, "picker state should reject duplicate page identities")
    picker_state.move(pure_state, spec, -1)
    eq(picker_state.selected_index(pure_state, spec), 2, { fail_reason = "selection should wrap" })
    local bounded_spec = { page_list = { { id = "bounded", wrap_selection = false,
      option_list = { { label = "One" }, { label = "Two" } } } } }
    local bounded_state = picker_state.new(bounded_spec)
    picker_state.move(bounded_state, bounded_spec, -1)
    eq(picker_state.selected_index(bounded_state, bounded_spec), 1,
      { fail_reason = "bounded selection should stop at the first option" })
    picker_state.move(bounded_state, bounded_spec, 1)
    picker_state.move(bounded_state, bounded_spec, 1)
    eq(picker_state.selected_index(bounded_state, bounded_spec), 2,
      { fail_reason = "bounded selection should stop at the last option" })
    picker_state.move_page(pure_state, spec, 1)
    eq(pure_state.page_index, 2, { fail_reason = "page movement should clamp" })
    picker_state.move_page(pure_state, spec, 1)
    eq(pure_state.page_index, 2, { fail_reason = "page movement should stay in range" })

    local multiple_spec = {
      page_list = { {
        id = "many",
        selection_mode = "multiple",
        option_list = { { id = "one", label = "One" }, { id = "two", label = "Two" } },
      } },
    }
    local multiple_state = picker_state.new(multiple_spec)
    picker_state.toggle_selected(multiple_state, multiple_spec)
    picker_state.move(multiple_state, multiple_spec, 1)
    picker_state.toggle_selected(multiple_state, multiple_spec)
    eq(#picker_state.selected_option_list(multiple_state, multiple_spec), 2,
      { fail_reason = "multiple selection should retain independent choices" })

    local stacked = layout.build({
      option_list = { { key = "n", label = "A long option label", detail = "A long detail that cannot share a narrow row" } },
    }, 1, 24)
    eq(stacked.option_range[1].last, stacked.option_range[1].first,
      { fail_reason = "narrow layouts should truncate cells without moving columns onto another row" })
    local aligned = layout.build({ option_list = {
      { key = "a", label = "Short", detail = "first" },
      { key = "long", label = "A considerably longer title", detail = "second" },
    } }, 1, 100)
    eq(aligned.lines[1]:find("first", 1, true), aligned.lines[2]:find("second", 1, true),
      { fail_reason = "all picker detail columns must align across different label and key widths" })
    local command = "Write-Output FORGE-BG-AUDIT; Start-Sleep -Seconds 90"
    local long_label = layout.build({
      option_list = { { label = command, detail = "Terminal 98704" } },
    }, 1, 120)
    assert(long_label.lines[1]:find(command .. "  Terminal 98704", 1, true) ~= nil,
      "labels wider than the preferred column must retain a separator before details")
    local narrow_label = layout.build({
      option_list = { { label = command, detail = "Terminal 98704" } },
    }, 1, 60)
    for _, line in ipairs(narrow_label.lines) do
      assert(vim.fn.strdisplaywidth(line) <= 56, "long labels must not overflow narrow picker rows")
    end
    local failure_frame = layout.build({ option_list = {},
      empty_text = "MCP status unavailable: MCP status did not finish within 30 seconds: deadline has elapsed",
    }, 1, 60)
    for _, line in ipairs(failure_frame.lines) do
      assert(vim.fn.strdisplaywidth(line) <= 56, "empty and error messages must wrap within the picker")
    end

    local input_page = {
      option_list = { { key = "n", label = "Choice" } },
      allow_input = true,
      input_height = 3,
    }
    local compact_input_frame = layout.build(input_page, 1, 80)
    local expanded_input_frame = layout.build(input_page, 1, 80, { input_visible = true })
    eq(compact_input_frame.input_start, nil, { fail_reason = "input capability should not reserve inactive rows" })
    assert(expanded_input_frame.input_start ~= nil, "visible input should reserve attached editor rows")
    assert(#expanded_input_frame.lines > #compact_input_frame.lines,
      "visible input should expand the picker from its natural height")

    local origin_win = vim.api.nvim_get_current_win()
    picker.open({ height_ratio = 0.3, host = { control_win = origin_win, window_list = { origin_win } },
      page_list = { { id = "height-limit", option_list = rows } } })
    local bounded_picker = picker._state_for_test()
    eq(vim.api.nvim_win_get_height(bounded_picker.win),
      math.min(24, math.max(4, math.floor(vim.api.nvim_win_get_height(origin_win) * 0.3))),
      { fail_reason = "fractional picker height must reserve the host preview" })
    for index = 1, 39 do keymap.invoke("<Down>") end
    assert(bounded_picker.frame.option_range[40] ~= nil,
      "height-limited picker must scroll to the final result")
    vim.api.nvim_set_current_win(origin_win)
    vim.cmd("aboveleft split")
    local resize_window = vim.api.nvim_get_current_win()
    vim.api.nvim_exec_autocmds("WinResized", {})
    vim.wait(100, function() return false end, 10)
    eq(vim.api.nvim_win_get_height(bounded_picker.win),
      math.min(24, math.max(4, math.floor(vim.api.nvim_win_get_height(origin_win) * 0.3))),
      { fail_reason = "resizing the host must recompute the fractional height" })
    assert(bounded_picker.frame.option_range[40] ~= nil,
      "resizing must retain the selected result in the viewport")
    picker.close(false)
    vim.api.nvim_win_close(resize_window, true)
    vim.api.nvim_set_current_win(origin_win)
    vim.cmd("belowright 3split")
    local composer_win = vim.api.nvim_get_current_win()
    vim.wo[composer_win].winhighlight = "Normal:Normal"
    local composer_winhighlight = vim.wo[composer_win].winhighlight
    vim.o.guicursor = ""
    local restored_guicursor = "a:block-Cursor"
    local selected = nil
    local selected_result = nil
    picker.open({
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = {
        {
          id = "choice",
          title = "Select model",
          subtitle = "Choose one model.",
          option_list = {
            { key = "n", label = "Sol", detail = "Fast coding model", value = "sol" },
            { key = "e", label = "Terra", detail = "Balanced coding model", value = "terra" },
          },
          allow_input = true,
          input_height = 3,
        },
        {
          id = "second",
          title = "Second page",
          option_list = { { key = "n", label = "Continue", value = "continue" } },
        },
      },
      on_confirm = function(result)
        selected = result.option.value
        selected_result = result
      end,
    })
    local active = picker._state_for_test()
    eq(vim.api.nvim_get_current_win(), active.win, { fail_reason = "presentation picker should own modal focus" })
    eq(vim.o.guicursor, "n:block-ForgeHiddenCursor",
      { fail_reason = "opening the picker should globally hide its focused cursor" })
    eq(vim.api.nvim_win_get_config(active.win).focusable, true, { fail_reason = "presentation picker should accept focus" })
    local bounds = layout.host_bounds({ origin_win, composer_win })
    local popup_config = vim.api.nvim_win_get_config(active.win)
    eq(popup_config.col, bounds.left, { fail_reason = "picker should align with the host left edge" })
    eq(popup_config.row, bounds.bottom - popup_config.height - 2, { fail_reason = "picker should anchor to the host bottom" })
    keymap.invoke("e")
    eq(active.state.selected_index_by_page.choice, 2, { fail_reason = "jump keys should select without confirming" })
    eq(selected, nil, { fail_reason = "jump keys should not submit" })
    local parent_row = popup_config.row
    local compact_height = popup_config.height
    keymap.invoke("<Tab>")
    assert(vim.api.nvim_win_is_valid(active.input_win), "Tab should open attached feedback input")
    local expanded_config = vim.api.nvim_win_get_config(active.win)
    assert(expanded_config.height > compact_height, "Tab should grow the picker only when feedback opens")
    assert(expanded_config.row < parent_row, "an expanded bottom-anchored picker should grow upward")
    eq(expanded_config.row + expanded_config.height, parent_row + compact_height,
      { fail_reason = "feedback expansion should preserve the picker's bottom edge" })
    local input_border = vim.api.nvim_win_get_config(active.input_win).border
    assert(input_border == "none" or vim.iter(input_border):all(function(border) return border == "" end),
      "attached feedback input should not draw a nested outline")
    eq(vim.wo[active.input_win].winbar, "", { fail_reason = "borderless feedback input should not display a title" })
    eq(vim.wo[active.input_win].foldcolumn, "2",
      { fail_reason = "attached feedback input should reserve the shared prompt gutter" })
    local input_prompt_gutter = vim.api.nvim_eval_statusline(vim.wo[active.input_win].statuscolumn, {
      winid = active.input_win,
      use_statuscol_lnum = 1,
    })
    eq(input_prompt_gutter.str, "❯ ", { fail_reason = "feedback input should render its prompt in the gutter" })
    eq(#vim.api.nvim_buf_get_extmarks(active.input_buf, -1, 0, -1, { details = true }), 0,
      { fail_reason = "feedback input should not shift buffer text with inline virtual text" })
    assert(active.frame.lines[active.frame.footer_line]:find("<Tab> options", 1, true) ~= nil,
      "open input help should expose Tab option focus")
    assert(active.frame.lines[active.frame.footer_line]:find("<C-c> clear", 1, true) ~= nil,
      "open input help should expose Ctrl-c clearing")
    eq(vim.api.nvim_get_current_win(), active.input_win,
      { fail_reason = "opening attached input should transfer focus into its editor" })
    buffer.set(active.input_buf, { "Use this for refactors", "and reviews" })
    local retained_input_win = active.input_win
    keymap.invoke("<Tab>", { mode = "i" })
    eq(vim.api.nvim_get_current_win(), active.win, { fail_reason = "Tab should return focus to the picker" })
    eq(vim.o.guicursor, "n:block-ForgeHiddenCursor",
      { fail_reason = "Tab should globally hide the cursor while picker controls own focus" })
    assert(vim.api.nvim_win_is_valid(retained_input_win), "Tab should keep attached input open")
    keymap.invoke("n")
    eq(active.state.selected_index_by_page.choice, 1,
      { fail_reason = "option navigation should remain available while attached input stays open" })
    eq(picker_input.text(active.input_buf), "Use this for refactors\nand reviews",
      { fail_reason = "changing options should retain feedback for the newly selected option" })
    keymap.invoke("e")
    keymap.invoke("<Tab>")
    eq(vim.api.nvim_get_current_win(), active.input_win,
      { fail_reason = "Tab on picker controls should refocus the existing input" })
    eq(vim.o.guicursor, restored_guicursor,
      { fail_reason = "refocusing attached input should restore an explicit visible cursor" })
    keymap.invoke("<Tab>", { mode = "i" })
    keymap.invoke("<Right>")
    assert(vim.api.nvim_win_get_config(active.win).height < expanded_config.height,
      "leaving feedback should shrink the picker to the destination page's natural height")
    eq(active.frame.input_start, nil, { fail_reason = "leaving feedback should release attached editor rows" })
    eq(active.state.page_index, 2, { fail_reason = "Right should move to the next page" })
    keymap.invoke("<Left>")
    keymap.invoke("<Tab>")
    eq(buffer.line(active.input_buf, 2), "and reviews",
      { fail_reason = "page navigation should preserve multiline drafts" })
    keymap.invoke("go")
    keymap.invoke("go")
    eq(vim.api.nvim_get_current_win(), active.input_win, { fail_reason = "go should return to attached input" })
    eq(vim.api.nvim_get_mode().mode:sub(1, 1), "n",
      { fail_reason = "returning to attached input with go should preserve Normal mode" })
    keymap.invoke("<C-s>", { mode = "i" })
    eq(selected, "terra", { fail_reason = "input submission should preserve the selected option" })
    eq(selected_result.text, "Use this for refactors\nand reviews",
      { fail_reason = "input submission should attach retained feedback to the selected option" })
    assert(not picker.is_open(), "successful selection should close the picker")

    picker.open({
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { {
        id = "clear",
        title = "Clear feedback",
        option_list = { { key = "n", label = "Choice", value = "choice" } },
        allow_input = true,
        input_height = 3,
      } },
    })
    keymap.invoke("<Tab>")
    local clear_instance = picker._state_for_test()
    local cleared_input_buf = clear_instance.input_buf
    local expanded_clear_height = vim.api.nvim_win_get_config(clear_instance.win).height
    buffer.set(cleared_input_buf, { "discard this feedback" })
    keymap.invoke("<C-c>", { mode = "i" })
    eq(clear_instance.input_buf, nil, { fail_reason = "Ctrl-c should release picker input state" })
    assert(not vim.api.nvim_buf_is_valid(cleared_input_buf), "Ctrl-c should destroy the picker input buffer")
    eq(clear_instance.state.draft_by_page.clear, "", { fail_reason = "Ctrl-c should clear the page feedback draft" })
    eq(vim.api.nvim_get_current_win(), clear_instance.win, { fail_reason = "Ctrl-c should restore picker focus" })
    assert(vim.api.nvim_win_get_config(clear_instance.win).height < expanded_clear_height,
      "Ctrl-c should shrink the picker after closing input")
    assert(picker.is_open(), "Ctrl-c should keep the picker itself open")
    eq(vim.o.guicursor, "n:block-ForgeHiddenCursor",
      { fail_reason = "Ctrl-c should hide the cursor after returning to picker controls" })
    picker.close(false)
    eq(vim.wo[composer_win].winhighlight, composer_winhighlight,
      { fail_reason = "closing the picker should restore control highlights" })
    eq(vim.o.guicursor, restored_guicursor, { fail_reason = "closing the picker should restore a visible cursor" })

    local closed = false
    picker.open({
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { { id = "close", title = "Close", option_list = { { key = "n", label = "One" } } } },
      on_close = function() closed = true end,
    })
    keymap.invoke("q")
    assert(closed, "q should close without resolving")
    assert(not picker.is_open(), "q should remove the picker")

    local multiple_result = nil
    picker.open({
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { {
        id = "multiple",
        title = "Select checks",
        selection_mode = "multiple",
        option_list = {
          { key = "n", id = "format", label = "Format" },
          { key = "e", id = "test", label = "Test" },
          { key = "b", id = "blocked", label = "Blocked", disabled_reason = "A native policy prevents selection" },
          { key = "y", action = "submit", label = "Apply selections" },
        },
      } },
      on_confirm = function(result) multiple_result = result.selected_option_list end,
    })
    keymap.invoke("<CR>")
    keymap.invoke("e")
    keymap.invoke("<CR>")
    keymap.invoke("b")
    keymap.invoke("<CR>")
    keymap.invoke("y")
    eq(multiple_result, nil, { fail_reason = "multiple-choice jump keys should not submit" })
    keymap.invoke("<CR>")
    eq(#multiple_result, 2, { fail_reason = "multiple-choice submission should return every selected option" })

    local disposed = 0
    picker.open({
      page_list = { { id = "dispose", option_list = {} } },
      on_dispose = function() disposed = disposed + 1 end,
    })
    picker.close(false)
    picker.close(false)
    eq(disposed, 1, { fail_reason = "silent close must release owned subscriptions exactly once" })

    local replaced_closed = false
    picker.open({
      owner = "first",
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { { id = "first", option_list = { { label = "First" } } } },
      on_close = function() replaced_closed = true end,
    })
    picker.open({
      owner = "second",
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { { id = "second", option_list = { { label = "Second" } } } },
    })
    assert(replaced_closed, "replacing a picker should notify the previous workflow")
    assert(picker.is_open("second"), "picker ownership should distinguish the active workflow")
    assert(not picker.is_open("first"), "inactive workflow ownership should not leak")
    picker.close(false)

    local previewed = {}
    local deleted = nil
    local scope_toggled = false
    local search_options = {}
    for option_index = 1, 10 do
      search_options[#search_options + 1] = {
        id = "session-" .. option_index,
        label = option_index == 7 and "Gamma migration" or ("Session " .. option_index),
        value = option_index,
      }
    end
    picker.open({
      owner = "search",
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { {
        id = "search",
        title = "Harness sessions",
        subtitle = "Search sessions.",
        option_list = search_options,
        show_item_counter = true,
        search = { choice_keys = { "n", "e", "a", "i", "l", "u", "o", "y" } },
      } },
      action_list = {
        {
          key = "<C-j>",
          modes = { "n", "i" },
          callback = function(context) deleted = context.option.value end,
        },
        {
          key = "<C-o>",
          modes = { "n", "i" },
          callback = function() scope_toggled = true end,
        },
      },
      on_change = function(context) previewed[#previewed + 1] = context.option and context.option.value end,
    })
    active = picker._state_for_test()
    eq(vim.api.nvim_get_current_win(), active.search_win, { fail_reason = "search picker should focus its input" })
    eq(active.state.query_by_page.search, "", { fail_reason = "search should begin empty" })
    eq(picker_state.page(active.state, active.spec).option_list[8].key, "y",
      { fail_reason = "the first eight search results should receive jump keys" })
    eq(picker_state.page(active.state, active.spec).option_list[9].key, nil,
      { fail_reason = "search results after eight should not receive jump keys" })
    buffer.set(active.search_buf, { "gma" })
    vim.api.nvim_exec_autocmds("TextChangedI", { buffer = active.search_buf })
    eq(#picker_state.page(active.state, active.spec).option_list, 1, { fail_reason = "fuzzy search should filter by session name" })
    assert(active.frame.lines[active.frame.footer_line]:match("1 of 1$"),
      "counter must use the filtered result count")
    eq(picker_state.selected_option(active.state, active.spec).label, "Gamma migration",
      { fail_reason = "fuzzy search should retain the matching session" })
    keymap.invoke("<C-j>", { mode = "i" })
    eq(deleted, 7, { fail_reason = "Ctrl-j should act on the filtered selection" })
    keymap.invoke("<C-o>", { mode = "i" })
    assert(scope_toggled, "Ctrl-o should toggle picker scope from Insert mode")
    vim.cmd("stopinsert")
    buffer.set(active.search_buf, { "" })
    vim.api.nvim_exec_autocmds("TextChanged", { buffer = active.search_buf })
    keymap.invoke("o")
    eq(picker_state.selected_option(active.state, active.spec).value, 7,
      { fail_reason = "normal-mode jump keys should select a visible search result without confirming" })
    eq(previewed[#previewed], 7, { fail_reason = "search selection changes should publish preview updates" })
    buffer.set(active.search_buf, { "zzzz" })
    vim.api.nvim_exec_autocmds("TextChanged", { buffer = active.search_buf })
    eq(#picker_state.page(active.state, active.spec).option_list, 0,
      { fail_reason = "unmatched search text should produce an explicit empty result set" })
    assert(buffer.text(active.buf):find("No matching options.", 1, true) ~= nil,
      "an empty result set should render its explanatory row")
    eq(vim.api.nvim_win_call(active.win, vim.fn.winsaveview).topline, 1,
      { fail_reason = "an empty result set should reset the presentation view to its title content" })
    picker.close(false)

    local long_options = {}
    for index = 1, 50 do long_options[index] = { label = "Option " .. index } end
    local overflow_spec = {
      owner = "overflow",
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { { id = "overflow", option_list = long_options, footer = "Enter insert  q close", show_item_counter = true } },
    }
    picker.open(overflow_spec)
    active = picker._state_for_test()
    assert(vim.api.nvim_win_get_config(active.win).footer[1][1]:find("Enter insert", 1, true),
      "overflowing picker lost its visible controls")
    eq(active.frame.footer_line, nil, { fail_reason = "overflowing picker duplicated its footer in scrollable content" })
    assert(vim.api.nvim_win_get_config(active.win).footer[1][1]:match("1 of 50%s*$"),
      "overflowing picker did not show its first selected item")
    eq(vim.fn.strdisplaywidth(vim.api.nvim_win_get_config(active.win).footer[1][1]),
      vim.api.nvim_win_get_width(active.win) - 2, { fail_reason = "border counter must align at the right inset" })
    keymap.invoke("<Up>")
    eq(picker_state.selected_index(active.state, active.spec), 50)
    assert(vim.api.nvim_win_get_config(active.win).footer[1][1]:match("50 of 50%s*$"),
      "counter did not follow selection outside the initial viewport")
    assert(vim.api.nvim_win_get_config(active.win).footer[1][1]:find("q close", 1, true))
    overflow_spec.page_list[1].option_list = { { label = "Only option" } }
    picker.update(overflow_spec)
    active = picker._state_for_test()
    assert(active.frame.footer_line ~= nil, "short picker did not restore its inline footer")
    assert(active.frame.lines[active.frame.footer_line]:match("1 of 1$"), "inline counter retained the old total")
    eq(vim.fn.strdisplaywidth(active.frame.lines[active.frame.footer_line]),
      vim.api.nvim_win_get_width(active.win) - 2, { fail_reason = "inline counter must align at the right inset" })
    assert(not vim.inspect(vim.api.nvim_win_get_config(active.win).footer):find("Enter insert", 1, true),
      "short picker retained a stale border footer")
    overflow_spec.page_list[1].option_list = {}
    picker.update(overflow_spec)
    assert(active.frame.lines[active.frame.footer_line]:match("0 of 0$"), "empty counter must not invent a selection")
    overflow_spec.page_list[1].show_item_counter = nil
    picker.update(overflow_spec)
    assert(not active.frame.lines[active.frame.footer_line]:find("of 0", 1, true),
      "item counter must remain opt-in")
    picker.close(false)

    local page_spec = {
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = {
        { id = "first", option_list = { { label = "First decision" } } },
        { id = "second", option_list = { { label = "Second decision" } } },
      },
    }
    picker.open(page_spec)
    keymap.invoke("]")
    picker.update(page_spec)
    active = picker._state_for_test()
    eq(picker_state.page(active.state, active.spec).id, "second", { fail_reason = "refresh must retain the selected page" })
    page_spec.initial_page = 1
    picker.update(page_spec)
    active = picker._state_for_test()
    eq(picker_state.page(active.state, active.spec).id, "first", { fail_reason = "explicit decision navigation must override page retention" })
    page_spec.initial_page = 2
    picker.update(page_spec)
    active = picker._state_for_test()
    eq(picker_state.page(active.state, active.spec).id, "second", { fail_reason = "multi-page forms must advance after a decision" })
    picker.close(false)

    require("forge.views.harness.model_picker").open({
      provider = "fixture",
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      model_list = { { id = "example", reasoning = { "medium", "high" },
        context_window = { { id = "small", token_limit = 100000 } }, description = "Example model" } },
      current_model = "example",
      on_confirm = function() end,
    })
    active = picker._state_for_test()
    eq(active.spec.page_list[1].column_headers,
      { "Model", "Reasoning", "Context", "Description" }, { fail_reason = "model picker should label each visible field" })
    eq(active.spec.page_list[1].subtitle, nil, { fail_reason = "model picker should omit the instructional subtitle" })
    eq(#active.spec.page_list[1].option_list[1].columns, 4,
      { fail_reason = "model values should align with the visible headers" })
    local original_reasoning = active.spec.page_list[1].option_list[1].value.picker_reasoning
    keymap.invoke("h")
    assert(active.spec.page_list[1].option_list[1].value.picker_reasoning ~= original_reasoning)
    keymap.invoke("r")
    eq(active.spec.page_list[1].option_list[1].value.picker_reasoning, original_reasoning)
    keymap.invoke("<Right>")
    assert(active.spec.page_list[1].option_list[1].value.picker_reasoning ~= original_reasoning)
    keymap.invoke("<Left>")
    eq(active.spec.page_list[1].option_list[1].value.picker_reasoning, original_reasoning)
    local picker_keymaps = require("forge.infra.config").options.keymaps.picker
    local previous_value, next_value = picker_keymaps.previous_value, picker_keymaps.next_value
    picker_keymaps.previous_value, picker_keymaps.next_value = { "b" }, false
    picker.update(active.spec)
    keymap.invoke("b")
    assert(active.spec.page_list[1].option_list[1].value.picker_reasoning ~= original_reasoning,
      "picker value bindings ignored the shared configuration")
    eq(vim.fn.maparg("r", "n", false, true).callback, nil, { fail_reason = "update retained the old previous-value alias" })
    eq(vim.fn.maparg("h", "n", false, true).callback, nil, { fail_reason = "disabled next-value retained its alias" })
    picker_keymaps.previous_value, picker_keymaps.next_value = previous_value, next_value
    picker.close(false)

    picker.open({
      host = { window_list = { origin_win, composer_win }, control_win = composer_win },
      page_list = { { id = "mode-colors", option_list = {
        { label = "Read (current)", detail = "Current mode", highlight_group = "ForgeHarnessRead", highlight_text = "Read" },
        { label = "Full", detail = "Approval mode", highlight_group = "ForgeHarnessFull", highlight_text = "Full" },
      } } },
    })
    active = picker._state_for_test()
    local color_mark_set = {}
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(active.buf, -1, 0, -1, { details = true })) do
      if mark[4].hl_group == "ForgeHarnessRead" or mark[4].hl_group == "ForgeHarnessFull" then
        color_mark_set[mark[4].hl_group] = mark[4].end_col - mark[3]
      end
    end
    eq(color_mark_set, { ForgeHarnessRead = 4, ForgeHarnessFull = 4 },
      { fail_reason = "mode colors should cover only their labels in selected and unselected rows" })
    picker.close(false)
  end
end

T["field_input"] = function()
  vim.o.columns, vim.o.lines = 76, 32
  require("forge").setup({ harness = { provider = "mock" } })

  local picker = require("forge.views.picker")
  local requests = {}
  local value_field = { id = "amount", kind = "input", value = "10", description = "Timeout amount.",
    input = { kind = "integer", minimum = 0, maximum = 4294967295 } }
  local spec = { owner = "field-input-test", title = "Settings", hint_mode = "help", page_navigation = "tabs",
    keymaps = { page_next = "<Tab>", page_previous = "<S-Tab>", next_field = "<C-Right>", previous_field = "<C-Left>" },
    page_list = {
      { id = "first", title = "First", field_navigation = true, field_description_height = 2, option_list = {
        { id = "timeout", label = "Timeout", fields = { value_field,
          { id = "unit", kind = "choice", value = "m", join_previous = true } } },
      } },
      { id = "second", title = "Second", option_list = {} },
    },
    on_field_submit = function(context, value, complete)
      requests[#requests + 1] = { context = context, value = value, complete = complete }
    end,
  }

  local function begin_edit()
    local instance = picker._state_for_test()
    keymap.invoke("<CR>", { buffer = instance.buf })
    local editor = assert(instance.field_editor, "Enter did not open an editor")
    assert(vim.api.nvim_get_current_win() == editor.window)
    assert(vim.bo[editor.buffer].filetype == "ForgePickerFieldInput")
    return editor
  end

  local function assert_draft(editor, text)
    assert(buffer.line(editor.buffer, 1) == text, "draft was replaced")
  end

  MiniTest.finally(function() pcall(picker.close, false) end)
  do
    picker.open(spec)
    local editor = begin_edit()
    assert_draft(editor, "10")
    buffer.set(editor.buffer, { "not a number" })
    keymap.invoke("<CR>", { buffer = editor.buffer })
    assert(#requests == 0 and editor.error, "invalid text was saved")
    assert_draft(editor, "not a number")
    assert(table.concat(picker._state_for_test().frame.lines, " "):find("Enter a whole number.", 1, true))
    for _, invalid in ipairs({ "-1", "1.5", "4294967296", "" }) do
      buffer.set(editor.buffer, { invalid })
      keymap.invoke("<CR>", { buffer = editor.buffer })
      assert(#requests == 0 and editor.error)
    end
    buffer.set(editor.buffer, { "20" })
    assert(keymap.get("<Tab>", { buffer = editor.buffer }).rhs == "<Nop>", "missing key: <Tab>")
    assert(keymap.get("<S-Tab>", { buffer = editor.buffer }).rhs == "<Nop>", "missing key: <S-Tab>")
    assert(picker._state_for_test().state.page_index == 1, "editing changed categories")
    value_field.value = "11"
    vim.o.columns = 40
    picker.update(spec)
    assert_draft(editor, "20")
    assert(vim.api.nvim_get_current_win() == editor.window, "redraw stole editor focus")
    local parent = vim.api.nvim_win_get_config(picker._state_for_test().win)
    local position = vim.api.nvim_win_get_config(editor.window)
    assert(position.col >= parent.col and position.col + position.width <= parent.col + parent.width)
    keymap.invoke("<CR>", { buffer = editor.buffer })
    assert(#requests == 1 and requests[1].value == 20 and editor.pending)
    keymap.invoke("<CR>", { buffer = editor.buffer })
    keymap.invoke("<Esc>", { buffer = editor.buffer })
    assert(#requests == 1 and picker._state_for_test().field_editor == editor, "pending save was submitted twice or cancelled")
    requests[1].complete("Save failed. Retry.")
    assert(not editor.pending and editor.error == "Save failed. Retry.")
    assert_draft(editor, "20")
    assert(vim.bo[editor.buffer].modifiable)
    keymap.invoke("<CR>", { buffer = editor.buffer })
    value_field.value = "20"
    picker.update(spec)
    assert_draft(editor, "20")
    requests[2].complete()
    assert(not picker._state_for_test().field_editor and not vim.api.nvim_win_is_valid(editor.window))
    editor = begin_edit()
    buffer.set(editor.buffer, { "77" })
    keymap.invoke("<Esc>", { buffer = editor.buffer })
    assert(value_field.value == "20" and #requests == 2, "cancel saved a draft")
    editor = begin_edit()
    buffer.set(editor.buffer, { "0" })
    keymap.invoke("<CR>", { buffer = editor.buffer })
    assert(requests[3].value == 0, "zero timeout was rejected")
    picker.open({ owner = "other-session", page_list = { { id = "other", option_list = {} } } })
    assert(not vim.api.nvim_win_is_valid(editor.window), "replacement picker leaked the editor")
    requests[3].complete()
    assert(picker.is_open("other-session"), "late save replaced another picker")
    spec.page_list[1].option_list[2] = { id = "second-timeout", label = "Other timeout", fields = {
      { id = "amount", kind = "input", value = "123", input = { kind = "integer", minimum = 0 } },
    } }
    spec.page_list[1].selected_index = 2
    picker.open(spec)
    editor = begin_edit()
    assert_draft(editor, "123")
    local instance = picker._state_for_test()
    local position = vim.api.nvim_win_get_config(editor.window)
    local parent = vim.api.nvim_win_get_config(instance.win)
    assert(position.row == parent.row + instance.frame.primary_range[2].first,
      "editor attached to another row with the same field id")
    table.remove(spec.page_list[1].option_list, 2)
    picker.update(spec)
    assert(not picker._state_for_test().field_editor and not vim.api.nvim_win_is_valid(editor.window),
      "removed row retained an active editor")
  end
end

T["navigation"] = function()
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
    eq(visible, original, { fail_reason = "composition mutated its source viewport" })
    eq(geometry.mode, "tabs")
    eq(geometry.content_width, 100)
    eq(composed.lines[1], "  [模型]  Tools")
    for line, content in ipairs(original.lines) do
      eq(composed.lines[line + geometry.header_height], content)
    end
    for _, name in ipairs({ "option_range", "primary_range", "child_range", "content_range" }) do
      for index, range in pairs(original[name]) do
        eq(composed[name][index].first, range.first + geometry.header_height)
        eq(composed[name][index].last, range.last + geometry.header_height)
        eq(composed[name][index].spans, range.spans)
      end
    end
    eq(composed.input_start, original.input_start + geometry.header_height)
    eq(composed.search_start, original.search_start + geometry.header_height)
    picker.open(spec)
    local instance = picker._state_for_test()
    require("forge.views.picker.render").apply(instance.buf, composed)
    local active_tab = false
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(instance.buf, -1, 0, -1, { details = true })) do
      if mark[2] == 0 and mark[4].hl_group == "ForgePickerSelected" then
        eq(composed.lines[1]:sub(mark[3] + 1, mark[4].end_col), "[模型]")
        active_tab = true
      end
    end
    assert(active_tab, "active Unicode tab is not highlighted")
    local parent = vim.api.nvim_win_get_config(instance.win)
    for _, module in ipairs({ "forge.views.picker.input", "forge.views.picker.search_input" }) do
      local _, window = require(module).open(instance.win, composed, "draft", instance.origin, function() end)
      owned_window[#owned_window + 1] = window
      local position = vim.api.nvim_win_get_config(window)
      eq(position.col, parent.col + 2)
      eq(position.width, geometry.content_width - 4)
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
          eq(active, "[" .. page.title .. "]")
        end
        eq(composed.content_width, width)
        eq(composed.content_column, 0)
      end
    end
    eq(navigation.measure({ pages[1] }, "tabs", 40, 20).mode, "none")
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
      eq(frame.option_range[2].first, frame.option_range[1].last + 1)
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
        eq(help.last - help.first + 1, 2)
        eq(frame.footer_line - help.last, 1)
        eq(#frame.lines, #first_frame.lines, { fail_reason = "help resized the picker" })
        eq(frame.option_range, first_frame.option_range, { fail_reason = "help displaced setting rows" })
        for _, line in ipairs(frame.lines) do assert(vim.fn.strdisplaywidth(line) <= width) end
        if selected == 2 then assert(frame.lines[help.last]:find("…", 1, true), "overflow is not indicated") end
        if selected == 3 then
          assert(vim.trim(frame.lines[help.first]) == "" and frame.lines[help.last] == "")
        end
        if selected == 4 then assert(frame.lines[help.first]:find("Unavailable.", 1, true)) end
        local visible = layout.viewport(frame, 6)
        local visible_help = visible.content_range[1]
        eq(visible_help.last - visible_help.first + 1, 2)
        eq(visible.footer_line - visible_help.last, 1)
        eq(#visible.lines, 6)
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
    eq(instance.frame.content_column, 0)
    assert(instance.frame.lines[1]:find("[模型]", 1, true), "active category tab is missing")
    local footer = table.concat(vim.list_slice(instance.frame.lines, assert(instance.frame.footer_line)), " ")
    eq(footer:gsub("%s+", " "):gsub("^ ", ""),
      picker._navigation_footer(instance):gsub("%s+", " "), { fail_reason = "narrow layout clipped its navigation hints" })
    assert(#instance.frame.lines <= vim.api.nvim_win_get_height(instance.win), "footer displaced visible content outside the popup")
    assert(instance.frame.option_scroll_max > 0, "long field became unreachable")
    keymap.invoke("<PageDown>", { buffer = picker._state_for_test().buf })
    assert(picker._state_for_test().frame.option_offset > 0, "long field did not scroll")
    vim.api.nvim_win_set_width(host, host_width)
    vim.api.nvim_win_set_height(host, host_height)
    picker.update(spec)
    instance = picker._state_for_test()
    eq(instance.frame.content_column, 0)
    eq(picker_state.selected_field(instance.state, instance.spec).id, "main-model")
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
        eq(columns, { columns[1], columns[1], columns[1] }, { fail_reason = "model values do not share a column" })
        for index, line in ipairs(frame.lines) do
          assert(vim.fn.strdisplaywidth(line) <= width, "responsive form overflowed")
          eq(line:gsub("←", " "):gsub("→", " "):gsub("%s+$", ""),
            first.lines[index]:gsub("←", " "):gsub("→", " "):gsub("%s+$", ""), { fail_reason = "focus moved form columns" })
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
    keymap.invoke("]", { buffer = picker._state_for_test().buf })
    eq(picker._state_for_test().state.page_index, 2)
    eq(changes, {})
    keymap.invoke("]", { buffer = picker._state_for_test().buf })
    eq(changes, {}, { fail_reason = "category boundary activated the colliding option" })
    keymap.invoke("<Right>", { buffer = picker._state_for_test().buf })
    eq(changes, { { "tool-value", "change" } })
    eq(picker._state_for_test().state.page_index, 2)
    config.options.keymaps.picker.page_previous = "{"
    config.options.keymaps.picker.page_next = "}"
    picker.update(spec)
    keymap.invoke("{", { buffer = picker._state_for_test().buf })
    eq(picker._state_for_test().state.page_index, 1)
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
    eq(instance.state.selected_index_by_page.models, 1, { fail_reason = "inactive page lost its row identity" })
    instance.state.field_by_page.tools.tool = "tool-value"
    eq(snapshot.field_by_page.tools.tool, "tool-enabled", { fail_reason = "snapshot retained mutable state" })
    picker.close(false)
    updated.resume_state = snapshot
    picker.open(updated)
    instance = picker._state_for_test()
    eq(instance.state.page_index, 2)
    eq(picker_state.selected_field(instance.state, instance.spec).id, "tool-enabled")
    eq(instance.state.query_by_page.tools, "Tool")
    eq(instance.state.draft_by_page.models, "unsent")
    picker.close(false)
    updated.page_list[1].option_list = { updated.page_list[1].option_list[2] }
    updated.initial_page = 1
    local restored = picker_state.new(updated)
    eq(restored.selected_index_by_page.models, 1, { fail_reason = "removed selection did not clamp" })
    eq(restored.page_index, 1, { fail_reason = "explicit initial page did not win" })
    updated.owner = "different-owner"
    local isolated = picker_state.new(updated)
    eq(isolated.draft_by_page.models, nil, { fail_reason = "resume state crossed picker owners" })
    local initial = field_spec(function() end)
    local initial_state = picker_state.new(initial)
    local initial_snapshot = picker_state.capture(initial_state, initial)
    table.insert(initial.page_list[1].option_list[1].fields, 1, { id = "inserted", kind = "choice", value = "new" })
    initial.resume_state = initial_snapshot
    local shifted = picker_state.new(initial)
    eq(picker_state.selected_field(shifted, initial).id, "main-model", { fail_reason = "default field lost its identity" })
  end

  --- Verify real configuration child editors restore their origin and reject stale responses.
  local function child_picker_return_preserves_configuration_location()
    local client = require("forge.client")
  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
    local controller = require("forge.views.harness.controller")
    local settings = require("forge.views.harness.settings")
    local background = require("forge.integrations.background_settings")
    local original_request, original_configure, original_background = client.request, controller.configure, background.request
    local state = { session = {
      id = "navigation-session", provider = "fixture_alpha", model = "first", effort = "low", workspace = vim.fn.getcwd(),
    }, capability = { model_selection = true, effort_selection = true, native_compact = true, os_sandbox = true } }
    local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
    local pending_response, pending_background
    local preferences = { provider = "fixture_alpha", auto_compaction = { enabled = true, threshold_tokens = 128000 },
      models = { fixture_alpha = { model = "first", thinking = "low" } } }
    client.request = function(request, callback)
      if request.method == "host.trace.status" then callback({ level = "none", path = vim.fn.tempname() })
      elseif request.method == "session.provider.model.list" then callback({
        { id = "first", reasoning = { "low", "high" }, default_reasoning = "low" },
        { id = "second", reasoning = { "low", "high" }, default_reasoning = "low" },
      })
      elseif request.method == "session.agent.model.list" then
        assert(request.params.provider == "fixture_alpha", "Background catalog ignored the saved provider")
        callback({ capability = {}, models = {
          { id = "first", reasoning = { "low", "high" }, default_reasoning = "low" },
          { id = "second", reasoning = { "low", "high" }, default_reasoning = "low" },
        } })
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
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      assert(not picker.is_open("harness-config"), "main selector did not open")
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      assert(pending_response and not picker.is_open(), "model completion was not deferred")
      pending_response()
      local instance = picker._state_for_test()
      eq(instance.spec.page_list[instance.state.page_index].id, "models")
      eq(picker_state.selected_field(instance.state, instance.spec).id, "main-model")
      select_field("ask-override")
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      select_field("ask-model")
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      picker.close(true)
      instance = picker._state_for_test()
      eq(picker_state.selected_field(instance.state, instance.spec).id, "ask-model")
      select_field("background-model")
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      assert(pending_background and not picker.is_open())
      pending_background()
      instance = picker._state_for_test()
      eq(picker_state.selected_field(instance.state, instance.spec).id, "background-model")
      select_field("readable-directories")
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      eq(vim.bo.buftype, "acwrite", { fail_reason = "directories did not open an editable buffer" })
      keymap.invoke("q")
      instance = picker._state_for_test()
      eq(instance.spec.page_list[instance.state.page_index].id, "permissions")
      eq(picker_state.selected_field(instance.state, instance.spec).id, "readable-directories")
      select_field("main-thinking")
      keymap.invoke("<Right>", { buffer = picker._state_for_test().buf })
      keymap.invoke("]", { buffer = picker._state_for_test().buf })
      pending_response()
      instance = picker._state_for_test()
      eq(instance.spec.page_list[instance.state.page_index].id, "workflow", { fail_reason = "late redraw restored old navigation" })
      select_field("main-model")
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
      keymap.invoke("<CR>", { buffer = picker._state_for_test().buf })
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
    eq(vim.trim(footer[1][1]), "g? help", { fail_reason = "long values exposed inline commands" })
    local before = picker.capture_state()
    keymap.invoke("g?", { buffer = picker._state_for_test().buf })
    local help_window = vim.api.nvim_get_current_win()
    eq(vim.bo.filetype, "ForgeHelp")
    local text = buffer.text(0)
    assert(text:find("}", 1, true) and text:find("{", 1, true), "help ignored category overrides")
    assert(text:find("g?", 1, true), "help ignored its own override")
    picker.update(spec)
    eq(vim.api.nvim_get_current_win(), help_window, { fail_reason = "redraw stole focus from help" })
    eq(picker.capture_state(), before, { fail_reason = "help changed navigation state" })
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
    eq(instance.frame.content_column, nil)
    eq(instance.frame.body_start_by_line, nil)
    keymap.invoke("]", { buffer = picker._state_for_test().buf })
    eq(picker._state_for_test().state.page_index, 2)
    keymap.invoke("[", { buffer = picker._state_for_test().buf })
    keymap.invoke("<Tab>", { buffer = picker._state_for_test().buf })
    instance = picker._state_for_test()
    assert(instance.input_win and vim.api.nvim_win_is_valid(instance.input_win))
    local position = vim.api.nvim_win_get_config(instance.input_win)
    eq(position.width, vim.api.nvim_win_get_width(instance.win) - 4)
    picker.close(false)
    eq(vim.api.nvim_get_current_win(), origin)
    eq(vim.o.guicursor, cursor)
  end

  --- Run scenarios in one isolated editor and release owned UI resources on failure.
  local function run_picker_navigation_tests()
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

  MiniTest.finally(function()
    pcall(picker.close, false)
    for _, window in ipairs(owned_window) do
      if vim.api.nvim_win_is_valid(window) then pcall(popup_window.close, window, false) end
    end
  end)
  run_picker_navigation_tests()
end

T["popup_parity"] = function()
  local fixture = dofile("nvim/lua/forge/tests/support/status_fixture.lua")
  local status = require("forge.status")
  local renderer = require("forge.status_render")
  local dialogs = require("forge.views.status.dialogs")
  local confirm = require("forge.infra.confirm")
  local input_count, last_input = 0, nil
  vim.ui.select = function() error("Legacy Forge interactions must not open a Snacks picker") end
  vim.ui.input = function() error("Legacy Forge dialogs must not open a Snacks input") end
  vim.notify = function() end
  status._set_runner_for_test(function(_, params, callback)
    if params.operation == "open" then
      local snapshot = fixture.snapshot(params.document)
      snapshot.file[2] = fixture.file("second.txt", 2)
      snapshot.section[1].file = { 1, 2 }
      callback(snapshot)
    elseif params.operation == "input" then
      input_count, last_input = input_count + 1, params
      callback(nil, "fixture captured mutation")
    elseif params.operation == "close" then callback({ closed = true })
    elseif params.operation == "close_view" then callback({})
    else error("Unexpected request " .. params.operation) end
  end)
  local state = status.open({ workspace = vim.fn.getcwd() })
  wait_until(function() return state.ready end, "Status did not open")
  local origin = vim.api.nvim_get_current_win()
  for _, cancel in ipairs({ "n", "q", "<Esc>" }) do
    vim.api.nvim_win_set_cursor(origin, { 4, 0 })
    status.action(state, "discard", false)
    assert(vim.bo.filetype == "ForgeConfirm")
    assert(vim.deep_equal(buffer.lines(0), {
      "Discard ALL changes to file?", "  source.txt", "", "  [y] yes    [n] no",
    }))
    local geometry = vim.api.nvim_win_get_config(0)
    assert(geometry.relative == "editor" and geometry.height == 4)
    keymap.feed(cancel)
    assert(vim.api.nvim_get_current_win() == origin and input_count == 0, "cancel changed Git state or lost the Status window")
  end
  vim.api.nvim_win_set_cursor(origin, { 4, 0 })
  vim.cmd("normal! V2j")
  status.action(state, "discard", true)
  assert(buffer.line(0, 1) == "Discard changes in 2 file(s)?")
  assert(input_count == 0, "discard ran before confirmation")
  keymap.feed("y")
  wait_until(function() return input_count == 1 and not state.request_active end,
    "confirmed discard did not send exactly one input")
  assert(vim.api.nvim_get_current_win() == origin and vim.fn.mode() == "n")
  assert(last_input.selection.target[1].id == 1 and last_input.selection.target[2].id == 2)
  assert(last_input.input.action == "discard")
  state.replica.file[1].record.untracked = true
  assert(dialogs.discard_message(state.replica, { location = { kind = "file", id = 1 } })[1] == "Delete untracked file?")
  state.replica.file[1].display_path = "../source.txt"
  assert(dialogs.discard_message(state.replica, { location = { kind = "file", id = 1 } })[2] == "  ../source.txt")
  assert(dialogs.discard_message(state.replica, { location = { kind = "body", file = 1 } })[1] == "Discard this hunk?")
  assert(dialogs.discard_message(state.replica, { location = { kind = "section", section = "unstaged" } })[1] == "Discard changes in 2 file(s)?")
  local yes, no = 0, 0
  local dialog = confirm.open({ "Cancel on leaving" }, function() yes = yes + 1 end, function() no = no + 1 end)
  vim.api.nvim_set_current_win(origin)
  assert(no == 1 and yes == 0 and not vim.api.nvim_buf_is_valid(dialog))
  vim.api.nvim_win_set_cursor(origin, { 4, 0 })
  status.action(state, "discard", false)
  local stale_dialog = vim.api.nvim_get_current_buf()
  local stale_confirm = keymap.get("y").callback
  status.close(state)
  stale_confirm()
  assert(input_count == 1, "closed Status accepted a retained confirmation")
  assert(not vim.api.nvim_buf_is_valid(stale_dialog))
  local branch_count, branch_name = 0, nil
  local branch = dialogs.branch_name("feature/", function(value) branch_count, branch_name = branch_count + 1, value end)
  assert(vim.bo[branch].filetype == "ForgeBranchPrompt")
  assert(vim.api.nvim_win_get_config(0).height == 1)
  assert(buffer.line(branch, 1) == "feature/")
  buffer.set(branch, { "feature/parity" })
  keymap.feed("<CR>")
  assert(branch_count == 1 and branch_name == "feature/parity" and vim.api.nvim_get_current_win() == origin)
  dialogs.branch_name("feature/", function(value) assert(value == nil) branch_count = branch_count + 1 end)
  keymap.feed("q")
  assert(branch_count == 2)
  local replica = renderer.open("issues", {})
  vim.api.nvim_buf_set_name(replica.buffer, "ForgeStatus://issues-test")
  local snapshot = fixture.snapshot("issues", { context = { workspace = "fixture", branch = "main", recent = {}, issues = { 12 }, branch_prefix = "" } })
  assert(renderer.apply_snapshot(replica, snapshot).kind == "Applied")
  vim.api.nvim_win_set_buf(origin, replica.buffer)
  local saved, complete
  local editor = require("forge.views.status.issues_editor").attach(replica, function(text, callback) saved, complete = text, callback end)
  local _, issue_row = replica.root:position("status:context:issues")
  vim.api.nvim_win_set_cursor(origin, { issue_row + 1, 8 })
  editor.sync()
  assert(vim.bo[replica.buffer].modifiable)
  vim.api.nvim_buf_set_lines(replica.buffer, issue_row, issue_row + 1, false, { "Issues: #12 #34" })
  assert(replica.changedtick == vim.api.nvim_buf_get_changedtick(replica.buffer) and editor.dirty)
  renderer.present_context(replica, { pr = { state = "none" }, about = { state = "ready", text = "new background message" } })
  assert(replica.status == "Applied" and editor.dirty, "background About invalidated the Issues edit")
  snapshot.revision = 1
  assert(renderer.apply_snapshot(replica, snapshot).kind == "Applied")
  -- Replica blocks keep layout only; the buffer holds their text.
  local _, refreshed_issue_row = replica.root:position("status:context:issues")
  assert(buffer.line(replica.buffer, refreshed_issue_row + 1) == "Issues: #12 #34",
    "refresh overwrote the Issues draft")
  vim.cmd("write")
  assert(saved == "Issues: #12 #34")
  complete(false)
  assert(editor.dirty, "failed save discarded the Issues draft")
  vim.cmd("write")
  complete(true)
  assert(not editor.dirty and not vim.bo[replica.buffer].modified)
  vim.api.nvim_buf_set_lines(replica.buffer, issue_row, issue_row + 1, false, { "Issues: #56" })
  editor.save()
  vim.api.nvim_buf_set_lines(replica.buffer, issue_row, issue_row + 1, false, { "Issues: #78" })
  complete(true)
  assert(editor.dirty and editor.text == "Issues: #78", "save completion discarded a newer Issues edit")
  local before_invalid = buffer.lines(replica.buffer)
  vim.api.nvim_buf_set_lines(replica.buffer, issue_row, issue_row + 1, false, { "Issues: #78", "unwanted extra row" })
  wait_until(function() return not editor.repairing end, "multi-line Issues edit was not repaired")
  assert(vim.deep_equal(buffer.lines(replica.buffer), before_invalid), "multi-line edit corrupted Status layout")
  assert(replica.changedtick == vim.api.nvim_buf_get_changedtick(replica.buffer))
  editor.save()
  complete(true)
  snapshot.context.issues = { 12, 34 }
  snapshot.revision = 2
  assert(renderer.apply_snapshot(replica, snapshot).kind == "Applied")
  vim.api.nvim_win_set_cursor(origin, { 1, 0 })
  editor.sync()
  assert(not vim.bo[replica.buffer].modifiable, "non-Issues status rows became editable")
end

return T
