local test_path = debug.getinfo(1, "S").source:sub(2)
local config_path = vim.fs.normalize(vim.fn.fnamemodify("nvim", ":p"))

if vim.g.forge_markdown_layout_child then
  local replica = require("forge.buffer")
  local owner = replica.open("markdown-layout")
  vim.api.nvim_set_current_buf(owner.buffer)
  vim.bo[owner.buffer].filetype = "ForgeHarness"
  vim.wo.number, vim.wo.relativenumber = false, false
  vim.wo.foldcolumn = "0"
  vim.wo.signcolumn, vim.wo.winbar = "yes:1", ""
  -- Harness timeline views draw fold and content markers through the shared node status column.
  vim.wo.statuscolumn = "%{%v:lua.require'forge.nodes'.sign()%}"
  vim.wo.wrap, vim.wo.linebreak, vim.wo.breakindent = true, true, true
  vim.o.showtabline, vim.o.laststatus = 0, 0
  local lines = {
    "## First header intact",
    "",
    "\\[",
    "L_o(x,\\omega_o) = L_e(x,\\omega_o) + \\int_{\\Omega} f_r(x,\\omega_i,\\omega_o) L_i(x,\\omega_i) (n\\cdot\\omega_i) d\\omega_i",
    "\\]",
    "",
    "## How the idea developed",
    "",
    "- **Radiometry** preserves the full bullet text.",
    "- **Geometric optics** stays on the bullet row.",
    "",
    "## How a path tracer uses it",
    "",
    "$$ L_i(x,\\omega_i) = L_o(x',-\\omega_i). $$",
    "Inline $L_o$ remains rendered.",
    "",
    "> Quoted text remains readable.",
    "",
    "1. Ordered item retains its text.",
    "2. Another ordered item.",
    "",
    "```rust",
    "let sample = \"literal **code**\";",
    "```",
    "",
    "| Name | Value |",
    "| --- | --- |",
    "| Sample | Present |",
    "",
    "[Readable link](https://example.com) and `inline code`.",
    "LAYOUT-READY",
  }
  local source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2
  local padding = string.rep(" ", source_indent)
  for index, line in ipairs(lines) do lines[index] = padding .. line end
  local function heading(id, text)
    return { id = id, text = { text }, metadata = {
      target = {}, decoration = {}, editable_region = {},
      layout = { indent = 2, marker = { text = "▸", capture = "Normal" } },
      conceal = { { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = #"▸ " } },
        replacement = "", line = false, priority = 100 } },
    } }
  end
  assert(replica.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
    heading("prompt", "▸ what day is it?"),
    heading("summary", "▸ Thought for 3s"),
    { id = "answer", text = { "It is Thursday." }, metadata = {
      markdown = true, target = {}, decoration = {}, editable_region = {},
      layout = { indent = 2 },
    } },
    { id = "commentary", text = { padding .. "Commentary " .. string.rep("word ", 45) }, metadata = {
      markdown = true, target = {}, decoration = {}, editable_region = {},
      layout = { indent = vim.g.forge_markdown_layout_indent or 2,
        source_indent = source_indent,
        marker = { text = "○", capture = "Normal" } },
    } },
    { id = "response", text = lines, metadata = {
      markdown = true, target = {}, decoration = {}, editable_region = {},
      layout = { indent = vim.g.forge_markdown_layout_indent or 2, source_indent = source_indent },
    } },
  } }).kind == "Applied")
  vim.api.nvim_win_set_cursor(0, {1, 0})
  require("forge.render.harness.markdown").render(owner.buffer, vim.api.nvim_get_current_win(), {
    { first0 = 2, after0 = 3 }, { first0 = 3, after0 = 4, indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
    { first0 = 4, after0 = #lines + 4, indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
  })
  return
end


local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harness_markdown_layout"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    local config_path = ...
    local data = vim.fn.stdpath("data")
    vim.opt.runtimepath:prepend(config_path)
    vim.opt.runtimepath:append(data .. "/site")
    vim.opt.runtimepath:append(data .. "/lazy/render-markdown.nvim")
    vim.opt.runtimepath:append(data .. "/lazy/nvim-treesitter")
    -- A mock math converter answers with fixed text art, so the case checks how Forge lays out
    -- converter output rather than the installed converter.
    local converter = require("markdown_math.dependency").executable_path()
    require("render-markdown.lib.env").commands = function() return { converter } end
    local integral = table.concat({
      "                ⌠",
      "L (x,ω ) = L  + ⎮ f  L  (n⋅ω ) dω",
      " o    o     e   ⌡  r  i     i    i",
      "                Ω",
    }, "\n")
    local system = vim.system
    vim.system = function(command, options, on_exit)
      if command[1] ~= converter then return system(command, options, on_exit) end
      local output = options.stdin:find("\\int", 1, true) and integral or options.stdin:gsub("[_^{}\\]", "")
      vim.schedule(function() on_exit({ code = 0, stdout = output, stderr = "" }) end)
      return { wait = function() end, kill = function() end }
    end
    require("render-markdown").setup({})
  ]], { config_path })
  child.api.nvim_ui_attach(120, 40, { rgb = true })
  child.lua("local path, indent = ...; vim.g.forge_markdown_layout_child=true; vim.g.forge_markdown_layout_indent=indent; dofile(path)", {test_path, vim.g.forge_markdown_layout_indent or 2})
  local function capture()
    local lines = {}
    for _, cells in ipairs(child.get_screenshot().text) do lines[#lines + 1] = table.concat(cells) end
    return lines
  end
  local function verify_layout(width)
    child.api.nvim_ui_try_resize(width, 40)
    child.lua([[
    require("forge.render.harness.markdown").render(vim.api.nvim_get_current_buf(), vim.api.nvim_get_current_win(), {
    { first0 = 2, after0 = 3 }, { first0 = 3, after0 = 4, indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
    { first0 = 4, after0 = vim.api.nvim_buf_line_count(0), indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
    })
  ]])
    wait_until(function()
      local screen = table.concat(capture(), "\n")
      return screen:find("LAYOUT-READY", 1, true) and screen:find("⌠", 1, true)
    end, "Markdown terminal did not finish rendering", { timeout_ms = 4000,
      context = function() return table.concat(capture(), "\n") end })
    local screen_lines = capture()
    local screen = table.concat(screen_lines, "\n")
    -- Fold markers reflect expansion state; these headings own no folded body, so they display expanded.
    assert(screen_lines[1]:find("▾ what day is it?", 1, true) == 1, "prompt marker is indented:\n" .. screen)
    assert(screen_lines[2]:find("▾ Thought for 3s", 1, true) == 1, "summary marker is indented:\n" .. screen)
    assert(screen_lines[3]:find("  It is Thursday.", 1, true) == 1, "response lost its blank gutter:\n" .. screen)
    local indent = vim.g.forge_markdown_layout_indent or 2
    assert(screen_lines[4]:find("○ Commentary", 1, true) == indent - 1, "commentary marker/content offset differs:\n" .. screen)
    for _, line in ipairs(screen_lines) do
      if line:match("^%s*word") then
        assert(line:find("word", 1, true) == indent + 1, "wrapped commentary lost its content indent:\n" .. screen)
      end
    end
    for _, heading in ipairs({ "First header intact", "How the idea developed", "How a path tracer uses it" }) do
      assert(screen:find(heading, 1, true), "heading was clipped: " .. heading .. "\n" .. screen)
    end
    assert(not screen:find("$$", 1, true), "cursor row revealed display delimiters:\n" .. screen)
    assert(not screen:find("$L_o$", 1, true), "inline math remained literal:\n" .. screen)
    assert(screen:find("Radiometry preserves the full bullet text.", 1, true), "bullet text was split or clipped:\n" .. screen)
    for _, content in ipairs({ "Quoted text remains readable.", "Ordered item retains its text.",
      'let sample = "literal **code**";', "Sample", "Present", "Readable link", "inline code" }) do
      assert(screen:find(content, 1, true), "Markdown content missing: " .. content .. "\n" .. screen)
    end
    local integral = {}
    for row, line in ipairs(screen_lines) do
      for _, symbol in ipairs({ "⌠", "⎮", "⌡", "Ω" }) do
        local column = line:find(symbol, 1, true)
        if column then integral[symbol] = { row = row, column = vim.fn.strdisplaywidth(line:sub(1, column - 1)) } end
      end
    end
    assert(integral["⌠"] and integral["⎮"] and integral["⌡"] and integral["Ω"], "incomplete integral:\n" .. screen)
    assert(integral["⎮"].row == integral["⌠"].row + 1 and integral["⌡"].row == integral["⎮"].row + 1
      and integral["Ω"].row == integral["⌡"].row + 1, "extra physical rows split the integral:\n" .. screen)
    assert(integral["⌠"].column == integral["⎮"].column and integral["⌡"].column == integral["⎮"].column,
      "integral segments shifted horizontally:\n" .. screen)
  end
  do
    for _, width in ipairs({ 120, 80, 100, 120 }) do verify_layout(width) end
    local function expect_equation(row, source)
      child.lua("vim.api.nvim_win_set_cursor(0, {..., 0})", { row })
      wait_until(function()
        local screen_lines = capture()
        local screen = table.concat(screen_lines, "\n")
        -- A delimiter row is revealed when it occupies its own screen row. Markdown treats `\[` as a
        -- backslash escape, and nvim-treesitter's markdown_inline query conceals that backslash even
        -- on the cursor row, so the row may display as `[` rather than `\[`.
        local function delimiter_row(delimiter)
          for _, line in ipairs(screen_lines) do
            if line:match("^%s*\\?%" .. delimiter .. "%s*$") then return true end
          end
          return false
        end
        local opening = delimiter_row("[")
        local body = screen:find("\\int", 1, true) ~= nil
        local closing = delimiter_row("]")
        local rendered = screen:find("⌠", 1, true) ~= nil
        return opening == source and body == source and closing == source and rendered ~= source
      end, "equation reveal was partial at source row " .. row, { timeout_ms = 3000 })
    end
    -- The transcript is read, not edited, so the cursor row stays rendered: no source row of the
    -- equation reveals its delimiters or body when the cursor enters it.
    for _, row in ipairs({6, 7, 8, 9, 10, 9, 8, 7, 6}) do
      expect_equation(row, false)
    end
    expect_equation(10, false)
    local after_equation = child.lua([[
    local cursor = vim.api.nvim_win_get_cursor(0)
    local position = vim.fn.screenpos(0, cursor[1], cursor[2] + 1)
    return { row = position.row, column = position.col }
  ]])
    local after_screen = capture()
    assert(after_screen[after_equation.row]:match("^%s*$"), "line after equation was concealed into its rendering")
    -- Display-line motion still crosses the rendered equation without stalling or moving backward.
    child.lua("vim.api.nvim_win_set_cursor(0, {6, 0})")
    local previous = 6
    for _ = 1, 12 do
      local before = child.api.nvim_win_get_cursor(0)
      child.api.nvim_input("gj")
      wait_until(function()
        return not vim.deep_equal(child.api.nvim_win_get_cursor(0), before)
      end, "down motion did not advance", { timeout_ms = 3000 })
      local current = child.api.nvim_win_get_cursor(0)[1]
      assert(current > previous, "down motion revisited equation source")
      previous = current
      if current >= 11 then break end
    end
    assert(previous >= 11, "down motion did not leave the equation")
    local crossed = table.concat(capture(), "\n")
    assert(crossed:find("⌠", 1, true) and not crossed:find("\\int", 1, true), "crossing the equation revealed its source")
  end
end

T["live_tail_with_wrapped_table"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(121, 62, { rgb = true })
  child.lua([[
    local config_path = ...
    vim.opt.runtimepath:prepend(config_path)
    vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/lazy/render-markdown.nvim")
    vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/lazy/nvim-treesitter")
    require("render-markdown").setup({})
    vim.o.showtabline, vim.o.laststatus = 0, 0
    local layout = require("forge.views.harness.layout")
    local transcript, window, composer, composer_window = layout.open("table-tail")
    vim.g.tail_transcript, vim.g.tail_window, vim.g.tail_composer = transcript, window, composer_window
    layout.attach_scroll_boundary(transcript, window)
    vim.wo[window].winbar = "Harness"
    local lines = {}
    for row = 1, 80 do lines[#lines + 1] = "Earlier response " .. row end
    lines[#lines + 1] = "| Area | Contract |"
    lines[#lines + 1] = "| --- | --- |"
    for row = 1, 20 do
      lines[#lines + 1] = "| Area " .. row .. " | " .. string.rep("Preserve the current invariant. ", 10) .. "|"
    end
    lines[#lines + 1] = ""
    lines[#lines + 1] = "The response remains available while the next turn runs."
    lines[#lines + 1] = ""
    local client = require("forge.client")
    client.host_is_accepting = function() return true end
    local revision = 0
    HarnessTailFixture = require("forge.views.harness.presentation").open({
      session_id = "table-tail", transcript_buffer = transcript, transcript_window = window,
      composer_buffer = composer, composer_window = composer_window, read_only = true,
      is_alive = function() return true end, notice = function(message) error(message) end,
      request = function(params, receive)
        if params.operation ~= "open" and params.operation ~= "sync" then receive({}) return end
        revision = revision + 1
        local snapshot = { document = params.document, revision = revision, block = {
          { id = "history", text = lines, metadata = {
            target = {}, decoration = {}, editable_region = {}, markdown = true,
            layout = { indent = 2 },
          } },
          { id = "prompt", text = { "Continue the investigation." }, metadata = {
            target = {}, decoration = {}, editable_region = {},
            node = { id = "prompt", kind = "message", lifecycle = "settled", display = "full",
              default_display = "full", generation = 1, more = false, content_revision = 1,
              loaded_rows = 1, loaded_bytes = 27, order = 0 },
            layout = { indent = 2, marker = { text = ">", capture = "ForgeHarnessPrompt" } },
          } },
          { id = "exchange", text = { "Thinking " .. revision .. "s" }, metadata = {
            target = {}, decoration = {}, editable_region = {},
            node = { id = "exchange", kind = "exchange", lifecycle = "live", display = "full",
              default_display = "full", generation = 1, more = false, content_revision = revision,
              loaded_rows = 1, loaded_bytes = 12, order = 0 },
          } },
        } }
        local status = { document = params.document .. ":status", revision = revision, block = {
          { id = "working", text = { "Working " .. revision .. "s" }, metadata = {
            target = {}, decoration = {}, editable_region = {},
            status = { row = 0, animated = false, hint = "working" },
          } },
        } }
        vim.schedule(function()
          receive({ [params.operation == "open" and "transcript" or "snapshot"] = snapshot, status = status })
        end)
      end,
    }, function(owner, failure) assert(owner, failure) end)
  ]], { config_path })
  wait_until(function()
    return child.lua([[
      for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(vim.g.tail_transcript,
        vim.api.nvim_get_namespaces()["render-markdown.nvim"], 0, -1, { details = true })) do
        if mark[4].virt_lines then return true end
      end
      return false
    ]])
  end, "wrapped table did not render")
  for _ = 1, 3 do
    child.lua([[
      HarnessTailFixture.sync()
    ]])
    wait_until(function() return child.lua("return not HarnessTailFixture.syncing and not HarnessTailFixture.refresh_timer") end,
      "live transcript update did not settle")
    local geometry = child.lua([[
      vim.cmd("redraw")
      local window = vim.g.tail_window
      local last = vim.api.nvim_buf_line_count(vim.g.tail_transcript)
      return { tail = vim.fn.screenpos(window, last, 1).row,
        bottom = vim.api.nvim_win_get_position(window)[1] + vim.api.nvim_win_get_height(window),
        focused = vim.api.nvim_get_current_win(), composer = vim.g.tail_composer,
        view = vim.api.nvim_win_call(window, vim.fn.winsaveview) }
    ]])
    MiniTest.expect.equality(geometry.focused, geometry.composer)
    assert(geometry.view.topfill > 0, vim.inspect(geometry))
    assert(geometry.tail == geometry.bottom, vim.inspect(geometry))
  end
end

T["wrapped_table_stays_rendered_under_the_cursor"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(100, 40, { rgb = true })
  child.lua([[
    local config_path = ...
    vim.opt.runtimepath:prepend(config_path)
    vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/site")
    vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/lazy/render-markdown.nvim")
    vim.opt.runtimepath:append(vim.fn.stdpath("data") .. "/lazy/nvim-treesitter")
    require("render-markdown").setup({})
    vim.o.showtabline, vim.o.laststatus = 0, 0
    vim.wo.wrap, vim.wo.linebreak = true, true
    local lines = { "Prose before the table.", "", "| Area | Contract |", "| --- | --- |" }
    for row = 1, 4 do
      lines[#lines + 1] = "| Area " .. row .. " | " .. string.rep("Preserve the current invariant. ", 10) .. "|"
    end
    lines[#lines + 1] = ""
    lines[#lines + 1] = "Prose after the table."
    vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
    vim.bo.filetype = "ForgeHarness"
    require("forge.render.harness.markdown").render(vim.api.nvim_get_current_buf(), vim.api.nvim_get_current_win(), {
      { first0 = 0, after0 = #lines, id = "message", version = 1 },
    })
  ]], { config_path })
  wait_until(function()
    return child.lua([[
      for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(0,
        vim.api.nvim_get_namespaces()["render-markdown.nvim"], 0, -1, { details = true })) do
        if mark[4].virt_lines then return true end
      end
      return false
    ]])
  end, "wrapped table did not render")
  -- Each cursor move makes render-markdown decide again which marks to draw, so counting those
  -- decisions tells the test when the move has taken effect.
  child.lua([[
    local updater = require("render-markdown.core.ui").updater
    local display = updater.display
    vim.g.display_count = 0
    function updater:display()
      display(self)
      vim.g.display_count = vim.g.display_count + 1
    end
  ]])
  local function hidden_rows_with_cursor_on(row)
    local displayed = child.lua_get("vim.g.display_count")
    child.type_keys(row .. "G")
    wait_until(function() return child.lua_get("vim.g.display_count") > displayed end, "cursor move did not update Markdown")
    return child.lua([[
      local hidden = 0
      for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(0,
        vim.api.nvim_get_namespaces()["render-markdown.nvim"], 0, -1, { details = true })) do
        if mark[4].conceal_lines then hidden = hidden + 1 end
      end
      return hidden
    ]])
  end
  local hidden_rows = hidden_rows_with_cursor_on(10)
  MiniTest.expect.equality(hidden_rows, 4)
  -- A wrapped row is hidden and drawn as virtual lines. Revealing its source under the cursor
  -- would change the transcript's height with cursor moves and unsettle the scroll boundary.
  MiniTest.expect.equality(hidden_rows_with_cursor_on(6), hidden_rows)
  MiniTest.expect.equality(hidden_rows_with_cursor_on(8), hidden_rows)
end

return T
