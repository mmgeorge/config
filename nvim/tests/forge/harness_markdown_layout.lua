local test_path = debug.getinfo(1, "S").source:sub(2)
local config_path = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(test_path)))

if vim.g.forge_markdown_layout_child then
  vim.loader.enable(false)
  local replica = require("forge.buffer")
  local owner = replica.open("markdown-layout")
  vim.api.nvim_set_current_buf(owner.buffer)
  vim.bo[owner.buffer].filetype = "ForgeHarness"
  vim.wo.number, vim.wo.relativenumber = false, false
  vim.wo.foldcolumn = "0"
  vim.wo.signcolumn, vim.wo.statuscolumn, vim.wo.winbar = "yes:1", "%s", ""
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
        marker = { text = "↳", capture = "Normal" } },
    } },
    { id = "response", text = lines, metadata = {
      markdown = true, target = {}, decoration = {}, editable_region = {},
      layout = { indent = vim.g.forge_markdown_layout_indent or 2, source_indent = source_indent },
    } },
  } }).kind == "Applied")
  vim.api.nvim_win_set_cursor(0, {18, 5})
  require("forge.render.harness.markdown").render(owner.buffer, vim.api.nvim_get_current_win(), {
    { first0 = 2, after0 = 3 }, { first0 = 3, after0 = 4, indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
    { first0 = 4, after0 = #lines + 4, indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
  })
  return
end

local job = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", vim.fs.joinpath(config_path, "init.lua") }, { rpc = true })
assert(job > 0, "could not start Markdown layout terminal")
vim.rpcrequest(job, "nvim_ui_attach", 120, 40, { rgb = true })
vim.rpcrequest(job, "nvim_exec_lua", "local path, indent = ...; vim.g.forge_markdown_layout_child=true; vim.g.forge_markdown_layout_indent=indent; dofile(path)", {test_path, vim.g.forge_markdown_layout_indent or 2})
local function capture()
  return vim.rpcrequest(job, "nvim_exec_lua", [[
    vim.cmd('redraw')
    local lines = {}
    for row = 1, vim.o.lines do
      local cells = {}
      for column = 1, vim.o.columns do cells[#cells + 1] = vim.fn.screenstring(row, column) end
      lines[#lines + 1] = table.concat(cells)
    end
    return lines
  ]], {})
end
local function verify_layout(width)
  vim.rpcrequest(job, "nvim_ui_try_resize", width, 40)
  vim.rpcrequest(job, "nvim_exec_lua", [[
    require("forge.render.harness.markdown").render(vim.api.nvim_get_current_buf(), vim.api.nvim_get_current_win(), {
    { first0 = 2, after0 = 3 }, { first0 = 3, after0 = 4, indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
    { first0 = 4, after0 = vim.api.nvim_buf_line_count(0), indent = vim.g.forge_markdown_layout_indent or 2, source_indent = (vim.g.forge_markdown_layout_indent or 2) - 2 },
    })
  ]], {})
  local ready = vim.wait(10000, function()
    local screen = table.concat(capture(), "\n")
    return screen:find("LAYOUT-READY", 1, true) and screen:find("⌠", 1, true)
  end, 20)
  local screen_lines = capture()
  local screen = table.concat(screen_lines, "\n")
  assert(screen_lines[1]:find("▸ what day is it?", 1, true) == 1, "prompt marker is indented:\n" .. screen)
  assert(screen_lines[2]:find("▸ Thought for 3s", 1, true) == 1, "summary marker is indented:\n" .. screen)
  assert(screen_lines[3]:find("  It is Thursday.", 1, true) == 1, "response lost its blank gutter:\n" .. screen)
  local indent = vim.g.forge_markdown_layout_indent or 2
  assert(screen_lines[4]:find("↳ Commentary", 1, true) == indent - 1, "commentary marker/content offset differs:\n" .. screen)
  for _, line in ipairs(screen_lines) do
    if line:match("^%s*word") then
      assert(line:find("word", 1, true) == indent + 1, "wrapped commentary lost its content indent:\n" .. screen)
    end
  end
  assert(ready, "Markdown terminal did not finish rendering:\n" .. screen)
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
local passed, failure = pcall(function()
  for _, width in ipairs({ 120, 80, 100, 120 }) do verify_layout(width) end
end)
vim.fn.jobstop(job)
assert(passed, failure)
print("harness_markdown_layout: headings, bullets, math, and cursor concealment passed at 120/80/100/120 columns")
vim.cmd("qa!")
