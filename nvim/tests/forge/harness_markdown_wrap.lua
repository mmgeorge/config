-- Verifies soft-wrapped Harness Markdown at a fixed terminal width: list continuations hang under
-- their item text, and a wrapped table that ends its response renders every body row.
local test_path = debug.getinfo(1, "S").source:sub(2)
local config_path = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(test_path)))

local long = "carrying a brass compass, a roll of vellum, and the stubborn belief that every lost trail could be found again"
local response_lines = {
  "# Desert Cartographer",
  "",
  "- TOP-ITEM crossed the salt flats at dawn, " .. long .. ".",
  "  - NESTED-ITEM followed the wind, " .. long .. ".",
  "    - DEEP-ITEM recorded the dunes, " .. long .. ".",
  "",
  "1. ORDERED-ITEM began each expedition, " .. long .. ".",
  "   - ORDERED-CHILD marked wells, " .. long .. ".",
  "",
  "| Place | What he noticed | What it suggested | How he recorded it |",
  "|---|---|---|---|",
  "| Salt flats | A pale crust stretched across the basin, cracked into broad plates that gleamed beneath the morning light. | The basin had once held water, although the surface offered little shelter today. | He drew the basin boundary and marked the buried channel. |",
  "| Night camp | A crescent of stones sheltered a patch of level sand from the strongest wind. | The site offered a safer place to rest. | He added the camp to the map. |",
  "",
  "| Map detail | Why it mattered |",
  "|---|---|",
  "| Date of observation | The dunes and springs could change, so a dated note helped travelers decide whether a route was still dependable. |",
  "| Direction of approach | A landmark was useful only if someone could find it from the route they were actually following. |",
  "| Confidence of the note | Distinguishing a confirmed spring from a possible water source helped readers make informed choices. |",
}

if vim.g.forge_markdown_wrap_child then
  vim.loader.enable(false)
  local layout = require("forge.views.harness.layout")
  local buffer = layout.create_transcript_buffer("markdown-wrap-" .. tostring(vim.g.forge_markdown_wrap_trailing))
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, buffer)
  vim.b[buffer].forge_native_document = true
  layout.configure_transcript_window(window)
  -- Harness presentation reserves a one-cell sign column for timeline markers.
  vim.wo[window].signcolumn, vim.wo[window].statuscolumn = "yes:1", "%s"
  vim.wo[window].conceallevel, vim.wo[window].concealcursor = 3, "nvic"
  vim.o.showtabline, vim.o.laststatus = 0, 0
  local lines = { "● Write a story." }
  vim.list_extend(lines, response_lines)
  -- The trailing variant appends the next transcript block, which lies outside the Markdown range.
  if vim.g.forge_markdown_wrap_trailing then lines[#lines + 1] = "● Next prompt" end
  vim.bo[buffer].modifiable = true
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.bo[buffer].modifiable = false
  -- The composer keeps focus, as it does while a response streams, so no transcript row is revealed.
  vim.cmd("belowright 3split")
  vim.api.nvim_win_set_buf(0, vim.api.nvim_create_buf(false, true))
  if vim.g.forge_markdown_wrap_follow then
    -- Follow the response to its end, as streaming does, so the final table meets the window bottom.
    vim.api.nvim_win_set_cursor(window, { #lines, 0 })
    vim.api.nvim_win_call(window, function() vim.cmd("normal! zb") end)
  end
  require("forge.render.harness.markdown").render(buffer, window, { { first0 = 1, after0 = 1 + #response_lines } })
  return
end

local function capture(job)
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

---Find the display column of a literal in screen text.
---@param line string
---@param literal string
---@return integer?
local function display_column(line, literal)
  local index = line:find(literal, 1, true)
  return index and vim.fn.strdisplaywidth(line:sub(1, index - 1)) or nil
end

---@param trailing boolean Whether another transcript row follows the response.
---@param follow boolean Whether the window shows the response end instead of the transcript start.
local function verify(trailing, follow)
  local job = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", vim.fs.joinpath(config_path, "init.lua") },
    { rpc = true })
  assert(job > 0, "could not start Markdown wrap terminal")
  local passed, failure = pcall(function()
    vim.rpcrequest(job, "nvim_ui_attach", 100, follow and 30 or 60, { rgb = true })
    vim.rpcrequest(job, "nvim_exec_lua", [[
      local path, trailing, follow = ...
      vim.g.forge_markdown_wrap_child = true
      vim.g.forge_markdown_wrap_trailing, vim.g.forge_markdown_wrap_follow = trailing, follow
      dofile(path)
    ]], { test_path, trailing, follow })
    local ready = vim.wait(10000, function()
      local screen = table.concat(capture(job), "\n")
      return screen:find("Map detail", 1, true) ~= nil and screen:find("Confidence", 1, true) ~= nil
    end, 20)
    local screen_lines = capture(job)
    local screen = table.concat(screen_lines, "\n")
    local context = (trailing and "with a trailing block" or "at buffer end")
      .. (follow and " following the response end" or "") .. ":\n" .. screen
    assert(ready, "Markdown did not render " .. context)

    -- Every wrapped continuation of a list item starts at the column of that item's first word.
    for _, label in ipairs(follow and {} or { "TOP-ITEM", "NESTED-ITEM", "DEEP-ITEM", "ORDERED-ITEM", "ORDERED-CHILD" }) do
      local first_row
      for row, line in ipairs(screen_lines) do
        if line:find(label, 1, true) then first_row = row break end
      end
      assert(first_row, label .. " is missing " .. context)
      local text_column = display_column(screen_lines[first_row], label)
      local continuation = screen_lines[first_row + 1]
      local continuation_column = display_column(continuation, continuation:match("%S+") or "")
      assert(continuation_column == text_column, ("%s continuation starts at column %s instead of %d %s")
        :format(label, tostring(continuation_column), text_column, context))
    end

    -- A wrapped table body replaces its source rows with bordered cells, including the final table.
    for _, cell in ipairs({ "Date of observation", "Direction of approach", "Confidence of the note" }) do
      local found
      for _, line in ipairs(screen_lines) do
        if line:find(cell, 1, true) then
          found = true
          assert(line:find("│ " .. cell, 1, true), "table cell kept raw source " .. cell .. " " .. context)
        end
      end
      assert(found, "table cell missing " .. cell .. " " .. context)
    end
    assert(not screen:find("| Date", 1, true), "final table left raw pipes " .. context)
    assert(screen:find("└", 1, true), "final table lost its bottom border " .. context)
  end)
  vim.fn.jobstop(job)
  assert(passed, failure)
end

for _, trailing in ipairs({ false, true }) do
  for _, follow in ipairs({ false, true }) do verify(trailing, follow) end
end
print("harness_markdown_wrap: list hanging indent and final wrapped table passed")
vim.cmd("qa!")
