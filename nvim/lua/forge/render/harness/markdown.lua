local M = {}

local failed = false
local language_registered = false
local layout_ranges = {}
local layout_handlers = false

--- Registers markdown Tree-sitter parser support for the Harness filetype.
---@return boolean registered True if registration succeeded.
local function ensure_language()
  if language_registered then return true end
  local ok, register_error = pcall(vim.treesitter.language.register, "markdown", "ForgeHarness")
  if ok then
    language_registered = true
    return true
  end
  if not failed then
    failed = true
    vim.notify(
      "Harness markdown language registration failed: " .. tostring(register_error),
      vim.log.levels.WARN,
      { title = "ForgeHarness" }
    )
  end
  return false
end

--- Tests whether a zero-based row index is enclosed by any range in the list.
---@param row integer Zero-based row index.
---@param range_list { first0: integer, after0: integer }[] Array of range boundaries.
---@return boolean in_range True if row falls inside any range.
local function row_in_range(row, range_list)
  for _, range in ipairs(range_list) do
    if row >= range.first0 and row < range.after0 then return true end
  end
  return false
end

--- Formats line ranges into Tree-sitter parser region bounding rectangles.
---@param range_list { first0: integer, after0: integer }[] Array of range boundaries.
---@return table[] regions Nested parser region coordinates.
local function parser_region_list(range_list)
  local region_list = {}
  for _, range in ipairs(range_list) do
    if range.after0 > range.first0 then
      local region = {}
      if (range.source_indent or 0) > 0 then
        for row = range.first0, range.after0 - 1 do
          region[#region + 1] = { row, range.source_indent, row + 1, 0 }
        end
      else
        region = { { range.first0, 0, range.after0, 0 } }
      end
      region_list[#region_list + 1] = region
    end
  end
  return region_list
end

--- Finds Markdown blocks in the adopted native transcript.
---@param session table Native document replica.
---@return { first0: integer, after0: integer }[] range_list Response ranges.
function M.ranges(session)
  local range_list = {}
  local first0 = 0
  for index = 0, session.sequence:count() - 1 do
    local entry = session.sequence:at(index).entry
    if entry.metadata.markdown then
      range_list[#range_list + 1] = { first0 = first0, after0 = first0 + entry.row_count,
        indent = entry.metadata.layout and entry.metadata.layout.indent or 2,
        source_indent = entry.metadata.layout and entry.metadata.layout.source_indent or 0 }
    end
    first0 = first0 + entry.row_count
  end
  return range_list
end

--- Removes render-markdown extmarks that fall outside designated markdown ranges.
---@param buf integer Target buffer number.
---@param range_list { first0: integer, after0: integer }[] Array of range boundaries.
local function prune_extmarks(buf, range_list)
  local ok, ui = pcall(require, "render-markdown.core.ui")
  if not ok or not ui.ns then return end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ui.ns, 0, -1, {})) do
    if not row_in_range(mark[2], range_list) then pcall(vim.api.nvim_buf_del_extmark, buf, ui.ns, mark[1]) end
  end
end

--- Clears markdown parser regions and rendered extmarks from a buffer.
---@param buf integer Target buffer number.
local function clear(buf)
  layout_ranges[buf] = nil
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
  if parser_ok then
    pcall(parser.set_included_regions, parser, {})
    if type(parser.invalidate) == "function" then pcall(parser.invalidate, parser, true) end
  end
  local ui_ok, ui = pcall(require, "render-markdown.core.ui")
  if ui_ok and ui.ns then pcall(vim.api.nvim_buf_clear_namespace, buf, ui.ns, 0, -1) end
end

--- Configures Tree-sitter regions and triggers markdown rendering for specified buffer ranges.
---@param buf integer Target buffer number.
---@param win integer? Target window handle.
---@param range_list { first0: integer, after0: integer }[] Array of zero-based line ranges.
function M.render(buf, win, range_list)
  if #range_list == 0 then
    clear(buf)
    return
  end
  if not ensure_language() then return end
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  local ok, render_markdown = pcall(require, "render-markdown")
  if not ok or type(render_markdown.render) ~= "function" then return end
  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
  if not parser_ok then return end
  pcall(parser.set_included_regions, parser, parser_region_list(range_list))
  if type(parser.invalidate) == "function" then pcall(parser.invalidate, parser, true) end
  local highlight_ok, highlight_error = pcall(vim.treesitter.start, buf, "markdown")
  if not highlight_ok then
    if not failed then
      failed = true
      vim.notify("Harness markdown highlighting failed: " .. tostring(highlight_error), vim.log.levels.WARN, { title = "ForgeHarness" })
    end
    return
  end
  local conceallevel = vim.api.nvim_get_option_value("conceallevel", { scope = "local", win = win })
  local concealcursor = vim.api.nvim_get_option_value("concealcursor", { scope = "local", win = win })
  local function handler(base)
    return { parse = function(context)
      local marks = base.parse(context)
      local ranges = layout_ranges[context.buf]
      if not ranges then return marks end
      marks = vim.deepcopy(marks)
      for _, mark in ipairs(marks) do
        for _, range in ipairs(ranges) do
          if mark.start_row >= range.first0 and mark.start_row < range.after0 then
            local padding = math.max(0, (range.indent or 2) - 2 - (range.source_indent or 0))
            if mark.opts.virt_text_win_col then
              mark.opts.virt_text_win_col = mark.opts.virt_text_win_col + math.max(0, (range.indent or 2) - 2)
            end
            if padding > 0 then
              for _, line in ipairs(mark.opts.virt_lines or {}) do
                table.insert(line, 1, { string.rep(" ", padding), "Normal" })
              end
            end
            break
          end
        end
      end
      return marks
    end }
  end
  layout_ranges[buf] = range_list
  if not layout_handlers then
    local state = require("render-markdown.state")
    for _, language in ipairs({ "markdown", "latex" }) do
      state.custom_handlers[language] = handler(state.custom_handlers[language]
        or require("render-markdown.handler." .. language))
    end
    vim.api.nvim_create_autocmd("BufWipeout", { callback = function(event) layout_ranges[event.buf] = nil end })
    layout_handlers = true
  end
  local render_ok, render_error = pcall(render_markdown.render, {
    buf = buf,
    win = win,
    config = {
      enabled = true,
      render_modes = true,
      debounce = 0,
      anti_conceal = { enabled = false },
      completions = { lsp = { enabled = false } },
      sign = { enabled = false },
      win_options = {
        conceallevel = { default = conceallevel, rendered = 3 },
        concealcursor = { default = concealcursor, rendered = "nvic" },
      },
      on = { render = function() prune_extmarks(buf, range_list) end },
    },
  })
  if render_ok then
    prune_extmarks(buf, range_list)
  elseif not failed then
    failed = true
    vim.notify("Harness markdown rendering failed: " .. tostring(render_error), vim.log.levels.WARN, { title = "ForgeHarness" })
  end
end

return M
