local M = {}
local perf = require("forge.infra.perf")

local failed = false
local language_registered = false
local layout_ranges = {}
local layout_handlers = false
local cursor_attached = {}
local render_epoch = {}

--- Registers markdown Tree-sitter parser support for the Harness filetype.
---@return boolean registered True if registration succeeded.
local function ensure_language()
  if language_registered then return true end
  local ok, register_error = pcall(vim.treesitter.language.register, "markdown", { "ForgeHarness", "ForgePlan" })
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
---@param range_list { first0: integer, after0: integer, source_indent?: integer }[] Array of range boundaries.
---@return boolean in_range True if row falls inside any range.
local function row_in_range(row, range_list)
  for _, range in ipairs(range_list) do
    if row >= range.first0 and row < range.after0 then return true end
  end
  return false
end

--- Formats line ranges into Tree-sitter parser region bounding rectangles.
---@param range_list { first0: integer, after0: integer, source_indent?: integer }[] Array of range boundaries.
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

--- Anchors render-markdown replacement lines that would otherwise follow the final buffer line.
--- render-markdown conceals each wrapped table row and attaches its replacement lines above the next
--- visible row. When the concealed rows end the buffer, no such row exists: the anchor lands past the
--- final line, where a window scrolled to the end cannot show it, so a response ending in a wrapped
--- table loses its body rows and bottom border. Attach those lines below the last visible row instead.
local function install_final_replacement_anchor()
  local replacements = require("render-markdown.lib.replacements")
  local resolve = replacements.resolve
  replacements.resolve = function(visible, line_count)
    local groups = resolve(visible, line_count)
    local hidden = {}
    for _, mark in ipairs(visible) do
      if mark.opts.conceal_lines then
        local end_row = mark.opts.end_row or mark.start_row
        if end_row > mark.start_row and mark.opts.end_col == 0 then end_row = end_row - 1 end
        for row = mark.start_row, end_row do hidden[row] = true end
      end
    end
    for _, group in ipairs(groups) do
      local anchor = group.start_row
      if anchor >= line_count then
        anchor = line_count - 1
        while anchor >= 0 and hidden[anchor] do anchor = anchor - 1 end
        if anchor >= 0 then group.start_row, group.opts.virt_lines_above = anchor, false end
      end
    end
    return groups
  end
end

--- Resolves visible Markdown owners without traversing folded or off-screen history.
---@param session table Native document replica with an indexed block sequence.
---@param window integer Transcript window.
---@return table[] range_list Full message ranges with retained content versions.
function M.viewport(session, window)
  if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= session.buffer then return {} end
  return vim.api.nvim_win_call(window, function()
    local range_list = {}
    local row, after = vim.fn.line("w0") - 1, math.min(vim.fn.line("w$"), session.sequence:rows())
    while row < after do
        local node = session.sequence:locate(row)
        if not node then break end
        local _, first = session.sequence:position(node.id)
        local entry = node.entry
        if entry.metadata.markdown then
          local layout = entry.metadata.layout or {}
          range_list[#range_list + 1] = { first0 = first, after0 = first + entry.row_count,
            indent = layout.indent or 2, source_indent = layout.source_indent or 0,
            id = node.id, version = entry.version, generation = session.markdown_generation }
        end
        row = first + entry.row_count
    end
    return range_list
  end)
end

--- Sibling positions computed during the current render-markdown parse, keyed by node identity.
local sibling_position = {}

--- Makes render-markdown's list-item numbering linear in the list length.
--- `Node:sibling_count` walks every preceding sibling for each item it numbers, so one parse of a
--- long list costs O(n²) Tree-sitter steps on the editor thread, and a sibling chain that revisits
--- a node never terminates. Both blocked the event loop while the transcript re-rendered. This
--- override memoizes each walked position for the current parse and stops at a revisited node.
local function install_linear_sibling_count()
  local Node = require("render-markdown.lib.node")
  if Node.forge_linear_sibling_count then return end
  Node.forge_linear_sibling_count = true
  ---@param target string
  ---@param level? integer
  ---@return integer
  function Node:sibling_count(target, level)
    local scope = target .. ":" .. tostring(level)
    local walked, visited = {}, {}
    local base = 0
    local node = self.node ---@type TSNode?
    while node and node:type() == target
      and (level == nil or self:create(node):level(false) == level) do
      local key = scope .. ":" .. node:id()
      local known = sibling_position[key]
      if known then
        base = known
        break
      end
      if visited[key] then break end
      visited[key] = true
      walked[#walked + 1] = key
      node = node:prev_sibling()
    end
    for offset = #walked, 1, -1 do
      base = base + 1
      sibling_position[walked[offset]] = base
    end
    return base
  end
end

--- Removes render-markdown extmarks that fall outside designated markdown ranges.
---@param buf integer Target buffer number.
---@param range_list { first0: integer, after0: integer, source_indent?: integer }[] Array of range boundaries.
local function prune_extmarks(buf, range_list)
  local ok, ui = pcall(require, "render-markdown.core.ui")
  if not ok or not ui.ns then return end
  perf.trace("harness", "ui.markdown.prune", { buf = buf, count = #range_list }, function()
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ui.ns, 0, -1, {})) do
      if not row_in_range(mark[2], range_list) then pcall(vim.api.nvim_buf_del_extmark, buf, ui.ns, mark[1]) end
    end
  end)
end

--- Clears markdown parser regions and rendered extmarks from a buffer.
---@param buf integer Target buffer number.
function M.clear(buf)
  render_epoch[buf] = (render_epoch[buf] or 0) + 1
  layout_ranges[buf] = nil
  local math_handler = package.loaded["markdown_math.display_handler"]
  if math_handler then math_handler.invalidate(buf) end
  if not vim.api.nvim_buf_is_valid(buf) then return end
  vim.treesitter.stop(buf)
  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
  if parser_ok and parser then
    -- Neovim has no public setter for restricting a live buffer parser to transcript regions.
    ---@diagnostic disable-next-line: invisible
    pcall(parser.set_included_regions, parser, {})
  end
  local ui_ok, ui = pcall(require, "render-markdown.core.ui")
  if ui_ok and ui.ns then pcall(vim.api.nvim_buf_clear_namespace, buf, ui.ns, 0, -1) end
end

--- Configures Tree-sitter regions and triggers markdown rendering for specified buffer ranges.
---@param buf integer Target buffer number.
---@param win integer? Target window handle.
---@param range_list { first0: integer, after0: integer, source_indent?: integer }[] Array of zero-based line ranges.
function M.render(buf, win, range_list)
  if #range_list == 0 then
    M.clear(buf)
    return
  end
  if not ensure_language() then return end
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  local ok, render_markdown = pcall(require, "render-markdown")
  if not ok or type(render_markdown.render) ~= "function" then return end
  render_markdown.setup()
  local parser_ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
  if not parser_ok or not parser then return end
  perf.trace("harness", "ui.markdown.regions", { buf = buf, count = #range_list }, function()
    -- Preserve source coordinates while excluding non-Markdown and folded transcript content.
    ---@diagnostic disable-next-line: invisible
    pcall(parser.set_included_regions, parser, parser_region_list(range_list))
  end)
  local epoch = (render_epoch[buf] or 0) + 1
  render_epoch[buf] = epoch
  local function after_parse()
    if render_epoch[buf] ~= epoch or not vim.api.nvim_buf_is_valid(buf)
      or not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then return end
    local highlight_ok, highlight_error = pcall(perf.trace, "harness", "ui.markdown.highlight", { buf = buf }, function()
      vim.treesitter.start(buf, "markdown")
    end)
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
        return perf.trace("harness", "ui.markdown.parse", { buf = context.buf }, function()
          -- Node identities are only stable within one parse of the tree.
          sibling_position = {}
          local marks = base.parse(context)
          local ranges = layout_ranges[context.buf]
          if not ranges then return marks end
          local visible = {}
          for _, source_mark in ipairs(marks) do
            for _, range in ipairs(ranges) do
              -- Incremental trees can retain deleted list items at a shortened region's boundary.
              if source_mark.start_row >= range.first0 and source_mark.start_row < range.after0 then
                local mark = vim.deepcopy(source_mark)
                if mark.opts.end_row and mark.opts.end_row >= range.after0 then
                  mark.opts.end_row, mark.opts.end_col = range.after0, 0
                end
                local padding = math.max(0, (range.indent or 2) - 2 - (range.source_indent or 0))
                if mark.opts.virt_text_win_col then
                  mark.opts.virt_text_win_col = mark.opts.virt_text_win_col + math.max(0, (range.indent or 2) - 2)
                end
                if padding > 0 then
                  for _, line in ipairs(mark.opts.virt_lines or {}) do
                    table.insert(line, 1, { string.rep(" ", padding), "Normal" })
                  end
                end
                visible[#visible + 1] = mark
                break
              end
            end
          end
          return visible
        end)
      end }
    end
    layout_ranges[buf] = range_list
    if not cursor_attached[buf] then
      cursor_attached[buf] = true
      vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "ModeChanged", "BufEnter" }, {
        buffer = buf,
        callback = function(event)
          if not layout_ranges[buf] or vim.api.nvim_get_current_buf() ~= buf then return end
          require("render-markdown.core.ui").update(buf, vim.api.nvim_get_current_win(), event.event, false)
        end,
      })
      vim.api.nvim_create_autocmd("BufWipeout", {
        buffer = buf,
        once = true,
        callback = function() cursor_attached[buf] = nil end,
      })
    end
    if not layout_handlers then
      install_final_replacement_anchor()
      install_linear_sibling_count()
      local state = require("render-markdown.state")
      -- A handler the editor configured wins; otherwise `markdown_math` converts math when the
      -- editor provides it, and render-markdown's built-in handler renders it unconverted.
      local math_handler = require("forge.integrations.markdown_math").module("display_handler")
      local math_handler_by_language = math_handler and { markdown = math_handler, latex = math_handler.latex } or {}
      for _, language in ipairs({ "markdown", "latex" }) do
        state.custom_handlers[language] = handler(state.custom_handlers[language]
          or math_handler_by_language[language]
          or require("render-markdown.handler." .. language))
      end
      vim.api.nvim_create_autocmd("BufWipeout", { callback = function(event) layout_ranges[event.buf] = nil end })
      layout_handlers = true
    end
    local render_ok, render_error = pcall(perf.trace, "harness", "ui.markdown.render", { buf = buf }, function()
      return render_markdown.render({
        buf = buf,
        win = win,
        config = {
          enabled = true,
          render_modes = true,
          debounce = 0,
          anti_conceal = { enabled = true, above = 0, below = 0 },
          completions = { lsp = { enabled = false } },
          sign = { enabled = false },
          win_options = {
            conceallevel = { default = conceallevel, rendered = 3 },
            concealcursor = { default = concealcursor, rendered = "nvic" },
          },
          on = { render = function() prune_extmarks(buf, layout_ranges[buf] or {}) end },
        },
      })
    end)
    if render_ok then
      prune_extmarks(buf, range_list)
    elseif not failed then
      failed = true
      vim.notify("Harness markdown rendering failed: " .. tostring(render_error), vim.log.levels.WARN, { title = "ForgeHarness" })
    end
  end
  parser:parse(true, function(parse_error)
    if parse_error then
      if not failed then
        failed = true
        vim.notify("Harness markdown parsing failed: " .. tostring(parse_error), vim.log.levels.WARN, { title = "ForgeHarness" })
      end
      return
    end
    after_parse()
  end)
end

return M
