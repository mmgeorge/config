--- Parses hunk bodies and computes change regions, render items, context scopes, and
--- boundary metadata for the diff renderer. Pure model: every function takes explicit
--- inputs and returns data, holding no buffer or render state.
---@class DiffReviewHunkModelModule
local M = {}

local diff_render = require("diff_review.render.diff_render")
-- syntax_engine edge kept lazy to avoid a load-time cycle.
local function syntax_engine() return require("diff_review.render.syntax_engine") end
local diff_parse = require("diff_review.render.diff_parse")

-- Seam to the init-owned hunk helpers shared with the model.
local parse_hunk_body = diff_parse.parse_hunk_body
local function hunk_add_gutter(...) return diff_render.hunk_add_gutter(...) end
local function treesitter_line_segments(...) return syntax_engine().treesitter_line_segments(...) end
local function hunk_context_scope_key(...) return syntax_engine().hunk_context_scope_key(...) end
local function same_hunk_context_scope(...) return syntax_engine().same_hunk_context_scope(...) end
local hunk_first_changed_current_line = diff_parse.hunk_first_changed_current_line
local function hunk_line_visible_in_context_scope(...) return syntax_engine().hunk_line_visible_in_context_scope(...) end

---@param context DiffReviewHunkTreeSitterContext|string?
---@return DiffReviewHunkBoundaryContext?
function M.context_ancestor_boundary(context)
  if type(context) ~= "table" or type(context.ancestor_boundaries) ~= "table" then return nil end
  return context.ancestor_boundaries[#context.ancestor_boundaries]
end

---@param context DiffReviewHunkTreeSitterContext|string?
---@return string?
function M.context_ancestor_key(context)
  local boundary = M.context_ancestor_boundary(context)
  return boundary and boundary.key or nil
end

---@param hunk DiffReviewParsedHunk
---@return fun(parsed_line: DiffReviewParsedHunkLine): boolean
function M.render_line_filter(hunk)
  local first_changed_position = nil
  local last_changed_position = nil
  for _, parsed_line in ipairs(hunk.lines or {}) do
    if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
      first_changed_position = math.min(first_changed_position or parsed_line.position, parsed_line.position)
      last_changed_position = math.max(last_changed_position or parsed_line.position, parsed_line.position)
    end
  end
  if not first_changed_position or not last_changed_position then
    return function() return true end
  end
  return function(parsed_line)
    if parsed_line.prefix ~= " " then return true end
    return parsed_line.position >= first_changed_position and parsed_line.position <= last_changed_position
  end
end

---@param parsed_lines DiffReviewParsedHunkLine[]
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return table<string, boolean>
function M.visible_parsed_source_lines(parsed_lines, include_line)
  local visible = {}
  for _, parsed_line in ipairs(parsed_lines or {}) do
    if (not include_line or include_line(parsed_line)) and (parsed_line.prefix == " " or parsed_line.prefix == "+" or parsed_line.prefix == "-") then
      visible[parsed_line.code] = true
    end
  end
  return visible
end

---@param hunk DiffReviewParsedHunk
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return integer
function M.after_current_line(hunk, include_line)
  local last_new_line = nil
  local saw_removed_line = false
  for _, parsed_line in ipairs(hunk.lines) do
    if not include_line or include_line(parsed_line) then
      if parsed_line.new_line then last_new_line = parsed_line.new_line end
      if parsed_line.prefix == "-" then saw_removed_line = true end
    end
  end
  if last_new_line then return last_new_line + 1 end
  if saw_removed_line then return hunk.new_start end
  return hunk.new_start
end

---@param hunk DiffReviewParsedHunk
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return table<integer, boolean>
function M.current_line_set(hunk, include_line)
  local lines = {}
  for _, parsed_line in ipairs(hunk.lines) do
    if parsed_line.new_line and (not include_line or include_line(parsed_line)) then lines[parsed_line.new_line] = true end
  end
  return lines
end

---@param item table
---@return DiffReviewParsedHunkLine
function M.render_item_line(item)
  return item.line or item.display_line
end

---@param item table
---@return DiffReviewParsedHunkLine[]
function M.render_item_backing_lines(item)
  if item.kind == "replacement" then return item.diff_lines or { item.display_line } end
  return { item.line }
end

---@param item table
---@return boolean
function M.render_item_changed(item)
  if item.kind == "replacement" then return true end
  local parsed_line = M.render_item_line(item)
  return parsed_line.prefix == "+" or parsed_line.prefix == "-"
end

---@param hunk DiffReviewParsedHunk
---@return table<integer, integer>
function M.current_line_by_position(hunk)
  local by_position = {}
  local current_line = hunk.new_start
  for _, parsed_line in ipairs(hunk.lines or {}) do
    if parsed_line.prefix == " " then
      if parsed_line.position and parsed_line.new_line then by_position[parsed_line.position] = parsed_line.new_line end
      if parsed_line.new_line then current_line = parsed_line.new_line + 1 end
    elseif parsed_line.prefix == "+" then
      if parsed_line.position and parsed_line.new_line then by_position[parsed_line.position] = parsed_line.new_line end
      if parsed_line.new_line then current_line = parsed_line.new_line + 1 end
    elseif parsed_line.prefix == "-" then
      if parsed_line.position then by_position[parsed_line.position] = current_line end
    end
  end
  return by_position
end

---@param parsed_line DiffReviewParsedHunkLine?
---@param line_by_position table<integer, integer>
---@return integer?
function M.parsed_line_current_line(parsed_line, line_by_position)
  if not parsed_line then return nil end
  return parsed_line.new_line or (parsed_line.position and line_by_position[parsed_line.position]) or parsed_line.old_line
end

---@param item table
---@param line_by_position table<integer, integer>
---@return integer?
function M.render_item_context_line(item, line_by_position)
  if item.kind == "replacement" then
    for _, parsed_line in ipairs(item.new_lines or {}) do
      if parsed_line.new_line then return parsed_line.new_line end
    end
  end
  return M.parsed_line_current_line(M.render_item_line(item), line_by_position)
end

---@param item table
---@param line_by_position table<integer, integer>
---@return integer?
---@return integer?
function M.render_item_changed_line_range(item, line_by_position)
  local first_line = nil
  local last_line = nil
  for _, parsed_line in ipairs(M.render_item_backing_lines(item)) do
    if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
      local current_line = M.parsed_line_current_line(parsed_line, line_by_position)
      if current_line then
        first_line = math.min(first_line or current_line, current_line)
        last_line = math.max(last_line or current_line, current_line)
      end
    end
  end
  return first_line, last_line
end

---@param context DiffReviewHunkTreeSitterContext|string?
---@param line integer?
---@return boolean
function M.context_contains_line(context, line)
  if type(context) ~= "table" or not line then return true end
  return line >= context.start_row + 1 and line <= context.end_row + 1
end

---@param hunk DiffReviewParsedHunk
---@param line_by_position? table<integer, integer>
---@return table<integer, boolean>
function M.changed_current_line_set(hunk, line_by_position)
  line_by_position = line_by_position or M.current_line_by_position(hunk)
  local lines = {}
  for _, parsed_line in ipairs(hunk.lines or {}) do
    if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
      local current_line = M.parsed_line_current_line(parsed_line, line_by_position)
      if current_line then lines[current_line] = true end
    end
  end
  return lines
end

---@param item table
---@return integer added
---@return integer removed
function M.render_item_stats(item)
  local added = 0
  local removed = 0
  for _, parsed_line in ipairs(M.render_item_backing_lines(item)) do
    if parsed_line.prefix == "+" then
      added = added + 1
    elseif parsed_line.prefix == "-" then
      removed = removed + 1
    end
  end
  return added, removed
end

---@param render_items table[]
---@param line_by_position table<integer, integer>
---@param context_for_line fun(line: integer): DiffReviewHunkTreeSitterContext|string?
---@return DiffReviewHunkChangeRegion[]
function M.change_regions(render_items, line_by_position, context_for_line)
  local regions = {}
  local current_region = nil
  for item_index, item in ipairs(render_items or {}) do
    if M.render_item_changed(item) then
      local context_line = M.render_item_context_line(item, line_by_position)
      local item_context = nil
      if type(current_region and current_region.context) == "table"
        and M.context_contains_line(current_region.context, context_line) then
        item_context = current_region.context
      else
        item_context = context_line and context_for_line(context_line) or nil
      end
      local context_key = hunk_context_scope_key(item_context)
      local changed_line, last_changed_line = M.render_item_changed_line_range(item, line_by_position)
      local added, removed = M.render_item_stats(item)
      local can_merge = current_region ~= nil
        and (
          (
            context_key ~= nil
            and current_region.context_key == context_key
            and M.context_contains_line(current_region.context, context_line)
          )
          or (
            context_key == nil
            and current_region.context_key == nil
            and type(current_region.context) ~= "table"
            and type(item_context) ~= "table"
          )
        )
      if not can_merge then
        current_region = {
          first_item = item_index,
          last_item = item_index,
          context_line = context_line,
          context = item_context,
          context_key = context_key,
          changed_line = changed_line or context_line,
          after_line = last_changed_line and (last_changed_line + 1) or context_line,
          added = added,
          removed = removed,
        }
        regions[#regions + 1] = current_region
      else
        current_region.last_item = item_index
        if changed_line then current_region.changed_line = math.min(current_region.changed_line or changed_line, changed_line) end
        if last_changed_line then current_region.after_line = math.max(current_region.after_line or last_changed_line + 1, last_changed_line + 1) end
        current_region.added = (current_region.added or 0) + added
        current_region.removed = (current_region.removed or 0) + removed
      end
    end
  end
  if #regions == 0 and #render_items > 0 then
    regions[1] = {
      first_item = 1,
      last_item = #render_items,
      added = 0,
      removed = 0,
    }
  end
  return regions
end

---@param region DiffReviewHunkChangeRegion
---@param fallback_hunk? DiffReviewParsedHunk
---@return table[]
function M.virtual_header_parts(region, fallback_hunk)
  local added = region.added
  local removed = region.removed
  if added == nil and fallback_hunk then added = fallback_hunk.added end
  if removed == nil and fallback_hunk then removed = fallback_hunk.removed end
  return {
    { "@@ ", "DiffReviewHunkHeader" },
    { ("+%d"):format(added or 0), "DiffReviewAddRange" },
    { " ", "DiffReviewHunkHeader" },
    { ("-%d"):format(removed or 0), "DiffReviewDeleteRange" },
  }
end

---@param parsed_line DiffReviewParsedHunkLine
---@param previous_visible_changed boolean
---@param context DiffReviewHunkTreeSitterContext|string?
---@param visible_in_hunk? boolean
---@return boolean
function M.hidden_closing_boundary_after_change(parsed_line, previous_visible_changed, context, visible_in_hunk)
  if not previous_visible_changed then return false end
  if parsed_line.prefix ~= " " or type(context) ~= "table" then return false end
  if visible_in_hunk and hunk_line_visible_in_context_scope(parsed_line, context) then return false end
  if not parsed_line.new_line then return false end
  return parsed_line.code:match("^%s*[})%]]+[,;]?%s*$") ~= nil
end

---@param parsed_line DiffReviewParsedHunkLine
---@param file string
---@return table
function M.diff_line_meta(parsed_line, file)
  return {
    side = parsed_line.new_line and "right" or "left",
    file = file,
    line = parsed_line.new_line or parsed_line.old_line,
    position = parsed_line.position,
    code = parsed_line.code,
    prefix = parsed_line.prefix,
  }
end

---@param parsed_lines DiffReviewParsedHunkLine[]?
---@param file string
---@return table[]
function M.diff_lines_meta(parsed_lines, file)
  local diff_lines = {}
  for _, parsed_line in ipairs(parsed_lines or {}) do
    diff_lines[#diff_lines + 1] = M.diff_line_meta(parsed_line, file)
  end
  return diff_lines
end

---@param replacement table
---@param gutter DiffReviewGutterSpec
---@param file string
---@param syntax? DiffReviewTreeSitterSyntax
---@param syntax_row? integer
---@return table
function M.replacement_row(replacement, gutter, file, syntax, syntax_row)
  local display_line = replacement.display_line
  local backing_lines = replacement.diff_lines or { display_line }
  local sign_hl = "DiffReviewModifyLineNr"
  local line_hl = "DiffReviewModifyBg"
  local row = {
    diff_review_bg_hl = line_hl,
    diff_review_inline_highlights = replacement.inline_spans or {},
  }
  row[#row + 1] = {
    "",
    nil,
    meta = {
      diff = M.diff_line_meta(display_line, file),
      diff_lines = M.diff_lines_meta(backing_lines, file),
    },
  }
  local old_line = replacement.old_lines and replacement.old_lines[1] and replacement.old_lines[1].old_line or display_line.old_line
  local new_line = replacement.new_lines and replacement.new_lines[1] and replacement.new_lines[1].new_line or display_line.new_line
  hunk_add_gutter(row, gutter, old_line, new_line, "~", sign_hl, line_hl, sign_hl)
  local segments = nil
  if syntax and syntax_row then
    segments = treesitter_line_segments(syntax.buf, syntax.tree, syntax.highlight_query, syntax_row, display_line.code)
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { display_line.code }
  end
  return row
end

---@param line_number integer
---@param text string
---@param gutter DiffReviewGutterSpec
---@param file? string
---@param syntax? DiffReviewTreeSitterSyntax
---@param old_line? integer
---@param new_line? integer
---@return table
function M.context_padding_row(line_number, text, gutter, file, syntax, old_line, new_line)
  local row = { diff_review_context_padding = true }
  old_line = old_line or line_number
  new_line = new_line or line_number
  if file then
    row[#row + 1] = {
      "",
      nil,
      meta = {
        diff = M.diff_line_meta({
          prefix = " ",
          old_line = old_line,
          new_line = new_line,
          code = text,
        }, file),
      },
    }
  end
  hunk_add_gutter(row, gutter, old_line, new_line, " ", nil)
  local segments = nil
  if syntax then
    segments = treesitter_line_segments(syntax.buf, syntax.tree, syntax.highlight_query, line_number - 1, text)
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { text }
  end
  return row
end

---@param text string?
---@return boolean
function M.context_padding_line_is_useful(text)
  if type(text) ~= "string" then return false end
  if text:match("^%s*$") then return false end
  if text:match("^%s*[})%]]+[,;]?%s*$") then return false end
  return true
end

---@param text string?
---@return boolean
function M.context_padding_line_starts_scope(text)
  if type(text) ~= "string" then return false end
  return text:match("^%s*#%[") ~= nil
    or text:match("^%s*pub%s+struct%s+") ~= nil
    or text:match("^%s*struct%s+") ~= nil
    or text:match("^%s*pub%s+enum%s+") ~= nil
    or text:match("^%s*enum%s+") ~= nil
    or text:match("^%s*pub%s+trait%s+") ~= nil
    or text:match("^%s*trait%s+") ~= nil
    or text:match("^%s*impl%s+") ~= nil
    or text:match("^%s*pub%s+fn%s+") ~= nil
    or text:match("^%s*fn%s+") ~= nil
    or text:match("^%s*pub%s+mod%s+") ~= nil
    or text:match("^%s*mod%s+") ~= nil
    or text:match("^%s*use%s+") ~= nil
end

---@param source_lines string[]?
---@param hunk DiffReviewParsedHunk
---@param context DiffReviewHunkTreeSitterContext|string?
---@param side "before"|"after"
---@param occupied_lines? table<integer, boolean>
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@param bounds? { changed_line?: integer, after_line?: integer }
---@return DiffReviewHunkContextPaddingLine[]
function M.context_padding_lines(source_lines, hunk, context, side, occupied_lines, include_line, bounds)
  if type(source_lines) ~= "table" or #source_lines == 0 then return {} end
  occupied_lines = occupied_lines or M.current_line_set(hunk)
  bounds = bounds or {}
  local padding_lines = {}
  local padding_limit = M.context_padding_limit()
  if type(context) == "table" then
    local changed_line = bounds.changed_line or hunk_first_changed_current_line(hunk)
    local after_line = bounds.after_line or M.after_current_line(hunk, include_line)
    local path_rows = side == "before" and context.path_start_rows or context.path_end_rows
    local sibling_rows = side == "before" and context.sibling_before_rows or context.sibling_after_rows
    local seen_candidates = {}
    local function collect_candidates(rows)
      local candidates = {}
      for _, line_number in ipairs(rows or {}) do
        local is_scope_boundary = line_number == (context.start_row + 1) or line_number == (context.end_row + 1)
        local eligible_before = side == "before" and line_number < changed_line
        local eligible_after = side == "after" and line_number >= after_line
        if not is_scope_boundary
          and not occupied_lines[line_number]
          and not seen_candidates[line_number]
          and (eligible_before or eligible_after) then
          seen_candidates[line_number] = true
          candidates[#candidates + 1] = line_number
        end
      end
      table.sort(candidates, function(left, right)
        if side == "before" then return left > right end
        return left < right
      end)
      return candidates
    end
    local candidates = {}
    for _, line_number in ipairs(collect_candidates(path_rows)) do
      if #candidates >= padding_limit then break end
      candidates[#candidates + 1] = line_number
    end
    for _, line_number in ipairs(collect_candidates(sibling_rows)) do
      if #candidates >= padding_limit then break end
      candidates[#candidates + 1] = line_number
    end
    if #candidates == 0 then
      local scope_start = context.start_row + 1
      local scope_end = context.end_row + 1
      if side == "before" then
        local first_line = math.max(scope_start + 1, changed_line - padding_limit)
        for line_number = changed_line - 1, first_line, -1 do
          local is_scope_boundary = line_number == scope_start or line_number == scope_end
          if is_scope_boundary or not M.context_padding_line_is_useful(source_lines[line_number]) then break end
          if not occupied_lines[line_number] and not seen_candidates[line_number] then
            seen_candidates[line_number] = true
            candidates[#candidates + 1] = line_number
          end
        end
      else
        local last_line = math.min(scope_end - 1, after_line + padding_limit - 1)
        for line_number = after_line, last_line do
          local is_scope_boundary = line_number == scope_start or line_number == scope_end
          if is_scope_boundary or not M.context_padding_line_is_useful(source_lines[line_number]) then break end
          if not occupied_lines[line_number] and not seen_candidates[line_number] then
            seen_candidates[line_number] = true
            candidates[#candidates + 1] = line_number
          end
        end
      end
    end
    table.sort(candidates)
    for _, line_number in ipairs(candidates) do
      padding_lines[#padding_lines + 1] = {
        line_number = line_number,
        text = source_lines[line_number] or "",
      }
    end
    return padding_lines
  end

  local first_line = nil
  local last_line = nil
  if side == "before" then
    local changed_line = bounds.changed_line or hunk_first_changed_current_line(hunk)
    first_line = math.max(1, changed_line - padding_limit)
    last_line = changed_line - 1
  else
    first_line = bounds.after_line or M.after_current_line(hunk, include_line)
    last_line = math.min(#source_lines, first_line + padding_limit - 1)
  end

  for line_number = first_line, last_line do
    if side == "after"
      and line_number > first_line
      and M.context_padding_line_starts_scope(source_lines[line_number]) then
      break
    end
    if not occupied_lines[line_number] then
      padding_lines[#padding_lines + 1] = {
        line_number = line_number,
        text = source_lines[line_number] or "",
      }
    end
  end
  return padding_lines
end

---@return integer
function M.context_padding_limit()
  return 3
end

---@param hunks DiffReviewParsedHunk[]
---@param new_line integer?
---@return integer?
function M.old_line_for_new_line(hunks, new_line)
  if not new_line then return nil end
  local delta = 0
  for _, hunk in ipairs(hunks or {}) do
    local old_start = hunk.old_start or 0
    local old_count = hunk.old_count or 0
    local new_start = hunk.new_start or 0
    local new_count = hunk.new_count or 0
    if new_line < new_start then return new_line + delta end
    if old_count > 0 and new_count == 0 then
      if new_line == new_start then return math.max(1, old_start - 1) end
      delta = old_start + old_count - new_start - 1
    else
      delta = delta + old_count - new_count
    end
    if new_count > 0 and new_line <= new_start + new_count - 1 then
      local parsed_hunk = hunk.lines and #hunk.lines > 0 and hunk or parse_hunk_body(vim.deepcopy(hunk))
      for _, parsed_line in ipairs(parsed_hunk.lines or {}) do
        if parsed_line.new_line == new_line and parsed_line.old_line then return parsed_line.old_line end
      end
      if old_count == new_count then return old_start + (new_line - new_start) end
      return nil
    end
  end
  return new_line + delta
end

---@param padding_lines DiffReviewHunkContextPaddingLine[]
---@param block DiffReviewParsedBlock
---@return DiffReviewHunkContextPaddingLine[]
function M.annotate_padding_line_numbers(padding_lines, block)
  for _, padding_line in ipairs(padding_lines or {}) do
    padding_line.new_line = padding_line.line_number
    padding_line.old_line = M.old_line_for_new_line(block and block.hunks or {}, padding_line.line_number)
  end
  return padding_lines
end

---@param region DiffReviewHunkChangeRegion
---@param render_items table[]
---@param include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean
---@return table<string, boolean>
function M.region_visible_source_lines(region, render_items, include_render_line)
  local visible = {}
  for item_index = region.first_item, region.last_item do
    local item = render_items[item_index]
    if item then
      for _, parsed_line in ipairs(M.render_item_backing_lines(item)) do
        if (not include_render_line or include_render_line(parsed_line))
          and (parsed_line.prefix == " " or parsed_line.prefix == "+" or parsed_line.prefix == "-") then
          visible[parsed_line.code] = true
        end
      end
      if item.kind == "replacement" and item.display_line then visible[item.display_line.code] = true end
    end
  end
  return visible
end

---@param region DiffReviewHunkChangeRegion
---@param render_items table[]
---@param line_by_position table<integer, integer>
---@return table<integer, boolean>
function M.region_changed_current_lines(region, render_items, line_by_position)
  local changed_lines = {}
  for item_index = region.first_item, region.last_item do
    local item = render_items[item_index]
    if item then
      for _, parsed_line in ipairs(M.render_item_backing_lines(item)) do
        if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
          local current_line = M.parsed_line_current_line(parsed_line, line_by_position)
          if current_line then changed_lines[current_line] = true end
        end
      end
    end
  end
  return changed_lines
end

---@param region DiffReviewHunkChangeRegion
---@param before_padding_lines DiffReviewHunkContextPaddingLine[]
---@param after_padding_lines DiffReviewHunkContextPaddingLine[]
---@return integer? display_start
---@return integer? display_end
function M.region_display_window(region, before_padding_lines, after_padding_lines)
  local display_start = region.changed_line or region.context_line
  local display_end = region.after_line and (region.after_line - 1) or display_start
  for _, padding_line in ipairs(before_padding_lines or {}) do
    display_start = math.min(display_start or padding_line.line_number, padding_line.line_number)
    display_end = math.max(display_end or padding_line.line_number, padding_line.line_number)
  end
  for _, padding_line in ipairs(after_padding_lines or {}) do
    display_start = math.min(display_start or padding_line.line_number, padding_line.line_number)
    display_end = math.max(display_end or padding_line.line_number, padding_line.line_number)
  end
  return display_start, display_end
end

---@param previous_plan DiffReviewHunkRenderPlan
---@param next_plan DiffReviewHunkRenderPlan
---@return boolean
function M.render_plans_should_merge(previous_plan, next_plan)
  if previous_plan.block.file ~= next_plan.block.file then return false end
  if previous_plan.display_end and next_plan.display_start and next_plan.display_start <= previous_plan.display_end + 1 then
    return true
  end
  if not M.contexts_related(previous_plan.region.context, next_plan.region.context) then return false end
  local gap = M.visible_plan_gap(previous_plan, next_plan)
  if gap and gap > 0 and type(previous_plan.source_lines) ~= "table" then return false end
  return gap ~= nil and gap >= 0 and gap <= M.context_bridge_limit()
end

---@param left_old integer?
---@param left_new integer?
---@param right_old integer?
---@param right_new integer?
---@return boolean
function M.render_coords_adjacent(left_old, left_new, right_old, right_new)
  if left_old and right_old and right_old == left_old + 1 then return true end
  if left_new and right_new and right_new == left_new + 1 then return true end
  return false
end

---@param left DiffReviewHunkTreeSitterContext|string?
---@param right DiffReviewHunkTreeSitterContext|string?
---@return boolean
function M.contexts_related(left, right)
  if same_hunk_context_scope(left, right) then return true end
  if type(left) ~= "table" or type(right) ~= "table" then return false end
  local left_start = left.start_row + 1
  local left_end = left.end_row + 1
  local right_start = right.start_row + 1
  local right_end = right.end_row + 1
  return (right_start >= left_start and right_end <= left_end)
    or (left_start >= right_start and left_end <= right_end)
end

---@return integer
function M.context_bridge_limit()
  return M.context_padding_limit() * 2
end

---@param left_old integer?
---@param left_new integer?
---@param right_old integer?
---@param right_new integer?
---@return integer?
function M.render_coord_gap(left_old, left_new, right_old, right_new)
  local gap = nil
  local function include_gap(left, right)
    if not (left and right) then return end
    local candidate = right - left - 1
    if gap == nil or candidate > gap then gap = candidate end
  end
  include_gap(left_new, right_new)
  include_gap(left_old, right_old)
  return gap
end

---@param left_new integer?
---@param right_new integer?
---@return integer?
function M.single_hidden_new_line(left_new, right_new)
  if left_new and right_new and right_new == left_new + 2 then return left_new + 1 end
  return nil
end

---@param source_lines string[]?
---@param block DiffReviewParsedBlock
---@param line_number integer?
---@param gutter DiffReviewGutterSpec
---@param file string?
---@param syntax? DiffReviewTreeSitterSyntax
---@return table?
function M.single_hidden_context_row(source_lines, block, line_number, gutter, file, syntax)
  if not line_number or type(source_lines) ~= "table" or not source_lines[line_number] then return nil end
  local old_line = M.old_line_for_new_line(block and block.hunks or {}, line_number)
  return M.context_padding_row(line_number, source_lines[line_number], gutter, file, syntax, old_line, line_number)
end

---@param source_lines string[]?
---@param block DiffReviewParsedBlock
---@param left_new integer?
---@param right_new integer?
---@param changed_lines table<integer, boolean>
---@param emitted_context_lines table<integer, boolean>
---@param gutter DiffReviewGutterSpec
---@param file string?
---@param syntax? DiffReviewTreeSitterSyntax
---@return table? row
---@return integer? line_number
function M.single_hidden_context_gap_row(source_lines, block, left_new, right_new, changed_lines, emitted_context_lines, gutter, file, syntax)
  local hidden_line = M.single_hidden_new_line(left_new, right_new)
  if not hidden_line or emitted_context_lines[hidden_line] or changed_lines[hidden_line] then return nil, nil end
  return M.single_hidden_context_row(source_lines, block, hidden_line, gutter, file, syntax), hidden_line
end

---@param plan DiffReviewHunkRenderPlan
---@param raw_context DiffReviewHunkTreeSitterContext|string?
---@return integer? old_line
---@return integer? new_line
function M.first_visible_plan_coords(plan, raw_context)
  local padding_line = plan.before_padding_lines and plan.before_padding_lines[1] or nil
  if padding_line then return padding_line.old_line, padding_line.new_line end
  for item_index = plan.region.first_item, plan.region.last_item do
    local item = plan.render_items[item_index]
    local parsed_line = M.render_item_line(item)
    if item.kind == "replacement" or (plan.include_render_line(parsed_line) and hunk_line_visible_in_context_scope(parsed_line, raw_context)) then
      return parsed_line.old_line, parsed_line.new_line
    end
  end
  return nil, nil
end

---@param plan DiffReviewHunkRenderPlan
---@param raw_context DiffReviewHunkTreeSitterContext|string?
---@return integer? old_line
---@return integer? new_line
function M.last_visible_plan_coords(plan, raw_context)
  local padding_line = plan.after_padding_lines and plan.after_padding_lines[#plan.after_padding_lines] or nil
  if padding_line then return padding_line.old_line, padding_line.new_line end
  for item_index = plan.region.last_item, plan.region.first_item, -1 do
    local item = plan.render_items[item_index]
    local parsed_line = M.render_item_line(item)
    if item.kind == "replacement" or (plan.include_render_line(parsed_line) and hunk_line_visible_in_context_scope(parsed_line, raw_context)) then
      return parsed_line.old_line, parsed_line.new_line
    end
  end
  return nil, nil
end

---@param previous_plan DiffReviewHunkRenderPlan
---@param next_plan DiffReviewHunkRenderPlan
---@return integer?
function M.visible_plan_gap(previous_plan, next_plan)
  local previous_old_line, previous_new_line = M.last_visible_plan_coords(previous_plan, previous_plan.region.context)
  local next_old_line, next_new_line = M.first_visible_plan_coords(next_plan, next_plan.region.context)
  return M.render_coord_gap(previous_old_line, previous_new_line, next_old_line, next_new_line)
end

return M
