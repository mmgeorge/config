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

--- Returns the innermost ancestor boundary descriptor from a Tree-sitter context record.
---@param context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor or name.
---@return DiffReviewHunkBoundaryContext? boundary Innermost ancestor boundary descriptor, or nil.
function M.context_ancestor_boundary(context)
  if type(context) ~= "table" or type(context.ancestor_boundaries) ~= "table" then return nil end
  return context.ancestor_boundaries[#context.ancestor_boundaries]
end

--- Returns the lookup key string of the innermost ancestor boundary in a context.
---@param context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor or name.
---@return string? key Unique boundary identifier key, or nil.
function M.context_ancestor_key(context)
  local boundary = M.context_ancestor_boundary(context)
  return boundary and boundary.key or nil
end

--- Constructs a line filter predicate restricting visible context lines to the hunk's active change window.
---@param hunk DiffReviewParsedHunk Parsed diff hunk descriptor.
---@return fun(parsed_line: DiffReviewParsedHunkLine): boolean filter Filter predicate returning true for visible lines.
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

--- Collects the set of code text lines visible under the given filter.
---@param parsed_lines DiffReviewParsedHunkLine[] Array of parsed hunk lines.
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean Optional line filter predicate.
---@return table<string, boolean> visible Set of visible code line strings.
function M.visible_parsed_source_lines(parsed_lines, include_line)
  local visible = {}
  for _, parsed_line in ipairs(parsed_lines or {}) do
    if (not include_line or include_line(parsed_line)) and (parsed_line.prefix == " " or parsed_line.prefix == "+" or parsed_line.prefix == "-") then
      visible[parsed_line.code] = true
    end
  end
  return visible
end

--- Computes the one-based new revision line number immediately following the last line in a hunk.
---@param hunk DiffReviewParsedHunk Parsed diff hunk descriptor.
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean Optional line filter predicate.
---@return integer after_line One-based line number after hunk.
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

--- Builds a set of all new revision line numbers occupied by lines in a hunk.
---@param hunk DiffReviewParsedHunk Parsed diff hunk descriptor.
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean Optional line filter predicate.
---@return table<integer, boolean> lines Set of occupied new line numbers.
function M.current_line_set(hunk, include_line)
  local lines = {}
  for _, parsed_line in ipairs(hunk.lines) do
    if parsed_line.new_line and (not include_line or include_line(parsed_line)) then lines[parsed_line.new_line] = true end
  end
  return lines
end

--- Resolves the primary parsed diff line associated with a render item.
---@param item table Render item descriptor table.
---@return DiffReviewParsedHunkLine line Primary parsed diff line.
function M.render_item_line(item)
  return item.line or item.display_line
end

--- Returns the array of underlying backing diff lines represented by a render item.
---@param item table Render item descriptor table.
---@return DiffReviewParsedHunkLine[] lines Array of backing diff lines.
function M.render_item_backing_lines(item)
  if item.kind == "replacement" then return item.diff_lines or { item.display_line } end
  return { item.line }
end

--- Reports whether a render item represents a modified, added, or deleted line.
---@param item table Render item descriptor table.
---@return boolean changed True if the item is a changed line or replacement.
function M.render_item_changed(item)
  if item.kind == "replacement" then return true end
  local parsed_line = M.render_item_line(item)
  return parsed_line.prefix == "+" or parsed_line.prefix == "-"
end

--- Maps hunk line positions to effective current new revision line numbers.
---@param hunk DiffReviewParsedHunk Parsed diff hunk descriptor.
---@return table<integer, integer> by_position Mapping from hunk position to current line number.
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

--- Resolves the effective current new revision line for a parsed diff line.
---@param parsed_line DiffReviewParsedHunkLine? Parsed diff line.
---@param line_by_position table<integer, integer> Mapping from hunk position to current line number.
---@return integer? line Effective current line number, or nil.
function M.parsed_line_current_line(parsed_line, line_by_position)
  if not parsed_line then return nil end
  return parsed_line.new_line or (parsed_line.position and line_by_position[parsed_line.position]) or parsed_line.old_line
end

--- Resolves the representative context line number for a render item.
---@param item table Render item descriptor table.
---@param line_by_position table<integer, integer> Mapping from hunk position to current line number.
---@return integer? line Representative context line number, or nil.
function M.render_item_context_line(item, line_by_position)
  if item.kind == "replacement" then
    for _, parsed_line in ipairs(item.new_lines or {}) do
      if parsed_line.new_line then return parsed_line.new_line end
    end
  end
  return M.parsed_line_current_line(M.render_item_line(item), line_by_position)
end

--- Computes the start and end changed line numbers for a render item.
---@param item table Render item descriptor table.
---@param line_by_position table<integer, integer> Mapping from hunk position to current line number.
---@return integer? first_line Starting changed line number.
---@return integer? last_line Ending changed line number.
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

--- Tests whether a context scope interval includes the specified one-based line number.
---@param context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor or name.
---@param line integer? One-based line number to test.
---@return boolean contains True if context spans the line.
function M.context_contains_line(context, line)
  if type(context) ~= "table" or not line then return true end
  return line >= context.start_row + 1 and line <= context.end_row + 1
end

--- Computes the set of changed line numbers present in a hunk.
---@param hunk DiffReviewParsedHunk Parsed diff hunk descriptor.
---@param line_by_position? table<integer, integer> Mapping from hunk position to current line number.
---@return table<integer, boolean> lines Set of changed line numbers.
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

--- Counts added and removed lines represented by a render item.
---@param item table Render item descriptor table.
---@return integer added Count of added lines.
---@return integer removed Count of removed lines.
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

--- Groups adjacent changed render items into semantic change regions bounded by context scope.
---@param render_items table[] Array of render items.
---@param line_by_position table<integer, integer> Mapping from hunk position to current line number.
---@param context_for_line fun(line: integer): DiffReviewHunkTreeSitterContext|string? Resolves Tree-sitter context for a line number.
---@return DiffReviewHunkChangeRegion[] regions Array of change region descriptor tables.
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

--- Builds formatted highlight chunks for virtual hunk headers displaying add and delete counts.
---@param region DiffReviewHunkChangeRegion Change region descriptor table.
---@param fallback_hunk? DiffReviewParsedHunk Optional fallback parsed hunk for missing counts.
---@return table[] chunks Array of `[text, hl_group]` tuples for extmark formatting.
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

--- Tests whether a hidden context line is a closing delimiter right after modified lines.
---@param parsed_line DiffReviewParsedHunkLine Parsed hunk line to inspect.
---@param previous_visible_changed boolean True if the previous visible line was changed.
---@param context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor.
---@param visible_in_hunk? boolean True if line is visible in the raw hunk.
---@return boolean is_closing True if line is a closing delimiter.
function M.hidden_closing_boundary_after_change(parsed_line, previous_visible_changed, context, visible_in_hunk)
  if not previous_visible_changed then return false end
  if parsed_line.prefix ~= " " or type(context) ~= "table" then return false end
  if visible_in_hunk and hunk_line_visible_in_context_scope(parsed_line, context) then return false end
  if not parsed_line.new_line then return false end
  return parsed_line.code:match("^%s*[})%]]+[,;]?%s*$") ~= nil
end

--- Constructs a metadata descriptor table for a single diff line.
---@param parsed_line DiffReviewParsedHunkLine Parsed hunk line.
---@param file string File path identifier.
---@return table meta Diff line metadata record.
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

--- Constructs metadata descriptor tables for an array of backing diff lines.
---@param parsed_lines DiffReviewParsedHunkLine[]? Array of parsed diff lines.
---@param file string File path identifier.
---@return table[] meta_list Array of diff line metadata records.
function M.diff_lines_meta(parsed_lines, file)
  local diff_lines = {}
  for _, parsed_line in ipairs(parsed_lines or {}) do
    diff_lines[#diff_lines + 1] = M.diff_line_meta(parsed_line, file)
  end
  return diff_lines
end

--- Constructs a rendered row chunk table for an inline modified replacement item.
---@param replacement table Replacement item descriptor table.
---@param gutter DiffReviewGutterSpec Gutter layout configuration.
---@param file string File path identifier.
---@param syntax? DiffReviewTreeSitterSyntax Syntax engine state for line highlights.
---@param syntax_row? integer Zero-based syntax row index.
---@return table row Formatted row chunk array.
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

--- Constructs a rendered row chunk table for a context padding line.
---@param line_number integer One-based source line number.
---@param text string Context line text content.
---@param gutter DiffReviewGutterSpec Gutter layout configuration.
---@param file? string Optional file path identifier.
---@param syntax? DiffReviewTreeSitterSyntax Optional syntax engine state.
---@param old_line? integer Optional old revision line number.
---@param new_line? integer Optional new revision line number.
---@return table row Formatted row chunk array.
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

--- Tests whether a source line contains informative context code rather than empty space or single closing braces.
---@param text string? Line text string.
---@return boolean is_useful True if line contains informative code text.
function M.context_padding_line_is_useful(text)
  if type(text) ~= "string" then return false end
  if text:match("^%s*$") then return false end
  if text:match("^%s*[})%]]+[,;]?%s*$") then return false end
  return true
end

--- Tests whether a code line begins a new declaration or block scope.
---@param text string? Line text string.
---@return boolean starts_scope True if line matches common scope opening patterns.
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

--- Collects candidate source context lines for padding before or after a change region.
---@param source_lines string[]? Full buffer source text lines.
---@param hunk DiffReviewParsedHunk Parsed diff hunk descriptor.
---@param context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor.
---@param side "before"|"after" Padding direction relative to changes.
---@param occupied_lines? table<integer, boolean> Set of line numbers already visible in hunk.
---@param include_line? fun(parsed_line: DiffReviewParsedHunkLine): boolean Optional line filter predicate.
---@param bounds? { changed_line?: integer, after_line?: integer } Optional explicit region bounds.
---@return DiffReviewHunkContextPaddingLine[] padding_lines Array of context padding line descriptor tables.
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

--- Returns the maximum number of context padding lines rendered around change regions.
---@return integer limit Maximum padding line count (3).
function M.context_padding_limit()
  return 3
end

--- Maps a new revision line number back to its old revision line number using diff hunk offsets.
---@param hunks DiffReviewParsedHunk[] Array of parsed diff hunks.
---@param new_line integer? New revision line number.
---@return integer? old_line Corresponding old revision line number, or nil.
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

--- Computes and attaches old and new line numbers to context padding lines.
---@param padding_lines DiffReviewHunkContextPaddingLine[] Array of padding line records.
---@param block DiffReviewParsedBlock Parent parsed hunk block descriptor.
---@return DiffReviewHunkContextPaddingLine[] padding_lines Annotated array of padding line records.
function M.annotate_padding_line_numbers(padding_lines, block)
  for _, padding_line in ipairs(padding_lines or {}) do
    padding_line.new_line = padding_line.line_number
    padding_line.old_line = M.old_line_for_new_line(block and block.hunks or {}, padding_line.line_number)
  end
  return padding_lines
end

--- Collects the set of source text lines visible within a change region.
---@param region DiffReviewHunkChangeRegion Target change region.
---@param render_items table[] Array of render items.
---@param include_render_line fun(parsed_line: DiffReviewParsedHunkLine): boolean Line filter predicate.
---@return table<string, boolean> visible Set of visible code line strings.
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

--- Computes the set of changed line numbers present in a change region.
---@param region DiffReviewHunkChangeRegion Target change region.
---@param render_items table[] Array of render items.
---@param line_by_position table<integer, integer> Mapping from hunk position to current line number.
---@return table<integer, boolean> changed Set of changed line numbers.
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

--- Calculates the starting and ending line number range displayed for a change region including its padding.
---@param region DiffReviewHunkChangeRegion Target change region.
---@param before_padding_lines DiffReviewHunkContextPaddingLine[] Leading padding lines.
---@param after_padding_lines DiffReviewHunkContextPaddingLine[] Trailing padding lines.
---@return integer? display_start Starting displayed line number.
---@return integer? display_end Ending displayed line number.
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

--- Evaluates whether two adjacent render plans should merge into one continuous render block.
---@param previous_plan DiffReviewHunkRenderPlan Earlier render plan in file order.
---@param next_plan DiffReviewHunkRenderPlan Subsequent render plan.
---@return boolean should_merge True if plans overlap or bridge within context limits.
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

--- Tests whether old or new line numbers between two render positions are immediately adjacent.
---@param left_old integer? Preceding old line number.
---@param left_new integer? Preceding new line number.
---@param right_old integer? Subsequent old line number.
---@param right_new integer? Subsequent new line number.
---@return boolean adjacent True if old or new coordinates differ by exactly one.
function M.render_coords_adjacent(left_old, left_new, right_old, right_new)
  if left_old and right_old and right_old == left_old + 1 then return true end
  if left_new and right_new and right_new == left_new + 1 then return true end
  return false
end

--- Tests whether two Tree-sitter contexts share identical scope or nested enclosing scope.
---@param left DiffReviewHunkTreeSitterContext|string? First context descriptor.
---@param right DiffReviewHunkTreeSitterContext|string? Second context descriptor.
---@return boolean related True if contexts share or nest scopes.
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

--- Returns the maximum line gap allowed when bridging related context scopes into one plan (6).
---@return integer limit Maximum bridging line count.
function M.context_bridge_limit()
  return M.context_padding_limit() * 2
end

--- Computes the maximum numeric line gap between two coordinate pairs.
---@param left_old integer? Preceding old line number.
---@param left_new integer? Preceding new line number.
---@param right_old integer? Subsequent old line number.
---@param right_new integer? Subsequent new line number.
---@return integer? gap Omitted line count gap, or nil.
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

--- Detects if two new line coordinates are separated by exactly one omitted line.
---@param left_new integer? Preceding new line number.
---@param right_new integer? Subsequent new line number.
---@return integer? hidden_line Single omitted line number, or nil.
function M.single_hidden_new_line(left_new, right_new)
  if left_new and right_new and right_new == left_new + 2 then return left_new + 1 end
  return nil
end

--- Builds a single rendered context row for an omitted line bridging a small gap.
---@param source_lines string[]? Buffer source text lines.
---@param block DiffReviewParsedBlock Parent hunk block descriptor.
---@param line_number integer? Target line number.
---@param gutter DiffReviewGutterSpec Gutter layout configuration.
---@param file string? Optional file path identifier.
---@param syntax? DiffReviewTreeSitterSyntax Optional syntax engine state.
---@return table? row Rendered row chunk array, or nil.
function M.single_hidden_context_row(source_lines, block, line_number, gutter, file, syntax)
  if not line_number or type(source_lines) ~= "table" or not source_lines[line_number] then return nil end
  local old_line = M.old_line_for_new_line(block and block.hunks or {}, line_number)
  return M.context_padding_row(line_number, source_lines[line_number], gutter, file, syntax, old_line, line_number)
end

--- Constructs a rendered context row for a 1-line gap if not already emitted or changed.
---@param source_lines string[]? Buffer source text lines.
---@param block DiffReviewParsedBlock Parent hunk block descriptor.
---@param left_new integer? Preceding new line number.
---@param right_new integer? Subsequent new line number.
---@param changed_lines table<integer, boolean> Set of changed line numbers.
---@param emitted_context_lines table<integer, boolean> Set of emitted context line numbers.
---@param gutter DiffReviewGutterSpec Gutter layout configuration.
---@param file string? Optional file path identifier.
---@param syntax? DiffReviewTreeSitterSyntax Optional syntax engine state.
---@return table? row Rendered row chunk array, or nil.
---@return integer? line_number Emitted line number, or nil.
function M.single_hidden_context_gap_row(source_lines, block, left_new, right_new, changed_lines, emitted_context_lines, gutter, file, syntax)
  local hidden_line = M.single_hidden_new_line(left_new, right_new)
  if not hidden_line or emitted_context_lines[hidden_line] or changed_lines[hidden_line] then return nil, nil end
  return M.single_hidden_context_row(source_lines, block, hidden_line, gutter, file, syntax), hidden_line
end

--- Returns the first visible old and new line coordinates rendered by a plan.
---@param plan DiffReviewHunkRenderPlan Target render plan.
---@param raw_context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor.
---@return integer? old_line First visible old line number.
---@return integer? new_line First visible new line number.
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

--- Returns the last visible old and new line coordinates rendered by a plan.
---@param plan DiffReviewHunkRenderPlan Target render plan.
---@param raw_context DiffReviewHunkTreeSitterContext|string? Syntax context descriptor.
---@return integer? old_line Last visible old line number.
---@return integer? new_line Last visible new line number.
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

--- Computes the line coordinate gap between two adjacent render plans.
---@param previous_plan DiffReviewHunkRenderPlan Earlier render plan.
---@param next_plan DiffReviewHunkRenderPlan Subsequent render plan.
---@return integer? gap Numeric line gap between plans, or nil.
function M.visible_plan_gap(previous_plan, next_plan)
  local previous_old_line, previous_new_line = M.last_visible_plan_coords(previous_plan, previous_plan.region.context)
  local next_old_line, next_new_line = M.first_visible_plan_coords(next_plan, next_plan.region.context)
  return M.render_coord_gap(previous_old_line, previous_new_line, next_old_line, next_new_line)
end

return M
