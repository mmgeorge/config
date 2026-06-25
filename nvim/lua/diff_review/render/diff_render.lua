--- Builds fancy-diff row data from parsed hunks: gutter chunks, boundary/header/body rows,
--- render-plan merging, and the full per-file row list with syntax and intraline spans.
--- Pure builder: returns row tables, holding no buffer state.
---@class DiffReviewDiffRenderModule
local M = {}

local syntax_engine = require("diff_review.render.syntax_engine")
local diff_parse = require("diff_review.render.diff_parse")

local util = require("diff_review.infra.util")
local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")

-- Seams to init-owned helpers the builders share.
local detect_filetype = util.detect_filetype
local parse_hunk_body = diff_parse.parse_hunk_body
local parse_unified_diff = diff_parse.parse_unified_diff

local function default_hunk_gutter_spec()
  return { width = 12, old_width = 3, new_width = 3 }
end

local function hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl, changed_line_hl)
  gutter = gutter or default_hunk_gutter_spec()
  local old_text = old_line and ("%" .. tostring(gutter.old_width) .. "d"):format(old_line) or string.rep(" ", gutter.old_width)
  local new_text = new_line and ("%" .. tostring(gutter.new_width) .. "d"):format(new_line) or string.rep(" ", gutter.new_width)
  local old_hl = line_hl
  if old_line then
    if sign == "-" then
      old_hl = changed_line_hl or sign_hl or "DiffReviewDeleteLineNr"
    elseif sign == "~" then
      old_hl = changed_line_hl or sign_hl or "DiffReviewModifyLineNr"
    else
      old_hl = "DiffReviewContextLineNr"
    end
  end
  local new_hl = line_hl
  if new_line then
    if sign == "+" then
      new_hl = changed_line_hl or sign_hl or "DiffReviewAddLineNr"
    elseif sign == "~" then
      new_hl = changed_line_hl or sign_hl or "DiffReviewModifyLineNr"
    else
      new_hl = "DiffReviewContextLineNr"
    end
  end
  local chunks = {}
  chunks[#chunks + 1] = { old_text, old_hl }
  chunks[#chunks + 1] = { "  ", line_hl }
  chunks[#chunks + 1] = { new_text, new_hl }
  chunks[#chunks + 1] = { "  ", line_hl }
  chunks[#chunks + 1] = { sign or " ", sign_hl or line_hl }
  chunks[#chunks + 1] = { " ", line_hl }
  return chunks
end

local function hunk_add_gutter(row, gutter, old_line, new_line, sign, sign_hl, line_hl, changed_line_hl)
  -- Inline virtual text, not buffer text: visual selection, yank, and search
  -- then operate on the code content only. The default hl_mode "replace"
  -- keeps the Visual highlight from bleeding into the gutter; the row's
  -- line background is carried explicitly on every chunk via line_hl.
  row[#row + 1] = {
    col = 0,
    virt_text = hunk_gutter_chunks(gutter, old_line, new_line, sign, sign_hl, line_hl, changed_line_hl),
    virt_text_pos = "inline",
  }
end

local function hunk_boundary_row(text, segments, line_number, gutter, file, old_line, new_line)
  local row = { diff_review_boundary = true }
  gutter = gutter or default_hunk_gutter_spec()
  old_line = old_line or line_number
  new_line = new_line or line_number
  if file and line_number then
    row[#row + 1] = {
      "",
      nil,
      meta = {
        diff = dr()._hunk_diff_line_meta({
          prefix = " ",
          old_line = old_line,
          new_line = new_line,
          code = text,
        }, file),
      },
    }
  end
  hunk_add_gutter(row, gutter, old_line, new_line, " ", nil)
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { text, "DiffReviewHunkBoundary" }
  end
  return row
end

local function hunk_boundary_ellipsis_row(reference_text, gutter)
  return hunk_boundary_row(syntax_engine.line_indent(reference_text) .. "...", nil, nil, gutter)
end

local function hunk_header_row(header_parts)
  local row = { diff_review_hunk_header = true }
  for _, part in ipairs(header_parts) do
    row[#row + 1] = part
  end
  return row
end

local function hunk_body_row(parsed_line, gutter, file, syntax, syntax_row)
  local sign_hl = nil
  local line_hl = nil
  if parsed_line.prefix == "+" then
    sign_hl = "DiffReviewAddLineNr"
    line_hl = "DiffReviewAddBg"
  elseif parsed_line.prefix == "-" then
    sign_hl = "DiffReviewDeleteLineNr"
    line_hl = "DiffReviewDeleteBg"
  end

  local row = { diff_review_bg_hl = line_hl }
  local diff_meta = {
    diff = dr()._hunk_diff_line_meta(parsed_line, file),
  }
  row[#row + 1] = { "", nil, meta = diff_meta }
  hunk_add_gutter(row, gutter, parsed_line.old_line, parsed_line.new_line, parsed_line.prefix, sign_hl, line_hl)
  local segments = nil
  if syntax and syntax_row then
    segments = syntax_engine.treesitter_line_segments(syntax.buf, syntax.tree, syntax.highlight_query, syntax_row, parsed_line.code)
  end
  if segments and #segments > 0 then
    for _, segment in ipairs(segments) do
      row[#row + 1] = segment.hl_group and { segment.text, segment.hl_group } or { segment.text }
    end
  else
    row[#row + 1] = { parsed_line.code }
  end
  return row
end

local function merge_hunk_gutter_specs(left, right)
  left = left or default_hunk_gutter_spec()
  right = right or default_hunk_gutter_spec()
  local old_width = math.max(left.old_width or 0, right.old_width or 0)
  local new_width = math.max(left.new_width or 0, right.new_width or 0)
  return {
    old_width = old_width,
    new_width = new_width,
    width = old_width + 2 + new_width + 2 + 1 + 1,
  }
end

local function add_plan_to_hunk_display_group(group, plan)
  group.plans[#group.plans + 1] = plan
  group.gutter = merge_hunk_gutter_specs(group.gutter, plan.gutter)
  group.added = (group.added or 0) + (plan.region.added or 0)
  group.removed = (group.removed or 0) + (plan.region.removed or 0)
  if plan.display_start then group.display_start = math.min(group.display_start or plan.display_start, plan.display_start) end
  if plan.display_end then group.display_end = math.max(group.display_end or plan.display_end, plan.display_end) end
  for line_number in pairs(plan.changed_lines or {}) do
    group.changed_lines[line_number] = true
  end
end

local function merge_hunk_render_plans(plans)
  local groups = {}
  local current_group = nil ---@type DiffReviewHunkDisplayGroup?
  for _, plan in ipairs(plans) do
    local previous_plan = current_group and current_group.plans[#current_group.plans] or nil
    if not (current_group and previous_plan and dr()._hunk_render_plans_should_merge(previous_plan, plan)) then
      current_group = {
        plans = {},
        gutter = plan.gutter,
        added = 0,
        removed = 0,
        changed_lines = {},
      }
      groups[#groups + 1] = current_group
    end
    add_plan_to_hunk_display_group(current_group, plan)
  end
  return groups
end

local function build_fancy_diff_rows(diff_text, hunk_staged, filename, context_callback_key, on_context_update, opts)
  opts = opts or {}

  local diff = parse_unified_diff(diff_text)
  local syntax_diff_text = opts.syntax_diff_text or diff_text
  local debug_context = opts.debug_context
  local ret = {} ---@type table[]
  local hunk_idx = 0
  local old_syntax = nil
  local old_file_syntax = nil
  local new_syntax = nil
  local file_syntax = nil
  local syntax_pending = false
  local context_pending = false
  local old_syntax_row = tonumber(opts.old_syntax_row_offset) or 0
  local new_syntax_row = tonumber(opts.new_syntax_row_offset) or 0
  local require_file_match_for_context = opts.require_file_match_for_context == true
  local source_matches_file = not require_file_match_for_context
  if filename then
    local syntax_callback_key = context_callback_key and context_callback_key(0) or ("diff-row:" .. filename .. ":0")
    local old_pending = false
    local old_file_pending = false
    local new_pending = false
    local file_pending = false
    local uses_file_syntax = dr()._diff_uses_file_syntax(hunk_staged, opts)
    local new_side_matches_file = dr()._diff_new_side_matches_file(filename, syntax_diff_text)
    source_matches_file = (not require_file_match_for_context) or new_side_matches_file
    if debug_context then
      dr()._gitstatus_debug.event("build_fancy_diff_rows.syntax", {
        context = debug_context,
        filename = filename,
        syntax_source = opts.syntax_source,
        uses_file_syntax = uses_file_syntax,
        new_side_matches_file = new_side_matches_file,
        source_context_enabled = source_matches_file,
        diff_len = #(diff_text or ""),
        syntax_diff_len = #(syntax_diff_text or ""),
      })
    end
    local fallback_syntax_diff_text = opts.fallback_syntax_diff_text or diff_text
    if uses_file_syntax and new_side_matches_file then
      old_file_syntax, old_file_pending = dr()._cached_old_file_syntax(filename, syntax_diff_text, syntax_callback_key .. ":old-file-syntax", on_context_update)
      if not old_file_syntax and not old_file_pending then
        old_syntax, old_pending = syntax_engine.cached_diff_syntax(filename, fallback_syntax_diff_text, "old", syntax_callback_key .. ":old-diff-syntax", on_context_update)
      end
      file_syntax, file_pending = syntax_engine.cached_file_syntax(filename, syntax_callback_key .. ":file-syntax", on_context_update)
      if not file_syntax and not file_pending then
        new_syntax, new_pending = syntax_engine.cached_diff_syntax(filename, fallback_syntax_diff_text, "new", syntax_callback_key .. ":new-diff-syntax", on_context_update)
      end
    else
      old_syntax, old_pending = syntax_engine.cached_diff_syntax(filename, fallback_syntax_diff_text, "old", syntax_callback_key .. ":old-diff-syntax", on_context_update)
      new_syntax, new_pending = syntax_engine.cached_diff_syntax(filename, fallback_syntax_diff_text, "new", syntax_callback_key .. ":new-diff-syntax", on_context_update)
    end
    syntax_pending = old_pending or old_file_pending or new_pending or file_pending
  end

  local source_lines = source_matches_file and filename and dr()._file_source_lines(filename) or nil
  local plans = {} ---@type DiffReviewHunkRenderPlan[]

  local function context_for_line(block, line)
    if not (source_matches_file and filename and line) then return nil end
    local callback_key = context_callback_key and context_callback_key(line)
      or ("diff-row:" .. (filename or block.file) .. ":" .. line)
    local context = syntax_engine.cached_hunk_context(filename, line, callback_key, on_context_update)
    local cached_context = syntax_engine.context_cache_entry(filename .. ":" .. line)
    if type(cached_context) == "table" and cached_context.pending then context_pending = true end
    return context
  end

  local function syntax_for_line(parsed_line)
    local row_syntax = nil
    local row_syntax_row = nil
    if parsed_line.prefix == "-" then
      row_syntax = old_file_syntax or old_syntax
      row_syntax_row = old_file_syntax and parsed_line.old_line and (parsed_line.old_line - 1) or old_syntax_row
    elseif file_syntax and parsed_line.new_line then
      row_syntax = file_syntax
      row_syntax_row = parsed_line.new_line - 1
    else
      row_syntax = new_syntax
      row_syntax_row = new_syntax_row
    end
    return row_syntax, row_syntax_row
  end

  local function advance_syntax_rows(parsed_line)
    if parsed_line.prefix == " " then
      old_syntax_row = old_syntax_row + 1
      new_syntax_row = new_syntax_row + 1
    elseif parsed_line.prefix == "-" then
      old_syntax_row = old_syntax_row + 1
    elseif parsed_line.prefix == "+" then
      new_syntax_row = new_syntax_row + 1
    end
  end

  for _, block in ipairs(diff) do
    for _, hunk in ipairs(block.hunks) do
      hunk_idx = hunk_idx + 1
      hunk = parse_hunk_body(hunk)
      local include_render_line = dr()._hunk_render_line_filter(hunk)
      if type(source_lines) ~= "table" then
        include_render_line = function() return true end
      end
      local line_by_position = dr()._hunk_current_line_by_position(hunk)
      local occupied_lines = dr()._hunk_changed_current_line_set(hunk, line_by_position)
      local render_items = nil
      if opts.compact_replacements then
        render_items = dr()._intraline_diff.compact_hunk_lines(hunk.lines)
      else
        render_items = {}
        for _, parsed_line in ipairs(hunk.lines) do
          render_items[#render_items + 1] = { kind = "line", line = parsed_line }
        end
      end

      local regions = dr()._hunk_change_regions(render_items, line_by_position, function(line)
        return context_for_line(block, line)
      end)
      if type(source_lines) ~= "table" then
        for _, region in ipairs(regions) do
          region.first_item = 1
          region.last_item = #render_items
        end
      end
      local syntax_by_item = {}
      for item_index, item in ipairs(render_items) do
        local parsed_line = dr()._hunk_render_item_line(item)
        local row_syntax, row_syntax_row = syntax_for_line(parsed_line)
        syntax_by_item[item_index] = {
          syntax = row_syntax,
          row = row_syntax_row,
        }
        for _, backing_line in ipairs(dr()._hunk_render_item_backing_lines(item)) do
          advance_syntax_rows(backing_line)
        end
      end

      for region_index, region in ipairs(regions) do
        local region_bounds = {
          changed_line = region.changed_line,
          after_line = region.after_line,
        }
        local before_padding_lines = dr()._hunk_context_padding_lines(
          source_lines,
          hunk,
          region.context,
          "before",
          occupied_lines,
          include_render_line,
          region_bounds
        )
        dr()._hunk_annotate_padding_line_numbers(before_padding_lines, block)
        local after_padding_lines = dr()._hunk_context_padding_lines(
          source_lines,
          hunk,
          region.context,
          "after",
          occupied_lines,
          include_render_line,
          region_bounds
        )
        dr()._hunk_annotate_padding_line_numbers(after_padding_lines, block)
        local display_start, display_end = dr()._hunk_region_display_window(region, before_padding_lines, after_padding_lines)
        plans[#plans + 1] = {
          block = block,
          hunk = hunk,
          hunk_index = hunk_idx,
          region = region,
          region_index = region_index,
          region_count = #regions,
          render_items = render_items,
          include_render_line = include_render_line,
          gutter = hunk.gutter,
          source_lines = source_lines,
          occupied_lines = occupied_lines,
          syntax_by_item = syntax_by_item,
          visible_source_lines = dr()._hunk_region_visible_source_lines(region, render_items, include_render_line),
          before_padding_lines = before_padding_lines,
          after_padding_lines = after_padding_lines,
          changed_lines = dr()._hunk_region_changed_current_lines(region, render_items, line_by_position),
          display_start = display_start,
          display_end = display_end,
        }
      end
    end
  end

  local display_groups = merge_hunk_render_plans(plans)
  local emitted_start_scope_keys = {}
  for group_index, group in ipairs(display_groups) do
    local next_group = display_groups[group_index + 1]
    ret[#ret + 1] = hunk_header_row(dr()._hunk_virtual_header_parts(group, group.plans[1] and group.plans[1].hunk or nil))
    local emitted_context_lines = {}

    local function add_context_padding_rows(padding_lines, gutter, block_file)
      for _, padding_line in ipairs(padding_lines or {}) do
        if not group.changed_lines[padding_line.line_number] and not emitted_context_lines[padding_line.line_number] then
          ret[#ret + 1] = dr()._hunk_context_padding_row(
            padding_line.line_number,
            padding_line.text,
            gutter,
            filename or block_file,
            file_syntax,
            padding_line.old_line,
            padding_line.new_line
          )
          emitted_context_lines[padding_line.line_number] = true
        end
      end
    end

    local function mark_emitted_context_line(parsed_line)
      if parsed_line.new_line then emitted_context_lines[parsed_line.new_line] = true end
    end

    local function add_context_bridge_rows(plan, next_plan)
      if not (plan and next_plan and type(source_lines) == "table") then return end
      local previous_old_line, previous_new_line = dr()._hunk_last_visible_plan_coords(plan, plan.region.context)
      local next_old_line, next_new_line = dr()._hunk_first_visible_plan_coords(next_plan, next_plan.region.context)
      local gap = dr()._hunk_render_coord_gap(previous_old_line, previous_new_line, next_old_line, next_new_line)
      if not (gap and gap > 0 and gap <= dr()._hunk_context_bridge_limit()) then return end
      if not (previous_new_line and next_new_line and next_new_line > previous_new_line + 1) then return end
      for line_number = previous_new_line + 1, next_new_line - 1 do
        if not group.changed_lines[line_number] and not emitted_context_lines[line_number] then
          ret[#ret + 1] = dr()._hunk_context_padding_row(
            line_number,
            source_lines[line_number] or "",
            group.gutter,
            filename or plan.block.file,
            file_syntax,
            dr()._hunk_old_line_for_new_line(plan.block.hunks, line_number),
            line_number
          )
          emitted_context_lines[line_number] = true
        end
      end
    end

    for plan_index, plan in ipairs(group.plans) do
      local raw_context = plan.region.context
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local start_text = raw_context.start_text or ""
        local previous_plan = group.plans[plan_index - 1]
        local start_boundaries = {}
        for _, boundary in ipairs(raw_context.ancestor_boundaries or {}) do
          start_boundaries[#start_boundaries + 1] = boundary
        end
        start_boundaries[#start_boundaries + 1] = {
          key = syntax_engine.hunk_context_scope_key(raw_context),
          row = node_start,
          text = start_text,
          segments = raw_context.start_segments,
          selected = true,
        }
        for boundary_index, boundary in ipairs(start_boundaries) do
          local boundary_key = boundary.key
          local boundary_seen = boundary_key and emitted_start_scope_keys[boundary_key] == true
          local suppress_boundary_key = boundary_key
            and opts.suppress_start_boundary_keys
            and opts.suppress_start_boundary_keys[boundary_key] == true
          local suppress_start_boundary = (group_index == 1 and plan_index == 1 and opts.suppress_start_boundary)
            or syntax_engine.same_hunk_context_scope(previous_plan and previous_plan.region.context, raw_context)
            or boundary_seen
            or suppress_boundary_key
          local boundary_visible = plan.visible_source_lines[boundary.text] == true
          local boundary_rendered = false
          if not suppress_start_boundary
            and not boundary_visible
            and not emitted_context_lines[boundary.row] then
            local boundary_old_line = dr()._hunk_old_line_for_new_line(plan.block.hunks, boundary.row)
            local boundary_new_line = boundary.row
            ret[#ret + 1] = hunk_boundary_row(
              boundary.text,
              boundary.segments,
              boundary.row,
              group.gutter,
              filename or plan.block.file,
              boundary_old_line,
              boundary_new_line
            )
            boundary_rendered = true
            emitted_context_lines[boundary.row] = true
            local next_old_line = nil
            local next_new_line = nil
            local next_boundary = start_boundaries[boundary_index + 1]
            if next_boundary then
              next_old_line = dr()._hunk_old_line_for_new_line(plan.block.hunks, next_boundary.row)
              next_new_line = next_boundary.row
            else
              next_old_line, next_new_line = dr()._hunk_first_visible_plan_coords(plan, raw_context)
            end
            local adjacent_to_next = dr()._hunk_render_coords_adjacent(boundary_old_line, boundary_new_line, next_old_line, next_new_line)
            if not adjacent_to_next then
              if boundary.selected then
                local hidden_row, hidden_line = dr()._hunk_single_hidden_context_gap_row(
                  source_lines,
                  plan.block,
                  boundary_new_line,
                  next_new_line,
                  group.changed_lines,
                  emitted_context_lines,
                  group.gutter,
                  filename or plan.block.file,
                  file_syntax
                )
                if hidden_row then
                  ret[#ret + 1] = hidden_row
                  emitted_context_lines[hidden_line] = true
                else
                  ret[#ret + 1] = hunk_boundary_ellipsis_row(boundary.text, group.gutter)
                end
              else
                ret[#ret + 1] = hunk_boundary_ellipsis_row(boundary.text, group.gutter)
              end
            end
          end
          if boundary_key and not boundary_seen and (boundary_rendered or boundary_visible) then
            emitted_start_scope_keys[boundary_key] = true
          end
        end
      end
      add_context_padding_rows(plan.before_padding_lines, group.gutter, plan.block.file)

      local previous_visible_changed = false
      local last_visible_new_line = nil
      for item_index = plan.region.first_item, plan.region.last_item do
        local item = plan.render_items[item_index]
        local parsed_line = dr()._hunk_render_item_line(item)
        local syntax_entry = plan.syntax_by_item[item_index] or {}
        local row_syntax = syntax_entry.syntax
        local row_syntax_row = syntax_entry.row
        local visible_in_scope = syntax_engine.hunk_line_visible_in_context_scope(parsed_line, raw_context)
        local visible_in_hunk = plan.include_render_line(parsed_line)
        local context_already_emitted = parsed_line.prefix == " " and parsed_line.new_line and emitted_context_lines[parsed_line.new_line]
        if item.kind == "replacement" then
          ret[#ret + 1] = dr()._hunk_replacement_row(item, group.gutter, filename or plan.block.file, row_syntax, row_syntax_row)
          for _, backing_line in ipairs(item.diff_lines or {}) do
            mark_emitted_context_line(backing_line)
          end
          last_visible_new_line = parsed_line.new_line
          previous_visible_changed = true
        elseif visible_in_hunk and visible_in_scope and not context_already_emitted then
          ret[#ret + 1] = hunk_body_row(parsed_line, group.gutter, filename or plan.block.file, row_syntax, row_syntax_row)
          mark_emitted_context_line(parsed_line)
          last_visible_new_line = parsed_line.new_line
          previous_visible_changed = parsed_line.prefix == "+" or parsed_line.prefix == "-"
        elseif dr()._hunk_hidden_closing_boundary_after_change(parsed_line, previous_visible_changed, raw_context, visible_in_hunk) then
          local boundary_segments = nil
          if row_syntax and row_syntax_row then
            boundary_segments = syntax_engine.treesitter_line_segments(row_syntax.buf, row_syntax.tree, row_syntax.highlight_query, row_syntax_row, parsed_line.code)
          end
          local previous_line_emitted = parsed_line.new_line and emitted_context_lines[parsed_line.new_line - 1] == true
          if not previous_line_emitted then
            local hidden_row, hidden_line = dr()._hunk_single_hidden_context_gap_row(
              source_lines,
              plan.block,
              last_visible_new_line,
              parsed_line.new_line,
              group.changed_lines,
              emitted_context_lines,
              group.gutter,
              filename or plan.block.file,
              file_syntax
            )
            if hidden_row then
              ret[#ret + 1] = hidden_row
              emitted_context_lines[hidden_line] = true
            else
              ret[#ret + 1] = hunk_boundary_ellipsis_row(parsed_line.code, group.gutter)
            end
          end
          ret[#ret + 1] = hunk_boundary_row(
            parsed_line.code,
            boundary_segments,
            parsed_line.new_line or parsed_line.old_line,
            group.gutter,
            filename or plan.block.file,
            parsed_line.old_line,
            parsed_line.new_line
          )
          mark_emitted_context_line(parsed_line)
          last_visible_new_line = parsed_line.new_line
          previous_visible_changed = false
        end
      end

      local next_plan = group.plans[plan_index + 1]
      local after_padding_lines = plan.after_padding_lines
      if next_plan and next_plan.region.changed_line then
        after_padding_lines = {}
        for _, padding_line in ipairs(plan.after_padding_lines or {}) do
          if padding_line.line_number < next_plan.region.changed_line then after_padding_lines[#after_padding_lines + 1] = padding_line end
        end
      end
      add_context_padding_rows(after_padding_lines, group.gutter, plan.block.file)
      add_context_bridge_rows(plan, next_plan)
      if opts.boundary_context and type(raw_context) == "table" then
        local node_start = raw_context.start_row + 1
        local node_end = raw_context.end_row + 1
        local end_text = raw_context.end_text or ""
        local next_group_plan = next_plan == nil and next_group and next_group.plans and next_group.plans[1] or nil
        local suppress_end_boundary = next_plan ~= nil
          or (group_index == #display_groups and plan_index == #group.plans and opts.suppress_end_boundary)
          or syntax_engine.same_hunk_context_scope(raw_context, next_plan and next_plan.region.context)
          or syntax_engine.same_hunk_context_scope(raw_context, next_group_plan and next_group_plan.region.context)
        if not suppress_end_boundary and not plan.visible_source_lines[end_text] and not emitted_context_lines[node_end] then
          local boundary_old_line = dr()._hunk_old_line_for_new_line(plan.block.hunks, node_end)
          local boundary_new_line = node_end
          local previous_old_line, previous_new_line = dr()._hunk_last_visible_plan_coords(plan, raw_context)
          local adjacent_to_previous = dr()._hunk_render_coords_adjacent(previous_old_line, previous_new_line, boundary_old_line, boundary_new_line)
          if node_end ~= node_start and not adjacent_to_previous then
            local hidden_row, hidden_line = dr()._hunk_single_hidden_context_gap_row(
              source_lines,
              plan.block,
              previous_new_line,
              boundary_new_line,
              group.changed_lines,
              emitted_context_lines,
              group.gutter,
              filename or plan.block.file,
              file_syntax
            )
            if hidden_row then
              ret[#ret + 1] = hidden_row
              emitted_context_lines[hidden_line] = true
            else
              ret[#ret + 1] = hunk_boundary_ellipsis_row(end_text, group.gutter)
            end
          end
          if node_end ~= node_start then
            ret[#ret + 1] = hunk_boundary_row(
              end_text,
              raw_context.end_segments,
              node_end,
              group.gutter,
              filename or plan.block.file,
              boundary_old_line,
              boundary_new_line
            )
          end
        end
        local ancestor_boundary = dr()._hunk_context_ancestor_boundary(raw_context)
        if ancestor_boundary
          and ancestor_boundary.end_row
          and ancestor_boundary.end_row ~= node_end then
          local suppress_ancestor_end = (ancestor_boundary.key ~= nil
              and opts.suppress_end_boundary_keys
              and opts.suppress_end_boundary_keys[ancestor_boundary.key] == true)
            or (next_plan ~= nil and dr()._hunk_context_ancestor_key(next_plan.region.context) == ancestor_boundary.key)
            or (next_group_plan ~= nil and dr()._hunk_context_ancestor_key(next_group_plan.region.context) == ancestor_boundary.key)
          if not suppress_ancestor_end and not emitted_context_lines[ancestor_boundary.end_row] then
            local boundary_old_line = dr()._hunk_old_line_for_new_line(plan.block.hunks, ancestor_boundary.end_row)
            local boundary_new_line = ancestor_boundary.end_row
            local previous_old_line, previous_new_line = dr()._hunk_last_visible_plan_coords(plan, raw_context)
            local adjacent_to_previous = dr()._hunk_render_coords_adjacent(previous_old_line, previous_new_line, boundary_old_line, boundary_new_line)
            if not adjacent_to_previous then
              local hidden_row, hidden_line = dr()._hunk_single_hidden_context_gap_row(
                source_lines,
                plan.block,
                previous_new_line,
                boundary_new_line,
                group.changed_lines,
                emitted_context_lines,
                group.gutter,
                filename or plan.block.file,
                file_syntax
              )
              if hidden_row then
                ret[#ret + 1] = hidden_row
                emitted_context_lines[hidden_line] = true
              else
                ret[#ret + 1] = hunk_boundary_ellipsis_row(ancestor_boundary.end_text, group.gutter)
              end
            end
            ret[#ret + 1] = hunk_boundary_row(
              ancestor_boundary.end_text,
              ancestor_boundary.end_segments,
              ancestor_boundary.end_row,
              group.gutter,
              filename or plan.block.file,
              boundary_old_line,
              boundary_new_line
            )
            emitted_context_lines[ancestor_boundary.end_row] = true
          end
        end
      end
    end
  end

  ret.diff_review_syntax_pending = syntax_pending
  ret.diff_review_context_pending = context_pending
  return ret
end

local function render_highlight_rows(buf, ns, rows)
  local lines = {}
  local highlights = {}
  local line_highlights = {}
  local extmarks = {}
  local empty_diff_rows = {}
  local content_lengths = {}

  for row_index, row in ipairs(rows) do
    local line_parts = {}
    local col = 0
    local empty_diff_row = false
    for _, chunk in ipairs(row) do
      if chunk.meta and chunk.meta.diff and chunk.meta.diff.code == "" then
        empty_diff_row = true
      end
      if type(chunk[1]) == "string" then
        local text = chunk[1]
        line_parts[#line_parts + 1] = text
        if chunk[2] and text ~= "" then
          highlights[#highlights + 1] = {
            line = row_index,
            start_col = col,
            end_col = col + #text,
            hl_group = chunk[2],
          }
        end
        col = col + #text
      elseif chunk.virt_text then
        local opts = {}
        for key, value in pairs(chunk) do
          if key ~= "col" then opts[key] = value end
        end
        extmarks[#extmarks + 1] = {
          line = row_index,
          col = chunk.col or 0,
          opts = opts,
        }
      end
    end
    lines[row_index] = table.concat(line_parts)
    if row.diff_review_bg_hl then
      local content_length
      lines[row_index], content_length = dr()._diff_pad_highlighted_line(lines[row_index], buf)
      content_lengths[row_index] = content_length
      extmarks[#extmarks + 1] = {
        line = row_index,
        col = 0,
        opts = {
          end_col = #lines[row_index],
          hl_group = row.diff_review_bg_hl,
          priority = 60,
        },
      }
    end
    for _, inline_highlight in ipairs(row.diff_review_inline_highlights or {}) do
      highlights[#highlights + 1] = {
        line = row_index,
        start_col = inline_highlight.start_col,
        end_col = inline_highlight.end_col,
        hl_group = inline_highlight.hl_group,
        priority = inline_highlight.priority or 110,
      }
    end
    if empty_diff_row then empty_diff_rows[row_index] = true end
  end

  session.empty_diff_rows = session.empty_diff_rows or {}
  session.empty_diff_rows[buf] = empty_diff_rows
  session.diff_line_content_lengths = session.diff_line_content_lengths or {}
  session.diff_line_content_lengths[buf] = content_lengths
  dr()._clear_diff_gutter_visual_line(buf)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  for _, line_hl in ipairs(line_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, ns, line_hl.line - 1, 0, {
      line_hl_group = line_hl.hl_group,
      priority = 80,
    })
  end
  for _, highlight in ipairs(highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, ns, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = highlight.priority or 90,
    })
  end
  for _, extmark in ipairs(extmarks) do
    local opts = vim.tbl_extend("force", { priority = 95 }, extmark.opts)
    pcall(vim.api.nvim_buf_set_extmark, buf, ns, extmark.line - 1, extmark.col, opts)
  end
end

local function render_fancy_diff(buf, diff_text, hunk_staged, filename)
  local ft = filename and detect_filetype(filename) or ""
  if ft ~= "" and vim.bo[buf].filetype ~= ft then
    vim.bo[buf].filetype = ft
  end

  render_highlight_rows(buf, dr()._ns, build_fancy_diff_rows(
    diff_text,
    hunk_staged,
    filename,
    function(hunk_line)
      return ("diff-buffer:%d:%s:%d"):format(buf, filename or "", hunk_line)
    end,
    function()
      if not (buf and vim.api.nvim_buf_is_valid(buf) and filename) then return end
      session.buf_last_rendered[buf] = nil
      dr()._refresh_diff_buffer(buf, filename)
    end
  ))
end

-- Public surface (init/hunk_model reach these via seams).
M.merge_hunk_gutter_specs = merge_hunk_gutter_specs
M.add_plan_to_hunk_display_group = add_plan_to_hunk_display_group
M.merge_hunk_render_plans = merge_hunk_render_plans
M.default_hunk_gutter_spec = default_hunk_gutter_spec
M.hunk_gutter_chunks = hunk_gutter_chunks
M.hunk_add_gutter = hunk_add_gutter
M.hunk_boundary_row = hunk_boundary_row
M.hunk_boundary_ellipsis_row = hunk_boundary_ellipsis_row
M.hunk_header_row = hunk_header_row
M.hunk_body_row = hunk_body_row
M.build_fancy_diff_rows = build_fancy_diff_rows
M.render_highlight_rows = render_highlight_rows
M.render_fancy_diff = render_fancy_diff

return M
