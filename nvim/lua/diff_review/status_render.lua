--- Orchestrates a full status/PR/review/diff render: builds head lines and sections into
--- the active state, writes the buffer, applies extmarks, restores the cursor, and drives
--- the decoration provider. Reads and writes the active status state via the init seam.
---@class DiffReviewStatusRenderModule
local M = {}

local status_buffer = require("diff_review.status_buffer")
local diff_render = require("diff_review.diff_render")
local syntax_engine = require("diff_review.syntax_engine")
local config = require("diff_review.config")

local status_file_indent = 0
local status_hunk_indent = 0

local function dr()
  return require("diff_review")
end

-- Render-accumulator shims: thread the active status into the state-passing buffer core.
local function status_add_line(text, entry, hl) return status_buffer.add_line(dr()._status, text, entry, hl) end
local function status_add_highlight(line, s, e, hl, p) return status_buffer.add_highlight(dr()._status, line, s, e, hl, p) end
local function status_add_extmark(line, col, opts) return status_buffer.add_extmark(dr()._status, line, col, opts) end
local function status_add_segment_line(segments, entry) return status_buffer.add_segment_line(dr()._status, segments, entry) end
local function status_add_fancy_row(row, entry, indent) return status_buffer.add_fancy_row(dr()._status, row, entry, indent) end
local function status_folded(key, default, state) return status_buffer.folded(state or dr()._status or {}, key, default) end

-- Seams to init-owned helpers (key builders, cursor, prewarm, highlights).
local function status_commit_relative_date(...) return dr()._status_commit_relative_date(...) end
local function status_hunk_key(...) return dr()._status_hunk_key(...) end
local function status_provider_hunk_key(...) return dr()._status_provider_hunk_key(...) end
local function status_provider_file_key(...) return dr()._status_provider_file_key(...) end
local function status_prewarm_entry_syntax(...) return dr()._status_prewarm_entry_syntax(...) end
local function setup_bg_highlights(...) return dr()._setup_bg_highlights(...) end
local function status_restore_cursor(...) return dr()._status_restore_cursor(...) end
local function status_commit_key(...) return dr()._status_commit_key(...) end
local function status_commit_hunk_key(...) return dr()._status_commit_hunk_key(...) end
local function status_commit_file_key(...) return dr()._status_commit_file_key(...) end
local function status_file_key(...) return dr()._status_file_key(...) end
local function status_section_key(...) return dr()._status_section_key(...) end
local function status_diff_hunks_for_file(...) return dr()._status_diff_hunks_for_file(...) end
local function status_cursor_target(...) return dr()._status_cursor_target(...) end
local function status_operations_pending(...) return dr()._status_operations_pending(...) end

local function status_render_hunk(file, hunk, previous_hunk, next_hunk, entry_kind, hunk_key_override)
  return dr()._status_perf_span("status_render.render_hunk", dr()._status and dr()._status.buf or nil, {
    file = file and file.filename or nil,
    relpath = file and file.relpath or nil,
    entry_kind = entry_kind,
    hunk_pos = hunk and hunk.pos or nil,
    hunk_added = hunk and hunk.added or nil,
    hunk_removed = hunk and hunk.removed or nil,
    diff_len = hunk and #(hunk.diff or "") or nil,
  }, function()
  local hunk_key = hunk_key_override or status_hunk_key(file.section_name, file.filename, hunk.diff)
  local hunk_folded = status_folded(hunk_key, false)
  local entry = { id = hunk_key, kind = entry_kind or "hunk", file = file, hunk = hunk, default_folded = false }
  if hunk_folded then
    local header = ("%s@@ +%d -%d"):format(string.rep(" ", status_hunk_indent), hunk.added or 0, hunk.removed or 0)
    local start_line = status_add_line(header, entry, "DiffReviewActiveHunkHeader")
    dr()._status_register_fold_range(hunk_key, start_line, #dr()._status.lines, false, dr()._status.lines[start_line])
    return
  end
  dr()._status.fancy_rows = dr()._status.fancy_rows or {}
  local rows_key
  local context_line = dr()._status_hunk_context_line(hunk) or hunk.pos
  local previous_context_line = previous_hunk and dr()._status_hunk_context_line(previous_hunk) or nil
  local next_context_line = next_hunk and dr()._status_hunk_context_line(next_hunk) or nil
  local syntax_source = dr()._status_syntax_source_for_entry_kind(entry_kind)
  local syntax_diff_text = nil
  if syntax_source == "file" then
    syntax_diff_text = dr()._status_perf_span("status_render.render_hunk.syntax_diff_text", dr()._status and dr()._status.buf or nil, {
      file = file.filename,
      hunk_key = hunk_key,
      entry_kind = entry_kind,
    }, function()
      return dr()._status_file_syntax_diff_text(file)
    end)
  end
  local require_file_match_for_context = entry_kind == "pr_hunk" or entry_kind == "pr_review_hunk"
  local semantic_context_enabled = true
  if require_file_match_for_context then
    semantic_context_enabled = syntax_diff_text ~= nil and dr()._status_perf_span("status_render.render_hunk.new_side_matches_file", dr()._status and dr()._status.buf or nil, {
      file = file.filename,
      hunk_key = hunk_key,
      entry_kind = entry_kind,
      diff_len = #(syntax_diff_text or ""),
    }, function()
      return dr()._diff_new_side_matches_file(file.filename, syntax_diff_text)
    end)
  end
  local function rerender_with_context()
    dr()._status = dr()._status or {}
    if dr()._status.context_rerender_pending then return end
    dr()._status.context_rerender_pending = true
    vim.schedule(function()
      if not dr()._status then return end
      dr()._status.context_rerender_pending = false
      dr()._status.fancy_rows = {}
      local buf = dr()._status.buf
      if buf and vim.api.nvim_buf_is_valid(buf) then
        if status_operations_pending() then return end
        local target_id, fallback_line = status_cursor_target(buf)
        dr().render_status(buf, target_id, fallback_line, { reuse_sections = true })
      end
    end)
  end
  local current_context = nil
  local previous_context = nil
  local next_context = nil
  if semantic_context_enabled then
    dr()._status_perf_span("status_render.render_hunk.context_lookup", dr()._status and dr()._status.buf or nil, {
      file = file.filename,
      hunk_key = hunk_key,
      context_line = context_line,
      previous_context_line = previous_context_line,
      next_context_line = next_context_line,
    }, function()
      current_context = syntax_engine.cached_hunk_context(
        file.filename,
        context_line,
        "status-neighbor:" .. hunk_key .. ":current",
        rerender_with_context
      )
      previous_context = previous_hunk and syntax_engine.cached_hunk_context(
        file.filename,
        previous_context_line or previous_hunk.pos,
        "status-neighbor:" .. hunk_key .. ":previous",
        rerender_with_context
      ) or nil
      next_context = next_hunk and syntax_engine.cached_hunk_context(
        file.filename,
        next_context_line or next_hunk.pos,
        "status-neighbor:" .. hunk_key .. ":next",
        rerender_with_context
      ) or nil
    end)
  end
  local suppress_start_boundary = syntax_engine.same_hunk_context_scope(previous_context, current_context)
  local suppress_end_boundary = syntax_engine.same_hunk_context_scope(current_context, next_context)
  local current_ancestor_key = dr()._hunk_context_ancestor_key(current_context)
  local suppress_ancestor_start = current_ancestor_key ~= nil and dr()._same_hunk_ancestor_scope(previous_context, current_context)
  local suppress_ancestor_end = current_ancestor_key ~= nil and dr()._same_hunk_ancestor_scope(current_context, next_context)
  local suppress_start_boundary_keys = suppress_ancestor_start and { [current_ancestor_key] = true } or nil
  local suppress_end_boundary_keys = suppress_ancestor_end and { [current_ancestor_key] = true } or nil
  local syntax_hash = dr()._status_perf_span("status_render.render_hunk.rows_key_sha256", dr()._status and dr()._status.buf or nil, {
    file = file.filename,
    hunk_key = hunk_key,
    diff_len = #(syntax_diff_text or hunk.diff or ""),
  }, function()
    return vim.fn.sha256(syntax_diff_text or hunk.diff or "")
  end)
  rows_key = ("%s:%s:%s:%s:%s:%s:%s"):format(
    hunk_key,
    hunk.staged and "staged" or "unstaged",
    suppress_start_boundary and "no-start" or "start",
    suppress_end_boundary and "no-end" or "end",
    suppress_ancestor_start and "no-ancestor-start" or "ancestor-start",
    suppress_ancestor_end and "no-ancestor-end" or "ancestor-end",
    syntax_source .. ":compact:" .. syntax_hash
  )
  local debug_context = nil
  if dr()._gitstatus_debug.enabled() then
    local view_kind = dr()._status and dr()._status.view_kind or nil
    if view_kind == "pr" or entry_kind == "pr_hunk" or entry_kind == "pr_review_hunk" then
      local source_lines = dr()._file_source_lines(file.filename)
      local new_side_matches_file = nil
      if syntax_diff_text then
        new_side_matches_file = semantic_context_enabled
      end
      debug_context = {
        view_kind = view_kind,
        entry_kind = entry_kind or "hunk",
        buf = dr()._status and dr()._status.buf or nil,
        file = file.filename,
        relpath = file.relpath,
        section = file.section_name,
        hunk_key = hunk_key,
        rows_key = rows_key,
        hunk_pos = hunk.pos,
        hunk_added = hunk.added,
        hunk_removed = hunk.removed,
        hunk_staged = hunk.staged,
        hunk_len = #(hunk.diff or ""),
        syntax_source = syntax_source,
        syntax_diff_len = syntax_diff_text and #syntax_diff_text or nil,
        new_side_matches_file = new_side_matches_file,
        semantic_context_enabled = semantic_context_enabled,
        source_line_count = type(source_lines) == "table" and #source_lines or nil,
        source_readable = vim.fn.filereadable(file.filename) == 1,
        context_line = context_line,
        previous_context_line = previous_context_line,
        next_context_line = next_context_line,
        suppress_start_boundary = suppress_start_boundary,
        suppress_end_boundary = suppress_end_boundary,
        suppress_ancestor_start = suppress_ancestor_start,
        suppress_ancestor_end = suppress_ancestor_end,
      }
      dr()._gitstatus_debug.event("status_render_hunk.before", debug_context)
    end
  end
  local rows = dr()._status.fancy_rows[rows_key]
  if rows and debug_context then
    dr()._gitstatus_debug.event("status_render_hunk.cache_hit", {
      context = debug_context,
      row_count = #rows,
      syntax_pending = rows.diff_review_syntax_pending,
      context_pending = rows.diff_review_context_pending,
      preview = dr()._gitstatus_debug.row_preview(rows, 8),
    })
  end
  if not rows then
    local ok, built_rows = dr()._status_perf_span("status_render.render_hunk.build_fancy_rows", dr()._status and dr()._status.buf or nil, {
      file = file.filename,
      hunk_key = hunk_key,
      rows_key = rows_key,
      diff_len = #(hunk.diff or ""),
      syntax_source = syntax_source,
      syntax_diff_len = syntax_diff_text and #syntax_diff_text or nil,
    }, function()
      return pcall(
        diff_render.build_fancy_diff_rows,
        hunk.diff,
        { hunk.staged },
        file.filename,
        function(hunk_line)
          return ("status:%s:%d"):format(rows_key, hunk_line)
        end,
        rerender_with_context,
        {
          context_line = context_line,
          boundary_context = true,
          suppress_start_boundary = suppress_start_boundary,
          suppress_end_boundary = suppress_end_boundary,
          suppress_start_boundary_keys = suppress_start_boundary_keys,
          suppress_end_boundary_keys = suppress_end_boundary_keys,
          syntax_source = syntax_source,
          syntax_diff_text = syntax_diff_text,
          fallback_syntax_diff_text = hunk.fallback_syntax_diff_text,
          old_syntax_row_offset = hunk.old_syntax_row_offset,
          new_syntax_row_offset = hunk.new_syntax_row_offset,
          compact_replacements = true,
          require_file_match_for_context = require_file_match_for_context,
          debug_context = debug_context,
        }
      )
    end)
    if debug_context then
      dr()._gitstatus_debug.event("status_render_hunk.built", {
        context = debug_context,
        ok = ok,
        error = ok and nil or tostring(built_rows),
        row_count = ok and type(built_rows) == "table" and #built_rows or nil,
        syntax_pending = ok and type(built_rows) == "table" and built_rows.diff_review_syntax_pending or nil,
        context_pending = ok and type(built_rows) == "table" and built_rows.diff_review_context_pending or nil,
        preview = ok and dr()._gitstatus_debug.row_preview(built_rows, 8) or nil,
      })
    end
    if ok then
      rows = built_rows
      dr()._status.lazy_hunk_render_pending = rows.diff_review_syntax_pending or rows.diff_review_context_pending or nil
      if not rows.diff_review_syntax_pending and not rows.diff_review_context_pending then
        dr()._status.fancy_rows[rows_key] = rows
      end
    end
  end
  if not rows then
    if debug_context then
      dr()._gitstatus_debug.event("status_render_hunk.fallback", {
        context = debug_context,
      })
    end
    local ts_context = current_context
    if semantic_context_enabled and not ts_context then
      ts_context = syntax_engine.cached_hunk_context(file.filename, context_line, "status-fallback:" .. hunk_key, rerender_with_context)
    end
    local header = ("%s@@ +%d -%d"):format(string.rep(" ", status_hunk_indent), hunk.added or 0, hunk.removed or 0)
    local visible_hunk_lines = nil
    local node_start = nil
    local node_end = nil
    local start_text = nil
    local end_text = nil
    if type(ts_context) == "table" then
      visible_hunk_lines = dr()._status_perf_span("status_render.render_hunk.visible_source_lines", dr()._status and dr()._status.buf or nil, {
        file = file.filename,
        hunk_key = hunk_key,
        diff_len = #(hunk.diff or ""),
      }, function()
        return syntax_engine.hunk_visible_source_lines(hunk.diff)
      end)
      node_start = ts_context.start_row + 1
      node_end = ts_context.end_row + 1
      start_text = ts_context.start_text or ""
      end_text = ts_context.end_text or ""
    end
    local start_line = status_add_line(header, entry, hunk_folded and "DiffReviewActiveHunkHeader" or "DiffReviewHunkHeader")
    if visible_hunk_lines and node_start and node_end and end_text then
      if not suppress_start_boundary and not visible_hunk_lines[start_text] then
        status_add_fancy_row(diff_render.hunk_boundary_row(start_text, ts_context.start_segments, node_start), nil, status_hunk_indent)
        if node_start ~= node_end then
          status_add_fancy_row(diff_render.hunk_boundary_ellipsis_row(start_text), nil, status_hunk_indent)
        end
      end
      if not suppress_end_boundary and not visible_hunk_lines[end_text] and node_end ~= node_start then
        status_add_fancy_row(diff_render.hunk_boundary_ellipsis_row(end_text), nil, status_hunk_indent)
        status_add_fancy_row(diff_render.hunk_boundary_row(end_text, ts_context.end_segments, node_end), nil, status_hunk_indent)
      end
    end
    dr()._status_register_fold_range(hunk_key, start_line, #dr()._status.lines, false, dr()._status.lines[start_line])
    return
  end

  local start_line = #dr()._status.lines + 1
  local fold_text = nil
  dr()._status_perf_span("status_render.render_hunk.emit_rows", dr()._status and dr()._status.buf or nil, {
    file = file.filename,
    hunk_key = hunk_key,
    row_count = #rows,
  }, function()
    for row_index = 1, #rows do
      local row = rows[row_index]
      if row then
        local line = status_add_fancy_row(row, entry, status_hunk_indent)
        if row.diff_review_hunk_header then fold_text = dr()._status.lines[line] end
      end
    end
  end)
  dr()._status_register_fold_range(hunk_key, start_line, #dr()._status.lines, false, fold_text or dr()._status.lines[start_line])
  end)
end

local function status_render_file(file, entry_kind, hunk_entry_kind, file_key_override, hunk_key_builder, opts)
  return dr()._status_perf_span("status_render.render_file", dr()._status and dr()._status.buf or nil, {
    file = file and file.filename or nil,
    relpath = file and file.relpath or nil,
    entry_kind = entry_kind,
    hunk_entry_kind = hunk_entry_kind,
    force_open = opts and opts.force_open or nil,
    default_open = opts and opts.default_open or nil,
  }, function()
  opts = opts or {}
  local file_key = file_key_override or status_file_key(file.section_name, file.filename)
  local default_folded = not (opts.default_open or opts.force_open)
  local stats, stat_segments = dr()._status_file_stat_text_and_segments(file)
  local change_label, change_label_hl = dr()._status_file_change_label(file)
  local change_label_width = #"Modified"
  local padded_change_label = change_label .. string.rep(" ", math.max(0, change_label_width - #change_label))
  local line = ("%s%s %s %s"):format(string.rep(" ", status_file_indent), padded_change_label, file.relpath, stats)
  local entry = { id = file_key, kind = entry_kind or "file", file = file, default_folded = default_folded }
  local line_number = status_add_line(line, entry)
  local label_start = status_file_indent
  local label_end = label_start + #change_label
  local path_start = label_start + #padded_change_label + 1
  local stats_start = #line - #stats
  status_add_highlight(line_number, label_start, label_end, change_label_hl)
  status_add_highlight(line_number, path_start, stats_start - 1, "DiffReviewStatusPath")
  for _, stat_segment in ipairs(stat_segments) do
    status_add_highlight(
      line_number,
      stats_start + stat_segment.start_col,
      stats_start + stat_segment.end_col,
      stat_segment.hl_group
    )
  end

  local file_folded = (not opts.force_open) and status_folded(file_key, default_folded)
  local render_folded_file_body = false
  if file_folded and dr()._status and dr()._status.view_kind == "status" then
    local walkthrough = package.loaded["diff_review.walkthrough"]
    render_folded_file_body = walkthrough and walkthrough._modes and walkthrough._modes[dr()._status.buf] ~= nil
  end
  dr()._status_record_diff_file_header_state(file, entry_kind, hunk_entry_kind, file_key)
  if file_folded and not render_folded_file_body then
    dr()._status_register_fold_range(file_key, line_number, #dr()._status.lines, default_folded, dr()._status.lines[line_number])
    return
  end

  local hunks = dr()._status_perf_span("status_render.render_file.hunks", dr()._status and dr()._status.buf or nil, {
    file = file.filename,
    relpath = file.relpath,
    entry_kind = entry_kind,
  }, function()
    return status_diff_hunks_for_file(file)
  end)
  if #hunks == 0 then
    status_add_line(string.rep(" ", status_hunk_indent) .. "No textual diff", entry, "Comment")
    dr()._status_register_fold_range(file_key, line_number, #dr()._status.lines, default_folded, dr()._status.lines[line_number])
    return
  end
  local display_hunks = dr()._status_perf_span("status_render.render_file.display_hunks", dr()._status and dr()._status.buf or nil, {
    file = file.filename,
    relpath = file.relpath,
    hunk_count = #hunks,
  }, function()
    return dr()._status_display_hunks(hunks)
  end)
  dr()._status_record_diff_file_state(file, hunks, display_hunks, entry_kind, hunk_entry_kind, file_key)
  dr()._status_perf_span("status_render.render_file.render_hunks", dr()._status and dr()._status.buf or nil, {
    file = file.filename,
    relpath = file.relpath,
    hunk_count = #hunks,
    display_hunk_count = #display_hunks,
  }, function()
    local render_budget = dr()._status_file_render_row_budget()
    local forced_hunks = dr()._status_file_forced_hunk_count(file_key)
    local body_start_line = #dr()._status.lines
    for hunk_index, hunk in ipairs(display_hunks) do
      -- Size gate: keep the first hunk and any force-loaded hunks, then stop once the
      -- budget is reached or the next hunk would overshoot it, so one giant merged hunk
      -- cannot freeze the render. The deferred hunks load through the load-more row.
      if render_budget then
        local rendered_rows = #dr()._status.lines - body_start_line
        local next_estimate = dr()._status_lazy_hunk_estimate(file, hunk)
        if dr()._status_size_gate_should_defer(rendered_rows, next_estimate, hunk_index, forced_hunks, render_budget) then
          local remaining = #display_hunks - hunk_index + 1
          status_add_line(
            string.rep(" ", status_hunk_indent) .. ("… %d more change blocks — press <CR>/o to load"):format(remaining),
            {
              kind = "load_more",
              file = file,
              file_key = file_key,
              load_more_from = hunk_index,
              entry_kind = entry_kind,
              hunk_entry_kind = hunk_entry_kind,
            },
            "DiffReviewStatusFetching"
          )
          break
        end
      end
      local hunk_key = hunk_key_builder and hunk_key_builder(hunk) or status_hunk_key(file.section_name, file.filename, hunk.diff)
      status_render_hunk(file, hunk, display_hunks[hunk_index - 1], display_hunks[hunk_index + 1], hunk_entry_kind, hunk_key)
    end
  end)
  dr()._status_register_fold_range(file_key, line_number, #dr()._status.lines, default_folded, dr()._status.lines[line_number])
  end)
end

local function status_render_commit(commit, date_width)
  local commit_key = status_commit_key(commit.oid)
  local line = commit.short_oid
  local date_text = status_commit_relative_date(commit)
  local date_start_col = nil
  if date_text then
    line = line .. "  "
    date_start_col = #line
    line = line .. date_text
    if date_width > #date_text then line = line .. string.rep(" ", date_width - #date_text) end
  end
  local suffix = commit.subject or ""
  local suffix_start_col = nil
  if suffix ~= "" then
    suffix_start_col = #line + 1
    line = line .. " " .. suffix
  end
  local entry = {
    id = commit_key,
    kind = "commit",
    commit = commit,
    default_folded = true,
    commit_subject_start_col = suffix_start_col,
    commit_subject_end_col = suffix_start_col and (suffix_start_col + #suffix) or nil,
  }
  dr()._status_register_commit_source_handle(commit)
  local line_number = status_add_line(line, entry)
  status_add_highlight(line_number, 0, #commit.short_oid, "DiffReviewStatusObjectId")
  if date_start_col then
    status_add_highlight(line_number, date_start_col, date_start_col + #date_text, "DiffReviewStatusDate")
  end
  local conventional_type_end = dr()._status_conventional_commit_type_end(suffix)
  if suffix_start_col and conventional_type_end then
    status_add_highlight(line_number, suffix_start_col, suffix_start_col + conventional_type_end, "DiffReviewStatusCommitType")
  end

  if status_folded(commit_key, true) then
    dr()._status_register_fold_range(commit_key, line_number, #dr()._status.lines, true, dr()._status.lines[line_number])
    return
  end

  if commit.files_loading then
    status_add_line("...loading...", entry, "DiffReviewStatusFetching")
    dr()._status_register_fold_range(commit_key, line_number, #dr()._status.lines, true, dr()._status.lines[line_number])
    return
  end
  if commit.files_error then
    status_add_line(commit.files_error, entry, "ErrorMsg")
    dr()._status_register_fold_range(commit_key, line_number, #dr()._status.lines, true, dr()._status.lines[line_number])
    return
  end
  if not commit.files_loaded then
    status_add_line("...loading...", entry, "DiffReviewStatusFetching")
    dr()._status_register_fold_range(commit_key, line_number, #dr()._status.lines, true, dr()._status.lines[line_number])
    return
  end
  if #(commit.files or {}) == 0 then
    status_add_line("No textual diff", entry, "Comment")
    dr()._status_register_fold_range(commit_key, line_number, #dr()._status.lines, true, dr()._status.lines[line_number])
    return
  end

  for _, file in ipairs(commit.files or {}) do
    status_render_file(
      file,
      "commit_file",
      "commit_hunk",
      status_commit_file_key(commit.oid, file.filename),
      function(hunk)
        return status_commit_hunk_key(commit.oid, file.filename, hunk.diff)
      end
    )
  end
  dr()._status_register_fold_range(commit_key, line_number, #dr()._status.lines, true, dr()._status.lines[line_number])
end

local function status_render_section(section)
  return dr()._status_perf_span("status_render.render_section", dr()._status and dr()._status.buf or nil, {
    section = section and section.name or nil,
    title = section and section.title or nil,
    file_count = section and section.files and #section.files or nil,
    commit_count = section and section.commits and #section.commits or nil,
    review_count = section and section.reviews and #section.reviews or nil,
    issue_comment_count = section and section.issue_comments and #section.issue_comments or nil,
  }, function()
  local section_key = status_section_key(section.name)
  local line = dr()._status_section_heading_text(section.title, dr()._status_section_count(section))
  local section_default_folded = section.default_folded
  local entry = { id = section_key, kind = "section", section = section, default_folded = section_default_folded }
  local line_number = status_add_line(line, entry, "DiffReviewStatusHeader")
  local section_folded = status_folded(section_key, section_default_folded)
  local render_folded_commit_headers = dr()._status and dr()._status.view_kind == "status"
  if section.commits and (not section_folded or render_folded_commit_headers) then
    local date_width = 0
    for _, commit in ipairs(section.commits) do
      local date_text = status_commit_relative_date(commit)
      if date_text and #date_text > date_width then date_width = #date_text end
    end
    for _, commit in ipairs(section.commits) do
      status_render_commit(commit, date_width)
    end
    dr()._status_register_fold_range(section_key, line_number, #dr()._status.lines, section_default_folded, dr()._status.lines[line_number])
    return
  end
  if section_folded then
    dr()._status_register_fold_range(section_key, line_number, #dr()._status.lines, section_default_folded, dr()._status.lines[line_number])
    return
  end
  if section.issue_comments then
    dr()._pr_overview.render_issue_comments(section.issue_comments)
    dr()._status_register_fold_range(section_key, line_number, #dr()._status.lines, section.default_folded, dr()._status.lines[line_number])
    return
  end
  if section.reviews then
    local alignment = dr()._pr_overview.review_summary_alignment(section.reviews)
    for _, review in ipairs(section.reviews) do
      dr()._pr_overview.render_review(review, alignment)
    end
    dr()._status_register_fold_range(section_key, line_number, #dr()._status.lines, section.default_folded, dr()._status.lines[line_number])
    return
  end
  for _, file in ipairs(section.files) do
    if section.file_key_prefix then
      status_render_file(
        file,
        section.file_entry_kind or "file",
        section.hunk_entry_kind or "hunk",
        status_provider_file_key(section.file_key_prefix, file.filename),
        function(hunk)
          return status_provider_hunk_key(section.file_key_prefix, file.filename, hunk.diff)
        end
      )
    else
      status_render_file(file)
    end
  end
  dr()._status_register_fold_range(section_key, line_number, #dr()._status.lines, section.default_folded, dr()._status.lines[line_number])
  end)
end

local function status_decorate_visible(buf, first_row, last_row)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  if not (status and status.entries) then return end
  if (dr().config or config.options or config.defaults).status_cursor_prewarm == false then return end
  first_row = math.max(1, math.floor(tonumber(first_row) or 1))
  last_row = math.max(first_row, math.floor(tonumber(last_row) or first_row))
  local saved = dr()._status
  dr()._status = status
  local seen = {}
  for row = first_row, last_row do
    local entry = status.entries[row]
    if entry and (dr()._status_entry_is_file_like(entry) or dr()._status_entry_is_hunk_like(entry)) then
      local key = entry.id or (entry.file and entry.file.filename) or tostring(row)
      if not seen[key] then
        seen[key] = true
        status_prewarm_entry_syntax(entry)
      end
    end
  end
  dr()._status = saved
end

local function status_emit_row_spans(buf, namespace, row, spans, ephemeral)
  if not spans then return end
  if spans.bg then
    pcall(vim.api.nvim_buf_set_extmark, buf, namespace, row, 0, {
      end_col = spans.bg.end_col,
      hl_group = spans.bg.hl_group,
      priority = spans.bg.priority or 60,
      ephemeral = ephemeral or nil,
    })
  end
  for _, highlight in ipairs(spans.highlights or {}) do
    pcall(vim.api.nvim_buf_set_extmark, buf, namespace, row, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = highlight.priority or 90,
      ephemeral = ephemeral or nil,
    })
  end
end

local function status_decorate_rows(buf, first_row, last_row)
  local status = dr()._status_states and dr()._status_states[buf] or dr()._status
  local applied = {}
  if not (status and status.diff_row_spans) then return applied end
  local count = vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_line_count(buf) or 0
  first_row = math.max(1, math.floor(tonumber(first_row) or 1))
  last_row = math.min(count > 0 and count or last_row or first_row, math.floor(tonumber(last_row) or count))
  pcall(vim.api.nvim_buf_clear_namespace, buf, dr()._status_decorate_ns, first_row - 1, last_row)
  for line = first_row, last_row do
    local spans = status.diff_row_spans[line]
    if spans then
      status_emit_row_spans(buf, dr()._status_decorate_ns, line - 1, spans, false)
      applied[line] = spans
    end
  end
  return applied
end

local function status_register_decoration_provider()
  if dr()._status_decoration_registered then return end
  dr()._status_decoration_registered = true
  vim.api.nvim_set_decoration_provider(dr()._status_decorate_ns, {
    on_win = function(_, _, win_buf, toprow, botrow)
      local status = dr()._status_states and dr()._status_states[win_buf] or nil
      if not status then return false end
      dr()._status_schedule_decorate_visible(win_buf, toprow + 1, botrow + 1)
      return true
    end,
    on_line = function(_, _, win_buf, row)
      local status = dr()._status_states and dr()._status_states[win_buf] or nil
      local spans = status and status.diff_row_spans and status.diff_row_spans[row + 1] or nil
      if spans then
        status_emit_row_spans(win_buf, dr()._status_decorate_ns, row, spans, true)
      end
    end,
  })
end

local function status_write_rendered_buffer(buf)
  local was_rendering = vim.b[buf].diff_review_status_rendering
  vim.b[buf].diff_review_status_rendering = true
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_clear_namespace(buf, dr()._status_ns, 0, -1)
  for index, line in ipairs(dr()._status.lines or {}) do
    if type(line) ~= "string" then dr()._status.lines[index] = tostring(line or "") end
  end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, dr()._status.lines)
  vim.bo[buf].modifiable = false
  vim.b[buf].diff_review_status_rendering = was_rendering
end

local function status_apply_rendered_extmarks(buf)
  for _, line_hl in ipairs(dr()._status.line_highlights or {}) do
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, line_hl.line - 1, 0, {
      line_hl_group = line_hl.hl_group,
      priority = 80,
    })
  end
  for _, highlight in ipairs(dr()._status.highlights or {}) do
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, highlight.line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = highlight.priority or 90,
    })
  end
  for _, extmark in ipairs(dr()._status.extmarks or {}) do
    local extmark_opts = vim.tbl_extend("force", { priority = 95 }, extmark.opts)
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, extmark.line - 1, extmark.col, extmark_opts)
  end
end

local function status_after_buffer_render(buf, walkthrough)
  return dr()._status_perf_span("status_render.after_buffer_render", buf, nil, function()
    status_register_decoration_provider()
    dr()._status_perf_span("status_render.after_render.hint_bar", buf, nil, function()
      dr()._status_apply_hint_bar(buf)
    end)
    if walkthrough then
      dr()._status_perf_span("status_render.after_render.walkthrough", buf, nil, function()
        walkthrough.on_status_rendered(buf)
      end)
    end

    -- Resolve per-view after-render behavior through the registered view controller,
    -- falling back to the default (folds only) for unregistered/branch-diff views.
    local view_kind = dr()._status.view_kind or "status"
    if not dr()._diff_view_controller_model.run_hook(view_kind, "after_render", { buf = buf }) then
      dr()._status_after_render_default(buf)
    end
    dr()._gitstatus_debug.dump(buf, "status_render_loaded")
  end)
end

local function status_render_loaded(buf, target_id, fallback_line, opts, head_lines, sections)
  return dr()._status_perf_span("status_render.render_loaded", buf, {
    target_id = target_id,
    fallback_line = fallback_line,
    reuse_sections = opts and opts.reuse_sections or nil,
    head_line_count = head_lines and #head_lines or nil,
    section_count = sections and #sections or nil,
  }, function()
  opts = opts or {}
  if dr()._pr_edit.blocks_render(buf) then return end
  setup_bg_highlights()
  local previous_registry = dr()._status and dr()._status.diff_source_registry or nil
  dr()._status = dr()._status or {}
  dr()._status.buf = buf
  dr()._status.lines = {}
  dr()._status.entries = {}
  dr()._status.highlights = {}
  dr()._status.line_highlights = {}
  dr()._status.extmarks = {}
  dr()._status.diff_row_spans = {}
  dr()._status.boundary_lines = {}
  dr()._status.fold_ranges_by_id = {}
  dr()._status.fold_range_order = {}
  dr()._status.fold_text_by_start_line = {}
  dr()._status.diff_source_registry = (opts.reuse_sections and previous_registry) or dr()._diff_source_model.new_registry()
  dr()._status_configure_diff_source_policies(dr()._status.diff_source_registry)
  dr()._diff_line_content_lengths = dr()._diff_line_content_lengths or {}
  dr()._diff_line_content_lengths[buf] = {}
  dr()._clear_diff_gutter_visual_line(buf)

  local walkthrough = package.loaded["diff_review.walkthrough"]
  if walkthrough and walkthrough.status_head_lines then
    head_lines = walkthrough.status_head_lines(buf, head_lines)
  end
  dr()._status_sort_sections_for_render(buf, sections)

  local active_head_stack = {}
  local function close_active_head_parent(parent, end_line)
    if parent then
      dr()._status_register_fold_range(
        parent.id,
        parent.start_line,
        end_line,
        parent.default_folded,
        parent.fold_text
      )
    end
  end
  local function close_head_stack_to(parent_id, end_line)
    while #active_head_stack > 0 and active_head_stack[#active_head_stack].id ~= parent_id do
      close_active_head_parent(table.remove(active_head_stack), end_line)
    end
  end
  local function close_all_head_parents(end_line)
    while #active_head_stack > 0 do
      close_active_head_parent(table.remove(active_head_stack), end_line)
    end
  end

  dr()._status_perf_span("status_render.render_loaded.head_rows", buf, {
    head_line_count = #head_lines,
  }, function()
    for _, head_line in ipairs(head_lines) do
      local parent_id = head_line.parent_id
      if parent_id then
        close_head_stack_to(parent_id, #dr()._status.lines)
      else
        close_all_head_parents(#dr()._status.lines)
      end
      local entry = head_line.entry
      if entry and entry.kind == "pr_head_section" then
        entry.default_folded = head_line.default_folded == true
      end
      local line_number
      if head_line.segments then
        line_number = status_add_segment_line(head_line.segments, entry)
      else
        line_number = status_add_segment_line(head_line)
      end
      if entry and entry.kind == "pr_head_section" and entry.id then
        active_head_stack[#active_head_stack + 1] = {
          id = entry.id,
          start_line = line_number,
          default_folded = head_line.default_folded == true,
          fold_text = dr()._status.lines[line_number],
        }
      end
    end
    close_all_head_parents(#dr()._status.lines)
    status_add_line("")
  end)

  dr()._status_perf_span("status_render.render_loaded.sections", buf, {
    section_count = #sections,
  }, function()
    if #sections == 0 then
      status_add_line("No changes", nil, "Comment")
    else
      for index, section in ipairs(sections) do
        if index > 1 then
          status_add_line("")
        end
        status_render_section(section)
      end
    end
  end)

  dr()._status_perf_span("status_render.render_loaded.write_buffer", buf, {
    line_count = #dr()._status.lines,
  }, function()
    status_write_rendered_buffer(buf)
  end)
  dr()._status_perf_span("status_render.render_loaded.apply_extmarks", buf, {
    highlight_count = #(dr()._status.highlights or {}),
    line_highlight_count = #(dr()._status.line_highlights or {}),
    extmark_count = #(dr()._status.extmarks or {}),
  }, function()
    status_apply_rendered_extmarks(buf)
  end)
  dr()._status_perf_span("status_render.render_loaded.restore_cursor", buf, {
    target_id = target_id,
    fallback_line = fallback_line,
  }, function()
    status_restore_cursor(buf, target_id, fallback_line)
  end)

  dr()._status_perf_span("status_render.render_loaded.after_render", buf, nil, function()
    status_after_buffer_render(buf, walkthrough)
  end)
  end)
end

local function status_render_current_model(target_id)
  local status = dr()._status
  if not (status and status.buf and vim.api.nvim_buf_is_valid(status.buf) and status.head_lines and status.sections) then
    return
  end
  status.fancy_rows = {}
  status_render_loaded(status.buf, target_id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true }, status.head_lines, status.sections)
end

-- Public surface (init reaches these via seams).
M.status_decorate_visible = status_decorate_visible
M.status_emit_row_spans = status_emit_row_spans
M.status_decorate_rows = status_decorate_rows
M.status_register_decoration_provider = status_register_decoration_provider
M.status_write_rendered_buffer = status_write_rendered_buffer
M.status_apply_rendered_extmarks = status_apply_rendered_extmarks
M.status_after_buffer_render = status_after_buffer_render
M.status_render_hunk = status_render_hunk
M.status_render_file = status_render_file
M.status_render_commit = status_render_commit
M.status_render_section = status_render_section
M.status_render_loaded = status_render_loaded
M.status_render_current_model = status_render_current_model

return M
