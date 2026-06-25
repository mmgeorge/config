--- Estimates render cost for the status diff views: how many rows a file's hunks and inline
--- comments will occupy, which raw hunks merge into one display window, and whether a file exceeds
--- the size-gate budget and should defer its body render.
---
--- Reads the parse helpers, hunk model, diff source, review/pr-overview comment data, and config
--- through the init module via dr().

local config = require("diff_review.infra.config")

--- Resolve the init module lazily so estimation can reach the shared parse/hunk/source/review seams
--- without a load-time circular require.
local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")

local diff_parse = require("diff_review.render.diff_parse")

local M = {}

---@param hunk DiffReviewHunk
---@return integer? first_line
---@return integer? last_line
function M._status_hunk_changed_current_range(hunk)
  local first_line = nil
  local last_line = nil
  for _, block in ipairs(diff_parse.parse_unified_diff(tostring(hunk.diff or ""))) do
    for _, parsed_hunk in ipairs(block.hunks or {}) do
      parsed_hunk = diff_parse.parse_hunk_body(parsed_hunk)
      local line_by_position = dr()._hunk_current_line_by_position(parsed_hunk)
      for _, parsed_line in ipairs(parsed_hunk.lines or {}) do
        if parsed_line.prefix == "+" or parsed_line.prefix == "-" then
          local current_line = dr()._hunk_parsed_line_current_line(parsed_line, line_by_position)
          if current_line then
            first_line = math.min(first_line or current_line, current_line)
            last_line = math.max(last_line or current_line, current_line)
          end
        end
      end
    end
  end
  if first_line then return first_line, last_line or first_line end
  if hunk.pos then return hunk.pos, hunk.pos end
  return nil, nil
end

---@param hunk DiffReviewHunk
---@return integer? first_line
---@return integer? last_line
function M._status_hunk_virtual_display_range(hunk)
  local first_line, last_line = dr()._status_hunk_changed_current_range(hunk)
  if not (first_line and last_line) then return nil, nil end
  local padding_limit = dr()._hunk_context_padding_limit()
  return math.max(1, first_line - padding_limit), last_line + padding_limit
end

---@param diff_text string?
---@return string[] header_lines
---@return string[][] hunk_sections
function M._status_hunk_diff_parts(diff_text)
  local header_lines = {}
  local hunk_sections = {}
  local current_section = nil ---@type string[]?
  for _, line in ipairs(vim.split(tostring(diff_text or ""), "\n", { plain = true })) do
    if line:match("^@@ ") then
      current_section = { line }
      hunk_sections[#hunk_sections + 1] = current_section
    elseif current_section then
      current_section[#current_section + 1] = line
    else
      header_lines[#header_lines + 1] = line
    end
  end
  return header_lines, hunk_sections
end

---@param left DiffReviewHunk?
---@param right DiffReviewHunk?
---@return boolean
function M._status_hunks_should_display_together(left, right)
  if not (left and right) then return false end
  if left.file ~= right.file then return false end
  if left.staged ~= right.staged then return false end
  if left.section_name ~= right.section_name then return false end
  local _, left_display_end = dr()._status_hunk_virtual_display_range(left)
  local right_display_start = dr()._status_hunk_virtual_display_range(right)
  if not (left_display_end and right_display_start) then return false end
  return right_display_start <= left_display_end + 1
end

---@param hunks DiffReviewHunk[]
---@return DiffReviewHunk
function M._status_combine_display_hunks(hunks)
  if #hunks == 1 then return hunks[1] end
  local header_lines = dr()._status_hunk_diff_parts(hunks[1].diff)
  local combined_lines = vim.deepcopy(header_lines)
  local added_count = 0
  local removed_count = 0
  for _, hunk in ipairs(hunks) do
    local _, hunk_sections = dr()._status_hunk_diff_parts(hunk.diff)
    for _, section in ipairs(hunk_sections) do
      vim.list_extend(combined_lines, section)
    end
    added_count = added_count + (hunk.added or 0)
    removed_count = removed_count + (hunk.removed or 0)
  end
  local combined_hunk = vim.deepcopy(hunks[1])
  combined_hunk.diff = table.concat(combined_lines, "\n")
  combined_hunk.added = added_count
  combined_hunk.removed = removed_count
  combined_hunk.raw_hunks = vim.deepcopy(hunks)
  return combined_hunk
end

---@param hunks DiffReviewHunk[]
---@return DiffReviewHunk[]
function M._status_display_hunks(hunks)
  return dr()._status_perf_span("status.display_hunks", session.status and session.status.buf or nil, {
    hunk_count = #(hunks or {}),
  }, function()
    local display_hunks = {}
    local current_group = {} ---@type DiffReviewHunk[]
    local function flush_group()
      if #current_group == 0 then return end
      display_hunks[#display_hunks + 1] = dr()._status_combine_display_hunks(current_group)
      current_group = {}
    end

    for _, hunk in ipairs(hunks or {}) do
      if #current_group == 0 or dr()._status_hunks_should_display_together(current_group[#current_group], hunk) then
        current_group[#current_group + 1] = hunk
      else
        flush_group()
        current_group[#current_group + 1] = hunk
      end
    end
    flush_group()
    return display_hunks
  end)
end
---@param diff_line string?
---@return boolean
function M._status_lazy_diff_body_line(diff_line)
  return dr()._diff_source_model.diff_body_line(diff_line)
end

---@param lines string[]
---@return integer added
---@return integer removed
function M._status_lazy_diff_stats(lines)
  return dr()._diff_source_model.diff_stats(lines)
end

---@param file DiffReviewStatusFile
---@param comment table
---@return boolean
function M._status_lazy_comment_matches_file(file, comment)
  if not (file and comment) then return false end
  local targets = {
    comment.abs_file,
    comment.path,
  }
  for _, target in ipairs(targets) do
    if target and target ~= "" then
      local normalized = vim.fs.normalize(tostring(target))
      if normalized == vim.fs.normalize(tostring(file.filename or "")) then return true end
      if normalized == vim.fs.normalize(tostring(file.relpath or "")) then return true end
    end
  end
  return false
end

---@param comment table
---@return integer
function M._status_lazy_comment_row_estimate(comment)
  if not comment or comment.local_state == "deleted" then return 0 end
  if comment.review_folded == true then return 1 end
  local body_count = #dr()._review.comment_body_lines(comment.body or "")
  local reply_count = 0
  for _, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
    reply_count = reply_count + 1 + #dr()._review.comment_body_lines(reply.body or "")
  end
  return 2 + body_count + reply_count
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return integer
function M._status_lazy_hunk_comment_estimate(file, hunk)
  local total = 0
  local seen = {}
  local sources = {
    file and file.pr_comments or {},
    file and file.pr_review_comments or {},
    session.status and session.status.review_comments or {},
  }
  for _, comments in ipairs(sources) do
    for _, comment in ipairs(type(comments) == "table" and comments or {}) do
      local key = comment.local_id or comment.remote_node_id or comment.remote_id or comment.id or comment
      if not seen[key]
        and dr()._status_lazy_comment_matches_file(file, comment)
        and dr()._pr_overview.hunk_contains_comment(hunk, comment) then
        seen[key] = true
        total = total + dr()._status_lazy_comment_row_estimate(comment)
      end
    end
  end
  return total
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return integer
function M._status_lazy_hunk_estimate(file, hunk)
  local lazy_estimate = tonumber(hunk and hunk.lazy_estimate)
  if lazy_estimate then return math.max(1, lazy_estimate + dr()._status_lazy_hunk_comment_estimate(file, hunk)) end
  local count = 0
  local in_hunk = false
  for _, line in ipairs(vim.split(tostring(hunk and hunk.diff or ""), "\n", { plain = true })) do
    if line:match("^@@ ") then
      count = count + 1
      in_hunk = true
    elseif in_hunk and dr()._status_lazy_diff_body_line(line) then
      count = count + 1
    end
  end
  return math.max(1, count + dr()._status_lazy_hunk_comment_estimate(file, hunk))
end


--- Resolve the per-file diff-body row budget for the size gate, or nil when disabled.
--- Bounds how many real rows a single file body renders up front so a huge diff does
--- not freeze the render; the rest loads on demand through its load-more row.
---@return integer?
function M._status_file_render_row_budget()
  local options = dr().config or config.options or config.defaults
  local base = tonumber(options.status_diff_viewport_threshold) or 0
  if base <= 0 then return nil end
  return base
end

--- Count the leading display hunks a file body must render regardless of the row
--- budget, so activating its load-more row always reveals more of a huge diff.
---@param file_key string
---@return integer
function M._status_file_forced_hunk_count(file_key)
  return (session.status and session.status.file_render_limits and session.status.file_render_limits[file_key]) or 0
end

--- Decide whether the size gate should defer the next hunk behind a load-more row.
--- Always render the first hunk and any force-loaded hunks, then stop once the budget
--- is reached or the next hunk would overshoot it, so one giant hunk cannot freeze the
--- render while load-more still guarantees progress.
---@param rendered_rows integer rows already emitted for this file body
---@param next_estimate integer estimated rendered rows of the next hunk
---@param hunk_index integer 1-based index of the next hunk
---@param forced_hunks integer leading hunks to render regardless of the budget
---@param budget integer? row budget, or nil when the gate is disabled
---@return boolean
function M._status_size_gate_should_defer(rendered_rows, next_estimate, hunk_index, forced_hunks, budget)
  if not budget then return false end
  if hunk_index <= 1 or hunk_index <= forced_hunks then return false end
  return rendered_rows >= budget or (rendered_rows + (next_estimate or 0)) > budget
end

return M
