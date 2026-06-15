-- Guided review walkthrough for the DiffReviewStatus buffer. An LLM writes
-- .walkthrough.json at the repo root (see walkthrough.schema.json next to this
-- file); `ow` in the status buffer shows the summary, then z/y step backward/
-- forward through the referenced regions inside the inline diff, with the
-- author's comment rendered as an inline virt_lines box above each region.
-- Positions refer to the NEW
-- (post-change) file; the document records the HEAD sha it was generated
-- against so stale walkthroughs degrade to best-effort jumps with a note.

---@class DiffReviewWalkthroughPosition
---@field line integer 1-based line in the new file
---@field col integer 1-based column

---@class DiffReviewWalkthroughStep
---@field file string repo-relative path, forward slashes
---@field start_pos DiffReviewWalkthroughPosition
---@field end_pos DiffReviewWalkthroughPosition
---@field comment string
---@field title? string
---@field part_index integer
---@field step_index integer
---@field label string
---@field part_title string
---@field group_title string
---@field item_action string
---@field item_title string
---@field item_note string
---@field item_path string

---@class DiffReviewWalkthroughItem
---@field action string
---@field title string
---@field note string
---@field steps DiffReviewWalkthroughStep[]
---@field children DiffReviewWalkthroughItem[]

---@class DiffReviewWalkthroughGroup
---@field title string
---@field items DiffReviewWalkthroughItem[]

---@class DiffReviewWalkthroughPart
---@field title string
---@field groups DiffReviewWalkthroughGroup[]

---@class DiffReviewWalkthroughStepContext
---@field part_index integer
---@field step_index integer
---@field part_title string
---@field group_title string
---@field item_action string
---@field item_title string
---@field item_note string
---@field item_path string

---@class DiffReviewWalkthroughDoc
---@field version integer
---@field overview string
---@field root string
---@field summary string
---@field commit string full sha the walkthrough was generated against
---@field parts DiffReviewWalkthroughPart[]
---@field steps DiffReviewWalkthroughStep[]

---@alias DiffReviewWalkthroughMatch "exact"|"nearest"|"file_only"|"missing"

---@class DiffReviewWalkthroughTarget
---@field match DiffReviewWalkthroughMatch
---@field start_row? integer 1-based status-buffer row
---@field end_row? integer

---@class DiffReviewWalkthroughHost narrow interface handed over by diff_review
---@field buf integer
---@field cwd fun(): string?
---@field get_state fun(): table? current status state (lines, entries, sections)
---@field file_key fun(section_name: string, filename: string): string
---@field hunk_key fun(section_name: string, filename: string, diff?: string): string
---@field set_folded fun(key: string, folded: boolean)
---@field rerender fun() synchronous reuse_sections re-render
---@field git_list_async fun(command: string[], cb: fun(output: string[], code: integer))

---@alias DiffReviewWalkthroughReader fun(path: string): string?

---@class DiffReviewWalkthroughMode
---@field host DiffReviewWalkthroughHost
---@field doc DiffReviewWalkthroughDoc
---@field index integer 0 = summary popup, 1..n = steps
---@field stale boolean
---@field head_sha? string
---@field saved_maps table<string, table> maparg dicts to restore on stop

---@class DiffReviewWalkthroughModule
---@field _modes table<integer, DiffReviewWalkthroughMode>
---@field _reader DiffReviewWalkthroughReader?
---@field _ns integer
local M = {
  _modes = {},
  _ns = vim.api.nvim_create_namespace("diff_review_walkthrough"),
}

local nav_keys = { "z", "y", "q", "<Esc>" }

---@param reader DiffReviewWalkthroughReader?
function M.set_reader(reader)
  M._reader = reader
end

function M.reset_reader()
  M._reader = nil
end

---@param path string
---@return string? contents
local function read_file(path)
  local file = io.open(path, "r")
  if not file then return nil end
  local data = file:read("*a")
  file:close()
  return data
end

---@param message string
---@param level integer
local function notify(message, level)
  vim.notify(message, level, { title = "DiffReview walkthrough" })
end

-- ─── document loading ────────────────────────────────────────────────────────

---@param value any
---@return DiffReviewWalkthroughPosition?
local function parse_position(value)
  if type(value) ~= "table" then return nil end
  local line = tonumber(value.line)
  local col = tonumber(value.col)
  if not line or line < 1 or not col or col < 1 then return nil end
  return { line = math.floor(line), col = math.floor(col) }
end

---@param value any
---@return boolean
local function is_non_empty_string(value)
  return type(value) == "string" and vim.trim(value) ~= ""
end

local action_width = 6
local action_order = { "Add", "Update", "Move", "Remove", "Split" }
local valid_item_actions = {
  Add = true,
  Update = true,
  Move = true,
  Remove = true,
  Split = true,
}
local action_highlights = {
  Add = "DiffReviewWalkthroughActionAdd",
  Update = "DiffReviewWalkthroughActionUpdate",
  Move = "DiffReviewWalkthroughActionMove",
  Remove = "DiffReviewWalkthroughActionRemove",
  Split = "DiffReviewWalkthroughActionSplit",
}

local action_display_labels = {
  Update = "Modify",
}

---@param is_last boolean
---@return string
local function tree_branch(is_last)
  return is_last and "└── " or "├── "
end

---@param is_last boolean
---@return string
local function tree_continuation(is_last)
  return is_last and "    " or "│   "
end

---@param is_last boolean
---@return string
local function tree_note_continuation(is_last)
  return is_last and "      " or "│     "
end

---@param action string
---@return string
local function format_action(action)
  return ("%-" .. action_width .. "s"):format(action_display_labels[action] or action)
end

---@param action string
---@param title string
---@return string
local function item_context_label(action, title)
  return ("%s %s"):format(action_display_labels[action] or action, title)
end

---@param labels string[]
---@return string[]
local function copy_labels(labels)
  local copy = {}
  for index, label in ipairs(labels) do
    copy[index] = label
  end
  return copy
end

---@param labels string[]
---@param label string
---@return string[]
local function append_label(labels, label)
  local copy = copy_labels(labels)
  copy[#copy + 1] = label
  return copy
end

---@param lines string[]
---@param item DiffReviewWalkthroughItem
---@param prefix string
---@param is_last boolean
local function append_summary_item(lines, item, prefix, is_last)
  lines[#lines + 1] = prefix .. tree_branch(is_last) .. format_action(item.action) .. " " .. item.title
  local children = item.children or {}
  local child_prefix = prefix .. tree_continuation(is_last)
  if #children > 0 then
    lines[#lines + 1] = child_prefix .. "│     " .. item.note
    for child_index, child in ipairs(children) do
      append_summary_item(lines, child, child_prefix, child_index == #children)
    end
  else
    lines[#lines + 1] = prefix .. tree_note_continuation(is_last) .. item.note
  end
end

---@param overview string
---@param _root string
---@param parts DiffReviewWalkthroughPart[]
---@return string
local function build_summary(overview, _root, parts)
  local lines = {}
  vim.list_extend(lines, vim.split(vim.trim(overview), "\n", { plain = true }))
  lines[#lines + 1] = ""
  for part_index, part in ipairs(parts) do
    if part_index > 1 then lines[#lines + 1] = "" end
    lines[#lines + 1] = ("%d. %s"):format(part_index, part.title)
    for group_index, group in ipairs(part.groups) do
      local group_is_last = group_index == #part.groups
      local group_prefix = "   "
      lines[#lines + 1] = group_prefix .. tree_branch(group_is_last) .. group.title
      local item_prefix = group_prefix .. tree_continuation(group_is_last)
      for item_index, item in ipairs(group.items) do
        local item_is_last = item_index == #group.items
        append_summary_item(lines, item, item_prefix, item_is_last)
      end
    end
  end

  return table.concat(lines, "\n")
end

---@param raw_step any
---@param error_prefix string
---@param context DiffReviewWalkthroughStepContext
---@return DiffReviewWalkthroughStep? step
---@return string? error
local function parse_step(raw_step, error_prefix, context)
  if type(raw_step) ~= "table" or not is_non_empty_string(raw_step.file) then
    return nil, error_prefix .. ": missing \"file\""
  end
  if not is_non_empty_string(raw_step.comment) then
    return nil, error_prefix .. ": missing \"comment\""
  end

  local start_pos = parse_position(raw_step.start)
  local end_pos = parse_position(raw_step["end"]) or start_pos
  if not start_pos then
    return nil, error_prefix .. ": missing or invalid \"start\" position"
  end
  ---@cast end_pos DiffReviewWalkthroughPosition
  if end_pos.line < start_pos.line then
    end_pos = start_pos
  end

  return {
    file = (raw_step.file:gsub("\\", "/")),
    start_pos = start_pos,
    end_pos = end_pos,
    comment = raw_step.comment,
    title = type(raw_step.title) == "string" and raw_step.title or nil,
    part_index = context.part_index,
    step_index = context.step_index,
    label = ("Part %d.%d"):format(context.part_index, context.step_index),
    part_title = context.part_title,
    group_title = context.group_title,
    item_action = context.item_action,
    item_title = context.item_title,
    item_note = context.item_note,
    item_path = context.item_path,
  }
end

---@param raw_item any
---@param error_prefix string
---@param base_context table
---@param parent_labels string[]
---@param part_step_index integer
---@param all_steps DiffReviewWalkthroughStep[]
---@return DiffReviewWalkthroughItem? item
---@return integer part_step_index
---@return string? error
local function parse_item(raw_item, error_prefix, base_context, parent_labels, part_step_index, all_steps)
  if type(raw_item) ~= "table" then
    return nil, part_step_index, error_prefix .. ": missing item object"
  end
  if type(raw_item.action) ~= "string" or not valid_item_actions[raw_item.action] then
    return nil, part_step_index, error_prefix .. ": missing or invalid \"action\""
  end
  if not is_non_empty_string(raw_item.title) then
    return nil, part_step_index, error_prefix .. ": missing \"title\""
  end
  if not is_non_empty_string(raw_item.note) then
    return nil, part_step_index, error_prefix .. ": missing \"note\""
  end

  local item_labels = append_label(parent_labels, item_context_label(raw_item.action, raw_item.title))
  ---@type DiffReviewWalkthroughStep[]
  local item_steps = {}
  if raw_item.steps ~= nil then
    if type(raw_item.steps) ~= "table" or #raw_item.steps == 0 then
      return nil, part_step_index, error_prefix .. ": \"steps\" must be a non-empty array when present"
    end
    for step_offset, raw_step in ipairs(raw_item.steps) do
      part_step_index = part_step_index + 1
      local step, parse_error = parse_step(raw_step, ("%s step %d"):format(error_prefix, step_offset), {
        part_index = base_context.part_index,
        step_index = part_step_index,
        part_title = base_context.part_title,
        group_title = base_context.group_title,
        item_action = raw_item.action,
        item_title = raw_item.title,
        item_note = raw_item.note,
        item_path = table.concat(item_labels, " / "),
      })
      if not step then return nil, part_step_index, parse_error end
      item_steps[#item_steps + 1] = step
      all_steps[#all_steps + 1] = step
    end
  end

  ---@type DiffReviewWalkthroughItem[]
  local children = {}
  if raw_item.children ~= nil then
    if type(raw_item.children) ~= "table" or #raw_item.children == 0 then
      return nil, part_step_index, error_prefix .. ": \"children\" must be a non-empty array when present"
    end
    for child_index, raw_child in ipairs(raw_item.children) do
      local child, next_step_index, child_error =
        parse_item(raw_child, ("%s child %d"):format(error_prefix, child_index), base_context, item_labels,
          part_step_index, all_steps)
      if not child then return nil, next_step_index, child_error end
      part_step_index = next_step_index
      children[#children + 1] = child
    end
  end

  if #item_steps == 0 and #children == 0 then
    return nil, part_step_index, error_prefix .. ": missing \"steps\" or \"children\""
  end

  return {
    action = raw_item.action,
    title = raw_item.title,
    note = raw_item.note,
    steps = item_steps,
    children = children,
  }, part_step_index, nil
end

--- Tolerantly validate and normalize a decoded walkthrough document.
---@param decoded any
---@return DiffReviewWalkthroughDoc? doc
---@return string? error
local function parse_doc(decoded)
  if type(decoded) ~= "table" then
    return nil, "document is not a JSON object"
  end
  if tonumber(decoded.version) ~= 4 then
    return nil, "unsupported \"version\" (expected 4)"
  end
  if not is_non_empty_string(decoded.overview) then
    return nil, "missing or empty \"overview\""
  end
  if not is_non_empty_string(decoded.root) then
    return nil, "missing or empty \"root\""
  end
  if type(decoded.commit) ~= "string" or #decoded.commit ~= 40 or not decoded.commit:match("^%x+$") then
    return nil, "missing or invalid \"commit\" (expected the full HEAD sha)"
  end
  if type(decoded.parts) ~= "table" or #decoded.parts == 0 then
    return nil, "missing or empty \"parts\""
  end

  ---@type DiffReviewWalkthroughStep[]
  local steps = {}
  ---@type DiffReviewWalkthroughPart[]
  local parts = {}
  for part_index, raw_part in ipairs(decoded.parts) do
    local part_prefix = ("part %d"):format(part_index)
    if type(raw_part) ~= "table" or not is_non_empty_string(raw_part.title) then
      return nil, part_prefix .. ": missing \"title\""
    end
    if type(raw_part.groups) ~= "table" or #raw_part.groups == 0 then
      return nil, part_prefix .. ": missing or empty \"groups\""
    end

    local part_step_index = 0
    ---@type DiffReviewWalkthroughGroup[]
    local groups = {}
    for group_index, raw_group in ipairs(raw_part.groups) do
      local group_prefix = ("%s group %d"):format(part_prefix, group_index)
      if type(raw_group) ~= "table" or not is_non_empty_string(raw_group.title) then
        return nil, group_prefix .. ": missing \"title\""
      end
      if type(raw_group.items) ~= "table" or #raw_group.items == 0 then
        return nil, group_prefix .. ": missing or empty \"items\""
      end

      ---@type DiffReviewWalkthroughItem[]
      local items = {}
      for item_index, raw_item in ipairs(raw_group.items) do
        local item_prefix = ("%s item %d"):format(group_prefix, item_index)
        local item, next_step_index, item_error = parse_item(raw_item, item_prefix, {
          part_index = part_index,
          part_title = raw_part.title,
          group_title = raw_group.title,
        }, {}, part_step_index, steps)
        if not item then return nil, item_error end
        part_step_index = next_step_index
        items[#items + 1] = item
      end
      groups[#groups + 1] = {
        title = raw_group.title,
        items = items,
      }
    end

    if part_step_index == 0 then
      return nil, part_prefix .. ": no walkthrough steps"
    end

    parts[#parts + 1] = {
      title = raw_part.title,
      groups = groups,
    }
  end

  return {
    version = 4,
    overview = decoded.overview,
    root = decoded.root,
    summary = build_summary(decoded.overview, decoded.root, parts),
    commit = decoded.commit,
    parts = parts,
    steps = steps,
  }
end

-- ─── step -> rendered row resolution ─────────────────────────────────────────

local section_preference = { "unstaged", "untracked", "staged" }

--- Whether a rendered file path refers to the step's repo-relative file.
--- Status entries may carry absolute paths, so match on a path-boundary
--- suffix as well as equality.
---@param candidate string?
---@param step_file string normalized repo-relative path
---@return boolean
local function matches_file(candidate, step_file)
  if type(candidate) ~= "string" then return false end
  candidate = candidate:gsub("\\", "/")
  return candidate == step_file or candidate:sub(-(#step_file + 1)) == "/" .. step_file
end

---@param entry table?
---@param step_file string
---@return boolean
local function entry_matches_file(entry, step_file)
  local file = entry and entry.file
  if not file then return false end
  return matches_file(file.relpath, step_file) or matches_file(file.filename, step_file)
end

--- Resolve a step against the rendered status state. Exposed for tests.
---@param state table status state with lines/entries
---@param step DiffReviewWalkthroughStep
---@return DiffReviewWalkthroughTarget
function M.resolve_step(state, step)
  local lines = state and state.lines or {}
  local entries = state and state.entries or {}

  ---@type table<string, { rows: { row: integer, new_line: integer }[], file_row: integer? }>
  local by_section = {}
  for row = 1, #lines do
    local entry = entries[row]
    if entry_matches_file(entry, step.file) then
      local section_name = entry.file.section_name or "unstaged"
      local section = by_section[section_name]
      if not section then
        section = { rows = {} }
        by_section[section_name] = section
      end
      if entry.kind == "file" and not section.file_row then
        section.file_row = row
      end
      local diff_line = entry.diff_line
      if diff_line and diff_line.side == "right" and type(diff_line.line) == "number" then
        section.rows[#section.rows + 1] = { row = row, new_line = diff_line.line }
      end
    end
  end

  -- A section containing rendered rows INSIDE the step's region wins,
  -- preferring sections in order. This finds regions that only live in
  -- staged hunks (partially staged files), and regions split across hunk
  -- boundaries anchor at the first hunk row that falls inside the region
  -- even when start.line itself is not rendered.
  for _, section_name in ipairs(section_preference) do
    local section = by_section[section_name]
    if section then
      local start_row, end_row = nil, nil
      for _, candidate in ipairs(section.rows) do
        if candidate.new_line >= step.start_pos.line and candidate.new_line <= step.end_pos.line then
          start_row = math.min(start_row or candidate.row, candidate.row)
          end_row = math.max(end_row or candidate.row, candidate.row)
        end
      end
      if start_row then
        return { match = "exact", start_row = start_row, end_row = end_row }
      end
    end
  end

  -- No rendered row inside the region anywhere: globally nearest row across
  -- all sections (ties keep the preferred section's row).
  local best = nil
  for _, section_name in ipairs(section_preference) do
    local section = by_section[section_name]
    for _, candidate in ipairs(section and section.rows or {}) do
      local distance = math.abs(candidate.new_line - step.start_pos.line)
      if not best or distance < best.distance then
        best = { row = candidate.row, distance = distance }
      end
    end
  end
  if best then
    return { match = "nearest", start_row = best.row, end_row = best.row }
  end

  for _, section_name in ipairs(section_preference) do
    local section = by_section[section_name]
    if section and section.file_row then
      return { match = "file_only", start_row = section.file_row, end_row = section.file_row }
    end
  end

  return { match = "missing" }
end

--- Unfold the step's file (and its hunks) when no diff rows are rendered yet,
--- then re-render synchronously so resolution can retry once.
---@param mode DiffReviewWalkthroughMode
---@param step DiffReviewWalkthroughStep
---@return boolean expanded whether anything was unfolded
local function ensure_expanded(mode, step)
  local state = mode.host.get_state()
  local sections = state and state.sections or {}
  local expanded = false
  for _, section in ipairs(sections) do
    for _, file in ipairs(section.files or {}) do
      if matches_file(file.relpath, step.file) or matches_file(file.filename, step.file) then
        mode.host.set_folded("section:" .. (file.section_name or section.name), false)
        mode.host.set_folded(mode.host.file_key(file.section_name, file.filename), false)
        for _, hunk in ipairs(file.hunks or {}) do
          mode.host.set_folded(mode.host.hunk_key(file.section_name, file.filename, hunk.diff), false)
        end
        expanded = true
      end
    end
  end
  if expanded then
    mode.host.rerender()
  end
  return expanded
end

-- ─── floats ──────────────────────────────────────────────────────────────────

---@param text string
---@param width integer
---@return string[] lines
local function wrap_text(text, width)
  local wrapped = {}
  for _, paragraph in ipairs(vim.split(text, "\n", { plain = true })) do
    if vim.trim(paragraph) == "" then
      wrapped[#wrapped + 1] = ""
    else
      local line = ""
      for word in paragraph:gmatch("%S+") do
        if line == "" then
          line = word
        elseif #line + 1 + #word <= width then
          line = line .. " " .. word
        else
          wrapped[#wrapped + 1] = line
          line = word
        end
      end
      if line ~= "" then wrapped[#wrapped + 1] = line end
    end
  end
  if #wrapped == 0 then wrapped[1] = "" end
  return wrapped
end

---@param target DiffReviewWalkthroughTarget
---@param stale boolean
---@return string? note
local function staleness_note(target, stale)
  if target.match == "nearest" then
    return "(stale: position approximated)"
  end
  if target.match == "file_only" then
    return "(stale: no matching hunk - showing file)"
  end
  if target.match == "missing" then
    return "(stale: file not in current diff)"
  end
  if stale then
    return "(walkthrough predates HEAD - positions may be stale)"
  end
  return nil
end

--- Render the per-step comment as an inline virt_lines box above the region:
--- it scrolls with the buffer and pushes real lines down instead of
--- overlapping them. Everything lives in the walkthrough namespace, so a
--- namespace clear removes the box together with the region highlight.
---@param mode DiffReviewWalkthroughMode
---@param step DiffReviewWalkthroughStep
---@param target DiffReviewWalkthroughTarget
---@param win integer
local function render_comment_box(mode, step, target, win)
  local buf = mode.host.buf
  local win_width = vim.api.nvim_win_get_width(win)
  local inner_width = math.max(30, math.min(72, win_width - 8))

  ---@type { text: string, hl: string }[][] content rows (without borders)
  local content = {}
  for _, line in ipairs(wrap_text(step.part_title, inner_width - 2)) do
    content[#content + 1] = { text = line, hl = "Title" }
  end
  local item_context = ("%s / %s"):format(
    step.group_title,
    step.item_path ~= "" and step.item_path or item_context_label(step.item_action, step.item_title)
  )
  for _, line in ipairs(wrap_text(item_context, inner_width - 2)) do
    content[#content + 1] = { text = line, hl = "DiffReviewStatusHint" }
  end
  for _, line in ipairs(wrap_text(step.item_note, inner_width - 2)) do
    content[#content + 1] = { text = line, hl = "DiffReviewStatusHint" }
  end
  content[#content + 1] = { text = "", hl = "DiffReviewWalkthroughComment" }
  for _, line in ipairs(wrap_text(step.comment, inner_width - 2)) do
    content[#content + 1] = { text = line, hl = "DiffReviewWalkthroughComment" }
  end
  local note = staleness_note(target, mode.stale)
  if note then
    content[#content + 1] = { text = "", hl = "DiffReviewWalkthroughComment" }
    content[#content + 1] = { text = note, hl = "DiffReviewWalkthroughStale" }
  end
  content[#content + 1] = { text = "", hl = "DiffReviewWalkthroughComment" }
  content[#content + 1] = { text = "[z] back  [y] next  [q] quit", hl = "DiffReviewStatusHint" }

  -- Header: "Part 3.1 - title" left-aligned, the file basename right-aligned.
  local heading_title = step.title or step.item_title
  local heading = (" %s%s "):format(step.label, heading_title and (" - " .. heading_title) or "")
  local basename = " " .. (step.file:match("([^/]+)$") or step.file) .. " "
  for _, row in ipairs(content) do
    inner_width = math.max(inner_width, vim.fn.strdisplaywidth(row.text) + 2)
  end
  local heading_width = vim.fn.strdisplaywidth(heading)
  local basename_width = vim.fn.strdisplaywidth(basename)
  inner_width = math.max(inner_width, heading_width + basename_width + 3)

  local pad = "  "
  local virt_lines = {}
  virt_lines[#virt_lines + 1] = {
    { pad .. "╭─", "FloatBorder" },
    { heading, "Title" },
    { ("─"):rep(math.max(inner_width - heading_width - basename_width - 2, 0)), "FloatBorder" },
    { basename, "DiffReviewStatusPath" },
    { "─╮", "FloatBorder" },
  }
  for _, row in ipairs(content) do
    local fill = (" "):rep(math.max(inner_width - vim.fn.strdisplaywidth(row.text) - 2, 0))
    virt_lines[#virt_lines + 1] = {
      { pad .. "│ ", "FloatBorder" },
      { row.text .. fill, row.hl },
      { " │", "FloatBorder" },
    }
  end
  virt_lines[#virt_lines + 1] = { { pad .. "╰" .. ("─"):rep(inner_width) .. "╯", "FloatBorder" } }

  pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, (target.start_row or 1) - 1, 0, {
    virt_lines = virt_lines,
    virt_lines_above = true,
  })
end

-- ─── mode lifecycle ──────────────────────────────────────────────────────────

--- Summary text mixes prose with preformatted content (ASCII code-flow
--- diagrams): lines that fit are preserved verbatim (indentation included);
--- only overlong lines fall back to word wrapping.
---@param text string
---@param width integer
---@return string[] lines
local function summary_lines(text, width)
  local lines = {}
  for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
    if vim.fn.strdisplaywidth(line) <= width then
      lines[#lines + 1] = line
    else
      vim.list_extend(lines, wrap_text(line, width))
    end
  end
  if #lines == 0 then lines[1] = "" end
  return lines
end

---@param line string
---@return integer? start_col 0-based byte column
---@return integer? end_col 0-based byte column, exclusive
---@return string? action
local function summary_action_range(line)
  for _, action in ipairs(action_order) do
    local padded_action = format_action(action) .. " "
    local start_byte = line:find(padded_action, 1, true)
    local prefix = start_byte and line:sub(1, start_byte - 1) or ""
    if start_byte and (vim.endswith(prefix, "├── ") or vim.endswith(prefix, "└── ")) then
      local start_col = start_byte - 1
      return start_col, start_col + #action, action
    end
  end
  return nil, nil, nil
end

---@param line string
---@param action_start_col integer
---@return integer? start_col 0-based byte column
---@return integer? end_col 0-based byte column, exclusive
local function summary_item_title_range(line, action_start_col)
  local title_start_col = action_start_col + action_width + 1
  if title_start_col >= #line then return nil, nil end
  return title_start_col, #line
end

---@param buf integer
---@param lines string[]
local function apply_summary_highlights(buf, lines)
  for row, line in ipairs(lines) do
    local start_col, end_col, action = summary_action_range(line)
    local hl_group = action and action_highlights[action] or nil
    if start_col and end_col and hl_group then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, start_col, {
        end_col = end_col,
        hl_group = hl_group,
      })
      local title_start_col, title_end_col = summary_item_title_range(line, start_col)
      if title_start_col and title_end_col then
        pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, title_start_col, {
          end_col = title_end_col,
          hl_group = "DiffReviewWalkthroughItemTitle",
        })
      end
    end
  end
end

---@param mode DiffReviewWalkthroughMode
---@param target DiffReviewWalkthroughTarget
local function apply_region_highlight(mode, target)
  local buf = mode.host.buf
  vim.api.nvim_buf_clear_namespace(buf, M._ns, 0, -1)
  if not target.start_row or target.match == "missing" then return end
  local end_row = target.end_row or target.start_row
  for row = target.start_row, end_row do
    pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, 0, {
      line_hl_group = "DiffReviewWalkthroughRegion",
      priority = 250,
    })
  end
end

---@param mode DiffReviewWalkthroughMode
---@param index integer
local function show_step(mode, index)
  local buf = mode.host.buf
  if not vim.api.nvim_buf_is_valid(buf) then
    M.stop(buf)
    return
  end

  if index <= 0 then
    vim.api.nvim_buf_clear_namespace(buf, M._ns, 0, -1)
    mode.index = 0
    M._open_summary(mode, { resume = true })
    return
  end
  if index > #mode.doc.steps then
    M.stop(buf)
    notify("Walkthrough complete", vim.log.levels.INFO)
    return
  end

  mode.index = index
  local step = mode.doc.steps[index]

  local state = mode.host.get_state()
  local target = M.resolve_step(state, step)
  -- Only unfold + re-render when no diff rows are rendered at all; a
  -- "nearest" match means the hunks are visible and just don't contain the
  -- exact line (TS-context scoping, deletions, staleness).
  if (target.match == "file_only" or target.match == "missing") and ensure_expanded(mode, step) then
    state = mode.host.get_state()
    target = M.resolve_step(state, step)
  end

  local win = vim.fn.bufwinid(buf)
  if win == -1 then
    M.stop(buf)
    notify("Status window is no longer visible", vim.log.levels.WARN)
    return
  end

  if target.start_row then
    pcall(vim.api.nvim_win_set_cursor, win, { target.start_row, 0 })
    vim.api.nvim_win_call(win, function() vim.cmd("normal! zz") end)
  end
  if target.match == "missing" then
    -- Anchor the box at the cursor's current row; no region to highlight.
    local cursor_row = vim.api.nvim_win_get_cursor(win)[1]
    target = { match = "missing", start_row = cursor_row, end_row = cursor_row }
  end
  apply_region_highlight(mode, target)
  render_comment_box(mode, step, target, win)
end

---@param mode DiffReviewWalkthroughMode
local function begin(mode)
  local buf = mode.host.buf
  M._modes[buf] = mode

  for _, key in ipairs(nav_keys) do
    mode.saved_maps[key] = vim.api.nvim_buf_call(buf, function()
      return vim.fn.maparg(key, "n", false, true)
    end)
  end

  local function nav(delta)
    return function()
      local active = M._modes[buf]
      if active then show_step(active, active.index + delta) end
    end
  end
  local map_opts = { buffer = buf, nowait = true, silent = true }
  vim.keymap.set("n", "z", nav(-1), vim.tbl_extend("force", map_opts, { desc = "Walkthrough back" }))
  vim.keymap.set("n", "y", nav(1), vim.tbl_extend("force", map_opts, { desc = "Walkthrough forward" }))
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, function() M.stop(buf) end,
      vim.tbl_extend("force", map_opts, { desc = "Walkthrough quit" }))
  end

  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buf,
    once = true,
    callback = function() M.stop(buf) end,
  })

  show_step(mode, 1)
end

--- Exit the walkthrough: clear decorations, close the float, restore the
--- original buffer-local mappings (q is the status close mapping).
---@param buf integer
function M.stop(buf)
  local mode = M._modes[buf]
  if not mode then return end
  M._modes[buf] = nil

  if vim.api.nvim_buf_is_valid(buf) then
    -- The namespace clear removes the region highlight and the inline
    -- comment box together.
    vim.api.nvim_buf_clear_namespace(buf, M._ns, 0, -1)
    for _, key in ipairs(nav_keys) do
      pcall(vim.keymap.del, "n", key, { buffer = buf })
      local saved = mode.saved_maps[key]
      if saved and saved.buffer == 1 and (saved.rhs ~= "" or saved.callback) then
        -- mapset restores buffer-local maps into the CURRENT buffer.
        vim.api.nvim_buf_call(buf, function()
          pcall(vim.fn.mapset, "n", false, saved)
        end)
      end
    end
  end
end

-- ─── summary popup ───────────────────────────────────────────────────────────

--- Open the centered summary popup. With opts.resume (navigated back from
--- step 1), q/<Esc> exits the whole walkthrough instead of cancelling entry.
---@param mode DiffReviewWalkthroughMode
---@param opts? { resume?: boolean }
function M._open_summary(mode, opts)
  opts = opts or {}
  local width = math.max(44, math.min(78, vim.o.columns - 8))
  -- Grow the popup to fit preformatted summary lines (e.g. flow diagrams).
  for _, line in ipairs(vim.split(mode.doc.summary, "\n", { plain = true })) do
    width = math.max(width, math.min(vim.fn.strdisplaywidth(line) + 6, vim.o.columns - 4))
  end
  local lines = summary_lines(mode.doc.summary, width - 4)
  table.insert(lines, 1, "")
  local stale_line = nil
  if mode.stale then
    lines[#lines + 1] = ""
    stale_line = #lines + 1
    lines[#lines + 1] = ("WARNING: generated against %s, HEAD is now %s"):format(
      mode.doc.commit:sub(1, 7),
      mode.head_sha and mode.head_sha:sub(1, 7) or "unknown"
    )
  end
  lines[#lines + 1] = ""
  lines[#lines + 1] = ("  %d steps    [y/<CR>] start    [q/<Esc>] %s"):format(
    #mode.doc.steps, opts.resume and "quit" or "cancel")

  local popup_buf = vim.api.nvim_create_buf(false, true)
  vim.bo[popup_buf].bufhidden = "wipe"
  vim.bo[popup_buf].buftype = "nofile"
  vim.api.nvim_buf_set_lines(popup_buf, 0, -1, false, lines)
  vim.bo[popup_buf].modifiable = false
  apply_summary_highlights(popup_buf, lines)
  if stale_line then
    pcall(vim.api.nvim_buf_set_extmark, popup_buf, M._ns, stale_line - 1, 0, {
      end_col = #lines[stale_line],
      hl_group = "DiffReviewWalkthroughStale",
    })
  end
  pcall(vim.api.nvim_buf_set_extmark, popup_buf, M._ns, #lines - 1, 0, {
    end_col = #lines[#lines],
    hl_group = "DiffReviewStatusHint",
  })

  local height = math.min(#lines, math.max(vim.o.lines - 6, 1))
  local win = vim.api.nvim_open_win(popup_buf, true, {
    relative = "editor",
    width = width,
    height = height,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - height) / 2),
    style = "minimal",
    border = "rounded",
    title = " Walkthrough ",
    title_pos = "center",
  })

  local function close_popup()
    if vim.api.nvim_win_is_valid(win) then
      pcall(vim.api.nvim_win_close, win, true)
    end
  end
  local popup_opts = { buffer = popup_buf, nowait = true, silent = true }
  for _, key in ipairs({ "y", "<CR>" }) do
    vim.keymap.set("n", key, function()
      close_popup()
      if opts.resume then
        show_step(mode, 1)
      else
        begin(mode)
      end
    end, vim.tbl_extend("force", popup_opts, { desc = "Start walkthrough" }))
  end
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, function()
      close_popup()
      if opts.resume then
        M.stop(mode.host.buf)
      end
    end, vim.tbl_extend("force", popup_opts, { desc = "Close walkthrough summary" }))
  end
end

-- ─── entry points ────────────────────────────────────────────────────────────

--- Start the walkthrough for a status buffer: load .walkthrough.json from the
--- repo root, check staleness against HEAD, and show the summary popup.
---@param host DiffReviewWalkthroughHost
function M.start(host)
  if M._modes[host.buf] then
    M.stop(host.buf)
  end

  local cwd = host.cwd()
  if not cwd or cwd == "" then
    notify("No repository loaded", vim.log.levels.WARN)
    return
  end

  local path = cwd .. "/.walkthrough.json"
  local text = (M._reader or read_file)(path)
  if not text then
    notify("No .walkthrough.json found at repo root", vim.log.levels.INFO)
    return
  end

  local ok, decoded = pcall(vim.json.decode, text, { luanil = { object = true, array = true } })
  if not ok then
    notify(".walkthrough.json is not valid JSON: " .. tostring(decoded), vim.log.levels.WARN)
    return
  end
  local doc, parse_err = parse_doc(decoded)
  if not doc then
    notify(".walkthrough.json: " .. parse_err, vim.log.levels.WARN)
    return
  end

  host.git_list_async({ "git", "-C", cwd, "rev-parse", "HEAD" }, function(output, code)
    if not vim.api.nvim_buf_is_valid(host.buf) then return end
    local head_sha = code == 0 and vim.trim((output or {})[1] or "") or nil
    ---@type DiffReviewWalkthroughMode
    local mode = {
      host = host,
      doc = doc,
      index = 0,
      head_sha = head_sha,
      stale = not head_sha or head_sha:lower() ~= doc.commit:lower(),
      saved_maps = {},
    }
    M._open_summary(mode)
  end)
end

--- Re-apply decorations after the status buffer re-renders (rows shift).
--- Never moves the cursor; the render path owns cursor restoration.
---@param buf integer
function M.on_status_rendered(buf)
  local mode = M._modes[buf]
  if not mode or mode.index < 1 then return end
  vim.schedule(function()
    local active = M._modes[buf]
    if not active or active.index < 1 then return end
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local win = vim.fn.bufwinid(buf)
    if win == -1 then return end
    local step = active.doc.steps[active.index]
    local target = M.resolve_step(active.host.get_state(), step)
    if target.match == "missing" then
      local cursor_row = vim.api.nvim_win_get_cursor(win)[1]
      target = { match = "missing", start_row = cursor_row, end_row = cursor_row }
    end
    apply_region_highlight(active, target)
    render_comment_box(active, step, target, win)
  end)
end

return M
