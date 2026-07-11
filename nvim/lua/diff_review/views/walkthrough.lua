--- Drives the guided code-review walkthrough rendered into the DiffReviewStatus buffer:
--- an LLM writes `.walkthrough.json` at the repo root (schema in
--- `walkthrough.schema.json` beside this file), and `ow` adds a foldable summary section
--- plus the author's comment boxes anchored below the referenced regions in the inline diff.
---
--- Resolves each step against the NEW (post-change) file and records the HEAD sha the
--- document was generated against, so a stale walkthrough degrades to best-effort jumps
--- with a visible note rather than wrong anchors.

---@class DiffReviewWalkthroughPosition
---@field line integer 1-based line in the new file
---@field col integer 1-based column, normalized to 1 for walkthrough anchors

---@class DiffReviewWalkthroughStep
---@field file string repo-relative path, forward slashes
---@field start_pos DiffReviewWalkthroughPosition
---@field end_pos DiffReviewWalkthroughPosition
---@field comment string
---@field title? string
---@field task_index integer
---@field step_index integer
---@field task_step_total integer
---@field label string
---@field task_title string
---@field subtask_title string
---@field item_title string

---@class DiffReviewWalkthroughItem
---@field action string
---@field type string
---@field subtype? string
---@field title string
---@field note string
---@field steps DiffReviewWalkthroughStep[]

---@class DiffReviewWalkthroughSubtask
---@field title string
---@field justification? string
---@field items DiffReviewWalkthroughItem[]

---@class DiffReviewWalkthroughTask
---@field title string
---@field justification? string
---@field subtasks DiffReviewWalkthroughSubtask[]

---@class DiffReviewWalkthroughFlowNode
---@field text string
---@field children DiffReviewWalkthroughFlowNode[]

---@class DiffReviewWalkthroughStepContext
---@field task_index integer
---@field step_index integer
---@field task_title string
---@field subtask_title string
---@field item_title string

---@class DiffReviewWalkthroughDoc
---@field version integer
---@field flow DiffReviewWalkthroughFlowNode[]
---@field overview string
---@field root string
---@field summary string
---@field commit string full sha the walkthrough was generated against
---@field tasks DiffReviewWalkthroughTask[]
---@field steps DiffReviewWalkthroughStep[]

---@alias DiffReviewWalkthroughMatch "exact"|"nearest"|"file_only"|"missing"

---@class DiffReviewWalkthroughTarget
---@field match DiffReviewWalkthroughMatch
---@field start_row? integer 1-based status-buffer row
---@field end_row? integer

---@class DiffReviewWalkthroughHost narrow interface handed over by diff_review
---@field buf integer
---@field cwd fun(): string?
---@field resolve_root_async fun(cb: fun(root?: string, err?: string))
---@field get_state fun(): table? current status state (lines, entries, sections)
---@field file_key fun(section_name: string, filename: string): string
---@field hunk_key fun(section_name: string, filename: string, diff?: string): string
---@field set_folded fun(key: string, folded: boolean)
---@field has_native_fold fun(key: string): boolean
---@field apply_native_folds fun()
---@field comment_anchor_key fun(file: string?, side: string?, line: integer|string?): string?
---@field set_comment_index fun(index?: table<string, { descriptor: DiffReviewCommentDescriptor, section_name?: string }[]>)
---@field rerender fun(target_id?: string, fallback_line?: integer) synchronous reuse_sections re-render
---@field git_list_async fun(command: string[], cb: fun(output: string[], code: integer))
---@field inventory_async? fun(cb: fun(result: DiffReviewInventoryResult))

---@alias DiffReviewWalkthroughReader fun(path: string): string?

---@class DiffReviewWalkthroughMode
---@field host DiffReviewWalkthroughHost
---@field doc DiffReviewWalkthroughDoc
---@field index integer 0 = status-integrated walkthrough, 1..n = focused step
---@field stale boolean
---@field head_sha? string
---@field saved_maps table<string, table> maparg dicts to restore on stop
---@field file_order? table<string, integer> first walkthrough step index by normalized repo-relative file
---@field inventory? DiffReviewInventoryResult
---@field inventory_signature? string
---@field inventory_pending_signature? string
---@field anchor_match_by_id? table<string, DiffReviewWalkthroughMatch>

---@class DiffReviewWalkthroughCommentBoxPlacement
---@field anchor_row integer
---@field line_count integer

---@class DiffReviewWalkthroughModule
---@field _modes table<integer, DiffReviewWalkthroughMode>
---@field _reader DiffReviewWalkthroughReader?
---@field _ns integer
---@field _jump_return_views table<integer, table>?
---@field _jump_return_autocmds table<integer, boolean>?
---@field _jump_debug_force? boolean
local M = {
  _modes = {},
  _ns = vim.api.nvim_create_namespace("diff_review.views.walkthrough"),
  _jump_return_views = {},
  _jump_return_autocmds = {},
}

local nav_keys = { "z", "y", "q", "<Esc>" }
local status_section_id = "walkthrough:section"
local jump_return_group = vim.api.nvim_create_augroup("DiffReviewWalkthroughJumpReturn", { clear = false })

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

local function jump_debug_enabled()
  local walkthrough_enabled = vim.g.diff_review_walkthrough_jump_debug
  local gitstatus_enabled = vim.g.diff_review_gitstatus_debug
  return M._jump_debug_force == true
    or walkthrough_enabled == true
    or walkthrough_enabled == 1
    or gitstatus_enabled == true
    or gitstatus_enabled == 1
end

local function jump_debug_log_path()
  local dir = (vim.fn.stdpath("state") or ".") .. "/diff_review"
  pcall(vim.fn.mkdir, dir, "p")
  return dir .. "/gitstatus-debug.log"
end

local function jump_debug_one_line(value)
  return vim.inspect(value):gsub("\r", "\\r"):gsub("\n", "\\n")
end

---@param win integer
---@param buf integer
---@return table
local function jump_debug_snapshot(win, buf)
  local snapshot = {
    win = win,
    buf = buf,
    win_valid = win ~= -1 and vim.api.nvim_win_is_valid(win),
    buf_valid = vim.api.nvim_buf_is_valid(buf),
  }
  if not (snapshot.win_valid and snapshot.buf_valid) then return snapshot end

  local ok_cursor, cursor = pcall(vim.api.nvim_win_get_cursor, win)
  if ok_cursor then snapshot.cursor = cursor end
  pcall(vim.api.nvim_win_call, win, function()
    snapshot.view = vim.fn.winsaveview()
    snapshot.window_top = vim.fn.line("w0")
    snapshot.window_bottom = vim.fn.line("w$")
  end)
  if ok_cursor and cursor and cursor[1] then
    local ok_line, line = pcall(vim.api.nvim_buf_get_lines, buf, cursor[1] - 1, cursor[1], false)
    if ok_line and line and line[1] then snapshot.cursor_line = line[1] end
  end
  return snapshot
end

---@param event string
---@param payload? table
local function jump_debug_event(event, payload)
  if not jump_debug_enabled() then return end
  local line = ("walkthrough_jump %s %s"):format(event, jump_debug_one_line(payload or {}))
  pcall(vim.fn.writefile, { line }, jump_debug_log_path(), "a")
end

---@param win integer
---@param buf integer
---@return table
function M._jump_debug_snapshot(win, buf)
  return jump_debug_snapshot(win, buf)
end

---@param event string
---@param payload? table
function M._debug_jump_event(event, payload)
  jump_debug_event(event, payload)
end

-- ─── document loading ────────────────────────────────────────────────────────

---@param value any
---@return DiffReviewWalkthroughPosition?
local function parse_anchor_line(value)
  local line = tonumber(value)
  if not line or line < 1 then return nil end
  return { line = math.floor(line), col = 1 }
end

---@param value any
---@return boolean
local function is_non_empty_string(value)
  return type(value) == "string" and vim.trim(value) ~= ""
end

---@param value any
---@param error_prefix string
---@param field string
---@return string? parsed
---@return string? error
local function parse_optional_string_field(value, error_prefix, field)
  if value == nil then return nil, nil end
  if not is_non_empty_string(value) then
    return nil, ("%s: invalid \"%s\""):format(error_prefix, field)
  end
  return value, nil
end

local action_order = { "Add", "Modify", "Remove" }
local valid_item_actions = {
  Add = true,
  Modify = true,
  Remove = true,
}
local valid_item_types = {
  Class = true,
  Struct = true,
  Enum = true,
  Trait = true,
  Interface = true,
  Field = true,
  Function = true,
  Method = true,
  Constant = true,
  Test = true,
  Config = true,
}
local action_highlights = {
  Add = "DiffReviewWalkthroughActionAdd",
  Modify = "DiffReviewWalkthroughActionModify",
  Remove = "DiffReviewWalkthroughActionRemove",
}
---@param is_last boolean
---@return string
local function tree_branch(is_last)
  return is_last and "└─ " or "├─ "
end

---@param is_last boolean
---@return string
local function tree_continuation(is_last)
  return is_last and "   " or "│  "
end

local walkthrough_status_body_prefix = "  "

---@param action string
---@return string
local function format_action(action)
  return action
end

---@param type_name string
---@return string
local function format_type_keyword(type_name)
  if type_name == "Function" or type_name == "Method" then return "fn" end
  return type_name:lower()
end

---@param item_type string
---@param item_subtype? string
---@return string
local function format_item_type_label(item_type, item_subtype)
  if is_non_empty_string(item_subtype) then return item_subtype --[[@as string]] end
  return format_type_keyword(item_type)
end

---@param item DiffReviewWalkthroughItem
---@param prefix string
---@param is_last boolean
---@return string
local function summary_item_text(item, prefix, is_last)
  return prefix
    .. tree_branch(is_last)
    .. format_action(item.action)
    .. " "
    .. format_item_type_label(item.type, item.subtype)
    .. " "
    .. item.title
    .. " to "
    .. item.note
end

---@param lines string[]
---@param item DiffReviewWalkthroughItem
---@param prefix string
---@param is_last boolean
local function append_summary_item(lines, item, prefix, is_last)
  lines[#lines + 1] = summary_item_text(item, prefix, is_last)
end

---@param task_index integer
---@param task DiffReviewWalkthroughTask
---@return string
local function task_summary_title(task_index, task)
  local line = ("%d. %s"):format(task_index, task.title)
  if task.justification and task.justification ~= "" then
    line = line .. " " .. task.justification
  end
  return line
end

local flow_arrow = " → "

---@class DiffReviewWalkthroughFlowLayout
---@field lines string[]
---@field penalty integer

---@param text string
---@return integer
local function text_width(text)
  return vim.fn.strdisplaywidth(text or "")
end

---@param line string
---@return integer
local function leading_indent_width(line)
  local indent = line:match("^%s*") or ""
  return #indent
end

---@param nodes DiffReviewWalkthroughFlowNode[]
---@return string
local function flow_chain_text(nodes)
  local labels = {}
  for _, node in ipairs(nodes or {}) do
    labels[#labels + 1] = node.text
  end
  return table.concat(labels, flow_arrow)
end

---@param node DiffReviewWalkthroughFlowNode
---@return DiffReviewWalkthroughFlowNode[] chain
---@return DiffReviewWalkthroughFlowNode[] children
local function collect_flow_chain(node)
  local chain = {}
  local current = node
  while current do
    chain[#chain + 1] = current
    local children = current.children or {}
    if #children ~= 1 then
      return chain, children
    end
    current = children[1]
  end
  return chain, {}
end

---@param lines string[]
---@param node DiffReviewWalkthroughFlowNode
---@param prefix string
---@param marker string
---@param child_prefix string
local function append_vertical_flow_node(lines, node, prefix, marker, child_prefix)
  lines[#lines + 1] = prefix .. marker .. node.text
  local children = node.children or {}
  if #children == 1 then
    append_vertical_flow_node(lines, children[1], child_prefix, "→ ", child_prefix)
    return
  end

  for child_index, child in ipairs(children) do
    local is_last = child_index == #children
    append_vertical_flow_node(lines, child, child_prefix, is_last and "└→ " or "├→ ", child_prefix .. "    ")
  end
end

---@param node DiffReviewWalkthroughFlowNode
---@param root_marker string
---@return string[]
local function vertical_flow_lines(node, root_marker)
  local lines = {}
  append_vertical_flow_node(lines, node, "", root_marker, "    ")
  return lines
end

---@param node DiffReviewWalkthroughFlowNode
---@return string[]
local function wrapped_flow_lines(node)
  local chain, children = collect_flow_chain(node)
  local lines = {}
  if #chain == 0 then return lines end

  lines[#lines + 1] = chain[1].text
  for index = 2, #chain do
    lines[#lines + 1] = "→ " .. chain[index].text
  end

  if #children == 0 then return lines end
  local branch_prefix = "    "
  for child_index, child in ipairs(children) do
    local is_last = child_index == #children
    local child_lines = wrapped_flow_lines(child)
    if #child_lines > 0 then
      lines[#lines + 1] = branch_prefix .. (is_last and "└→ " or "├→ ") .. child_lines[1]
      local continuation = branch_prefix .. (is_last and "    " or "│   ")
      for line_index = 2, #child_lines do
        lines[#lines + 1] = continuation .. child_lines[line_index]
      end
    end
  end
  return lines
end

---@param lines string[]
---@param branch_prefix string
---@param marker string
---@param is_last boolean
---@param child_lines string[]
local function append_branch_child_lines(lines, branch_prefix, marker, is_last, child_lines)
  if #child_lines == 0 then return end
  lines[#lines + 1] = branch_prefix .. marker .. child_lines[1]
  local continuation = branch_prefix .. (is_last and "   " or "│  ")
  for line_index = 2, #child_lines do
    local child_line = child_lines[line_index]
    if child_line:sub(1, 2) == "  " then child_line = child_line:sub(3) end
    lines[#lines + 1] = continuation .. child_line
  end
end

---@param layout DiffReviewWalkthroughFlowLayout
---@param width integer
---@return number
local function flow_layout_score(layout, width)
  local overflow = 0
  local max_indent = 0
  local max_width = 0
  for _, line in ipairs(layout.lines or {}) do
    local line_width = text_width(line)
    max_width = math.max(max_width, line_width)
    if line_width > width then
      overflow = overflow + line_width - width
    end
    max_indent = math.max(max_indent, leading_indent_width(line))
  end

  local overflow_penalty = overflow == 0 and 0 or 1000000 + overflow * 1000 + max_width
  return overflow_penalty + #layout.lines * 100 + layout.penalty + max_width / 100 + max_indent / 100
end

---@param candidates DiffReviewWalkthroughFlowLayout[]
---@param width integer
---@return string[]
local function choose_flow_layout(candidates, width)
  local best = candidates[1]
  local best_score = best and flow_layout_score(best, width) or math.huge
  for index = 2, #candidates do
    local score = flow_layout_score(candidates[index], width)
    if score < best_score then
      best = candidates[index]
      best_score = score
    end
  end
  return best and best.lines or {}
end

---@param node DiffReviewWalkthroughFlowNode
---@param width integer
---@param root_marker string
---@return string[]
local function layout_flow_node(node, width, root_marker)
  local chain, children = collect_flow_chain(node)
  local chain_text = flow_chain_text(chain)
  ---@type DiffReviewWalkthroughFlowLayout[]
  local candidates = {}

  if #children == 0 then
    candidates[#candidates + 1] = { lines = { chain_text }, penalty = 0 }
  end

  if #children > 0 then
    local branch_prefix = (" "):rep(text_width(chain_text) + 1)
    local branch_width = math.max(20, width - text_width(branch_prefix) - text_width("└→ "))

    local detached_branch_prefix = "  "
    local detached_branch_width = math.max(20, width - text_width(detached_branch_prefix) - text_width("└→ "))
    local detached_lines = { chain_text }
    for child_index, child in ipairs(children) do
      local is_last = child_index == #children
      local child_lines = layout_flow_node(child, detached_branch_width, "")
      append_branch_child_lines(detached_lines, detached_branch_prefix, is_last and "└→ " or "├→ ", is_last, child_lines)
    end
    candidates[#candidates + 1] = { lines = detached_lines, penalty = 0 }

    local horizontal_lines = { chain_text }
    for child_index, child in ipairs(children) do
      local is_last = child_index == #children
      local child_lines = layout_flow_node(child, branch_width, "")
      append_branch_child_lines(horizontal_lines, branch_prefix, is_last and "└→ " or "├→ ", is_last, child_lines)
    end
    candidates[#candidates + 1] = { lines = horizontal_lines, penalty = 2 }

    local wrapped_lines = { chain_text }
    for child_index, child in ipairs(children) do
      local is_last = child_index == #children
      local child_lines = wrapped_flow_lines(child)
      append_branch_child_lines(wrapped_lines, branch_prefix, is_last and "└→ " or "├→ ", is_last, child_lines)
    end
    candidates[#candidates + 1] = { lines = wrapped_lines, penalty = 6 }
  end

  candidates[#candidates + 1] = { lines = vertical_flow_lines(node, root_marker), penalty = 20 }
  return choose_flow_layout(candidates, width)
end

---@param flow DiffReviewWalkthroughFlowNode[]
---@param width? integer
---@return string[]
local function flow_summary_lines(flow, width)
  width = width or math.max(40, math.min(100, vim.o.columns - 8))
  local lines = {}
  for node_index, node in ipairs(flow or {}) do
    if node_index > 1 then lines[#lines + 1] = "" end
    vim.list_extend(lines, layout_flow_node(node, width, "• "))
  end
  return lines
end

---@param flow DiffReviewWalkthroughFlowNode[]
---@param width integer
---@return string[]
function M._flow_summary_lines_for_test(flow, width)
  return flow_summary_lines(flow, width)
end

---@param win integer?
---@return integer
local function effective_window_columns(win)
  local columns = vim.o.columns
  if win and win ~= -1 and #vim.api.nvim_list_uis() > 0 then
    local ok, win_width = pcall(vim.api.nvim_win_get_width, win)
    if ok and type(win_width) == "number" and win_width > 0 then
      columns = win_width
    end
  end
  return columns
end

---@param mode DiffReviewWalkthroughMode
---@return integer
local function status_render_width(mode)
  local win = mode and mode.host and mode.host.buf and vim.fn.bufwinid(mode.host.buf) or -1
  return math.max(40, effective_window_columns(win) - 4)
end

---@param lines string[]
---@param task DiffReviewWalkthroughTask
local function append_task_summary_body(lines, task)
  local subtask_prefix = ""
  for subtask_index, subtask in ipairs(task.subtasks) do
    local subtask_is_last = subtask_index == #task.subtasks
    lines[#lines + 1] = subtask_prefix .. tree_branch(subtask_is_last) .. subtask.title
    local item_prefix = subtask_prefix .. tree_continuation(subtask_is_last)
    if subtask.justification then lines[#lines + 1] = item_prefix .. subtask.justification end
    for item_index, item in ipairs(subtask.items) do
      local item_is_last = item_index == #subtask.items
      append_summary_item(lines, item, item_prefix, item_is_last)
    end
  end
end

---@param tasks DiffReviewWalkthroughTask[]
---@return { type: string, subtype: string?, label: string, title: string }[] specs
local function collect_summary_item_specs(tasks)
  local specs = {}
  for _, task in ipairs(tasks) do
    for _, subtask in ipairs(task.subtasks) do
      for _, item in ipairs(subtask.items) do
        specs[#specs + 1] = {
          type = item.type,
          subtype = item.subtype,
          label = format_item_type_label(item.type, item.subtype),
          title = item.title,
        }
      end
    end
  end
  table.sort(specs, function(left, right)
    local left_text = left.label .. " " .. left.title
    local right_text = right.label .. " " .. right.title
    return #left_text > #right_text
  end)
  return specs
end

---@param tasks DiffReviewWalkthroughTask[]
---@return string[] titles
local function collect_summary_item_titles(tasks)
  local titles = {}
  for _, task in ipairs(tasks) do
    for _, subtask in ipairs(task.subtasks) do
      for _, item in ipairs(subtask.items) do
        titles[#titles + 1] = item.title
      end
    end
  end
  table.sort(titles, function(left, right) return #left > #right end)
  return titles
end

---@param tasks DiffReviewWalkthroughTask[]
---@return string[] justifications
local function collect_summary_justifications(tasks)
  local justifications = {}
  for _, task in ipairs(tasks) do
    for _, subtask in ipairs(task.subtasks) do
      if subtask.justification then justifications[#justifications + 1] = subtask.justification end
    end
  end
  table.sort(justifications, function(left, right) return #left > #right end)
  return justifications
end

---@param overview string
---@param _root string
---@param flow DiffReviewWalkthroughFlowNode[]
---@param tasks DiffReviewWalkthroughTask[]
---@return string
local function build_summary(overview, _root, flow, tasks)
  local lines = {}
  vim.list_extend(lines, vim.split(vim.trim(overview), "\n", { plain = true }))
  lines[#lines + 1] = ""
  vim.list_extend(lines, flow_summary_lines(flow))
  lines[#lines + 1] = ""
  for task_index, task in ipairs(tasks) do
    if task_index > 1 then lines[#lines + 1] = "" end
    lines[#lines + 1] = task_summary_title(task_index, task)
    append_task_summary_body(lines, task)
  end

  return table.concat(lines, "\n")
end

local annotation_allowed_fields = {
  title = true,
  comment = true,
}

---@param raw_change any
---@param raw_annotation any
---@param error_prefix string
---@param context DiffReviewWalkthroughStepContext
---@return DiffReviewWalkthroughStep? step
---@return string? error
local function parse_step(raw_change, raw_annotation, error_prefix, context)
  if type(raw_annotation) ~= "table" then
    return nil, error_prefix .. ": invalid \"annotation\""
  end
  for field in pairs(raw_annotation) do
    if not annotation_allowed_fields[field] then
      return nil, ("%s: unsupported \"%s\""):format(error_prefix, field)
    end
  end
  if not is_non_empty_string(raw_annotation.title) then
    return nil, error_prefix .. ": missing \"title\""
  end
  if not is_non_empty_string(raw_annotation.comment) then
    return nil, error_prefix .. ": missing \"comment\""
  end
  if not is_non_empty_string(raw_change.file) then
    return nil, error_prefix .. ": missing \"file\""
  end

  local start_pos = parse_anchor_line(raw_change.line)
  if not start_pos then
    return nil, error_prefix .. ": missing or invalid \"line\""
  end

  return {
    file = (raw_change.file:gsub("\\", "/")),
    start_pos = start_pos,
    end_pos = start_pos,
    comment = raw_annotation.comment,
    title = raw_annotation.title,
    task_index = context.task_index,
    step_index = context.step_index,
    label = ("Task %d.%d"):format(context.task_index, context.step_index),
    task_title = context.task_title,
    subtask_title = context.subtask_title,
    item_title = context.item_title,
  }
end

---@param raw_change any
---@param error_prefix string
---@param base_context table
---@param task_step_index integer
---@param all_steps DiffReviewWalkthroughStep[]
---@return DiffReviewWalkthroughItem? item
---@return integer task_step_index
---@return string? error
local function parse_change(raw_change, error_prefix, base_context, task_step_index, all_steps)
  if type(raw_change) ~= "table" then
    return nil, task_step_index, error_prefix .. ": missing change object"
  end
  if type(raw_change.action) ~= "string" or not valid_item_actions[raw_change.action] then
    return nil, task_step_index, error_prefix .. ": missing or invalid \"action\""
  end
  if not is_non_empty_string(raw_change.target) then
    return nil, task_step_index, error_prefix .. ": missing \"target\""
  end
  if not is_non_empty_string(raw_change.note) then
    return nil, task_step_index, error_prefix .. ": missing \"note\""
  end

  if type(raw_change.kind) ~= "string" or not valid_item_types[raw_change.kind] then
    return nil, task_step_index, error_prefix .. ": missing or invalid \"kind\""
  end
  local item_subtype, role_error = parse_optional_string_field(raw_change.role, error_prefix, "role")
  if role_error then return nil, task_step_index, role_error end
  if raw_change.children ~= nil then
    return nil, task_step_index, error_prefix .. ": \"children\" is not supported; use a subtask or sibling change"
  end
  if raw_change.annotations ~= nil then
    return nil, task_step_index, error_prefix .. ": \"annotations\" is not supported; use singular \"annotation\""
  end

  ---@type DiffReviewWalkthroughStep[]
  local item_steps = {}
  if raw_change.annotation ~= nil then
    task_step_index = task_step_index + 1
    local step, parse_error = parse_step(raw_change, raw_change.annotation, ("%s annotation"):format(error_prefix), {
      task_index = base_context.task_index,
      step_index = task_step_index,
      task_title = base_context.task_title,
      subtask_title = base_context.subtask_title,
      item_title = raw_change.target,
    })
    if not step then return nil, task_step_index, parse_error end
    item_steps[#item_steps + 1] = step
    all_steps[#all_steps + 1] = step
  elseif raw_change.file ~= nil or raw_change.line ~= nil then
    return nil, task_step_index, error_prefix .. ": \"file\" and \"line\" require \"annotation\""
  end

  return {
    action = raw_change.action,
    type = raw_change.kind,
    subtype = item_subtype,
    title = raw_change.target,
    note = raw_change.note,
    steps = item_steps,
  }, task_step_index, nil
end

---@param raw_subtask any
---@param error_prefix string
---@param base_context table
---@param task_step_index integer
---@param all_steps DiffReviewWalkthroughStep[]
---@return DiffReviewWalkthroughSubtask? subtask
---@return integer task_step_index
---@return string? error
local function parse_subtask(raw_subtask, error_prefix, base_context, task_step_index, all_steps)
  if type(raw_subtask) ~= "table" or not is_non_empty_string(raw_subtask.title) then
    return nil, task_step_index, error_prefix .. ": missing \"title\""
  end
  if type(raw_subtask.changes) ~= "table" or #raw_subtask.changes == 0 then
    return nil, task_step_index, error_prefix .. ": missing or empty \"changes\""
  end
  local justification, justification_error = parse_optional_string_field(raw_subtask.justification, error_prefix,
    "justification")
  if justification_error then return nil, task_step_index, justification_error end

  ---@type DiffReviewWalkthroughItem[]
  local items = {}
  for change_index, raw_change in ipairs(raw_subtask.changes) do
    local change_prefix = ("%s change %d"):format(error_prefix, change_index)
    local item, next_step_index, item_error = parse_change(raw_change, change_prefix, {
      task_index = base_context.task_index,
      task_title = base_context.task_title,
      subtask_title = raw_subtask.title,
    }, task_step_index, all_steps)
    if not item then return nil, next_step_index, item_error end
    task_step_index = next_step_index
    items[#items + 1] = item
  end

  return {
    title = raw_subtask.title,
    justification = justification,
    items = items,
  }, task_step_index, nil
end

---@param raw_node any
---@param error_prefix string
---@return DiffReviewWalkthroughFlowNode? node
---@return string? error
local function parse_flow_node(raw_node, error_prefix)
  if type(raw_node) ~= "table" then
    return nil, error_prefix .. ": invalid flow node"
  end
  if not is_non_empty_string(raw_node.text) then
    return nil, error_prefix .. ": missing \"text\""
  end

  ---@type DiffReviewWalkthroughFlowNode[]
  local children = {}
  if raw_node.children ~= nil then
    if type(raw_node.children) ~= "table" then
      return nil, error_prefix .. ": invalid \"children\""
    end
    for child_index, raw_child in ipairs(raw_node.children) do
      local child, child_error = parse_flow_node(raw_child, ("%s child %d"):format(error_prefix, child_index))
      if not child then return nil, child_error end
      children[#children + 1] = child
    end
  end

  return {
    text = raw_node.text,
    children = children,
  }, nil
end

--- Tolerantly validate and normalize a decoded walkthrough document.
---@param decoded any
---@return DiffReviewWalkthroughDoc? doc
---@return string? error
local function parse_doc(decoded)
  if type(decoded) ~= "table" then
    return nil, "document is not a JSON object"
  end
  if tonumber(decoded.version) ~= 12 then
    return nil, "unsupported \"version\" (expected 12)"
  end
  if type(decoded.flow) ~= "table" or #decoded.flow == 0 then
    return nil, "missing or empty \"flow\""
  end
  ---@type DiffReviewWalkthroughFlowNode[]
  local flow = {}
  for flow_index, raw_flow_node in ipairs(decoded.flow) do
    local flow_node, flow_error = parse_flow_node(raw_flow_node, ("flow %d"):format(flow_index))
    if not flow_node then return nil, flow_error end
    flow[#flow + 1] = flow_node
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
  if type(decoded.tasks) ~= "table" or #decoded.tasks == 0 then
    return nil, "missing or empty \"tasks\""
  end

  ---@type DiffReviewWalkthroughStep[]
  local steps = {}
  ---@type DiffReviewWalkthroughTask[]
  local tasks = {}
  for task_index, raw_task in ipairs(decoded.tasks) do
    local task_prefix = ("task %d"):format(task_index)
    if type(raw_task) ~= "table" or not is_non_empty_string(raw_task.title) then
      return nil, task_prefix .. ": missing \"title\""
    end
    if type(raw_task.subtasks) ~= "table" or #raw_task.subtasks == 0 then
      return nil, task_prefix .. ": missing or empty \"subtasks\""
    end
    local task_justification, task_justification_error = parse_optional_string_field(raw_task.justification,
      task_prefix, "justification")
    if task_justification_error then return nil, task_justification_error end

    local task_step_index = 0
    local task_first_step_offset = #steps + 1
    ---@type DiffReviewWalkthroughSubtask[]
    local subtasks = {}
    for subtask_index, raw_subtask in ipairs(raw_task.subtasks) do
      local subtask_prefix = ("%s subtask %d"):format(task_prefix, subtask_index)
      local subtask, next_step_index, subtask_error = parse_subtask(raw_subtask, subtask_prefix, {
        task_index = task_index,
        task_title = raw_task.title,
      }, task_step_index, steps)
      if not subtask then return nil, subtask_error end
      task_step_index = next_step_index
      subtasks[#subtasks + 1] = subtask
    end

    if task_step_index == 0 then
      return nil, task_prefix .. ": no walkthrough steps"
    end
    for step_offset = task_first_step_offset, #steps do
      steps[step_offset].task_step_total = task_step_index
    end

    tasks[#tasks + 1] = {
      title = raw_task.title,
      justification = task_justification,
      subtasks = subtasks,
    }
  end

  return {
    version = 12,
    flow = flow,
    overview = decoded.overview,
    root = decoded.root,
    summary = build_summary(decoded.overview, decoded.root, flow, tasks),
    commit = decoded.commit,
    tasks = tasks,
    steps = steps,
  }
end

-- ─── step -> rendered row resolution ─────────────────────────────────────────

local section_preference = { "unstaged", "staged" }

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

---@param mode DiffReviewWalkthroughMode
---@return table<string, integer>
local function file_order_for_mode(mode)
  if mode.walkthrough_source and mode.walkthrough_source.file_rank then
    return mode.walkthrough_source.file_rank
  end
  local file_rank = {}
  local file_order = {}
  for index, step in ipairs(mode.doc.steps or {}) do
    local step_file = tostring(step.file or ""):gsub("\\", "/")
    if step_file ~= "" and file_rank[step_file] == nil then
      file_rank[step_file] = index
      file_order[#file_order + 1] = step_file
    end
  end
  -- Build the canonical walkthrough diff source so step ordering, annotations, and
  -- navigation live in one model layered over the base Git sources.
  local source_module = require("diff_review.render.source")
  local host_buf = mode.host and mode.host.buf or "walkthrough"
  mode.walkthrough_source = source_module.new_walkthrough_source(
    { id = "walkthrough:" .. tostring(host_buf), kind = "walkthrough" },
    { file_order = file_order, step_annotations = mode.doc.steps or {} }
  )
  mode.walkthrough_source.file_rank = file_rank
  return file_rank
end

---@param buf integer
---@param file table?
---@return integer?
function M.file_sort_rank(buf, file)
  local mode = M._modes[buf]
  if not mode then return nil end

  local file_order = file_order_for_mode(mode)
  local best_rank = nil
  for _, candidate in ipairs({ file and file.relpath, file and file.filename, file and file.original_relpath }) do
    if type(candidate) == "string" then
      candidate = candidate:gsub("\\", "/")
      local direct_rank = file_order[candidate]
      if direct_rank and (not best_rank or direct_rank < best_rank) then
        best_rank = direct_rank
      end
      for step_file, rank in pairs(file_order) do
        if (not best_rank or rank < best_rank) and matches_file(candidate, step_file) then
          best_rank = rank
        end
      end
    end
  end
  return best_rank
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
  local can_apply_native = true
  for _, section in ipairs(sections) do
    for _, file in ipairs(section.files or {}) do
      if matches_file(file.relpath, step.file) or matches_file(file.filename, step.file) then
        local section_key = "section:" .. (file.section_name or section.name)
        local file_key = mode.host.file_key(file.section_name, file.filename)
        can_apply_native = can_apply_native and mode.host.has_native_fold(file_key)
        mode.host.set_folded(section_key, false)
        mode.host.set_folded(file_key, false)
        for _, hunk in ipairs(file.hunks or {}) do
          local hunk_key = mode.host.hunk_key(file.section_name, file.filename, hunk.diff)
          mode.host.set_folded(hunk_key, false)
        end
        expanded = true
      end
    end
  end
  if expanded then
    if can_apply_native then
      mode.host.apply_native_folds()
    else
      mode.host.rerender()
    end
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

---@param line string
---@return string? first_prefix
---@return string? continuation_prefix
---@return string? body
local function tree_wrap_parts(line)
  local prefix, branch, body = line:match("^(%s*[│ ]*)([├└]─%s+)(%S.*)$")
  if not prefix or not branch or not body then return nil, nil, nil end
  local continuation = branch:find("├", 1, true) and "│  " or "   "
  return prefix .. branch, prefix .. continuation, body
end

---@param line string
---@return string? prefix
---@return string? body
local function tree_indent_wrap_parts(line)
  local prefix, body = line:match("^(%s*[│ ]*│[│ ]+)(%S.*)$")
  if not prefix or not body then return nil, nil end
  return prefix, body
end

---@param lines string[]
---@param first_prefix string
---@param continuation_prefix string
---@param body string
---@param width integer
local function append_wrapped_tree_line(lines, first_prefix, continuation_prefix, body, width)
  local body_width = math.max(width - vim.fn.strdisplaywidth(first_prefix), 8)
  for index, wrapped_line in ipairs(wrap_text(body, body_width)) do
    lines[#lines + 1] = (index == 1 and first_prefix or continuation_prefix) .. wrapped_line
  end
end

---@param line string
---@param item_titles string[]
---@return string? prefix
---@return string? note
local function item_note_wrap_parts(line, item_titles)
  local tree_prefix = tree_wrap_parts(line)
  if not tree_prefix then return nil, nil end
  for _, title in ipairs(item_titles or {}) do
    local title_start_byte = line:find(title .. " to ", #tree_prefix + 1, true)
    if title_start_byte then
      local note_start_byte = title_start_byte + #title + #" to "
      return line:sub(1, note_start_byte - 1), line:sub(note_start_byte)
    end
  end
  return nil, nil
end

---@param lines string[]
---@param prefix string
---@param note string
---@param width integer
---@return boolean wrapped
local function append_wrapped_item_note(lines, prefix, note, width)
  local prefix_width = vim.fn.strdisplaywidth(prefix)
  if prefix_width >= width - 8 then return false end
  local note_width = width - prefix_width
  for index, wrapped_note in ipairs(wrap_text(note, note_width)) do
    lines[#lines + 1] = (index == 1 and prefix or (" "):rep(prefix_width)) .. wrapped_note
  end
  return true
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
  return nil
end


---@param step DiffReviewWalkthroughStep
---@return string
local function format_step_heading_label(step)
  local task_step_total = step.task_step_total or step.step_index
  return ("%d.%d-%d"):format(step.task_index, step.step_index, task_step_total)
end

---@param step DiffReviewWalkthroughStep
---@return string
local function step_comment_id(step)
  return ("walkthrough:%d:%d"):format(step.task_index, step.step_index)
end

---@param file DiffReviewStatusFile
---@return integer[]
local function walkthrough_file_candidate_lines(file)
  local candidate_set = {}
  local diff_parse = require("diff_review.render.diff_parse")
  for _, raw_hunk in ipairs(file.hunks or {}) do
    for _, block in ipairs(diff_parse.parse_unified_diff(raw_hunk.diff or "")) do
      for _, parsed_hunk in ipairs(block.hunks or {}) do
        diff_parse.parse_hunk_body(parsed_hunk)
        candidate_set[parsed_hunk.new_start] = true
        for _, parsed_line in ipairs(parsed_hunk.lines or {}) do
          if parsed_line.new_line then candidate_set[parsed_line.new_line] = true end
        end
      end
    end
  end
  local candidates = {}
  for line in pairs(candidate_set) do candidates[#candidates + 1] = line end
  table.sort(candidates)
  return candidates
end

---@param candidates integer[]
---@param requested integer
---@return integer? line
---@return DiffReviewWalkthroughMatch match
local function resolve_candidate_line(candidates, requested)
  local nearest = nil
  local nearest_distance = nil
  for _, candidate in ipairs(candidates) do
    if candidate == requested then return candidate, "exact" end
    local distance = math.abs(candidate - requested)
    if not nearest_distance or distance < nearest_distance then
      nearest = candidate
      nearest_distance = distance
    end
  end
  return nearest, nearest and "nearest" or "file_only"
end

---@param mode DiffReviewWalkthroughMode
---@param state table?
---@return table<string, { descriptor: DiffReviewCommentDescriptor, section_name?: string }[]>
local function build_comment_index(mode, state)
  local index = {}
  mode.anchor_match_by_id = {}
  local sections = state and state.sections or {}
  for _, step in ipairs(mode.doc.steps or {}) do
    local matched_file = nil
    local matched_section = nil
    for _, preferred_section in ipairs(section_preference) do
      for _, section in ipairs(sections) do
        for _, file in ipairs(section.files or {}) do
          if file.section_name == preferred_section
            and (matches_file(file.relpath, step.file) or matches_file(file.filename, step.file)) then
            matched_file = file
            matched_section = preferred_section
            break
          end
        end
        if matched_file then break end
      end
      if matched_file then break end
    end
    if matched_file then
      local resolved_line, match = resolve_candidate_line(
        walkthrough_file_candidate_lines(matched_file),
        step.start_pos.line
      )
      mode.anchor_match_by_id[step_comment_id(step)] = match
      local key = resolved_line and mode.host.comment_anchor_key(matched_file.filename, "RIGHT", resolved_line) or nil
      if key then
        local heading_title = step.title or step.item_title
        local heading_label = format_step_heading_label(step)
        local heading = (" %s%s "):format(heading_label, heading_title and (" " .. heading_title) or "")
        local descriptor = {
          id = step_comment_id(step),
          anchor = { file = matched_file.filename, side = "RIGHT", line = resolved_line },
          heading = heading,
          body_lines = vim.split(step.comment or "", "\n", { plain = true }),
          stale_note = staleness_note({ match = match }, mode.stale),
          readonly = true,
          source = step,
        }
        index[key] = index[key] or {}
        index[key][#index[key] + 1] = { descriptor = descriptor, section_name = matched_section }
      end
    else
      mode.anchor_match_by_id[step_comment_id(step)] = "missing"
    end
  end
  return index
end

---@param win_height integer
---@param line_count integer
---@return integer screen_row
local function comment_box_start_screen_row(win_height, line_count)
  local one_third_from_bottom = win_height - math.floor(win_height / 3)
  local latest_start_with_box_visible = math.max(1, win_height - line_count + 1)
  return math.max(1, math.min(one_third_from_bottom, latest_start_with_box_visible))
end

---@param target DiffReviewWalkthroughTarget
---@param win_height integer
---@param comment_line_count integer
---@return integer anchor_row
-- ─── mode lifecycle ──────────────────────────────────────────────────────────

---@param win integer
---@param target DiffReviewWalkthroughTarget
---@param placement DiffReviewWalkthroughCommentBoxPlacement
local function focus_comment_box_below(win, target, placement)
  local start_row = target.start_row
  if not start_row then return end

  local win_height = vim.api.nvim_win_get_height(win)
  local target_screen_row = comment_box_start_screen_row(win_height, placement.line_count)
  local desired_topline = placement.anchor_row + 2 - target_screen_row
  desired_topline = math.max(1, math.min(desired_topline, start_row))

  vim.api.nvim_win_call(win, function()
    local view = vim.fn.winsaveview()
    view.topline = desired_topline
    vim.fn.winrestview(view)
  end)
end

--- Summary text mixes prose with preformatted content (ASCII code-flow
--- diagrams): lines that fit are preserved verbatim (indentation included);
--- only overlong lines fall back to word wrapping.
---@param text string
---@param width integer
---@param item_titles? string[]
---@return string[] lines
local function summary_lines(text, width, item_titles)
  local lines = {}
  for _, line in ipairs(vim.split(text, "\n", { plain = true })) do
    if vim.fn.strdisplaywidth(line) <= width then
      lines[#lines + 1] = line
    else
      local item_prefix, item_note = item_note_wrap_parts(line, item_titles or {})
      if item_prefix and item_note and append_wrapped_item_note(lines, item_prefix, item_note, width) then
        -- Item notes wrap as their own prose segment, aligned under the note.
      else
        local first_prefix, continuation_prefix, tree_body = tree_wrap_parts(line)
        if first_prefix and continuation_prefix and tree_body then
          append_wrapped_tree_line(lines, first_prefix, continuation_prefix, tree_body, width)
        else
          local tree_indent, tree_indent_body = tree_indent_wrap_parts(line)
          if tree_indent and tree_indent_body then
            local body_width = math.max(width - vim.fn.strdisplaywidth(tree_indent), 8)
            for _, wrapped_line in ipairs(wrap_text(tree_indent_body, body_width)) do
              lines[#lines + 1] = tree_indent .. wrapped_line
            end
          else
            local indent, body = line:match("^(%s+)(%S.*)$")
            if indent and body and #indent < width then
              for _, wrapped_line in ipairs(wrap_text(body, width - #indent)) do
                lines[#lines + 1] = indent .. wrapped_line
              end
            else
              vim.list_extend(lines, wrap_text(line, width))
            end
          end
        end
      end
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
    local action_label = format_action(action)
    local start_byte = line:find(action_label .. " ", 1, true)
    local prefix = start_byte and line:sub(1, start_byte - 1) or ""
    if start_byte and (vim.endswith(prefix, "├─ ") or vim.endswith(prefix, "└─ ")) then
      local start_col = start_byte - 1
      return start_col, start_col + #action_label, action
    end
  end
  return nil, nil, nil
end

---@param line string
---@param action_start_col integer
---@param action string
---@param item_specs { type: string, subtype: string?, label: string, title: string }[]
---@return integer? start_col 0-based byte column
---@return integer? end_col 0-based byte column, exclusive
---@return string? item_type
---@return integer? type_start_col 0-based byte column
---@return string? type_label
local function summary_item_type_range(line, action_start_col, action, item_specs)
  local label_start_byte = action_start_col + #format_action(action) + 2
  for _, spec in ipairs(item_specs or {}) do
    local label = spec.label
    if line:sub(label_start_byte, label_start_byte + #label - 1) == label
        and line:sub(label_start_byte + #label, label_start_byte + #label) == " " then
      local label_start_col = label_start_byte - 1
      return label_start_col, label_start_col + #label, spec.type, label_start_col, label
    end
  end
  return nil, nil, nil, nil, nil
end

---@param line string
---@param _item_type string
---@param item_type_label string
---@param item_type_start_col integer 0-based byte column
---@param item_titles string[]
---@return integer? start_col 0-based byte column
---@return integer? end_col 0-based byte column, exclusive
local function summary_item_title_range(line, _item_type, item_type_label, item_type_start_col, item_titles)
  local title_start_col = item_type_start_col + #item_type_label + 1
  if title_start_col >= #line then return nil, nil end
  local title_and_note = line:sub(title_start_col + 1)
  for _, title in ipairs(item_titles or {}) do
    if vim.startswith(title_and_note, title) then
      return title_start_col, title_start_col + #title
    end
  end
  for _, title in ipairs(item_titles or {}) do
    if vim.startswith(title, title_and_note) then
      return title_start_col, #line
    end
  end
  return title_start_col, #line
end

---@param line string
---@return integer
---@return string
local function summary_line_prose(line)
  local byte_index = 1
  while byte_index <= #line do
    local byte = line:sub(byte_index, byte_index)
    if byte == " " or byte == "\t" then
      byte_index = byte_index + 1
    elseif line:sub(byte_index, byte_index + #"│" - 1) == "│" then
      byte_index = byte_index + #"│"
    else
      break
    end
  end
  return byte_index - 1, line:sub(byte_index)
end

---@param line string
---@param justifications string[]
---@return boolean
local function summary_is_justification_line(line, justifications)
  local _, text = summary_line_prose(line)
  text = vim.trim(text)
  if text == "" then return false end
  for _, justification in ipairs(justifications or {}) do
    if justification:find(text, 1, true) then return true end
  end
  return false
end

---@param line string
---@return boolean
local function summary_is_task_line(line)
  return line:match("^%d+%.%s+") ~= nil
end

---@param line string
---@param tasks DiffReviewWalkthroughTask[]
---@return integer? title_start_col
---@return integer? title_end_col
local function summary_task_ranges(line, tasks)
  for task_index, task in ipairs(tasks or {}) do
    local title = ("%d. %s"):format(task_index, task.title)
    if vim.startswith(line, title) then
      return 0, #title
    end
    if vim.startswith(title, line) then
      return 0, #line
    end
  end
  return nil, nil
end

---@param buf integer
---@param lines string[]
---@param doc DiffReviewWalkthroughDoc
---@param row_offset? integer
local function apply_summary_highlights(buf, lines, doc, row_offset)
  row_offset = row_offset or 0
  local item_titles = collect_summary_item_titles(doc.tasks)
  local item_specs = collect_summary_item_specs(doc.tasks)
  local justifications = collect_summary_justifications(doc.tasks)
  for row, line in ipairs(lines) do
    local task_title_start_col, task_title_end_col = summary_task_ranges(line, doc.tasks)
    if task_title_start_col and task_title_end_col then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row_offset + row - 1, 0, {
        end_col = task_title_end_col,
        hl_group = "DiffReviewWalkthroughItemTitle",
      })
    elseif summary_is_task_line(line) then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row_offset + row - 1, 0, {
        end_col = #line,
        hl_group = "DiffReviewWalkthroughItemTitle",
      })
    end

    if summary_is_justification_line(line, justifications) then
      local start_col = summary_line_prose(line)
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row_offset + row - 1, start_col, {
        end_col = #line,
        hl_group = "DiffReviewWalkthroughJustification",
      })
    end

    local start_col, end_col, action = summary_action_range(line)
    local hl_group = action and action_highlights[action] or nil
    if start_col and end_col then
      if hl_group then
        pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row_offset + row - 1, start_col, {
          end_col = end_col,
          hl_group = hl_group,
        })
      end
      local type_start_col, type_end_col, item_type, item_type_start_col, item_type_label =
        summary_item_type_range(line, start_col, action, item_specs)
      if type_start_col and type_end_col and hl_group then
        pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row_offset + row - 1, type_start_col, {
          end_col = type_end_col,
          hl_group = hl_group,
        })
      end
      if item_type and item_type_start_col and item_type_label then
        local title_start_col, title_end_col =
          summary_item_title_range(line, item_type, item_type_label, item_type_start_col, item_titles)
        if title_start_col and title_end_col then
          pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row_offset + row - 1, title_start_col, {
            end_col = title_end_col,
            hl_group = "DiffReviewWalkthroughItemTitle",
          })
        end
      end
    end
  end
end

---@param mode DiffReviewWalkthroughMode
---@return string[] lines
local function status_summary_lines(mode)
  local width = status_render_width(mode)
  return summary_lines(mode.doc.summary, width, collect_summary_item_titles(mode.doc.tasks))
end

---@param text string
---@param width integer
---@return string first_line
---@return string rest
local function split_first_wrapped_line(text, width)
  local current = ""
  for start_byte, word in text:gmatch("()(%S+)") do
    local candidate = current == "" and word or (current .. " " .. word)
    if current ~= "" and vim.fn.strdisplaywidth(candidate) > width then
      return current, vim.trim(text:sub(start_byte))
    end
    current = candidate
  end
  return current, ""
end

---@param task_index integer
---@param task DiffReviewWalkthroughTask
---@param width integer
---@return { text: string, segments: table[] }[]
local function status_task_heading_rows(task_index, task, width)
  local title = ("%d. %s"):format(task_index, task.title)
  if not (task.justification and task.justification ~= "") then
    local rows = {}
    for _, line in ipairs(summary_lines(title, width, {})) do
      rows[#rows + 1] = { text = line, segments = { { line, "DiffReviewWalkthroughItemTitle" } } }
    end
    return rows
  end

  local rows = {}
  local title_width = vim.fn.strdisplaywidth(title)
  if title_width + 2 >= width then
    for _, line in ipairs(summary_lines(title, width, {})) do
      rows[#rows + 1] = { text = line, segments = { { line, "DiffReviewWalkthroughItemTitle" } } }
    end
    for _, line in ipairs(wrap_text(task.justification, width)) do
      rows[#rows + 1] = { text = line, segments = { { line } } }
    end
    return rows
  end

  local first_justification, rest = split_first_wrapped_line(task.justification, width - title_width - 1)
  local first_line = first_justification ~= "" and (title .. " " .. first_justification) or title
  local first_segments = { { title, "DiffReviewWalkthroughItemTitle" } }
  if first_justification ~= "" then
    first_segments[#first_segments + 1] = { " " }
    first_segments[#first_segments + 1] = { first_justification }
  end
  rows[#rows + 1] = { text = first_line, segments = first_segments }
  for _, line in ipairs(wrap_text(rest, width)) do
    rows[#rows + 1] = { text = line, segments = { { line } } }
  end
  return rows
end

---@param rows table[]
---@param text string
---@param opts { id: string, parent_id: string, kind?: string, fold_target_id?: string|false, default_folded?: boolean, segments?: table[], walkthrough_step?: DiffReviewWalkthroughStep, inventory_cells?: table[] }
---@param width integer
---@param item_titles string[]
local function append_status_summary_rows(rows, text, opts, width, item_titles)
  local wrapped_lines = summary_lines(text, width, item_titles)
  for wrapped_index, wrapped_line in ipairs(wrapped_lines) do
    local is_primary = wrapped_index == 1
    local id = is_primary and opts.id or ("%s:line:%d"):format(opts.id, wrapped_index)
    local kind = is_primary and (opts.kind or "pr_head_line") or "pr_head_line"
    local fold_target_id = opts.fold_target_id
    if is_primary and opts.kind == "pr_head_section" and fold_target_id == nil then
      fold_target_id = false
    elseif not is_primary and opts.kind == "pr_head_section" then
      fold_target_id = opts.id
    end
    rows[#rows + 1] = {
      text = wrapped_line,
      segments = is_primary and #wrapped_lines == 1 and opts.segments or nil,
      id = id,
      parent_id = opts.parent_id,
      kind = kind,
      fold_target_id = fold_target_id,
      default_folded = is_primary and opts.default_folded == true,
      walkthrough_step = is_primary and opts.walkthrough_step or nil,
      inventory_cells = is_primary and opts.inventory_cells or nil,
    }
  end
end

---@param path string?
---@return string
local function basename(path)
  local normalized = tostring(path or ""):gsub("\\", "/")
  return normalized:match("([^/]+)$") or normalized
end

---@param rows table[]
---@param task DiffReviewWalkthroughTask
---@param task_id string
---@param width integer
---@param item_titles string[]
local function append_status_task_body_rows(rows, task, task_id, width, item_titles)
  local subtask_prefix = walkthrough_status_body_prefix
  for subtask_index, subtask in ipairs(task.subtasks) do
    local subtask_id = ("%s:subtask:%d"):format(task_id, subtask_index)
    local subtask_is_last = subtask_index == #task.subtasks
    append_status_summary_rows(rows, subtask_prefix .. tree_branch(subtask_is_last) .. subtask.title, {
      id = subtask_id,
      parent_id = task_id,
      kind = "pr_head_section",
      default_folded = true,
    }, width, item_titles)

    local item_prefix = subtask_prefix .. tree_continuation(subtask_is_last)
    if subtask.justification then
      append_status_summary_rows(rows, item_prefix .. subtask.justification, {
        id = ("%s:justification"):format(subtask_id),
        parent_id = subtask_id,
        fold_target_id = subtask_id,
      }, width, item_titles)
    end
    for item_index, item in ipairs(subtask.items) do
      local item_id = ("%s:item:%d"):format(subtask_id, item_index)
      local item_is_last = item_index == #subtask.items
      local item_steps = item.steps or {}
      append_status_summary_rows(rows, summary_item_text(item, item_prefix, item_is_last), {
        id = item_id,
        parent_id = subtask_id,
        fold_target_id = subtask_id,
        walkthrough_step = item_steps[1],
      }, width, item_titles)
    end
  end
end

---@param state table|nil
---@return string?
local function status_inventory_signature(state)
  if type(state) ~= "table" or type(state.sections) ~= "table" then return nil end
  local parts = { tostring(state.request_id or 0) }
  for _, section in ipairs(state.sections or {}) do
    parts[#parts + 1] = tostring(section.name or "")
    for _, file in ipairs(section.files or {}) do
      parts[#parts + 1] = table.concat({
        tostring(file.filename or ""),
        tostring(file.relpath or ""),
        tostring(file.original_relpath or ""),
        tostring(file.git_status or ""),
        tostring(file.untracked == true),
        tostring(file.added or 0),
        tostring(file.removed or 0),
      }, "\t")
    end
  end
  return table.concat(parts, "\n")
end

---@param mode DiffReviewWalkthroughMode
---@param state table|nil
local function ensure_status_inventory(mode, state)
  if type(mode.host.inventory_async) ~= "function" then return end
  local signature = status_inventory_signature(state)
  if not signature or signature == mode.inventory_signature or signature == mode.inventory_pending_signature then
    return
  end

  mode.inventory_pending_signature = signature
  mode.host.inventory_async(function(result)
    vim.schedule(function()
      local active = M._modes[mode.host.buf]
      if not active or active.inventory_pending_signature ~= signature then return end
      active.inventory_pending_signature = nil
      active.inventory_signature = signature
      active.inventory = result or { rows = {} }
      if vim.api.nvim_buf_is_valid(active.host.buf) then
        active.host.rerender()
      end
    end)
  end)
end

---@param value integer
---@param prefix string
---@return string
local function inventory_count_text(value, prefix)
  value = tonumber(value) or 0
  if value == 0 then return "0" end
  return prefix .. tostring(value)
end

---@param rows DiffReviewInventoryRow[]
---@return { label: integer, added: integer, removed: integer, modified: integer }
local function inventory_layout(rows)
  local layout = { label = 0, added = 0, removed = 0, modified = 0 }
  for _, row in ipairs(rows or {}) do
    layout.label = math.max(layout.label, vim.fn.strdisplaywidth(row.label or ""))
    layout.added = math.max(layout.added, #inventory_count_text(row.added, "+"))
    layout.removed = math.max(layout.removed, #inventory_count_text(row.removed, "-"))
    layout.modified = math.max(layout.modified, #inventory_count_text(row.modified, "~"))
  end
  return layout
end

---@param text string
---@param width integer
---@return string
local function pad_right(text, width)
  return text .. (" "):rep(math.max(0, width - vim.fn.strdisplaywidth(text)))
end

---@param row DiffReviewInventoryRow
---@param layout { label: integer, added: integer, removed: integer, modified: integer }
---@return string
---@return table[] segments
local function status_inventory_cell_text(row, layout)
  local label = pad_right(row.label or "", layout.label)
  local added = inventory_count_text(row.added, "+")
  local removed = inventory_count_text(row.removed, "-")
  local modified = inventory_count_text(row.modified, "~")
  local added_cell = pad_right(added, layout.added)
  local removed_cell = pad_right(removed, layout.removed)
  local modified_cell = pad_right(modified, layout.modified)
  local text = ("%s %s %s %s"):format(label, added_cell, removed_cell, modified_cell)
  local segments = {
    { label },
    { " " },
    { added, (tonumber(row.added) or 0) ~= 0 and "DiffReviewAddRange" or nil },
    { added_cell:sub(#added + 1) },
    { " " },
    { removed, (tonumber(row.removed) or 0) ~= 0 and "DiffReviewDeleteRange" or nil },
    { removed_cell:sub(#removed + 1) },
    { " " },
    { modified, (tonumber(row.modified) or 0) ~= 0 and "DiffReviewModifyRange" or nil },
    { modified_cell:sub(#modified + 1) },
  }
  return text, segments
end

---@param rows DiffReviewInventoryRow[]
---@param layout { label: integer, added: integer, removed: integer, modified: integer }
---@return integer
local function inventory_cell_width(rows, layout)
  local width = 0
  for _, row in ipairs(rows or {}) do
    local text = status_inventory_cell_text(row, layout)
    width = math.max(width, vim.fn.strdisplaywidth(text))
  end
  return width
end

---@param segments table[]
---@param extra table[]
local function extend_segments(segments, extra)
  for _, segment in ipairs(extra or {}) do
    segments[#segments + 1] = segment
  end
end

local documentation_inventory_labels = {
  docs = true,
  plans = true,
}

---@param rows DiffReviewInventoryRow[]
---@param split_index integer
---@return DiffReviewInventoryRow[] left_rows
---@return DiffReviewInventoryRow[] right_rows
local function split_inventory_column_rows(rows, split_index)
  local left_rows = {}
  local right_rows = {}
  for index, row in ipairs(rows or {}) do
    if index <= split_index then
      left_rows[#left_rows + 1] = row
    else
      right_rows[#right_rows + 1] = row
    end
  end
  return left_rows, right_rows
end

---@param inventory_rows DiffReviewInventoryRow[]
---@return { rows: DiffReviewInventoryRow[], layout: table }[]
local function status_inventory_columns(inventory_rows)
  local regular_rows = {}
  local document_rows = {}
  for _, row in ipairs(inventory_rows or {}) do
    if documentation_inventory_labels[row.label] then
      document_rows[#document_rows + 1] = row
    else
      regular_rows[#regular_rows + 1] = row
    end
  end

  if #document_rows > 0 and #regular_rows > 0 then
    local split_index = math.ceil(#regular_rows / 2)
    local left_rows, right_rows = split_inventory_column_rows(regular_rows, split_index)
    local regular_layout = inventory_layout(regular_rows)
    local columns = {
      { rows = left_rows, layout = regular_layout },
    }
    if #right_rows > 0 then
      columns[#columns + 1] = { rows = right_rows, layout = regular_layout }
    end
    columns[#columns + 1] = { rows = document_rows, layout = inventory_layout(document_rows) }
    return columns
  end

  local split_index = math.ceil(#inventory_rows / 2)
  local left_rows, right_rows = split_inventory_column_rows(inventory_rows, split_index)
  local layout = inventory_layout(inventory_rows)
  local columns = {
    { rows = left_rows, layout = layout },
  }
  if #right_rows > 0 then
    columns[#columns + 1] = { rows = right_rows, layout = layout }
  end
  return columns
end

---@param columns { rows: DiffReviewInventoryRow[], layout: table }[]
---@return integer[]
local function inventory_column_widths(columns)
  local widths = {}
  for index, column in ipairs(columns or {}) do
    widths[index] = inventory_cell_width(column.rows, column.layout)
  end
  return widths
end

---@param columns { rows: DiffReviewInventoryRow[], layout: table }[]
---@return integer
local function inventory_display_row_count(columns)
  local count = 0
  for _, column in ipairs(columns or {}) do
    count = math.max(count, #(column.rows or {}))
  end
  return count
end

---@param widths integer[]
---@param column_index integer
---@return integer
local function inventory_column_start(widths, column_index)
  local start_col = 0
  for index = 1, column_index - 1 do
    start_col = start_col + (widths[index] or 0) + 2
  end
  return start_col
end

---@param inventory_rows DiffReviewInventoryRow[]
---@return { text: string, segments: table[], cells: table[] }[]
local function status_inventory_display_rows(inventory_rows)
  local columns = status_inventory_columns(inventory_rows)
  local widths = inventory_column_widths(columns)
  local display_rows = {}
  for row_index = 1, inventory_display_row_count(columns) do
    local text = ""
    local segments = {}
    local cells = {}
    for column_index, column in ipairs(columns) do
      local row = column.rows[row_index]
      if row then
        local target_col = inventory_column_start(widths, column_index)
        local padding = (" "):rep(math.max(0, target_col - vim.fn.strdisplaywidth(text)))
        if padding ~= "" then
          text = text .. padding
          segments[#segments + 1] = { padding }
        end

        local cell_text, cell_segments = status_inventory_cell_text(row, column.layout)
        local start_col = #text
        text = text .. cell_text
        extend_segments(segments, cell_segments)
        cells[#cells + 1] = {
          label = row.label,
          start_col = start_col,
          end_col = start_col + #cell_text,
        }
      end
    end

    if text ~= "" then
      display_rows[#display_rows + 1] = {
        text = text,
        segments = segments,
        cells = cells,
      }
    end
  end
  return display_rows
end

---@param rows table[]
---@param inventory DiffReviewInventoryResult?
---@return boolean appended
local function append_status_inventory_rows(rows, inventory)
  local inventory_rows = inventory and inventory.rows or {}
  if #inventory_rows == 0 then return false end
  rows[#rows + 1] = {
    text = "Inventory:",
    segments = { { "Inventory:", "DiffReviewStatusHeader" } },
    id = "walkthrough:inventory",
    parent_id = status_section_id,
    kind = "pr_head_line",
  }
  for index, row in ipairs(status_inventory_display_rows(inventory_rows)) do
    rows[#rows + 1] = {
      text = row.text,
      segments = row.segments,
      id = ("walkthrough:inventory:%d"):format(index),
      parent_id = status_section_id,
      kind = "pr_head_line",
      inventory_cells = row.cells,
    }
  end
  return true
end

---@param inventory_rows DiffReviewInventoryRow[]
---@return { text: string, segments: table[], cells: table[] }[]
function M._status_inventory_display_rows_for_test(inventory_rows)
  return status_inventory_display_rows(inventory_rows)
end

local inventory_titles = {
  ["function"] = "Functions changed",
  struct = "Structs changed",
  class = "Classes changed",
  interface = "Interfaces changed",
  enum = "Enums changed",
  trait = "Traits changed",
  type = "Types changed",
  module = "Modules changed",
  docs = "Docs changed",
  plans = "Plans changed",
  files = "Files changed",
}

---@param label string
---@return string
local function inventory_detail_title(label)
  return inventory_titles[label] or ((label:sub(1, 1):upper() .. label:sub(2)) .. " changed")
end

---@param left DiffReviewInventoryChange
---@param right DiffReviewInventoryChange
---@return boolean
local function inventory_change_less(left, right)
  local left_path = tostring(left.relpath or "")
  local right_path = tostring(right.relpath or "")
  if left_path ~= right_path then return left_path < right_path end
  local left_line = tonumber(left.line) or math.huge
  local right_line = tonumber(right.line) or math.huge
  if left_line ~= right_line then return left_line < right_line end
  return tostring(left.name or "") < tostring(right.name or "")
end

---@param change DiffReviewInventoryChange
---@return string
local function inventory_change_line(change)
  local name = tostring(change.name or "")
  return name ~= "" and name or basename(change.relpath)
end

---@param changes DiffReviewInventoryChange[]
---@return table<string, DiffReviewInventoryChange[]>
---@return string[]
local function inventory_changes_by_file(changes)
  local by_file = {}
  local relpaths = {}
  for _, change in ipairs(changes or {}) do
    local relpath = tostring(change.relpath or "")
    if relpath == "" then relpath = tostring(change.name or "") end
    if relpath == "" then relpath = "(unknown)" end
    if not by_file[relpath] then
      by_file[relpath] = {}
      relpaths[#relpaths + 1] = relpath
    end
    by_file[relpath][#by_file[relpath] + 1] = change
  end
  table.sort(relpaths)
  for _, relpath in ipairs(relpaths) do
    table.sort(by_file[relpath], inventory_change_less)
  end
  return by_file, relpaths
end

---@param cwd string?
---@param relpath string?
---@return string?
local function inventory_target_path(cwd, relpath)
  relpath = tostring(relpath or "")
  if relpath == "" then return nil end
  if relpath:match("^%a:[/\\]") or relpath:sub(1, 1) == "/" then return relpath end
  local root = tostring(cwd or ""):gsub("[/\\]+$", "")
  if root == "" then return relpath end
  return root .. "/" .. relpath
end

---@param buf integer
---@param force? boolean
---@return boolean restored
function M.restore_jump_return_view(buf, force)
  local saved = M._jump_return_views and M._jump_return_views[buf] or nil
  if not saved then
    jump_debug_event("restore_skip_no_saved", { buf = buf, force = force })
    return false
  end
  local win = vim.fn.bufwinid(buf)
  if win == -1 or not vim.api.nvim_win_is_valid(win) then
    jump_debug_event("restore_skip_no_window", { buf = buf, force = force, saved = saved })
    return false
  end
  local ok, cursor = pcall(vim.api.nvim_win_get_cursor, win)
  if not ok or cursor[1] ~= saved.row then
    jump_debug_event("restore_skip_cursor_mismatch", {
      buf = buf,
      force = force,
      saved = saved,
      cursor = ok and cursor or nil,
      view = jump_debug_snapshot(win, buf),
    })
    return false
  end
  if not (force or saved.active) then
    jump_debug_event("restore_skip_inactive", {
      buf = buf,
      force = force,
      saved = saved,
      view = jump_debug_snapshot(win, buf),
    })
    return false
  end
  jump_debug_event("restore_before", {
    buf = buf,
    force = force,
    saved = saved,
    view = jump_debug_snapshot(win, buf),
  })
  M._jump_return_views[buf] = nil
  pcall(vim.api.nvim_win_call, win, function()
    vim.fn.winrestview(saved.view)
  end)
  jump_debug_event("restore_after", {
    buf = buf,
    force = force,
    view = jump_debug_snapshot(win, buf),
  })
  return true
end

---@param buf integer
local function ensure_jump_return_autocmds(buf)
  if M._jump_return_autocmds and M._jump_return_autocmds[buf] then return end
  M._jump_return_autocmds = M._jump_return_autocmds or {}
  M._jump_return_autocmds[buf] = true
  local function restore_when_ready()
    local saved = M._jump_return_views and M._jump_return_views[buf] or nil
    if saved and not saved.active then
      local win = vim.fn.bufwinid(buf)
      if win ~= -1 and vim.api.nvim_win_is_valid(win) then
        local ok, cursor = pcall(vim.api.nvim_win_get_cursor, win)
        if ok and cursor[1] ~= saved.row then saved.active = true end
      end
    end
    M.restore_jump_return_view(buf)
  end
  vim.api.nvim_create_autocmd({ "BufEnter", "CursorMoved", "WinEnter" }, {
    buffer = buf,
    group = jump_return_group,
    callback = function()
      restore_when_ready()
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(buf) then restore_when_ready() end
      end)
    end,
  })
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = buf,
    group = jump_return_group,
    callback = function()
      local saved = M._jump_return_views and M._jump_return_views[buf] or nil
      if saved then saved.active = true end
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buf,
    group = jump_return_group,
    callback = function()
      if M._jump_return_views then M._jump_return_views[buf] = nil end
      if M._jump_return_autocmds then M._jump_return_autocmds[buf] = nil end
    end,
  })
end

---@param win integer
---@param buf integer
---@param cursor? integer[]
---@return boolean saved
local function save_jump_return_location(win, buf, cursor)
  if win == -1 or not vim.api.nvim_win_is_valid(win) or not vim.api.nvim_buf_is_valid(buf) then
    jump_debug_event("save_skip_invalid", { win = win, buf = buf, cursor = cursor })
    return false
  end
  local ok, source_cursor = pcall(vim.api.nvim_win_get_cursor, win)
  if not ok then
    jump_debug_event("save_skip_no_cursor", { win = win, buf = buf, cursor = cursor })
    return false
  end
  cursor = cursor or source_cursor
  jump_debug_event("save_before", {
    win = win,
    buf = buf,
    passed_cursor = cursor,
    source_cursor = source_cursor,
    view = jump_debug_snapshot(win, buf),
  })
  pcall(vim.api.nvim_win_call, win, function()
    M._jump_return_views = M._jump_return_views or {}
    M._jump_return_views[buf] = {
      row = cursor[1],
      col = cursor[2] or 0,
      view = vim.fn.winsaveview(),
      active = false,
    }
    ensure_jump_return_autocmds(buf)
    pcall(vim.cmd, "normal! m'")
    pcall(function()
      vim.fn.setpos("''", { 0, cursor[1], (cursor[2] or 0) + 1, 0 })
    end)
  end)
  jump_debug_event("save_after", {
    win = win,
    buf = buf,
    saved = M._jump_return_views and M._jump_return_views[buf] or nil,
    view = jump_debug_snapshot(win, buf),
  })
  return true
end

---@param label string
---@param details table<string, DiffReviewInventoryChangeSet>?
---@return string[]
---@return table[] folds
---@return table<integer, DiffReviewInventoryChange> row_targets
local function inventory_detail_lines(label, details)
  local row = details and details[label] or nil
  local lines = { inventory_detail_title(label), "" }
  local folds = {}
  local row_targets = {}
  local sections = {
    { key = "added", title = "Added:" },
    { key = "modified", title = "Modified:" },
    { key = "removed", title = "Deleted:" },
  }
  for _, section in ipairs(sections) do
    local changes = vim.deepcopy(row and row[section.key] or {}) or {}
    if #changes > 0 then
      if #lines > 2 then lines[#lines + 1] = "" end
      local start_line = #lines + 1
      lines[#lines + 1] = section.title
      local by_file, relpaths = inventory_changes_by_file(changes)
      for _, relpath in ipairs(relpaths) do
        lines[#lines + 1] = basename(relpath)
        local file_changes = by_file[relpath] or {}
        for _, change in ipairs(file_changes) do
          lines[#lines + 1] = "  " .. inventory_change_line(change)
          row_targets[#lines] = change
        end
      end
      folds[#folds + 1] = { start_line = start_line, end_line = #lines }
    end
  end
  if #lines == 2 then
    lines[#lines + 1] = "No changes"
  end
  return lines, folds, row_targets
end

---@param label string
---@param details table<string, DiffReviewInventoryChangeSet>?
---@return string[]
---@return table[] folds
---@return table<integer, DiffReviewInventoryChange> row_targets
function M._inventory_detail_lines_for_test(label, details)
  return inventory_detail_lines(label, details)
end

---@param buf integer
---@param folds table[]
local function apply_inventory_detail_folds(buf, folds)
  local win = vim.fn.bufwinid(buf)
  if win == -1 then return end
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldenable = true
  vim.api.nvim_win_call(win, function()
    vim.cmd("silent! normal! zE")
    for _, fold in ipairs(folds or {}) do
      if fold.end_line > fold.start_line then
        vim.cmd(("silent! %d,%dfold"):format(fold.start_line, fold.end_line))
      end
    end
    vim.cmd("silent! normal! zR")
  end)
end

---@param buf integer
---@param cwd string?
---@param row_targets table<integer, DiffReviewInventoryChange>
---@return boolean handled
local function jump_inventory_detail_target(buf, cwd, row_targets)
  local win = vim.fn.bufwinid(buf)
  if win == -1 then return false end
  local cursor = vim.api.nvim_win_get_cursor(win)
  local target = row_targets[cursor[1]]
  if not target then return false end
  local path = inventory_target_path(cwd, target.relpath)
  if not path then return false end
  save_jump_return_location(win, buf, cursor)
  pcall(vim.api.nvim_win_call, win, function()
    vim.cmd.edit(vim.fn.fnameescape(path))
    local line = math.max(1, tonumber(target.line) or 1)
    pcall(vim.api.nvim_win_set_cursor, 0, { line, 0 })
  end)
  return true
end

---@param label string
---@param details table<string, DiffReviewInventoryChangeSet>?
---@param cwd string?
local function open_inventory_detail_buffer(label, details, cwd)
  local lines, folds, row_targets = inventory_detail_lines(label, details)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "DiffReviewInventory"
  local name = ("GitInventory://%s"):format(label)
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
  end, { buffer = buf, nowait = true, silent = true, desc = "Close inventory detail" })
  vim.keymap.set("n", "<Esc>", function()
    if vim.api.nvim_buf_is_valid(buf) then pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
  end, { buffer = buf, nowait = true, silent = true, desc = "Close inventory detail" })
  vim.keymap.set("n", ".", function()
    jump_inventory_detail_target(buf, cwd, row_targets or {})
  end, { buffer = buf, nowait = true, silent = true, desc = "Open inventory item" })
  vim.cmd(("buffer %d"):format(buf))
  pcall(vim.api.nvim_buf_add_highlight, buf, M._ns, "DiffReviewStatusHeader", 0, 0, -1)
  for row_index, line in ipairs(lines) do
    if line == "Added:" then
      pcall(vim.api.nvim_buf_add_highlight, buf, M._ns, "DiffReviewAddRange", row_index - 1, 0, -1)
    elseif line == "Modified:" then
      pcall(vim.api.nvim_buf_add_highlight, buf, M._ns, "DiffReviewModifyRange", row_index - 1, 0, -1)
    elseif line == "Deleted:" then
      pcall(vim.api.nvim_buf_add_highlight, buf, M._ns, "DiffReviewDeleteRange", row_index - 1, 0, -1)
    end
  end
  apply_inventory_detail_folds(buf, folds)
end

---@param entry table
---@param cursor_col integer
---@return string?
local function inventory_label_at_cursor(entry, cursor_col)
  for _, cell in ipairs(entry.inventory_cells or {}) do
    if cursor_col >= (cell.start_col or 0) and cursor_col < (cell.end_col or 0) then
      return cell.label
    end
  end
  return nil
end

---@param rows table[]
---@param flow DiffReviewWalkthroughFlowNode[]
---@param width integer
local function append_status_flow_rows(rows, flow, width)
  for index, line in ipairs(flow_summary_lines(flow, width)) do
    rows[#rows + 1] = {
      text = line,
      id = ("walkthrough:flow:%d"):format(index),
      parent_id = status_section_id,
      kind = "pr_head_line",
    }
  end
end

---@param mode DiffReviewWalkthroughMode
---@return table[] rows
local function status_summary_rows(mode)
  local width = status_render_width(mode)
  local item_titles = collect_summary_item_titles(mode.doc.tasks)
  local state = mode.host.get_state()
  ensure_status_inventory(mode, state)
  local rows = {}

  for _, line in ipairs(summary_lines(vim.trim(mode.doc.overview or ""), width, item_titles)) do
    rows[#rows + 1] = { text = line, parent_id = status_section_id }
  end
  local has_overview = #rows > 0
  local has_flow = mode.doc.flow and #mode.doc.flow > 0
  if has_flow then
    if has_overview then
      rows[#rows + 1] = { text = "", parent_id = status_section_id }
    end
    append_status_flow_rows(rows, mode.doc.flow, width)
  end

  local has_inventory = mode.inventory and #(mode.inventory.rows or {}) > 0
  if (has_overview or has_flow) and #(mode.doc.tasks or {}) > 0 then
    rows[#rows + 1] = { text = "", parent_id = status_section_id }
  end

  for task_index, task in ipairs(mode.doc.tasks or {}) do
    local task_id = ("walkthrough:task:%d"):format(task_index)
    local heading_rows = status_task_heading_rows(task_index, task, width)
    for heading_index, heading_row in ipairs(heading_rows) do
      local anchors_task_fold = heading_index == #heading_rows
      rows[#rows + 1] = {
        text = heading_row.text,
        segments = heading_row.segments,
        id = anchors_task_fold and task_id or ("%s:heading:%d"):format(task_id, heading_index),
        parent_id = status_section_id,
        kind = anchors_task_fold and "pr_head_section" or "pr_head_line",
        fold_target_id = anchors_task_fold and false or task_id,
        default_folded = anchors_task_fold,
      }
    end

    append_status_task_body_rows(rows, task, task_id, width, item_titles)
    if task_index < #(mode.doc.tasks or {}) then
      rows[#rows + 1] = {
        text = "",
        id = ("%s:separator"):format(task_id),
        parent_id = task_id,
        kind = "pr_head_line",
        fold_target_id = task_id,
      }
    end
  end

  if has_inventory then
    if #rows > 0 then
      rows[#rows + 1] = { text = "", parent_id = status_section_id }
    end
    append_status_inventory_rows(rows, mode.inventory)
  end

  return rows
end

---@param buf integer
---@param head_lines table[]
---@return table[]
function M.status_head_lines(buf, head_lines)
  local mode = M._modes[buf]
  if not mode then return head_lines end

  mode.host.set_comment_index(build_comment_index(mode, mode.host.get_state()))

  local lines = {}
  vim.list_extend(lines, head_lines or {})
  lines[#lines + 1] = { segments = { { "" } } }
  lines[#lines + 1] = {
    segments = { { "Walkthrough:", "DiffReviewStatusHeader" } },
    default_folded = false,
    entry = { id = status_section_id, kind = "pr_head_section" },
  }

  local summary_index = 0
  local child_index = 0
  local function append_child(text, segments, id, parent_id, fold_target_id, kind, default_folded, walkthrough_step, inventory_cells)
    child_index = child_index + 1
    local entry_kind = kind or "pr_head_line"
    local entry_fold_target_id = nil
    if fold_target_id ~= false then
      entry_fold_target_id = fold_target_id
      if entry_fold_target_id == nil and entry_kind ~= "pr_head_section" then
        entry_fold_target_id = status_section_id
      end
    end
    lines[#lines + 1] = {
      segments = segments or { { text } },
      parent_id = parent_id or status_section_id,
      default_folded = default_folded == true,
      entry = {
        id = id or ("walkthrough:child:%d"):format(child_index),
        kind = entry_kind,
        fold_target_id = entry_fold_target_id,
        default_folded = default_folded == true,
        walkthrough_summary_line = text,
        walkthrough_step = walkthrough_step,
        inventory_cells = inventory_cells,
      },
    }
  end

  if mode.stale then
    append_child("", { { ("WARNING: generated against %s, HEAD is now %s"):format(
      mode.doc.commit:sub(1, 7),
      mode.head_sha and mode.head_sha:sub(1, 7) or "unknown"
    ), "DiffReviewWalkthroughStale" } }, "walkthrough:stale")
  end
  for _, row in ipairs(status_summary_rows(mode)) do
    summary_index = summary_index + 1
    append_child(
      row.text,
      row.segments,
      row.id or ("walkthrough:summary:%d"):format(summary_index),
      row.parent_id,
      row.fold_target_id,
      row.kind,
      row.default_folded,
      row.walkthrough_step,
      row.inventory_cells
    )
  end
  return lines
end

---@param mode DiffReviewWalkthroughMode
---@param state table?
local function apply_status_summary_highlights(mode, state)
  local entries = state and state.entries or {}
  for row, entry in pairs(entries) do
    if entry and entry.walkthrough_summary_line then
      apply_summary_highlights(mode.host.buf, { entry.walkthrough_summary_line }, mode.doc, row - 1)
    end
  end
end

---@param mode DiffReviewWalkthroughMode
---@param step DiffReviewWalkthroughStep
---@param state table?
---@return DiffReviewWalkthroughTarget
local function rendered_step_target(mode, step, state)
  local comment_id = step_comment_id(step)
  local match = mode.anchor_match_by_id and mode.anchor_match_by_id[comment_id] or "missing"
  local record = state and state.comment_box_record_by_id and state.comment_box_record_by_id[comment_id] or nil
  if record then
    return { match = match, start_row = record.anchor_line, end_row = record.anchor_line }
  end
  if match == "file_only" then
    for row, entry in pairs(state and state.entries or {}) do
      local file = entry and entry.file
      if entry and entry.kind == "file" and file
        and (matches_file(file.relpath, step.file) or matches_file(file.filename, step.file)) then
        return { match = "file_only", start_row = row, end_row = row }
      end
    end
  end
  return { match = match }
end

---@param mode DiffReviewWalkthroughMode
---@param step DiffReviewWalkthroughStep
---@return DiffReviewWalkthroughTarget target
local function resolve_visible_step_target(mode, step)
  local state = mode.host.get_state()
  local target = rendered_step_target(mode, step, state)
  local function target_row_is_folded(candidate)
    local win = vim.fn.bufwinid(mode.host.buf)
    if win == -1 or not candidate.start_row then return false end
    return vim.api.nvim_win_call(win, function()
      return vim.fn.foldclosed(candidate.start_row) ~= -1
    end)
  end
  jump_debug_event("resolve_initial", {
    buf = mode.host.buf,
    step = {
      file = step.file,
      title = step.title,
      start_line = step.start_pos and step.start_pos.line or nil,
      end_line = step.end_pos and step.end_pos.line or nil,
    },
    target = target,
    view = jump_debug_snapshot(vim.fn.bufwinid(mode.host.buf), mode.host.buf),
  })
  -- Only unfold + re-render when no diff rows are rendered at all; a
  -- "nearest" match means the hunks are visible and just don't contain the
  -- exact line (TS-context scoping, deletions, staleness).
  if (not target.start_row or target_row_is_folded(target)) and ensure_expanded(mode, step) then
    jump_debug_event("resolve_after_expand", {
      buf = mode.host.buf,
      previous_target = target,
      view = jump_debug_snapshot(vim.fn.bufwinid(mode.host.buf), mode.host.buf),
    })
    state = mode.host.get_state()
    target = rendered_step_target(mode, step, state)
  end
  jump_debug_event("resolve_final", {
    buf = mode.host.buf,
    target = target,
    view = jump_debug_snapshot(vim.fn.bufwinid(mode.host.buf), mode.host.buf),
  })
  return target
end

---@param buf integer
---@param row? integer
---@return boolean handled
function M.jump_status_step(buf, row)
  local mode = M._modes[buf]
  if not mode then return false end
  local state = mode.host.get_state()
  if not (state and state.entries) then return false end
  local win = vim.fn.bufwinid(buf)
  if win == -1 then return false end

  local cursor_row = row or vim.api.nvim_win_get_cursor(win)[1]
  local entry = state.entries[cursor_row]
  local cursor = vim.api.nvim_win_get_cursor(win)
  jump_debug_event("jump_status_start", {
    buf = buf,
    row = cursor_row,
    cursor = cursor,
    entry = entry and {
      id = entry.id,
      kind = entry.kind,
      has_step = entry.walkthrough_step ~= nil,
      has_inventory = entry.inventory_cells ~= nil,
    } or nil,
    view = jump_debug_snapshot(win, buf),
  })
  local inventory_label = entry and inventory_label_at_cursor(entry, cursor[2])
  if inventory_label then
    jump_debug_event("jump_status_inventory", { buf = buf, label = inventory_label, cursor = cursor })
    save_jump_return_location(win, buf, cursor)
    open_inventory_detail_buffer(inventory_label, mode.inventory and mode.inventory.details or nil, mode.host.cwd())
    return true
  end

  local step = entry and entry.walkthrough_step or nil
  if not step then return false end

  local source_cursor = cursor
  local target = resolve_visible_step_target(mode, step)
  jump_debug_event("jump_status_target", {
    buf = buf,
    source_cursor = source_cursor,
    target = target,
    view = jump_debug_snapshot(win, buf),
  })
  if target.start_row then
    save_jump_return_location(win, buf, source_cursor)
    pcall(vim.api.nvim_win_call, win, function()
      vim.cmd(("normal! %dG0"):format(target.start_row))
      vim.cmd("normal! zz")
    end)
    jump_debug_event("jump_status_after_move", {
      buf = buf,
      target = target,
      view = jump_debug_snapshot(win, buf),
    })
  else
    notify("Walkthrough target is not in the current diff", vim.log.levels.WARN)
    jump_debug_event("jump_status_missing", { buf = buf, target = target })
  end
  return true
end

---@param target DiffReviewWalkthroughTarget
---@param win integer
---@return boolean
---@param mode DiffReviewWalkthroughMode
---@param row integer
---@return string
local function region_highlight_group(mode, row)
  local state = mode.host.get_state()
  local entry = state and state.entries and state.entries[row] or nil
  local prefix = entry and entry.diff_line and entry.diff_line.prefix or nil
  if prefix == "+" then return "DiffReviewWalkthroughRegionAdd" end
  if prefix == "-" then return "DiffReviewWalkthroughRegionDelete" end
  return "DiffReviewWalkthroughRegion"
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
      line_hl_group = region_highlight_group(mode, row),
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
  local target = resolve_visible_step_target(mode, step)

  local win = vim.fn.bufwinid(buf)
  if win == -1 then
    M.stop(buf)
    notify("Status window is no longer visible", vim.log.levels.WARN)
    return
  end

  if target.start_row then
    pcall(vim.api.nvim_win_set_cursor, win, { target.start_row, 0 })
  end
  if target.match == "missing" then
    -- Anchor the box at the cursor's current row; no region to highlight.
    local cursor_row = vim.api.nvim_win_get_cursor(win)[1]
    target = { match = "missing", start_row = cursor_row, end_row = cursor_row }
  end
  apply_region_highlight(mode, target)
  local state = mode.host.get_state()
  local record = state and state.comment_box_record_by_id and state.comment_box_record_by_id[step_comment_id(step)] or nil
  if record then
    focus_comment_box_below(win, target, {
      anchor_row = record.anchor_line,
      line_count = math.max(1, (record.end_line or record.start_line) - record.start_line + 1),
    })
  elseif target.start_row then
    pcall(vim.api.nvim_win_call, win, function() vim.cmd("normal! zz") end)
  end
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
    callback = function() M.stop(buf, { rerender = false }) end,
  })

  show_step(mode, 1)
end

--- Exit the walkthrough: clear decorations, close the float, restore the
--- original buffer-local mappings (q is the status close mapping).
---@param buf integer
---@param opts? { rerender?: boolean }
function M.stop(buf, opts)
  opts = opts or {}
  local mode = M._modes[buf]
  if not mode then return end
  M._modes[buf] = nil
  if mode.host then mode.host.set_comment_index(nil) end

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
  if opts.rerender ~= false and mode.host and vim.api.nvim_buf_is_valid(buf) then
    mode.host.rerender()
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
  local item_titles = collect_summary_item_titles(mode.doc.tasks)
  local lines = summary_lines(mode.doc.summary, width - 4, item_titles)
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
  apply_summary_highlights(popup_buf, lines, mode.doc)
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
--- repo root, check staleness against HEAD, and rerender the integrated summary.
---@param host DiffReviewWalkthroughHost
function M.start(host)
  if M._modes[host.buf] then
    M.stop(host.buf, { rerender = true })
    return
  end

  host.resolve_root_async(function(root, root_err)
    if not vim.api.nvim_buf_is_valid(host.buf) then return end
    if not root then
      notify(root_err or "No repository loaded", vim.log.levels.WARN)
      return
    end

    local path = root .. "/.walkthrough.json"
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

    host.git_list_async({ "git", "-C", root, "rev-parse", "HEAD" }, function(output, code)
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
      for task_index, task in ipairs(doc.tasks or {}) do
        local task_id = ("walkthrough:task:%d"):format(task_index)
        host.set_folded(task_id, true)
        for subtask_index, _ in ipairs(task.subtasks or {}) do
          host.set_folded(("%s:subtask:%d"):format(task_id, subtask_index), true)
        end
      end
      M._modes[host.buf] = mode
      local initial_target_id = #(doc.flow or {}) > 0 and "walkthrough:flow:1" or status_section_id
      host.rerender(initial_target_id)
    end)
  end)
end

--- Re-apply decorations after the status buffer re-renders (rows shift).
--- Never moves the cursor; the render path owns cursor restoration.
---@param buf integer
function M.on_status_rendered(buf)
  local mode = M._modes[buf]
  if not mode then return end
  vim.schedule(function()
    local active = M._modes[buf]
    if not active then return end
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local win = vim.fn.bufwinid(buf)
    if win == -1 then return end
    vim.api.nvim_buf_clear_namespace(buf, M._ns, 0, -1)
    local state = active.host.get_state()
    apply_status_summary_highlights(active, state)
    if active.index == 0 then return end
    local step = active.doc.steps[active.index]
    local target = rendered_step_target(active, step, state)
    if target.match == "missing" then
      local cursor_row = vim.api.nvim_win_get_cursor(win)[1]
      target = { match = "missing", start_row = cursor_row, end_row = cursor_row }
    end
    apply_region_highlight(active, target)
  end)
end

return M
