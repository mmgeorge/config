-- Guided review walkthrough for the DiffReviewStatus buffer. An LLM writes
-- .walkthrough.json at the repo root (see walkthrough.schema.json next to this
-- file); `ow` in the status buffer embeds the walkthrough presentation, then
-- z/y step backward/forward through the referenced regions inside the inline
-- diff, with the author's comment rendered as an inline virt_lines box below
-- each region.
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
---@field callout? DiffReviewWalkthroughCallout
---@field task_index integer
---@field step_index integer
---@field task_step_total integer
---@field label string
---@field task_title string
---@field subtask_title string
---@field item_title string

---@class DiffReviewWalkthroughCallout
---@field kind string
---@field text string

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

---@class DiffReviewWalkthroughGroup
---@field type string
---@field title string
---@field subtasks DiffReviewWalkthroughSubtask[]

---@class DiffReviewWalkthroughTask
---@field title string
---@field justification? string
---@field groups DiffReviewWalkthroughGroup[]

---@class DiffReviewWalkthroughStepContext
---@field task_index integer
---@field step_index integer
---@field task_title string
---@field subtask_title string
---@field item_title string

---@class DiffReviewWalkthroughDoc
---@field version integer
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
---@field get_state fun(): table? current status state (lines, entries, sections)
---@field file_key fun(section_name: string, filename: string): string
---@field hunk_key fun(section_name: string, filename: string, diff?: string): string
---@field set_folded fun(key: string, folded: boolean)
---@field rerender fun() synchronous reuse_sections re-render
---@field git_list_async fun(command: string[], cb: fun(output: string[], code: integer))
---@field set_walkthrough_presentation? fun(doc: DiffReviewWalkthroughDoc, stale: boolean, head_sha?: string)
---@field clear_walkthrough_presentation? fun()

---@alias DiffReviewWalkthroughReader fun(path: string): string?

---@class DiffReviewWalkthroughMode
---@field host DiffReviewWalkthroughHost
---@field doc DiffReviewWalkthroughDoc
---@field index integer current step index, 1..n after navigation starts
---@field stale boolean
---@field head_sha? string
---@field saved_maps table<string, table> maparg dicts to restore on stop
---@field nav_started? boolean

---@class DiffReviewWalkthroughCommentBoxPlacement
---@field anchor_row integer
---@field line_count integer

---@class DiffReviewWalkthroughModule
---@field _modes table<integer, DiffReviewWalkthroughMode>
---@field _reader DiffReviewWalkthroughReader?
---@field _ns integer
local M = {
  _modes = {},
  _ns = vim.api.nvim_create_namespace("diff_review_walkthrough"),
}

local nav_keys = { "z", "y", "q", "<Esc>" }
local comment_box_max_inner_width = 84

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

---@param value any
---@param error_prefix string
---@param field string
---@param max_length? integer
---@return string? parsed
---@return string? error
local function parse_optional_string_field(value, error_prefix, field, max_length)
  if value == nil then return nil, nil end
  if not is_non_empty_string(value) then
    return nil, ("%s: invalid \"%s\""):format(error_prefix, field)
  end
  if max_length and vim.fn.strchars(value) > max_length then
    return nil, ("%s: \"%s\" must be %d characters or less"):format(error_prefix, field, max_length)
  end
  return value, nil
end

local action_order = { "Add", "Update", "Move", "Remove", "Split" }
local group_type_order = { "Module", "File", "Package", "Directory" }
local callout_text_max_length = 180
local valid_item_actions = {
  Add = true,
  Update = true,
  Move = true,
  Remove = true,
  Split = true,
}
local item_note_max_length = 50
local justification_max_length = 80
local valid_group_types = {
  Module = true,
  File = true,
  Package = true,
  Directory = true,
}
local valid_callout_kinds = {
  important = true,
  limitation = true,
  temporary = true,
  risk = true,
  followup = true,
  deviation = true,
  workaround = true,
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
  Update = "DiffReviewWalkthroughActionUpdate",
  Move = "DiffReviewWalkthroughActionMove",
  Remove = "DiffReviewWalkthroughActionRemove",
  Split = "DiffReviewWalkthroughActionSplit",
}
local callout_labels = {
  important = "Important",
  limitation = "Limitation",
  temporary = "Temporary",
  risk = "Risk",
  followup = "Follow-up",
  deviation = "Deviation",
  workaround = "Workaround",
}
local callout_highlights = {
  important = "DiffReviewWalkthroughCalloutImportant",
  limitation = "DiffReviewWalkthroughCalloutLimitation",
  temporary = "DiffReviewWalkthroughCalloutTemporary",
  risk = "DiffReviewWalkthroughCalloutRisk",
  followup = "DiffReviewWalkthroughCalloutFollowup",
  deviation = "DiffReviewWalkthroughCalloutDeviation",
  workaround = "DiffReviewWalkthroughCalloutWorkaround",
}
local action_display_labels = {
  Add = "Add",
  Update = "Modify",
  Move = "Move",
  Remove = "Remove",
  Split = "Split",
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

---@param action string
---@return string
local function format_action(action)
  return action_display_labels[action] or action
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
  if is_non_empty_string(item_subtype) then return item_subtype end
  return format_type_keyword(item_type)
end

---@param lines string[]
---@param item DiffReviewWalkthroughItem
---@param prefix string
---@param is_last boolean
local function append_summary_item(lines, item, prefix, is_last)
  lines[#lines + 1] = prefix
    .. tree_branch(is_last)
    .. format_action(item.action)
    .. " "
    .. format_item_type_label(item.type, item.subtype)
    .. " "
    .. item.title
    .. " to "
    .. item.note
end

---@param item DiffReviewWalkthroughItem
---@param prefix string
---@param is_last boolean
---@return table[] segments
local function task_item_segments(item, prefix, is_last)
  local action_hl = action_highlights[item.action] or "DiffReviewWalkthroughActionUpdate"
  return {
    { prefix .. tree_branch(is_last) },
    { format_action(item.action), action_hl },
    { " " },
    { format_item_type_label(item.type, item.subtype), action_hl },
    { " " },
    { item.title, "DiffReviewWalkthroughItemTitle" },
    { " to " .. item.note },
  }
end

---@param task DiffReviewWalkthroughTask
---@return table[] rows
function M.task_presentation_rows(task)
  local rows = {}
  if task.justification then
    rows[#rows + 1] = { segments = { { " " .. task.justification, "Comment" } } }
  end
  for _, group in ipairs(task.groups or {}) do
    rows[#rows + 1] = {
      segments = {
        { " " },
        { format_type_keyword(group.type), "DiffReviewWalkthroughType" },
        { " " },
        { group.title, "DiffReviewWalkthroughItemTitle" },
      },
    }
    local subtask_prefix = "    "
    for subtask_index, subtask in ipairs(group.subtasks or {}) do
      local subtask_is_last = subtask_index == #(group.subtasks or {})
      rows[#rows + 1] = {
        segments = {
          { subtask_prefix .. tree_branch(subtask_is_last) },
          { subtask.title, "DiffReviewWalkthroughItemTitle" },
        },
      }
      local item_prefix = subtask_prefix .. tree_continuation(subtask_is_last)
      if subtask.justification then
        rows[#rows + 1] = { segments = { { item_prefix .. subtask.justification, "Comment" } } }
      end
      for item_index, item in ipairs(subtask.items or {}) do
        local item_is_last = item_index == #(subtask.items or {})
        rows[#rows + 1] = { segments = task_item_segments(item, item_prefix, item_is_last) }
      end
    end
  end
  return rows
end

---@param tasks DiffReviewWalkthroughTask[]
---@return { type: string, subtype: string?, label: string, title: string }[] specs
local function collect_summary_item_specs(tasks)
  local specs = {}
  for _, task in ipairs(tasks) do
    for _, group in ipairs(task.groups) do
      for _, subtask in ipairs(group.subtasks) do
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
    for _, group in ipairs(task.groups) do
      for _, subtask in ipairs(group.subtasks) do
        for _, item in ipairs(subtask.items) do
          titles[#titles + 1] = item.title
        end
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
    if task.justification then justifications[#justifications + 1] = task.justification end
    for _, group in ipairs(task.groups) do
      for _, subtask in ipairs(group.subtasks) do
        if subtask.justification then justifications[#justifications + 1] = subtask.justification end
      end
    end
  end
  table.sort(justifications, function(left, right) return #left > #right end)
  return justifications
end

---@param overview string
---@param _root string
---@param tasks DiffReviewWalkthroughTask[]
---@return string
local function build_summary(overview, _root, tasks)
  local lines = {}
  vim.list_extend(lines, vim.split(vim.trim(overview), "\n", { plain = true }))
  lines[#lines + 1] = ""
  for task_index, task in ipairs(tasks) do
    if task_index > 1 then lines[#lines + 1] = "" end
    lines[#lines + 1] = ("%d. %s"):format(task_index, task.title)
    local task_body_prefix = " "
    if task.justification then lines[#lines + 1] = task_body_prefix .. task.justification end
    for _, group in ipairs(task.groups) do
      local group_prefix = task_body_prefix
      lines[#lines + 1] = group_prefix .. format_type_keyword(group.type) .. " " .. group.title
      local subtask_prefix = group_prefix .. "   "
      for subtask_index, subtask in ipairs(group.subtasks) do
        local subtask_is_last = subtask_index == #group.subtasks
        lines[#lines + 1] = subtask_prefix .. tree_branch(subtask_is_last) .. subtask.title
        local item_prefix = subtask_prefix .. tree_continuation(subtask_is_last)
        if subtask.justification then lines[#lines + 1] = item_prefix .. subtask.justification end
        for item_index, item in ipairs(subtask.items) do
          local item_is_last = item_index == #subtask.items
          append_summary_item(lines, item, item_prefix, item_is_last)
        end
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

  if raw_step.callouts ~= nil then
    return nil, error_prefix .. ": \"callouts\" is not supported; use singular \"callout\""
  end
  local callout = nil
  if raw_step.callout ~= nil then
    if type(raw_step.callout) ~= "table" then
      return nil, error_prefix .. ": invalid \"callout\""
    end
    if type(raw_step.callout.kind) ~= "string" or not valid_callout_kinds[raw_step.callout.kind] then
      return nil, error_prefix .. ": callout has invalid \"kind\""
    end
    if not is_non_empty_string(raw_step.callout.text) then
      return nil, error_prefix .. ": callout has missing \"text\""
    end
    if vim.fn.strchars(raw_step.callout.text) > callout_text_max_length then
      return nil, ("%s: callout \"text\" must be %d characters or less"):format(error_prefix,
        callout_text_max_length)
    end
    callout = {
      kind = raw_step.callout.kind,
      text = raw_step.callout.text,
    }
  end

  return {
    file = (raw_step.file:gsub("\\", "/")),
    start_pos = start_pos,
    end_pos = end_pos,
    comment = raw_step.comment,
    title = type(raw_step.title) == "string" and raw_step.title or nil,
    callout = callout,
    task_index = context.task_index,
    step_index = context.step_index,
    label = ("Task %d.%d"):format(context.task_index, context.step_index),
    task_title = context.task_title,
    subtask_title = context.subtask_title,
    item_title = context.item_title,
  }
end

---@param raw_item any
---@param error_prefix string
---@param base_context table
---@param task_step_index integer
---@param all_steps DiffReviewWalkthroughStep[]
---@return DiffReviewWalkthroughItem? item
---@return integer task_step_index
---@return string? error
local function parse_item(raw_item, error_prefix, base_context, task_step_index, all_steps)
  if type(raw_item) ~= "table" then
    return nil, task_step_index, error_prefix .. ": missing item object"
  end
  if type(raw_item.action) ~= "string" or not valid_item_actions[raw_item.action] then
    return nil, task_step_index, error_prefix .. ": missing or invalid \"action\""
  end
  if not is_non_empty_string(raw_item.title) then
    return nil, task_step_index, error_prefix .. ": missing \"title\""
  end
  if not is_non_empty_string(raw_item.note) then
    return nil, task_step_index, error_prefix .. ": missing \"note\""
  end
  if vim.fn.strchars(raw_item.note) > item_note_max_length then
    return nil, task_step_index, ("%s: \"note\" must be %d characters or less"):format(error_prefix,
      item_note_max_length)
  end

  if type(raw_item.type) ~= "string" or not valid_item_types[raw_item.type] then
    return nil, task_step_index, error_prefix .. ": missing or invalid \"type\""
  end
  local item_subtype, subtype_error = parse_optional_string_field(raw_item.subtype, error_prefix, "subtype")
  if subtype_error then return nil, task_step_index, subtype_error end
  if raw_item.children ~= nil then
    return nil, task_step_index, error_prefix .. ": \"children\" is not supported; use a subtask or sibling item"
  end
  if type(raw_item.steps) ~= "table" or #raw_item.steps == 0 then
    return nil, task_step_index, error_prefix .. ": missing or empty \"steps\""
  end

  ---@type DiffReviewWalkthroughStep[]
  local item_steps = {}
  for step_offset, raw_step in ipairs(raw_item.steps) do
    task_step_index = task_step_index + 1
    local step, parse_error = parse_step(raw_step, ("%s step %d"):format(error_prefix, step_offset), {
      task_index = base_context.task_index,
      step_index = task_step_index,
      task_title = base_context.task_title,
      subtask_title = base_context.subtask_title,
      item_title = raw_item.title,
    })
    if not step then return nil, task_step_index, parse_error end
    item_steps[#item_steps + 1] = step
    all_steps[#all_steps + 1] = step
  end

  return {
    action = raw_item.action,
    type = raw_item.type,
    subtype = item_subtype,
    title = raw_item.title,
    note = raw_item.note,
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
  if type(raw_subtask.items) ~= "table" or #raw_subtask.items == 0 then
    return nil, task_step_index, error_prefix .. ": missing or empty \"items\""
  end
  local justification, justification_error = parse_optional_string_field(raw_subtask.justification, error_prefix,
    "justification", justification_max_length)
  if justification_error then return nil, task_step_index, justification_error end

  ---@type DiffReviewWalkthroughItem[]
  local items = {}
  for item_index, raw_item in ipairs(raw_subtask.items) do
    local item_prefix = ("%s item %d"):format(error_prefix, item_index)
    local item, next_step_index, item_error = parse_item(raw_item, item_prefix, {
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

--- Tolerantly validate and normalize a decoded walkthrough document.
---@param decoded any
---@return DiffReviewWalkthroughDoc? doc
---@return string? error
local function parse_doc(decoded)
  if type(decoded) ~= "table" then
    return nil, "document is not a JSON object"
  end
  if tonumber(decoded.version) ~= 7 then
    return nil, "unsupported \"version\" (expected 7)"
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
    if type(raw_task.groups) ~= "table" or #raw_task.groups == 0 then
      return nil, task_prefix .. ": missing or empty \"groups\""
    end
    local task_justification, task_justification_error = parse_optional_string_field(raw_task.justification,
      task_prefix, "justification", justification_max_length)
    if task_justification_error then return nil, task_justification_error end

    local task_step_index = 0
    local task_first_step_offset = #steps + 1
    ---@type DiffReviewWalkthroughGroup[]
    local groups = {}
    for group_index, raw_group in ipairs(raw_task.groups) do
      local group_prefix = ("%s group %d"):format(task_prefix, group_index)
      if type(raw_group) ~= "table" or not is_non_empty_string(raw_group.title) then
        return nil, group_prefix .. ": missing \"title\""
      end
      if type(raw_group.type) ~= "string" or not valid_group_types[raw_group.type] then
        return nil, group_prefix .. ": missing or invalid \"type\""
      end
      if type(raw_group.subtasks) ~= "table" or #raw_group.subtasks == 0 then
        return nil, group_prefix .. ": missing or empty \"subtasks\""
      end

      ---@type DiffReviewWalkthroughSubtask[]
      local subtasks = {}
      for subtask_index, raw_subtask in ipairs(raw_group.subtasks) do
        local subtask_prefix = ("%s subtask %d"):format(group_prefix, subtask_index)
        local subtask, next_step_index, subtask_error = parse_subtask(raw_subtask, subtask_prefix, {
          task_index = task_index,
          task_title = raw_task.title,
        }, task_step_index, steps)
        if not subtask then return nil, subtask_error end
        task_step_index = next_step_index
        subtasks[#subtasks + 1] = subtask
      end
      groups[#groups + 1] = {
        type = raw_group.type,
        title = raw_group.title,
        subtasks = subtasks,
      }
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
      groups = groups,
    }
  end

  return {
    version = 7,
    overview = decoded.overview,
    root = decoded.root,
    summary = build_summary(decoded.overview, decoded.root, tasks),
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
  for _, entry in pairs(state and state.entries or {}) do
    if entry_matches_file(entry, step.file) then
      if entry.kind == "section" and entry.id then
        mode.host.set_folded(entry.id, false)
        expanded = true
      elseif entry.kind == "file" and entry.id then
        mode.host.set_folded(entry.id, false)
        expanded = true
      elseif entry.kind == "hunk" and entry.id then
        mode.host.set_folded(entry.id, false)
        expanded = true
      end
    end
  end
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
  if stale then
    return "(walkthrough predates HEAD - positions may be stale)"
  end
  return nil
end

---@param content { text: string, hl: string, chunks?: { text: string, hl: string }[] }[]
---@param text string
---@param hl string
local function append_comment_row(content, text, hl)
  content[#content + 1] = { text = text, hl = hl }
end

---@param content { text: string, hl: string, chunks?: { text: string, hl: string }[] }[]
---@param callout DiffReviewWalkthroughCallout
---@param inner_width integer
local function append_callout_rows(content, callout, inner_width)
  local label = (callout_labels[callout.kind] or callout.kind) .. ": "
  local label_width = vim.fn.strdisplaywidth(label)
  local text_width = math.max(inner_width - 2 - label_width, 8)
  local label_hl = callout_highlights[callout.kind] or "DiffReviewWalkthroughCalloutImportant"
  for index, line in ipairs(wrap_text(callout.text, text_width)) do
    if index == 1 then
      content[#content + 1] = {
        text = label .. line,
        hl = "DiffReviewWalkthroughComment",
        chunks = {
          { text = label, hl = label_hl },
          { text = line, hl = "DiffReviewWalkthroughComment" },
        },
      }
    else
      local prefix = (" "):rep(label_width)
      content[#content + 1] = {
        text = prefix .. line,
        hl = "DiffReviewWalkthroughComment",
        chunks = {
          { text = prefix, hl = "DiffReviewWalkthroughComment" },
          { text = line, hl = "DiffReviewWalkthroughComment" },
        },
      }
    end
  end
end

---@param step DiffReviewWalkthroughStep
---@return string
local function format_step_heading_label(step)
  local task_step_total = step.task_step_total or step.step_index
  return ("%d.%d-%d"):format(step.task_index, step.step_index, task_step_total)
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
local function comment_box_anchor_row(target, win_height, comment_line_count)
  local start_row = target.start_row or 1
  local end_row = target.end_row or start_row
  if end_row < start_row then end_row = start_row end

  local target_screen_row = comment_box_start_screen_row(win_height, comment_line_count)
  local max_anchor_offset = math.max(target_screen_row - 2, 0)
  local latest_anchor_with_start_visible = start_row + max_anchor_offset
  return math.max(start_row, math.min(end_row, latest_anchor_with_start_visible))
end

--- Render the per-step comment as an inline virt_lines box below the region:
--- it scrolls with the buffer and pushes following lines down instead of
--- overlapping them. Everything lives in the walkthrough namespace, so a
--- namespace clear removes the box together with the region highlight.
---@param mode DiffReviewWalkthroughMode
---@param step DiffReviewWalkthroughStep
---@param target DiffReviewWalkthroughTarget
---@param win integer
---@return DiffReviewWalkthroughCommentBoxPlacement placement
local function render_comment_box(mode, step, target, win)
  local buf = mode.host.buf
  local win_width = vim.api.nvim_win_get_width(win)
  local inner_width = math.max(30, math.min(comment_box_max_inner_width, win_width - 8))

  ---@type { text: string, hl: string, chunks?: { text: string, hl: string }[] }[] content rows (without borders)
  local content = {}
  for _, line in ipairs(wrap_text(step.comment, inner_width - 2)) do
    append_comment_row(content, line, "DiffReviewWalkthroughComment")
  end
  if step.callout then
    append_comment_row(content, "", "DiffReviewWalkthroughComment")
    append_callout_rows(content, step.callout, inner_width)
  end
  local note = staleness_note(target, mode.stale)
  if note then
    append_comment_row(content, "", "DiffReviewWalkthroughComment")
    append_comment_row(content, note, "DiffReviewWalkthroughStale")
  end
  -- Header: "3.1-4 title" left-aligned; the body carries the mini-justification.
  local heading_title = step.title or step.item_title
  local heading_label = format_step_heading_label(step)
  local heading = (" %s%s "):format(heading_label, heading_title and (" " .. heading_title) or "")
  for _, row in ipairs(content) do
    inner_width = math.max(inner_width, vim.fn.strdisplaywidth(row.text) + 2)
  end
  local heading_width = vim.fn.strdisplaywidth(heading)
  inner_width = math.max(inner_width, heading_width + 3)

  local pad = "  "
  local virt_lines = {}
  virt_lines[#virt_lines + 1] = {
    { pad .. "╭─", "FloatBorder" },
    { heading, "DiffReviewWalkthroughItemTitle" },
    { ("─"):rep(math.max(inner_width - heading_width - 2, 0)), "FloatBorder" },
    { "─╮", "FloatBorder" },
  }
  for _, row in ipairs(content) do
    local fill = (" "):rep(math.max(inner_width - vim.fn.strdisplaywidth(row.text) - 2, 0))
    local virt_line = { { pad .. "│ ", "FloatBorder" } }
    if row.chunks then
      for _, chunk in ipairs(row.chunks) do
        virt_line[#virt_line + 1] = { chunk.text, chunk.hl }
      end
      virt_line[#virt_line + 1] = { fill, row.hl }
    else
      virt_line[#virt_line + 1] = { row.text .. fill, row.hl }
    end
    virt_line[#virt_line + 1] = { " │", "FloatBorder" }
    virt_lines[#virt_lines + 1] = virt_line
  end
  virt_lines[#virt_lines + 1] = { { pad .. "╰" .. ("─"):rep(inner_width) .. "╯", "FloatBorder" } }

  local anchor_row = comment_box_anchor_row(target, vim.api.nvim_win_get_height(win), #virt_lines)
  pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, anchor_row - 1, 0, {
    virt_lines = virt_lines,
    virt_lines_above = false,
  })
  return { anchor_row = anchor_row, line_count = #virt_lines }
end

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
---@return integer? start_col 0-based byte column
---@return integer? end_col 0-based byte column, exclusive
---@return string? group_type
local function summary_group_type_range(line)
  for _, group_type in ipairs(group_type_order) do
    local keyword = format_type_keyword(group_type)
    local keyword_start_byte = line:find(keyword .. " ", 1, true)
    local keyword_prefix = keyword_start_byte and line:sub(1, keyword_start_byte - 1) or ""
    if keyword_start_byte and keyword_prefix:match("^%s*$") then
      local keyword_start_col = keyword_start_byte - 1
      return keyword_start_col, keyword_start_col + #keyword, group_type
    end
  end
  return nil, nil, nil
end

---@param line string
---@param _group_type string
---@param group_type_highlight_end_col integer
---@return integer? start_col 0-based byte column
---@return integer? end_col 0-based byte column, exclusive
local function summary_group_title_range(line, _group_type, group_type_highlight_end_col)
  local title_start_col = group_type_highlight_end_col + 1
  if title_start_col >= #line then return nil, nil end
  return title_start_col, #line
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

---@param buf integer
---@param lines string[]
---@param doc DiffReviewWalkthroughDoc
local function apply_summary_highlights(buf, lines, doc)
  local item_titles = collect_summary_item_titles(doc.tasks)
  local item_specs = collect_summary_item_specs(doc.tasks)
  local justifications = collect_summary_justifications(doc.tasks)
  for row, line in ipairs(lines) do
    if summary_is_task_line(line) then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, 0, {
        end_col = #line,
        hl_group = "DiffReviewWalkthroughItemTitle",
      })
    end

    if summary_is_justification_line(line, justifications) then
      local start_col = summary_line_prose(line)
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, start_col, {
        end_col = #line,
        hl_group = "DiffReviewWalkthroughJustification",
      })
    end

    local group_type_start_col, group_type_end_col, group_type = summary_group_type_range(line)
    if group_type_start_col and group_type_end_col then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, group_type_start_col, {
        end_col = group_type_end_col,
        hl_group = "DiffReviewWalkthroughType",
      })
      local group_title_start_col, group_title_end_col = summary_group_title_range(line, group_type, group_type_end_col)
      if group_title_start_col and group_title_end_col then
        pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, group_title_start_col, {
          end_col = group_title_end_col,
          hl_group = "DiffReviewWalkthroughItemTitle",
        })
      end
    end

    local start_col, end_col, action = summary_action_range(line)
    local hl_group = action and action_highlights[action] or nil
    if start_col and end_col and hl_group then
      pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, start_col, {
        end_col = end_col,
        hl_group = hl_group,
      })
      local type_start_col, type_end_col, item_type, item_type_start_col, item_type_label =
        summary_item_type_range(line, start_col, action, item_specs)
      if type_start_col and type_end_col then
        pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, type_start_col, {
          end_col = type_end_col,
          hl_group = hl_group,
        })
      end
      if item_type and item_type_start_col and item_type_label then
        local title_start_col, title_end_col =
          summary_item_title_range(line, item_type, item_type_label, item_type_start_col, item_titles)
        if title_start_col and title_end_col then
          pcall(vim.api.nvim_buf_set_extmark, buf, M._ns, row - 1, title_start_col, {
            end_col = title_end_col,
            hl_group = "DiffReviewWalkthroughItemTitle",
          })
        end
      end
    end
  end
end

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

  if index <= 0 then index = 1 end
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

  local function place_cursor()
    if M._modes[buf] ~= mode or mode.index ~= index then return end
    if not (target.start_row and vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf) then return end
    local line = math.min(math.max(target.start_row, 1), vim.api.nvim_buf_line_count(buf))
    pcall(vim.api.nvim_win_set_cursor, win, { line, 0 })
  end
  place_cursor()
  if target.match == "missing" then
    -- Anchor the box at the cursor's current row; no region to highlight.
    local cursor_row = vim.api.nvim_win_get_cursor(win)[1]
    target = { match = "missing", start_row = cursor_row, end_row = cursor_row }
  end
  apply_region_highlight(mode, target)
  local placement = render_comment_box(mode, step, target, win)
  focus_comment_box_below(win, target, placement)
  place_cursor()
  vim.schedule(place_cursor)
end

---@param mode DiffReviewWalkthroughMode
local function begin(mode)
  local buf = mode.host.buf
  M._modes[buf] = mode
  if mode.nav_started then
    show_step(mode, 1)
    return
  end
  mode.nav_started = true

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

--- Exit the walkthrough: clear decorations, clear presentation, restore the
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
    if mode.host.clear_walkthrough_presentation then
      pcall(mode.host.clear_walkthrough_presentation)
    end
    pcall(mode.host.rerender)
  end
end

-- ─── entry points ────────────────────────────────────────────────────────────

--- Start the walkthrough for a status buffer: load .walkthrough.json from the
--- repo root, check staleness against HEAD, embed the presentation, and show
--- the first step.
---@param host DiffReviewWalkthroughHost
function M.start(host)
  if M._modes[host.buf] then
    M.stop(host.buf)
    return
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
    M._modes[host.buf] = mode
    if host.set_walkthrough_presentation then
      host.set_walkthrough_presentation(doc, mode.stale, head_sha)
    end
    host.rerender()
    vim.schedule(function()
      if M._modes[host.buf] ~= mode or not vim.api.nvim_buf_is_valid(host.buf) then return end
      if mode.stale then
        notify(("Walkthrough generated against %s, HEAD is now %s"):format(
          mode.doc.commit:sub(1, 7),
          mode.head_sha and mode.head_sha:sub(1, 7) or "unknown"
        ), vim.log.levels.WARN)
      end
      begin(mode)
    end)
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
