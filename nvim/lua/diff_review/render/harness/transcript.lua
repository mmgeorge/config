local M = {}

local control_tool_name = {
  harness_plan_submit = true,
  harness_goal_complete = true,
  harness_goal_blocked = true,
  harness_goal_status = true,
}

---@param raw_event table
---@return boolean
local function is_control_tool_event(raw_event)
  local event = raw_event.payload or raw_event
  local activity = type(event.activity) == "table" and event.activity or nil
  return event.kind == "tool" and activity ~= nil and control_tool_name[activity.title] == true
end

---@class DiffReviewHarnessTranscriptRender
---@field lines string[]
---@field prompt_line integer[]
---@field activity_range { id: string, first: integer, last: integer, default_view?: "collapsed"|"full", fold_first?: integer, fold_last?: integer }[]
---@field markdown_range { first0: integer, after0: integer }[]
---@field highlights { line: integer, first: integer, last: integer, group: string }[]

---@param event table
---@return string
local function event_text(event)
  if type(event.text) == "string" then return event.text end
  if type(event.data) == "table" and type(event.data.text) == "string" then return event.data.text end
  if type(event.payload) == "table" and type(event.payload.text) == "string" then return event.payload.text end
  return ""
end

---@param output string
---@return string[]
local function output_line_list(output)
  local normalized = output:gsub("\r\n", "\n"):gsub("\r", "")
  normalized = vim.fn.substitute(normalized, "\\%x1b\\[[0-?]*[ -/]*[@-~]", "", "g")
  local line_list = vim.split(normalized, "\n", { plain = true })
  while #line_list > 1 and line_list[#line_list] == "" do table.remove(line_list) end
  return line_list
end

---@param activity table
---@param output string
---@return boolean
local function activity_rejected(activity, output)
  if activity.status == "rejected" then return true end
  return output:lower():find(" rejected:", 1, true) ~= nil
end

---@param activity table
---@param rejected boolean
---@return boolean
local function activity_failed(activity, rejected)
  if rejected or activity.error ~= nil then return true end
  local status = tostring(activity.status or ""):lower()
  return status == "failed"
    or status == "error"
    or status == "denied"
    or status == "cancelled"
    or status == "canceled"
end

---@param token_count integer
---@return string
local function format_token_count(token_count)
  if token_count < 1000 then return tostring(token_count) end
  if token_count < 1000000 then
    local value = token_count / 1000
    return value % 1 == 0 and ("%dk"):format(value) or ("%.1fk"):format(value)
  end
  local value = token_count / 1000000
  return value % 1 == 0 and ("%dm"):format(value) or ("%.1fm"):format(value)
end

---@param summary table
---@return string
local function thought_summary(summary)
  local duration_ms = tonumber(summary.duration_ms) or 0
  local duration = duration_ms < 1000 and "<1s" or (("%ds"):format(math.floor((duration_ms + 500) / 1000)))
  local token_count = tonumber(summary.token_count)
  local usage = token_count and (", " .. format_token_count(token_count) .. " tokens") or ""
  return "▸ Thought for " .. duration .. usage
end

---@param activity table
---@return string
local function activity_verb(activity)
  local running = activity.status == "inProgress" or activity.status == "in_progress"
  if activity.kind == "command" then return running and "Running" or "Ran" end
  if activity.kind == "file_change" then return running and "Editing" or "Edited" end
  return running and "Calling" or "Called"
end

---@param title string
---@return string
local function display_command(title)
  local command = title:match("[Pp][Oo][Ww][Ee][Rr][Ss][Hh][Ee][Ll][Ll][^%s]*.-%s%-Command%s+(.+)$")
    or title:match("[Pp][Ww][Ss][Hh][^%s]*.-%s%-Command%s+(.+)$")
  if not command then return title end
  local quote = command:sub(1, 1)
  if (quote == '"' or quote == "'") and command:sub(-1) == quote then command = command:sub(2, -2) end
  return command:gsub('\\"', '"')
end

---@param result DiffReviewHarnessTranscriptRender
---@param line integer
---@param command string
---@param offset integer
local function highlight_command(result, line, command, offset)
  local expects_command = true
  local search_start = 1
  while true do
    local token_start, token_end, token = command:find("(%S+)", search_start)
    if not token_start then break end
    local group = "DiffReviewHarnessArgument"
    if expects_command then
      group = "DiffReviewHarnessCommand"
      expects_command = false
    elseif token:match("^%-%-?") then
      group = "DiffReviewHarnessOption"
    end
    result.highlights[#result.highlights + 1] = {
      line = line,
      first = offset + token_start - 1,
      last = offset + token_end,
      group = group,
    }
    if token == "|" or token == ";" or token == "&&" then expects_command = true end
    search_start = token_end + 1
  end
end

---@param result DiffReviewHarnessTranscriptRender
---@param first_line integer
---@param last_line integer
local function highlight_output(result, first_line, last_line)
  for line = first_line, last_line do
    result.highlights[#result.highlights + 1] = {
      line = line,
      first = 0,
      last = -1,
      group = "DiffReviewHarnessOutput",
    }
  end
end

---@return string
function M.foldtext()
  return ""
end

---@param transcript table[]
---@param options? { ready?: boolean, working_seconds?: integer, activity_view?: table<string, "collapsed"|"full"> }
---@return DiffReviewHarnessTranscriptRender
function M.build(transcript, options)
  options = options or {}
  local visible_transcript = vim.tbl_filter(function(event)
    local payload = event.payload or event
    return not is_control_tool_event(event)
      and not (payload.kind == "plan" and type(payload.plan_progress) ~= "table")
  end, transcript or {})
  local result = { lines = {}, prompt_line = {}, activity_range = {}, markdown_range = {}, highlights = {} }
  local commentary_index = {}
  local later_interaction_activity = false
  for event_index = #visible_transcript, 1, -1 do
    local raw_event = visible_transcript[event_index]
    local event = raw_event.payload or raw_event
    if event.kind == "user_message" then
      later_interaction_activity = false
    elseif event.kind == "assistant_message" then
      commentary_index[event_index] = later_interaction_activity
      later_interaction_activity = true
    elseif event.kind == "tool" or event.kind == "reasoning" or event.kind == "plan" then
      later_interaction_activity = true
    end
  end
  local function append(text)
    local lines = vim.split(tostring(text or ""), "\n", { plain = true })
    if #lines == 0 then lines = { "" } end
    for _, line in ipairs(lines) do result.lines[#result.lines + 1] = line end
  end
  local previous_kind = nil
  for event_index, raw_event in ipairs(visible_transcript) do
    local event = raw_event.payload or raw_event
    local kind = event.kind or "event"
    if #result.lines > 0 and not (previous_kind == "tool" and kind == "tool") then
      result.lines[#result.lines + 1] = ""
    end
    local heading_line = #result.lines + 1
    if kind == "user_message" then
      result.prompt_line[#result.prompt_line + 1] = heading_line
      local prompt_line = vim.split(event_text(event), "\n", { plain = true })
      result.lines[#result.lines + 1] = "▸ " .. (prompt_line[1] or "")
      for line_index = 2, #prompt_line do result.lines[#result.lines + 1] = "  " .. prompt_line[line_index] end
      result.highlights[#result.highlights + 1] = {
        line = heading_line,
        first = 0,
        last = -1,
        group = "DiffReviewHarnessPrompt",
      }
    elseif kind == "user_action" then
      result.lines[#result.lines + 1] = "You • action"
      result.highlights[#result.highlights + 1] = { line = heading_line, first = 0, last = -1, group = "DiffReviewStatusHint" }
      append(event_text(event))
    elseif kind == "assistant_message" then
      local commentary = commentary_index[event_index] == true
      if commentary then
        local commentary_line = vim.split(event_text(event), "\n", { plain = true })
        for _, line in ipairs(commentary_line) do
          result.lines[#result.lines + 1] = "  " .. line
          result.highlights[#result.highlights + 1] = {
            line = #result.lines,
            first = 0,
            last = -1,
            group = "DiffReviewHarnessCommentary",
          }
        end
      elseif type(event.summary) == "table" then
        result.lines[#result.lines + 1] = thought_summary(event.summary)
        result.highlights[#result.highlights + 1] = {
          line = heading_line,
          first = 0,
          last = -1,
          group = "DiffReviewHarnessThought",
        }
      end
      if not commentary then
        local response_line = vim.split(event_text(event), "\n", { plain = true })
        local response_first = #result.lines + 1
        for _, line in ipairs(response_line) do result.lines[#result.lines + 1] = "  " .. line end
        if #result.lines >= response_first then
          result.markdown_range[#result.markdown_range + 1] = {
            first0 = response_first - 1,
            after0 = #result.lines,
          }
        end
      end
    elseif kind == "plan" and type(event.plan_progress) == "table" then
      local progress = event.plan_progress
      local progress_id = "plan:" .. tostring(progress.id or event_index)
      local completed = progress.status == "completed" or progress.status == "complete"
      local name = type(progress.name) == "string" and vim.trim(progress.name) or ""
      local label = name ~= "" and ("Plan " .. name) or "Plan"
      result.lines[#result.lines + 1] = "• " .. label .. (completed and " Complete" or "")
      result.highlights[#result.highlights + 1] = {
        line = heading_line,
        first = 0,
        last = -1,
        group = completed and "DiffReviewHarnessThought" or "DiffReviewStatusHint",
      }
      local default_view = completed and "collapsed" or "full"
      local view = options.activity_view and options.activity_view[progress_id] or default_view
      if view == "full" then
        for _, step in ipairs(progress.step_list or {}) do
          local status = completed and "completed" or (step.status or "pending")
          local marker = (status == "completed" or status == "complete") and "[x]"
            or ((status == "inProgress" or status == "in_progress") and "[-]" or "[ ]")
          result.lines[#result.lines + 1] = ("  %s %s"):format(marker, step.text or "")
          result.highlights[#result.highlights + 1] = {
            line = #result.lines,
            first = 0,
            last = -1,
            group = marker == "[x]" and "Comment"
              or (marker == "[-]" and "DiffReviewHarnessThought" or "DiffReviewHarnessOutput"),
          }
        end
      end
      result.activity_range[#result.activity_range + 1] = {
        id = progress_id,
        first = heading_line,
        last = #result.lines,
        default_view = default_view,
      }
    elseif kind == "tool" and type(event.activity) == "table" then
      local activity = event.activity
      local activity_id = tostring(activity.id or ("tool-" .. event_index))
      local output = type(activity.output) == "string" and activity.output or ""
      local rejected = activity_rejected(activity, output)
      local failed = activity_failed(activity, rejected)
      local verb = activity_verb(activity)
      local title = activity.kind == "command" and display_command(activity.title or "command") or (activity.title or "tool")
      local prefix = ("• %s "):format(verb)
      result.lines[#result.lines + 1] = prefix .. title
      result.highlights[#result.highlights + 1] = {
        line = heading_line,
        first = 0,
        last = #"•",
        group = failed and "DiffReviewHarnessToolFailure" or "DiffReviewHarnessToolSuccess",
      }
      result.highlights[#result.highlights + 1] = {
        line = heading_line,
        first = #"•",
        last = #prefix,
        group = "Normal",
      }
      if activity.kind == "command" then
        highlight_command(result, heading_line, title, #prefix)
      else
        result.highlights[#result.highlights + 1] = {
          line = heading_line,
          first = #prefix,
          last = -1,
          group = "DiffReviewHarnessArgument",
        }
      end
      local output_line = output ~= "" and output_line_list(output) or {}
      local view = options.activity_view and options.activity_view[activity_id] or "collapsed"
      local output_first = #result.lines + 1
      if not rejected and view ~= "collapsed" and #output_line == 0 then
        local status = activity.status == "inProgress" and "working…" or "no output"
        result.lines[#result.lines + 1] = "  └ " .. status
      elseif not rejected and view ~= "collapsed" then
        for line_index = 1, #output_line do
          local prefix = line_index == 1 and "  └ " or "    "
          result.lines[#result.lines + 1] = prefix .. output_line[line_index]
        end
      end
      if not rejected then
        result.activity_range[#result.activity_range + 1] = {
          id = activity_id,
          first = heading_line,
          last = #result.lines,
        }
      end
      if #result.lines >= output_first then highlight_output(result, output_first, #result.lines) end
    elseif kind == "tool" or kind == "reasoning" then
      result.lines[#result.lines + 1] = kind == "tool" and "• Tool activity" or "Reasoning"
      result.highlights[#result.highlights + 1] = { line = heading_line, first = 0, last = -1, group = "DiffReviewStatusHint" }
      append(event_text(event) ~= "" and event_text(event) or vim.inspect(event.data or {}))
      result.activity_range[#result.activity_range + 1] = {
        id = kind .. "-" .. event_index,
        first = heading_line,
        last = #result.lines,
        fold_first = heading_line,
        fold_last = #result.lines,
      }
    elseif kind == "system_message" then
      result.lines[#result.lines + 1] = "Harness"
      result.highlights[#result.highlights + 1] = { line = heading_line, first = 0, last = -1, group = "WarningMsg" }
      append(event_text(event))
    elseif kind == "error" then
      result.lines[#result.lines + 1] = "Error"
      result.highlights[#result.highlights + 1] = { line = heading_line, first = 0, last = -1, group = "ErrorMsg" }
      append(event_text(event))
    else
      result.lines[#result.lines + 1] = "Assistant"
      result.highlights[#result.highlights + 1] = { line = heading_line, first = 0, last = -1, group = "DiffReviewStatusLabel" }
      append(event_text(event) ~= "" and event_text(event) or vim.inspect(event.data or event))
    end
    previous_kind = kind
  end
  if options.working_seconds ~= nil then
    if #result.lines > 0 then result.lines[#result.lines + 1] = "" end
    local working_line = #result.lines + 1
    result.lines[#result.lines + 1] = ("  Working (%ds)"):format(options.working_seconds)
    result.highlights[#result.highlights + 1] = {
      line = working_line,
      first = 0,
      last = -1,
      group = "DiffReviewStatusHint",
    }
  end
  return result
end

return M
