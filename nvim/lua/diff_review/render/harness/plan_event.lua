local M = {}

---@class DiffReviewHarnessPlanEventHost
---@field append_response fun(result: table, text: string)

--- Render one durable plan lifecycle event with semantic content expansion.
---@param result table
---@param entry table
---@param host DiffReviewHarnessPlanEventHost
function M.append(result, entry, host)
  if #result.lines > 0 then result.lines[#result.lines + 1] = "" end
  local lifecycle = entry.lifecycle or {}
  local label = ({
    created = "Plan created",
    question_asked = "Planning paused for feedback",
    question_answered = "Plan feedback received",
    question_withdrawn = "Question withdrawn",
    changes_requested = "Plan changes requested",
    revision_created = "Plan revision created",
    accepted = "Plan accepted",
    cancelled = "Plan cancelled",
  })[lifecycle.kind] or "Plan"
  if lifecycle.kind == "changes_requested" and lifecycle.overall_comment and lifecycle.overall_comment ~= "" then
    label = label .. ": " .. lifecycle.overall_comment
  elseif lifecycle.kind == "question_answered" and lifecycle.answer and lifecycle.answer ~= "" then
    label = label .. ": " .. lifecycle.answer:gsub("[\r\n]+", " "):sub(1, 100)
  elseif lifecycle.kind == "question_withdrawn" and lifecycle.answer and lifecycle.answer ~= "" then
    label = label .. ": " .. lifecycle.answer:gsub("[\r\n]+", " "):sub(1, 100)
  end
  local key = "plan_lifecycle:" .. tostring(entry.id or lifecycle.id)
  local line = #result.lines + 1
  result.lines[line] = "▸ " .. label
  result.rows[line] = { kind = "plan_lifecycle", lifecycle = lifecycle, expand_key = key }
  if lifecycle.kind == "question_asked" and lifecycle.question then
    for _, question in ipairs(lifecycle.question.questions or {}) do
      result.lines[#result.lines + 1] = "  ▸ " .. (question.header or "Question")
      result.rows[#result.lines] = { kind = "plan_question", lifecycle = lifecycle, question = question }
      host.append_response(result, question.question or "")
      for _, option in ipairs(question.options or {}) do
        local description = option.description and option.description ~= "" and (" — " .. option.description) or ""
        result.lines[#result.lines + 1] = "    ○ " .. (option.label or "[unnamed option]") .. description
        result.rows[#result.lines] = { kind = "plan_question_option", lifecycle = lifecycle, question = question }
      end
    end
    return
  end
  if not result.expanded[key] then return end
  if type(entry.content) == "string" and entry.content ~= "" then
    result.lines[#result.lines + 1] = "  ▸ Content"
    result.rows[#result.lines] = { kind = "plan_content", lifecycle = lifecycle }
    local first_markdown = #result.lines + 1
    host.append_response(result, entry.content)
    result.markdown_ranges[#result.markdown_ranges + 1] = { first0 = first_markdown - 1, after0 = #result.lines }
  end
  if #(lifecycle.annotation or {}) > 0 then
    result.lines[#result.lines + 1] = "  ▸ Annotations"
    result.rows[#result.lines] = { kind = "plan_annotations", lifecycle = lifecycle }
    for _, annotation in ipairs(lifecycle.annotation) do
      result.lines[#result.lines + 1] = ("    %d: %s"):format(annotation.line or 0, annotation.body or "")
      result.rows[#result.lines] = { kind = "plan_annotation", lifecycle = lifecycle }
    end
  end
end

return M
