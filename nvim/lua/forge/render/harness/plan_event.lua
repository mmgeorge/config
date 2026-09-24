local M = {}

---@class ForgeHarnessPlanEventHost
---@field append_response fun(result: table, text: string)

--- Appends a durable plan lifecycle event row and expandable questions/annotations.
---@param result table Target render collection table.
---@param entry table Plan lifecycle event entry record.
---@param host ForgeHarnessPlanEventHost Host adapter providing response formatting callbacks.
function M.append(result, entry, host)
  local lifecycle = entry.lifecycle or {}
  if lifecycle.kind == "question_asked" and lifecycle.question then
    M.question(result, entry.id or lifecycle.id, lifecycle.question, lifecycle.answer, host)
    return
  end
  local label = ({
    created = "Plan created",
    question_asked = "Planning paused for feedback",
    question_answered = "You answered",
    question_withdrawn = "Question withdrawn",
    changes_requested = "Plan changes requested",
    revision_created = "Plan revision created",
    accepted = "Plan accepted",
    cancelled = "Plan cancelled",
  })[lifecycle.kind] or "Plan"
  if lifecycle.kind == "changes_requested" and lifecycle.overall_comment and lifecycle.overall_comment ~= "" then
    label = label .. ": " .. lifecycle.overall_comment
  elseif lifecycle.kind == "question_answered" and lifecycle.answer and lifecycle.answer ~= "" then
    label = label .. ": " .. lifecycle.answer:gsub("^Planning feedback:%s*", ""):gsub("^%- ", ""):gsub("\n%- ", ", ")
  elseif lifecycle.kind == "question_withdrawn" and lifecycle.answer and lifecycle.answer ~= "" then
    label = label .. ": " .. lifecycle.answer:gsub("[\r\n]+", " "):sub(1, 100)
  end
  local key = "plan_lifecycle:" .. tostring(entry.id or lifecycle.id)
  local line = #result.lines + 1
  result.lines[line] = (lifecycle.kind == "question_answered" and "○ " or "▸ ") .. label
  result.rows[line] = { kind = "plan_lifecycle", lifecycle = lifecycle, expand_key = key }
  if not result.expanded[key] then return end
  if #(lifecycle.annotation or {}) > 0 then
    result.lines[#result.lines + 1] = "  ▸ Annotations"
    result.rows[#result.lines] = { kind = "plan_annotations", lifecycle = lifecycle }
    for _, annotation in ipairs(lifecycle.annotation) do
      result.lines[#result.lines + 1] = ("    %s: %s"):format(annotation.label or "Plan subject", annotation.body or "")
      result.rows[#result.lines] = { kind = "plan_annotation", lifecycle = lifecycle }
    end
  end
end

---@param result table
---@param id string
---@param question_set table
---@param answer string?
---@param host ForgeHarnessPlanEventHost
function M.question(result, id, question_set, answer, host)
  local headers = {}
  for _, question in ipairs(question_set.questions or {}) do headers[#headers + 1] = question.header end
  local key = "question:" .. id
  result.lines[#result.lines + 1] = "▸ Question presented: " .. table.concat(headers, ", ")
  result.rows[#result.lines] = { kind = "question_presented", node_id = id, expand_key = key }
  if not result.expanded[key] then return end
  for _, question in ipairs(question_set.questions or {}) do
    result.lines[#result.lines + 1] = "  " .. question.question
    result.rows[#result.lines] = { kind = "question_detail", node_id = id }
    for _, option in ipairs(question.options or {}) do
      result.lines[#result.lines + 1] = "  " .. option.label .. ": " .. option.description
      result.rows[#result.lines] = { kind = "question_detail", node_id = id }
    end
  end
  if answer then host.append_response(result, answer) end
end

return M
