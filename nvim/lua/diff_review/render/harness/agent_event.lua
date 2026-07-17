local M = {}

local agent_summary = require("diff_review.render.harness.agent_summary")
local display_text = require("diff_review.render.display_text")

local function append_wrapped(result, text, first_prefix, continuation_prefix, width)
  for _, line in ipairs(display_text.wrap(text, width, first_prefix, continuation_prefix)) do
    result.lines[#result.lines + 1] = line
    result.highlights[#result.highlights + 1] = {
      line = #result.lines,
      first = 0,
      last = -1,
      group = "DiffReviewHarnessCommentary",
    }
  end
end

local function append_interaction_detail(result, interaction, width, indent)
  local thought_prefix = string.rep(" ", indent) .. "↳ "
  local continuation_prefix = string.rep(" ", indent + 2)
  local item_prefix = string.rep(" ", indent + 2)
  for _, node in ipairs(interaction.node_list or {}) do
    local segment = node.segment
    if segment then
      for _, thought in ipairs(segment.thought or {}) do
        append_wrapped(result, thought.text, thought_prefix, continuation_prefix, width)
        local count = #(thought.tool or {})
        if count > 0 then
          result.lines[#result.lines + 1] = item_prefix
            .. ("▸ Ran %d %s"):format(count, count == 1 and "tool" or "tools")
        end
      end
      if segment.active then
        append_wrapped(result, segment.active.text, thought_prefix, continuation_prefix, width)
        local count = segment.active.tool_count or 0
        if count > 0 then
          result.lines[#result.lines + 1] = item_prefix
            .. ("▸ Running %d %s"):format(count, count == 1 and "tool" or "tools")
        end
      end
      if type(segment.response) == "string" and segment.response ~= "" then
        for line_index, line in ipairs(vim.split(segment.response, "\n", { plain = true })) do
          local prefix = line_index == 1 and (string.rep(" ", indent) .. "▸ ") or continuation_prefix
          result.lines[#result.lines + 1] = prefix .. line
        end
      end
    end
  end
end

---Render one durable child-agent lifecycle node in the parent timeline.
---@param result table
---@param entry table
---@param options? table
---@param indent? integer
---@param node_id? string
function M.append(result, entry, options, indent, node_id)
  options = options or {}
  indent = indent or 0
  local run = entry.run or {}
  local label = agent_summary.label(run)
  local status = run.status or "unknown"
  local key = "agent_lifecycle:" .. tostring(node_id or entry.id or run.id)
  local ended = status == "completed" or status == "failed" or status == "interrupted" or status == "closed"
  local status_text = agent_summary.status_detail(
    run,
    entry.interaction,
    options.now_ms or run.updated_at_ms or 0,
    false,
    false
  )
  local prefix = string.rep(" ", indent)
  local line = #result.lines + 1
  result.lines[line] = ("%s▸ Agent %s %s"):format(prefix, label, status_text)
  result.rows[line] = {
    kind = "agent_lifecycle",
    run = run,
    expand_key = key,
    agent_run_id = run.id,
    node_id = node_id or entry.id or run.id,
  }
  result.highlights[#result.highlights + 1] = {
    line = line,
    first = 0,
    last = -1,
    group = ended and "DiffReviewHarnessThought" or "DiffReviewHarnessThinking",
  }
  if not result.expanded[key] then return end
  for _, interaction in ipairs(entry.interaction or {}) do
    append_interaction_detail(result, interaction, options.content_width, indent + 2)
  end
  for _, child in ipairs(entry.agent or {}) do
    M.append(result, child, options, indent + 2)
  end
end

return M
