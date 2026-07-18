local M = {}

local display_text = require("diff_review.render.display_text")

local diff_tree = require("diff_review.render.diff_tree")
local plan_event = require("diff_review.render.harness.plan_event")
local timeline_status = require("diff_review.render.harness.timeline_status")
local agent_event = require("diff_review.render.harness.agent_event")
local markdown_text = require("diff_review.render.harness.markdown_text")
local task_tree = require("diff_review.render.harness.task_tree")
local tool_render = require("diff_review.render.harness.tool")

---@param line? string
---@return string|table
function M.foldtext(line)
  local text = line or vim.fn.getline(vim.v.foldstart)
  if text:match("^▸ Thought for ") then return { { text, "DiffReviewHarnessThought" } } end
  local fold_start = tonumber(vim.v.foldstart)
  local state = require("diff_review.session").harness
  local row = fold_start and state.render_rows and state.render_rows[fold_start] or nil
  if row and (row.kind == "tool" or row.kind == "tool_continuation") then
    return row.foldtext or tool_render.foldtext_chunks(row.tool, "  ", text)
  end
  if text:match("^  ▸ Ran %d+ tools?") then return { { text, "Normal" } } end
  return text
end

local function format_token_count(value)
  if type(value) ~= "number" then return nil end
  if value >= 1000 then return ("%.1fk"):format(value / 1000) end
  return tostring(value)
end

local function tool_count_text(verb, count, failed)
  if count == 0 then return nil end
  local text = ("%s %d %s"):format(verb, count, count == 1 and "tool" or "tools")
  if failed > 0 then text = text .. (" (%d failed)"):format(failed) end
  return text
end

local function interaction_counts(interaction)
  local count, failed = 0, 0
  for _, thought in ipairs(interaction.thought or {}) do
    count = count + #(thought.tool or {})
    for _, tool in ipairs(thought.tool or {}) do
      if tool_render.failed(tool) then failed = failed + 1 end
    end
  end
  if interaction.active then
    count = count + (interaction.active.tool_count or 0)
    failed = failed + (interaction.active.failed_count or 0)
  end
  return count, failed
end

local function append_wrapped(result, text, first_prefix, continuation_prefix, group, width)
  local line_list = display_text.wrap(text, width, first_prefix, continuation_prefix)
  for _, line in ipairs(line_list) do
    result.lines[#result.lines + 1] = line
    if group then
      result.highlights[#result.highlights + 1] = {
        line = #result.lines,
        first = 0,
        last = -1,
        group = group,
      }
    end
  end
end

local function append_response(result, text, first_prefix, first_prefix_group)
  local line_list = vim.split(markdown_text.normalize_math(tostring(text or "")), "\n", { plain = true })
  if #line_list == 0 then line_list = { "" } end
  local indentation = string.rep(" ", vim.fn.strdisplaywidth(first_prefix or "  "))
  for line_index, line in ipairs(line_list) do
    local first_line = line_index == 1
    result.lines[#result.lines + 1] = indentation .. line
    if first_line and first_prefix then
      result.extmarks[#result.extmarks + 1] = {
        line = #result.lines,
        col = 0,
        options = {
          virt_text = { { first_prefix, first_prefix_group } },
          virt_text_pos = "overlay",
          hl_mode = "combine",
        },
      }
    end
  end
end

local function append_timeline_response(result, text)
  append_response(result, text, "▸ ", "DiffReviewHarnessResponse")
end

local function append_prebuilt_diff(result, tree)
  local offset = #result.lines
  for _, line in ipairs(tree.lines) do result.lines[#result.lines + 1] = line end
  for line, row in pairs(tree.rows) do result.rows[offset + line] = row end
  for _, highlight in ipairs(tree.highlights) do
    result.highlights[#result.highlights + 1] = {
      line = highlight.line + offset,
      first = highlight.start_col,
      last = highlight.end_col,
      group = highlight.hl_group,
      priority = highlight.priority,
    }
  end
  for _, line_highlight in ipairs(tree.line_highlights or {}) do
    result.extmarks[#result.extmarks + 1] = {
      line = line_highlight.line + offset,
      col = 0,
      options = { line_hl_group = line_highlight.hl_group },
    }
  end
  for _, extmark in ipairs(tree.extmarks or {}) do
    result.extmarks[#result.extmarks + 1] = {
      line = extmark.line + offset,
      col = extmark.col,
      options = vim.deepcopy(extmark.opts),
    }
  end
  for line, spans in pairs(tree.diff_row_spans or {}) do
    result.diff_row_spans[offset + line] = spans
  end
  for _, fold in ipairs(tree.folds) do
    local shifted = vim.deepcopy(fold)
    shifted.first = shifted.first + offset
    shifted.last = shifted.last + offset
    result.folds[#result.folds + 1] = shifted
  end
  return tree
end

local function append_tool(result, tool, thought_key, tool_index, content_width)
  local tool_key = ("%s:tool:%d"):format(thought_key, tool_index)
  local first = #result.lines + 1
  local heading_line_list = tool_render.heading_lines(tool, content_width, "  ")
  for heading_index, heading_line in ipairs(heading_line_list) do
    result.lines[#result.lines + 1] = heading_line.text
    heading_line.line = #result.lines
    result.rows[#result.lines] = {
      kind = heading_index == 1 and "tool" or "tool_continuation",
      tool = tool,
      foldtext = tool_render.foldtext_chunks(tool, "  ", heading_line.text),
      expand_key = tool_key,
    }
  end
  result.highlights[#result.highlights + 1] = {
    line = first,
    first = 2,
    last = 2 + #"•",
    group = tool_render.failed(tool) and "DiffReviewHarnessToolFailure" or "DiffReviewHarnessToolSuccess",
  }
  if tool.kind == "command" then
    tool_render.highlight_command_lines(result, heading_line_list)
  end
  if not result.expanded[tool_key] then return end
  local output_line_list = tool_render.output_lines(tool.output)
  if #output_line_list == 0 then output_line_list = { "no output" } end
  for line_index, line in ipairs(output_line_list) do
    result.lines[#result.lines + 1] = (line_index == 1 and "    └ " or "      ") .. line
    result.rows[#result.lines] = { kind = "tool_output", tool = tool }
    result.highlights[#result.highlights + 1] = {
      line = #result.lines,
      first = 0,
      last = -1,
      group = "DiffReviewHarnessOutput",
    }
  end
end

local function append_active_tool_preview(result, tool, content_width)
  local first = #result.lines + 1
  local heading_line_list = tool_render.heading_lines(tool, content_width, "  ")
  for heading_index, heading_line in ipairs(heading_line_list) do
    result.lines[#result.lines + 1] = heading_line.text
    heading_line.line = #result.lines
    result.rows[#result.lines] = {
      kind = heading_index == 1 and "active_tool" or "active_tool_continuation",
      tool = tool,
    }
  end
  result.highlights[#result.highlights + 1] = {
    line = first,
    first = 2,
    last = 2 + #"•",
    group = tool_render.failed(tool) and "DiffReviewHarnessToolFailure" or "DiffReviewHarnessToolSuccess",
  }
  if tool.kind == "command" then tool_render.highlight_command_lines(result, heading_line_list) end

  local output_line_list = tool_render.output_lines(tool.output)
  for output_index = 1, math.min(#output_line_list, 4) do
    result.lines[#result.lines + 1] = (output_index == 1 and "    └ " or "      ") .. output_line_list[output_index]
    result.rows[#result.lines] = { kind = "active_tool_output", tool = tool }
    result.highlights[#result.highlights + 1] = {
      line = #result.lines,
      first = 0,
      last = -1,
      group = "DiffReviewHarnessOutput",
    }
  end
end

---@param result table
---@param tree table
---@param options { indent: string, label: string, suffix?: string, kind: string, interaction: table, expand_key: string }
---@return integer
local function append_change_summary(result, tree, options)
  local count_text = ("%d %s"):format(tree.file_count, tree.file_count == 1 and "file" or "files")
  local prefix = options.indent .. "▸ " .. options.label .. " " .. count_text
  local added_text = ("+%d"):format(tree.added)
  local removed_text = ("-%d"):format(tree.removed)
  local line = prefix .. " " .. added_text .. " " .. removed_text .. (options.suffix or "")
  local summary_line = #result.lines + 1
  result.lines[summary_line] = line
  result.rows[summary_line] = {
    kind = options.kind,
    interaction = options.interaction,
    expand_key = options.expand_key,
    node_id = options.expand_key,
  }
  local added_first = #prefix + 1
  local removed_first = added_first + #added_text + 1
  result.highlights[#result.highlights + 1] = {
    line = summary_line,
    first = added_first,
    last = added_first + #added_text,
    group = "DiffReviewAddRange",
  }
  result.highlights[#result.highlights + 1] = {
    line = summary_line,
    first = removed_first,
    last = removed_first + #removed_text,
    group = "DiffReviewDeleteRange",
  }
  if result.expanded[options.expand_key] then append_prebuilt_diff(result, tree) end
  for row = summary_line + 1, #result.lines do
    result.rows[row] = result.rows[row] or { kind = options.kind, interaction = options.interaction }
    result.rows[row].node_id = options.expand_key
  end
  return summary_line
end

local function append_completed_thought(result, interaction, thought, thought_index, options)
  local thought_key = ("interaction:%s:thought:%s"):format(interaction.id or interaction.ordinal, thought.id or thought_index)
  local first = #result.lines + 1
  append_wrapped(result, thought.text, "↳ ", "  ", "DiffReviewHarnessCommentary", options.content_width)
  local thought_last = #result.lines
  for line = first, thought_last do
    result.rows[line] = {
      kind = "thought",
      thought = thought,
      interaction = interaction,
      expand_key = thought_key,
      expanded_by_default = true,
    }
  end
  if result.expanded[thought_key] == false then return end
  local failed = 0
  for _, tool in ipairs(thought.tool or {}) do if tool_render.failed(tool) then failed = failed + 1 end end
  local count_text = tool_count_text("Ran", #(thought.tool or {}), failed)
  if count_text then
    result.lines[#result.lines + 1] = "  ▸ " .. count_text
    result.rows[#result.lines] = {
      kind = "thought_tools",
      thought = thought,
      interaction = interaction,
      expand_key = thought_key .. ":tools",
    }
  end
  if result.expanded[thought_key .. ":tools"] then
    for tool_index, tool in ipairs(thought.tool or {}) do
      append_tool(result, tool, thought_key, tool_index, options.content_width)
    end
  end
  local tree = diff_tree.build(thought.diff_text, {
    indent = 4,
    key_prefix = thought_key .. ":changes",
    interaction = interaction,
    cwd = options.cwd,
    on_update = options.on_diff_update,
    expanded = result.expanded,
  })
  if tree.file_count > 0 then
    local changed_line = append_change_summary(result, tree, {
      indent = "  ",
      label = "Changed",
      kind = "thought_changes",
      interaction = interaction,
      expand_key = thought_key .. ":changes",
    })
    result.rows[changed_line].thought = thought
  end
end

local function append_active_thought(result, interaction, active, options)
  local active_first = #result.lines + 1
  append_wrapped(result, active.text, "↳ ", "  ", "DiffReviewHarnessCommentary", options.content_width)
  for line = active_first, #result.lines do
    result.rows[line] = { kind = "active_thought", interaction = interaction }
  end
  local running = tool_count_text("Running", active.tool_count or 0, active.failed_count or 0)
  if running then
    result.lines[#result.lines + 1] = "  ▸ " .. running
    result.rows[#result.lines] = { kind = "active_tool_count", interaction = interaction }
  end
  if type(active.latest_tool) == "table" then
    append_active_tool_preview(result, active.latest_tool, options.content_width)
  end
end

local function append_segment(result, interaction, segment, options, defer_response)
  local segment_first = #result.lines + 1
  local segment_interaction = {
    id = interaction.id,
    segment_id = segment.id,
    ordinal = interaction.ordinal,
    kind = interaction.kind,
    state = segment.state == "complete" and "complete" or "running",
    awaiting_input = interaction.awaiting_input,
    thought = segment.thought or {},
    active = segment.active,
    task = interaction.task,
    duration_ms = segment.duration_ms or 0,
    token_count = segment.token_count,
  }
  local complete = segment.state == "complete"
  local tool_count, failed_count = interaction_counts(segment_interaction)
  local duration_ms = math.max(
    segment.duration_ms or 0,
    segment.presentation_duration_ms or 0,
    complete and 0 or ((options.working_seconds or 0) * 1000)
  )
  local duration = math.floor(duration_ms / 1000)
  local token_text = complete and format_token_count(segment.token_count) or nil
  local summary
  if interaction.state == "cancelled" then
    summary = ("▸ Cancelled after %ds"):format(duration)
  elseif interaction.kind == "plan_draft" or interaction.kind == "plan_revision" then
    if complete and interaction.awaiting_input then
      summary = ("▸ Planning paused after %ds"):format(duration)
    else
      summary = complete and ("▸ Planned for %ds"):format(duration) or ("▸ Planning for %ds"):format(duration)
    end
  elseif interaction.kind == "plan_execution" then
    local current = interaction.task and interaction.task.current or {}
    local completed = 0
    for _, task in ipairs(current) do if task.status == "completed" then completed = completed + 1 end end
    local progress = #current > 0 and (" (%d/%d)"):format(completed, #current) or ""
    summary = complete and ("▸ Executed plan%s for %ds"):format(progress, duration)
      or ("▸ Executing plan%s for %ds"):format(progress, duration)
  else
    summary = complete and ("▸ Thought for %ds"):format(duration) or ("▸ Thinking for %ds"):format(duration)
  end
  if token_text then summary = summary .. ", " .. token_text .. " tokens" end
  if tool_count > 0 then
    summary = summary .. (", %d %s called"):format(tool_count, tool_count == 1 and "tool" or "tools")
    if failed_count > 0 then summary = summary .. (" (%d failed)"):format(failed_count) end
  end
  local spawned_agent_count = segment.spawned_agent_count or 0
  if spawned_agent_count > 0 then
    summary = summary .. (", %d %s spawned"):format(
      spawned_agent_count,
      spawned_agent_count == 1 and "agent" or "agents"
    )
  end
  local summary_line = #result.lines + 1
  result.lines[summary_line] = summary
  result.rows[summary_line] = {
    kind = complete and "thought_summary" or "thinking_summary",
    interaction = interaction,
    node_id = segment.id,
  }
  local summary_key = ("interaction:%s:segment:%s"):format(interaction.id or interaction.ordinal, segment.id)
  if complete then result.rows[summary_line].expand_key = summary_key end
  result.highlights[#result.highlights + 1] = {
    line = summary_line,
    first = 0,
    last = -1,
    group = complete and "DiffReviewHarnessThought" or "DiffReviewHarnessThinking",
  }
  local task_owns_active = task_tree.append(result, segment_interaction, options, complete, {
    append_completed_thought = append_completed_thought,
    append_active_thought = append_active_thought,
  })
  if (not complete or result.expanded[summary_key]) and not interaction.task then
    for thought_index, thought in ipairs(segment.thought or {}) do
      append_completed_thought(result, segment_interaction, thought, thought_index, options)
    end
  elseif result.expanded[summary_key] and interaction.task then
    for thought_index, thought in ipairs(segment.thought or {}) do
      if not thought.task_id then
        append_completed_thought(result, segment_interaction, thought, thought_index, options)
      end
    end
    task_tree.append_superseded(result, segment_interaction)
  end
  if segment.active and not task_owns_active then
    append_active_thought(result, segment_interaction, segment.active, options)
  end
  if complete and not defer_response and type(segment.response) == "string" and segment.response ~= "" then
    local response_line = #result.lines + 1
    result.rows[response_line] = { kind = "response", interaction = interaction, node_id = segment.id }
    append_timeline_response(result, segment.response)
    result.markdown_ranges[#result.markdown_ranges + 1] = { first0 = response_line - 1, after0 = #result.lines }
  end
  for line = segment_first, #result.lines do
    result.rows[line] = result.rows[line] or { kind = "segment_content", interaction = interaction }
    result.rows[line].node_id = segment.id
  end
end

local function append_interaction(result, interaction, options, agent_by_id)
  if #result.lines > 0 then result.lines[#result.lines + 1] = "" end
  if not interaction.hide_prompt and interaction.kind ~= "plan_execution" then
    local prompt_line = #result.lines + 1
    append_wrapped(result, interaction.prompt, "▸ ", "  ", "DiffReviewHarnessPrompt", options.content_width)
    result.prompt_lines[#result.prompt_lines + 1] = prompt_line
    for line = prompt_line, #result.lines do
      result.rows[line] = {
        kind = "prompt",
        interaction = interaction,
        node_id = (interaction.id or tostring(interaction.ordinal)) .. ":prompt",
      }
    end
  end
  local complete = interaction.state == "complete" or interaction.state == "failed"
    or interaction.state == "cancelled" or interaction.state == "rolled_back"
    or interaction.state == "superseded"
  local deferred_response = nil
  local final_response_segment_id = nil
  if complete then
    for node_index = #(interaction.node_list or {}), 1, -1 do
      local node = interaction.node_list[node_index]
      if node.kind == "main_segment" and node.segment
        and type(node.segment.response) == "string" and node.segment.response ~= ""
      then
        final_response_segment_id = node.segment.id
        break
      end
    end
  end
  for _, node in ipairs(interaction.node_list or {}) do
    if node.kind == "main_segment" and node.segment then
      local defer_response = complete and node.segment.id == final_response_segment_id
      append_segment(result, interaction, node.segment, options, defer_response)
      if defer_response then deferred_response = node.segment end
    elseif node.kind == "steering_prompt" and node.prompt then
      local first = #result.lines + 1
      append_wrapped(result, node.prompt.text, "▸ ", "  ", "DiffReviewHarnessPrompt", options.content_width)
      result.prompt_lines[#result.prompt_lines + 1] = first
      for line = first, #result.lines do
        result.rows[line] = {
          kind = "steering_prompt",
          interaction = interaction,
          steering_input = node.prompt,
          node_id = node.prompt.id,
        }
      end
    elseif node.kind == "agent_reference" and node.agent then
      local agent = agent_by_id and agent_by_id[node.agent.agent_run_id]
      if agent then
        local first = #result.lines + 1
        agent_event.append(result, agent, options, 0, node.agent.id)
        for line = first, #result.lines do
          result.rows[line] = result.rows[line] or { kind = "agent_content", interaction = interaction }
          result.rows[line].node_id = node.agent.id
        end
      end
    end
  end
  if complete then
    local attributed_key = ("interaction:%s:attributed"):format(interaction.id or interaction.ordinal)
    local attributed = diff_tree.build(interaction.attributed_diff_text, {
      indent = 2,
      key_prefix = attributed_key,
      interaction = interaction,
      cwd = options.cwd,
      on_update = options.on_diff_update,
      expanded = result.expanded,
    })
    if attributed.file_count > 0 then
      append_change_summary(result, attributed, {
        indent = "",
        label = "Changed",
        suffix = interaction.attributed_matches_checkpoint and " · checkpoint matched" or nil,
        kind = "attributed_changes",
        interaction = interaction,
        expand_key = attributed_key,
      })
    end
    if not interaction.attributed_matches_checkpoint then
      local checkpoint_key = ("interaction:%s:checkpoint"):format(interaction.id or interaction.ordinal)
      local checkpoint = diff_tree.build(interaction.checkpoint_diff_text, {
        indent = 2,
        key_prefix = checkpoint_key,
        interaction = interaction,
        cwd = options.cwd,
        on_update = options.on_diff_update,
        expanded = result.expanded,
      })
      if checkpoint.file_count > 0 then
        append_change_summary(result, checkpoint, {
          indent = "",
          label = "Checkpoint total:",
          kind = "checkpoint_changes",
          interaction = interaction,
          expand_key = checkpoint_key,
        })
      end
    end
    if deferred_response then
      local response_line = #result.lines + 1
      result.rows[response_line] = {
        kind = "response",
        interaction = interaction,
        node_id = deferred_response.id .. ":response",
      }
      append_timeline_response(result, deferred_response.response)
      for line = response_line + 1, #result.lines do
        result.rows[line] = {
          kind = "response_body",
          interaction = interaction,
          node_id = deferred_response.id .. ":response",
        }
      end
      result.markdown_ranges[#result.markdown_ranges + 1] = {
        first0 = response_line - 1,
        after0 = #result.lines,
      }
    end
  end
end

local function aggregate_execution(entry)
  local aggregate = {
    id = entry.id,
    prompt = "",
    hide_prompt = true,
    kind = "plan_execution",
    state = entry.execution and entry.execution.state == "active" and "running" or "complete",
    node_list = {},
  }
  for _, interaction in ipairs(entry.interaction or {}) do
    for _, node in ipairs(interaction.node_list or {}) do
      aggregate.node_list[#aggregate.node_list + 1] = node
    end
    aggregate.task = interaction.task or aggregate.task
    aggregate.attributed_diff_text = interaction.attributed_diff_text or aggregate.attributed_diff_text
    aggregate.checkpoint_diff_text = interaction.checkpoint_diff_text or aggregate.checkpoint_diff_text
    aggregate.attributed_matches_checkpoint = interaction.attributed_matches_checkpoint == true
  end
  return aggregate
end

---@param interactions table[]
---@param options? { working_seconds?: integer, cwd?: string, on_diff_update?: function, expanded?: table<string, boolean> }
---@return table
function M.build(interactions, options)
  options = options or {}
  local expanded = options.expanded
  if expanded == nil then expanded = setmetatable({}, { __index = function() return true end }) end
  local result = {
    lines = {},
    highlights = {},
    extmarks = {},
    folds = {},
    rows = {},
    prompt_lines = {},
    markdown_ranges = {},
    diff_row_spans = {},
    expanded = expanded,
  }
  for _, entry in ipairs(interactions or {}) do
    if entry.kind == "interaction" and entry.interaction then
      append_interaction(result, entry.interaction, options, entry.agent_by_id)
    elseif entry.kind == "plan_lifecycle" then
      plan_event.append(result, entry, { append_response = append_response })
    elseif entry.kind == "plan_execution" then
      append_interaction(result, aggregate_execution(entry), options)
    elseif entry.kind == "agent_lifecycle" then
      agent_event.append(result, entry, options, 0)
    else
      append_interaction(result, entry, options)
    end
  end
  timeline_status.append(result, options.timeline_status)
  if #result.lines == 0 then result.lines = { "" } end
  local next_node_id = nil
  local node_id_by_line = {}
  for line = #result.lines, 1, -1 do
    local row = result.rows[line]
    if row and row.node_id then next_node_id = row.node_id end
    node_id_by_line[line] = next_node_id or ("tail:%d"):format(line)
  end
  result.blocks = {}
  local block = nil
  for line = 1, #result.lines do
    local node_id = node_id_by_line[line]
    if not block or block.id ~= node_id then
      block = { id = node_id, first = line, last = line, lines = {} }
      result.blocks[#result.blocks + 1] = block
    end
    block.last = line
    block.lines[#block.lines + 1] = result.lines[line]
  end
  return result
end

return M
