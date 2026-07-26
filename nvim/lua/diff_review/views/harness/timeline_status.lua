local M = {}

local keymaps = require("diff_review.shared.keymaps")
local render = require("diff_review.render.harness.timeline_status")

local namespace = vim.api.nvim_create_namespace("diff_review_harness_timeline_status")

local function status_trace(state, status)
  local elicitation = state.active_elicitation
  local wait = state.active_wait
  local plan = state.active_plan
  local record = {
    timestamp_ms = vim.uv.now(),
    busy = state.busy == true,
    elicitation_owner = elicitation and elicitation.owner or nil,
    elicitation_plan_id = elicitation and elicitation.plan_id or nil,
    active_plan_id = plan and plan.id or nil,
    active_plan_state = plan and plan.state or nil,
    plan_generation_turn = plan and plan.generation and plan.generation.turn_count or nil,
    plan_generation_max_turn = plan and plan.generation and plan.generation.max_turn_count or nil,
    plan_generation_no_progress = plan and plan.generation and plan.generation.consecutive_no_progress or nil,
    wait_agent_count = wait and wait.agent_count or nil,
    status_id = status and status.id or nil,
    status_kind = status and status.kind or nil,
    status_text = status and status.text or nil,
  }
  local fingerprint = vim.json.encode({
    busy = record.busy,
    elicitation_owner = record.elicitation_owner,
    elicitation_plan_id = record.elicitation_plan_id,
    active_plan_id = record.active_plan_id,
    active_plan_state = record.active_plan_state,
    plan_generation_turn = record.plan_generation_turn,
    plan_generation_max_turn = record.plan_generation_max_turn,
    plan_generation_no_progress = record.plan_generation_no_progress,
    wait_agent_count = record.wait_agent_count,
    status_id = record.status_id,
    status_kind = record.status_kind,
    status_text = record.status_text,
  })
  if state.timeline_status_trace_fingerprint == fingerprint then return end
  state.timeline_status_trace_fingerprint = fingerprint
  local ok, line = pcall(vim.json.encode, record)
  if not ok then return end
  pcall(vim.fn.writefile, { line }, vim.fs.joinpath(vim.fn.stdpath("cache"), "diff-review-harness-status-debug.jsonl"), "a")
end

---@param state DiffReviewHarnessPresentationState
---@return table?
function M.resolve(state)
  local status = state.status or { kind = "idle" }
  if status.kind == "awaiting_input" then
    local reopen_key = keymaps.view_keys_for("harness", "reopen_question")[1]
    local suffix = reopen_key and (" (press " .. reopen_key .. ")") or ""
    return { id = "input", kind = "input", text = "Waiting for input" .. suffix }
  end
  if status.kind == "awaiting_plan_review" then
    local open_key = keymaps.view_keys_for("harness", "open_artifact")[1]
    local suffix = open_key and (" (press " .. open_key .. ")") or ""
    return { id = "plan_review", kind = "plan_review", text = "Waiting for plan review" .. suffix }
  end
  if status.kind == "retrying_plan_generation" then
    return {
      id = "plan_retry",
      kind = "working",
      text = ("Retrying plan generation %d/%d"):format(status.turn or 1, status.max_turn or 20),
      started_at_ms = status.started_at_ms,
    }
  end
  if status.kind == "planning_failed" then
    return {
      id = "plan_failed",
      kind = "error",
      text = "Plan generation stopped. Run /plan retry or /plan cancel",
    }
  end
  if status.kind == "waiting_for_agent" then
    local count = status.agent_count or 0
    local noun = count == 1 and "subagent" or "subagents"
    return { id = "subagents", kind = "subagents", text = ("Waiting for %d %s"):format(count, noun) }
  end
  if status.kind == "working" then
    local elapsed_seconds = math.max(0, math.floor((os.time() * 1000 - (status.started_at_ms or 0)) / 1000))
    local label = status.activity == "planning" and "Thinking" or "Working"
    return {
      id = "working",
      kind = "working",
      text = ("%s for %ds"):format(label, elapsed_seconds),
      started_at_ms = status.started_at_ms,
    }
  end
  return nil
end

--- Record the resolved status and its state inputs when the visible status changes.
---@param state DiffReviewHarnessPresentationState
---@param status table?
function M.record(state, status)
  status_trace(state, status)
end

local function stop_timer(state)
  local timer = state.timeline_status_timer
  if timer and not timer:is_closing() then
    timer:stop()
    timer:close()
  end
  state.timeline_status_timer = nil
end

---@param state DiffReviewHarnessPresentationState
function M.stop(state)
  stop_timer(state)
  if state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf) then
    vim.api.nvim_buf_clear_namespace(state.transcript_buf, namespace, 0, -1)
  end
  state.timeline_status_line = nil
end

local function draw(state)
  local buf = state.transcript_buf
  local line = state.timeline_status_line
  if not (buf and vim.api.nvim_buf_is_valid(buf) and line) then return end
  if line > vim.api.nvim_buf_line_count(buf) then return end
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  if state.timeline_status_animated == false then return end
  vim.api.nvim_buf_set_extmark(buf, namespace, line - 1, 0, {
    virt_text = { { render.frame_at(vim.uv.now()), "DiffReviewTimelineStatusSpinner" } },
    virt_text_pos = "overlay",
    hl_mode = "combine",
  })
end

---@param state DiffReviewHarnessPresentationState
---@param line integer?
function M.synchronize(state, line)
  state.timeline_status_line = line
  local status = M.resolve(state)
  state.timeline_status_animated = status == nil or status.kind ~= "error"
  if not line then
    M.stop(state)
    return
  end
  draw(state)
  if not state.timeline_status_animated then
    stop_timer(state)
    return
  end
  if state.timeline_status_timer then return end
  state.timeline_status_timer = vim.uv.new_timer()
  state.timeline_status_timer:start(120, 120, vim.schedule_wrap(function()
    if state.timeline_status_line then draw(state) else stop_timer(state) end
  end))
end

return M
