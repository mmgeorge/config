local M = {}

local keymaps = require("diff_review.shared.keymaps")
local render = require("diff_review.render.harness.timeline_status")

local namespace = vim.api.nvim_create_namespace("diff_review_harness_timeline_status")

---@param state DiffReviewHarnessPresentationState
---@return table?
function M.resolve(state)
  local elicitation = state.active_elicitation and state.active_elicitation.elicitation
  if elicitation then
    local reopen_key = keymaps.view_keys_for("harness", "reopen_question")[1]
    local suffix = reopen_key and (" (press " .. reopen_key .. ")") or ""
    return { id = "input", kind = "input", text = "Waiting for input" .. suffix }
  end
  local wait = state.active_wait
  if not wait then return nil end
  local count = wait.agent_count or 0
  local noun = count == 1 and "subagent" or "subagents"
  return { id = "subagents", kind = "subagents", text = ("Waiting for %d %s"):format(count, noun) }
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
  if not line then
    M.stop(state)
    return
  end
  draw(state)
  if state.timeline_status_timer then return end
  state.timeline_status_timer = vim.uv.new_timer()
  state.timeline_status_timer:start(120, 120, vim.schedule_wrap(function()
    if state.timeline_status_line then draw(state) else stop_timer(state) end
  end))
end

return M
