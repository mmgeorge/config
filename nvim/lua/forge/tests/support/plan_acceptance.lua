-- Plan acceptance asks how execution handles tool approval before execution starts. Host fixtures
-- that accept a plan answer that review question through the same routes the question picker uses.
local client = require("forge.client")
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/state.lua")
local M = {}

local approval_question_prefix = "review-execution-mode:"

---@param session_id string
---@param method string
---@param params table
---@return table
local function request(session_id, method, params)
  local completed, result, failure = false, nil, nil
  client.request({ session_id = session_id, method = method, params = params }, function(response, diagnostic)
    result, failure, completed = response, diagnostic, true
  end)
  assert(vim.wait(10000, function() return completed end), method .. " did not complete within 10 seconds")
  assert(not failure, failure)
  return result
end

---@param snapshot table
---@return table?
local function approval_review(snapshot)
  local review = snapshot.active_elicitation
  local elicitation = review and review.elicitation
  if not elicitation then return nil end
  for _, question in ipairs(elicitation.question_set.questions) do
    if vim.startswith(question.id, approval_question_prefix) then
      return { review = review, elicitation = elicitation, question = question }
    end
  end
end

---Answer the pending execution tool-approval question and confirm the review answers.
---@param session_id string
---@param mode "ask"|"deny"|"allow"
function M.answer_execution_approval(session_id, mode)
  local pending
  assert(vim.wait(10000, function()
    -- Reconnect through the controller so the snapshot's publication page is replayed, not consumed.
    pending = approval_review(state_snapshot.get())
    return pending ~= nil
  end, 10), "Plan acceptance did not ask for execution tool approval")
  local target = { owner = pending.review.owner, id = pending.review.exchange_id or pending.review.plan_id,
    set_id = pending.elicitation.question_set.id, revision = pending.elicitation.revision }
  local answered = request(session_id, "session.question.answer", { target = target, question_id = pending.question.id,
    operation_id = require("forge.command").identity(), response = { kind = "selected", option = mode } })
  local current = answered.active_elicitation and answered.active_elicitation.elicitation
  target.revision = current and current.revision or target.revision
  request(session_id, "session.question.continue", { target = target })
end

return M
