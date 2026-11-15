local M = {}

---@class ForgeUserExchangeOpening
---@field kind 'user'
---@field content table

---@class ForgeTaskStartedOpening
---@field kind 'task_started'
---@field start { kind: 'goal'|'plan'|'execute', title: string }

---@class ForgeTaskContinuationOpening
---@field kind 'task_continuation'
---@field reason string

---@class ForgeNativeAgentOpening
---@field kind 'native_agent'

---@alias ForgeExchangeOpening ForgeUserExchangeOpening|ForgeTaskStartedOpening|ForgeTaskContinuationOpening|ForgeNativeAgentOpening

---Identify the user-facing task notice independently of provider instruction text.
---@param opening? ForgeExchangeOpening
---@return string?
function M.notice(opening)
  if not opening then return nil end
  if opening.kind == "task_started" then
    local label = ({ goal = "Goal", plan = "Plan", execute = "Execution" })[opening.start.kind]
    return label .. " started: " .. opening.start.title
  elseif opening.kind == "task_continuation" then
    return opening.reason
  end
end

return M
