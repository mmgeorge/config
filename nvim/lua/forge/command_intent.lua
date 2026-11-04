local M = {}

-- This table describes caller intent only. Rust captures source revisions and owns every transition.
local routes = {
  cancel_exchange = { default = "session.turn.cancel" },
  configure_session = {
    settings = "session.configure", permission = "session.permission.set",
    check_preferences = "session.check.override", input_timeouts = "session.input.timeout.configure",
    check_editing = "session.check.editing.set",
  },
  compact_context = { default = "session.compact" },
  task = {
    start_goal = "session.goal.set", pause = "session.goal.pause", resume = "session.goal.resume",
    clear = "session.goal.clear", execute_plan = "session.plan.accept",
    request_plan_changes = "session.plan.changes.request", cancel_plan = "session.plan.cancel",
    rename_entity = "session.plan.entity.rename", delete_tests = "session.plan.test.delete",
    update_checks = "session.plan.check.update",
    start_plan = "session.command.apply", attach_plan = "session.command.apply",
    fork_plan = "session.command.apply", replan = "session.command.apply", set_auto_accept = "session.command.apply",
  },
  continue_workflow = { default = "session.question.continue" },
  agent = {
    start = "session.agent.start", message = "session.agent.message.send", followup = "session.agent.followup",
    interrupt = "session.agent.interrupt", interrupt_all = "session.agent.interrupt_all",
  },
  rollback = {
    prepare = "session.exchange.rollback.prepare", apply = "session.exchange.rollback.apply",
    resume_restore = "session.exchange.recovery", prepare_undo_restore = "session.exchange.recovery",
  },
  input = { default = "session.input.apply", steer = "session.turn.steer", submit = "session.prompt.submit" },
}

local task_actions = {
  goal = "start_goal", plan = "start_plan", attach_plan = "attach_plan", fork = "fork_plan",
  replan = "replan", plan_accept = "set_auto_accept", execute = "execute_plan", pause = "pause",
  resume = "resume", clear = "clear", request_changes = "request_plan_changes", cancel_plan = "cancel_plan",
}
local action_names = {
  start_plan = "plan", attach_plan = "attach_plan", fork_plan = "fork", replan = "replan", set_auto_accept = "plan_accept",
}
local aliases = {}
for family, actions in pairs(routes) do
  for action, method in pairs(actions) do
    if method ~= "session.command.apply" and method ~= "session.exchange.recovery" then
      aliases[method] = { command = family, action = action ~= "default" and action or nil }
    end
  end
end

---@alias ForgeCommandFamily 'cancel_exchange'|'configure_session'|'compact_context'|'task'|'continue_workflow'|'agent'|'rollback'|'input'
---@class ForgeCommandIntent
---@field command ForgeCommandFamily Caller operation family; no captured backend authority.
---@field action? string Family-specific operation, such as task.execute_plan or agent.start.
---@field request? table User content and observed public identities only.
---@class ForgeCommandSpec
---@field session_id string Session whose owner receives the command.
---@field command ForgeCommandIntent

---Recognize legacy lifecycle aliases without interpreting provider or workflow state.
---@param spec ForgeRequestSpec
---@return ForgeCommandSpec?
function M.from_request(spec)
  if type(spec) ~= "table" or not spec.session_id then return end
  if spec.params ~= nil and type(spec.params) ~= "table" then return end
  local alias = aliases[spec.method]
  local params = spec.params or {}
  if spec.method == "session.command.apply" then
    local action = task_actions[params.action]
    if action then alias = { command = "task", action = action }
    elseif params.action == "continue" then alias = { command = "continue_workflow" }
    elseif params.action == "configure" then alias = { command = "configure_session", action = "settings" }
    elseif params.action == "permission" then alias = { command = "configure_session", action = "permission" }
    elseif params.action == "compact" then alias = { command = "compact_context" }
    elseif params.action == "cancel" or params.action == "stop" then alias = { command = "cancel_exchange" }
    elseif params.action == "undo" then alias = { command = "rollback", action = "apply" }
    end
  elseif spec.method == "session.exchange.recovery" then
    if params.action == "continue" then alias = { command = "rollback", action = "resume_restore" }
    elseif params.action == "undo" then alias = { command = "rollback", action = "prepare_undo_restore" }
    end
  end
  if not alias then return end
  return { session_id = spec.session_id, command = { command = alias.command, action = alias.action, request = params } }
end

---Encode public caller intent using existing IPC aliases; Rust supplies private source guards.
---@param spec ForgeCommandSpec
---@return ForgeRequestSpec
function M.to_request(spec)
  assert(type(spec) == "table" and type(spec.session_id) == "string" and spec.session_id:find("%S"), "Command requires a session_id")
  local command = spec.command
  assert(type(command) == "table", "Command intent is required")
  local family = routes[command.command]
  assert(family, "Unknown command family: " .. tostring(command.command))
  local method = family[command.action or "default"]
  assert(method, "Unknown command action: " .. tostring(command.action))
  assert(command.request == nil or type(command.request) == "table", "Command request must be an object")
  local params = vim.deepcopy(command.request or {})
  if method == "session.command.apply" then params.action = assert(action_names[command.action]) end
  if method == "session.exchange.recovery" then params.action = command.action == "resume_restore" and "continue" or "undo" end
  return { session_id = spec.session_id, method = method, params = params }
end

return M
