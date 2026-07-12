---@class DiffReviewViewCommandAction
---@field id string
---@field run fun(context: table)
---@field enabled? fun(context: table): boolean

---@class DiffReviewViewCommandSet
---@field action_by_id table<string, DiffReviewViewCommandAction>
---@field order string[]

---@class DiffReviewViewCommandSetModule
local M = {}

---@return DiffReviewViewCommandSet
function M.new()
  return { action_by_id = {}, order = {} }
end

--- Register a view command so a buffer keymap can resolve it through the active view.
---@param set DiffReviewViewCommandSet
---@param id string
---@param run fun(context: table)
---@param opts? { enabled?: fun(context: table): boolean }
function M.register(set, id, run, opts)
  if not set.action_by_id[id] then set.order[#set.order + 1] = id end
  set.action_by_id[id] = { id = id, run = run, enabled = opts and opts.enabled or nil }
end

--- Unregister a view command so unsupported capabilities disappear from hints and help.
---@param set DiffReviewViewCommandSet
---@param id string
function M.unregister(set, id)
  if not set.action_by_id[id] then return end
  set.action_by_id[id] = nil
  for index, command_id in ipairs(set.order) do
    if command_id == id then
      table.remove(set.order, index)
      return
    end
  end
end

---@param set DiffReviewViewCommandSet
---@param id string
---@return DiffReviewViewCommandAction?
function M.action(set, id)
  return set.action_by_id[id]
end

---@param set DiffReviewViewCommandSet
---@return string[]
function M.command_ids(set)
  return vim.deepcopy(set.order)
end

--- Resolve and run a command, honoring its enabled guard.
--- Return false when the command is unknown or disabled so the caller can fall back.
---@param set DiffReviewViewCommandSet
---@param id string
---@param context table
---@return boolean dispatched
function M.dispatch(set, id, context)
  local action = set.action_by_id[id]
  if not action then return false end
  if action.enabled and not action.enabled(context) then return false end
  action.run(context)
  return true
end

return M
