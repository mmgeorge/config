---@class DiffReviewStatusViewController
---@field view_kind string
---@field sources? fun(state: table)
---@field head_rows? fun(state: table)
---@field sections? fun(state: table)
---@field command_set? fun(state: table): DiffReviewViewCommandSet
---@field after_render? fun(state: table)

---@class DiffReviewViewControllerModule
local M = {}

---@type table<string, DiffReviewStatusViewController>
local registry = {}

--- Validate and return a controller spec for one status-like view kind.
---@param spec DiffReviewStatusViewController
---@return DiffReviewStatusViewController
function M.new(spec)
  assert(type(spec) == "table" and type(spec.view_kind) == "string" and spec.view_kind ~= "", "view controller requires a view_kind")
  return spec
end

---@param controller DiffReviewStatusViewController
function M.register(controller)
  registry[M.new(controller).view_kind] = controller
end

---@param view_kind string
---@return DiffReviewStatusViewController?
function M.resolve(view_kind)
  return registry[view_kind]
end

---@param state table
---@return DiffReviewStatusViewController?
function M.for_state(state)
  return state and registry[state.view_kind] or nil
end

--- Run a controller hook by name if the controller defines it.
---@param view_kind string
---@param hook "sources"|"head_rows"|"sections"|"after_render"
---@param state table
---@return boolean ran
function M.run_hook(view_kind, hook, state)
  local controller = registry[view_kind]
  local fn = controller and controller[hook] or nil
  if type(fn) ~= "function" then return false end
  fn(state)
  return true
end

--- Clear registered controllers (test seam).
function M.reset()
  registry = {}
end

return M
