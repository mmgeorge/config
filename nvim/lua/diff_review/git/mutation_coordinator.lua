--- Coordinates repository-scoped Git index mutations and their authoritative sync boundary.
local M = {}

local paths = require("diff_review.infra.paths")

---@class DiffReviewMutationResult
---@field ok boolean
---@field error? string
---@field failure? table
---@field count? integer

---@class DiffReviewMutationTask
---@field id? integer
---@field burst_id? integer
---@field label string
---@field paths string[]
---@field execute fun(done: fun(result: DiffReviewMutationResult))
---@field on_enqueue? fun(task: DiffReviewMutationTask)
---@field on_complete? fun(result: DiffReviewMutationResult, task: DiffReviewMutationTask)
---@field on_cancel? fun(task: DiffReviewMutationTask)
---@field metadata? table
---@field result? DiffReviewMutationResult

---@class DiffReviewMutationBurst
---@field id integer
---@field root string
---@field tasks DiffReviewMutationTask[]
---@field successful_tasks DiffReviewMutationTask[]
---@field cancelled_tasks DiffReviewMutationTask[]
---@field path_set table<string, boolean>
---@field failed_task? DiffReviewMutationTask
---@field failure? DiffReviewMutationResult

---@class DiffReviewMutationHandler
---@field settle fun(burst: DiffReviewMutationBurst, done: fun(ok: boolean))
---@field recover fun(burst: DiffReviewMutationBurst, done: fun(ok: boolean))

---@class DiffReviewMutationRootState
---@field root string
---@field queue DiffReviewMutationTask[]
---@field running_task? DiffReviewMutationTask
---@field accepting_burst? DiffReviewMutationBurst
---@field syncing_burst? DiffReviewMutationBurst
---@field recovering_burst? DiffReviewMutationBurst
---@field handler? DiffReviewMutationHandler
---@field next_task_id integer
---@field next_burst_id integer
---@field quiet_generation integer
---@field quiet_delay_ms integer

---@type table<string, DiffReviewMutationRootState>
local state_by_root = {}

local default_quiet_delay_ms = 120

---@param root string
---@return string
local function root_key(root)
  local normalized = paths.normalize_path(root)
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

---@param root string
---@return DiffReviewMutationRootState
local function state_for_root(root)
  local key = root_key(root)
  local state = state_by_root[key]
  if state then return state end
  state = {
    root = root,
    queue = {},
    next_task_id = 1,
    next_burst_id = 1,
    quiet_generation = 0,
    quiet_delay_ms = default_quiet_delay_ms,
  }
  state_by_root[key] = state
  return state
end

---@param state DiffReviewMutationRootState
---@return DiffReviewMutationBurst
local function accepting_burst(state)
  if state.accepting_burst then return state.accepting_burst end
  local burst = {
    id = state.next_burst_id,
    root = state.root,
    tasks = {},
    successful_tasks = {},
    cancelled_tasks = {},
    path_set = {},
  }
  state.next_burst_id = state.next_burst_id + 1
  state.accepting_burst = burst
  return burst
end

---@param burst DiffReviewMutationBurst
---@return string[]
local function burst_paths(burst)
  local result = {}
  for path in pairs(burst.path_set) do
    result[#result + 1] = path
  end
  table.sort(result)
  return result
end

---@param state DiffReviewMutationRootState
local function run_next(state) end

---@param state DiffReviewMutationRootState
---@param burst DiffReviewMutationBurst
local function finish_sync(state, burst)
  if state.syncing_burst == burst then state.syncing_burst = nil end
  if state.recovering_burst == burst then state.recovering_burst = nil end
  run_next(state)
end

---@param state DiffReviewMutationRootState
---@param burst DiffReviewMutationBurst
local function recover_burst(state, burst)
  state.recovering_burst = burst
  local remaining_queue = {}
  for _, queued_task in ipairs(state.queue) do
    if queued_task.burst_id == burst.id then
      burst.cancelled_tasks[#burst.cancelled_tasks + 1] = queued_task
      if queued_task.on_cancel then queued_task.on_cancel(queued_task) end
    else
      remaining_queue[#remaining_queue + 1] = queued_task
    end
  end
  state.queue = remaining_queue
  state.accepting_burst = nil
  state.quiet_generation = state.quiet_generation + 1

  local handler = state.handler
  if not (handler and handler.recover) then
    finish_sync(state, burst)
    return
  end
  local completed = false
  handler.recover(burst, function()
    if completed then return end
    completed = true
    finish_sync(state, burst)
  end)
end

---@param state DiffReviewMutationRootState
---@param burst DiffReviewMutationBurst
local function schedule_settle(state, burst)
  state.quiet_generation = state.quiet_generation + 1
  local generation = state.quiet_generation
  vim.defer_fn(function()
    if state.quiet_generation ~= generation then return end
    if state.running_task or #state.queue > 0 or state.recovering_burst or state.syncing_burst then return end
    if state.accepting_burst ~= burst then return end

    state.accepting_burst = nil
    state.syncing_burst = burst
    local handler = state.handler
    if not (handler and handler.settle) then
      finish_sync(state, burst)
      return
    end
    local completed = false
    handler.settle(burst, function()
      if completed then return end
      completed = true
      finish_sync(state, burst)
    end)
  end, state.quiet_delay_ms)
end

---@param state DiffReviewMutationRootState
local function run_next_impl(state)
  if state.running_task or state.syncing_burst or state.recovering_burst then return end
  local task = table.remove(state.queue, 1)
  if not task then
    if state.accepting_burst then schedule_settle(state, state.accepting_burst) end
    return
  end

  state.running_task = task
  local completed = false
  local function complete(result)
    if completed then return end
    completed = true
    task.result = result
    state.running_task = nil

    local burst = state.accepting_burst
    if not burst or task.burst_id ~= burst.id then
      run_next(state)
      return
    end
    if task.on_complete then task.on_complete(result, task) end
    if result.ok then
      burst.successful_tasks[#burst.successful_tasks + 1] = task
      run_next(state)
      return
    end

    burst.failed_task = task
    burst.failure = result
    recover_burst(state, burst)
  end
  local execute_ok, execute_error = pcall(task.execute, complete)
  if not execute_ok then
    complete({ ok = false, error = tostring(execute_error) })
  end
end

run_next = run_next_impl

--- Register lifecycle handlers for one repository root.
---@param root string
---@param handler DiffReviewMutationHandler
function M.set_handler(root, handler)
  state_for_root(root).handler = handler
end

--- Enqueue one Git index mutation in repository FIFO order.
---@param root string
---@param task DiffReviewMutationTask
---@return integer? task_id
---@return integer? burst_id
---@return string? error
function M.enqueue(root, task)
  if not root or root == "" then return nil, nil, "Missing repository root" end
  local state = state_for_root(root)
  if state.recovering_burst then return nil, nil, "Git state recovery is in progress" end

  state.quiet_generation = state.quiet_generation + 1
  local burst = accepting_burst(state)
  task.id = state.next_task_id
  task.burst_id = burst.id
  state.next_task_id = state.next_task_id + 1
  if task.on_enqueue then
    local enqueue_ok, enqueue_error = pcall(task.on_enqueue, task)
    if not enqueue_ok then
      if #burst.tasks == 0 then state.accepting_burst = nil end
      return nil, nil, tostring(enqueue_error)
    end
  end
  burst.tasks[#burst.tasks + 1] = task
  for _, path in ipairs(task.paths or {}) do
    if path and path ~= "" then burst.path_set[path] = true end
  end
  state.queue[#state.queue + 1] = task
  run_next(state)
  return task.id, burst.id, nil
end

--- Report whether a repository still owns mutation or synchronization work.
---@param root string
---@return boolean
function M.pending(root)
  local state = state_by_root[root_key(root)]
  if not state then return false end
  return state.running_task ~= nil
    or #state.queue > 0
    or state.accepting_burst ~= nil
    or state.syncing_burst ~= nil
    or state.recovering_burst ~= nil
end

--- Report whether a repository currently recovers from a failed mutation.
---@param root string
---@return boolean
function M.recovering(root)
  local state = state_by_root[root_key(root)]
  return state ~= nil and state.recovering_burst ~= nil
end

--- Return the sorted path set touched by a burst.
---@param burst DiffReviewMutationBurst
---@return string[]
function M.paths(burst)
  return burst_paths(burst)
end

--- Return paths owned by a later burst while an earlier snapshot synchronizes.
---@param root string
---@return string[]
function M.pending_paths(root)
  local state = state_by_root[root_key(root)]
  if not (state and state.accepting_burst) then return {} end
  return burst_paths(state.accepting_burst)
end

--- Override the quiet delay for deterministic tests.
---@param root string
---@param delay_ms integer
function M.set_quiet_delay_for_test(root, delay_ms)
  state_for_root(root).quiet_delay_ms = delay_ms
end

--- Clear coordinator state for deterministic tests.
function M.reset_for_test()
  state_by_root = {}
end

return M
