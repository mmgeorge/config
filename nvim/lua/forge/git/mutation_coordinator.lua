--- Coordinates repository-scoped Git index mutations and their authoritative sync boundary.
local M = {}

local paths = require("forge.infra.paths")

---@class ForgeMutationResult
---@field ok boolean
---@field error? string
---@field failure? table
---@field count? integer

---@class ForgeMutationTask
---@field id? integer
---@field burst_id? integer
---@field label string
---@field paths string[]
---@field execute fun(done: fun(result: ForgeMutationResult))
---@field on_enqueue? fun(task: ForgeMutationTask)
---@field on_complete? fun(result: ForgeMutationResult, task: ForgeMutationTask)
---@field on_cancel? fun(task: ForgeMutationTask)
---@field metadata? table
---@field result? ForgeMutationResult

---@class ForgeMutationBurst
---@field id integer
---@field root string
---@field tasks ForgeMutationTask[]
---@field successful_tasks ForgeMutationTask[]
---@field cancelled_tasks ForgeMutationTask[]
---@field path_set table<string, boolean>
---@field failed_task? ForgeMutationTask
---@field failure? ForgeMutationResult

---@class ForgeMutationHandler
---@field settle fun(burst: ForgeMutationBurst, done: fun(ok: boolean))
---@field recover fun(burst: ForgeMutationBurst, done: fun(ok: boolean))

---@class ForgeMutationRootState
---@field root string
---@field queue ForgeMutationTask[]
---@field running_task? ForgeMutationTask
---@field accepting_burst? ForgeMutationBurst
---@field syncing_burst? ForgeMutationBurst
---@field recovering_burst? ForgeMutationBurst
---@field handler? ForgeMutationHandler
---@field next_task_id integer
---@field next_burst_id integer
---@field quiet_generation integer
---@field quiet_delay_ms integer
---@field idle_callback_list fun(error?: string)[]
---@field idle_error? string

---@type table<string, ForgeMutationRootState>
local state_by_root = {}

local default_quiet_delay_ms = 120

---@param root string
---@return string
local function root_key(root)
  local normalized = paths.normalize_path(root)
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

---@param root string
---@return ForgeMutationRootState
local function state_for_root(root)
  local key = root_key(root)
  local state = state_by_root[key]
  if state then return state end
  state = {
    root = root,
    queue = {},
    idle_callback_list = {},
    next_task_id = 1,
    next_burst_id = 1,
    quiet_generation = 0,
    quiet_delay_ms = default_quiet_delay_ms,
  }
  state_by_root[key] = state
  return state
end

---@param state ForgeMutationRootState
---@return ForgeMutationBurst
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

---@param burst ForgeMutationBurst
---@return string[]
local function burst_paths(burst)
  local result = {}
  for path in pairs(burst.path_set) do
    result[#result + 1] = path
  end
  table.sort(result)
  return result
end

---@param state ForgeMutationRootState
local function run_next(state) end

---@param state ForgeMutationRootState
---@param burst ForgeMutationBurst
---@param ok boolean
local function finish_sync(state, burst, ok)
  if burst.failure then
    state.idle_error = burst.failure.error or "Git index mutation failed"
  elseif not ok then
    state.idle_error = state.idle_error or "Git index verification failed"
  end
  if state.syncing_burst == burst then state.syncing_burst = nil end
  if state.recovering_burst == burst then state.recovering_burst = nil end
  run_next(state)
end

---@param state ForgeMutationRootState
---@param burst ForgeMutationBurst
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
    finish_sync(state, burst, true)
    return
  end
  local completed = false
  handler.recover(burst, function(ok)
    if completed then return end
    completed = true
    finish_sync(state, burst, ok)
  end)
end

---@param state ForgeMutationRootState
---@param burst ForgeMutationBurst
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
      finish_sync(state, burst, true)
      return
    end
    local completed = false
    handler.settle(burst, function(ok)
      if completed then return end
      completed = true
      finish_sync(state, burst, ok)
    end)
  end, state.quiet_delay_ms)
end

---@param state ForgeMutationRootState
local function run_next_impl(state)
  if state.running_task or state.syncing_burst or state.recovering_burst then return end
  local task = table.remove(state.queue, 1)
  if not task then
    if state.accepting_burst then
      schedule_settle(state, state.accepting_burst)
    else
      local callback_list = state.idle_callback_list
      local idle_error = state.idle_error
      state.idle_callback_list = {}
      state.idle_error = nil
      for _, callback in ipairs(callback_list) do
        local callback_ok, callback_error = pcall(callback, idle_error)
        if not callback_ok then vim.notify(tostring(callback_error), vim.log.levels.ERROR) end
      end
    end
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

--- Registers lifecycle settlement and recovery handlers for a repository root.
---@param root string Git repository root path.
---@param handler ForgeMutationHandler Handler callbacks for settling and recovering mutations.
function M.set_handler(root, handler)
  state_for_root(root).handler = handler
end

--- Enqueues a Git index mutation task in repository FIFO order.
--- Returns monotonic task and burst identifiers, or an error message if queueing fails.
---@param root string Git repository root path.
---@param task ForgeMutationTask Task specification table.
---@return integer? task_id Monotonic task identifier.
---@return integer? burst_id Batch burst identifier.
---@return string? error Error message if enqueueing is rejected.
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

--- Reports whether a repository root has active, queued, or synchronizing mutation tasks.
---@param root string Git repository root path.
---@return boolean pending True if tasks or synchronization are in progress.
function M.pending(root)
  local state = state_by_root[root_key(root)]
  if not state then return false end
  return state.running_task ~= nil
    or #state.queue > 0
    or state.accepting_burst ~= nil
    or state.syncing_burst ~= nil
    or state.recovering_burst ~= nil
end

---Invoke once after all repository mutations and synchronization finish, or immediately when idle.
---The callback receives any mutation or verification failure encountered while waiting.
---@param root string Git repository root path.
---@param callback fun(error?: string)
function M.when_idle(root, callback)
  if not M.pending(root) then
    callback()
    return
  end
  local state = state_for_root(root)
  state.idle_callback_list[#state.idle_callback_list + 1] = callback
end

--- Reports whether a repository root is currently rolling back a failed mutation.
---@param root string Git repository root path.
---@return boolean recovering True if recovery is active.
function M.recovering(root)
  local state = state_by_root[root_key(root)]
  return state ~= nil and state.recovering_burst ~= nil
end

--- Returns the sorted list of relative file paths affected by a mutation burst.
---@param burst ForgeMutationBurst Target mutation burst table.
---@return string[] paths Sorted unique file path strings.
function M.paths(burst)
  return burst_paths(burst)
end

--- Returns relative file paths in an active uncommitted burst while earlier work synchronizes.
---@param root string Git repository root path.
---@return string[] paths Array of affected path strings.
function M.pending_paths(root)
  local state = state_by_root[root_key(root)]
  if not (state and state.accepting_burst) then return {} end
  return burst_paths(state.accepting_burst)
end

--- Overrides the quiet debounce interval in milliseconds for test environments.
---@param root string Git repository root path.
---@param delay_ms integer Debounce delay duration in milliseconds.
function M.set_quiet_delay_for_test(root, delay_ms)
  state_for_root(root).quiet_delay_ms = delay_ms
end

--- Resets all coordinator state across repository roots for test cleanup.
function M.reset_for_test()
  state_by_root = {}
end

return M
