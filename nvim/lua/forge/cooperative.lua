local M = {}
local task_by_thread = setmetatable({}, { __mode = "k" })

---@class ForgeCooperativeTask
---@field alive fun(): boolean
---@field started integer
---@field units integer
---@field slices integer
---@field maximum_ms number
---@field maximum_prepare_ms number
---@field maximum_commit_ms number
---@field atomic_ms number
---@field atomic? boolean

--- Yields owned work after 1024 units or four milliseconds. Synchronous callers do not yield.
---@param units? integer
function M.checkpoint(units)
  local thread = coroutine.running()
  local task = thread and task_by_thread[thread]
  if not task or task.atomic then return end
  task.units = task.units + (units or 1)
  if task.units < 1024 and (vim.uv.hrtime() - task.started) < 4000000 then return end
  coroutine.yield()
end

--- Commits prepared editor changes without exposing intermediate frames between checkpoints.
---@param work fun(): any
---@return any result
function M.atomic(work)
  local thread = coroutine.running()
  local task = thread and task_by_thread[thread]
  if not task then return work() end
  local previous = task.atomic
  local started = not previous and vim.uv.hrtime()
  task.atomic = true
  local ok, result = pcall(work)
  task.atomic = previous
  if started then
    local elapsed = (vim.uv.hrtime() - started) / 1e6
    task.atomic_ms = task.atomic_ms + elapsed
    task.maximum_commit_ms = math.max(task.maximum_commit_ms, elapsed)
  end
  if not ok then error(result, 0) end
  return result
end

--- Runs ordered work in scheduled slices and reports cancellation before another mutation.
---@param work fun(): any
---@param alive fun(): boolean
---@param done fun(result: any, failure: string?, timing: ForgeCooperativeTask)
---@param after_slice? fun() Records the owner's source identity after its own writes.
function M.run(work, alive, done, after_slice)
  local thread = coroutine.create(work)
  local task = { alive = alive, started = 0, units = 0, slices = 0, maximum_ms = 0,
    atomic_ms = 0, maximum_prepare_ms = 0, maximum_commit_ms = 0 }
  task_by_thread[thread] = task
  local advance
  advance = function()
    if not alive() then
      task_by_thread[thread] = nil
      done(nil, "Document update cancelled", task)
      return
    end
    task.started, task.units, task.atomic_ms = vim.uv.hrtime(), 0, 0
    local accepted, result = coroutine.resume(thread)
    task.slices = task.slices + 1
    local elapsed = (vim.uv.hrtime() - task.started) / 1e6
    task.maximum_ms = math.max(task.maximum_ms, elapsed)
    task.maximum_prepare_ms = math.max(task.maximum_prepare_ms, elapsed - task.atomic_ms)
    if after_slice then after_slice() end
    if not accepted or coroutine.status(thread) == "dead" then
      task_by_thread[thread] = nil
      done(accepted and result or nil, not accepted and tostring(result) or nil, task)
    else
      vim.defer_fn(advance, 1)
    end
  end
  advance()
end

return M
