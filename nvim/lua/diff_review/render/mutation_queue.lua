---@class DiffReviewMutationQueue
---@field queue fun(done: fun())[]
---@field running boolean
---@field idle_callback fun()[]
---@field generation integer

---@class DiffReviewMutationQueueModule
local M = {}

---@return DiffReviewMutationQueue
function M.new()
  return {
    queue = {},
    running = false,
    idle_callback = {},
    generation = 0,
  }
end

---@param queue DiffReviewMutationQueue
---@return boolean
function M.pending(queue)
  return queue.running or #(queue.queue or {}) > 0
end

---@param queue DiffReviewMutationQueue
local function flush_idle_callback(queue)
  if M.pending(queue) then return end
  local callback = queue.idle_callback or {}
  queue.idle_callback = {}
  for _, idle_callback in ipairs(callback) do
    idle_callback()
  end
end

---@param queue DiffReviewMutationQueue
local function run_next(queue)
  if queue.running then return end
  local operation = table.remove(queue.queue, 1)
  if not operation then
    flush_idle_callback(queue)
    return
  end
  queue.running = true
  operation(function()
    queue.running = false
    run_next(queue)
  end)
end

---@param queue DiffReviewMutationQueue
---@param operation fun(done: fun())
function M.enqueue(queue, operation)
  queue.generation = (queue.generation or 0) + 1
  queue.queue = queue.queue or {}
  queue.queue[#queue.queue + 1] = operation
  run_next(queue)
end

---@param queue DiffReviewMutationQueue
---@param callback fun()
function M.on_idle(queue, callback)
  if not M.pending(queue) then
    callback()
    return
  end
  queue.idle_callback = queue.idle_callback or {}
  queue.idle_callback[#queue.idle_callback + 1] = callback
end

return M
