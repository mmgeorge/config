local M = {}

local queues = {}
local queue_count = 0

local function advance(queue)
  if queue.active then return end
  if #queue.pending == 0 then
    queues[queue.key] = nil
    queue_count = queue_count - 1
    return
  end
  queue.active = true
  local item = table.remove(queue.pending, 1)
  local delivered = false
  local function finish(result, failure)
    if delivered then return end
    delivered = true
    if failure then vim.notify("Review draft persistence failed: " .. tostring(failure), vim.log.levels.ERROR, { title = "Forge" }) end
    if item.callback then
      local notified, callback_failure = pcall(item.callback, failure and tostring(failure) or nil, result)
      if not notified then vim.notify("Review draft completion failed: " .. tostring(callback_failure), vim.log.levels.ERROR, { title = "Forge" }) end
    end
    queue.active = false
    advance(queue)
  end
  local params = { resource = queue.resource }
  local route = "github.review.draft"
  if not item.read then
    local built, payload = pcall(item.build)
    if not built or type(payload) ~= "table" then finish(nil, tostring(payload)) return end
    params.draft = payload
    route = "github.review.draft.write"
    if item.record then
      params.resource = item.record.resource
      params.operation_id = item.record.capture.operation_id
      route = "github.recovery.settle_draft"
    end
  end
  local admitted, failure = pcall(function() require("forge.client").request_host(route, params, finish) end)
  if not admitted then finish(nil, failure) end
end

local function enqueue_item(state, item)
  local owner, name = tostring(state.pr and state.pr.repo or ""):match("^([^/]+)/([^/]+)$")
  local function reject(failure)
    vim.notify(failure, vim.log.levels.ERROR, { title = "Forge" })
    if item.callback then item.callback(failure) end
  end
  if not owner then reject("Review draft has no repository identity") return end
  local hostname = require("github.repo_cache").hostname()
  local key = table.concat({ hostname, owner, name, tostring(state.pr.number) }, "/")
  local queue = queues[key]
  if not queue then
    if queue_count >= 64 then reject("Review draft resource admission is full") return end
    queue = { key = key, active = false, pending = {}, resource = {
      repository = { hostname = hostname, owner = owner, name = name }, kind = "pull_request", number = state.pr.number,
    } }
    queues[key] = queue
    queue_count = queue_count + 1
  end
  local last = queue.pending[#queue.pending]
  if not item.read and not item.record and not item.callback and last
    and last.state == state and not last.read and not last.record and not last.callback then
    last.build = item.build
  else
    if #queue.pending >= 64 then reject("Review draft queue is full") return end
    item.state = state
    queue.pending[#queue.pending + 1] = item
  end
  advance(queue)
end

function M.enqueue(state, build, callback, record)
  enqueue_item(state, { build = build, callback = callback, record = record })
end

function M.read(state, callback)
  enqueue_item(state, { read = true, callback = function(failure, result) callback(result, failure) end })
end

return M
