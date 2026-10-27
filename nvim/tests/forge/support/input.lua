local M = {}
local input = require("forge.views.harness.input_state")

---@param state table
---@return table
function M.new(state)
  local host = { revision = 0, messages = {}, operations = {}, requests = {} }
  local function position(id)
    for index, message in ipairs(host.messages) do if message.id == id then return index end end
  end
  function host.request(method, parameters, callback)
    if method == "input.get" then
      callback({ revision = host.revision, messages = vim.deepcopy(host.messages), suspended = false, execution_busy = false })
    elseif method == "input.operation" then callback(host.operations[parameters.id])
    elseif method == "input.apply" then
      host.requests[#host.requests + 1] = { parameters = parameters, callback = callback }
    else return false end
    return true
  end
  function host.complete(request, failure)
    request = request or host.requests[#host.requests]
    local parameters = request.parameters
    if request.completed then request.callback(vim.deepcopy(host.operations[parameters.id])) return end
    local update = { operation_id = parameters.id, upsert = {}, removed = {}, suspended = false }
    if failure then update.error = failure
    else
      local action = parameters.action
      if action.kind == "submit" then
        local message = vim.deepcopy(action.message)
        message.revision = 1
        host.messages[#host.messages + 1] = message
        update.upsert[1] = vim.deepcopy(message)
      elseif action.kind == "reserve" then
        local message = assert(host.messages[position(action.id)])
        assert(message.revision == action.revision)
        message.revision, message.reservation = message.revision + 1, action.reservation
        update.upsert[1] = vim.deepcopy(message)
      elseif action.kind == "commit" then
        for _, edit in ipairs(action.edits) do
          local index = assert(position(edit.id))
          local message = host.messages[index]
          assert(message.revision == edit.revision and message.reservation == action.reservation)
          if edit.text == "" and not edit.config then
            table.remove(host.messages, index)
            update.removed[#update.removed + 1] = edit.id
          else
            message.text, message.attachments, message.revision, message.reservation =
              edit.text, vim.deepcopy(edit.attachments), message.revision + 1, nil
            if edit.config then message.config = vim.deepcopy(edit.config) end
            update.upsert[#update.upsert + 1] = vim.deepcopy(message)
          end
        end
      elseif action.kind == "remove" then
        table.remove(host.messages, assert(position(action.id)))
        update.removed[1] = action.id
      end
    end
    host.revision = host.revision + 1
    update.revision = host.revision
    host.operations[parameters.id] = vim.deepcopy(update)
    request.callback(update)
    request.completed = true
    return update
  end
  function host.flush()
    local index = 1
    while index <= #host.requests do
      if not host.requests[index].completed then host.complete(host.requests[index]) end
      index = index + 1
    end
  end
  function host.seed(messages)
    host.messages = vim.deepcopy(messages)
    for index, message in ipairs(host.messages) do
      message.id, message.revision, message.delivery = message.id or ("message-" .. index), 1, "queued"
      message.attachments = message.attachments or {}
    end
    host.revision = host.revision + 1
    input.refresh(state)
  end
  function host.deliver(id)
    table.remove(host.messages, assert(position(id)))
    host.revision = host.revision + 1
    input.receive(state, { revision = host.revision, suspended = false, upsert = {}, removed = { id } })
  end
  return host
end

return M
