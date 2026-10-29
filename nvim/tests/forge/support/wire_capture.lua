local M = {}

function M.start()
  local protocol = require("forge.protocol")
  local encode, decode = protocol.encode_request, protocol.decode_message
  local request_by_id, completed_by_method = {}, {}
  protocol.encode_request = function(...)
    local frame = encode(...)
    local request = vim.json.decode(frame)
    if request.method ~= "host.transport.consumed" then request_by_id[request.id] = request end
    return frame
  end
  protocol.decode_message = function(frame)
    local message, failure = decode(frame)
    if message then
      local response = vim.json.decode(frame)
      local request = request_by_id[response.id]
      if request then
        completed_by_method[request.method] = { request = request, response = response }
        request_by_id[response.id] = nil
      end
    end
    return message, failure
  end
  return {
    completed = function(method)
      return vim.deepcopy(assert(completed_by_method[method], "missing wire response for " .. method))
    end,
    close = function()
      protocol.encode_request, protocol.decode_message = encode, decode
    end,
  }
end

return M
