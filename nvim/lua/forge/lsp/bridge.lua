local query = require("forge.lsp.query")
local M = {}

---@type table<string, fun()>
local pending = {}

---@param event string
---@param payload table
---@param session_id string
---@param generation integer
function M.handle(event, payload, session_id, generation)
  local client = require("forge.client")
  if event == "lsp.cancel" then
    local cancel = pending[payload.id]
    pending[payload.id] = nil
    if cancel then cancel() end
    return
  end
  if pending[payload.id] then return end
  local function current()
    return client.host_get_generation() == generation and client.host_is_accepting()
  end
  local function authorize(paths, receive)
    local allowed, index = {}, 1
    local function next_batch()
      if not current() then receive(nil, "editor connection changed") return end
      if index > #paths then receive(allowed) return end
      local batch = {}
      for _ = 1, 64 do
        if index > #paths then break end
        batch[#batch + 1] = paths[index]
        index = index + 1
      end
      client.request({ session_id = session_id, method = "session.lsp.authorize", params = { id = payload.id, paths = batch } }, function(result, failure)
        if failure then receive(nil, failure) return end
        for path, permitted in pairs(result) do allowed[path] = permitted end
        next_batch()
      end)
    end
    next_batch()
  end
  local completed = false
  local function analyze(path, contents, receive)
    if not current() then receive(nil, "editor connection changed") return end
    local offset = 0
    local function next_part()
      if not current() then receive(nil, "editor connection changed") return end
      local ending = math.min(#contents, offset + 32768)
      while ending < #contents and contents:byte(ending + 1) >= 128 and contents:byte(ending + 1) < 192 do ending = ending - 1 end
      client.request({ session_id = session_id, method = "session.lsp.analyze", params = { id = payload.id, path = path,
        source = contents:sub(offset + 1, ending), source_offset = offset, source_total = #contents } }, function(result, failure)
        if failure then receive(nil, failure) return end
        offset = ending
        if offset < #contents then next_part() else receive(result) end
      end)
    end
    next_part()
  end
  local cancel = query.request(payload.workspace, payload.arguments, authorize, analyze, function(result, failure)
    completed = true
    pending[payload.id] = nil
    if not current() then return end
    client.request({ session_id = session_id, method = "session.lsp.respond", params = { id = payload.id, result = result, failure = failure } }, function(_, response_error)
      if response_error and not response_error:find("no longer active", 1, true) then
        require("forge.infra.notifications").error(response_error, "Harness LSP")
      end
    end)
  end)
  if not completed then pending[payload.id] = cancel end
end

function M.close()
  local active = pending
  pending = {}
  for _, cancel in pairs(active) do cancel() end
  require("forge.lsp.startup").close()
end

return M
