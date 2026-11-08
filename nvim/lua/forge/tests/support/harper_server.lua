-- An in-process stand-in for `harper-ls`. `install()` replaces `vim.lsp.rpc.start`, which Forge's
-- Harper transport calls to reach its server, with a connection that answers the language-server
-- protocol directly. Each connection is one server: it tracks the full text of every open document,
-- reports `RepeatedWords` for a repeated "the" and `SpellCheck` for "teh" unless the linter is
-- switched off in the settings it last received, and publishes those diagnostics with the
-- document's version after every open, change, or configuration update. A code action request
-- returns one edit that collapses the repeated word inside the requested range.
local M = {}

local function diagnostics_for(text, linters)
  local diagnostics = {}
  local row = 0
  for line in (text .. "\n"):gmatch("(.-)\n") do
    local function report(code, pattern, message)
      if linters[code] == false then return end
      local first = 1
      while true do
        local start, finish = line:lower():find(pattern, first)
        if not start then break end
        diagnostics[#diagnostics + 1] = { source = "Harper", code = code, message = message, severity = 2,
          range = { start = { line = row, character = start - 1 }, ["end"] = { line = row, character = finish } } }
        first = finish + 1
      end
    end
    report("RepeatedWords", "%f[%w]the the%f[%W]", "Repeated word")
    report("SpellCheck", "%f[%w]teh%f[%W]", "Unknown spelling")
    row = row + 1
  end
  return diagnostics
end

---@return { connections: table[] }
function M.install()
  local server = { connections = {} }
  vim.lsp.rpc.start = function(_, dispatchers)
    local connection = { documents = {}, linters = {}, closing = false }
    server.connections[#server.connections + 1] = connection
    local request_id = 0
    local function publish(uri)
      local document = connection.documents[uri]
      vim.schedule(function()
        if connection.closing or connection.documents[uri] ~= document then return end
        dispatchers.notification("textDocument/publishDiagnostics",
          { uri = uri, version = document.version, diagnostics = diagnostics_for(document.text, connection.linters) })
      end)
    end
    function connection.request(method, params, callback)
      request_id = request_id + 1
      local result
      if method == "initialize" then
        result = { capabilities = { textDocumentSync = 1, codeActionProvider = true } }
      elseif method == "textDocument/codeAction" then
        local document = connection.documents[params.textDocument.uri]
        local line = document and vim.split(document.text, "\n", { plain = true })[params.range.start.line + 1] or ""
        local start = line:lower():find("the the", 1, true)
        result = {}
        if start then
          local range = { start = { line = params.range.start.line, character = start - 1 },
            ["end"] = { line = params.range.start.line, character = start + 6 } }
          result[1] = { title = 'Replace with "the"', kind = "quickfix",
            edit = { changes = { [params.textDocument.uri] = { { range = range, newText = "the" } } } } }
        end
      end
      vim.schedule(function() if not connection.closing then callback(nil, result) end end)
      return true, request_id
    end
    function connection.notify(method, params)
      if method == "textDocument/didOpen" then
        connection.documents[params.textDocument.uri] = { text = params.textDocument.text, version = params.textDocument.version }
        publish(params.textDocument.uri)
      elseif method == "textDocument/didChange" then
        connection.documents[params.textDocument.uri] = { text = params.contentChanges[#params.contentChanges].text,
          version = params.textDocument.version }
        publish(params.textDocument.uri)
      elseif method == "textDocument/didClose" then
        connection.documents[params.textDocument.uri] = nil
      elseif method == "workspace/didChangeConfiguration" then
        local settings = params.settings and params.settings["harper-ls"] or {}
        connection.linters = settings.linters or {}
        for uri in pairs(connection.documents) do publish(uri) end
      elseif method == "exit" then
        connection.closing = true
      end
      return true
    end
    function connection.is_closing() return connection.closing end
    function connection.terminate() connection.closing = true end
    return connection
  end
  return server
end

return M
