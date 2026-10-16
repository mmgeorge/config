local source = require("forge.lsp.source")
local M = {}

local methods = {
  definition = "textDocument/definition", type_definition = "textDocument/typeDefinition",
  implementation = "textDocument/implementation", references = "textDocument/references",
  hover = "textDocument/hover", document_symbols = "textDocument/documentSymbol",
  workspace_symbols = "workspace/symbol", calls = "textDocument/prepareCallHierarchy",
  diagnostics = "textDocument/diagnostic",
}

---@class ForgeLspArguments
---@field operation string
---@field path? string
---@field line? integer
---@field column? integer
---@field query? string
---@field direction? string
---@field limit? integer
---@field offset? integer

---@class ForgeLspRow
---@field path string
---@field line integer
---@field column integer
---@field text string
---@field modified? boolean

---@class ForgeLspResult
---@field rows ForgeLspRow[]
---@field source string
---@field next_offset? integer
---@field warnings? string[]
---@field hover? string
---@field freshness? string

---@class ForgeLspRawRow
---@field path string
---@field position table
---@field encoding string
---@field text? string

local function shorten(text, limit)
  text = tostring(text or ""):gsub("%s+", " ")
  if vim.fn.strchars(text) > limit then return vim.fn.strcharpart(text, 0, limit - 1) .. "…" end
  return text
end

local function hover_text(value)
  if type(value) == "string" then return value end
  if type(value) ~= "table" then return "" end
  if value.value then return value.value end
  local parts = {}
  for _, part in ipairs(value) do parts[#parts + 1] = hover_text(part) end
  return table.concat(parts, "\n")
end

local function bounded_hover(text)
  if #text <= 4096 then return text end
  local last = 4080
  while text:byte(last + 1) >= 128 and text:byte(last + 1) < 192 do last = last - 1 end
  return text:sub(1, last) .. "\n[truncated]\n"
end

---@param workspace string
---@param arguments ForgeLspArguments
---@param authorize fun(paths: string[], receive: fun(allowed: table<string, boolean>?, failure: string?))
---@param receive fun(result: ForgeLspResult?, failure: string?)
---@return fun() cancel
function M.request(workspace, arguments, authorize, receive)
  local finished, outstanding, attempted, responses, collecting = false, 0, 0, 0, false
  local requests, buffers, versions, clients, raw, warnings = {}, {}, {}, {}, {}, {}
  local result = { rows = {}, source = "workspace" }
  local deadline = assert(vim.uv.new_timer())
  local guard = assert(vim.uv.new_timer())
  local cancel_startup
  local function finish(failure)
    if finished then return end
    finished = true
    if cancel_startup then cancel_startup() end
    deadline:stop() deadline:close()
    guard:stop() guard:close()
    for _, request in ipairs(requests) do request.client:cancel_request(request.id) end
    for buffer in pairs(buffers) do
      source.release(buffer)
    end
    result.warnings = #warnings > 0 and warnings or nil
    receive(not failure and result or nil, failure)
  end
  local function valid()
    if finished then return false end
    for buffer, version in pairs(versions) do
      if not vim.api.nvim_buf_is_valid(buffer) or vim.api.nvim_buf_get_changedtick(buffer) ~= version then
        finish("stale_source: source buffer changed during the request") return false
      end
    end
    for _, client in pairs(clients) do
      if client:is_stopped() then finish("unavailable: language server stopped") return false end
    end
    return true
  end
  guard:start(50, 50, vim.schedule_wrap(valid))
  local function location(value, client, label)
    local uri = value.targetUri or value.uri
    local range = value.targetSelectionRange or value.selectionRange or value.range or value.targetRange
    if type(uri) ~= "string" or uri:sub(1, 7) ~= "file://" or not range or not range.start then
      warnings[#warnings + 1] = "A server location had no supported file position"
      return
    end
    raw[#raw + 1] = { path = source.canonical(vim.uri_to_fname(uri)), position = range.start,
      encoding = client.offset_encoding or "utf-16", text = label }
  end
  local function symbols(values, client, path, parent)
    for _, symbol in ipairs(values or {}) do
      local name = parent and (parent .. "." .. symbol.name) or symbol.name
      if not arguments.query or arguments.operation == "workspace_symbols" or name:lower():find(arguments.query:lower(), 1, true) then
        location(symbol.location or { uri = vim.uri_from_fname(path), range = symbol.selectionRange or symbol.range }, client,
          (vim.lsp.protocol.SymbolKind[symbol.kind] or "Symbol") .. " " .. name)
      end
      if symbol.children then symbols(symbol.children, client, path, name) end
    end
  end
  local function collect()
    if outstanding ~= 0 or collecting or not valid() then return end
    if attempted > 0 and responses == 0 then finish("server_error: " .. table.concat(warnings, "; ")) return end
    collecting = true
    if result.hover then finish() return end
    table.sort(raw, function(left, right)
      if left.path ~= right.path then return left.path < right.path end
      if left.position.line ~= right.position.line then return left.position.line < right.position.line end
      if left.position.character ~= right.position.character then return left.position.character < right.position.character end
      return (left.text or "") < (right.text or "")
    end)
    local unique, seen, paths = {}, {}, {}
    for _, row in ipairs(raw) do
      local key = row.path .. ":" .. row.position.line .. ":" .. row.position.character .. ":" .. (row.text or "")
      if not seen[key] then
        seen[key] = true unique[#unique + 1] = row
        if not paths[row.path] then paths[row.path] = true end
      end
    end
    local path_list = vim.tbl_keys(paths)
    authorize(path_list, function(allowed, failure)
      if not valid() then return end
      if failure then finish(failure) return end
      local permitted = {}
      for _, row in ipairs(unique) do
        if allowed[row.path] then permitted[#permitted + 1] = row end
      end
      if #permitted ~= #unique then warnings[#warnings + 1] = "Results omitted by read policy" end
      local offset, limit = arguments.offset or 0, arguments.limit or 20
      local last = math.min(#permitted, offset + limit)
      if last < #permitted then result.next_offset = last end
      local function enrich(index)
        if not valid() then return end
        if index > last then finish() return end
        local row = permitted[index]
        local function accept(lines)
          if not valid() then return end
          local line = lines[row.position.line + 1]
          if line then
            local success, byte_column = pcall(vim.str_byteindex, line, row.encoding, row.position.character, false)
            if success then
              local target = source.loaded(row.path)
              result.rows[#result.rows + 1] = { path = row.path, line = row.position.line + 1,
                column = vim.str_utfindex(line, "utf-32", byte_column, false) + 1,
                text = shorten(row.text or line, 160), modified = target and vim.bo[target].modified or false }
            else warnings[#warnings + 1] = "A server position could not be converted" end
          else warnings[#warnings + 1] = "A server position was outside the target file" end
          enrich(index + 1)
        end
        local target = source.loaded(row.path)
        if target then accept(vim.api.nvim_buf_get_lines(target, 0, -1, false))
        else source.read(row.path, 16 * 1024 * 1024, function(contents, read_error)
          if not valid() then return end
          if read_error then warnings[#warnings + 1] = "A target file could not be read" enrich(index + 1)
          else accept(source.rows(contents)) end
        end) end
      end
      enrich(offset + 1)
    end)
  end
  local function send(client, method, params, buffer, callback)
    outstanding = outstanding + 1
    attempted = attempted + 1
    clients[client.id] = client
    local accepted, id = client:request(method, params, function(failure, value)
      outstanding = outstanding - 1
      if not valid() then return end
      if failure then warnings[#warnings + 1] = shorten(failure.message or "Language server request failed", 160)
      else
        responses = responses + 1
        local success, callback_error = pcall(callback, value == vim.NIL and nil or value)
        if not success then finish("invalid_response: " .. tostring(callback_error)) return end
      end
      collect()
    end, buffer)
    if accepted then requests[#requests + 1] = { client = client, id = id }
    else outstanding = outstanding - 1 warnings[#warnings + 1] = "Language server rejected request" end
  end
  local function run(buffer, client)
    source.retain(buffer, buffers[buffer] == true)
    buffers[buffer] = true
    versions[buffer] = vim.api.nvim_buf_get_changedtick(buffer)
    result.source = string.format("%s version=%d", vim.bo[buffer].modified and "buffer (unsaved)" or "buffer", versions[buffer])
    if not vim.lsp.buf_is_attached(buffer, client.id) and not vim.lsp.buf_attach_client(buffer, client.id) then
      finish("unavailable: could not attach the running language server") return
    end
    local params = { textDocument = { uri = vim.uri_from_fname(arguments.path) } }
    if arguments.line then
      local line = vim.api.nvim_buf_get_lines(buffer, arguments.line - 1, arguments.line, false)[1]
      if not line then finish("invalid_position: line is outside the source") return end
      local success, byte_column = pcall(vim.str_byteindex, line, "utf-32", arguments.column - 1, false)
      if not success or arguments.column - 1 > vim.fn.strchars(line) then finish("invalid_position: column is outside the source") return end
      params.position = { line = arguments.line - 1, character = vim.str_utfindex(line, client.offset_encoding or "utf-16", byte_column, false) }
    end
    if arguments.operation == "references" then params.context = { includeDeclaration = false } end
    if arguments.operation == "diagnostics" and not client:supports_method(methods.diagnostics, buffer) then
      result.freshness = "unknown"
      local namespace = vim.lsp.diagnostic.get_namespace(client.id, false)
      for _, diagnostic in ipairs(vim.diagnostic.get(buffer, { namespace = namespace })) do
        if diagnostic.severity <= vim.diagnostic.severity.WARN then
          raw[#raw + 1] = { path = source.canonical(arguments.path), encoding = "utf-8",
            position = { line = diagnostic.lnum, character = diagnostic.col },
            text = (diagnostic.severity == vim.diagnostic.severity.ERROR and "error: " or "warning: ") .. diagnostic.message }
        end
      end
      collect() return
    end
    send(client, methods[arguments.operation], params, buffer, function(value)
      if arguments.operation == "hover" then result.hover = bounded_hover(hover_text(value and value.contents))
      elseif arguments.operation == "document_symbols" then symbols(value, client, arguments.path)
      elseif arguments.operation == "diagnostics" then
        result.freshness = "current"
        for _, diagnostic in ipairs(value and value.items or {}) do
          if (diagnostic.severity or 1) <= 2 then
            location({ uri = params.textDocument.uri, range = diagnostic.range }, client,
              (diagnostic.severity == 2 and "warning: " or "error: ") .. diagnostic.message)
          end
        end
      elseif arguments.operation == "calls" then
        for index, item in ipairs(value or {}) do
          if index > 50 then warnings[#warnings + 1] = "Call preparation limited to 50 symbols" break end
          local incoming = arguments.direction == "incoming"
          send(client, incoming and "callHierarchy/incomingCalls" or "callHierarchy/outgoingCalls", { item = item }, buffer, function(calls)
            for _, call in ipairs(calls or {}) do
              local target = incoming and call.from or call.to
              location(target, client, target.name)
            end
          end)
        end
      else
        if value and (value.uri or value.targetUri) then value = { value } end
        for _, target in ipairs(value or {}) do location(target, client) end
      end
    end)
    collect()
  end
  local function start()
    if arguments.operation == "status" then
      for _, client in ipairs(vim.lsp.get_clients()) do
        local filetype = arguments.path and vim.filetype.match({ filename = arguments.path })
        local config = client.config --[[@as vim.lsp.Config]]
        if not client:is_stopped() and source.belongs(client, workspace)
          and (not filetype or vim.tbl_contains(config.filetypes or {}, filetype)) then
          local supported = {}
          for operation, method in pairs(methods) do if client:supports_method(method) then supported[#supported + 1] = operation end end
          table.sort(supported)
          result.rows[#result.rows + 1] = { path = client.root_dir or client.config.root_dir or workspace, line = 1, column = 1,
            text = client.name .. " running: " .. table.concat(supported, ", ") }
        end
      end
      table.sort(result.rows, function(left, right) return left.path .. left.text < right.path .. right.text end)
      local offset, limit, rows = arguments.offset or 0, arguments.limit or 20, result.rows
      result.rows = {}
      for index = offset + 1, math.min(#rows, offset + limit) do result.rows[#result.rows + 1] = rows[index] end
      if offset + limit < #rows then result.next_offset = offset + limit end
      finish() return
    end
    if arguments.operation == "workspace_symbols" and not arguments.path then
      local selected = {}
      for _, client in ipairs(vim.lsp.get_clients()) do
        if not client:is_stopped() and source.belongs(client, workspace) and client:supports_method(methods.workspace_symbols) then selected[#selected + 1] = client end
      end
      if #selected == 0 then finish("unavailable: no project server supports workspace symbols") return end
      outstanding = outstanding + 1
      for _, client in ipairs(selected) do
        send(client, methods.workspace_symbols, { query = arguments.query }, nil, function(value) symbols(value, client) end)
      end
      outstanding = outstanding - 1 collect() return
    end
    local path = assert(arguments.path, "operation requires a source path")
    local buffer = source.loaded(path)
    local filetype = buffer and vim.bo[buffer].filetype or vim.filetype.match({ filename = path })
    if not filetype then finish("unavailable: could not detect source filetype") return end
    local method = arguments.operation == "diagnostics" and "textDocument/definition" or methods[arguments.operation]
    local client, failure = source.select_client(path, filetype, buffer, method, workspace)
    if not client then finish("unavailable: " .. failure) return end
    if arguments.operation == "workspace_symbols" then
      send(client, methods.workspace_symbols, { query = arguments.query }, buffer, function(value) symbols(value, client) end)
      collect() return
    end
    if buffer then run(buffer, client)
    else source.read(path, 1024 * 1024, function(contents, read_error)
      if not valid() then return end
      if read_error then finish(read_error) return end
      local opened, owned, open_error = source.open(path, contents, filetype)
      if not opened then finish(open_error) return end
      if owned then buffers[opened] = true end
      run(opened, client)
    end) end
  end
  cancel_startup = require("forge.lsp.startup").wait(workspace, arguments.path, function(startup_error)
    if finished then return end
    if startup_error then finish(startup_error) return end
    deadline:start(10000, 0, vim.schedule_wrap(function() finish("timeout: LSP operation exceeded ten seconds") end))
    local success, failure = pcall(start)
    if not success then finish(tostring(failure)) end
  end)
  return function() finish("cancelled: LSP request was revoked") end
end

return M
