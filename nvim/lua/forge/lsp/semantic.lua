local source = require("forge.lsp.source")
local diagnostics = require("forge.lsp.diagnostics")
local M = {}

local methods = {
  definition = "textDocument/definition", type_definition = "textDocument/typeDefinition",
  implementation = "textDocument/implementation", references = "textDocument/references",
  hover = "textDocument/hover", document_symbols = "textDocument/documentSymbol",
  workspace_symbols = "workspace/symbol", calls = "textDocument/prepareCallHierarchy",
  diagnostics = "textDocument/diagnostic", call_root = "textDocument/prepareCallHierarchy",
}

---@class ForgeLspSemanticArguments
---@field operation string
---@field path? string
---@field line? integer
---@field column? integer
---@field query? string
---@field search_path? string
---@field name_filter? string
---@field direction? string
---@field limit? integer
---@field offset? integer
---@field scope? string
---@field severity? string
---@field include_declaration? boolean
---@field startup_issues? table[]

---@class ForgeLspSemanticRow
---@field path string
---@field line integer
---@field column integer
---@field text string
---@field modified? boolean
---@field call_root? string
---@field is_root? boolean
---@field offset? integer

---@class ForgeLspSemanticResult
---@field rows ForgeLspSemanticRow[]
---@field source string
---@field next_offset? integer
---@field warnings? string[]
---@field hover? string
---@field freshness? string
---@field coverage? table[]

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

---@param workspace string
---@param arguments ForgeLspSemanticArguments
---@param authorize fun(paths: string[], receive: fun(allowed: table<string, boolean>?, failure: string?))
---@param receive fun(result: ForgeLspSemanticResult?, failure: string?)
---@return fun() cancel
---@return fun()? expire
function M.request(workspace, arguments, authorize, receive, budget)
  local finished, outstanding, attempted, responses, collecting = false, 0, 0, 0, false
  local requests, buffers, versions, clients, raw, warnings = {}, {}, {}, {}, {}, {}
  local result = { rows = {}, source = "workspace" }
  local workspace_query = not arguments.path and (arguments.operation == "workspace_symbols" or arguments.operation == "diagnostics")
  if workspace_query then result.coverage = {} end
  local deadline = assert(vim.uv.new_timer())
  local guard = assert(vim.uv.new_timer())
  local cancel_startup
  local send
  local function finish(failure)
    if finished then return end
    finished = true
    if cancel_startup then cancel_startup() end
    deadline:stop() deadline:close()
    guard:stop() guard:close()
    for _, request in ipairs(requests) do
      if request.cancel then request.cancel() else request.client:cancel_request(request.id) end
    end
    for buffer in pairs(buffers) do
      source.release(buffer)
    end
    result.warnings = #warnings > 0 and warnings or nil
    if workspace_query and arguments.operation == "diagnostics" then
      result.freshness = #result.coverage > 0 and "current" or "unknown"
      for _, coverage in ipairs(result.coverage) do
        if coverage.state ~= "complete" then result.freshness = "unknown" end
      end
    end
    receive(not failure and result or nil, failure)
  end
  local function valid()
    if finished then return false end
    for buffer, version in pairs(versions) do
      if not vim.api.nvim_buf_is_valid(buffer) or vim.api.nvim_buf_get_changedtick(buffer) ~= version then
        finish("source_changed: " .. vim.api.nvim_buf_get_name(buffer) .. " changed during the request.") return false
      end
    end
    for _, client in pairs(clients) do
      if client:is_stopped() and not workspace_query then finish("disconnected: " .. client.name .. " stopped during the request.") return false end
    end
    return true
  end
  guard:start(50, 50, vim.schedule_wrap(valid))
  local function location(value, client, label, call_root, is_root)
    local uri = value.targetUri or value.uri
    local range = value.targetSelectionRange or value.selectionRange or value.range or value.targetRange
    if type(uri) ~= "string" or uri:sub(1, 7) ~= "file://" or not range or not range.start then
      warnings[#warnings + 1] = "invalid_response: " .. client.name .. " returned an invalid " .. arguments.operation .. " location."
      return
    end
    local path = source.canonical(vim.uri_to_fname(uri))
    if workspace_query and arguments.scope ~= "all" and not source.inside(path, workspace) then return end
    if arguments.search_path and not source.inside(path, arguments.search_path) then return end
    raw[#raw + 1] = { path = path, position = range.start,
      encoding = client.offset_encoding or "utf-16", text = label, call_root = call_root, is_root = is_root }
  end
  local function symbols(values, client, path, parent)
    for _, symbol in ipairs(values or {}) do
      local owner = parent or symbol.containerName
      local name = owner and owner ~= "" and (owner .. "." .. symbol.name) or symbol.name
      local filter = arguments.name_filter or (arguments.operation ~= "workspace_symbols" and arguments.query or nil)
      if not filter or name:lower():gsub("::", "."):find(filter:lower():gsub("::", "."), 1, true) then
        if symbol.location and not symbol.location.range and client:supports_method("workspaceSymbol/resolve") then
          send(client, "workspaceSymbol/resolve", symbol, nil, function(resolved)
            if resolved and resolved.location and resolved.location.range then location(resolved.location, client, name) end
          end)
        else
          location(symbol.location or { uri = vim.uri_from_fname(path), range = symbol.selectionRange or symbol.range }, client,
            (vim.lsp.protocol.SymbolKind[symbol.kind] or "Symbol") .. " " .. name)
        end
      end
      if symbol.children then symbols(symbol.children, client, path, name) end
    end
  end
  local function symbol_query(client)
    local query = arguments.query:match("([^:.]+)$") or arguments.query
    if client.name == "rust_analyzer" or client.server_info and client.server_info.name == "rust-analyzer" then
      query = query .. "#" .. (arguments.scope == "all" and "*" or "")
    end
    return { query = query }
  end
  local function collect()
    if outstanding ~= 0 or collecting or not valid() then return end
    if not workspace_query and attempted > 0 and responses == 0 then finish(table.concat(warnings, "\n")) return end
    if not workspace_query and #raw == 0 and #warnings > 0 then finish(table.concat(warnings, "\n")) return end
    collecting = true
    if result.hover then finish() return end
    table.sort(raw, function(left, right)
      if arguments.operation == "calls" and left.call_root ~= right.call_root then return left.call_root < right.call_root end
      if left.path ~= right.path then return left.path < right.path end
      if left.position.line ~= right.position.line then return left.position.line < right.position.line end
      if left.position.character ~= right.position.character then return left.position.character < right.position.character end
      return (left.text or "") < (right.text or "")
    end)
    local unique, seen, paths = {}, {}, {}
    for _, row in ipairs(raw) do
      local key = (row.call_root or "") .. tostring(row.is_root or false) .. row.path .. ":" .. row.position.line .. ":" .. row.position.character .. ":" .. (row.text or "")
      if not seen[key] then
        seen[key] = true unique[#unique + 1] = row
        if not paths[row.path] then paths[row.path] = true end
      end
    end
    local path_list = vim.tbl_keys(paths)
    authorize(path_list, function(allowed, failure)
      if not valid() then return end
      if failure then finish(failure) return end
      local permitted, root_allowed, denied = {}, {}, false
      for _, row in ipairs(unique) do
        if row.is_root then root_allowed[row.call_root] = allowed[row.path] end
      end
      for _, row in ipairs(unique) do
        if not allowed[row.path] then denied = true end
        if allowed[row.path] and (row.is_root or arguments.scope ~= "workspace" or source.inside(row.path, workspace)) and
          (not row.call_root or root_allowed[row.call_root]) then permitted[#permitted + 1] = row end
      end
      if denied then warnings[#warnings + 1] = "Results omitted by read policy" end
      local root_count = 0
      for _, row in ipairs(permitted) do if row.is_root then root_count = root_count + 1 end end
      local offset, limit = arguments.offset or 0, math.min(arguments.limit or 20, 64, math.max(1, 100 - root_count))
      local selected, roots, child_count = {}, {}, 0
      for _, row in ipairs(permitted) do
        if row.is_root then roots[#roots + 1] = row
        else
          if child_count >= offset and child_count < offset + limit then
            row.offset = child_count
            selected[#selected + 1] = row
          end
          child_count = child_count + 1
        end
      end
      if offset + limit < child_count then result.next_offset = offset + limit end
      for _, row in ipairs(roots) do selected[#selected + 1] = row end
      if arguments.operation == "calls" then
        table.sort(selected, function(left, right)
          if left.call_root ~= right.call_root then return left.call_root < right.call_root end
          if left.is_root ~= right.is_root then return left.is_root == true end
          return left.offset < right.offset
        end)
      end
      permitted = selected
      local last = #permitted
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
                text = row.text or line, call_root = row.call_root, is_root = row.is_root, offset = row.offset, modified = target and vim.bo[target].modified or false }
            else warnings[#warnings + 1] = "invalid_response: Invalid character offset in " .. row.path end
          else warnings[#warnings + 1] = "invalid_response: Position outside " .. row.path end
          enrich(index + 1)
        end
        local target = source.loaded(row.path)
        if target and versions[target] then accept(vim.api.nvim_buf_get_lines(target, 0, -1, false))
        elseif target then source.refresh(target, function(refresh_error)
          if not valid() then return end
          if refresh_error then finish(refresh_error) return end
          versions[target] = vim.api.nvim_buf_get_changedtick(target)
          accept(vim.api.nvim_buf_get_lines(target, 0, -1, false))
        end)
        else source.read(row.path, function(contents, read_error)
          if not valid() then return end
          if read_error then warnings[#warnings + 1] = read_error enrich(index + 1)
          else accept(source.rows(contents)) end
        end) end
      end
      enrich(1)
    end)
  end
  send = function(client, method, params, buffer, callback, retries, coverage)
    if finished then return end
    if budget then
      if budget.remaining <= 0 then finish("budget_exhausted: native LSP request limit reached") return end
      budget.remaining = budget.remaining - 1
    end
    outstanding = outstanding + 1
    attempted = attempted + 1
    clients[client.id] = client
    local received = false
    local function response(failure, value)
      if received or finished then return end
      received = true
      outstanding = outstanding - 1
      if not valid() then return end
      if failure and failure.code == -32801 and (retries or 0) < 2 then
        outstanding = outstanding + 1
        local pending = true
        requests[#requests + 1] = { cancel = function() pending = false end, expire = function()
          if not pending then return end
          pending = false
          outstanding = outstanding - 1
          local message = "timeout: " .. client.name .. " retry exceeded the operation deadline."
          warnings[#warnings + 1] = message
          if coverage then coverage.state, coverage.message = "failed", message end
          collect()
        end }
        vim.defer_fn(function()
          if not pending then return end
          pending = false
          outstanding = outstanding - 1
          if valid() then send(client, method, params, buffer, callback, (retries or 0) + 1, coverage) end
        end, 100)
        return
      end
      if failure then
        local message = "server_error: " .. client.name .. " — " .. shorten(failure.message or "Request failed.", 160)
        warnings[#warnings + 1] = message
        if coverage then coverage.state, coverage.message = "failed", message end
      else
        responses = responses + 1
        local success, callback_error = pcall(callback, value == vim.NIL and nil or value)
        if not success then
          local message = "invalid_response: " .. client.name .. " — " .. tostring(callback_error)
          if not coverage then finish(message) return end
          coverage.state, coverage.message = "failed", message
          warnings[#warnings + 1] = message
        elseif coverage then coverage.state = "complete" end
      end
      collect()
    end
    local accepted, id, cancel
    if method == methods.diagnostics then
      accepted, cancel = diagnostics.request(client, buffer, params, response)
    else accepted, id = client:request(method, params, response, buffer) end
    if accepted then requests[#requests + 1] = { client = client, id = id, cancel = cancel,
      expire = function() response({ message = "Request timed out." }) end }
    else response({ message = "Request rejected." }) end
  end
  local function run(buffer, client)
    source.retain(buffer, buffers[buffer] == true)
    buffers[buffer] = true
    versions[buffer] = vim.api.nvim_buf_get_changedtick(buffer)
    result.source = string.format("%s version=%d", vim.bo[buffer].modified and "buffer (unsaved)" or "buffer", versions[buffer])
    if not vim.lsp.buf_is_attached(buffer, client.id) and not vim.lsp.buf_attach_client(buffer, client.id) then
      finish("unavailable: Could not attach " .. client.name .. " to " .. arguments.path) return
    end
    local params = { textDocument = { uri = vim.uri_from_bufnr(buffer) } }
    if arguments.line then
      local line = vim.api.nvim_buf_get_lines(buffer, arguments.line - 1, arguments.line, false)[1]
      if not line then finish("invalid_position: " .. arguments.path .. ":" .. arguments.line .. " is outside the source.") return end
      local success, byte_column = pcall(vim.str_byteindex, line, "utf-32", arguments.column - 1, false)
      if not success or arguments.column - 1 > vim.fn.strchars(line) then finish("invalid_position: " .. arguments.path .. ":" .. arguments.line .. ":" .. arguments.column .. " is outside the source.") return end
      params.position = { line = arguments.line - 1, character = vim.str_utfindex(line, client.offset_encoding or "utf-16", byte_column, false) }
    end
    if arguments.operation == "references" then params.context = { includeDeclaration = arguments.include_declaration == true } end
    if arguments.operation == "diagnostics" and not client:supports_method(methods.diagnostics, buffer) then
      result.freshness = "unknown"
      local namespace = vim.lsp.diagnostic.get_namespace(client.id, false)
      for _, diagnostic in ipairs(vim.diagnostic.get(buffer, { namespace = namespace })) do
        if not arguments.severity or arguments.severity == "all" or ({ "error", "warning", "information", "hint" })[diagnostic.severity] == arguments.severity then
          raw[#raw + 1] = { path = source.canonical(arguments.path), encoding = "utf-8",
            position = { line = diagnostic.lnum, character = diagnostic.col },
            text = (({ "error", "warning", "information", "hint" })[diagnostic.severity] or "error") .. (diagnostic.code and "[" .. tostring(diagnostic.code) .. "]" or "") .. ": " .. diagnostic.message }
        end
      end
      collect() return
    end
    send(client, methods[arguments.operation], params, buffer, function(value)
      if arguments.operation == "hover" then result.hover = hover_text(value and value.contents)
      elseif arguments.operation == "document_symbols" then symbols(value, client, arguments.path)
      elseif arguments.operation == "diagnostics" then
        result.freshness = "current"
        for _, diagnostic in ipairs(value and value.items or {}) do
          if not arguments.severity or arguments.severity == "all" or ({ "error", "warning", "information", "hint" })[diagnostic.severity or 1] == arguments.severity then
            location({ uri = params.textDocument.uri, range = diagnostic.range }, client,
              (({ "error", "warning", "information", "hint" })[diagnostic.severity or 1] or "error") .. (diagnostic.code and "[" .. tostring(diagnostic.code) .. "]" or "") .. ": " .. diagnostic.message)
          end
        end
      elseif arguments.operation == "call_root" then
        for _, item in ipairs(value or {}) do location(item, client, item.name) end
      elseif arguments.operation == "calls" then
        for index, item in ipairs(value or {}) do
          if index > 50 then warnings[#warnings + 1] = "Call preparation limited to 50 symbols" break end
          local incoming = arguments.direction == "incoming"
          local call_root = item.uri .. ":" .. item.selectionRange.start.line .. ":" .. item.selectionRange.start.character
          location(item, client, item.name, call_root, true)
          send(client, incoming and "callHierarchy/incomingCalls" or "callHierarchy/outgoingCalls", { item = item }, buffer, function(calls)
            for _, call in ipairs(calls or {}) do
              local target = incoming and call.from or call.to
              location(target, client, target.name, call_root, false)
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
      for _, client in ipairs(vim.lsp.get_clients({ _uninitialized = true })) do
        local filetype = arguments.path and vim.filetype.match({ filename = arguments.path })
        local config = client.config --[[@as vim.lsp.Config]]
        if not client:is_stopped() and source.belongs(client, workspace)
          and (not filetype or vim.tbl_contains(config.filetypes or {}, filetype)) then
          local supported = {}
          for operation, method in pairs(methods) do if client:supports_method(method) then supported[#supported + 1] = operation end end
          table.sort(supported)
          result.rows[#result.rows + 1] = { path = client.root_dir or client.config.root_dir or workspace, line = 1, column = 1,
            text = client.name .. (not client.initialized and "  starting" or client.progress and next(client.progress.pending or {}) ~= nil and "  indexing" or "  ready") .. "\n  " .. table.concat(supported, ", ") }
        end
      end
      table.sort(result.rows, function(left, right) return left.path .. left.text < right.path .. right.text end)
      local offset, limit, rows = arguments.offset or 0, arguments.limit or 20, result.rows
      result.rows = {}
      for index = offset + 1, math.min(#rows, offset + limit) do result.rows[#result.rows + 1] = rows[index] end
      if offset + limit < #rows then result.next_offset = offset + limit end
      finish() return
    end
    if workspace_query then
      local selected = {}
      for _, client in ipairs(vim.lsp.get_clients()) do
        local root = client.root_dir or client.config.root_dir
        root = type(root) == "string" and root or workspace
        local startup_failed = false
        for _, coverage in ipairs(result.coverage) do
          if coverage.name == client.name and source.canonical(coverage.root) == source.canonical(root) then startup_failed = true break end
        end
        if not startup_failed and not client:is_stopped() and source.belongs(client, arguments.search_path or workspace) then
          local coverage = { name = client.name, root = type(root) == "string" and root or workspace, state = "pending" }
          result.coverage[#result.coverage + 1] = coverage
          local provider = client.server_capabilities and client.server_capabilities.diagnosticProvider
          local supported = arguments.operation == "workspace_symbols" and client:supports_method(methods.workspace_symbols)
            or arguments.operation == "diagnostics" and type(provider) == "table" and provider.workspaceDiagnostics == true
          if supported then selected[#selected + 1] = { client = client, coverage = coverage, provider = provider }
          else
            coverage.state, coverage.message = "unsupported", "Server does not support workspace " .. (arguments.operation == "diagnostics" and "diagnostics." or "symbols.")
            warnings[#warnings + 1] = client.name .. ": " .. coverage.message
          end
        end
      end
      if #selected == 0 then
        if #result.coverage == 0 then finish("unavailable: No running project language server.")
        else collect() end
        return
      end
      outstanding = outstanding + 1
      for _, selected in ipairs(selected) do
        local client = selected.client
        if arguments.operation == "workspace_symbols" then
          send(client, methods.workspace_symbols, symbol_query(client), nil, function(value) symbols(value, client) end, nil, selected.coverage)
        else
          send(client, "workspace/diagnostic", { identifier = selected.provider.identifier, previousResultIds = {} }, nil, function(value)
            assert(type(value) == "table" and type(value.items) == "table", "Missing workspace diagnostic report")
            for _, report in ipairs(value.items) do
              assert(report.kind == "full" and type(report.items) == "table", "Workspace report omitted diagnostics without a previous result")
              for _, diagnostic in ipairs(report.items) do
                local severity = ({ "error", "warning", "information", "hint" })[diagnostic.severity or 1] or "error"
                if not arguments.severity or arguments.severity == "all" or arguments.severity == severity then
                  location({ uri = report.uri, range = diagnostic.range }, client,
                    client.name .. ": " .. severity .. (diagnostic.code and "[" .. tostring(diagnostic.code) .. "]" or "") .. ": " .. diagnostic.message)
                end
              end
            end
          end, nil, selected.coverage)
        end
      end
      outstanding = outstanding - 1 collect() return
    end
    local path = assert(arguments.path, "operation requires a source path")
    local buffer = source.loaded(path)
    local filetype = buffer and vim.bo[buffer].filetype or vim.filetype.match({ filename = path })
    if not filetype then finish("unsupported: Cannot determine the language of " .. path) return end
    local method = arguments.operation ~= "diagnostics" and methods[arguments.operation] or nil
    local client, failure = source.select_client(path, filetype, buffer, method, workspace)
    if not client then finish(failure) return end
    if arguments.operation == "workspace_symbols" then
      send(client, methods.workspace_symbols, symbol_query(client), buffer, function(value) symbols(value, client) end)
      collect() return
    end
    if buffer then source.refresh(buffer, function(refresh_error)
      if not valid() then return end
      if refresh_error then finish(refresh_error) else run(buffer, client) end
    end)
    else source.read(path, function(contents, read_error)
      if not valid() then return end
      if read_error then finish(read_error) return end
      local opened, owned, open_error = source.open(path, contents, filetype)
      if not opened then finish(open_error) return end
      if owned then buffers[opened] = true end
      run(opened, client)
    end) end
  end
  local function expire()
    if finished then return end
    if not workspace_query or collecting then finish("timeout: " .. arguments.operation .. " lookup exceeded 10 seconds.") return end
    for _, request in ipairs(requests) do if request.expire then request.expire() end end
    if not finished and outstanding == 0 then collect() end
  end
  if arguments.operation == "status" then start() return function() end end
  local function ready(startup_error, issues)
    if finished then return end
    if startup_error then finish(startup_error) return end
    for _, issue in ipairs(issues or {}) do
      warnings[#warnings + 1] = issue.message
      if workspace_query then result.coverage[#result.coverage + 1] = issue end
    end
    deadline:start(10000, 0, vim.schedule_wrap(expire))
    local success, failure = pcall(start)
    if not success then finish(tostring(failure)) end
  end
  if workspace_query and arguments.startup_issues then ready(nil, arguments.startup_issues)
  else cancel_startup = require("forge.lsp.startup").wait(workspace, arguments.path or arguments.search_path, ready) end
  return function() finish("cancelled: LSP request cancelled.") end, expire
end

return M
