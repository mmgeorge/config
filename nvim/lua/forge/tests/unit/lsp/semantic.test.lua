-- Unit tests for `forge.lsp.semantic` (semantic.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["lsp_query"] = function()
  local startup = require("forge.lsp.startup")
  local query = require("forge.lsp.semantic")
  local directory = temp.directory("workspace")
  local path = directory .. "/source.lua"
  vim.fn.writefile({ "😀 value", "function sample() end" }, path)
  local buffer = vim.api.nvim_create_buf(false, false)
  vim.api.nvim_buf_set_name(buffer, path)
  buffer_support.set(buffer, { "😀 value", "function sample() end" })
  vim.bo[buffer].filetype = "lua"
  local handler, calls, pending_callback = nil, {}, nil
  local client = { initialized = true, id = 981, name = "fixture", config = { root_dir = directory, filetypes = { "lua" } }, offset_encoding = "utf-16" }
  function client:is_stopped() return false end
  function client:supports_method() return true end
  function client:cancel_request() end
  function client:request(method, params, callback)
    calls[#calls + 1] = { method = method, params = params }
    pending_callback = callback
    vim.schedule(function() if handler then handler(method, params, callback) end end)
    return true, #calls
  end
  vim.lsp.get_clients = function() return { client } end
  vim.lsp.buf_is_attached = function() return true end
  local range = { start = { line = 0, character = 3 }, ["end"] = { line = 0, character = 8 } }
  local function execute(arguments, authorize, budget, workspace)
    local done, result, failure = false
    local cancel = query.request(workspace or directory, arguments, authorize or function(paths, receive)
      local allowed = {}
      for _, target in ipairs(paths) do allowed[target] = true end
      receive(allowed)
    end, function(value, error) result, failure, done = value, error, true end, budget, { client = client })
    return function()
      wait_until(function() return done end, "LSP query did not finish", { timeout_ms = 1500 })
      return result, failure
    end, cancel
  end
  for _, operation in ipairs({ "definition", "type_definition", "implementation", "references" }) do
    handler = function(_, params, callback)
      eq(params.position.character, 3, { fail_reason = "input did not use server UTF-16 coordinates" })
      assert(params.textDocument.uri == vim.uri_from_bufnr(buffer), "request changed the attached document URI")
      callback(nil, { { targetUri = vim.uri_from_fname(path), targetSelectionRange = range },
        { uri = vim.uri_from_fname(path), range = range } })
    end
    local await = execute({ operation = operation, path = vim.fn.has("win32") == 1 and path:upper() or path, line = 1, column = 3 })
    local result, error = await()
    assert(not error, error)
    assert(#result.rows == 1 and result.rows[1].column == 3, vim.inspect(result))
    assert(result.source:find("unsaved", 1, true))
  end
  local retries = 0
  handler = function(_, _, callback)
    retries = retries + 1
    if retries == 1 then callback({ code = -32801, message = "content modified" })
    else callback(nil, { { uri = vim.uri_from_fname(path), range = range } }) end
  end
  local retry = execute({ operation = "definition", path = path, line = 1, column = 3 })
  assert(#assert(retry()).rows == 1 and retries == 2, "content-modified response was not retried")
  handler = function(_, _, callback) callback(nil, {}) end
  local await = execute({ operation = "references", path = path, line = 1, column = 3 })
  eq(#assert(await()).rows, 0)
  handler = function(_, _, callback) callback(nil, { { name = "sample", kind = 12, selectionRange = range,
    children = { { name = "value", kind = 13, selectionRange = range } } } }) end
  await = execute({ operation = "document_symbols", path = path, query = "sample.value" })
  local result = assert(await())
  assert(#result.rows == 1 and result.rows[1].text:find("sample.value", 1, true))
  handler = function(_, _, callback) callback(nil, { contents = { kind = "markdown", value = "function sample()" } }) end
  await = execute({ operation = "hover", path = path, line = 1, column = 3 })
  eq(assert(await()).hover, "function sample()")
  handler = function(method, _, callback)
    if method == "textDocument/prepareCallHierarchy" then callback(nil, { { name = "sample", uri = vim.uri_from_fname(path), range = range, selectionRange = range } })
    else callback(nil, { { from = { name = "caller", uri = vim.uri_from_fname(path), range = range } } }) end
  end
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "incoming" })
  result = assert(await())
  assert(#result.rows == 2 and result.rows[1].text == "sample" and result.rows[1].is_root)
  assert(result.rows[2].text == "caller" and not result.rows[2].is_root and result.rows[2].offset == 0)
  handler = function(method, _, callback)
    if method == "textDocument/prepareCallHierarchy" then
      callback(nil, { { name = "sample", uri = vim.uri_from_fname(path), range = range, selectionRange = range } })
    else callback(nil, {}) end
  end
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing" })
  result = assert(await())
  assert(#result.rows == 1 and result.rows[1].is_root, "empty hierarchy lost its prepared root")
  handler = function(method, _, callback)
    if method == "textDocument/prepareCallHierarchy" then
      callback(nil, { { name = "sample", uri = vim.uri_from_fname(path), range = range, selectionRange = range } })
    else callback(nil, {
      { to = { name = "first", uri = vim.uri_from_fname(path), range = range } },
      { to = { name = "second", uri = vim.uri_from_fname(path), range = range } },
    }) end
  end
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing", limit = 1 })
  result = assert(await())
  assert(#result.rows == 2 and result.rows[1].is_root and result.rows[2].text == "first" and result.next_offset == 1)
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing", limit = 1, offset = 1 })
  result = assert(await())
  assert(#result.rows == 2 and result.rows[1].is_root and result.rows[2].text == "second" and not result.next_offset)
  local before_budget = #calls
  local budget = { remaining = 1 }
  await = execute({ operation = "calls", path = path, line = 1, column = 3, direction = "outgoing" }, nil, budget)
  local limited, budget_error = await()
  assert(not limited and budget_error:find("budget_exhausted", 1, true), budget_error)
  assert(#calls == before_budget + 1 and budget.remaining == 0, "native request budget was exceeded")
  handler = function(_, _, callback) callback(nil, { items = { { range = range, severity = 1, message = "fixture error" } } }) end
  await = execute({ operation = "diagnostics", path = path })
  result = assert(await())
  assert(result.freshness == "current" and result.rows[1].text == "error: fixture error")
  handler = function(_, _, callback) callback(nil, { { uri = vim.uri_from_fname(path), range = range } }) end
  await = execute({ operation = "definition", path = path, line = 1, column = 3 }, function(_, receive) receive({}) end)
  result = assert(await())
  assert(#result.rows == 0 and #result.warnings == 1, "denied target was included")
  await = execute({ operation = "references", path = path, line = 1, column = 3, scope = "workspace" }, nil, nil, directory .. "/inner")
  result = assert(await())
  assert(#result.rows == 0 and not result.warnings, "scope filtering was reported as a permission denial")
  handler = function(_, _, callback)
    callback(nil, {
      { name = "sample", kind = 12, location = { uri = vim.uri_from_fname(directory .. "/other.lua"), range = range } },
      { name = "unrelated", kind = 12, location = { uri = vim.uri_from_fname(path), range = range } },
      { name = "sample", kind = 12, location = { uri = vim.uri_from_fname(path), range = range } },
    })
  end
  await = execute({ operation = "workspace_symbols", path = path, query = "sample", name_filter = "sample", search_path = path, limit = 1 })
  result = assert(await())
  assert(#result.rows == 1 and result.rows[1].path == require("forge.lsp.source").canonical(path), "symbol search escaped its file")
  assert(result.rows[1].text == "Function sample" and not result.next_offset, "filtering happened after pagination")
  client.server_info = { name = "rust-analyzer" }
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  stub(vim.lsp, "get_client_by_id", function() return client end)
  require("forge.lsp.server_status").setup()
  vim.lsp.handlers["experimental/serverStatus"](nil, { health = "ok", quiescent = true }, { client_id = client.id })
  for _, scope in ipairs({ "workspace", "all" }) do
    handler = function(_, arguments, callback)
      assert(arguments.query == (scope == "all" and "sample#*" or "sample#"), "Rust lookup excluded non-type symbols or dependencies")
      callback(nil, {})
    end
    await = execute({ operation = "workspace_symbols", path = path, query = "sample", scope = scope })
    assert(await())
  end
  client.server_info = nil
  handler = nil
  local cancel
  await, cancel = execute({ operation = "definition", path = path, line = 1, column = 3 })
  cancel()
  local cancelled, error = await()
  assert(not cancelled and error:find("cancelled", 1, true))
  pending_callback(nil, { uri = vim.uri_from_fname(path), range = range })
  await = execute({ operation = "definition", path = path, line = 1, column = 3 })
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, { "changed" })
  local stale
  stale, error = await()
  assert(not stale and error:find("source_changed", 1, true))
  assert(vim.api.nvim_buf_is_valid(buffer), "user buffer was deleted")
end

T["workspace_search_never_fans_out"] = function()
  local startup = require("forge.lsp.startup")
  local calls = {}
  local function server(identity)
    return { id = identity, name = "fixture", initialized = true, root_dir = "/project",
      config = { root_dir = "/project" }, is_stopped = function() return false end,
      supports_method = function() return true end, cancel_request = function() end,
      request = function(_, _, _, callback) calls[#calls + 1] = identity callback(nil, {}) return true, 1 end }
  end
  local selected, other = server(1), server(2)
  vim.lsp.get_clients = function() return { selected, other } end
  local done, failure
  require("forge.lsp.semantic").request("/project", { operation = "workspace_symbols", query = "test" },
    function(_, receive) receive({}) end, function(_, message) done, failure = true, message end,
    nil, { client = selected })
  wait_until(function() return done end, "search did not finish")
  eq(failure, nil)
  eq(calls, { 1 })
end

T["all_semantic_operations_wait_for_the_selected_client"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local await_ready, calls
  stub(require("forge.lsp.startup"), "ready", function(_, receive)
    await_ready = receive
    return function() end
  end)
  local directory = temp.directory("semantic-readiness")
  local path = directory .. "/source.rs"
  vim.fn.writefile({ "fn sample() {}" }, path)
  local client = { id = 300, initialized = true, name = "rust", root_dir = directory,
    config = { root_dir = directory, filetypes = { "rust" } }, offset_encoding = "utf-16",
    server_capabilities = { diagnosticProvider = { workspaceDiagnostics = true } },
    is_stopped = function() return false end, supports_method = function() return true end,
    cancel_request = function() end }
  function client:request(method, _, receive)
    calls = calls + 1
    receive(nil, method == "workspace/diagnostic" and { items = {} } or method == "textDocument/hover" and { contents = "ready" } or {})
    return true, calls
  end
  stub(vim.lsp, "buf_is_attached", function() return true end)
  for _, operation in ipairs({ "definition", "type_definition", "implementation", "references", "calls", "hover", "document_symbols", "workspace_symbols", "diagnostics" }) do
    calls = 0
    local done, failure
    require("forge.lsp.semantic").request(directory, {
      operation = operation, path = operation ~= "workspace_symbols" and operation ~= "diagnostics" and path or nil,
      line = 1, column = 1, query = "sample", direction = "outgoing",
    }, function(paths, receive)
      local allowed = {} for _, target in ipairs(paths) do allowed[target] = true end receive(allowed)
    end, function(_, message) done, failure = true, message end, nil, { client = client })
    eq(calls, 0)
    eq(done, nil)
    await_ready(client)
    wait_until(function() return done end, operation .. " did not settle")
    eq(failure, nil)
    eq(calls > 0, true)
  end
end

T["workspace_warning_is_returned_with_symbol_results"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local client = { id = 990, name = "rust_analyzer", initialized = true, root_dir = "/project",
    config = {}, is_stopped = function() return false end,
    supports_method = function() return true end, cancel_request = function() end,
    request = function(_, _, _, receive) receive(nil, {}) return true, 1 end }
  stub(vim.lsp, "get_client_by_id", function() return client end)
  require("forge.lsp.server_status").setup()
  vim.lsp.handlers["experimental/serverStatus"](nil,
    { health = "warning", quiescent = true, message = "build scripts failed" }, { client_id = client.id })
  local reply, failure
  require("forge.lsp.semantic").request("/project", {operation = "workspace_symbols", query = "Session"},
    function(_, receive) receive({}) end, function(value, message) reply, failure = value, message end,
    nil, {client = client})
  wait_until(function() return reply or failure end, "warning-state search did not finish")
  eq(failure, nil)
  eq(reply.warnings[1]:find("build scripts failed", 1, true) ~= nil, true)
end

T["workspace_published_diagnostics_respect_root_authorization_and_pagination"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local directory = temp.directory("published-root")
  vim.fn.mkdir(directory .. "/nested", "p")
  local path = directory .. "/source.rs"
  local denied = directory .. "/denied.rs"
  local outside = temp.file("outside.rs", { "outside" })
  vim.fn.writefile({ "😀 value", "second" }, path)
  vim.fn.writefile({ "private source" }, denied)
  local client = { id = 987, name = "publisher", initialized = true, root_dir = directory,
    config = { root_dir = directory }, offset_encoding = "utf-16",
    server_capabilities = { diagnosticProvider = { workspaceDiagnostics = false } },
    is_stopped = function() return false end,
    request = function() error("snapshot sent a server request") end }
  stub(vim.lsp, "get_client_by_id", function(identity) return identity == client.id and client or nil end)
  stub(require("forge.lsp.startup"), "ready", function(selected, receive) receive(selected) return function() end end)
  stub(require("forge.lsp.source"), "open", function() error("snapshot opened a source buffer") end)
  stub(vim, "system", function() error("snapshot started a process") end)
  local namespace = vim.lsp.diagnostic.get_namespace(client.id, false)
  local other_namespace = vim.lsp.diagnostic.get_namespace(988, false)
  MiniTest.finally(function() vim.diagnostic.reset(namespace) vim.diagnostic.reset(other_namespace) end)
  local function publish(target, items, selected_namespace)
    local buffer = vim.fn.bufadd(target)
    vim.diagnostic.set(selected_namespace or namespace, buffer, items)
    return buffer
  end
  local buffer = publish(path, {
    { lnum = 0, col = 5, severity = 2, message = "first warning" },
    { lnum = 1, col = 0, severity = 2, message = "second warning" },
    { lnum = 0, col = 0, severity = 1, message = "filtered error" },
  })
  publish(denied, { { lnum = 0, col = 0, severity = 2, message = "secret diagnostic" } })
  publish(outside, { { lnum = 0, col = 0, severity = 2, message = "outside diagnostic" } })
  publish(path, { { lnum = 0, col = 0, severity = 2, message = "other client diagnostic" } }, other_namespace)
  local source = require("forge.lsp.source")
  local original_read = source.read
  local reads, authorized = {}, {}
  stub(source, "read", function(target, receive)
    eq(authorized[target], true)
    reads[#reads + 1] = target
    original_read(target, receive)
  end)
  local function execute(offset)
    local result, failure
    require("forge.lsp.semantic").request(directory .. "/nested", {
      operation = "diagnostics", severity = "warning", limit = 1, offset = offset, scope = "all",
    }, function(paths, receive)
      local allowed = {}
      for _, target in ipairs(paths) do
        eq(target == source.canonical(outside), false)
        allowed[target] = target ~= source.canonical(denied)
        authorized[target] = allowed[target]
      end
      receive(allowed)
    end, function(reply, message) result, failure = reply, message end, nil, { client = client })
    wait_until(function() return result or failure end, "published query did not settle")
    eq(failure, nil)
    return result
  end
  local result = execute(0)
  eq(#result.rows, 1)
  eq(result.rows[1].column, 3)
  eq(result.rows[1].text, "publisher: warning: first warning")
  eq(result.next_offset, 1)
  eq(result.coverage[1].root, directory)
  eq(result.coverage[1].state, "partial")
  eq(result.freshness, "unknown")
  eq(result.warnings, { "Results omitted by read policy" })
  result = execute(1)
  eq(result.rows[1].text, "publisher: warning: second warning")
  eq(result.next_offset, nil)
  eq(reads, { source.canonical(path), source.canonical(path) })
  eq(vim.api.nvim_buf_is_loaded(buffer), false)
  vim.diagnostic.reset(namespace)
  result = execute(0)
  eq(result.rows, {})
  eq(result.coverage[1].state, "partial")
  eq(result.freshness, "unknown")
end

T["workspace_native_diagnostics_validate_reports"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local directory = temp.directory("native-diagnostics")
  local client = { id = 989, name = "native", initialized = true, root_dir = directory,
    config = { root_dir = directory }, is_stopped = function() return false end,
    cancel_request = function() end, server_capabilities = { diagnosticProvider = { workspaceDiagnostics = true } } }
  stub(require("forge.lsp.startup"), "ready", function(selected, receive) receive(selected) return function() end end)
  local report = { items = {} }
  function client:request(method, _, receive)
    eq(method, "workspace/diagnostic")
    receive(nil, report)
    return true, 1
  end
  local function execute()
    local result
    require("forge.lsp.semantic").request(directory, { operation = "diagnostics" },
      function(_, receive) receive({}) end, function(reply, failure) eq(failure, nil) result = reply end,
      nil, { client = client })
    return result
  end
  eq(execute().coverage[1].state, "complete")
  eq(execute().freshness, "current")
  report = { items = { { kind = "unchanged" } } }
  local result = execute()
  eq(result.coverage[1].state, "failed")
  eq(result.freshness, "unknown")
  eq(result.warnings[1]:find("invalid_response", 1, true) ~= nil, true)
end

T["workspace_snapshot_distinguishes_unsupported_storage_from_unavailable_client"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local client = { id = 991, name = "publisher", initialized = true, root_dir = "/project",
    config = {}, server_capabilities = {}, is_stopped = function() return false end }
  stub(vim.lsp, "get_client_by_id", function() return client end)
  stub(require("forge.lsp.startup"), "ready", function(selected, receive) receive(selected) return function() end end)
  stub(vim.diagnostic, "get", nil)
  local result, failure
  local function execute()
    require("forge.lsp.semantic").request("/project", { operation = "diagnostics" },
      function(_, receive) receive({}) end, function(reply, message) result, failure = reply, message end,
      nil, { client = client })
  end
  execute()
  eq(failure, nil)
  eq(result.coverage[1].state, "unsupported")
  eq(result.freshness, "unknown")
  client.is_stopped = function() return true end
  execute()
  eq(result, nil)
  eq(failure, "unavailable: selected LSP is not running")
end

return T
