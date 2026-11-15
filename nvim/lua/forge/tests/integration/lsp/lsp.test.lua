local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local host = dofile("nvim/lua/forge/tests/support/host.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
local state_fixture = dofile("nvim/lua/forge/tests/support/state.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local lua_selection_recording = vim.json.decode(table.concat(vim.fn.readfile("nvim/lua/forge/tests/fixtures/protocol/lsp-lua-selection.json"), "\n"))

local T = MiniTest.new_set({ hooks = isolation.hooks() })

-- requires-forge-host
T["semantic_tools_wait_for_rust_workspace_and_preserve_write_access"] = function()
  local root = vim.fn.getcwd()
  -- Release the workspace, stop the host, and leave the workspace directory before the
  -- temporary directories registered below are deleted.
  MiniTest.finally(function()
    pcall(function() require("forge.lsp.rename").close() end)
    pcall(function() require("forge.views.harness.workspace").release(require("forge.session").harness) end)
    pcall(host.stop)
    pcall(vim.api.nvim_set_current_dir, root)
  end)
  local workspace = temp.directory("workspace")
  vim.fn.mkdir(workspace .. "/src", "p")
  vim.fn.writefile({ '[package]', 'name="rename_fixture"', 'version="0.1.0"', 'edition="2021"' }, workspace .. "/Cargo.toml")
  vim.fn.writefile({ "pub mod item;", "pub fn call() -> u32 { item::value() }" }, workspace .. "/src/lib.rs")
  vim.fn.writefile({ "pub fn value() -> u32 { 7 }" }, workspace .. "/src/item.rs")
  git_repository.create({ path = workspace })
  local client = host.start().client
  local state = require("forge.session").harness
  local errors = notify.capture({ min_level = vim.log.levels.ERROR })
  local function error_text() return table.concat(errors:messages(), "\n") end
  local function request(method, params)
    local deadline = vim.uv.now() + 4000
    repeat
      local done, value, failure
      client.request({ method = method, params = params or {}, session_id = state.session and state.session.id },
        function(result, error) value, failure, done = result, error, true end)
      wait_until(function() return done end, method .. " did not answer", { timeout_ms = 4000 })
      if not failure then return value end
      local diagnostic = tostring(failure)
      local rejected_setup = method == "session.configure" and diagnostic:find("configuration authority changed", 1, true)
        or method == "session.prompt.submit" and (diagnostic:find("Harness cleanup is still running", 1, true)
          or diagnostic:find("no permits available", 1, true))
      assert(rejected_setup, method .. ": " .. diagnostic)
      vim.wait(25, function() return false end, 5)
    until vim.uv.now() >= deadline
    error(method .. " remained unavailable after setup")
  end
  local sequence = 0
  local function query(tool, arguments)
    sequence = sequence + 1
    local marker = "rename-completed-" .. sequence
    request("session.provider.mock_response", { operation = "script", script = { steps = {
      { kind = "tool_call", name = tool, arguments = arguments },
      { kind = "message", content = "response", text = marker },
    }, terminal = "completed" } })
    request("session.prompt.submit", { text = "Run " .. tool .. " " .. vim.json.encode(arguments) })
    wait_until(function()
      return state.transcript_buf and buffer.text(state.transcript_buf):find(marker, 1, true) ~= nil
    end, "tool did not finish", { timeout_ms = 4000, context = error_text })
    wait_until(function()
      local snapshot = state_fixture.get(1000)
      return snapshot.status and snapshot.status.kind == "idle"
    end, "provider did not settle", { timeout_ms = 4000 })
    return buffer.text(state.transcript_buf)
  end
  local startup = require("forge.lsp.startup")
  local selected_startup = startup.selected
  local mock = { id = 8901, name = "rename-fixture", initialized = true, root_dir = workspace,
    config = { root_dir = workspace, filetypes = { "rust" } }, offset_encoding = "utf-16",
    server_capabilities = {} }
  function mock:is_stopped() return false end
  function mock:supports_method() return true end
  function mock:cancel_request() end
  function mock:notify() end
  function mock:request(method, params, callback)
    vim.schedule(function()
      if method == "textDocument/prepareRename" then callback(nil, { defaultBehavior = true })
      elseif method == "textDocument/rename" then callback(nil, { changes = {
        [vim.uri_from_fname(workspace .. "/src/item.rs")] = { { range = { start = { line = 0, character = 7 }, ["end"] = { line = 0, character = 12 } }, newText = params.newName } },
        [vim.uri_from_fname(workspace .. "/src/lib.rs")] = { { range = { start = { line = 1, character = 29 }, ["end"] = { line = 1, character = 34 } }, newText = params.newName } },
      } })
      else callback({ message = "unsupported fixture request: " .. method }) end
    end)
    return true, 1
  end
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { selectable = true } } } })
  require("forge.views.harness").open()
  wait_until(function() return state.session ~= nil end, "Harness did not start",
    { timeout_ms = 4000, context = error_text })
  request("session.configure", { lsp_tools = true })
  vim.lsp.get_configs = function() return { { name = "rename-fixture", filetypes = { "rust" } } } end
  startup.selected = function(_, _, callback) callback(mock) return function() end end
  vim.lsp.get_clients = function() return { mock } end
  vim.lsp.get_client_by_id = function(id) return id == mock.id and mock or nil end
  vim.lsp.buf_is_attached = function() return true end
  stub(vim.lsp, "buf_attach_client", function() return true end)
  stub(vim.lsp, "buf_detach_client", function() return true end)
  local item_buffer = vim.fn.bufadd(workspace .. "/src/item.rs")
  vim.fn.bufload(item_buffer)
  vim.bo[item_buffer].filetype = "rust"
  local unrelated_calls = 0
  local unrelated = vim.deepcopy(mock)
  unrelated.id = 8902
  unrelated.name = "unrelated"
  unrelated.request = function() unrelated_calls = unrelated_calls + 1 error("unrelated LSP contacted") end
  vim.lsp.get_clients = function() return { mock, unrelated } end
  query("lsp_workspace_list", {})
  assert(unrelated_calls == 0, "inventory contacted an LSP")
  -- Replay the first-query race: initialized and progress-idle does not mean Rust is loaded.
  local loaded, symbol_requests = false, 0
  mock.server_info = { name = "rust-analyzer" }
  local rename_request = mock.request
  function mock:request(method, params, callback)
    if method ~= "workspace/symbol" then return rename_request(self, method, params, callback) end
    symbol_requests = symbol_requests + 1
    assert(loaded, "workspace/symbol reached rust-analyzer before its quiescent notification")
    vim.schedule(function() callback(nil, { { name = "value", kind = 12, location = {
      uri = vim.uri_from_fname(workspace .. "/src/item.rs"),
      range = { start = { line = 0, character = 7 }, ["end"] = { line = 0, character = 12 } },
    } } }) end)
    return true, symbol_requests
  end
  local original_timer = vim.uv.new_timer
  vim.uv.new_timer = function()
    local timer = assert(original_timer())
    return {
      start = function(_, delay, interval, callback)
        if interval == 20 then
          vim.schedule(function()
            assert(symbol_requests == 0, "provider queried before readiness")
            assert(not buffer.text(state.transcript_buf):find("rename-completed-2", 1, true), "provider resumed before readiness")
            loaded = true
            vim.lsp.handlers["experimental/serverStatus"](nil, { health = "warning", quiescent = true, message = "build scripts failed" }, { client_id = mock.id })
            callback()
          end)
          return 0
        end
        return timer:start(delay, interval, callback)
      end,
      stop = function() return timer:stop() end,
      close = function() return timer:close() end,
      is_closing = function() return timer:is_closing() end,
    }
  end
  startup.selected = selected_startup
  query("lsp_symbols", { workspace = { path = ".", language = "rust" }, query = "value" })
  vim.uv.new_timer = original_timer
  startup.selected = function(_, _, callback) callback(mock) return function() end end
  assert(loaded and symbol_requests == 1, "first symbol request did not wait for loaded workspace")
  vim.lsp.handlers["experimental/serverStatus"](nil, { health = "ok", quiescent = true }, { client_id = mock.id })
  local result = query("lsp_rename_symbol", { workspace = { path = ".", language = "rust" }, path = "src/item.rs", line = 1, column = 8, new_name = "count" })
  assert(table.concat(vim.fn.readfile(workspace .. "/src/item.rs"), "\n"):find("fn count", 1, true), result)
  assert(table.concat(vim.fn.readfile(workspace .. "/src/lib.rs"), "\n"):find("item::count()", 1, true), result)
  assert(buffer.line(item_buffer, 1):find("fn count", 1, true), "editor did not refresh")
  request("session.configure", { access = vim.tbl_extend("force", state.session.access, { write_access = "off" }) })
  result = query("lsp_rename_symbol", { workspace = { path = ".", language = "rust" }, path = "src/item.rs", line = 1, column = 8, new_name = "blocked" })
  assert(table.concat(vim.fn.readfile(workspace .. "/src/item.rs"), "\n"):find("fn count", 1, true), result)
  assert(#(state.approval or {}) == 0, "write-off opened approval")
  assert(unrelated_calls == 0, "rename contacted an unrelated LSP")
end

-- requires-forge-host
T["workspace_published_diagnostics_are_partial_even_when_empty"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local original_directory = vim.fn.getcwd()
  MiniTest.finally(function()
    pcall(function() require("forge.lsp.bridge").close() end)
    pcall(function() require("forge.views.harness.workspace").release(require("forge.session").harness) end)
    pcall(host.stop)
    pcall(vim.api.nvim_set_current_dir, original_directory)
  end)
  local workspace = temp.directory("published-diagnostics")
  vim.fn.writefile({ '[package]', 'name="published"', 'version="0.1.0"' }, workspace .. "/Cargo.toml")
  vim.fn.mkdir(workspace .. "/nested", "p")
  vim.fn.writefile({ '[package]', 'name="nested"', 'version="0.1.0"' }, workspace .. "/nested/Cargo.toml")
  local path = workspace .. "/lib.rs"
  vim.fn.writefile({ "fn sample() {}" }, path)
  local client = host.start().client
  local state = require("forge.session").harness
  local mock = { id = 8910, name = "publishing-fixture", initialized = true, root_dir = workspace,
    config = { root_dir = workspace, filetypes = { "rust" } }, offset_encoding = "utf-16",
    server_capabilities = { diagnosticProvider = { workspaceDiagnostics = false } },
    is_stopped = function() return false end, supports_method = function() return true end,
    request = function() error("snapshot query sent a server request") end }
  stub(require("forge.text.harper"), "attach", function() end)
  stub(vim.lsp, "get_client_by_id", function(identity) return identity == mock.id and mock or nil end)
  stub(vim.lsp, "get_clients", function() return { mock } end)
  stub(require("forge.lsp.startup"), "selected", function(_, _, receive) receive(mock) return function() end end)
  stub(vim.lsp, "get_configs", function() return { { name = mock.name, filetypes = { "rust" } } } end)
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { selectable = true } } } })
  require("forge.views.harness").open()
  wait_until(function() return state.session ~= nil end, "Harness did not start")
  local function request(method, params)
    local done, failure
    client.request({ method = method, params = params, session_id = state.session.id },
      function(_, message) done, failure = true, message end)
    wait_until(function() return done end, method .. " did not answer")
    MiniTest.expect.equality(failure, nil)
  end
  request("session.configure", { lsp_tools = true })
  local function query(marker)
    request("session.provider.mock_response", { operation = "script", script = { steps = {
      { kind = "tool_call", name = "lsp_diagnostics", arguments = { workspace = { path = "nested", language = "rust" } } },
      { kind = "message", content = "response", text = marker },
    }, terminal = "completed" } })
    request("session.prompt.submit", { text = "Read published diagnostics " .. marker })
    wait_until(function() return state.transcript_buf and buffer.text(state.transcript_buf):find(marker, 1, true) end,
      "diagnostics did not finish")
    wait_until(function() return state_fixture.get(1000).status.kind == "idle" end, "provider did not settle")
    wait_until(function()
      local transcript = state.presentation.transcript
      for index = 0, transcript.sequence:count() - 1 do
        local node = transcript.sequence:at(index).entry.metadata.node
        if node and node ~= vim.NIL and node.display == "heading" then
          state.presentation.set_section(node.id, true, false)
        end
      end
      return buffer.text(state.transcript_buf):find(marker == "empty-result-complete"
        and "No results returned." or "published workspace warning", 1, true)
    end, "published tool output did not expand", { context = function() return buffer.text(state.transcript_buf) end })
    return buffer.text(state.transcript_buf)
  end
  local publication = { uri = vim.uri_from_fname(path), diagnostics = {
    { range = { start = { line = 0, character = 3 }, ["end"] = { line = 0, character = 9 } },
      severity = 2, message = "published workspace warning" },
  } }
  vim.lsp.diagnostic.on_publish_diagnostics(nil, publication, { client_id = mock.id })
  local output = query("published-result-complete")
  MiniTest.expect.equality(output:find("coverage: partial", 1, true) ~= nil, true, { fail_reason = output })
  MiniTest.expect.equality(output:find("freshness: unknown", 1, true) ~= nil, true)
  MiniTest.expect.equality(output:find("published workspace warning", 1, true) ~= nil, true)
  publication.diagnostics = {}
  vim.lsp.diagnostic.on_publish_diagnostics(nil, publication, { client_id = mock.id })
  output = query("empty-result-complete")
  MiniTest.expect.equality(output:find("No published diagnostics in the", 1, true) ~= nil, true)
  MiniTest.expect.equality(output:find("workspace has no diagnostics", 1, true), nil)
  vim.diagnostic.reset(vim.lsp.diagnostic.get_namespace(mock.id, false))
end

-- requires-forge-host
T["lua_project_selection_reaches_only_the_configured_server"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local original_directory = vim.fn.getcwd()
  MiniTest.finally(function()
    pcall(function() require("forge.lsp.bridge").close() end)
    pcall(function() require("forge.views.harness.workspace").release(require("forge.session").harness) end)
    pcall(host.stop)
    pcall(vim.api.nvim_set_current_dir, original_directory)
  end)
  local workspace = temp.directory("lua-selection")
  vim.fn.writefile({ "{}" }, workspace .. "/.luarc.json")
  vim.fn.writefile({ "local function sample() end", "return sample" }, workspace .. "/init.lua")
  local client = host.start().client
  local state = require("forge.session").harness
  local calls = {}
  local mock = { id = 8920, name = "lua-fixture", initialized = true, root_dir = workspace,
    config = { root_dir = workspace, filetypes = { "lua" } }, offset_encoding = "utf-16",
    server_capabilities = {}, is_stopped = function() return false end,
    supports_method = function(_, method) return method == "workspace/symbol" end, cancel_request = function() end }
  function mock:request(method, _, receive)
    calls[#calls + 1] = method
    receive(nil, { { name = "sample", kind = 12, location = { uri = vim.uri_from_fname(workspace .. "/init.lua"),
      range = { start = { line = 0, character = 15 }, ["end"] = { line = 0, character = 21 } } } } })
    return true, #calls
  end
  stub(require("forge.text.harper"), "attach", function() end)
  stub(vim.lsp, "get_clients", function() return { mock } end)
  stub(vim.lsp, "get_client_by_id", function(identity) return identity == mock.id and mock or nil end)
  stub(vim.lsp, "get_configs", function() return { { name = mock.name, filetypes = { "lua" }, root_markers = { ".luarc.json" } } } end)
  stub(vim.lsp, "start", function() error("selection restarted an editor client") end)
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { selectable = true } } } })
  require("forge.views.harness").open()
  wait_until(function() return state.session ~= nil end, "Harness did not start")
  local function request(method, params)
    local done, failure
    client.request({ method = method, params = params, session_id = state.session.id },
      function(_, message) done, failure = true, message end)
    wait_until(function() return done end, method .. " did not answer")
    MiniTest.expect.equality(failure, nil)
  end
  request("session.configure", { lsp_tools = true })
  local symbol_arguments = vim.deepcopy(lua_selection_recording.arguments)
  symbol_arguments.operation = nil
  symbol_arguments.workspace.path = "."
  request("session.provider.mock_response", { operation = "script", script = { steps = {
    { kind = "tool_call", name = "lsp_workspace_list", arguments = {} },
    { kind = "tool_call", name = "lsp_symbols", arguments = symbol_arguments },
    { kind = "message", content = "response", text = "lua-selection-complete" },
  }, terminal = "completed" } })
  request("session.prompt.submit", { text = "List Lua workspaces and find sample" })
  wait_until(function() return state.transcript_buf and buffer.text(state.transcript_buf):find("lua-selection-complete", 1, true) end,
    "Lua selection did not finish")
  wait_until(function() return state_fixture.get(1000).status.kind == "idle" end, "provider did not settle")
  MiniTest.expect.equality(calls, { "workspace/symbol" })
  wait_until(function()
    local transcript = state.presentation.transcript
    for index = 0, transcript.sequence:count() - 1 do
      local node = transcript.sequence:at(index).entry.metadata.node
      if node and node ~= vim.NIL and node.display == "heading" then state.presentation.set_section(node.id, true, false) end
    end
    local contents = buffer.text(state.transcript_buf)
    return contents:find("local function sample", 1, true) ~= nil
  end, "Lua symbol output did not reach the transcript")
end

return T
