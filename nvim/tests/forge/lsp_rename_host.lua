vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace .. "/src", "p")
vim.fn.mkdir(data, "p")
vim.fn.writefile({ '[package]', 'name="rename_fixture"', 'version="0.1.0"', 'edition="2021"' }, workspace .. "/Cargo.toml")
vim.fn.writefile({ "pub mod item;", "pub fn call() -> u32 { item::value() }" }, workspace .. "/src/lib.rs")
vim.fn.writefile({ "pub fn value() -> u32 { 7 }" }, workspace .. "/src/item.rs")
local git = vim.system({ "git", "init", "--quiet", workspace }, { text = true }):wait(10000)
assert(git.code == 0, git.stderr)
local binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
local original_notify = vim.notify
vim.notify = function(message, level) if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end end
local function request(method, params)
  local deadline = vim.uv.now() + 20000
  repeat
    local done, value, failure
    client.request({ method = method, params = params or {}, session_id = state.session and state.session.id },
      function(result, error) value, failure, done = result, error, true end)
    assert(vim.wait(20000, function() return done end, 10), method .. " timed out")
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
  assert(vim.wait(75000, function()
    if not state.transcript_buf then return false end
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n"):find(marker, 1, true) ~= nil
  end, 10), "tool did not finish: " .. table.concat(errors, "\n"))
  local idle, pending = false, false
  assert(vim.wait(10000, function()
    if not pending then pending = true
      client.request({ method = "session.state.get", session_id = state.session.id, params = {} }, function(snapshot)
        pending = false
        idle = snapshot and snapshot.status and snapshot.status.kind == "idle"
      end)
    end
    return idle
  end, 10), "provider did not settle")
  return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
end
local source = require("forge.lsp.source")
local startup = require("forge.lsp.startup")
local original_wait, original_clients, original_attached = startup.wait, vim.lsp.get_clients, vim.lsp.buf_is_attached
local mock = { id = 8901, name = "rename-fixture", initialized = true, root_dir = workspace,
  config = { root_dir = workspace, filetypes = { "rust" } }, offset_encoding = "utf-16",
  server_capabilities = { workspace = { fileOperations = {
    willRename = { filters = { { pattern = { glob = "**/*.rs" } } } },
    didRename = { filters = { { pattern = { glob = "**/*.rs" } } } },
  } } } }
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
    elseif method == "workspace/willRenameFiles" then callback(nil, nil)
    else callback({ message = "unsupported fixture request: " .. method }) end
  end)
  return true, 1
end
local identity, buffer
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { provider = "mock", providers = { mock = { selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(15000, function() return state.session ~= nil end, 10), "Harness startup: " .. table.concat(errors, "\n"))
  request("session.configure", { lsp_tools = true })
  if vim.g.forge_test_real_lsp then
    local executable = vim.trim(vim.system({ "rustup", "which", "rust-analyzer" }, { text = true }):wait().stdout)
    identity = vim.lsp.start({ name = "rename-rust", cmd = { executable }, root_dir = workspace, filetypes = { "rust" },
      capabilities = require("forge.lsp.rename").capabilities(),
      settings = { ["rust-analyzer"] = { cargo = { extraArgs = { "--offline" } }, checkOnSave = false } } })
    assert(vim.wait(60000, function() local running = vim.lsp.get_client_by_id(identity) return running and running.initialized end, 10), "rust-analyzer startup")
  else
    startup.wait = function(_, _, callback) callback() return function() end end
    vim.lsp.get_clients = function() return { mock } end
    vim.lsp.buf_is_attached = function() return true end
  end
  buffer = vim.fn.bufadd(workspace .. "/src/item.rs")
  vim.fn.bufload(buffer)
  vim.bo[buffer].filetype = "rust"
  if identity then
    vim.lsp.buf_attach_client(buffer, identity)
    local ready, pending = false, false
    assert(vim.wait(60000, function()
      if not pending and not ready then
        pending = true
        vim.lsp.get_client_by_id(identity):request("textDocument/documentSymbol", { textDocument = { uri = vim.uri_from_bufnr(buffer) } }, function(error, symbols)
          pending = false
          ready = not error and type(symbols) == "table" and #symbols > 0
        end, buffer)
      end
      return ready
    end, 50), "rust-analyzer did not index the fixture")
  end
  if vim.g.forge_test_manual then return end
  local result = query("lsp_rename_symbol", { path = "src/item.rs", line = 1, column = 8, new_name = "count" })
  assert(table.concat(vim.fn.readfile(workspace .. "/src/item.rs"), "\n"):find("fn count", 1, true), result)
  assert(table.concat(vim.fn.readfile(workspace .. "/src/lib.rs"), "\n"):find("item::count()", 1, true), result)
  assert(vim.api.nvim_buf_get_lines(buffer, 0, 1, false)[1]:find("fn count", 1, true), "editor did not refresh")
  request("session.configure", { access = vim.tbl_extend("force", state.session.access, { write_access = "off" }) })
  result = query("lsp_rename_symbol", { path = "src/item.rs", line = 1, column = 8, new_name = "blocked" })
  assert(table.concat(vim.fn.readfile(workspace .. "/src/item.rs"), "\n"):find("fn count", 1, true), result)
  assert(#(state.approval or {}) == 0, "write-off opened approval")
  request("session.configure", { access = vim.tbl_extend("force", state.session.access, { write_access = "workspace" }) })
  result = query("lsp_rename_file", { old_path = "src/item.rs", new_path = "src/renamed.rs" })
  assert(vim.uv.fs_stat(workspace .. "/src/renamed.rs") and not vim.uv.fs_stat(workspace .. "/src/item.rs"), result)
  assert(vim.api.nvim_buf_get_name(buffer):gsub("\\", "/"):find("/renamed.rs", 1, true), "buffer path did not follow file")
  if identity then
    assert(vim.wait(3000, function() return vim.lsp.buf_is_attached(buffer, identity) end, 10), "renamed buffer lost its LSP attachment")
    assert(table.concat(vim.fn.readfile(workspace .. "/src/lib.rs"), "\n"):find("renamed::count()", 1, true), "file rename did not update references")
  end
  print("lsp_rename_host: passed")
end, debug.traceback)
if success and vim.g.forge_test_manual then
  _G.forge_rename_test = { workspace = workspace, data = data, query = query, request = request, buffer = buffer }
  print("Rename fixture ready for Terminal MCP inspection: " .. workspace)
  return
end
startup.wait, vim.lsp.get_clients, vim.lsp.buf_is_attached = original_wait, original_clients, original_attached
if identity then vim.lsp.get_client_by_id(identity):stop(true) end
require("forge.lsp.rename").close()
require("forge.views.harness.workspace").release(state)
client.host_stop()
vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success, failure)
