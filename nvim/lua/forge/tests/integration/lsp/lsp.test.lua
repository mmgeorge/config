local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local host = dofile("nvim/lua/forge/tests/support/host.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
local state_fixture = dofile("nvim/lua/forge/tests/support/state.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["rename_tool_applies_edits_and_respects_write_access"] = function()
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
  startup.wait = function(_, _, callback) callback() return function() end end
  vim.lsp.get_clients = function() return { mock } end
  vim.lsp.get_client_by_id = function(id) return id == mock.id and mock or nil end
  local item_buffer = vim.fn.bufadd(workspace .. "/src/item.rs")
  vim.fn.bufload(item_buffer)
  vim.bo[item_buffer].filetype = "rust"
  local result = query("lsp_rename_symbol", { path = "src/item.rs", line = 1, column = 8, new_name = "count" })
  assert(table.concat(vim.fn.readfile(workspace .. "/src/item.rs"), "\n"):find("fn count", 1, true), result)
  assert(table.concat(vim.fn.readfile(workspace .. "/src/lib.rs"), "\n"):find("item::count()", 1, true), result)
  assert(buffer.line(item_buffer, 1):find("fn count", 1, true), "editor did not refresh")
  request("session.configure", { access = vim.tbl_extend("force", state.session.access, { write_access = "off" }) })
  result = query("lsp_rename_symbol", { path = "src/item.rs", line = 1, column = 8, new_name = "blocked" })
  assert(table.concat(vim.fn.readfile(workspace .. "/src/item.rs"), "\n"):find("fn count", 1, true), result)
  assert(#(state.approval or {}) == 0, "write-off opened approval")
end

return T
