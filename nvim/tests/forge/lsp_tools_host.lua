vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local language = vim.g.forge_test_language or "lua"
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
local function copy(from, to)
  vim.fn.mkdir(to, "p")
  for name, kind in vim.fs.dir(from) do
    if kind == "directory" then copy(from .. "/" .. name, to .. "/" .. name)
    else assert(vim.uv.fs_copyfile(from .. "/" .. name, to .. "/" .. name)) end
  end
end
copy(root .. "/nvim/tests/forge/fixtures/plan_navigation/" .. language, workspace)
vim.fn.mkdir(data, "p")
local function command(arguments, cwd)
  local result = vim.system(arguments, { cwd = cwd, text = true }):wait(30000)
  assert(result.code == 0, (result.stderr or "") .. (result.stdout or ""))
  return result.stdout
end
command({ "git", "init", "--quiet" }, workspace)
if language == "typescript" then
  local npm = vim.fs.joinpath(vim.fs.dirname(vim.fn.exepath("node")), "node_modules", "npm", "bin", "npm-cli.js")
  command({ "node", npm, "install", "--offline", "--ignore-scripts", "--no-audit", "--no-fund" }, workspace)
end
local binary = vim.g.forge_test_executable or require("forge.builder").binary_path()
if vim.fn.has("win32") == 1 then
  local copy_path = data .. "/forge.exe"
  assert(vim.uv.fs_copyfile(binary, copy_path))
  binary = copy_path
end
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local received, errors = {}, {}
local original_notify = vim.notify
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
  original_notify(message, level)
end
local unsubscribe = client.subscribe(function(event, payload)
  if event == "backend_event" and payload.kind == "assistant_message" then received[#received + 1] = payload.text end
end)
local function request(method, params)
  local deadline = vim.uv.now() + 20000
  repeat
    local complete, value, failure
    client.request(method, params or {}, function(result, error) value, failure, complete = result, error, true end)
    assert(vim.wait(20000, function() return complete end, 10), method .. " timed out: " .. table.concat(errors, "\n"))
    if not failure then return value end
    assert(method == "prompt.submit" and tostring(failure):find("no permits available", 1, true), method .. ": " .. tostring(failure))
    vim.wait(20, function() return false end, 5)
  until vim.uv.now() >= deadline
  error(method .. ": execution permit did not become available")
end

local function query(arguments)
  local marker = string.char(226, 151, 143)
  local prompt_marker = marker .. " LSP "
  local before = 0
  for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:find(prompt_marker, 1, true) then before = before + 1 end
  end
  request("prompt.submit", { text = "LSP " .. vim.json.encode(arguments) })
  local output
  assert(vim.wait(75000, function()
    local lines = vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)
    local boundary, count = 0, 0
    for index, line in ipairs(lines) do
      if line:find(prompt_marker, 1, true) then boundary, count = index, count + 1 end
    end
    if count <= before then return false end
    local response_start
    for index = boundary + 1, #lines do
      if lines[index]:sub(1, #marker) == marker then response_start = index break end
    end
    if not response_start then return false end
    for index = response_start + 1, #lines do
      local line = lines[index]
      if line ~= "" and line:sub(1, #marker) ~= marker then output = table.concat(lines, "\n", index) return true end
    end
    return false
  end, 10), "tool result did not arrive: " .. arguments.tool)
  local settled, pending = false, false
  assert(vim.wait(5000, function()
    if not pending then
      pending = true
      client.request("state.get", {}, function(snapshot)
        pending = false
        settled = snapshot and snapshot.status and snapshot.status.kind == "idle"
      end)
    end
    return settled
  end, 10), "model response did not settle")
  vim.wait(20, function() return false end, 5)
  return output
end

local spec = ({
  lua = { file = "main.lua", filetype = "lua", expression = "External.read", member = "read", cmd = { "lua-language-server", "--logpath=" .. data .. "/lua-lsp" },
    settings = { Lua = { workspace = { checkThirdParty = false }, telemetry = { enable = false }, runtime = { version = "LuaJIT" } } } },
  typescript = { file = "src/main.ts", filetype = "typescript", expression = "external.read", member = "read",
    cmd = { vim.g.forge_test_tsgo or vim.fn.exepath("tsgo"), "--lsp", "--stdio" } },
  rust = { file = "src/lib.rs", filetype = "rust", expression = "external.count", member = "count",
    cmd = { vim.g.forge_test_rust_analyzer or "rust-analyzer" },
    init_options = { cargo = { buildScripts = { enable = false }, extraArgs = { "--offline" } }, procMacro = { enable = false }, checkOnSave = false } },
})[language]
if language == "rust" and not vim.g.forge_test_rust_analyzer then spec.cmd = { vim.trim(command({ "rustup", "which", "rust-analyzer" }, root .. "/nvim/rust/forge")) } end
local identity, buffer
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "lsp-tools" }, selectable = true } } } })
  vim.lsp.config("fixture-" .. language, { cmd = spec.cmd, root_dir = workspace,
    filetypes = { spec.filetype }, settings = spec.settings, init_options = spec.init_options })
  vim.lsp.enable("fixture-" .. language)
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "Harness did not start")
  assert(#vim.lsp.get_clients({ name = "fixture-" .. language }) == 0, "opening Harness started a server")
  assert(request("state.get").lsp_tools == false)
  assert(query({ tool = "lsp_status" }):find("disabled", 1, true), "disabled tool call was admitted")
  request("session.configure", { lsp_tools = true })
  assert(request("state.get").lsp_tools == true)
  assert(query({ tool = "lsp_status" }):find("No project language servers", 1, true), "status started a server")
  if language == "typescript" then
    buffer = vim.fn.bufadd(workspace .. "/" .. spec.file)
    vim.fn.bufload(buffer)
    vim.bo[buffer].filetype = spec.filetype
    vim.api.nvim_exec_autocmds("FileType", { buffer = buffer, group = "nvim.lsp.enable" })
  else
    query({ tool = "lsp_symbols", query = spec.member })
  end
  assert(vim.wait(60000, function() local clients = vim.lsp.get_clients({ name = "fixture-" .. language }) return #clients == 1 and clients[1].initialized end, 10))
  local running = vim.lsp.get_clients({ name = "fixture-" .. language })
  assert(#running == 1 and running[1].initialized, "lazy startup did not initialize the configured editor server")
  identity = running[1].id
  assert(query({ tool = "lsp_status" }):find("fixture-" .. language, 1, true))
  local workspace_diagnostics = query({ tool = "lsp_diagnostics" })
  assert(workspace_diagnostics:find("coverage:", 1, true), workspace_diagnostics)
  assert(#vim.lsp.get_clients({ name = "fixture-" .. language }) == 1, "workspace query duplicated the editor server")
  buffer = vim.fn.bufadd(workspace .. "/" .. spec.file)
  vim.fn.bufload(buffer)
  vim.bo[buffer].filetype = spec.filetype
  vim.api.nvim_exec_autocmds("FileType", { buffer = buffer, group = "nvim.lsp.enable" })
  assert(vim.wait(2000, function() return vim.lsp.buf_is_attached(buffer, identity) end, 10), "editor did not reuse Harness server")
  assert(#vim.lsp.get_clients({ name = "fixture-" .. language }) == 1, "opening source created a second server")
  local line_number, column
  for index, line in ipairs(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)) do
    local start = line:find(spec.expression, 1, true)
    if start then
      line_number = index
      column = vim.str_utfindex(line, "utf-32", start - 1 + #spec.expression - #spec.member, false) + 1
      break
    end
  end
  assert(line_number, "fixture occurrence missing")
  local position = { path = spec.file, line = line_number, column = column }
  local definition
  local deadline = vim.uv.now() + 30000
  repeat
    definition = query(vim.tbl_extend("force", position, { tool = "lsp_definition" }))
  until definition:find("dependency", 1, true) or vim.uv.now() > deadline
  assert(definition:find("dependency", 1, true), definition)
  local by_spelling = query({ tool = "lsp_definition", path = spec.file, line = line_number, symbol = spec.member })
  if language == "rust" then
    local methods = query({ tool = "lsp_symbols", path = spec.file, query = "run", limit = 1000 })
    assert(methods:find("run", 1, true) and not methods:find("No matching", 1, true), methods)
    local missing = query({ tool = "lsp_context", path = "missing.rs", line = 1, column = 1 })
    assert(missing:find("path_not_found:", 1, true) and not missing:find("startup_failed:", 1, true), missing)
    local recovered = query({ tool = "lsp_context", path = spec.file, symbol = "run" })
    assert(recovered:find("run", 1, true) and not recovered:find("LSP error:", 1, true), recovered)
    assert(by_spelling:find("ambiguous_symbol", 1, true), by_spelling)
    local exact = query({ tool = "lsp_definition", path = spec.file, line = line_number, column = column, symbol = spec.member })
    assert(exact:find("dependency", 1, true), exact)
  else assert(by_spelling:find("dependency", 1, true), by_spelling) end
  local by_name = query({ tool = "lsp_context", path = spec.file, symbol = "run" })
  if language == "lua" then
    assert(by_name:find("ambiguous_symbol", 1, true), by_name)
    by_name = query({ tool = "lsp_context", path = spec.file, line = 4, symbol = "run" })
  end
  assert(by_name:find("run", 1, true) and not by_name:find("LSP error:", 1, true), by_name)
  local hover = query(vim.tbl_extend("force", position, { tool = "lsp_context" }))
  assert(not hover:find("LSP error:", 1, true), hover)
  local outline = query({ tool = "lsp_symbols", path = spec.file })
  assert(outline:find("run", 1, true), outline)
  local diagnostics = query({ tool = "lsp_diagnostics", path = spec.file })
  assert(diagnostics:find("freshness:", 1, true), diagnostics)
  local overview = query({ tool = "lsp_overview", path = ".", visibility = "all" })
  assert(overview:find("run", 1, true), overview)
  for _, operation in ipairs({ "references", "type_definition", "implementation", "calls", "workspace_symbols" }) do
    local method = ({ references = "textDocument/references", type_definition = "textDocument/typeDefinition",
      implementation = "textDocument/implementation", calls = "textDocument/prepareCallHierarchy", workspace_symbols = "workspace/symbol" })[operation]
    if vim.lsp.get_client_by_id(identity):supports_method(method, buffer) then
      local arguments = operation == "workspace_symbols" and { path = spec.file, query = "run" } or vim.deepcopy(position)
      arguments.tool = ({ references = "lsp_references", type_definition = "lsp_definition", implementation = "lsp_definition", calls = "lsp_calls", workspace_symbols = "lsp_symbols" })[operation]
      if operation == "calls" then arguments.direction = "incoming" end
      if operation == "type_definition" then arguments.kind = "type" end
      if operation == "implementation" then arguments.kind = "implementation" end
      local value = query(arguments)
      assert(not value:find("LSP error:", 1, true), value)
      if operation == "calls" then
        assert(not value:find("call sites:", 1, true) and not value:find(">", 1, true), value)
        assert(value:find(spec.file .. ":", 1, true) or value:find("dependency", 1, true), value)
        assert(value:find(spec.member, 1, true) and value:find("run", 1, true), value)
        print("lsp_calls tree:\n" .. value)
        local route = query({ tool = "lsp_paths", from = { path = spec.file, symbol = "run" },
          to = { path = spec.file, line = line_number, column = column, symbol = spec.member }, max_depth = 3, max_paths = 2 })
        assert(not route:find("LSP error:", 1, true) and route:find("run", 1, true) and route:find("dependency", 1, true), route)
        print("lsp_paths tree:\n" .. route)
      end
    end
  end
  if not vim.lsp.get_client_by_id(identity):supports_method("textDocument/prepareCallHierarchy", buffer) then
    local unavailable = query({ tool = "lsp_paths", from = position, to = position })
    assert(unavailable:find("unsupported:", 1, true) and not unavailable:find("No call path", 1, true), unavailable)
  end
  if language == "rust" then
    local large_source = string.rep("// Large source fixture with Unicode α\n", 11000)
    for index = 1, 70 do large_source = large_source .. "pub fn large_" .. index .. "() {}\n" end
    vim.fn.writefile(vim.split(large_source, "\n", { plain = true }), workspace .. "/large.rs")
    local large = query({ tool = "lsp_context", path = "large.rs", symbol = "large_70" })
    assert(large:find("large_70", 1, true) and not large:find("LSP error:", 1, true), large)
    local many = query({ tool = "lsp_symbols", path = "large.rs", limit = 1000000, token_budget = 8000 })
    assert(many:find("large_60", 1, true) and not many:find("invalid_argument:", 1, true), many)
    assert(many:find("next_offset=64", 1, true), many)
    vim.fn.writefile({ "struct First;", "impl First { fn insert(&self) {} }", "struct Second;", "impl Second { fn insert(&self) {} }" }, workspace .. "/ambiguity.rs")
    local ambiguous = query({ tool = "lsp_calls", path = "ambiguity.rs", symbol = "insert", direction = "incoming" })
    assert(ambiguous:find("ambiguous_symbol", 1, true) and ambiguous:find("First::insert", 1, true) and ambiguous:find("Second::insert", 1, true), ambiguous)
    local selected = query({ tool = "lsp_context", path = "ambiguity.rs", symbol = "Second::insert" })
    assert(not selected:find("LSP error:", 1, true) and selected:find("insert", 1, true), selected)
    local unique = query({ tool = "lsp_context", symbol = "run" })
    assert(not unique:find("LSP error:", 1, true) and unique:find("run", 1, true), unique)
  end
  vim.api.nvim_buf_set_lines(buffer, -1, -1, false, { language == "lua" and "-- unsaved editor change" or "// unsaved editor change" })
  local unsaved = query(vim.tbl_extend("force", position, { tool = "lsp_context" }))
  assert(unsaved:find("unsaved", 1, true), unsaved)
  request("session.configure", { lsp_tools = false })
  assert(query({ tool = "lsp_status" }):find("disabled", 1, true))
  assert(#vim.lsp.get_clients() == 1 and vim.lsp.get_client_by_id(identity), "tool changed the project server lifecycle")
  assert(#errors == 0, table.concat(errors, "\n"))
  print("lsp_tools_host: " .. language .. " passed")
end, debug.traceback)
if success and vim.g.forge_test_manual then
  _G.forge_lsp_test = { workspace = workspace, data = data, buffer = buffer, client = identity, query = query, request = request }
  print("LSP fixture ready for Terminal MCP inspection")
  return
end
unsubscribe()
if identity then vim.lsp.get_client_by_id(identity):stop(true) end
require("forge.views.harness.workspace").release(state)
client.stop()
vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf") vim.fn.delete(data, "rf")
assert(success, failure)
