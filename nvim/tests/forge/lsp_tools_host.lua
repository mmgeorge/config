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
  local complete, value, failure
  client.request(method, params or {}, function(result, error) value, failure, complete = result, error, true end)
  assert(vim.wait(20000, function() return complete end, 10), method .. " timed out: " .. table.concat(errors, "\n"))
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local function query(arguments)
  local prompt = "LSP " .. vim.json.encode(arguments)
  request("prompt.submit", { text = prompt })
  local result
  assert(vim.wait(2000, function()
    local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    local boundary, position = nil, 1
    while true do
      local found = text:find("LSP {", position, true)
      if not found then break end
      boundary, position = found + 5, found + 5
    end
    if not boundary then return false end
    text = text:sub(boundary)
    local last = 0
    for _, needle in ipairs({ arguments.operation .. ":", "LSP error:" }) do
      local position = 1
      while true do
        local found = text:find(needle, position, true)
        if not found then break end
        last, position = math.max(last, found), found + #needle
      end
    end
    if last > 0 then result = text:sub(last) end
    return result ~= nil
  end, 10), "tool result did not reach the transcript: " .. arguments.operation)
  return result
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
  assert(request("state.get").lsp_tools == false)
  assert(query({ operation = "status" }):find("disabled", 1, true), "disabled tool call was admitted")
  request("session.configure", { lsp_tools = true })
  assert(request("state.get").lsp_tools == true)
  assert(query({ operation = "status" }):find("fixture-" .. language, 1, true))
  local running = vim.lsp.get_clients({ name = "fixture-" .. language })
  assert(#running == 1 and running[1].initialized, "Harness did not start the configured editor server")
  identity = running[1].id
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
    definition = query(vim.tbl_extend("force", position, { operation = "definition" }))
  until definition:find("dependency", 1, true) or vim.uv.now() > deadline
  assert(definition:find("dependency", 1, true), definition)
  local hover = query(vim.tbl_extend("force", position, { operation = "hover" }))
  assert(not hover:find("LSP error:", 1, true), hover)
  local outline = query({ operation = "document_symbols", path = spec.file })
  assert(outline:find("run", 1, true), outline)
  local diagnostics = query({ operation = "diagnostics", path = spec.file })
  assert(diagnostics:find("freshness=", 1, true), diagnostics)
  for _, operation in ipairs({ "references", "type_definition", "implementation", "calls", "workspace_symbols" }) do
    local method = ({ references = "textDocument/references", type_definition = "textDocument/typeDefinition",
      implementation = "textDocument/implementation", calls = "textDocument/prepareCallHierarchy", workspace_symbols = "workspace/symbol" })[operation]
    if vim.lsp.get_client_by_id(identity):supports_method(method, buffer) then
      local arguments = operation == "workspace_symbols" and { path = spec.file, query = "run" } or vim.deepcopy(position)
      arguments.operation = operation
      if operation == "calls" then arguments.direction = "incoming" end
      local value = query(arguments)
      assert(not value:find("LSP error:", 1, true), value)
    end
  end
  vim.api.nvim_buf_set_lines(buffer, -1, -1, false, { language == "lua" and "-- unsaved editor change" or "// unsaved editor change" })
  local unsaved = query(vim.tbl_extend("force", position, { operation = "hover" }))
  assert(unsaved:find("unsaved", 1, true), unsaved)
  request("session.configure", { lsp_tools = false })
  assert(query({ operation = "status" }):find("disabled", 1, true))
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
