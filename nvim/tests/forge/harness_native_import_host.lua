vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local executable = vim.g.forge_native_import_binary or require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the release Forge host first")
local original_stdpath, original_input = vim.fn.stdpath, vim.ui.input
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local harness = require("forge.views.harness")
local controller = require("forge.views.harness.controller")
local session = require("forge.session")
local picker = require("forge.views.picker")
---@type string[]
local errors, warnings = {}, {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
require("forge.infra.notifications").warn = function(message) warnings[#warnings + 1] = message end
controller.resolve_runtime_model = function() end
local source = data .. "/native.json"
local function write_source(command)
  vim.fn.writefile({ vim.json.encode({ mcpServers = {
    portable = { command = command, env = { TOKEN = "IMPORT_LITERAL$VALUE" } },
    restricted = { command = "node", disabled_tools = { "private" } },
  }, apiKey = "IMPORT_AUTHENTICATION_VALUE" }) }, source)
end
write_source("node")
vim.ui.input = function(_, callback) callback(source) end
local function request(method, params)
  local result, failure, completed
  client.request(method, params or {}, function(value, request_error)
    result, failure, completed = value, request_error, true
  end)
  assert(vim.wait(10000, function() return completed end, 10), method .. " did not complete")
  assert(not failure, failure)
  return result
end
local function confirm(identity)
  local instance = assert(picker._state_for_test())
  local page = instance.spec.page_list[instance.state.page_index]
  for index, option in ipairs(page.option_list) do
    if identity == option.id or identity == option.value or identity == option.label then
      instance.state.selected_index_by_page[page.id] = index
      local mapping = vim.fn.maparg("<CR>", "n", false, true)
      assert(mapping.callback, "picker select mapping absent")
      mapping.callback()
      return option
    end
  end
  error("picker option absent: " .. identity)
end
local function preview_ready()
  local instance = picker._state_for_test()
  return instance and instance.spec.page_list[1].id == "preview"
    and #instance.spec.page_list[1].option_list > 0
    and instance.spec.page_list[1].option_list[1].disabled_reason == nil
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "codex", config_dir = data } })
  harness.open()
  assert(vim.wait(10000, function() return (session.harness.ready and session.harness.session) or #errors > 0 end, 10))
  assert(#errors == 0, table.concat(errors, "\n"))
  local state = session.harness
  local identity = state.session.id
  require("forge.views.harness.native_import").open(state, {
    window_list = { state.transcript_win, state.composer_win }, control_win = state.composer_win,
  })
  confirm("mcp")
  confirm("workspace")
  assert(vim.wait(10000, preview_ready, 10), "native import preview did not render")
  if vim.g.forge_native_import_inspect then return end
  confirm("restricted")
  assert(#warnings == 1 and warnings[1]:find("disabled_tools", 1, true), "blocked native policy was selectable")
  local candidate = confirm("portable")
  local imported_id = candidate.id
  local rendered = vim.api.nvim_buf_get_lines(picker._state_for_test().buf, 0, -1, false)
  assert(not table.concat(rendered, "\n"):find("IMPORT_LITERAL", 1, true), "preview exposed native values")
  write_source("changed-command")
  confirm("apply")
  assert(vim.wait(10000, function() return #errors > 0 and not state.busy and preview_ready() end, 10), "source-change rejection did not recover")
  assert(errors[1]:find("source changed", 1, true), errors[1])
  local unchanged = request("settings.get")
  assert(unchanged.revision == 0 and #unchanged.catalog.mcp == 0, "stale preview mutated settings")
  errors = {}
  confirm("portable")
  confirm("apply")
  assert(vim.wait(30000, function() return not state.busy and not picker.is_open("harness-native-import") end, 10),
    "native import application did not settle: " .. vim.inspect({ errors = errors, operation = state.task_operation }))
  assert(#errors == 0, table.concat(errors, "\n"))
  local imported = request("settings.get")
  assert(imported.revision == 1 and #imported.catalog.mcp == 1)
  assert(imported.catalog.mcp[1].id == imported_id and imported.desired.mcp[imported_id] == false)
  local imported_server = request("backend.mcp")[1]
  assert(imported_server.scope == "workspace", "native import lost its source scope")
  assert(vim.fs.normalize(imported_server.source_path):lower() == vim.fs.normalize(source):lower(),
    "native import lost its source configuration path")
  assert(imported.application.applied_revision == imported.revision, "import did not pass application verification")
  assert(state.session.id == identity, "configuration import replaced the Forge conversation")
  assert(vim.wait(5000, function()
    for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
      if line:find("Settings completed:", 1, true) and line:find("mcp_definition", 1, true) then return true end
    end
    return false
  end, 10), "native import omitted its configuration timeline event")
  local preserved = table.concat(vim.fn.readfile(source), "\n")
  assert(preserved:find("IMPORT_LITERAL$VALUE", 1, true) and preserved:find("IMPORT_AUTHENTICATION_VALUE", 1, true),
    "import changed native source or authentication")
  for _, backend in ipairs({ "copilot", "claude", "antigravity" }) do
    harness.switch_backend(backend, { kind = "new" })
    assert(vim.wait(10000, function()
      return session.harness.ready and not session.harness.switching_backend and session.harness.session.backend == backend
    end, 10), "provider switch did not settle")
    local transferred = request("settings.get")
    assert(transferred.catalog.mcp[1].id == imported_id and transferred.desired.mcp[imported_id] == false,
      "provider switch lost imported identity or enabled it")
    local server = request("backend.mcp")[1]
    assert(server.scope == "workspace" and server.source_path == imported_server.source_path,
      "provider switch lost source metadata")
  end
end, debug.traceback)
if success and vim.g.forge_native_import_inspect then return end
picker.close(false)
client.stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath, vim.ui.input = original_stdpath, original_input
if not success or not collected then
  io.stderr:write(tostring(failure or "host was not collected"), "\n")
  vim.cmd("cquit 1")
end
print("harness_native_import_host: passed (picker, native host, storage, stale-source recovery, disabled application)")
vim.cmd("qa!")
