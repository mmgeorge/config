local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks({ post_case = function() require("forge.lsp.startup").close() end }) })

T["selected_manifest_starts_only_its_configuration"] = function()
  local directory = temp.directory("selected-workspace")
  local manifest = directory .. "/Cargo.toml"
  vim.fn.writefile({ "[package]" }, manifest)
  local client, starts, answers = nil, 0, 0
  local config = { name = "rust", filetypes = { "rust" }, root_dir = function(_, receive) receive(directory) end }
  stub(vim.lsp, "get_configs", function() error("startup enumerated all configurations") end)
  stub(vim.lsp, "get_clients", function() return client and { client } or {} end)
  stub(vim.lsp, "get_client_by_id", function() return client end)
  stub(vim.lsp, "start", function(actual)
    eq(actual.name, "rust")
    eq(actual.cmd_cwd, directory)
    starts = starts + 1
    client = { id = 9, name = "rust", initialized = true, root_dir = directory,
      config = actual, is_stopped = function() return false end }
    return client.id
  end)
  local startup = require("forge.lsp.startup")
  for _ = 1, 2 do startup.selected(manifest, config, function(value, failure)
    eq(failure, nil) eq(value, client) answers = answers + 1
  end) end
  wait_until(function() return answers == 2 end, "selected startup did not settle")
  eq(starts, 1)
end

T["duplicate_clients_fail_without_startup"] = function()
  local clients = {}
  for identity = 1, 2 do clients[identity] = { id = identity, name = "rust", root_dir = "/project",
    config = {}, is_stopped = function() return false end } end
  stub(vim.lsp, "get_clients", function() return clients end)
  stub(vim.lsp, "start", function() error("ambiguous startup") end)
  local failure
  require("forge.lsp.startup").selected("/project/Cargo.toml", {name = "rust"}, function(_, message) failure = message end)
  eq(failure:match("^ambiguous_server:") ~= nil, true)
end

T["editor_started_client_is_reused_while_indexing"] = function()
  local client = { id = 19, name = "rust", initialized = true, root_dir = "/project",
    config = {}, progress = { pending = { indexing = true } }, is_stopped = function() return false end }
  stub(vim.lsp, "get_clients", function() return { client } end)
  stub(vim.lsp, "start", function() error("restarted editor client") end)
  local callbacks = {}
  stub(vim.uv, "new_timer", function()
    local timer = { closed = false }
    function timer:start(_, _, callback) callbacks[#callbacks + 1] = callback end
    function timer:stop() end
    function timer:close() self.closed = true end
    function timer:is_closing() return self.closed end
    return timer
  end)
  local answers = 0
  local startup = require("forge.lsp.startup")
  local config = { name = "rust", root_dir = function() error("reran root discovery") end }
  local cancel = startup.selected("/project/Cargo.toml", config, function(value, failure)
    eq(failure, nil) eq(value, client) answers = answers + 1
  end)
  eq(answers, 0)
  callbacks[1]()
  vim.wait(0, function() return false end)
  eq(answers, 0)
  client.progress.pending = {}
  callbacks[1]()
  wait_until(function() return answers == 1 end, "ready client was not returned")
  callbacks[1]()
  vim.wait(0, function() return false end)
  eq(answers, 1)
  cancel()
end

T["cancelled_readiness_ignores_late_completion"] = function()
  local callback
  local closed = false
  stub(vim.uv, "new_timer", function() return {
    start = function(_, _, _, receive) callback = receive end,
    stop = function() end, close = function() closed = true end,
    is_closing = function() return closed end,
  } end)
  local client = { initialized = false, is_stopped = function() return false end }
  local answers = 0
  local cancel = require("forge.lsp.startup").ready(client, function() answers = answers + 1 end)
  cancel()
  client.initialized = true
  callback()
  vim.wait(0, function() return false end)
  eq(answers, 0)
  eq(closed, true)
end

T["readiness_reports_timeout_and_disconnect_once"] = function()
  local clock, callback = 0, nil
  stub(vim.uv, "now", function() return clock end)
  stub(vim.uv, "new_timer", function()
    local closed = false
    return { start = function(_, _, _, receive) callback = receive end,
      stop = function() end, close = function() closed = true end, is_closing = function() return closed end }
  end)
  local stopped, count, failure = false, 0, nil
  local client = { initialized = false, is_stopped = function() return stopped end }
  local function receive(_, message) count = count + 1 failure = message end
  require("forge.lsp.startup").ready(client, receive)
  clock = 60000
  callback()
  wait_until(function() return count == 1 end, "readiness timeout did not finish")
  eq(failure:match("^timeout:") ~= nil, true)
  callback()
  vim.wait(0, function() return false end)
  eq(count, 1)
  stopped = true
  require("forge.lsp.startup").ready(client, receive)
  eq(count, 2)
  eq(failure:match("^disconnected:") ~= nil, true)
end

T["launch_preserves_explicit_process_directory"] = function()
  local directory = temp.directory("launch-directory")
  local explicit = temp.directory("configured-directory")
  local manifest = directory .. "/package.json"
  vim.fn.writefile({ "{}" }, manifest)
  local client, received
  local config = { name = "typescript", filetypes = { "typescript" }, root_dir = directory, cmd_cwd = explicit }
  stub(vim.lsp, "get_clients", function() return client and { client } or {} end)
  stub(vim.lsp, "get_client_by_id", function() return client end)
  stub(vim.lsp, "start", function(actual)
    eq(actual.root_dir, directory)
    eq(actual.cmd_cwd, explicit)
    client = { id = 71, name = "typescript", initialized = true, root_dir = directory,
      config = actual, is_stopped = function() return false end }
    return client.id
  end)
  require("forge.lsp.startup").selected(manifest, config, function(value, failure)
    eq(failure, nil)
    received = value
  end)
  wait_until(function() return received ~= nil end, "configured directory launch did not finish")
  eq(received, client)
  eq(config.cmd_cwd, explicit)
end

return T
