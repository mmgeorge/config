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

return T
