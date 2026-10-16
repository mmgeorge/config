local startup = require("forge.lsp.startup")
local source = require("forge.lsp.source")
local directory = vim.fn.tempname()
local project = directory .. "/blue"
local nested = project .. "/a-tools"
vim.fn.mkdir(nested, "p")
vim.fn.mkdir(project .. "/z-app", "p")
local nested_path, main_path = nested .. "/tool.lua", project .. "/z-app/main.lua"
vim.fn.writefile({ "return {}" }, nested_path)
vim.fn.writefile({ "return {}" }, main_path)
local original = { configs = vim.lsp.get_configs, start = vim.lsp.start,
  client = vim.lsp.get_client_by_id, clients = vim.lsp.get_clients }
local launched, clients = {}, {}
local nested_only = false
local main_root = directory
vim.lsp.get_configs = function()
  return { { name = "workspace-fixture", filetypes = { "lua" }, root_dir = function(buffer, on_root)
    local path = vim.api.nvim_buf_get_name(buffer)
    local root = (nested_only or source.inside(path, nested)) and nested or main_root
    vim.uv.fs_stat(path, function() on_root(root) end)
  end } }
end
vim.lsp.start = function(config)
  assert(not vim.in_fast_event())
  for identity, client in pairs(clients) do
    if source.canonical(client.root_dir) == source.canonical(config.root_dir) then return identity end
  end
  local identity = #launched + 1
  launched[identity] = source.canonical(config.root_dir)
  local client = { id = identity, name = config.name, config = config, root_dir = config.root_dir, initialized = true }
  function client:is_stopped() return false end
  clients[identity] = client
  return identity
end
vim.lsp.get_client_by_id = function(identity) return clients[identity] end
vim.lsp.get_clients = function() return vim.tbl_values(clients) end
local function await(path)
  local done, failure
  startup.wait(directory, path, function(error) done, failure = true, error end)
  assert(vim.wait(1500, function() return done end, 5), "root selection did not finish")
  assert(not failure, failure)
end
local success, failure = xpcall(function()
  await()
  assert(#launched == 1 and launched[1] == source.canonical(directory), "startup warmed the nested workspace instead of the main project")
  await(main_path)
  assert(#launched == 1, "first main-project request launched another server")
  await(nested_path)
  assert(#launched == 2 and launched[2] == source.canonical(nested), "an explicit independent-workspace request did not start its own server")
  startup.close()
  clients, launched = {}, {}
  await(main_path)
  assert(#launched == 1, "query racing discovery did not share startup")
  startup.close()
  clients, launched, main_root = {}, {}, project
  await()
  assert(#launched == 1 and launched[1] == source.canonical(project), "repository parent selected a deeper independent workspace")
  startup.close()
  clients, launched, nested_only = {}, {}, true
  await()
  assert(#launched == 1 and launched[1] == source.canonical(nested), "nested-only project lost startup fallback")
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start = original.configs, original.start
vim.lsp.get_client_by_id, vim.lsp.get_clients = original.client, original.clients
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_startup_roots: passed")
