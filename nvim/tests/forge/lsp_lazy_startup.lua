local startup = require("forge.lsp.startup")
local source = require("forge.lsp.source")
local directory = vim.fn.tempname()
local frontend, backend, sibling = directory .. "/frontend", directory .. "/backend", directory .. "/sibling"
for _, path in ipairs({ frontend, backend, sibling }) do vim.fn.mkdir(path, "p") end
vim.fn.writefile({ "{}" }, frontend .. "/package.json")
vim.fn.writefile({ "{}" }, sibling .. "/package.json")
vim.fn.writefile({ "const ready = true" }, frontend .. "/main.ts")
vim.fn.writefile({ "const other = true" }, sibling .. "/main.ts")
vim.fn.writefile({ "[package]" }, backend .. "/Cargo.toml")
local original = { configs = vim.lsp.get_configs, start = vim.lsp.start, client = vim.lsp.get_client_by_id,
  clients = vim.lsp.get_clients, attached = vim.lsp.buf_is_attached, scan = vim.uv.fs_scandir, now = vim.uv.now }
local launched, scans, reads = {}, 0, 0
local hold_initialization = true
local clients = { [71] = { id = 71, name = "typescript", root_dir = frontend,
  config = { name = "typescript", root_dir = frontend, filetypes = { "typescript" } }, initialized = true } }
clients[71].is_stopped = function() return false end
vim.lsp.get_configs = function()
  reads = reads + 1
  return {
    { name = "typescript", filetypes = { "typescript" }, root_dir = function(buffer, done) done(vim.fs.dirname(vim.api.nvim_buf_get_name(buffer))) end },
    { name = "rust", filetypes = { "rust" }, root_dir = function(buffer, done) done(vim.fs.dirname(vim.api.nvim_buf_get_name(buffer))) end },
  }
end
vim.uv.fs_scandir = function(...) scans = scans + 1 return original.scan(...) end
vim.lsp.get_clients = function() return vim.tbl_values(clients) end
vim.lsp.get_client_by_id = function(identity) return clients[identity] end
vim.lsp.buf_is_attached = function() return false end
vim.lsp.start = function(config)
  local identity = 71 + #launched + 1
  launched[#launched + 1] = { name = config.name, root = source.canonical(config.root_dir) }
  clients[identity] = { id = identity, name = config.name, config = config, root_dir = config.root_dir, initialized = config.name ~= "typescript" or not hold_initialization }
  clients[identity].is_stopped = function() return false end
  return identity
end
local function await(path)
  local done, failure
  startup.wait(directory, path, function(error) done, failure = true, error end)
  assert(vim.wait(2000, function() return done end, 5), "lazy startup did not finish")
  assert(not failure, failure)
end
local success, failure = xpcall(function()
  assert(reads == 0 and scans == 0 and #launched == 0, "loading startup performed work")
  await(frontend .. "/main.ts")
  assert(scans == 0 and #launched == 0, "file query scanned workspace or duplicated editor client")
  local done, failure, issues
  startup.wait(directory, nil, function(error, gaps) done, failure, issues = true, error, gaps end)
  assert(vim.wait(2000, function() return #launched == 2 end, 5), "one initializing server blocked another project root")
  assert(scans > 0 and #launched == 2, "workspace request omitted a distinct project root")
  await(frontend .. "/main.ts")
  assert(not done, "workspace ignored an initializing project server")
  local now = original.now()
  vim.uv.now = function() return now + 60001 end
  assert(vim.wait(2000, function() return done end, 5), "workspace startup timeout did not settle")
  vim.uv.now = original.now
  assert(not failure and #issues == 1 and issues[1].name == "typescript", "startup timeout discarded ready project servers")
  hold_initialization = false
  for _, client in pairs(clients) do client.initialized = true end
  local roots = {}
  for _, launch in ipairs(launched) do roots[launch.name .. ":" .. launch.root] = true end
  assert(roots["rust:" .. source.canonical(backend)] and roots["typescript:" .. source.canonical(sibling)])
  await(frontend .. "/main.ts")
  await(sibling .. "/main.ts")
  await(nil)
  assert(#launched == 2, "repeated queries restarted shared editor servers")
  local selected = source.select_client(sibling .. "/main.ts", "typescript", nil, nil, directory)
  assert(selected and source.canonical(selected.root_dir) == source.canonical(sibling), "file routed to its sibling's client")
  local unrelated = source.select_client(directory .. "/unowned.ts", "typescript", nil, nil, directory)
  assert(not unrelated, "unowned file was routed to an unrelated project")
  selected.is_stopped = function() return true end
  await(nil)
  assert(#launched == 3, "workspace query did not restart a stopped project server")
  startup.close()
  assert(clients[71] and clients[71].initialized, "Harness closed an editor-owned client")
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start, vim.lsp.get_client_by_id = original.configs, original.start, original.client
vim.lsp.get_clients, vim.lsp.buf_is_attached, vim.uv.fs_scandir = original.clients, original.attached, original.scan
vim.uv.now = original.now
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_lazy_startup: passed")
