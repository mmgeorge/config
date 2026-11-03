local startup = require("forge.lsp.startup")
local directory = vim.fn.tempname()
local project, sibling = directory .. "/project", directory .. "/sibling"
vim.fn.mkdir(project, "p")
vim.fn.mkdir(sibling, "p")
local path = project .. "/main.lua"
vim.fn.writefile({ "return {}" }, path)
local original = { configs = vim.lsp.get_configs, clients = vim.lsp.get_clients, attached = vim.lsp.buf_is_attached,
  start = vim.lsp.start, client = vim.lsp.get_client_by_id }
local function client(id, root, supported, pending)
  return { id = id, name = "fixture-" .. id, initialized = true, root_dir = root,
    config = { root_dir = root, filetypes = { "lua" } }, progress = { pending = pending },
    is_stopped = function() return false end, supports_method = function() return supported end }
end
local selected = client(1, project, true, {})
local unrelated = client(2, sibling, true, { indexing = true })
local unsupported = client(3, project, false, { indexing = true })
vim.lsp.get_configs = function() return {} end
vim.lsp.get_clients = function() return { selected, unrelated, unsupported } end
vim.lsp.buf_is_attached = function() return false end
local function wait(path, readiness)
  local done, failure
  local cancel = startup.wait(directory, path, function(error) done, failure = true, error end,
    { method = "textDocument/definition", ready = readiness })
  return function() return done, failure end, cancel
end
local success, failure = xpcall(function()
  local readiness = {}
  local inspect = wait(project, readiness)
  assert(vim.wait(500, function() return inspect() end, 5), "unrelated or unsupported indexing blocked the project")
  assert(readiness[1] == selected and not readiness[2] and not readiness[3])
  selected.progress.pending.indexing = true
  inspect = wait(path, readiness)
  assert(vim.wait(500, function() return inspect() end, 5), "one request waited twice for its ready server")
  inspect = wait(path, {})
  assert(not vim.wait(100, function() return inspect() end, 5), "a new request skipped indexing readiness")
  selected.progress.pending = {}
  assert(vim.wait(500, function() return inspect() end, 5), "request did not resume after indexing")
  local _, error = inspect()
  assert(not error, error)
  startup.close()
  vim.fn.mkdir(project .. "/src/vendor/unused", "p")
  vim.fn.writefile({ "[workspace]", "members = []" }, project .. "/Cargo.toml")
  vim.fn.writefile({ "[workspace]", "members = []" }, project .. "/src/vendor/unused/Cargo.toml")
  local launches, running = 0, {}
  vim.lsp.get_configs = function() return { { name = "rust", filetypes = { "rust" },
    root_dir = function(_, receive) receive(project) end } } end
  vim.lsp.get_clients = function() return running end
  vim.lsp.get_client_by_id = function(id) return running[id] end
  vim.lsp.start = function(config, options)
    launches = launches + 1
    assert(vim.api.nvim_buf_get_name(options.bufnr):gsub("\\", "/"):match("/project/Cargo%.toml$"),
      "directory startup selected vendored sources")
    running[launches] = client(launches, config.root_dir, true, {})
    running[launches].name = config.name
    return launches
  end
  inspect = wait(project .. "/src", {})
  assert(vim.wait(1000, function() return inspect() end, 5), "cold subdirectory missed its ancestor project")
  assert(launches == 1, "cold directory did not start exactly its ancestor server")
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.get_clients, vim.lsp.buf_is_attached = original.configs, original.clients, original.attached
vim.lsp.start, vim.lsp.get_client_by_id = original.start, original.client
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_readiness: passed")
