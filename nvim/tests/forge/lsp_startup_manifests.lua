local startup = require("forge.lsp.startup")
local source = require("forge.lsp.source")
local directory = vim.fn.tempname()
vim.fn.mkdir(directory, "p")
vim.fn.writefile({ "console.log('loose')" }, directory .. "/loose.js")
vim.fn.writefile({ "fn main() {}" }, directory .. "/loose.rs")
local original = {
  configs = vim.lsp.get_configs, start = vim.lsp.start, client = vim.lsp.get_client_by_id,
  clients = vim.lsp.get_clients, notify = vim.notify, now = vim.uv.now, attached = vim.lsp.buf_is_attached,
}
local clients, launched, notices = {}, {}, {}
local elapsed, initialize = 0, true
vim.uv.now = function() return original.now() + elapsed end
vim.notify = function(message) notices[#notices + 1] = message end
vim.lsp.get_configs = function()
  local function root(buffer, receive)
    local path = vim.api.nvim_buf_get_name(buffer)
    assert(path:match("/package%.json$") or path:match("/Cargo%.toml$"), "startup selected a source file: " .. path)
    vim.uv.fs_stat(path, function(failure)
      assert(not failure, failure)
      receive(vim.fs.dirname(path))
    end)
  end
  return {
    { name = "typescript", filetypes = { "javascript", "typescript" }, root_dir = root },
    { name = "rust", filetypes = { "rust" }, root_dir = root },
    { name = "eslint", filetypes = { "javascript", "typescript" }, root_dir = function() end },
  }
end
vim.lsp.start = function(config, options)
  assert(not vim.in_fast_event(), "manifest startup ran in a fast event")
  assert(options.attach == false, "language server attached to the manifest as source")
  assert(config.name ~= "eslint", "declined ESLint root started a server")
  local path = vim.api.nvim_buf_get_name(options.bufnr)
  assert(vim.bo[options.bufnr].filetype == vim.filetype.match({ filename = path }), "manifest received the source language filetype")
  local identity = #launched + 1
  launched[identity] = { name = config.name, path = path }
  local client = { id = identity, name = config.name, config = config, initialized = initialize }
  function client:is_stopped() return false end
  clients[identity] = client
  return identity
end
vim.lsp.get_client_by_id = function(identity) return clients[identity] end
vim.lsp.get_clients = function() return vim.tbl_values(clients) end
vim.lsp.buf_is_attached = function() return false end
local success, failure = xpcall(function()
  local state = startup.start(directory)
  assert(vim.wait(1000, function() return not state.discovering end, 5))
  assert(next(state.jobs) == nil and #launched == 0, "loose JavaScript or Rust files triggered startup")
  startup.close()

  local project = directory
  for depth = 1, 10 do project = project .. "/level-" .. depth end
  vim.fn.mkdir(project, "p")
  vim.fn.writefile({ '{"name":"manifest-only"}' }, project .. "/package.json")
  vim.fn.writefile({ '[workspace]', 'members = []' }, project .. "/Cargo.toml")
  for index = 1, 130 do vim.fn.mkdir(directory .. "/directory-" .. index, "p") end
  local noise = directory .. "/noise"
  vim.fn.mkdir(noise, "p")
  for index = 1, 4100 do vim.fn.writefile({}, noise .. "/entry-" .. index) end

  state = startup.start(directory)
  assert(vim.wait(10000, function() return #launched == 2 end, 5), "nested manifests were not discovered")
  elapsed = 60001
  assert(vim.wait(1000, function() return not state.discovering end, 5), "declined root blocked discovery")
  assert(#notices == 0, "declined root reported an initialization failure: " .. vim.inspect(notices))
  local selected = {}
  for _, launch in ipairs(launched) do selected[launch.name] = vim.fs.basename(launch.path) end
  assert(selected.typescript == "package.json" and selected.rust == "Cargo.toml")
  for _, job in pairs(state.jobs) do
    assert(not job.failure, job.failure)
    assert(job.path:match("package%.json$") or job.path:match("Cargo%.toml$"))
  end
  startup.close()
  assert(not source.loaded(project .. "/package.json") and not source.loaded(project .. "/Cargo.toml"))

  local stalled = directory .. "/stalled"
  vim.fn.mkdir(stalled, "p")
  vim.fn.writefile({ '{}' }, stalled .. "/package.json")
  initialize, elapsed = false, 0
  state = startup.start(stalled)
  assert(vim.wait(1000, function() return #launched == 3 end, 5), vim.inspect({ launched = launched, jobs = state.jobs }))
  elapsed = 60001
  assert(vim.wait(1000, function() return #notices > 0 end, 5), "client initialization timeout did not settle")
  assert(#notices == 1 and notices[1]:find("typescript initialization exceeded", 1, true),
    "an actual client initialization timeout was suppressed")
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start = original.configs, original.start
vim.lsp.get_client_by_id, vim.lsp.get_clients = original.client, original.clients
vim.lsp.buf_is_attached = original.attached
vim.notify, vim.uv.now = original.notify, original.now
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_startup_manifests: passed")
