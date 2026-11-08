-- Unit tests for `forge.lsp.startup` (startup.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "lsp_root_deadline", run = function()
local startup = require("forge.lsp.startup")
local directory = vim.fn.tempname()
for _, name in ipairs({ "one", "two", "three" }) do
  vim.fn.mkdir(directory .. "/" .. name, "p")
  vim.fn.writefile({ "return {}" }, directory .. "/" .. name .. "/main.lua")
end
local original = { configs = vim.lsp.get_configs, start = vim.lsp.start, clients = vim.lsp.get_clients,
  client = vim.lsp.get_client_by_id, attached = vim.lsp.buf_is_attached, now = vim.uv.now, notify = vim.notify }
local callbacks, clients, notices = {}, {}, {}
local elapsed, launches, respond = 0, 0, false
vim.uv.now = function() return original.now() + elapsed end
vim.notify = function(message) notices[#notices + 1] = message end
vim.lsp.get_configs = function()
  return { { name = "root-deadline", filetypes = { "lua" }, root_dir = function(_, receive)
    callbacks[#callbacks + 1] = receive
    if respond then vim.schedule(function() receive(directory) end) end
  end } }
end
vim.lsp.get_clients = function() return clients end
vim.lsp.get_client_by_id = function(id) return clients[id] end
vim.lsp.buf_is_attached = function() return false end
vim.lsp.start = function(config)
  launches = launches + 1
  assert(launches == 1, "concurrent roots started duplicate servers")
  clients[1] = { id = 1, name = config.name, root_dir = directory, config = config, initialized = false,
    is_stopped = function() return false end }
  return 1
end
local success, failure = xpcall(function()
  local state = startup.start(directory)
  assert(vim.wait(500, function() return #callbacks == 3 end, 5), "root discovery serialized unanswered callbacks")
  elapsed = 2001
  assert(vim.wait(500, function() return not state.discovering end, 5), "root discovery did not stop after two seconds")
  for _, job in pairs(state.jobs) do assert(job.done and job.rejected and not job.failure) end
  assert(#notices == 0, "a skipped root was reported as a server failure")
  for _, receive in ipairs(callbacks) do receive(directory) end
  vim.wait(50, function() return launches > 0 end, 5)
  assert(launches == 0, "late root callback launched a server")
  startup.close()

  elapsed, respond, callbacks = 0, true, {}
  state = startup.start(directory)
  assert(vim.wait(500, function() return not state.discovering end, 5), "valid roots did not finish discovery")
  assert(launches == 1)
  elapsed = 2500
  vim.wait(100, function() return #notices > 0 end, 5)
  assert(#notices == 0, "root timeout was applied to server initialization")
  elapsed = 60001
  assert(vim.wait(500, function() return #notices > 0 end, 5), "server initialization lost its longer deadline")
  assert(#notices == 1 and notices[1]:find("initialization exceeded 60 seconds", 1, true))
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start, vim.lsp.get_clients = original.configs, original.start, original.clients
vim.lsp.get_client_by_id, vim.lsp.buf_is_attached = original.client, original.attached
vim.uv.now, vim.notify = original.now, original.notify
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_root_deadline: passed")
  end },
  { name = "lsp_startup_manifests", run = function()
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
  -- Discovery yields after every 256 directory entries, so a few batches of noise prove it keeps scanning.
  for index = 1, 600 do vim.fn.writefile({}, noise .. "/entry-" .. index) end

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
  end },
  { name = "lazy_startup", run = function()
local startup = require("forge.lsp.startup")
local source = require("forge.lsp.source")
local directory = vim.fn.tempname()
local frontend, provider, sibling = directory .. "/frontend", directory .. "/provider", directory .. "/sibling"
for _, path in ipairs({ frontend, provider, sibling }) do vim.fn.mkdir(path, "p") end
vim.fn.writefile({ "{}" }, frontend .. "/package.json")
vim.fn.writefile({ "{}" }, sibling .. "/package.json")
vim.fn.writefile({ "const ready = true" }, frontend .. "/main.ts")
vim.fn.writefile({ "const other = true" }, sibling .. "/main.ts")
vim.fn.writefile({ "[package]" }, provider .. "/Cargo.toml")
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
  assert(roots["rust:" .. source.canonical(provider)] and roots["typescript:" .. source.canonical(sibling)])
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
  end },
  { name = "startup", run = function()
local startup = require("forge.lsp.startup")
local query = require("forge.lsp.semantic")
local source = require("forge.lsp.source")
local directory = vim.fn.tempname()
vim.fn.mkdir(directory, "p")
local path = directory .. "/main.lua"
vim.fn.writefile({ "function ready() end" }, path)
local original = { configs = vim.lsp.get_configs, start = vim.lsp.start, client = vim.lsp.get_client_by_id,
  clients = vim.lsp.get_clients, attached = vim.lsp.buf_is_attached }
local calls, queried, client = 0, false, nil
local root_in_fast_event = false
local deferred_root
local hold_root = false
vim.lsp.get_configs = function(filter)
  assert(filter.enabled)
  return { { name = "delayed", filetypes = { "lua" }, root_dir = function(_, on_root)
    if hold_root then deferred_root = on_root return end
    vim.uv.fs_stat(path, function(failure)
      assert(not failure, failure)
      root_in_fast_event = vim.in_fast_event()
      on_root(directory)
    end)
  end } }
end
vim.lsp.start = function(config, options)
  assert(not vim.in_fast_event(), "LSP start ran in a fast event")
  assert(vim.api.nvim_buf_is_loaded(options.bufnr), "startup source was unloaded")
  assert(config.name == "delayed" and config.root_dir == directory)
  if client then return client.id end
  calls = calls + 1
  client = { id = 91, initialized = false, config = config, name = config.name }
  function client:is_stopped() return false end
  function client:supports_method() return true end
  function client:request(_, _, callback)
    assert(self.initialized, "query reached a server before initialization")
    queried = true
    vim.schedule(function() callback(nil, { contents = "ready" }) end)
    return true, 1
  end
  function client:cancel_request() end
  return client.id
end
vim.lsp.get_client_by_id = function(identity) return client and client.id == identity and client end
vim.lsp.get_clients = function() return client and client.initialized and { client } or {} end
vim.lsp.buf_is_attached = function() return true end
local success, failure = xpcall(function()
  startup.start(directory)
  assert(vim.wait(1000, function() return client ~= nil end, 5), "background startup did not run")
  assert(root_in_fast_event, "fixture did not exercise a libuv root callback")
  local done, result, error
  query.request(directory, { operation = "hover", path = path, line = 1, column = 1 }, function(_, receive) receive({}) end,
    function(value, message) done, result, error = true, value, message end)
  vim.wait(150, function() return done end, 5)
  assert(not done and not queried, "query resolved while the server was initializing")
  client.initialized = true
  client.progress = { pending = { indexing = "Indexing" } }
  vim.wait(150, function() return done end, 5)
  assert(not done and not queried, "query ran while the server reported indexing")
  client.progress.pending = {}
  assert(vim.wait(1000, function() return done end, 5), "query did not resume after initialization")
  assert(not error and result.hover == "ready", tostring(error))
  assert(calls == 1, "query created a duplicate server")
  local missing, missing_done
  startup.wait(directory, directory .. "/missing.lua", function(failure) missing, missing_done = failure, true end)
  assert(vim.wait(1000, function() return missing_done end, 5))
  assert(missing:find("path_not_found:", 1, true) and calls == 1)
  local recovered, recovery_error
  startup.wait(directory, path, function(failure) recovered, recovery_error = true, failure end)
  assert(vim.wait(1000, function() return recovered end, 5))
  assert(not recovery_error, "missing source poisoned the valid startup waiter")
  local cancelled = false
  client.initialized = false
  local cancel = startup.wait(directory, path, function() cancelled = true end)
  cancel()
  vim.wait(80, function() return cancelled end, 5)
  assert(not cancelled, "cancelled startup waiter called its consumer")
  startup.close()
  assert(client and calls == 1, "closing Harness stopped an editor-owned server")
  assert(not source.loaded(path), "startup leaked a hidden buffer")
  hold_root = true
  startup.start(directory)
  assert(vim.wait(1000, function() return deferred_root ~= nil end, 5), "deferred root was not requested")
  startup.close()
  deferred_root(directory)
  vim.wait(80, function() return calls > 1 end, 5)
  assert(calls == 1 and not source.loaded(path), "late root callback restarted a closed workspace")
end, debug.traceback)
startup.close()
vim.lsp.get_configs, vim.lsp.start, vim.lsp.get_client_by_id = original.configs, original.start, original.client
vim.lsp.get_clients, vim.lsp.buf_is_attached = original.clients, original.attached
vim.fn.delete(directory, "rf")
assert(success, failure)
print("lsp_startup: passed")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
