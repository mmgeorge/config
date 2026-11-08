-- Unit tests for `forge.lsp.startup` (startup.lua), as a mini.test set. `.forge/checks/lua-tests.nu`
-- runs the file as one shard through `tests/support/minitest_shard.lua`, one case after another in a
-- single Neovim, so every case restores the editor functions it replaces in `post_case`.
local MiniTest = require("mini.test")
local eq = MiniTest.expect.equality

-- The editor functions the cases replace with fakes, captured before each case and restored after
-- it, even when the case fails.
local original_editor = {}

local T = MiniTest.new_set({
  hooks = {
    pre_case = function()
      original_editor = {
        get_configs = vim.lsp.get_configs, start = vim.lsp.start, get_clients = vim.lsp.get_clients,
        get_client_by_id = vim.lsp.get_client_by_id, buf_is_attached = vim.lsp.buf_is_attached,
        now = vim.uv.now, fs_scandir = vim.uv.fs_scandir, notify = vim.notify,
      }
    end,
    post_case = function()
      require("forge.lsp.startup").close()
      vim.lsp.get_configs, vim.lsp.start = original_editor.get_configs, original_editor.start
      vim.lsp.get_clients, vim.lsp.get_client_by_id = original_editor.get_clients, original_editor.get_client_by_id
      vim.lsp.buf_is_attached = original_editor.buf_is_attached
      vim.uv.now, vim.uv.fs_scandir = original_editor.now, original_editor.fs_scandir
      vim.notify = original_editor.notify
    end,
  },
})

-- A fresh directory that the case's `finally` step deletes.
local function temporary_directory()
  local directory = vim.fn.tempname()
  vim.fn.mkdir(directory, "p")
  MiniTest.finally(function() vim.fn.delete(directory, "rf") end)
  return directory
end

T["lsp_root_deadline"] = function()
  local startup = require("forge.lsp.startup")
  local directory = temporary_directory()
  for _, name in ipairs({ "one", "two", "three" }) do
    vim.fn.mkdir(directory .. "/" .. name, "p")
    vim.fn.writefile({ "return {}" }, directory .. "/" .. name .. "/main.lua")
  end
  local callbacks, clients, notices = {}, {}, {}
  local elapsed, launches, respond = 0, 0, false
  vim.uv.now = function() return original_editor.now() + elapsed end
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

  local state = startup.start(directory)
  assert(vim.wait(500, function() return #callbacks == 3 end, 5), "root discovery serialized unanswered callbacks")
  elapsed = 2001
  assert(vim.wait(500, function() return not state.discovering end, 5), "root discovery did not stop after two seconds")
  for _, job in pairs(state.jobs) do assert(job.done and job.rejected and not job.failure) end
  eq(notices, {})
  for _, receive in ipairs(callbacks) do receive(directory) end
  vim.wait(50, function() return launches > 0 end, 5)
  assert(launches == 0, "late root callback launched a server")
  startup.close()

  elapsed, respond, callbacks = 0, true, {}
  state = startup.start(directory)
  assert(vim.wait(500, function() return not state.discovering end, 5), "valid roots did not finish discovery")
  eq(launches, 1)
  elapsed = 2500
  vim.wait(100, function() return #notices > 0 end, 5)
  assert(#notices == 0, "root timeout was applied to server initialization")
  elapsed = 60001
  assert(vim.wait(500, function() return #notices > 0 end, 5), "server initialization lost its longer deadline")
  eq(#notices, 1)
  assert(notices[1]:find("initialization exceeded 60 seconds", 1, true), notices[1])
end

T["lsp_startup_manifests"] = function()
  local startup = require("forge.lsp.startup")
  local source = require("forge.lsp.source")
  local directory = temporary_directory()
  vim.fn.writefile({ "console.log('loose')" }, directory .. "/loose.js")
  vim.fn.writefile({ "fn main() {}" }, directory .. "/loose.rs")
  local clients, launched, notices = {}, {}, {}
  local elapsed, initialize = 0, true
  vim.uv.now = function() return original_editor.now() + elapsed end
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
  eq(notices, {})
  local selected = {}
  for _, launch in ipairs(launched) do selected[launch.name] = vim.fs.basename(launch.path) end
  eq(selected, { typescript = "package.json", rust = "Cargo.toml" })
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
  eq(#notices, 1)
  assert(notices[1]:find("typescript initialization exceeded", 1, true), "an actual client initialization timeout was suppressed")
end

T["lazy_startup"] = function()
  local startup = require("forge.lsp.startup")
  local source = require("forge.lsp.source")
  local directory = temporary_directory()
  local frontend, provider, sibling = directory .. "/frontend", directory .. "/provider", directory .. "/sibling"
  for _, path in ipairs({ frontend, provider, sibling }) do vim.fn.mkdir(path, "p") end
  vim.fn.writefile({ "{}" }, frontend .. "/package.json")
  vim.fn.writefile({ "{}" }, sibling .. "/package.json")
  vim.fn.writefile({ "const ready = true" }, frontend .. "/main.ts")
  vim.fn.writefile({ "const other = true" }, sibling .. "/main.ts")
  vim.fn.writefile({ "[package]" }, provider .. "/Cargo.toml")
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
  vim.uv.fs_scandir = function(...) scans = scans + 1 return original_editor.fs_scandir(...) end
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

  assert(reads == 0 and scans == 0 and #launched == 0, "loading startup performed work")
  await(frontend .. "/main.ts")
  assert(scans == 0 and #launched == 0, "file query scanned workspace or duplicated editor client")
  local done, failure, issues
  startup.wait(directory, nil, function(error, gaps) done, failure, issues = true, error, gaps end)
  assert(vim.wait(2000, function() return #launched == 2 end, 5), "one initializing server blocked another project root")
  assert(scans > 0 and #launched == 2, "workspace request omitted a distinct project root")
  await(frontend .. "/main.ts")
  assert(not done, "workspace ignored an initializing project server")
  local now = original_editor.now()
  vim.uv.now = function() return now + 60001 end
  assert(vim.wait(2000, function() return done end, 5), "workspace startup timeout did not settle")
  vim.uv.now = original_editor.now
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
end

T["startup"] = function()
  local startup = require("forge.lsp.startup")
  local query = require("forge.lsp.semantic")
  local source = require("forge.lsp.source")
  local directory = temporary_directory()
  local path = directory .. "/main.lua"
  vim.fn.writefile({ "function ready() end" }, path)
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
  assert(not error, tostring(error))
  eq(result.hover, "ready")
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
end

return T
