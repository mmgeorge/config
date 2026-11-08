local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

-- The real editor functions, captured before each case so mocks can delegate to them.
local original_editor = {}

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    pre_case = function() original_editor = { now = vim.uv.now, fs_scandir = vim.uv.fs_scandir } end,
    post_case = function() require("forge.lsp.startup").close() end,
  }),
})

T["lsp_root_deadline"] = function()
  local startup = require("forge.lsp.startup")
  local directory = temp.directory("workspace")
  for _, name in ipairs({ "one", "two", "three" }) do
    vim.fn.mkdir(directory .. "/" .. name, "p")
    vim.fn.writefile({ "return {}" }, directory .. "/" .. name .. "/main.lua")
  end
  local callbacks, clients = {}, {}
  local elapsed, launches, respond = 0, 0, false
  vim.uv.now = function() return original_editor.now() + elapsed end
  local notices = notify.capture()
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
  wait_until(function() return #callbacks == 3 end, "root discovery serialized unanswered callbacks")
  elapsed = 2001
  wait_until(function() return not state.discovering end, "root discovery did not stop after two seconds")
  for _, job in pairs(state.jobs) do assert(job.done and job.rejected and not job.failure) end
  notices:assert_none()
  for _, receive in ipairs(callbacks) do receive(directory) end
  vim.wait(50, function() return launches > 0 end, 5)
  assert(launches == 0, "late root callback launched a server")
  startup.close()

  elapsed, respond, callbacks = 0, true, {}
  state = startup.start(directory)
  wait_until(function() return not state.discovering end, "valid roots did not finish discovery")
  eq(launches, 1)
  elapsed = 2500
  vim.wait(100, function() return #notices > 0 end, 5)
  assert(#notices == 0, "root timeout was applied to server initialization")
  elapsed = 60001
  wait_until(function() return #notices > 0 end, "server initialization lost its longer deadline")
  eq(#notices, 1)
  assert(notices[1].message:find("initialization exceeded 60 seconds", 1, true), notices[1].message)
end

T["lsp_startup_manifests"] = function()
  local startup = require("forge.lsp.startup")
  local source = require("forge.lsp.source")
  local directory = temp.directory("workspace")
  vim.fn.writefile({ "console.log('loose')" }, directory .. "/loose.js")
  vim.fn.writefile({ "fn main() {}" }, directory .. "/loose.rs")
  local clients, launched = {}, {}
  local elapsed, initialize = 0, true
  vim.uv.now = function() return original_editor.now() + elapsed end
  local notices = notify.capture()
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
  wait_until(function() return not state.discovering end, "discovery of loose source files did not finish")
  assert(next(state.jobs) == nil and #launched == 0, "loose JavaScript or Rust files triggered startup")
  startup.close()

  local project = directory
  for depth = 1, 10 do project = project .. "/level-" .. depth end
  vim.fn.mkdir(project, "p")
  vim.fn.writefile({ '{"name":"manifest-only"}' }, project .. "/package.json")
  vim.fn.writefile({ '[workspace]', 'members = []' }, project .. "/Cargo.toml")
  vim.fn.mkdir(directory .. "/noise", "p")
  -- Discovery yields after every 256 directory entries, so a few batches of noise prove it keeps
  -- scanning. The scanner lists the noise directory's entries without creating them on disk.
  local noise_entries = {}
  for index = 1, 130 do noise_entries[#noise_entries + 1] = { "directory-" .. index, "directory" } end
  for index = 1, 600 do noise_entries[#noise_entries + 1] = { "entry-" .. index, "file" } end
  local noise_listings = {}
  local original_scandir_next = vim.uv.fs_scandir_next
  vim.uv.fs_scandir = function(path, callback)
    local noise_root = path:find("/noise$") ~= nil
    if not noise_root and not path:find("/noise/", 1, true) then return original_editor.fs_scandir(path, callback) end
    local listing = { entries = noise_root and noise_entries or {}, cursor = 0 }
    noise_listings[listing] = true
    vim.schedule(function() callback(nil, listing) end)
    return listing
  end
  vim.uv.fs_scandir_next = function(scan)
    if not noise_listings[scan] then return original_scandir_next(scan) end
    scan.cursor = scan.cursor + 1
    local entry = scan.entries[scan.cursor]
    if entry then return entry[1], entry[2] end
  end

  state = startup.start(directory)
  wait_until(function() return #launched == 2 end, "nested manifests were not discovered", { timeout_ms = 4000 })
  elapsed = 60001
  wait_until(function() return not state.discovering end, "declined root blocked discovery")
  notices:assert_none()
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
  wait_until(function() return #launched == 3 end, "stalled manifest did not launch its server",
    { context = function() return vim.inspect({ launched = launched, jobs = state.jobs }) end })
  elapsed = 60001
  wait_until(function() return #notices > 0 end, "client initialization timeout did not settle")
  eq(#notices, 1)
  assert(notices[1].message:find("typescript initialization exceeded", 1, true), "an actual client initialization timeout was suppressed")
end

T["lazy_startup"] = function()
  local startup = require("forge.lsp.startup")
  local source = require("forge.lsp.source")
  local directory = temp.directory("workspace")
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
    wait_until(function() return done end, "lazy startup did not finish", { timeout_ms = 2000 })
    assert(not failure, failure)
  end

  assert(reads == 0 and scans == 0 and #launched == 0, "loading startup performed work")
  await(frontend .. "/main.ts")
  assert(scans == 0 and #launched == 0, "file query scanned workspace or duplicated editor client")
  local done, failure, issues
  startup.wait(directory, nil, function(error, gaps) done, failure, issues = true, error, gaps end)
  wait_until(function() return #launched == 2 end, "one initializing server blocked another project root", { timeout_ms = 2000 })
  assert(scans > 0 and #launched == 2, "workspace request omitted a distinct project root")
  await(frontend .. "/main.ts")
  assert(not done, "workspace ignored an initializing project server")
  local now = original_editor.now()
  vim.uv.now = function() return now + 60001 end
  wait_until(function() return done end, "workspace startup timeout did not settle", { timeout_ms = 2000 })
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
  local directory = temp.directory("workspace")
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
  wait_until(function() return client ~= nil end, "background startup did not run")
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
  wait_until(function() return done end, "query did not resume after initialization")
  assert(not error, tostring(error))
  eq(result.hover, "ready")
  assert(calls == 1, "query created a duplicate server")
  local missing, missing_done
  startup.wait(directory, directory .. "/missing.lua", function(failure) missing, missing_done = failure, true end)
  wait_until(function() return missing_done end, "startup wait for a missing source did not finish")
  assert(missing:find("path_not_found:", 1, true) and calls == 1)
  local recovered, recovery_error
  startup.wait(directory, path, function(failure) recovered, recovery_error = true, failure end)
  wait_until(function() return recovered end, "startup wait for a valid source did not finish after a missing one")
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
  wait_until(function() return deferred_root ~= nil end, "deferred root was not requested")
  startup.close()
  deferred_root(directory)
  vim.wait(80, function() return calls > 1 end, 5)
  assert(calls == 1 and not source.loaded(path), "late root callback restarted a closed workspace")
end

return T
