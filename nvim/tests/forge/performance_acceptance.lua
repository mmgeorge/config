local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.loader.enable(false)

local metrics = {}

local function rss_bytes()
  local ok, value = pcall(vim.uv.resident_set_memory)
  return ok and value or nil
end

local function measure(operation, callback)
  collectgarbage("collect")
  local before_kib, before_rss = collectgarbage("count"), rss_bytes()
  local started = vim.uv.hrtime()
  callback()
  local elapsed_ms = (vim.uv.hrtime() - started) / 1000000
  metrics[#metrics + 1] = {
    operation = operation,
    elapsed_ms = elapsed_ms,
    lua_kib_before = before_kib,
    lua_kib_after = collectgarbage("count"),
    rss_bytes_before = before_rss,
    rss_bytes_after = rss_bytes(),
  }
end

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function metadata(target)
  return { target = { { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } },
    decoration = {}, editable_region = {}, visible_decoration = {}, fold = {}, gutter = {} }
end

local status = require("forge.status")
local review = require("forge.review_document")
local client = require("forge.client")
local builder = require("forge.builder")
local protocol = require("forge.protocol")
local original_builder_ensure = builder.ensure

local ok, failure = xpcall(function()
  measure("status.open_demand_close", function()
    status._set_runner_for_test(function(method, params, callback)
      assert_true(method == "status", "status did not use its native route")
      if params.operation == "open" then
        callback(fixture.snapshot(params.document))
      elseif params.operation == "demand" then callback(fixture.body(params.input.document, { "changed line" }))
      elseif params.operation == "close_view" then callback(vim.NIL)
      elseif params.operation == "close" then callback({ closed = true })
      else error("unexpected status operation " .. params.operation) end
    end)
    local state = status.open({ workspace = vim.fn.getcwd() })
    assert_true(vim.wait(1000, function() return state.replica.status == "Applied" end, 5), "status did not settle")
    vim.api.nvim_win_set_cursor(0, { 4, 0 })
    vim.cmd("normal! za")
    status.demand(state)
    assert(vim.wait(1000, function() return state.replica.file[1].body ~= nil end))
    status.close(state)
    status._set_runner_for_test(nil)
  end)

  local diff = table.concat({
    "diff --git a/sample.lua b/sample.lua", "--- a/sample.lua", "+++ b/sample.lua", "@@ -1 +1 @@", "-local value = 1", "+local value = 2",
  }, "\n") .. "\n"
  measure("diff.parse", function()
    local parsed = require("forge.render.diff_parse").parse_unified_diff(diff)
    assert_true(#parsed == 1 and #parsed[1].hunks == 1, "diff parser lost the representative hunk")
  end)
  measure("syntax.native_document_bypass", function()
    local buffer = vim.api.nvim_create_buf(false, true)
    vim.b[buffer].forge_native_document = true
    assert_true(not require("forge.native_syntax").attach_global_parser(buffer), "native syntax boundary attached a global parser")
    vim.api.nvim_buf_delete(buffer, { force = true })
  end)

  measure("review.open_close", function()
    review._set_runner_for_test(function(method, params, callback)
      if method == "review.open" or method == "review.open_pr" then callback({ document = "performance-review" })
      elseif method == "review.materialize" then
        callback({ snapshot = { document = params.document, revision = 0,
          block = { { id = "review", text = { "review" }, metadata = metadata("review") } } }, patch = vim.NIL })
      elseif method == "review.section" then callback({ complete = true })
      elseif method == "review.view" or method == "review.close_view" then callback(nil)
      elseif method == "review.close" then callback({ closed = true })
      else error("unexpected review route " .. method) end
    end)
    local state = review.open({ directory = vim.fn.getcwd(), target = { number = 1 } })
    assert_true(vim.wait(1000, function() return state.shown end, 5), "review did not materialize")
    review.close(state)
    review._set_runner_for_test(nil)
  end)

  measure("host.initialize_shutdown", function()
    client._reset_for_test()
    builder.ensure = function(callback)
      callback({ ok = true, path = "fixture-forge" })
      return function() end
    end
    client._set_launcher_for_test(function(_, options, on_exit)
      local process = {}
      function process:write(frame)
        local request = vim.json.decode(frame)
        if request.method == "initialize" then
          vim.schedule(function()
            options.stdout(nil, vim.json.encode({ id = request.id, result = { protocol_version = protocol.VERSION } }) .. "\n")
          end)
        elseif request.method == "shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
      end
      function process:wait() return { code = 0 } end
      function process:kill() on_exit({ code = 0 }) end
      return process
    end)
    local started = false
    client.start(function(_, start_error) started = start_error == nil end)
    assert_true(vim.wait(1000, function() return started end, 5), "host initialize did not settle")
    client.stop()
    assert_true(vim.wait(1000, function() return client._client.process == nil end, 5), "host shutdown did not collect")
    client._reset_for_test()
  end)
end, debug.traceback)

status._set_runner_for_test(nil)
review._set_runner_for_test(nil)
builder.ensure = original_builder_ensure
client._reset_for_test()
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
io.write("FORGE_PERFORMANCE_METRICS=" .. vim.json.encode({ version = 1, metrics = metrics }) .. "\n")
vim.cmd("qa!")
