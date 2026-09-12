vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local original_systemlist = vim.fn.systemlist
local host_count, harness_count, page_count = 0, 0, 0
local expected = {}
for index = 1, 20000 do expected[index] = string.format("branch-%05d-with-a-long-name-for-paging", index) end
local packed = table.concat(expected, "\0") .. "\0"
local initialized = false

local ok, failure = xpcall(function()
  local plugin = dofile("nvim/lua/plugins/forge.lua")[1]
  plugin.config(nil, { harness = { backend = "mock" }, harness_logging = false, diff_logging = false })
  builder.ensure = function(done) done({ ok = true, path = "injected-forge" }) end
  client._set_launcher_for_test(function(_, options)
    host_count = host_count + 1
    local function emit(id, result)
      options.stdout(nil, vim.json.encode({ id = id, result = result }) .. "\n")
    end
    return {
      write = function(_, bytes)
        local request = vim.json.decode(bytes)
        if request.method == "transport.consumed" or request.method == "shutdown" then return end
        if request.method == "initialize" then
          assert(request.params.protocol_version == 3)
          assert(request.params.backend == nil and request.params.data_root == nil)
          emit(request.id, { protocol_version = require("forge.protocol").VERSION })
        elseif request.method == "repository.revisions" then
          page_count = page_count + 1
          local offset = request.params.offset or 0
          local next_offset = math.min(offset + 128 * 1024, #packed)
          emit(request.id, {
            repository = "repository-fixture", reference_digest = string.rep("1", 64),
            revision = 1, offset = offset, next_offset = next_offset, total_bytes = #packed,
            count = #expected, truncated = false, data = vim.base64.encode(packed:sub(offset + 1, next_offset)),
          })
        elseif request.method == "harness.initialize" then
          harness_count = harness_count + 1
          emit(request.id, { session = { id = "shared-host" } })
        elseif request.method == "plan.scope_deviation_review" then emit(request.id, {})
        else error("unexpected shared-host method " .. request.method) end
      end,
      kill = function() end,
    }
  end)
  vim.fn.systemlist = function() error("completion executed synchronous Git") end
  local first = vim.fn.getcompletion("ForgeBranchDiff branch-", "cmdline")
  assert(#first == 0 and host_count == 0, "cold command callback performed I/O")
  assert(vim.wait(2000, function()
    local values = vim.fn.getcompletion("ForgeBranchDiff branch-", "cmdline")
    return #values == 200
  end, 1))
  assert(host_count == 1 and harness_count == 0 and page_count > 1)
  local requests = page_count
  local values = vim.fn.getcompletion("ForgeFileRevision file branch-04999", "cmdline")
  assert(#values == 1 and values[1] == expected[4999])
  local file_values = vim.fn.getcompletion("ForgeBranchDiffFile file branch-05000", "cmdline")
  assert(#file_values == 1 and file_values[1] == expected[5000])
  assert(page_count == requests)
  local sample = {}
  for index = 1, 500 do
    local started = vim.uv.hrtime()
    assert(#vim.fn.getcompletion("ForgeBranchDiff branch-", "cmdline") == 200)
    sample[index] = (vim.uv.hrtime() - started) / 1000000
  end
  table.sort(sample)
  print(vim.json.encode({ completion_values = #expected, samples = #sample, p95_ms = sample[475], maximum_ms = sample[500] }))
  client.start_harness(function(_, start_error)
    assert(not start_error, start_error)
    initialized = true
  end)
  assert(vim.wait(1000, function() return initialized end, 1))
  assert(host_count == 1 and harness_count == 1, "Harness launched a second host")
end, debug.traceback)

vim.fn.systemlist = original_systemlist
builder.ensure = original_ensure
client._reset_for_test()
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
