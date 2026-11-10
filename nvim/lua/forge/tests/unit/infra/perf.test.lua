local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["saturated_queue_skips_encoding"] = function()
  local perf = require("forge.infra.perf")
  perf.options.harness.enabled = true
  perf.queue.harness = {}
  for index = 1, 4096 do perf.queue.harness[index] = "{}" end
  local encode = vim.json.encode
  local calls = 0
  vim.json.encode = function(...)
    calls = calls + 1
    return encode(...)
  end
  local ok, result = pcall(perf.event, "harness", "saturated", { rows = 100 })
  vim.json.encode = encode
  perf.options.harness.enabled = false
  eq(ok, true)
  eq(result, "saturated")
  eq(calls, 0)
  eq(perf.rejected_record_count.harness, 1)
  eq(#perf.queue.harness, 4096)
end

return T
