-- Unit tests for `forge.startup_log` (startup_log.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["per_request_events_need_trace"] = function()
  local recorded = {}
  package.loaded["forge.infra.jsonl_log"] = { new = function()
    return { append = function(record) recorded[#recorded + 1] = record.event end, idle = function() return true end }
  end }
  MiniTest.finally(function() package.loaded["forge.infra.jsonl_log"] = nil end)
  local perf = require("forge.infra.perf")
  local startup_log = require("forge.startup_log")

  perf.setup({ harness = { enabled = false, info = true } })
  startup_log.write("host.request.send", { id = 1 })
  startup_log.write("host.response.received", { id = 1 })
  startup_log.write("host.result.decoded", { id = 1 })
  startup_log.write("host.exit", { code = 0 })
  MiniTest.expect.equality(recorded, { "host.exit" })

  perf.setup({ harness = { enabled = true, info = true } })
  startup_log.write("host.request.send", { id = 2 })
  MiniTest.expect.equality(recorded, { "host.exit", "host.request.send" })
end

return T
