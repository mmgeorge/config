vim.loader.enable(false)

---@param actual string
---@param expected string
local function assert_equals(actual, expected)
  assert(actual == expected, ("expected: %s\nactual: %s"):format(expected, actual))
end

local ok, failure = pcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  local renderer = require("forge.render.harness.interaction_tree")
  local exchange = {
    id = "metrics", kind = "chat", state = "complete", prompt = "Work",
    completed_at_ms = 7000, duration_ms = 6000, node_list = {},
    metrics = { timing_complete = true, blocked_duration_ms = 3000, request_count = 1 },
    turn = { {
      id = "turn", usage = { input = 76000, cached_input = 68400, reasoning = 3000, output = 4200 },
      tool = { order = { "call" }, item = { call = { id = "call", title = "cargo test", status = "failed", failed = true } } },
    } },
  }
  local function summary(options)
    options = options or {}
    options.expanded = {}
    local result = renderer.build({ { kind = "exchange", exchange = exchange } }, options)
    for index, row in pairs(result.rows) do
      if row.kind == "thought_summary" or row.kind == "thinking_summary" then return result.lines[index] end
    end
    error("exchange summary is missing")
  end
  assert_equals(summary(), "▸ Thought 6s (3s), 76.0k I (90%) → 3.0k R / 1.2k O (~1400 tps), 1 request, 1 tool (1 failed)")
  exchange.duration_ms = 6500
  assert_equals(summary(), "▸ Thought 6s (3s), 76.0k I (90%) → 3.0k R / 1.2k O (~1200 tps), 1 request, 1 tool (1 failed)")
  exchange.duration_ms = 3500
  assert_equals(summary(), "▸ Thought 3s (0s), 76.0k I (90%) → 3.0k R / 1.2k O (~8400 tps), 1 request, 1 tool (1 failed)")
  exchange.duration_ms = 3000
  assert_equals(summary(), "▸ Thought 3s (0s), 76.0k I (90%) → 3.0k R / 1.2k O (— tps), 1 request, 1 tool (1 failed)")
  exchange.duration_ms = 6000
  exchange.turn[1].usage.output = 4251
  assert_equals(summary(), "▸ Thought 6s (3s), 76.0k I (90%) → 3.0k R / 1.3k O (~1417 tps), 1 request, 1 tool (1 failed)")
  exchange.turn[1].usage.output = 4200
  exchange.turn[2] = { id = "second", usage = { input = 4000, cached_input = 0, reasoning = 0, output = 100 }, tool = { order = {}, item = {} } }
  exchange.metrics.request_count = 2
  assert_equals(summary(), "▸ Thought 6s (3s), 80.0k I (86%) → 3.0k R / 1.3k O (~1433 tps), 2 requests, 1 tool (1 failed)")
  exchange.turn[2].usage.reasoning = vim.NIL
  assert_equals(summary(), "▸ Thought 6s (3s), 80.0k I (86%) → — R / — O (~1433 tps), 2 requests, 1 tool (1 failed)")
  exchange.turn[2].usage = vim.NIL
  exchange.metrics.timing_complete = false
  assert_equals(summary(), "▸ Thought 6s (—), — I (—) → — R / — O (— tps), 2 requests, 1 tool (1 failed)")
  exchange.state = "running"
  exchange.completed_at_ms = vim.NIL
  exchange.execution_started_at_ms = 10000
  exchange.metrics.timing_complete = true
  exchange.metrics.blocked_started_ms = 6000
  assert_equals(summary({ now_ms = 12000 }), "▾ Thinking 8s (3s), 1 tool (1 failed)")
  exchange.execution_started_at_ms = vim.NIL
  assert_equals(summary({ now_ms = 90000 }), "▾ Paused 6s (3s), — I (—) → — R / — O (— tps), 2 requests, 1 tool (1 failed)")
  exchange.metrics.request_count = 0
  assert_equals(summary({ now_ms = 90000 }), "▾ Paused 6s (3s), — I (—) → — R / — O (— tps), — requests, 1 tool (1 failed)")
  assert_equals(renderer.foldtext("▸ Thought 6s (3s)")[1][2], "ForgeHarnessThought")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("Harness summary metrics passed")
  vim.cmd("qa!")
end
