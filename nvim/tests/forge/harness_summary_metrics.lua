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
    metrics = { timing_complete = true, blocked_duration_ms = 3000, tool_duration_ms = 3000, request_count = 1,
      reported_output_tokens = 4200, reported_response_ms = 3000 },
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
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1400 tps │ 1 failed")
  local baseline = renderer.build({ { kind = "exchange", exchange = exchange } }, { expanded = {} })
  local reported = "--- a/lib.rs\n+++ b/lib.rs\n@@ -0,0 +1 @@\n+reported\n"
  local checkpoint = "--- a/generated.rs\n+++ b/generated.rs\n@@ -0,0 +1 @@\n+generated\n"
  for _, state in ipairs({
    { attributed_diff_text = reported },
    { checkpoint_after = "checkpoint", attributed_diff_text = reported, checkpoint_diff_text = reported, attributed_matches_checkpoint = true },
    { checkpoint_after = "checkpoint", attributed_diff_text = reported, checkpoint_diff_text = checkpoint },
    { checkpoint_after = "checkpoint", checkpoint_diff_text = checkpoint },
  }) do
    local saved = vim.tbl_extend("force", vim.deepcopy(exchange), state)
    local rendered = renderer.build({ { kind = "exchange", exchange = saved } }, { expanded = {} })
    assert(vim.deep_equal(rendered.lines, baseline.lines), "checkpoint metadata must not add transcript rows")
    for _, row in pairs(rendered.rows) do
      assert(row.kind ~= "checkpoint_changes" and row.kind ~= "attributed_changes", "aggregate change actions must stay out of the transcript")
    end
  end
  exchange.metrics.tool_duration_ms = 439
  exchange.metrics.reported_response_ms = 5561
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 439 ms │ 76.0k (90%) -> 4.2k · 755 tps │ 1 failed")
  exchange.metrics.tool_duration_ms = 2500
  exchange.metrics.reported_response_ms = 3500
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 2.5 s │ 76.0k (90%) -> 4.2k · 1200 tps │ 1 failed")
  exchange.metrics.tool_duration_ms = 3000
  exchange.metrics.reported_response_ms = 3000
  exchange.duration_ms = 6500
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1400 tps │ 1 failed")
  exchange.duration_ms = 3500
  assert_equals(summary(), "▸ Thought 3s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1400 tps │ 1 failed")
  exchange.duration_ms = 3000
  assert_equals(summary(), "▸ Thought 3s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1400 tps │ 1 failed")
  exchange.duration_ms = 6000
  exchange.turn[1].usage.output = 4251
  exchange.metrics.reported_output_tokens = 4251
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.3k · 1417 tps │ 1 failed")
  exchange.turn[1].usage.output = 4200
  exchange.turn[2] = { id = "second", usage = { input = 4000, cached_input = 0, reasoning = 0, output = 100 }, tool = { order = {}, item = {} } }
  exchange.metrics.request_count = 2
  exchange.metrics.reported_output_tokens = 4300
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 3 s │ 80.0k (86%) -> 4.3k · 1433 tps │ 1 failed")
  exchange.turn[2].usage.reasoning = vim.NIL
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools · 3 s │ 80.0k (86%) -> 4.3k · 1433 tps │ 1 failed")
  exchange.turn[2].usage = vim.NIL
  exchange.metrics.timing_complete = false
  assert_equals(summary(), "▸ Thought 6s │ 0/1 tools │ 76.0k (90%) -> 4.2k │ 1 failed")
  exchange.state = "running"
  exchange.completed_at_ms = vim.NIL
  exchange.execution_started_at_ms = 10000
  exchange.metrics.timing_complete = true
  exchange.metrics.blocked_started_ms = 6000
  exchange.metrics.tool_started_ms = 6000
  assert_equals(summary({ now_ms = 12000 }), "▾ Working 8s │ 0/1 tools · 5 s │ 76.0k (90%) -> 4.2k · 1433 tps │ 1 failed")
  exchange.execution_started_at_ms = vim.NIL
  exchange.turn[#exchange.turn].state = { kind = "finished", outcome = "interrupted" }
  assert_equals(summary({ now_ms = 90000 }), "▾ Paused 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1433 tps │ 1 failed")
  exchange.kind = "plan_draft"
  for _, now_ms in ipairs({ 90000, 900000 }) do
    assert_equals(summary({ now_ms = now_ms }), "▾ Planning paused 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1433 tps │ 1 failed")
  end
  exchange.execution_started_at_ms = 900000
  assert_equals(summary({ now_ms = 902000 }), "▾ Planning 8s │ 0/1 tools · 5 s │ 76.0k (90%) -> 4.2k · 1433 tps │ 1 failed")
  exchange.kind, exchange.execution_started_at_ms = "chat", vim.NIL
  exchange.metrics.request_count = 0
  assert_equals(summary({ now_ms = 90000 }), "▾ Paused 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1433 tps │ 1 failed")
  exchange.metrics = { timing_complete = true, request_count = 0 }
  exchange.turn = {}
  assert_equals(summary(), "▾ Working 6s")
  exchange.metrics.request_count = 1
  assert_equals(summary(), "▾ Working 6s")
  exchange.turn = { { id = "partial", usage = { input = 1000, output = 100 } } }
  assert_equals(summary(), "▾ Working 6s │ 1.0k -> 100")
  exchange.turn[1].usage = { reasoning = 23 }
  assert_equals(summary(), "▾ Working 6s")
  exchange.turn[1].usage = { input = 1000, cached_input = 0, reasoning = 0, output = 0 }
  exchange.metrics.reported_output_tokens = 0
  exchange.metrics.reported_response_ms = 2000
  assert_equals(summary(), "▾ Working 6s │ 1.0k (0%) -> 0")
  exchange.turn = {}
  exchange.metrics = { request_count = 0 }
  exchange.node_list = { { kind = "agent_reference", agent = { child_agent_id = "child" } } }
  assert_equals(summary(), "▾ Working 6s")
  exchange.kind, exchange.state = "plan_draft", "complete"
  exchange.duration_ms, exchange.completed_at_ms = 44000, 44000
  exchange.node_list = {}
  exchange.metrics = { timing_complete = true, tool_duration_ms = 4000,
    reported_output_tokens = 7906, reported_response_ms = 40132 }
  local calls = { order = {}, item = {} }
  for index = 1, 20 do
    local id = tostring(index)
    calls.order[index] = id
    calls.item[id] = { id = id, status = index == 20 and "failed" or "completed", failed = index == 20 }
  end
  exchange.turn = { { id = "compact", usage = { input = 855100, output = 7906, reasoning = 906 }, tool = calls } }
  assert_equals(summary(), "▸ Planned 44s │ 19/20 tools · 4 s │ 855.1k -> 7.9k · 197 tps │ 1 failed")
  calls.item["1"].status = "running"
  assert(summary():find("18/20 tools", 1, true), "running tools must not count as passed")
  calls.item["1"].status = "cancelled"
  assert(summary():find("18/20 tools", 1, true), "cancelled tools must not count as passed")
  exchange.kind, exchange.duration_ms = "chat", 31000
  exchange.metrics = { timing_complete = true, tool_duration_ms = 29,
    reported_output_tokens = 846, reported_response_ms = 9097 }
  exchange.turn = { { id = "example", usage = { input = 77000, cached_input = 49280, output = 846 },
    tool = { order = { "first", "second" }, item = {
      first = { id = "first", status = "completed" }, second = { id = "second", status = "completed" },
    } },
  } }
  assert_equals(summary(), "▸ Thought 31s │ 2/2 tools · 29 ms │ 77.0k (64%) -> 846 · 93 tps")
  local first, second = exchange.turn[1].tool.item.first, exchange.turn[1].tool.item.second
  first.kind, second.kind = "file_change", "file_change"
  first.change = { file = { { path = "src\\main.rs" } } }
  second.change = { file = { { path = "src/main.rs" } } }
  assert(summary():sub(-#"│ 1 file changed") == "│ 1 file changed", "repeated file edits were counted twice")
  second.change.file[2] = { path = "src/other.rs" }
  assert(summary():sub(-#"│ 2 files changed") == "│ 2 files changed", "distinct file changes were omitted")
  second.status, second.failed = "failed", false
  assert(summary():sub(-#"│ 1 failed · 1 file changed") == "│ 1 failed · 1 file changed",
    "a failed status without the failure flag was lost or counted as a successful edit")
  second.status, second.failed = "completed", true
  assert(summary():sub(-#"│ 1 failed · 1 file changed") == "│ 1 failed · 1 file changed",
    "reported tool failure was lost")
  second.status, second.failed = "running", false
  assert(summary():sub(-#"│ 1 file changed") == "│ 1 file changed", "running work counted as failure or changed files")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("Harness summary metrics passed")
  vim.cmd("qa!")
end
