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

T["info_filters_aggregates_and_redacts"] = function()
  local perf = require("forge.infra.perf")
  local now = 0
  perf.now = function() return now end
  perf.setup({ harness = { info = true } })
  perf.flush_pending.harness = true
  perf.diagnostic("unknown", { elapsed_ms = 900 })
  perf.diagnostic("ui.render", { elapsed_ms = 99 })
  eq(perf.queue.harness, nil)
  perf.diagnostic("ui.render", { elapsed_ms = 100, text = "private draft", session_id = "session" })
  perf.diagnostic("ui.render", { elapsed_ms = 900 })
  eq(#perf.queue.harness, 1)
  now = 10e9
  perf.diagnostic("ui.render", { elapsed_ms = 150 })
  local first, second = vim.json.decode(perf.queue.harness[1]), vim.json.decode(perf.queue.harness[2])
  eq(first.level, "info")
  eq(first.text, nil)
  eq(first.session_id, "session")
  eq(second.suppressed, 1)
  eq(second.maximum_ms, 900)
  perf.setup({ harness = { info = false } })
  perf.diagnostic("ui.queue.recovered", { reason = "entry_missing" })
  eq(#perf.queue.harness, 2)
end

T["watcher_follows_view_and_level_lifecycle"] = function()
  local perf = require("forge.infra.perf")
  local active, callback = 0, nil
  vim.uv.new_timer = function()
    return { start = function(_, _, _, run) active = active + 1 callback = run end,
      stop = function() active = active - 1 end, close = function() end }
  end
  perf.setup({ harness = { info = true } })
  eq(active, 0)
  local buf = vim.api.nvim_get_current_buf()
  perf.watch_view(buf, "session")
  eq(active, 1)
  perf.setup({ harness = { info = false, enabled = false } })
  eq(active, 0)
  perf.setup({ harness = { info = true } })
  eq(active, 1)
  vim.api.nvim_set_current_buf(vim.api.nvim_create_buf(false, true))
  vim.wait(20, function() return active == 0 end)
  eq(active, 0)
  perf.setup({ harness = { info = false } })
end

T["byte_budget_is_bounded"] = function()
  local perf = require("forge.infra.perf")
  perf.setup({ harness = { info = true } })
  perf.flush_pending.harness = true
  local encoded = vim.json.encode
  vim.json.encode = function() return string.rep("x", 600000) end
  eq(perf.event("harness", "ui.render", {}, true), "queued")
  eq(perf.event("harness", "ui.render", {}, true), "saturated")
  vim.json.encode = encoded
  eq(#perf.queue.harness, 1)
  eq(perf.rejected_record_count.harness, 1)
end

return T
