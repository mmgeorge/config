local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["task_notice_uses_typed_opening"] = function()
  local opening = require("forge.render.harness.exchange_opening")
  eq(opening.notice({
    kind = "task_started", start = { kind = "goal", title = "Inspect the configuration" },
  }), "Goal started: Inspect the configuration")
  eq(opening.notice({ kind = "task_continuation", reason = "Review the results" }), "Review the results")
  eq(opening.notice({ kind = "user", content = { kind = "user_text", text = "A message" } }), nil)
end

return T
