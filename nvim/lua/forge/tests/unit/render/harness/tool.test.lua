local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["stopped_tools_use_their_own_marker_and_are_not_failures"] = function()
  local tool = require("forge.render.harness.tool")
  local marker = {}
  for _, status in ipairs({ "completed", "failed", "denied", "cancelled", "interrupted" }) do
    marker[status] = tool.foldtext_chunks({ kind = "file_change", title = "Edit", status = status })[1][2]
  end
  eq(marker, {
    completed = "ForgeHarnessToolSuccess",
    failed = "ForgeHarnessToolFailure",
    denied = "ForgeHarnessToolDenied",
    cancelled = "ForgeHarnessToolInterrupted",
    interrupted = "ForgeHarnessToolInterrupted",
  })
  eq({ tool.failed({ status = "cancelled" }), tool.failed({ status = "interrupted" }), tool.failed({ status = "failed" }) },
    { false, false, true })
end

return T
