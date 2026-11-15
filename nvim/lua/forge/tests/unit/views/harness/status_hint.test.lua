local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["thought_uses_space_after_controls"] = function()
  local commands = require("forge.shared.view_command_set").new()
  require("forge.shared.view_command_set").register(commands, "cancel", function() end)
  local hint = require("forge.views.harness.status_hint")
  local snapshot = { block = { { text = { "Working · 42s", "Reviewing 界界界 diagnostics" }, metadata = {
    status = { row = 0, animated = true, hint = "working" },
    decoration = { { capture = "ForgeHarnessCommentary", range = {
      start = { row = 1, column = 0 }, ["end"] = { row = 2, column = 0 },
    } } },
  } } } }
  local full = hint.compose(snapshot, {}, commands, 120)
  MiniTest.expect.equality(full.lines, { "  Working · 42s · <C-c> interrupt │ Reviewing 界界界 diagnostics" })
  local control = "  Working · 42s · <C-c> interrupt"
  local width = vim.fn.strdisplaywidth(control .. " │ Reviewing 界…")
  local narrow = hint.compose(snapshot, {}, commands, width)
  MiniTest.expect.equality(narrow.lines, { control .. " │ Reviewing 界…" })
  MiniTest.expect.equality(vim.fn.strdisplaywidth(narrow.lines[1]), width)
  MiniTest.expect.equality(hint.compose(snapshot, {}, commands, vim.fn.strdisplaywidth(control)).lines, { control })
  MiniTest.expect.equality(full.animated, 0)
  MiniTest.expect.equality(full.highlights[2].capture, "ForgeHarnessCommentary")
  MiniTest.expect.equality(snapshot.block[1].text[2], "Reviewing 界界界 diagnostics")
end

return T
