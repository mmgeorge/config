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


T["agent_thought_stays_on_its_row_and_clips_to_width"] = function()
  local commands = require("forge.shared.view_command_set").new()
  local hint = require("forge.views.harness.status_hint")
  local row = "└─ ⠋ 4s explorer │ 2/2 tools · 1.0 s │ Reading 界界界 the admission queue"
  local column = #"└─ ⠋ 4s explorer │ 2/2 tools · 1.0 s │ "
  local snapshot = { block = { { text = { "Agents · 1 running", row, "Root preview" }, metadata = {
    status = { row = 0, animated = true },
    decoration = {
      { capture = "ForgeHarnessAgentThought", range = {
        start = { row = 1, column = column }, ["end"] = { row = 1, column = #row },
      } },
      { capture = "ForgeHarnessCommentary", range = {
        start = { row = 2, column = 0 }, ["end"] = { row = 3, column = 0 },
      } },
    },
  } } } }
  local full = hint.compose(snapshot, {}, commands, 200)
  MiniTest.expect.equality(full.lines, {
    "  Agents · 1 running │ Root preview",
    "  " .. row,
  })
  local agent_highlight = vim.iter(full.highlights):find(function(highlight)
    return highlight.capture == "ForgeHarnessAgentThought"
  end)
  MiniTest.expect.equality({ agent_highlight.row, agent_highlight.column, agent_highlight.end_column },
    { 1, column + 2, #full.lines[2] })
  local head = "  └─ ⠋ 4s explorer │ 2/2 tools · 1.0 s │ "
  local width = vim.fn.strdisplaywidth(head .. "Reading 界…")
  local narrow = hint.compose(snapshot, {}, commands, width)
  MiniTest.expect.equality(narrow.lines[2], head .. "Reading 界…")
  MiniTest.expect.equality(vim.fn.strdisplaywidth(narrow.lines[2]), width)
  local cramped = hint.compose(snapshot, {}, commands, vim.fn.strdisplaywidth(head))
  MiniTest.expect.equality(cramped.lines[2], "  └─ ⠋ 4s explorer │ 2/2 tools · 1.0 s")
end

return T
