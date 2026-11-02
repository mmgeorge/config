vim.opt.runtimepath:prepend("nvim")
local hint = require("forge.views.harness.status_hint")
local commands = require("forge.shared.view_command_set")
local config = require("forge.infra.config")
local actions = commands.new()
for _, action in ipairs({ "open_artifact", "cancel", "background", "reopen_question", "abort_plan" }) do
  commands.register(actions, action, function() end)
end
local snapshot = { block = { { text = { "Waiting for plan review" }, metadata = {
  status = { row = 0, animated = false, hint = "review" }, decoration = {},
} } } }
local initial = vim.deepcopy(snapshot)
local function displayed(context) return table.concat(hint.compose(snapshot, context or {}, actions).lines, "\n") end
assert(displayed():find("op open plan", 1, true))
assert(displayed():find("or abort plan", 1, true))
local original = config.options.keymaps.harness.open_artifact
config.options.keymaps.harness.open_artifact = "<F6>"
assert(displayed():find("<F6> open plan", 1, true))
config.options.keymaps.harness.open_artifact = false
assert(not displayed():find("open plan", 1, true))
config.options.keymaps.harness.open_artifact = original
local notice = ("Long Unicode λ notice\n"):rep(500) .. "final notice"
assert(displayed({ message_notice = { text = notice, failed = true } }):find("final notice", 1, true))
assert(vim.deep_equal(snapshot, initial), "local notices mutated the native snapshot")
assert(displayed({ rename_status = "Renaming" }):find("Renaming", 1, true))
assert(displayed({ restore_recovery = {} }):find("Restore interrupted", 1, true))
assert(displayed({ background_terminals = { supported = true, terminal = { {} } } }):find("1 terminal running", 1, true))
assert(displayed({ recap = { text = "complete recap" } }):find("complete recap", 1, true))
local empty = hint.compose({ block = {} }, {}, actions)
assert(#empty.lines == 0)
assert(#hint.compose({ block = {} }, { status_notice = { text = "Disconnected" } }, actions).lines == 1)

-- Pending questions branch directly beneath the status, ahead of thoughts and checks.
local question_context = { questions = { revision = 1, questions = {
  { content = { question = "Which behavior should the new function provide?" }, resolution = { kind = "pending" } },
  { content = { question = "Already answered" }, resolution = { kind = "answered" } },
  { content = { question = "Should empty input use a default value or return an empty result?" }, resolution = { kind = "pending" } },
} } }
local original_questions = vim.deepcopy(question_context)
local working = { block = { { text = { "Working · 24s", "Current thought" }, metadata = {
  status = { row = 0, animated = true, hint = "working" },
  decoration = { { range = { start = { row = 1 }, ["end"] = { row = 2 } }, capture = "ForgeHarnessThought" } },
} } } }
local branched = hint.compose(working, question_context, actions, 35)
assert(branched.lines[1] == "  Working · 24s · <C-c> interrupt · oe answer questions", branched.lines[1])
assert(branched.lines[2]:find("  ├─ Which", 1, true) == 1)
assert(branched.lines[3]:find("  │  ", 1, true) == 1)
assert(branched.lines[4]:find("  └─ Should", 1, true) == 1)
assert(branched.lines[5]:find("     ", 1, true) == 1)
assert(branched.lines[5]:sub(-#"…") == "…", "clipped preview lacks an ellipsis")
assert(branched.lines[6] == "  Current thought")
assert(branched.animated == 0 and branched.highlights[6].row == 5)
assert(branched.highlights[6].capture == "ForgeHarnessThought", "preview displaced native decorations")
for index = 2, 5 do assert(vim.fn.strdisplaywidth(branched.lines[index]) <= 35) end
assert(vim.deep_equal(question_context, original_questions), "preview changed the full question")

local question_key = config.options.keymaps.harness.reopen_question
config.options.keymaps.harness.reopen_question = "<F7>"
assert(hint.compose(working, question_context, actions, 35).lines[1]:find("<F7> answer questions", 1, true))
config.options.keymaps.harness.reopen_question = false
assert(not hint.compose(working, question_context, actions, 35).lines[1]:find("answer questions", 1, true))
config.options.keymaps.harness.reopen_question = question_key

local waiting = vim.deepcopy(working)
waiting.block[1].metadata.status = { row = 0, animated = false, hint = "question" }
local waiting_lines = hint.compose(waiting, question_context, actions, 80).lines
assert(select(2, waiting_lines[1]:gsub("answer questions", "")) == 1, "required question duplicated the action")
local idle_questions = hint.compose({ block = {} }, question_context, actions, 80)
assert(idle_questions.lines[1] == "  Questions · oe answer questions")
assert(idle_questions.lines[2]:find("  ├─ ", 1, true) == 1)
assert(idle_questions.lines[3]:find("  └─ ", 1, true) == 1)
for _, question in ipairs(question_context.questions.questions) do question.resolution.kind = "answered" end
assert(#hint.compose({ block = {} }, question_context, actions, 80).lines == 0)
assert(#hint.compose(working, question_context, actions, 80).lines == 2)

-- Wide characters and unbroken text still fit two physical display lines.
question_context.questions.questions = {
  { content = { question = ("界🙂"):rep(30) }, resolution = { kind = "pending" } },
}
local unicode_preview = hint.compose({ block = {} }, question_context, actions, 24)
assert(#unicode_preview.lines == 3)
assert(unicode_preview.lines[3]:find("     ", 1, true) == 1)
assert(unicode_preview.lines[3]:sub(-#"…") == "…")
for index = 2, 3 do assert(vim.fn.strdisplaywidth(unicode_preview.lines[index]) <= 24) end
print("harness_status_hint: passed")
