local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local buffer = require("forge.buffer")
local hint = require("forge.views.harness.status_hint")
require("forge.infra.highlights").setup()
client.subscribe = function() return function() end end
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
  require("forge.views.harness.layout").open("question-resume-manual")
state.session = { id = "question-resume-manual", model = "fixture", mode = "plan" }
state.active_elicitation = {
  owner = "plan", plan_id = "fixture-plan",
  elicitation = {
    question_set = { id = "fixture-question", questions = {
      { id = "scope", header = "Migration scope", question = "What should the replacement demonstrate?",
        options = {
          { label = "Rust CLI", description = "Replace the TypeScript demo." },
          { label = "TypeScript", description = "Redesign the existing demo." },
        }, allow_freeform = true },
    } },
    answer = {}, current_index = 0,
  },
}
local transcript = buffer.open("question-resume-status", {})
state.transcript_buf = transcript.buffer
vim.api.nvim_win_set_buf(state.transcript_win, transcript.buffer)
local revision = -1
local explanation = false
controller.render = function()
  revision = revision + 1
  local text = {
    "▸ Plan out a new one",
    explanation and "↳ Choose a Rust CLI replacement or a redesigned TypeScript demo." or "↳ The migration scope determines the implementation.",
    "", state.busy and "Working (1s)" or "Awaiting input",
  }
  assert(buffer.apply_snapshot(transcript, {
    document = transcript.document, revision = revision,
    block = { { id = "status", text = text, metadata = {
      target = { { id = state.busy and "status:working" or "status:question", range = {
        start = { row = 3, column = 0 }, ["end"] = { row = 4, column = 0 },
      } } }, decoration = {}, fold = {}, editable_region = {},
    } } },
  }).kind == "Applied")
  hint.render(transcript, state.command_set, 100)
end
client.request_for = function(_, method, _, callback)
  if method == "question.ask" then
    vim.defer_fn(function() explanation = true callback({}) end, 3000)
  elseif method == "state.get" then
    callback({ session = state.session, active_elicitation = state.active_elicitation })
  elseif method == "history.record" then callback({})
  else error("Unexpected fixture request: " .. method) end
end
controller.attach()
controller.render()
controller.present_plan_question(true)
