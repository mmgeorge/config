local buffer = require("forge.buffer")
local folds = require("forge.folds")
local controller = require("forge.views.harness.controller")
local state = require("forge.session").harness
local client = require("forge.client")
require("forge.infra.highlights").setup()
local blocks = vim.json.decode(table.concat(vim.fn.readfile(assert(vim.g.question_fixture)), "\n"))
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
  require("forge.views.harness.layout").open("question-projection")
local transcript = buffer.open("question-projection", {})
state.transcript_buf = transcript.buffer
vim.api.nvim_win_set_buf(state.transcript_win, transcript.buffer)
vim.api.nvim_set_current_win(state.transcript_win)
state.session = { id = "question-projection", backend = "fixture", mode = "plan" }
state.active_elicitation = { owner = "interaction", exchange_id = "question-flow", elicitation = {
  question_set = { id = "scope-set", questions = {
    { id = "scope", header = "Migration scope", question = "What should the replacement demonstrate?",
      options = { { label = "Rust CLI", description = "Replace the demo" } }, allow_freeform = true },
  } }, answer = {}, current_index = 0,
} }
blocks[#blocks + 1] = { id = "status", text = { "", "Awaiting input" }, metadata = {
  target = { { id = "status:question", range = { start = { row = 1, column = 0 }, ["end"] = { row = 2, column = 0 } } } },
  decoration = {}, fold = {}, editable_region = {},
} }
assert(buffer.apply_snapshot(transcript, { document = transcript.document, revision = 0, block = blocks }).kind == "Applied")
state.presentation = {
  transcript = transcript,
  activate = function(callback)
    local cursor = vim.api.nvim_win_get_cursor(state.transcript_win)
    local location = buffer.locate(transcript, cursor[1] - 1, cursor[2])
    if location and location.target == "presented" then callback({ kind = "question", question_set_id = "scope-set" }) end
  end,
  open_output = function() return false end,
  toggle_tool = function() return false end,
}
client.subscribe = function() return function() end end
controller.render = function() end
controller.attach()
folds.attach(transcript, state.transcript_win)
require("forge.views.harness.status_hint").render(transcript, state.command_set, 100)
vim.fn.search("Question presented", "w")
