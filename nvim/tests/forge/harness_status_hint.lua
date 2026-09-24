vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local config = require("forge.infra.config")
local commands = require("forge.shared.view_command_set")
local hint = require("forge.views.harness.status_hint")
local transcript = buffer.open("status-hint", {})
local command_set = commands.new()
commands.register(command_set, "open_artifact", function() end)
commands.register(command_set, "cancel", function() end)
local original = vim.deepcopy(config.options.keymaps.harness.open_artifact)
local original_cancel = vim.deepcopy(config.options.keymaps.harness.cancel)
local namespace = vim.api.nvim_create_namespace("ForgeHarnessStatusHint")
local function displayed()
  hint.render(transcript, command_set, 120)
  local marks = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
  if #marks == 0 then return "" end
  local text = {}
  for _, chunk in ipairs(marks[1][4].virt_text) do text[#text + 1] = chunk[1] end
  return table.concat(text)
end
assert(buffer.apply_snapshot(transcript, {
  document = transcript.document, revision = 0, block = { {
    id = "status", text = { "", "Awaiting plan review · revision 1" },
    metadata = { target = { { id = "status:review-plan", range = {
      start = { row = 1, column = 0 }, ["end"] = { row = 2, column = 0 },
    } } }, decoration = {}, fold = {}, editable_region = {} },
  } },
}).kind == "Applied")
assert(displayed():find("op open plan", 1, true))
config.options.keymaps.harness.open_artifact = "<F6>"
assert(displayed():find("<F6> open plan", 1, true))
config.options.keymaps.harness.open_artifact = false
assert(displayed() == "", "disabled binding must not be advertised")
config.options.keymaps.harness.open_artifact = original
commands.register(command_set, "abort_plan", function() end)
assert(displayed():find("or abort plan", 1, true), "review status omitted the configured abort hint")
local review_hint = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
for _, chunk in ipairs(review_hint[1][4].virt_text) do
  assert(chunk[2] ~= "ForgeHarnessPlan", "review hints inherited the purple status text")
end
assert(buffer.apply_snapshot(transcript, {
  document = transcript.document, revision = 1, block = { {
    id = "status", text = { "", "Working (1s)" },
    metadata = { target = {}, decoration = {}, fold = {}, editable_region = {} },
  } },
}).kind == "Applied")
assert(displayed() == "", "review hint must disappear when the status changes")
assert(buffer.apply_snapshot(transcript, {
  document = transcript.document, revision = 2, block = { {
    id = "status", text = { "", "Working (2s · Inspecting repository structure)" },
    metadata = { target = { { id = "status:working", range = {
      start = { row = 1, column = 0 }, ["end"] = { row = 2, column = 0 },
    } } }, decoration = {}, fold = {}, editable_region = {} },
  } },
}).kind == "Applied")
hint.render(transcript, command_set, 120)
local function working_marks()
  local marks = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
  local text = {}
  for _, mark in ipairs(marks) do
    assert(mark[4].virt_text_pos == "inline")
    for _, chunk in ipairs(mark[4].virt_text) do text[#text + 1] = chunk[1] end
  end
  return table.concat(text), marks
end
local text, marks = working_marks()
assert(text:find("<C-c> to interrupt", 1, true), text)
assert(#marks == 2 and marks[1][2] == 1 and marks[1][3] == 0)
assert(marks[2][3] == #"Working (2s · Inspecting repository structure)" - 1)
local first = marks[1][4].virt_text[1][1]
local state = require("forge.session").harness
local original_session = state.session
for _, mode in ipairs({ "read", "write", "full", "yolo", "plan" }) do
  state.session = { mode = mode }
  hint.render(transcript, command_set, 120)
  local _, colored = working_marks()
  local capture = require("forge.infra.highlights").harness_mode(mode)
  assert(colored[1][4].hl_group == capture, "Working text has the wrong mode color")
  assert(colored[1][4].virt_text[1][2] == capture, "spinner differs from Working text")
end
state.session = original_session
assert(vim.wait(500, function()
  local _, current = working_marks()
  return current[1][4].virt_text[1][1] ~= first
end, 20), "spinner did not animate")
config.options.keymaps.harness.cancel = "<F7>"
hint.render(transcript, command_set, 120)
assert(working_marks():find("<F7> to interrupt", 1, true))
config.options.keymaps.harness.cancel = false
hint.render(transcript, command_set, 120)
local _, disabled = working_marks()
assert(#disabled == 1, "disabled interrupt binding should leave only the spinner")
config.options.keymaps.harness.cancel = original_cancel
transcript.background_terminals = { supported = true, terminal = { { id = "1" }, { id = "2" } } }
hint.render(transcript, command_set, 120)
assert(working_marks():find("2 background terminals", 1, true))
transcript.recap = { text = "We fixed terminal counts." }
hint.render(transcript, command_set, 120)
local during_work = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
assert(#during_work == 3, "recap replaced the working spinner or interrupt hint")
transcript.recap = nil
hint.render(transcript, command_set, 120)
transcript.rename_status = "Generating session name…"
hint.render(transcript, command_set, 120)
local naming = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
assert(#naming == 3, "naming replaced the main spinner or interrupt hint")
local footer = vim.api.nvim_buf_get_extmark_by_id(transcript.buffer, namespace, 3, { details = true })
assert(footer[3].virt_lines[1][1][1] == "Generating session name…")
assert(footer[1] == 1, "naming status was not placed below Working")
transcript.rename_status = nil
hint.render(transcript, command_set, 120)
assert(vim.api.nvim_buf_get_lines(transcript.buffer, 1, 2, false)[1] == "Working (2s · Inspecting repository structure)",
  "terminal status modified the exchange clock")
assert(buffer.apply_snapshot(transcript, {
  document = transcript.document, revision = 3, block = { {
    id = "finished", text = { "Thought for 2s", "Finished response" },
    metadata = { target = {}, decoration = {}, fold = {}, editable_region = {} },
  } },
}).kind == "Applied")
hint.render(transcript, command_set, 120)
local idle = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
assert(#idle == 1 and idle[1][4].virt_lines[2][1][1] == "2 background terminals running")
transcript.background_terminals = { supported = true, terminal = {} }
hint.render(transcript, command_set, 120)
assert(#vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, {}) == 0)
transcript.rename_status = "Generating session name…"
hint.render(transcript, command_set, 120)
local idle_naming = vim.api.nvim_buf_get_extmark_by_id(transcript.buffer, namespace, 3, { details = true })
assert(idle_naming[3].virt_lines[1][1][1] == "Generating session name…", "idle session hid naming progress")
transcript.rename_status = nil
hint.render(transcript, command_set, 120)
assert(vim.api.nvim_buf_get_lines(transcript.buffer, 0, 1, false)[1] == "Thought for 2s")
transcript.recap = { text = "We fixed background terminal rendering and added the picker. Next we will validate provider cleanup." }
hint.render(transcript, command_set, 45)
local recap_marks = vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, { details = true })
local recap_lines = recap_marks[1][4].virt_lines
assert(recap_lines[2][1][1] == "Recap: " and recap_lines[2][2][2] == "ForgeHarnessRecap")
require("forge.infra.highlights").setup()
assert(vim.api.nvim_get_hl(0, { name = "ForgeHarnessRecap", link = false }).italic, "recap highlight lost italics")
for _, line in ipairs(recap_lines) do
  local rendered = ""
  for _, chunk in ipairs(line) do rendered = rendered .. chunk[1] end
  assert(vim.fn.strdisplaywidth(rendered) <= 45, "recap overflowed the window")
end
assert(vim.api.nvim_buf_line_count(transcript.buffer) == 2, "recap became durable timeline text")
transcript.recap = nil
hint.render(transcript, command_set, 45)
assert(#vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, {}) == 0)
hint.clear(transcript.buffer)
vim.wait(150, function() return false end, 25)
assert(#vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, {}) == 0,
  "closed presentation retained its animation")
buffer.close(transcript)
local question_transcript = buffer.open("question-status-hint", {})
commands.register(command_set, "reopen_question", function() end)
assert(buffer.apply_snapshot(question_transcript, {
  document = question_transcript.document, revision = 0, block = { {
    id = "status", text = { "", "Awaiting input" },
    metadata = { target = { { id = "status:question", range = {
      start = { row = 1, column = 0 }, ["end"] = { row = 2, column = 0 },
    } } }, decoration = {}, fold = {}, editable_region = {} },
  } },
}).kind == "Applied")
local original_question_key = config.options.keymaps.harness.reopen_question
for _, key in ipairs({ "oe", "<F8>" }) do
  config.options.keymaps.harness.reopen_question = key
  hint.render(question_transcript, command_set, 120)
  local mark = vim.api.nvim_buf_get_extmark_by_id(question_transcript.buffer, namespace, 2, { details = true })
  local chunks = {}
  for _, chunk in ipairs(mark[3].virt_text) do chunks[#chunks + 1] = chunk[1] end
  assert(table.concat(chunks):find(key .. " open question", 1, true), "question hint ignored configured binding")
  assert(mark[1] == 1 and mark[3].virt_text_pos == "eol", "question hint left the waiting row")
end
config.options.keymaps.harness.reopen_question = false
hint.render(question_transcript, command_set, 120)
assert(#vim.api.nvim_buf_get_extmarks(question_transcript.buffer, namespace, 0, -1, {}) == 0)
config.options.keymaps.harness.reopen_question = original_question_key
hint.clear(question_transcript.buffer)
buffer.close(question_transcript)
print("harness status hint passed")
vim.cmd("qa!")
