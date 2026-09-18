vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local config = require("forge.infra.config")
local commands = require("forge.shared.view_command_set")
local hint = require("forge.views.harness.status_hint")
local transcript = buffer.open("status-hint", {})
local command_set = commands.new()
commands.register(command_set, "open_timeline", function() end)
commands.register(command_set, "cancel", function() end)
local original = vim.deepcopy(config.options.keymaps.harness.open_timeline)
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
assert(displayed():find("<CR> open plan", 1, true))
config.options.keymaps.harness.open_timeline = "<F6>"
assert(displayed():find("<F6> open plan", 1, true))
config.options.keymaps.harness.open_timeline = false
assert(displayed() == "", "disabled binding must not be advertised")
config.options.keymaps.harness.open_timeline = original
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
hint.clear(transcript.buffer)
vim.wait(150, function() return false end, 25)
assert(#vim.api.nvim_buf_get_extmarks(transcript.buffer, namespace, 0, -1, {}) == 0,
  "closed presentation retained its animation")
buffer.close(transcript)
print("harness status hint passed")
vim.cmd("qa!")
