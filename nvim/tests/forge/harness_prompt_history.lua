vim.loader.enable(false)
local state = require("forge.session").harness
state.session = { id = "history-session", backend = "mock" }
local history = require("forge.views.harness.prompt_history")
local attachment = require("forge.views.harness.attachment")
local snapshot = require("forge.views.harness.snapshot")
local client = require("forge.client")
local controller = require("forge.views.harness.controller")
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
state.composer_buf, state.composer_win = buffer, vim.api.nvim_get_current_win()
attachment.attach(buffer, nil)
history.attach(buffer)
require("forge.shared.keymaps").setup_view_keymaps(buffer, "harness",
  controller.composer_command_set())

---@param text string
---@return ForgePromptDraft
local function draft(text)
  return { text = text, attachments = {} }
end

---@param text string
local function write(text)
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, vim.split(text, "\n", { plain = true }))
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buffer })
end

---@param keys string
local function press(keys)
  vim.api.nvim_feedkeys(vim.keycode(keys), "xt", false)
end

---@param expected string
local function expect(expected)
  assert(attachment.capture(buffer).text == expected, vim.inspect(attachment.capture(buffer)))
end

local original_present_question = controller.present_plan_question
local question_opened = false
controller.present_plan_question = function(force)
  assert(force, "prompt feedback did not explicitly reopen the question")
  question_opened = true
end
state.active_elicitation = { elicitation = {} }
write("preserved question draft")
press("oe")
assert(question_opened, "Normal-mode prompt feedback did not open the pending question")
expect("preserved question draft")
controller.present_plan_question = original_present_question
state.active_elicitation = nil
assert(vim.fn.maparg("oe", "i") == "", "feedback command replaced Insert-mode typing")
assert(vim.fn.maparg("W", "n") == "", "timeline selection replaced Normal-mode word motion")
write("")

history.replace({ draft("newest"), draft("older\nsecond line") })
press("<Up>")
expect("newest")
press("<Up>")
expect("older\nsecond line")
press("<Down><Down>")
expect("")

write("unfinished draft")
press("i<Up><Esc>")
expect("newest")
press("i<Down><Esc>")
expect("unfinished draft")

write("first line\nsecond line")
vim.api.nvim_win_set_cursor(state.composer_win, { 2, 0 })
press("<Up>")
expect("first line\nsecond line")
assert(vim.api.nvim_win_get_cursor(state.composer_win)[1] == 1, "Up did not move within the draft")
press("<Up><Down>")
expect("first line\nsecond line")
press("<Down>")
assert(vim.api.nvim_win_get_cursor(state.composer_win)[1] == 2, "Down did not move within the draft")
press("i<Up><Up><Esc>")
expect("newest")
press("<Down>")
expect("first line\nsecond line")

write("draft across refresh")
vim.api.nvim_win_set_cursor(state.composer_win, { 1, 0 })
press("<Up>")
snapshot.apply(state, { session = state.session, prompt_history = { draft("newest"), draft("older\nsecond line") } })
press("<Up>")
expect("older\nsecond line")
history.replace({ draft("added elsewhere"), draft("newest"), draft("older\nsecond line") })
press("<Down><Down><Down>")
expect("draft across refresh")

history.replace({ draft("duplicate"), draft("between"), draft("duplicate") })
write("")
press("<Up><Up><Up>")
history.replace({ draft("new entry"), draft("duplicate"), draft("between"), draft("duplicate") })
press("<Down>")
expect("between")

local response
client.request = function(request_specification, callback)
  local method = request_specification.method

  assert(method == "session.prompt.history.record")
  assert(request_specification.session_id == "history-session", "history lost its owning session")
  response = callback
end
history.record("submitted")
write("")
press("<Up><Up>")
expect("new entry")
response({ draft("submitted"), draft("new entry"), draft("duplicate"), draft("between"), draft("duplicate") })
press("<Down><Down>")
expect("")

write("[image]")
local image = { id = "draft-attachment", label = "[image]", name = "image.png", media_type = "image/png", start = 0, ["end"] = 7 }
attachment.restore(buffer, { image })
press("<Up><Down>")
assert(vim.deep_equal(attachment.capture(buffer), { text = "[image]", attachments = { image } }),
  "history navigation discarded draft attachments")

write("edited recall")
press("<Up>")
write("edited submitted")
press("<Down>")
expect("edited submitted")

write("")
press("<Up>")
local other = vim.deepcopy(state)
other.composer_buf = vim.api.nvim_create_buf(false, true)
other.prompt_history_index = 0
other.prompt_history_draft = nil
require("forge.session").activate_harness(other)
history.attach(other.composer_buf)
write("edit in inactive composer")
assert(state.prompt_history_index == 0, "another composer removed the original history-edit listener")
assert(other.prompt_history_index == 0, "inactive composer changed the selected session")
require("forge.session").activate_harness(state)

local prompts = {}
for index = 1, 105 do prompts[index] = draft("history " .. index) end
history.replace(prompts)
write("")
for _ = 1, 105 do press("<Up>") end
expect("history 105")
press("<Up>")
expect("history 105")
print("harness_prompt_history: passed")
