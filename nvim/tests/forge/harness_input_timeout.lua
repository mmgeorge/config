vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
vim.o.columns = 130
require("forge").setup({ harness = { backend = "mock" } })
local picker = require("forge.views.picker")
local approval = require("forge.views.harness.approval")
local settings = require("forge.views.harness.settings")
local controller = require("forge.views.harness.controller")
local client = require("forge.client")
local duration = require("forge.views.harness.duration")
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win(), closed = function() end }
local seconds, microseconds = vim.uv.gettimeofday()
local deadline = seconds * 1000 + microseconds / 1000 + 62000
approval.open({ id = "timeout-request", deadline = { expires_at_ms = deadline }, item_list = {
  { id = "item", title = "Read", detail = "Inspect source", choice_list = { { id = "deny_once", label = "Deny once" } } },
} }, vim.tbl_extend("force", host, { resolve = function() error("UI must not resolve the deadline") end }))
local instance = picker._state_for_test()
local before = vim.api.nvim_buf_get_changedtick(instance.buf)
local cursor = vim.api.nvim_win_get_cursor(instance.win)
local function title()
  local chunks = vim.api.nvim_win_get_config(instance.win).title
  local text = {}
  for _, chunk in ipairs(chunks) do text[#text + 1] = chunk[1] end
  return table.concat(text)
end
assert(title():find("01:02", 1, true), title())
assert(vim.wait(1700, function() return title():find("01:01", 1, true) ~= nil end, 20), title())
assert(vim.api.nvim_buf_get_changedtick(instance.buf) == before, "countdown rewrote the body")
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(instance.win), cursor), "countdown moved cursor")
local timer = instance.timer
picker.close(false)
assert(timer:is_closing(), "picker timer leaked")

local configured = { id = "timeout-settings", backend = "mock", approval_timeout = { amount = 10, unit = "m" } }
local state = { session = configured }
local original_request, original_configure = client.request, controller.configure
local saved = 0
local deferred, pending_save
client.request = function(request_specification, callback)
  local method = request_specification.method
  if method == "host.trace.status" then callback({ enabled = false })
  elseif method == "session.provider.model.list" then callback({})
  elseif method == "background.settings" then callback({ revision = 0, settings = request_specification.params.defaults })
  else error(method) end
end
controller.configure = function(value, _, completed)
  saved = saved + 1
  local function complete(applied)
    if applied then for key, entry in pairs(value) do configured[key] = entry end end
    completed(applied)
  end
  if deferred then pending_save = complete else complete(true) end
end
local function press(buffer, key)
  local mapping = vim.api.nvim_buf_call(buffer, function() return vim.fn.maparg(key, "n", false, true) end)
  assert(mapping.callback, "missing key " .. key)
  mapping.callback()
end
local function edit(id)
  local current = picker._state_for_test()
  for page_index, page in ipairs(current.spec.page_list) do
    for index, option in ipairs(page.option_list) do
      if option.id == id then
        assert(#option.fields == 1, "timeout uses separate amount and unit fields")
        current.state.page_index = page_index
        current.state.selected_index_by_page[page.id] = index
        picker.update(current.spec)
        press(current.buf, "<CR>")
        return assert(current.field_editor)
      end
    end
  end
  error("missing timeout " .. id)
end
local function shown(editor)
  return vim.api.nvim_buf_get_lines(editor.buffer, 0, 1, false)[1]
end
settings.open(state, host)
local editor = edit("approval_timeout")
assert(shown(editor) == "10m" and not vim.bo[editor.buffer].modifiable)
press(editor.buffer, "<Up>")
assert(shown(editor) == "11m")
press(editor.buffer, "<Down>")
assert(shown(editor) == "10m")
press(editor.buffer, "<Esc>")
assert(saved == 0 and configured.approval_timeout.amount == 10, "cancel saved the draft")
editor = edit("approval_timeout")
for minute = 9, 2, -1 do
  press(editor.buffer, "<Down>")
  assert(shown(editor) == minute .. "m")
end
for _, label in ipairs({ "60s", "45s", "30s", "15s", "15s" }) do
  press(editor.buffer, "<Down>")
  assert(shown(editor) == label, "incorrect lower-bound step: " .. shown(editor))
end
assert(saved == 0, "stepping saved before confirmation")
press(editor.buffer, "<CR>")
assert(saved == 1 and configured.approval_timeout.amount == 15 and configured.approval_timeout.unit == "s")
editor = edit("approval_timeout")
local choices = duration.choices()
assert(#choices == 63)
for index = 2, #choices do
  press(editor.buffer, "<Up>")
  assert(shown(editor) == choices[index], "incorrect increment at " .. choices[index])
end
press(editor.buffer, "<Up>")
assert(shown(editor) == "60m", "maximum wrapped or exceeded 60 minutes")
press(editor.buffer, "<CR>")
assert(saved == 2 and configured.approval_timeout.amount == 60 and configured.approval_timeout.unit == "m")
configured.approval_timeout = { amount = 10, unit = "m" }
settings.open(state, host)
editor = edit("approval_timeout")
press(editor.buffer, "<Down>")
press(editor.buffer, "<CR>")
assert(saved == 3 and configured.approval_timeout.amount == 9 and configured.approval_timeout.unit == "m")
deferred = true
editor = edit("approval_timeout")
press(editor.buffer, "<Down>")
assert(shown(editor) == "8m")
press(editor.buffer, "<CR>")
press(editor.buffer, "<Up>")
press(editor.buffer, "<Esc>")
assert(editor.pending and shown(editor) == "8m" and saved == 4, "pending choice changed or closed")
pending_save(false)
assert(not editor.pending and not vim.bo[editor.buffer].modifiable, "failed choice save enabled free text")
press(editor.buffer, "<Down>")
assert(shown(editor) == "7m")
press(editor.buffer, "<CR>")
pending_save(true)
assert(saved == 5 and configured.approval_timeout.amount == 7 and not picker._state_for_test().field_editor)
deferred = false
editor = edit("tool_timeout")
assert(shown(editor) == "10m", "tool and shell timeout did not default to ten minutes")
press(editor.buffer, "<Up>")
press(editor.buffer, "<CR>")
assert(saved == 6 and configured.tool_timeout.amount == 11 and configured.tool_timeout.unit == "m",
  "tool and shell deadline was not saved through the timeout editor")
for index, choice in ipairs(choices) do
  local value = duration.parse(choice)
  assert(duration.render(value) == choice)
  assert(duration.render(duration.change(value, 1)) == choices[math.min(#choices, index + 1)])
  assert(duration.render(duration.change(value, -1)) == choices[math.max(1, index - 1)])
end
assert(duration.render({ amount = 1, unit = "m" }) == "60s")
assert(duration.render(duration.change({ amount = 0, unit = "s" }, -1)) == "15s")
assert(duration.render(duration.change({ amount = 10, unit = "h" }, 1)) == "60m")
picker.close(false)
client.request, controller.configure = original_request, original_configure
local timeout_configuration = require("forge.views.harness.timeout_configuration")
local timeout_state = { session = { id = "ordered-timeouts", approval_timeout = { amount = 10, unit = "m" },
  status_thoughts = true } }
local requests, completions, failures = {}, {}, {}
client.request = function(request, callback) requests[#requests + 1] = { request = request, callback = callback } end
local function configure_timeout(values)
  timeout_configuration.configure(timeout_state, values, function(applied) completions[#completions + 1] = applied end,
    function(message) failures[#failures + 1] = message end)
end
configure_timeout({ approval_timeout = { amount = 11, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 12, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 14, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 13, unit = "m" } })
assert(#requests == 1, "rapid edits sent concurrent timeout writes")
requests[1].callback({ approval_timeout = { amount = 11, unit = "m" } })
assert(#requests == 2 and requests[2].request.params.approval_timeout.amount == 13
  , "pending edits lost the newest value")
assert(timeout_state.session.approval_timeout.amount == 10, "older response replaced a pending edit")
requests[2].callback({ approval_timeout = { amount = 13, unit = "m" } })
assert(timeout_state.session.approval_timeout.amount == 13 and timeout_state.session.status_thoughts == true)
assert(#completions == 4 and not vim.tbl_contains(completions, false))
configure_timeout({ approval_timeout = { amount = 14, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 15, unit = "m" } })
requests[3].callback(nil, "write failed")
assert(#failures == 1 and completions[5] == false and #requests == 4, "a failed write stranded pending edits")
requests[4].callback({ approval_timeout = { amount = 15, unit = "m" }, status_thoughts = false })
assert(timeout_state.session.approval_timeout.amount == 15 and timeout_state.session.status_thoughts == true,
  "timeout response overwrote another setting")
configure_timeout({ approval_timeout = { amount = 16, unit = "m" } })
configure_timeout({ approval_timeout = { amount = 17, unit = "m" } })
timeout_state.session = { id = "replacement-session", approval_timeout = { amount = 10, unit = "m" } }
requests[5].callback({ approval_timeout = { amount = 16, unit = "m" } })
assert(#requests == 5 and timeout_state.session.approval_timeout.amount == 10,
  "a stale timeout callback wrote to the replacement session")
assert(completions[7] == false and completions[8] == false)
client.request = original_request
print("HARNESS_INPUT_TIMEOUT_PASS")
vim.cmd("qa!")
