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

local configured = { id = "timeout-settings", backend = "mock", approval_timeout = { amount = 10, unit = "m" }, answer_timeout = { amount = 10, unit = "m" } }
local state = { session = configured }
local original_request, original_configure, original_input = client.request_for, controller.configure, vim.ui.input
client.request_for = function(_, method, _, callback)
  assert(method == "trace.status")
  callback({ enabled = false })
end
controller.configure = function(value, _, completed)
  for key, entry in pairs(value) do configured[key] = entry end
  completed(true)
end
settings.open(state, host)
instance = picker._state_for_test()
local option
for index, candidate in ipairs(instance.spec.page_list[1].option_list) do
  if candidate.id == "approval_timeout" then
    instance.state.selected_index_by_page.config = index
    option = candidate
  end
end
assert(option and option.columns[2]:find("10", 1, true))
instance.spec.action_list[2].callback({ option = option })
assert(configured.approval_timeout.amount == 11)
instance = picker._state_for_test()
instance.spec.action_list[3].callback({ option = option })
instance = picker._state_for_test()
instance.spec.action_list[2].callback({ option = option })
assert(configured.approval_timeout.unit == "h" and configured.approval_timeout.amount == 11)
instance = picker._state_for_test()
instance.spec.action_list[3].callback({ option = option })
vim.ui.input = function(_, callback) callback("0") end
instance = picker._state_for_test()
instance.spec.on_confirm({ option = option })
assert(configured.approval_timeout.amount == 0)
assert(configured.answer_timeout.amount == 10 and configured.answer_timeout.unit == "m")
assert(duration.render(configured.approval_timeout, "amount"):find("Off", 1, true))
picker.close(false)
client.request_for, controller.configure, vim.ui.input = original_request, original_configure, original_input
print("HARNESS_INPUT_TIMEOUT_PASS")
