vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local requests, messages, opened, refreshed = {}, {}, nil, 0
client.host_generation = function() return 1 end
client.request_for = function(session_id, method, params, callback)
  assert(session_id == "background-test" and method == "harness.document")
  requests[#requests + 1] = { params = params, callback = callback }
end
picker.open = function(spec) opened = spec end
notifications.info = function(message) messages[#messages + 1] = message end
notifications.error = notifications.info
state.session = { id = "background-test" }
state.busy, state.queue = true, {}
state.presentation = { terminals = { refresh = function() refreshed = refreshed + 1 end } }
state.composer_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/bg" })
controller.queue_submit()
assert(#requests == 1 and #state.queue == 0 and state.busy, "background picker changed exchange or queue")
assert(requests[1].params.operation == "background_terminals")
requests[1].callback({ supported = true, terminal = {
  { id = "first", command = "cargo\ntest" }, { id = "second", command = "npm run dev" },
} })
assert(opened.page_list[1].option_list[1].label == "cargo test")
assert(#requests == 1, "opening the picker terminated a process")
opened.on_confirm({ option = opened.page_list[1].option_list[2] })
assert(requests[2].params.operation == "terminate_terminal" and requests[2].params.id == "second")
requests[2].callback({})
assert(refreshed == 1 and state.busy, "termination restarted or completed the exchange")
controller.open_background_picker()
requests[3].callback({ supported = true, terminal = {} })
assert(messages[#messages] == "No background terminals running")
controller.open_background_picker()
state.session = { id = "different-session" }
requests[4].callback({ supported = true, terminal = { { id = "stale", command = "old" } } })
opened.on_confirm({ option = { value = "stale" } })
assert(#requests == 4, "stale picker crossed session boundaries")
print("harness_background_picker: passed")
vim.cmd("qa!")
