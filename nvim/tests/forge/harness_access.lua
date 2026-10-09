vim.opt.runtimepath:prepend("nvim")
local access = require("forge.views.harness.access")
local picker = require("forge.views.picker")
local root = vim.fn.tempname()
assert(vim.fn.mkdir(root, "p") == 1)
local nested = root .. "/nested"
assert(vim.fn.mkdir(nested, "p") == 1)
local state = { session = { id = "access", workspace = root, access = {
  sandbox = true, write_access = "workspace", windows_sandbox = "elevated", writable_directory = {},
} } }
local host = { control_win = vim.api.nvim_get_current_win() }
local input_reply, input_spec, failures, applied, returned = nil, nil, {}, 0, 0
vim.ui.input = function(spec, reply) input_spec, input_reply = spec, reply end
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
local function apply(policy, done)
  state.session.access = policy
  applied = applied + 1
  done()
end
local function selected(index)
  local instance = picker._state_for_test()
  instance.spec.on_confirm({ option = instance.spec.page_list[1].option_list[index] })
end
access.directories(state, host, apply, function() returned = returned + 1 end)
selected(1)
assert(input_spec.completion == "dir")
input_reply(root .. "/missing")
assert(#failures == 1 and applied == 0)
selected(1)
input_reply(nested)
assert(applied == 1 and #state.session.access.writable_directory == 1)
local instance = picker._state_for_test()
assert(instance.spec.page_list[1].option_list[2].detail == "Already covered")
instance.spec.action_list[1].callback({ option = instance.spec.page_list[1].option_list[2] })
assert(picker.is_open("harness-directory-remove"))
selected(1)
assert(#state.session.access.writable_directory == 1, "cancel removed the path")
instance = picker._state_for_test()
instance.spec.action_list[1].callback({ option = instance.spec.page_list[1].option_list[2] })
selected(2)
assert(applied == 2 and #state.session.access.writable_directory == 0)
state.session.access.write_access = "full"
picker.close()
assert(returned == 1)
access.directories(state, host, apply, function() end)
assert(picker._state_for_test().spec.page_list[1].subtitle:find("Inactive", 1, true))
selected(1)
state.session = { id = "other" }
input_reply(nested)
assert(applied == 2 and not picker.is_open(), "stale input changed another session")
vim.fn.delete(root, "rf")
print("harness_access: passed")
vim.cmd("qa!")
