vim.opt.runtimepath:prepend("nvim")
local controller = require("forge.views.harness.controller")
local notifications = require("forge.infra.notifications")
local state = require("forge.session").harness
local notice_list, actions = {}, {}
notifications.info = function(message) notice_list[#notice_list + 1] = message end
notifications.warn = notifications.info
notifications.error = notifications.info
state.session = { id = "notification-test", approval_mode = "deny", access = { read_access = "full", write_access = "full" } }
controller.task_transition = function(action) actions[#actions + 1] = action end
controller.toggle_write()
assert(actions[1].change.kind == "toggle_write_enabled")
assert(state.session.access.write_access == "full" and state.session.approval_mode == "deny",
  "an unconfirmed intent mutated displayed access or approval")
controller.task_transition({ action = "permission", change = { kind = "set_approval", mode = "allow" } })
assert(actions[2].change.mode == "allow" and state.session.approval_mode == "deny")
assert(#notice_list == 0, "routine permission commands should not notify")
print("harness_notifications: passed")
vim.cmd("qa!")
