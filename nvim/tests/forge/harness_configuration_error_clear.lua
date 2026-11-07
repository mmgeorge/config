vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
-- A rejected settings change marks the winbar until a later settings change is accepted.
require("forge.infra.config").setup({})
local client = require("forge.client")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
client.host_get_generation = function() return 1 end
local configure_failure
client.request = function(request, callback)
  assert(request.method == "session.configure", request.method)
  vim.schedule(function() callback({}, configure_failure) end)
end
require("forge.infra.notifications").error = function() end
local ok, failure = xpcall(function()
  local state = session.harness
  state.session = { id = "session", provider = "mock" }
  state.capability = { fast_mode = false }
  controller.render = function() end
  controller.refresh_winbar = function() end
  controller.configure_service_tier("fast")
  assert(state.configuration_error, "an unsupported tier did not mark the settings rejection")
  configure_failure = "rejected again"
  local applied
  controller.configure({ effort = "low" }, function(value) applied = value end)
  assert(vim.wait(1000, function() return applied ~= nil end, 1), "configuration did not settle")
  assert(state.configuration_error, "a rejected change cleared the settings rejection")
  configure_failure = nil
  applied = nil
  controller.configure({ effort = "low" }, function(value) applied = value end)
  assert(vim.wait(1000, function() return applied ~= nil end, 1), "configuration did not settle")
  assert(state.configuration_error == nil, "an accepted change left the settings rejection visible")
end, debug.traceback)
if not ok then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("harness_configuration_error_clear: passed")
vim.cmd("qa!")
