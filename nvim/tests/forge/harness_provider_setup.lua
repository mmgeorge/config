vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local state = require("forge.session").harness
state.session = { id = "setup-test", backend = "claude" }
local notices = {}
vim.notify = function(message, level) notices[#notices + 1] = { message = message, level = level } end
local installation = false
local refreshes = 0
local auth_command = { "node", "-e", "process.exit(0)" }
local calls = {}
package.loaded["forge.views.harness.controller"] = { render = function() end }
package.loaded["forge.client"] = { request_for = function(session_id, method, params, callback)
  assert(session_id == "setup-test")
  calls[#calls + 1] = { method = method, params = params }
  vim.schedule(function()
    if method == "backend.setup" then
      refreshes = refreshes + 1
      installation = installation or params.install
      callback({ available = installation, managed = installation, installable = true,
        supported_version = "fixture", version = installation and "fixture" or nil,
        auth_methods = installation and { { id = "oauth", label = "OAuth", terminal = true, command = auth_command } } or {},
      })
    else callback({ authenticated = true }) end
  end)
end }
local picker = require("forge.views.picker")
local setup = require("forge.views.harness.provider_setup")
local function selected(id)
  local current = assert(picker._state_for_test())
  for _, option in ipairs(current.spec.page_list[1].option_list) do
    if option.id == id then return current.spec, option end
  end
end
local function wait_for(id)
  assert(vim.wait(5000, function() return selected(id) ~= nil end, 10), "setup action missing: " .. id)
  return selected(id)
end
local host = { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() }
setup.open({ host = host, session_id = "setup-test" })
local spec, option = wait_for("install")
spec.on_confirm({ option = option })
spec, option = wait_for("oauth")
assert(calls[#calls].params.install == true)
spec.on_confirm({ option = option })
assert(vim.wait(5000, function() return not state.configuring and refreshes == 3 end, 10), "successful terminal login did not refresh setup")
wait_for("oauth")
local before = refreshes
auth_command = { "node", "-e", "process.exit(7)" }
setup.open({ host = host, session_id = "setup-test" })
spec, option = wait_for("oauth")
spec.on_confirm({ option = option })
assert(vim.wait(5000, function() return not state.configuring end, 10), "failed login left configuration blocked")
assert(refreshes == before + 1, "failed login was reported as successful")
assert(vim.iter(notices):any(function(notice) return notice.message:find("status 7", 1, true) end), "terminal failure was not reported")
for _, window in ipairs(vim.api.nvim_list_wins()) do
  if vim.api.nvim_win_get_config(window).relative ~= "" then pcall(vim.api.nvim_win_close, window, true) end
end
auth_command = { "node", "-e", "setInterval(() => {}, 1000)" }
setup.open({ host = host, session_id = "setup-test" })
spec, option = wait_for("oauth")
before = refreshes
spec.on_confirm({ option = option })
local login_window = vim.api.nvim_get_current_win()
local login_buffer = vim.api.nvim_win_get_buf(login_window)
local login_job = vim.b[login_buffer].terminal_job_id
assert(login_job and state.configuring, "terminal login was not admitted")
vim.api.nvim_win_close(login_window, true)
assert(vim.wait(5000, function()
  return not state.configuring and vim.fn.jobwait({ login_job }, 0)[1] ~= -1
end, 10), "cancelled login retained its process or configuration lock")
assert(refreshes == before, "cancelled login was reported as successful")
print("harness_provider_setup: passed")
vim.cmd("qa!")
