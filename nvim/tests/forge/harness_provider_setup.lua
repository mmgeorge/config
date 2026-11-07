vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local state = require("forge.session").harness
state.session = { id = "setup-test", provider = "fixture" }
local notices = {}
vim.notify = function(message, level) notices[#notices + 1] = { message = message, level = level } end
local available = false
local version_error
local refreshes = 0
local auth_command = { "node", "-e", "process.exit(process.env.FORGE_AUTH_FIXTURE === 'installed-cli' ? 0 : 8)" }
local calls = {}
package.loaded["forge.views.harness.controller"] = { render = function() end }
package.loaded["forge.client"] = { request = function(request_specification, callback)
  local session_id = request_specification.session_id
  local method = request_specification.method
  local params = request_specification.params or {}

  assert(session_id == "setup-test")
  calls[#calls + 1] = { method = method, params = params }
  vim.schedule(function()
    if method == "session.provider.setup" then
      refreshes = refreshes + 1
      assert(params.install == nil, "setup must never request installation")
      callback({ runtime = { kind = available and "available" or "missing" }, origin = "configured",
        versions = {
          adapter = available and { kind = "known", version = "0.89.1" } or { kind = "not_reported" },
          provider = version_error and { kind = "failed", failure = version_error } or { kind = "known", version = "2.1.302" },
        },
        authentication = available and { { id = "login", label = "Sign in", interaction = {
          kind = "terminal", launch = { program = auth_command[1], arguments = { unpack(auth_command, 2) },
            environment = { FORGE_AUTH_FIXTURE = "installed-cli" } },
        } } } or {},
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
local spec, option = wait_for("refresh")
assert(not selected("install"), "missing runtime exposed an installer")
assert(spec.page_list[1].subtitle:find("Provider version 2.1.302", 1, true), "installed CLI version is hidden without an adapter")
available = true
spec.on_confirm({ option = option })
spec, option = wait_for("login")
assert(not selected("install"), "available runtime exposed an installer")
assert(spec.page_list[1].subtitle:find("Provider version 2.1.302 · Adapter 0.89.1", 1, true), "CLI and adapter versions were conflated")
spec.on_confirm({ option = option })
assert(vim.wait(5000, function() return refreshes == 3 end, 10), "successful terminal login did not refresh setup")
wait_for("login")
local before = refreshes
auth_command = { "node", "-e", "process.exit(7)" }
setup.open({ host = host, session_id = "setup-test" })
spec, option = wait_for("login")
spec.on_confirm({ option = option })
local function reported_failure()
  return vim.iter(notices):any(function(notice) return notice.message:find("status 7", 1, true) end)
end
assert(vim.wait(5000, reported_failure, 10), "terminal failure was not reported: " .. vim.inspect(notices))
assert(refreshes == before + 1, "failed login was reported as successful")
for _, window in ipairs(vim.api.nvim_list_wins()) do
  if vim.api.nvim_win_get_config(window).relative ~= "" then pcall(vim.api.nvim_win_close, window, true) end
end
auth_command = { "node", "-e", "setInterval(() => {}, 1000)" }
setup.open({ host = host, session_id = "setup-test" })
spec, option = wait_for("login")
before = refreshes
spec.on_confirm({ option = option })
local login_window = vim.api.nvim_get_current_win()
local login_buffer = vim.api.nvim_win_get_buf(login_window)
local login_job = vim.b[login_buffer].terminal_job_id
assert(login_job and login_job > 0, "terminal login was not admitted")
vim.api.nvim_win_close(login_window, true)
assert(vim.wait(5000, function()
  return vim.fn.jobwait({ login_job }, 0)[1] ~= -1
end, 10), "cancelled login retained its process")
assert(refreshes == before, "cancelled login was reported as successful")
version_error = "runtime"
setup.open({ host = host, session_id = "setup-test" })
spec, option = wait_for("login")
assert(spec.page_list[1].subtitle:find("Provider version check failed (runtime)", 1, true), "version failure was hidden")
assert(option.value.interaction.kind == "terminal", "version failure removed authentication actions")
picker.close()
print("harness_provider_setup: passed")
vim.cmd("qa!")
