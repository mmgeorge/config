vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
vim.loader.enable(false)
local stage = arg[1]
if not stage then
  local script = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")
  local data = vim.fn.tempname()
  for _, phase in ipairs({ "save", "reopen" }) do
    local result = vim.system({ vim.v.progpath, "--headless", "-u", "NONE", "-i", "NONE",
      "-l", script, phase, data }, { text = true, timeout = 25000 }):wait()
    assert(result.code == 0, phase .. ": " .. (result.stderr or "") .. (result.stdout or ""))
  end
  print("commit settings host: saved controls survive fresh hosts and preserve chat settings")
  vim.cmd("qa!")
  return
end
local executable = require("forge.builder").binary_path()
local data = assert(arg[2])
local workspace = vim.fs.joinpath(data, stage)
vim.fn.mkdir(workspace, "p")
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
require("forge").setup({ harness = { backend = "mock" },
  background_generation = { mock = { model = "mock-model", reasoning_effort = "low" } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local picker = require("forge.views.picker")
local settings = require("forge.views.harness.settings")
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local function wait(predicate, message)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function option(id)
  local instance = picker._state_for_test()
  if not instance then return end
  for _, value in ipairs(instance.spec.page_list[1].option_list) do if value.id == id then return value end end
end
require("forge").open_harness()
wait(function() return state.ready and state.session end, "Harness did not start")
local before = { model = state.session.model, effort = state.session.effort }
settings.open(state, { window_list = { vim.api.nvim_get_current_win() }, control_win = vim.api.nvim_get_current_win() })
wait(function() local model = option("background-model") return model and not model.disabled_reason end, "Background controls did not load")
local expected = stage == "save" and "low" or "medium"
assert(option("background-model").columns[2]:find("mock-model", 1, true))
assert(option("background-thinking").columns[2]:find(expected, 1, true), vim.inspect(option("background-thinking")))
local lines = vim.api.nvim_buf_get_lines(picker._state_for_test().buf, 0, -1, false)
assert(table.concat(lines, "\n"):find("Background model", 1, true), "Background controls did not render")
if stage == "save" then
  picker._state_for_test().spec.action_list[2].callback({ option = option("background-thinking") })
  wait(function() return option("background-thinking").columns[2]:find("medium", 1, true) ~= nil end, "Thinking did not save")
end
assert(state.session.model == before.model and state.session.effort == before.effort, "Commit settings changed the chat")
local generator = require("forge.integrations.ai_commit")
generator.set_backend({ request_async = function(_, callback)
  callback({ state = "ready", message = "test: use saved preferences" })
end })
local generated
generator.ensure(workspace, { ref = "HEAD", force = true }, function(result) generated = result end)
wait(function() return generated ~= nil end, "Generation did not read saved settings")
assert(generated.state == "ready" and generated.model == "mock-model · thinking medium", vim.inspect(generated))
generator.reset_backend()
picker.close(false)
client.stop()
assert(vim.wait(5000, function() return not client._client.process end, 10), "Host did not stop")
print("commit settings " .. stage .. " passed")
vim.cmd("qa!")
