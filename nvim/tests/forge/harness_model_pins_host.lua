vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)

local stage = arg[1]
if not stage then
  local script = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")
  local data, workspace = vim.fn.tempname(), vim.fn.tempname()
  assert(vim.fn.mkdir(data, "p") == 1 and vim.fn.mkdir(workspace, "p") == 1)
  for _, step in ipairs({ "seed", "verify", "unpin-verify" }) do
    local result = vim.system({ vim.v.progpath, "--headless", "-u", "NONE", "-i", "NONE",
      "-l", script, step, workspace, data }, { text = true }):wait(20000)
    assert(result.code == 0, step .. " failed: " .. (result.stderr or "") .. (result.stdout or ""))
  end
  print("harness_model_pins_host: passed across three hosts and workspaces")
  vim.cmd("qa!")
  return
end

local workspace, data = assert(arg[2]), assert(arg[3])
workspace = vim.fs.joinpath(workspace, stage)
vim.fn.mkdir(workspace, "p")
local executable = require("forge.builder").binary_path()
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local picker = require("forge.views.picker")
local pins = require("forge.views.harness.model_pins")
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 5), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function first()
  return picker._state_for_test().spec.page_list[1].option_list[1]
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  await(function() return state.ready and state.session end, "Harness did not open")
  await(function() return state.model_backend == "mock" and state.model_list end, "initial model catalog did not resolve")
  local active_model = state.session.model
  state.model_backend = "mock"
  state.model_list = { { id = "provider-first" }, { id = "pinned-model", is_default = true } }
  controller.select_model()
  await(function()
    local instance = picker._state_for_test()
    return instance and instance.spec.owner and instance.spec.owner:match("^harness%-model%-")
  end, "model picker did not open")
  if stage == "seed" then
    assert(first().id == "provider-first", vim.inspect(first()))
    vim.fn.maparg("p", "n", false, true).callback()
    await(function() return first().id == "pinned-model" end, "pin was not acknowledged")
    assert(first().label == "* pinned-model")
  elseif stage == "verify" then
    assert(first().id == "pinned-model" and pins.get("mock")["pinned-model"], "pin did not survive restart/workspace change")
    vim.fn.maparg("p", "n", false, true).callback()
    await(function() return first().id == "provider-first" end, "unpin was not acknowledged")
  else
    assert(first().id == "provider-first" and not pins.get("mock")["pinned-model"], "unpin did not survive restart")
  end
  assert(state.session.model == active_model, "pinning changed the active model")
  picker.close()
  client.stop()
end, debug.traceback)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_model_pins_host " .. stage .. ": passed")
vim.cmd("qa!")
