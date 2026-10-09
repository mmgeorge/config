vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)

local stage = arg[1]
if not stage then
  local script = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")
  local workspace, data = vim.fn.tempname(), vim.fn.tempname()
  assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
  for _, step in ipairs({ "seed", "resubmit", "verify" }) do
    local result = vim.system({ vim.v.progpath, "--headless", "-u", "NONE", "-i", "NONE",
      "-l", script, step, workspace, data }, { text = true }):wait(30000)
    assert(result.code == 0, step .. " failed: " .. (result.stderr or "") .. (result.stdout or ""))
  end
  print("harness_prompt_history_host: passed across three Neovim processes")
  vim.cmd("qa!")
  return
end

local workspace, data = assert(arg[2]), assert(arg[3])
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
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
local history = require("forge.views.harness.prompt_history")
local plan = "/plan Demonstrate persisted prompt recall"

local function await(predicate, message)
  assert(vim.wait(15000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end

local function submit(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
  controller.submit()
  await(function()
    return not state.busy and not state.task_operation and not state.state_sync_pending
      and not state.configuring and not state.configuration_debounce
      and state.prompt_history[1] == text
  end, "submission or history did not settle: " .. text)
end

local function composer_text()
  return table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n")
end

local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  await(function() return state.ready and state.session ~= nil end, "Harness did not open")
  if stage == "seed" then
    submit(plan)
    submit("/fast")
    submit("/ultrafast")
  elseif stage == "resubmit" then
    assert(state.prompt_history[1] == "/ultrafast", "latest command was not restored")
    assert(state.prompt_history[3] == plan, "older plan command was not persisted")
    for _ = 1, 3 do history.previous() end
    assert(composer_text() == plan, "Up did not recall the older plan command")
    controller.submit()
    await(function()
      return not state.busy and not state.task_operation and not state.state_sync_pending
        and state.prompt_history[1] == plan
    end, "recalled plan was not promoted to newest history")
    assert(state.prompt_history_index == 0, "resubmission retained its old navigation index")
  elseif stage == "verify" then
    assert(state.prompt_history[1] == plan, "restart lost the resubmitted plan's history order")
    history.previous()
    assert(composer_text() == plan, "first Up after restart did not recall the resubmitted plan")
  else
    error("unknown stage: " .. stage)
  end
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "Harness host was not collected")
vim.cmd("qa!")
