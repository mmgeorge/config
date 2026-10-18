vim.opt.runtimepath:prepend(vim.fs.normalize(vim.fn.getcwd()) .. "/nvim")
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
local request = client.request
local pending_history = 0
client.request = function(method, params, callback)
  if method ~= "history.record" then return request(method, params, callback) end
  pending_history = pending_history + 1
  return request(method, params, function(result, failure)
    pending_history = pending_history - 1
    callback(result, failure)
  end)
end
local errors = {}
require("forge.infra.notifications").error = function(message) errors[#errors + 1] = message end
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
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
      and pending_history == 0 and state.prompt_history[1] and state.prompt_history[1].text == text
  end, "submission or history did not settle: " .. text)
end

local function composer_text()
  return table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n")
end

---@param keys string
local function press(keys)
  vim.api.nvim_set_current_win(state.composer_win)
  vim.api.nvim_feedkeys(vim.keycode(keys), "xt", false)
end

local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge").open_harness()
  await(function() return state.ready and state.session ~= nil end, "Harness did not open")
  if stage == "seed" then
    for index = 1, 105 do
      local recorded = false
      client.request("history.record", { text = "persisted prompt " .. index, attachments = {} }, function(_, problem)
        assert(not problem, problem)
        recorded = true
      end)
      await(function() return recorded end, "history write was not acknowledged")
    end
    submit(plan)
    submit("/fast")
    submit("/ultrafast")
  elseif stage == "resubmit" then
    assert(state.prompt_history[1].text == "/ultrafast", "latest command was not restored: " .. vim.inspect(state.prompt_history))
    assert(state.prompt_history[3].text == plan, "older plan command was not persisted")
    press("<Up><Up><Up>")
    assert(composer_text() == plan, "Up did not recall the older plan command")
    controller.submit()
    await(function()
      return not state.busy and not state.task_operation and not state.state_sync_pending
        and pending_history == 0 and state.prompt_history[1].text == plan
    end, "recalled plan was not promoted to newest history")
    assert(state.prompt_history_index == 0, "resubmission retained its old navigation index")
  elseif stage == "verify" then
    assert(state.prompt_history[1].text == plan, "restart lost the resubmitted plan's history order")
    press("i<Up><Esc>")
    assert(composer_text() == plan, "first Up after restart did not recall the resubmitted plan")
    assert(#state.prompt_history == 109, "host truncated persisted prompt history")
    for _ = 2, #state.prompt_history do press("<Up>") end
    assert(composer_text() == "persisted prompt 1", "oldest prompt was not reachable after restart")
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
