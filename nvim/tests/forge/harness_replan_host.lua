vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(vim.g.forge_test_executable or require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local picker = require("forge.views.picker")
local function await(predicate, message)
  assert(vim.wait(10000, function() return #errors > 0 or predicate() end, 10), message)
  assert(#errors == 0, table.concat(errors, "\n"))
end
local function invoke(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert(mapping.callback, "missing binding " .. key)
  mapping.callback()
end
local function submit(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { text })
  controller.submit()
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness did not open")
  submit("/plan build the feature")
  await(function() return not state.busy and state.active_plan and state.active_plan.model_revision == 1 end, "initial plan missing")
  local source = vim.deepcopy(state.active_plan)
  local revision_path = vim.fs.joinpath(vim.fs.dirname(source.working_path), "revisions", "submitted-0001.json")
  local original = vim.fn.readfile(revision_path, "b")
  local response
  client.request_for(state.session.id, "plan.request_changes", { plan_id = source.id, comment = "Refine the description" }, function(result, request_error)
    assert(not request_error, request_error)
    response = result
  end)
  await(function() return response and state.active_plan.model_revision == 2 end, "second revision missing")
  await(function()
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(state.transcript_buf,
      state.presentation.transcript.namespace, 0, -1, { details = true })) do
      local text = vim.api.nvim_buf_get_lines(state.transcript_buf, mark[2], mark[2] + 1, false)[1] or ""
      if mark[4].hl_group == "ForgeHarnessPlan" and text:find("Awaiting plan review", 1, true) then return true end
    end
    return false
  end, "review status did not use Plan purple")
  vim.api.nvim_set_current_win(state.transcript_win)
  invoke("or")
  await(function() return not state.aborting_plan and not state.state_sync_pending
    and not state.active_plan and state.session.mode == "read" end, "abort did not exit Plan mode")
  assert(not state.active_elicitation, "abort retained input status")
  await(function()
    return not table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
      :find("Awaiting plan review", 1, true)
  end, "abort retained the rendered review status")
  submit("/replan")
  await(function() return picker.is_open() end, "replan picker missing")
  local choice = picker._state_for_test().spec.page_list[1].option_list[1]
  assert(choice.id == source.id and choice.detail:find("2/2", 1, true))
  invoke("<Left>")
  assert(picker._state_for_test().spec.page_list[1].option_list[1].detail:find("1/2", 1, true))
  invoke("<CR>")
  await(function() return not state.busy and state.active_plan and state.active_plan.id ~= source.id
    and state.active_plan.model_revision == 1 end, "replan did not create a new planning exchange")
  assert(state.session.mode == "plan")
  assert(vim.deep_equal(vim.fn.readfile(revision_path, "b"), original), "replan modified the source revision")
  controller.abort_plan()
  await(function() return not state.aborting_plan and not state.active_plan end, "replanned plan did not abort")
  await(function() return state.presentation.close() end, "presentation did not finish pending edits")
end, debug.traceback)
picker.close(false)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "Harness host was not collected")
print("harness_replan_host: passed")
