vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local config = vim.json.encode({ checks = { version = 2, definitions = {{
  id = "tests", name = "Package tests", category = "test", area = "Project", enabled = true,
  trigger = { kind = "any_change" }, scope = "all", directory = ".", depends_on = {},
  all = { command = "nu", args = { "-c", "print checked" } },
}} } })
vim.fn.writefile({ config }, workspace .. "/.forge.json")
assert(vim.system({ "git", "init", workspace }, { text = true }):wait(10000).code == 0)
local executable = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local notices = {}
vim.notify = function(message, level) if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end end
local function await(predicate, message)
  assert(vim.wait(10000, function() return #notices > 0 or predicate() end, 10), message)
  assert(#notices == 0, table.concat(notices, "\n"))
end
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ harness = { backend = "mock" } })
  require("forge.views.harness").open()
  await(function() return state.presentation and state.presentation.ready end, "Harness startup")
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "/plan add a small feature" })
  controller.submit()
  await(function() return not state.busy and state.active_plan end, "plan submission")
  require("forge.views.plan_review.native_controller").open(state.active_plan)
  await(function() return state.plan_review and state.plan_review.owner.ready end, "plan review")
  local review = state.plan_review
  assert(review.owner.checks[1].enabled)
  review.command_set.action_by_id.checks.run({})
  vim.fn.maparg("<Space>", "n", false, true).callback()
  await(function() return not state.busy and not review.owner.checks[1].enabled end, "disable check")
  assert(table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n"):find("○ Package tests", 1, true))
  vim.fn.maparg("<Space>", "n", false, true).callback()
  await(function() return not state.busy and review.owner.checks[1].enabled end, "enable check")
  require("forge.views.picker").close()
  vim.api.nvim_set_current_win(review.win)
  local selected
  for index, metadata in ipairs(review.owner.projection.line_meta_list) do
    if metadata.check == "tests" then selected = index break end
  end
  assert(selected, "check has no semantic selection target")
  vim.api.nvim_win_set_cursor(review.win, { selected, 0 })
  vim.cmd("normal! V")
  vim.fn.maparg("j", "x", false, true).callback()
  vim.fn.maparg("y", "n", false, true).callback()
  await(function() return not state.busy and not review.owner.checks[1].enabled end, "delete check")
  local document = vim.json.decode(table.concat(vim.fn.readfile(review.plan.working_path:gsub("%.md$", ".json")), "\n"))
  assert(document.design.document.disabled_checks[1] == "tests", "disable was not persisted")
  assert(vim.fn.readfile(workspace .. "/.forge.json")[1] == config, "project defaults changed")
  assert(#(document.design.document.checks or {}) == 0, "disabled check retained explicit selection")
end, debug.traceback)
require("forge.views.picker").close()
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.stop()
vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
if not success then error(failure) end
print("plan_checks_host passed")
