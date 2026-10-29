vim.loader.enable(false)
if not vim.g.forge_checks_grid then
  for _, width in ipairs({ 60, 100, 160 }) do
    local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
    assert(child > 0)
    local success, failure = pcall(function()
      vim.rpcrequest(child, "nvim_ui_attach", width, 40, { rgb = true })
      vim.rpcrequest(child, "nvim_exec_lua", [[
        vim.g.forge_checks_grid = true
        vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
        dofile('nvim/tests/forge/plan_checks_host.lua')
      ]], {})
    end)
    vim.fn.jobstop(child)
    assert(success, failure)
  end
  print("plan_checks_host: command syntax and check controls passed at 60, 100, and 160 columns")
  return
end
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local config = vim.json.encode({ checks = {
  ["tests"] = { trigger = { auto = true }, action = { command = { "cargo",  "+nightly-2026-04-01-x86_64-pc-windows-msvc", "fmt", "--all", "--", "--check"  }, label = "Package tests", directory = ".", category = "test", after = {} } }
} })
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
client._host_set_launcher_for_test(vim.system)
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
  local color = { ForgeHarnessCommand = 0xff3333, String = 0x00ff44, ForgeHarnessOption = 0x00aaff }
  for group, value in pairs(color) do vim.api.nvim_set_hl(0, group, { fg = value }) end
  local captured = {}
  for _, source in ipairs(review.owner.source) do
    if source.block:find("plan:configured-checks:", 1, true) then
      for _, span in ipairs(source.metadata.decoration or {}) do
        if span.range.start.row == source.position.row and color[span.capture] then
          local _, start = review.owner.replica.sequence:position(source.block)
          local canonical = start + source.position.row
          local row = review.owner.replica.physical_row(canonical)
          local column = review.owner.replica.physical_column(canonical, span.range.start.column)
          vim.api.nvim_win_set_cursor(review.win, { row + 1, column })
          vim.cmd("normal! zz")
          vim.cmd("redraw!")
          local screen = vim.fn.screenpos(review.win, row + 1, column + 1)
          local cell = vim.api.nvim__inspect_cell(1, screen.row - 1, screen.col - 1)
          assert(cell[2].foreground == color[span.capture], "command capture lost its screen color: " .. vim.inspect({ span, cell }))
          captured[span.capture] = (captured[span.capture] or "") .. source.text:sub(span.range.start.column + 1, span.range["end"].column)
        end
      end
    end
  end
  assert(captured.ForgeHarnessCommand == "cargo", "check executable was not highlighted")
  assert(captured.String == '"+nightly-2026-04-01-x86_64-pc-windows-msvc"', "wrapped quoted argument lost syntax")
  assert(captured.ForgeHarnessOption and captured.ForgeHarnessOption:find("--check", 1, true), "check options were not highlighted")
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
client.host_stop()
vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath = original_stdpath
if not success then error(failure) end
print("plan_checks_host passed")
