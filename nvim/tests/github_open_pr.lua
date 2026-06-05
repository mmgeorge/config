vim.loader.enable(false)

local open_pr = require("github.open_pr")

local root = "D:/mock/github"
local captured_picker = nil
local system_calls = {}
local original_system = vim.system
local original_notify = vim.notify
local original_snacks = _G.Snacks
local original_picker_pick = original_snacks and original_snacks.picker and original_snacks.picker.pick

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function wait_for(predicate, message)
  local ok = vim.wait(1000, predicate, 10)
  assert_true(ok, message)
end

local function reset()
  captured_picker = nil
  system_calls = {}
end

vim.notify = function() end
if not _G.Snacks then _G.Snacks = {} end
if not Snacks.picker then Snacks.picker = {} end
Snacks.picker.pick = function(opts)
  captured_picker = opts
  return opts
end

vim.system = function(command, opts, callback)
  system_calls[#system_calls + 1] = {
    command = vim.deepcopy(command),
    key = command_key(command),
    cwd = opts and opts.cwd or nil,
  }
  callback({
    code = 0,
    stdout = table.concat({
      "main",
      "feature/current",
      "remotes/origin/HEAD -> origin/main",
      "remotes/origin/develop",
      "remotes/origin/main",
      "origin/release/2026",
      "develop",
    }, "\n"),
    stderr = "",
  })
end

local function cleanup()
  open_pr._set_base_branch_for_test("main")
  if original_snacks then
    _G.Snacks = original_snacks
    if original_snacks.picker then original_snacks.picker.pick = original_picker_pick end
  else
    _G.Snacks = nil
  end
  vim.system = original_system
  vim.notify = original_notify
end

local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "x", false)
end

local function run_tests()
  reset()
  open_pr._set_base_branch_for_test("main")
  local selected = nil
  open_pr._choose_base_branch_for_test(root, "feature/current", function(branch)
    selected = branch
  end)
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: main")
  end, "base confirmation did not render")
  press("y")
  wait_for(function() return selected == "main" end, "yes did not keep the last selected base branch")
  assert_true(#system_calls == 0, "confirming the existing base should not list branches")

  reset()
  selected = nil
  open_pr._choose_base_branch_for_test(root, "feature/current", function(branch)
    selected = branch
  end)
  press("n")
  wait_for(function() return captured_picker ~= nil end, "no did not open Snacks branch picker")
  assert_true(captured_picker.title == "Select PR base branch", "unexpected picker title")
  assert_true(#captured_picker.items == 3, "branch picker should deduplicate and exclude current/HEAD")
  assert_true(captured_picker.items[1].branch == "develop", "expected sorted develop branch")
  assert_true(captured_picker.items[2].branch == "main", "expected sorted main branch")
  assert_true(captured_picker.items[3].branch == "release/2026", "expected normalized release branch")
  captured_picker.confirm({ close = function() end }, captured_picker.items[1])
  wait_for(function() return selected == "develop" end, "selected branch was not returned")
  assert_true(open_pr._get_base_branch_for_test() == "develop", "selected base was not persisted")
  assert_true(system_calls[1].cwd == root, "branch picker did not list branches from repo root")
end

local ok, err = xpcall(run_tests, debug.traceback)
cleanup()
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("github_open_pr: ok")
vim.cmd("qa!")
