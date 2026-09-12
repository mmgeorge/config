local config = vim.fn.getcwd()
vim.g.forge_manual_edge_cases = true
dofile(config .. "/nvim/tests/forge/fixtures/status_contention_manual.lua")
local client = require("forge.client")
local state = forge_manual.state
local function key(value)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(value, true, false, true), "xt", false)
end
local function settled()
  return state.replica.status == "Applied" and #(state.replica.inventory.pending or {}) == 0
    and not state.request_active and not state.pending and next(client._client.pending) == nil
end
local function await(predicate, phase)
  assert(vim.wait(15000, predicate, 10), phase .. ": " .. vim.inspect(forge_manual.notices))
end
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", forge_manual.fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout:gsub("\r", "")
end
local function select_file(path, section)
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == path and file.section == section then
      local _, row = state.replica.sequence:position("file:" .. file.id)
      vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
      return
    end
  end
  error("missing " .. section .. " " .. path)
end
local ok, failure = xpcall(function()
  await(settled, "open")
  for _, file in ipairs(state.replica.inventory.file) do assert(file.path ~= "ignored.txt", "Git-ignored file entered Status") end
  for _, case in ipairs({
    { path = "new file.txt", section = "untracked", change = "A" },
    { path = "binary.bin", section = "untracked", change = "A" },
    { path = "deleted.rs", section = "unstaged", change = "D" },
  }) do
    select_file(case.path, case.section)
    key("S")
    await(settled, "stage " .. case.path)
    assert(git({ "diff", "--cached", "--name-status" }) == case.change .. "\t" .. case.path .. "\n")
    select_file(case.path, "staged")
    key("U")
    await(settled, "unstage " .. case.path)
    assert(git({ "diff", "--cached", "--name-only" }) == "")
    select_file(case.path, case.section)
  end
  for _, notice in ipairs(forge_manual.notices) do
    assert(notice.message:find("no Copilot OAuth token found", 1, true), notice.message)
  end
  local baseline = #forge_manual.notices
  local lock = forge_manual.fixture .. "/.git/index.lock"
  vim.fn.writefile({ "fixture-owned lock" }, lock)
  select_file("source_01.rs", "unstaged")
  key("S")
  await(function() return settled() and #forge_manual.notices > baseline end, "write failure rollback")
  assert(git({ "diff", "--cached", "--name-only" }) == "", "failed write changed index")
  select_file("source_01.rs", "unstaged")
  for index = baseline + 1, #forge_manual.notices do
    assert(forge_manual.notices[index].message:find("index.lock", 1, true), forge_manual.notices[index].message)
  end
  assert(vim.fn.delete(lock) == 0)
  key("S")
  await(settled, "successful retry after lock release")
  assert(git({ "diff", "--cached", "--name-only" }) == "source_01.rs\n")
  select_file("source_01.rs", "staged")
  key("U")
  await(settled, "final unstage")
  assert(git({ "diff", "--cached", "--name-only" }) == "")
  print("host edges: untracked space path, binary, deletion, ignored path, and failed-write rollback passed")
end, debug.traceback)
require("forge.status").close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not exit")
vim.fn.chdir(config)
assert(ok, failure)
