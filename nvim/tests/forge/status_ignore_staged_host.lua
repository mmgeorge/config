local root = vim.fn.getcwd()
dofile(root .. "/nvim/tests/forge/fixtures/commit_reuse_manual.lua")
local state = forge_reuse.state
local status = require("forge.status")
local client = require("forge.client")
local notices = {}
vim.notify = function(message, level) if level == vim.log.levels.ERROR then notices[#notices + 1] = message end end
local function idle()
  return state.ready and not state.request_active and next(client._client.pending) == nil
    and #(state.replica.inventory.pending or {}) == 0
end
local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "xt", false)
end
local function find(path, section)
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == path and (not section or file.section == section) then return file end
  end
end
local function select_file(path, section)
  local file = assert(find(path, section), "missing " .. section .. " file " .. path)
  local _, row = state.replica.sequence:position("file:" .. file.id)
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
end
local function await_section(path, section)
  assert(vim.wait(10000, function() return idle() and find(path, section) ~= nil end, 10),
    "expected " .. path .. " in " .. section .. ": " .. table.concat(notices, "\n"))
end
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", forge_reuse.fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end
local success, failure = xpcall(function()
  assert(vim.wait(15000, idle, 10), "native Status did not open")
  vim.v.errmsg = ""
  for _, path in ipairs({ "tracked.txt", "untracked.txt", "deleted.txt" }) do
    local before = vim.uv.fs_stat(forge_reuse.fixture .. "/" .. path)
      and vim.fn.readfile(forge_reuse.fixture .. "/" .. path) or false
    local original_section = path == "untracked.txt" and "untracked" or "unstaged"
    select_file(path, original_section)
    press("S")
    await_section(path, "staged")
    select_file(path, "staged")
    press("I")
    await_section(path, "ignored")
    assert(not find(path, "staged") and not find(path, "unstaged"), "ignored path remained in another section")
    assert(git({ "diff", "--cached", "--name-only", "--", path }) == "", "ignore left file staged")
    local after = vim.uv.fs_stat(forge_reuse.fixture .. "/" .. path)
      and vim.fn.readfile(forge_reuse.fixture .. "/" .. path) or false
    assert(vim.deep_equal(before, after), "ignore changed the worktree")
    select_file(path, "ignored")
    press("<Tab>")
    assert(vim.wait(5000, idle, 10))
    select_file(path, "ignored")
    press("U")
    await_section(path, original_section)
  end
  assert(find("new-name.txt", "untracked") and find("old-name.txt", "unstaged"), "ignore changed unrelated files")
  assert(vim.v.errmsg == "", vim.v.errmsg)
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)
status.close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not stop")
assert(success, failure)
print("staged ignore key mapping, Git state, worktree preservation, and unignore passed")
vim.cmd("qa!")
