dofile(vim.fn.getcwd() .. "/nvim/tests/forge/fixtures/commit_reuse_manual.lua")
local state = forge_reuse.state
local repository = forge_reuse.fixture
local remote = vim.fn.tempname() .. "-forge-push-remote"
local client = require("forge.client")
local writer = require("forge.git.write")
local original_execute = writer.execute
local errors = {}
local original_notify = vim.notify
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = message end
end
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", repository }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
local function idle() return state.ready and not state.request_active and next(client._client.pending) == nil end
assert(vim.wait(15000, idle, 10), "status did not open")
git({ "init", "--bare", remote })
git({ "remote", "add", "origin", remote })
git({ "push", "--set-upstream", "origin", "HEAD" })
git({ "add", "tracked.txt" })
git({ "commit", "--quiet", "-m", "test: push fixture" })
local expected = git({ "rev-parse", "HEAD" })
local branch = git({ "symbolic-ref", "--short", "HEAD" })
local function remote_head()
  local result = vim.system({ "git", "--git-dir", remote, "rev-parse", "refs/heads/" .. branch }, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
assert(remote_head() ~= expected, "fixture remote already has the commit")
require("forge.status").refresh(state)
assert(vim.wait(10000, idle, 10), "status did not refresh")
local header
for row, text in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
  if text:find("Unstaged changes", 1, true) then header = row break end
end
assert(header, "missing section header")
vim.api.nvim_win_set_cursor(0, { header, 0 })
local outcome, writes = nil, 0
local left_status, progress_seen = false, false
vim.api.nvim_create_autocmd("BufWinLeave", { buffer = state.replica.buffer, callback = function() left_status = true end })
writer.execute = function(workspace, action, callback, progress)
  assert(workspace == repository and action.kind == "push", "unexpected write")
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer, "push switched buffers before starting")
  assert(state.replica.presentation.remote_action.status == "Pushing...", "push did not present initial progress")
  writes = writes + 1
  return original_execute(workspace, action, function(result) outcome = result callback(result) end, function(text, stream)
    progress(text, stream)
    progress_seen = true
    assert(not left_status and vim.api.nvim_get_current_buf() == state.replica.buffer, "push progress replaced Status")
    local status = state.replica.presentation.remote_action.status
    local rows = table.concat(state.replica.block["status:context:push"].text, " ")
    assert(rows:find(status, 1, true) or rows:find(status:sub(1, 20), 1, true), "Git progress did not reach the Push header")
  end)
end
_G.forge_push_check = function()
  assert(outcome and outcome.ok, "push failed: " .. vim.inspect(outcome))
  assert(writes == 1, "push was not dispatched exactly once")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(remote_head() == expected, "remote did not receive commit")
  assert(not left_status and progress_seen, "Status did not stay visible throughout streamed progress")
  assert(state.replica.presentation.remote_action == nil, "finished push retained temporary progress")
  assert(vim.api.nvim_get_current_buf() == state.replica.buffer, "push did not restore Status")
  print("status_push_host OK: inline Git progress, no buffer switch, local remote updated")
end
if vim.g.forge_manual_push then return end
vim.o.timeout, vim.o.timeoutlen = true, 300
local index = 0
local timer = assert(vim.uv.new_timer())
timer:start(50, 50, function()
  index = index + 1
  vim.api.nvim_input(("opp"):sub(index, index))
  if index == 3 then timer:stop() timer:close() end
end)
local started = vim.uv.hrtime()
local checking_rejection = false
local function finish()
  if not outcome and vim.uv.hrtime() - started < 10e9 then vim.defer_fn(finish, 50) return end
  local ok, failure = xpcall(function()
    if not checking_rejection then
      forge_push_check()
      vim.fn.writefile({ "#!/bin/sh", "echo fixture-push-rejected >&2", "exit 1" }, remote .. "/hooks/pre-receive")
      assert(vim.uv.fs_chmod(remote .. "/hooks/pre-receive", 493))
      vim.fn.writefile({ "next rejected change" }, repository .. "/tracked.txt")
      git({ "add", "tracked.txt" })
      git({ "commit", "--quiet", "-m", "test: rejected push" })
      outcome, errors, progress_seen = nil, {}, false
      checking_rejection, started = true, vim.uv.hrtime()
      vim.api.nvim_input("opp")
      return false
    end
    assert(outcome and not outcome.ok and writes == 2, "rejecting hook did not fail exactly one push")
    assert(table.concat(errors, "\n"):find("fixture-push-rejected", 1, true), "rejection omitted its error notification")
    assert(remote_head() == expected, "rejected push changed the remote")
    assert(not left_status and progress_seen and vim.api.nvim_get_current_buf() == state.replica.buffer,
      "failed push replaced Status")
    assert(state.replica.presentation.remote_action == nil, "failed push retained temporary progress")
    print("status_push_host rejection OK: Status retained, progress cleared, error notified, remote unchanged")
    return true
  end, debug.traceback)
  if ok and failure == false then vim.defer_fn(finish, 50) return end
  writer.execute, vim.notify = original_execute, original_notify
  require("forge.status").close(state)
  if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") return end
  vim.cmd("qa!")
end
vim.defer_fn(finish, 250)
