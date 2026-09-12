local config = vim.fn.getcwd()
vim.g.forge_manual_edge_cases = true
dofile(config .. "/nvim/tests/forge/fixtures/status_contention_manual.lua")
local client = require("forge.client")
local status = require("forge.status")
local state = forge_manual.state
local accepted = {}
local observed = {}
local rendered = {}
local apply_update = status.apply_update
status.apply_update = function(owner, update)
  apply_update(owner, update)
  if owner == state then
    observed[#observed + 1] = vim.deepcopy(owner.replica.inventory)
    rendered[#rendered + 1] = vim.api.nvim_buf_get_lines(owner.replica.buffer, 0, -1, false)
  end
  if owner == state and update.phase == "accepted" then
    accepted[#accepted + 1] = vim.deepcopy(owner.replica.inventory)
  end
end
local ok, failure = xpcall(function()
  assert(vim.wait(15000, function()
    return state.replica.status == "Applied" and not state.request_active and next(client._client.pending) == nil
  end, 10), "open timed out")
  local expected = {}
  local initial_section = {}
  local initial_change = {}
  for _, file in ipairs(state.replica.inventory.file) do
    expected[file.path] = vim.deepcopy(file.stats)
    initial_section[file.path] = file.section
    initial_change[file.path] = file.change
  end
  local _, row = state.replica.sequence:position("section:unstaged")
  vim.api.nvim_win_set_cursor(0, { row + 2, 0 })
  vim.api.nvim_feedkeys("S", "xt", false)
  assert(vim.wait(15000, function()
    return #accepted > 0 and #(state.replica.inventory.pending or {}) == 0 and next(client._client.pending) == nil
  end, 10), "stage timed out")
  assert(#accepted[1].pending > 0, "test missed pending update")
  assert(#accepted[1].file == 75, "accepted inventory lost files")
  for _, file in ipairs(accepted[1].file) do
    assert(file.section == "staged", file.path .. " did not move in the accepted update")
  end
  local result = vim.system({ "git", "-C", forge_manual.fixture, "diff", "--cached", "--name-only" }, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  assert(#vim.split(vim.trim(result.stdout), "\n") == 75, "Git did not stage all selected files")
  for update_index, inventory in ipairs(observed) do
    for _, file in ipairs(inventory.file) do
      assert(file.change == initial_change[file.path], file.path .. " label changed to " .. file.change)
      assert(vim.deep_equal(file.stats, expected[file.path]), file.path .. " count changed: " .. vim.inspect(file.stats))
      if file.stats.state == "exact" then
        local suffix = file.path .. " +" .. file.stats.added .. " -" .. file.stats.deleted
        local found = false
        for _, line in ipairs(rendered[update_index]) do
          if line:find(suffix, 1, true) then found = true break end
        end
        assert(found, "rendered count missing for " .. file.path)
        if file.change == "deleted" then
          assert(table.concat(rendered[update_index], "\n"):find("Deleted  " .. suffix, 1, true), "rendered deletion label missing")
        end
      end
    end
  end
  local stage_updates = #accepted
  local _, staged_row = state.replica.sequence:position("section:staged")
  vim.api.nvim_win_set_cursor(0, { staged_row + 2, 0 })
  vim.api.nvim_feedkeys("U", "xt", false)
  assert(vim.wait(15000, function()
    return #accepted > stage_updates and #(state.replica.inventory.pending or {}) == 0 and next(client._client.pending) == nil
  end, 10), "unstage timed out: " .. vim.inspect({ accepted = #accepted, pending = state.replica.inventory.pending, cursor = vim.api.nvim_win_get_cursor(0), line = vim.api.nvim_get_current_line(), notices = forge_manual.notices }))
  assert(#accepted[stage_updates + 1].pending > 0, "test missed pending unstage update")
  for update_index, inventory in ipairs(observed) do
    assert(#inventory.file == 75, "update lost files")
    for _, file in ipairs(inventory.file) do
      assert(file.change == initial_change[file.path], file.path .. " label changed to " .. file.change)
      assert(vim.deep_equal(file.stats, expected[file.path]), file.path .. " count changed: " .. vim.inspect(file.stats))
      if file.stats.state == "exact" then
        local suffix = file.path .. " +" .. file.stats.added .. " -" .. file.stats.deleted
        local found = false
        for _, line in ipairs(rendered[update_index]) do
          if line:find(suffix, 1, true) then found = true break end
        end
        assert(found, "rendered count missing for " .. file.path)
        if file.change == "deleted" then
          assert(table.concat(rendered[update_index], "\n"):find("Deleted  " .. suffix, 1, true), "rendered deletion label missing")
        end
      end
    end
  end
  for _, file in ipairs(accepted[stage_updates + 1].file) do assert(file.section == initial_section[file.path], file.path .. " did not unstage immediately") end
  local unstaged = vim.system({ "git", "-C", forge_manual.fixture, "diff", "--cached", "--name-only" }, { text = true, timeout = 10000 }):wait()
  assert(unstaged.code == 0 and vim.trim(unstaged.stdout) == "", "index is not empty")
  for _, notice in ipairs(forge_manual.notices) do
    assert(notice.message:find("no Copilot OAuth token found", 1, true), notice.message)
  end
  print("all 75 files moved immediately and retained counts through every stage/unstage update")
end, debug.traceback)
status.apply_update = apply_update
status.close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not exit")
vim.fn.chdir(config)
assert(ok, failure)
