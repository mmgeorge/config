local config = vim.fn.getcwd()
dofile(config .. "/nvim/tests/forge/fixtures/status_contention_manual.lua")
local client = require("forge.client")
local state = forge_manual.state
local function await(predicate, phase)
  assert(vim.wait(15000, predicate, 20), phase .. " timed out: " .. vim.inspect(forge_manual.notices))
end
local function key(value)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(value, true, false, true), "xt", false)
end
local function audit()
  local replica = state.replica
  local owned, installed = 0, 0
  for id, handles in pairs(replica.marks) do
    local entry = assert(replica.block[id], "marks retained for removed body")
    local _, start = replica.sequence:position(id)
    local expected = {}
    for _, gutter in ipairs(entry.metadata.gutter or {}) do expected[start + gutter.position.row] = true end
    for _, handle in ipairs(handles) do
      local mark = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, replica.namespace, handle, { details = true })
      if mark[3] and mark[3].virt_text then
        owned = owned + 1
        assert(expected[mark[1]], ("staging moved %s gutter to row %d outside its body at %d"):format(id, mark[1], start))
      end
    end
  end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace, 0, -1, { details = true })) do
    if mark[4].virt_text then installed = installed + 1 end
  end
  assert(installed == owned, ("staging left %d orphan gutter extmarks"):format(installed - owned))
end
local ok, failure = xpcall(function()
  await(function() return state.replica.status == "Applied" end, "initial inventory")
  local neighbor
  for _, file in ipairs(state.replica.inventory.file) do if file.path == "source_02.rs" then neighbor = file.id end end
  assert(neighbor)
  local _, neighbor_row = state.replica.sequence:position("file:" .. neighbor)
  vim.api.nvim_win_set_cursor(0, { neighbor_row + 1, 0 })
  key("<Tab>")
  await(function()
    local _, current_row = state.replica.sequence:position("file:" .. neighbor)
    return vim.fn.foldclosed(current_row + 1) == -1 and not next(client._client.pending)
  end, "neighbor expansion")
  local function audit_neighbor()
    local _, current_row = state.replica.sequence:position("file:" .. neighbor)
    assert(vim.fn.foldlevel(current_row + 1) > 0, "neighbor lost its fold")
    assert(vim.fn.foldclosed(current_row + 1) == -1, "mutation closed the expanded neighboring file")
    for _, file in ipairs(state.replica.inventory.file) do
      if file.path ~= "source_01.rs" and file.path ~= "source_02.rs" then
        local _, file_row = state.replica.sequence:position("file:" .. file.id)
        assert(vim.fn.foldclosed(file_row + 1) == file_row + 1, "mutation opened unrelated file " .. file.path)
      end
    end
  end
  local selected
  for _, file in ipairs(state.replica.inventory.file) do if file.path == "source_01.rs" then selected = file.id end end
  assert(selected)
  local _, row = state.replica.sequence:position("file:" .. selected)
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  key("<Tab>")
  await(function()
    for _, text in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
      if text:find("Source1.render", 1, true) then return true end
    end
  end, "structural context")
  audit()
  _, row = state.replica.sequence:position("file:" .. selected)
  vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
  key("S")
  await(function()
    if #(state.replica.inventory.pending or {}) ~= 0 or next(client._client.pending) then return false end
    for _, file in ipairs(state.replica.inventory.file) do
      if file.path == "source_01.rs" and file.section == "staged" then return true end
    end
  end, "stage settlement")
  audit()
  audit_neighbor()
  local result = vim.system({ "git", "-C", forge_manual.fixture, "diff", "--cached", "--name-only" }, { text = true, timeout = 10000 }):wait()
  assert(result.code == 0 and result.stdout:gsub("\r", "") == "source_01.rs\n", "staging changed an unselected file")
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == "source_01.rs" and file.section == "staged" then
      local _, staged_row = state.replica.sequence:position("file:" .. file.id)
      vim.api.nvim_win_set_cursor(0, { staged_row + 1, 0 })
      break
    end
  end
  key("U")
  await(function()
    if #(state.replica.inventory.pending or {}) ~= 0 or next(client._client.pending) then return false end
    for _, file in ipairs(state.replica.inventory.file) do
      if file.path == "source_01.rs" and file.section == "unstaged" then return true end
    end
  end, "unstage settlement")
  audit()
  audit_neighbor()
  local previous = {}
  for _, file in ipairs(state.replica.inventory.file) do
    assert(not previous[file.section] or previous[file.section] < file.path, "staging reordered unrelated files")
    previous[file.section] = file.path
  end
end, debug.traceback)
require("forge.status").close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not exit")
vim.fn.chdir(config)
assert(ok, failure)
print("status staging host gutters passed")
