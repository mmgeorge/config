local config = vim.fn.getcwd()
dofile(config .. "/nvim/tests/forge/fixtures/status_contention_manual.lua")
local client = require("forge.client")
local state = forge_manual.state

local function await(predicate, phase)
  assert(vim.wait(15000, predicate, 10), phase .. ": " .. vim.inspect(forge_manual.notices))
end

local function settled()
  return state.replica.status == "Applied" and #(state.replica.inventory.pending or {}) == 0
    and not state.pending and not state.request_active and next(client._client.pending) == nil
end

local function key(value)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(value, true, false, true), "xt", false)
end

local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", forge_manual.fixture }, arguments), { timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end

local function select_file(path, section)
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == path and file.section == section then
      local _, row = state.replica.sequence:position("file:" .. file.id)
      vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
      return file
    end
  end
  error("missing " .. section .. " " .. path)
end

local function source_row(text, select)
  for id, block in pairs(state.replica.block) do
    for offset, row in ipairs(block.text or {}) do
      if row:find(text, 1, true) then
        if select then
          local _, start = state.replica.sequence:position(id)
          vim.api.nvim_win_set_cursor(0, { start + offset, 0 })
        end
        return true
      end
    end
  end
  return false
end

local function audit(require_gutters)
  vim.cmd.ForgeFixtureAudit()
  local report = vim.json.decode(table.concat(vim.fn.readfile(require("forge.builder").artifact_root() .. "/manual-status-audit.json"), "\n"))
  assert(report.orphans == 0 and #report.misplaced == 0, "gutter corruption after rejection")
  if require_gutters then assert(report.gutters > 0, "rollback audit did not exercise gutters") end
end

local ok, failure = xpcall(function()
  await(settled, "open")
  select_file("source_01.rs", "unstaged")
  key("<Tab>")
  await(function() return settled() and source_row("let value_31 = 1031;") end, "load hunk")
  assert(source_row("let value_31 = 1031;", true))
  local path = forge_manual.fixture .. "/source_01.rs"
  local rows = vim.fn.readfile(path)
  local scenario = vim.g.forge_source_change_case or "selected"
  local expected, retired
  local stamp = assert(vim.uv.fs_stat(path))
  if scenario == "outside" then
    rows[52] = "        let value_50 = 900050;"
    expected = "let value_50 = 900050;"
  elseif scenario == "same_size" then
    rows[33] = "        let value_31 = 9031;"
    expected, retired = "let value_31 = 9031;", "let value_31 = 1031;"
  else
    rows[33] = "        let value_31 = 900031;"
    expected, retired = "let value_31 = 900031;", "let value_31 = 1031;"
  end
  vim.fn.writefile(rows, path)
  assert(vim.uv.fs_utime(path, stamp.atime.sec, stamp.mtime.sec + 2))
  if scenario == "same_size" then assert(vim.uv.fs_stat(path).size == stamp.size, "fixture size changed") end
  key("S")
  await(function()
    for _, notice in ipairs(forge_manual.notices) do
      if notice.message:find("selected source changed", 1, true) then return settled() end
    end
    return false
  end, "reject stale hunk")
  assert(git({ "diff", "--cached", "--name-only" }) == "", "stale hunk changed index")
  select_file("source_01.rs", "unstaged")
  if vim.fn.foldclosed(vim.api.nvim_win_get_cursor(0)[1]) >= 0 then key("<Tab>") end
  await(function() return settled() and source_row(expected) end, "refresh changed source")
  if retired then assert(not source_row(retired), "retained stale hunk body") end
  audit(true)
  rows[33] = "        let value_31 = 700031;"
  vim.fn.writefile(rows, path)
  select_file("source_01.rs", "unstaged")
  key("S")
  await(settled, "stage current whole file")
  assert(git({ "show", ":source_01.rs" }) == table.concat(rows, "\n") .. "\n", "whole file staged stale bytes")
  select_file("source_01.rs", "staged")
  key("U")
  await(settled, "unstage current file")
  assert(git({ "diff", "--cached", "--name-only" }) == "")
  local large = string.rep("x", 9 * 1024 * 1024)
  local handle = assert(io.open(forge_manual.fixture .. "/large.bin", "wb"))
  handle:write(large)
  handle:close()
  key("R")
  await(function()
    if not settled() then return false end
    for _, file in ipairs(state.replica.inventory.file) do if file.path == "large.bin" then return true end end
  end, "discover large file")
  select_file("large.bin", "untracked")
  key("S")
  await(settled, "stage large file")
  assert(git({ "show", ":large.bin" }) == large, "large file index bytes differ")
  select_file("large.bin", "staged")
  key("U")
  await(settled, "unstage large file")
  assert(git({ "diff", "--cached", "--name-only" }) == "")
  audit(false)
  local rejected = 0
  for _, notice in ipairs(forge_manual.notices) do
    if notice.message:find("selected source changed", 1, true) then rejected = rejected + 1
    else assert(notice.message:find("no Copilot OAuth token found", 1, true), notice.message) end
  end
  assert(rejected == 1, "unexpected rejection count")
  print("source changes: stale hunk rollback, fresh body, current whole-file bytes, and 9 MiB staging passed")
end, debug.traceback)
require("forge.status").close(state)
client.stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not exit")
vim.fn.chdir(config)
assert(ok, failure)
