local config = vim.fn.getcwd()
dofile(config .. "/nvim/tests/forge/fixtures/status_contention_manual.lua")
local client = require("forge.client")
local state = forge_manual.state

local function settled()
  return state.ready and state.replica.status == "Applied" and #(state.replica.inventory.pending or {}) == 0
    and not state.pending and not state.request_active and next(client._client.pending) == nil
end

local function await(phase)
  assert(vim.wait(15000, settled, 10), phase .. ": " .. vim.inspect(forge_manual.notices))
  assert(#forge_manual.notices == 0, phase .. ": " .. vim.inspect(forge_manual.notices))
end

local function select_file(section)
  for _, file in ipairs(state.replica.inventory.file) do
    if file.path == "source_01.rs" and file.section == section then
      local _, row = state.replica.sequence:position("file:" .. file.id)
      vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
      return
    end
  end
  error("missing " .. section .. " source_01.rs")
end

local function action(key, section)
  select_file(section)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "xt", false)
  await(key .. " on " .. section)
end

local function index_paths()
  local result = vim.system({ "git", "-C", forge_manual.fixture, "diff", "--cached", "--name-only" },
    { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout:gsub("\r", "")
end

local ok, failure = xpcall(function()
  await("open")
  action("U", "unstaged")
  assert(index_paths() == "", "redundant unstage changed the index")
  action("S", "unstaged")
  assert(index_paths() == "source_01.rs\n", "stage missed its selected file")
  action("S", "staged")
  action("S", "staged")
  assert(index_paths() == "source_01.rs\n", "redundant stage changed the index")
  action("U", "staged")
  action("U", "unstaged")
  assert(index_paths() == "", "unstage did not restore the empty index")
  action("S", "unstaged")
  action("U", "staged")
  assert(index_paths() == "", "ordinary input failed after redundant actions")
end, debug.traceback)
require("forge.status").close(state)
client.host_stop()
assert(vim.wait(6000, function() return client._client.process == nil end, 20), "host did not exit")
vim.fn.chdir(config)
assert(ok, failure)
print("status_staging_admission_host: passed")
