-- Runs one mini.test file as a shard: every case in the file, or the single case the runner names,
-- sequentially in this headless Neovim. `.forge/checks/lua-tests.nu` launches it as
-- `nvim ... -l nvim/lua/forge/tests/support/minitest_shard.lua <test file> [case]`, with the pinned
-- mini.test checkout on 'runtimepath', and runs shards in parallel with one another.
--
-- mini.test executes cases through `vim.schedule` and has no per-case deadline, so this script
-- keeps the event loop running until the reporter finishes, arms a five-second deadline for each
-- case as it starts, and exits with the shard's status. A case that blocks the event loop never lets
-- the deadline fire; the runner's outer kill bounds that case instead.
local test_path, case_name = arg[1], arg[2]
assert(test_path, "usage: minitest_shard.lua <test file> [case]")

local case_deadline_ms = 5000
local MiniTest = require("mini.test")
-- An empty script path keeps mini.test from sourcing a project `scripts/minitest.lua` instead.
MiniTest.setup({ script_path = "", silent = true })

-- On Windows, mini.test names a child's `--listen` pipe after the tail of `tempname()`, a counter
-- that restarts in every process, so concurrent shards collide on `\\.\pipe\mininvim1` and a case
-- can connect to another shard's child. The process ID makes the name unique while a child starts.
local new_child_neovim = MiniTest.new_child_neovim
MiniTest.new_child_neovim = function()
  local child = new_child_neovim()
  local start = child.start
  child.start = function(...)
    local tempname = vim.fn.tempname
    vim.fn.tempname = function() return tempname() .. "-" .. vim.fn.getpid() end
    local ok, result = pcall(start, ...)
    vim.fn.tempname = tempname
    if not ok then error(result, 0) end
    return result
  end
  return child
end

-- A case's label is its description below the file name, such as `group | case`.
local function case_label(case) return table.concat(case.desc, " | ", 2) end

local collector = dofile("nvim/lua/forge/tests/support/minitest_collect.lua")
local cases = collector.collect(MiniTest, test_path, case_name)
if vim.env.FORGE_TEST_COLLECT then
  local file = assert(io.open(vim.env.FORGE_TEST_COLLECT, "w"))
  assert(file:write(vim.json.encode(collector.items(cases, test_path))))
  assert(file:close())
  os.exit(0)
end
if #cases == 0 then
  io.stderr:write(("No mini.test case in %s matches %s\n"):format(test_path, tostring(case_name)))
  os.exit(1)
end

local deadline = assert(vim.uv.new_timer())
local event_reporter
if vim.env.FORGE_TEST_EVENTS then
  local event_path = vim.env.FORGE_TEST_EVENTS
  event_reporter = dofile("nvim/lua/forge/tests/support/minitest_events.lua").new(cases, test_path, function(event)
    local file = assert(io.open(event_path, "a"))
    assert(file:write(vim.json.encode(event) .. "\n"))
    assert(file:flush())
    assert(file:close())
  end, vim.uv.hrtime)
end
local stdout_reporter = MiniTest.gen_reporter.stdout({ quit_on_finish = false })
local started_case_number, finished = nil, false
local reporter = {
  start = stdout_reporter.start,
  update = function(case_number)
    if event_reporter then event_reporter:update(case_number) end
    if case_number ~= started_case_number then
      started_case_number = case_number
      local label = case_label(cases[case_number])
      if vim.env.FORGE_TEST_PROGRESS then
        local file = assert(io.open(vim.env.FORGE_TEST_PROGRESS, "w"))
        assert(file:write(vim.json.encode({ case = label, index = case_number })))
        assert(file:close())
      end
      deadline:start(case_deadline_ms, 0, function()
        if event_reporter then event_reporter:timeout() end
        io.stdout:write(("\nTimed out after %d ms in case %s\n"):format(case_deadline_ms, label))
        io.stdout:flush()
        os.exit(124)
      end)
    end
    stdout_reporter.update(case_number)
  end,
  finish = function()
    deadline:stop()
    stdout_reporter.finish()
    finished = true
  end,
}

MiniTest.execute(cases, { reporter = reporter })
while not finished do vim.wait(1000, function() return finished end, 10) end

local failed = vim.iter(cases):any(function(case) return #case.exec.fails > 0 end)
os.exit(failed and 1 or 0)
