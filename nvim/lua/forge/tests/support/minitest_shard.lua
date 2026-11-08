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

-- A case's label is its description below the file name, such as `group | case`.
local function case_label(case) return table.concat(case.desc, " | ", 2) end

-- The runner names top-level entries, so a name selects that case, or every case of that set,
-- including each parametrization.
local function selects(case)
  if case_name == nil then return true end
  return case.desc[2] == case_name
end

local cases = MiniTest.collect({
  find_files = function() return { test_path } end,
  filter_cases = selects,
})
if #cases == 0 then
  io.stderr:write(("No mini.test case in %s matches %s\n"):format(test_path, tostring(case_name)))
  os.exit(1)
end

local deadline = assert(vim.uv.new_timer())
local stdout_reporter = MiniTest.gen_reporter.stdout({ quit_on_finish = false })
local started_case_number, finished = nil, false
local reporter = {
  start = stdout_reporter.start,
  update = function(case_number)
    if case_number ~= started_case_number then
      started_case_number = case_number
      local label = case_label(cases[case_number])
      deadline:start(case_deadline_ms, 0, function()
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
