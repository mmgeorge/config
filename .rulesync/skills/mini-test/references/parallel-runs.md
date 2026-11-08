# Parallel runs

*When to read this:* When a mini.test suite must run faster than one sequential process allows, when CI splits a suite across jobs, or when each case needs a deadline.

## Contents

- What mini.test provides
- Choosing a shard
- Shard bootstrap
- Outer scheduler
- Isolation and failure semantics

## What mini.test provides

mini.test has no parallel execution and no per-case timeout; its help lists parallelism as unsupported to limit implementation complexity, and mini.nvim's `make test` runs the whole suite in one process. The documented building blocks for running a subset are `collect.find_files`, `collect.filter_cases`, `MiniTest.run_file`, and `MiniTest.run_at_location`, and the stdout reporter's `cquit` exit status. Parallelism therefore comes from running several headless Neovim processes, each collecting a subset, under a scheduler outside mini.test.

The simplest form is splitting CI jobs by test directory, as grug-far.nvim does with `make test dir=base` and `make test dir=astgrep` in separate jobs. Finer sharding needs a scheduler.

## Choosing a shard

A shard is the set of cases one process runs:

- **One file per process** is the usual choice. A file is the unit `MiniTest.run_file` and the `make test_<module>` convention already use, its hooks were written to run together, and process startup is paid once per file.
- **One top-level set per process** (`filter_cases` on `case.desc[2]`) splits a large file, at the cost of re-running its `pre_once` hooks in every shard.
- **One case per process** gives full isolation and lets the scheduler apply an exact deadline, but pays a Neovim startup, roughly 50 ms, per case, and exposes every hidden dependency between cases.

Bound concurrency below the CPU count when cases start children or external processes, since each case may hold several processes.

On Windows, concurrent shards that start child Neovims collide on the child's `--listen` pipe, because `child.start` names it after a `tempname()` counter that restarts in every process; the [child reference](child-neovim.md) shows the workaround. Install it in the shard bootstrap so that every test file inherits it.

## Shard bootstrap

Run each shard with `nvim --headless --noplugin -u scripts/minimal_init.lua -l tests/shard.lua <file> [top-level name]`. The bootstrap collects and filters cases itself, keeps the event loop alive until the reporter finishes, arms a deadline per case, and exits with the shard's status:

```lua
local test_path, entry_name = arg[1], arg[2]
local case_deadline_ms = 5000
local MiniTest = require("mini.test")
MiniTest.setup({ script_path = "", silent = true })

local cases = MiniTest.collect({
  find_files = function() return { test_path } end,
  filter_cases = function(case) return entry_name == nil or case.desc[2] == entry_name end,
})
if #cases == 0 then
  io.stderr:write(("No case in %s matches %s\n"):format(test_path, tostring(entry_name)))
  os.exit(1)
end

local deadline = assert(vim.uv.new_timer())
local stdout_reporter = MiniTest.gen_reporter.stdout({ quit_on_finish = false })
local started_case_number, finished = nil, false
MiniTest.execute(cases, {
  reporter = {
    start = stdout_reporter.start,
    update = function(case_number)
      if case_number ~= started_case_number then
        started_case_number = case_number
        local label = table.concat(cases[case_number].desc, " | ", 2)
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
  },
})
while not finished do vim.wait(1000, function() return finished end, 10) end
os.exit(vim.iter(cases):any(function(case) return #case.exec.fails > 0 end) and 1 or 0)
```

- `nvim -l` exits when the script returns, and `MiniTest.execute` only schedules work, so the `vim.wait` loop is what keeps the process alive until `finish`.
- `script_path = ""` stops `setup` defaults from sourcing a project `scripts/minitest.lua`.
- The reporter's `update` fires on every state change of a case, so the deadline is re-armed only when the case number changes.
- The timer fires only while the event loop runs. A case stuck in a busy Lua loop or a blocking RPC never yields, so the scheduler's process kill must back it up.
- Checking for an empty selection matters: mini.test versions before the fix for nvim-mini/mini.nvim#2538 hang headlessly when a filter collects no cases.

## Outer scheduler

The scheduler launches shards with bounded concurrency, collects each exit status and output, and kills any shard that outlives its deadline together with its process tree, which includes child Neovims and any processes the cases started. The case count of a shard is known only after collection, because nested sets and parametrization multiply cases, but the stdout reporter prints `Total number of cases: N` at start. A scheduler can begin with a one-case deadline and extend it to N case deadlines once that line appears, plus a short grace period so the shard reports its own timeout before the kill. The stdout reporter colors its output with hard-coded ANSI escapes; strip them before storing output in reports.

## Isolation and failure semantics

- Cases in one shard share a process, so the isolation checklist in the main skill applies inside every shard. Prove that a sharded suite does not depend on order by running each shard alone and the full suite.
- A deadline exit ends its shard: later cases in that shard never run, and their absence is not a pass. Report the timed-out case and fix it before trusting the shard's other results.
- Selection by top-level name keeps every case of a nested set or parametrization together, so their `*_once` hooks run once per shard as written.
