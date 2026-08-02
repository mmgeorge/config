package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local coordinator = require("diff_review.git.mutation_coordinator")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual), 2)
  end
end

local deferred_callback_list = {}
local original_defer_fn = vim.defer_fn
vim.defer_fn = function(callback)
  deferred_callback_list[#deferred_callback_list + 1] = callback
end

local function run_deferred()
  local callback_list = deferred_callback_list
  deferred_callback_list = {}
  for _, callback in ipairs(callback_list) do callback() end
end

local function test_repository_fifo_and_quiet_sync()
  coordinator.reset_for_test()
  local event_list = {}
  local first_done = nil
  local settle_done = nil
  coordinator.set_handler("repo-a", {
    settle = function(burst, done)
      event_list[#event_list + 1] = "settle:" .. table.concat(coordinator.paths(burst), ",")
      settle_done = done
    end,
    recover = function(_, done) done(true) end,
  })

  coordinator.enqueue("repo-a/.", {
    label = "first",
    paths = { "a.txt" },
    execute = function(done)
      event_list[#event_list + 1] = "first"
      first_done = done
    end,
  })
  coordinator.enqueue("repo-a", {
    label = "second",
    paths = { "b.txt" },
    execute = function(done)
      event_list[#event_list + 1] = "second"
      done({ ok = true })
    end,
  })
  coordinator.enqueue("repo-b", {
    label = "independent",
    paths = { "c.txt" },
    execute = function(done)
      event_list[#event_list + 1] = "independent"
      done({ ok = true })
    end,
  })

  assert_equal(table.concat(event_list, ","), "first,independent", "repositories should execute independently")
  first_done({ ok = true })
  assert_equal(table.concat(event_list, ","), "first,independent,second", "one repository should execute FIFO")
  run_deferred()
  assert_equal(event_list[#event_list], "settle:a.txt,b.txt", "quiet sync should receive the unioned path set")

  local later_started = false
  coordinator.enqueue("repo-a", {
    label = "later",
    paths = { "a.txt" },
    execute = function(done)
      later_started = true
      done({ ok = true })
    end,
  })
  assert_true(not later_started, "a later burst should wait for the prior authoritative sync")
  settle_done(true)
  assert_true(later_started, "a later burst should start after sync commits the prior baseline")
end

local function test_failure_cancels_burst_and_blocks_recovery()
  coordinator.reset_for_test()
  deferred_callback_list = {}
  local first_done = nil
  local recovery_done = nil
  local cancelled = false
  local recovered_burst = nil
  coordinator.set_handler("repo", {
    settle = function(_, done) done(true) end,
    recover = function(burst, done)
      recovered_burst = burst
      recovery_done = done
    end,
  })
  coordinator.enqueue("repo", {
    label = "fails",
    paths = { "one.txt" },
    execute = function(done) first_done = done end,
  })
  coordinator.enqueue("repo", {
    label = "cancelled",
    paths = { "two.txt" },
    execute = function() error("cancelled task executed") end,
    on_cancel = function() cancelled = true end,
  })

  first_done({ ok = false, error = "apply failed" })
  assert_true(cancelled, "the first failure should cancel the rest of its burst")
  assert_true(recovered_burst ~= nil, "failure should enter authoritative recovery")
  assert_equal(#recovered_burst.cancelled_tasks, 1, "recovery should report cancelled work")
  local task_id, _, enqueue_error = coordinator.enqueue("repo", {
    label = "blocked",
    paths = { "three.txt" },
    execute = function() error("recovery-blocked task executed") end,
  })
  assert_true(task_id == nil and enqueue_error ~= nil, "new work should stay blocked during recovery")
  recovery_done(true)
  assert_true(not coordinator.pending("repo"), "recovery completion should release the repository")
end

local function test_handler_completion_callbacks_are_one_shot()
  coordinator.reset_for_test()
  deferred_callback_list = {}
  local first_settle_done = nil
  local settle_count = 0
  local later_execute_count = 0
  coordinator.set_handler("settle-repo", {
    settle = function(_, done)
      settle_count = settle_count + 1
      if settle_count == 1 then
        first_settle_done = done
      else
        done(true)
      end
    end,
    recover = function(_, done) done(true) end,
  })
  coordinator.enqueue("settle-repo", {
    label = "first",
    paths = { "one.txt" },
    execute = function(done) done({ ok = true }) end,
  })
  run_deferred()
  coordinator.enqueue("settle-repo", {
    label = "later",
    paths = { "two.txt" },
    execute = function(done)
      later_execute_count = later_execute_count + 1
      done({ ok = true })
    end,
  })

  first_settle_done(true)
  first_settle_done(true)
  assert_equal(later_execute_count, 1, "a repeated settle callback should not execute later work twice")
  assert_equal(#deferred_callback_list, 1, "a repeated settle callback should not reschedule the next quiet sync")
  run_deferred()
  assert_equal(settle_count, 2, "the later burst should settle exactly once")
  assert_true(not coordinator.pending("settle-repo"), "the settle one-shot test should drain the repository")

  coordinator.reset_for_test()
  deferred_callback_list = {}
  local recovery_done = nil
  local recovery_count = 0
  later_execute_count = 0
  coordinator.set_handler("recovery-repo", {
    settle = function(_, done) done(true) end,
    recover = function(_, done)
      recovery_count = recovery_count + 1
      recovery_done = done
    end,
  })
  coordinator.enqueue("recovery-repo", {
    label = "failure",
    paths = { "one.txt" },
    execute = function(done) done({ ok = false, error = "failed" }) end,
  })
  recovery_done(true)
  coordinator.enqueue("recovery-repo", {
    label = "later",
    paths = { "two.txt" },
    execute = function(done)
      later_execute_count = later_execute_count + 1
      done({ ok = true })
    end,
  })
  recovery_done(true)
  assert_equal(recovery_count, 1, "a repeated recovery callback should not re-enter recovery")
  assert_equal(later_execute_count, 1, "a repeated recovery callback should not execute later work twice")
  assert_equal(#deferred_callback_list, 1, "a repeated recovery callback should not reschedule the next quiet sync")
  run_deferred()
  assert_true(not coordinator.pending("recovery-repo"), "the recovery one-shot test should drain the repository")
end

local test_ok, test_error = pcall(function()
  test_repository_fifo_and_quiet_sync()
  test_failure_cancels_burst_and_blocks_recovery()
  test_handler_completion_callbacks_are_one_shot()
end)
vim.defer_fn = original_defer_fn
coordinator.reset_for_test()
if not test_ok then error(test_error, 0) end

print("mutation_coordinator OK")
vim.cmd("qa!")
