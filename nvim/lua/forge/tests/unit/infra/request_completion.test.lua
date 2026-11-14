local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local deferred = dofile("nvim/lua/forge/tests/support/deferred.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local eq = MiniTest.expect.equality
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["response_cancels_timeout_and_settles_once"] = function()
  local timers = deferred.install({ 30000 })
  local results, releases = {}, 0
  local receive = require("forge.infra.request_completion").new({
    timeout_ms = 30000, timeout_message = "delayed",
    complete = function(result) results[#results + 1] = result end,
    failed = function(failure) error(failure) end,
    finally = function() releases = releases + 1 end,
  })
  receive("first")
  receive("late")
  eq(timers.fire(30000), 0)
  eq(results, { "first" })
  eq(releases, 1)
end

T["timeout_uses_closed_timer_and_ignores_late_response"] = function()
  local timers = deferred.install({ 30000 })
  local results, releases = {}, 0
  local receive = require("forge.infra.request_completion").new({
    timeout_ms = 30000, timeout_message = "delayed",
    complete = function(result, failure, detail) results[#results + 1] = { result, failure, detail.code } end,
    failed = function(failure) error(failure) end,
    finally = function() releases = releases + 1 end,
  })
  eq(timers.fire(30000), 1)
  receive("late")
  eq(results, { { nil, "delayed", "outcome_unknown" } })
  eq(releases, 1)
end

T["callback_and_failure_reporter_errors_release_ownership"] = function()
  deferred.install({ 30000 })
  local releases, notices = 0, {}
  stub(vim.api, "nvim_echo", function(message) notices[#notices + 1] = message[1][1] end)
  local receive = require("forge.infra.request_completion").new({
    timeout_ms = 30000, timeout_message = "delayed",
    complete = function() error("callback failed") end,
    failed = function() error("reporter failed") end,
    finally = function() releases = releases + 1 end,
  })
  receive({})
  receive({})
  eq(releases, 1)
  eq(#notices, 1)
  eq(notices[1]:find("callback failed", 1, true) ~= nil, true)
  eq(notices[1]:find("reporter failed", 1, true) ~= nil, true)
end

return T
