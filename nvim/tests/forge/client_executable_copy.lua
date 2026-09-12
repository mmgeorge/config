vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local builder = require("forge.builder")
local client = require("forge.client")
local original_ensure, original_acquire, original_system = builder.ensure, builder.acquire, vim.system
---@type fun(lease?: RustSidecarLease, failure?: string)?
local complete_copy
local released, spawned, completed = 0, 0, 0
local succeeded, failure = xpcall(function()
  builder.ensure = function(callback) callback({ ok = true, path = "fixture" }) end
  builder.acquire = function(_, callback) complete_copy = callback end
  vim.system = function() spawned = spawned + 1 error("cancelled startup spawned a process") end
  client.start(function(_, start_error)
    assert(start_error, "cancelled startup succeeded")
    completed = completed + 1
  end)
  assert(vim.wait(1000, function() return complete_copy ~= nil end, 5))
  client.stop()
  complete_copy({ path = "copy", release = function() released = released + 1 end })
  assert(released == 1 and spawned == 0 and completed == 1, "cancelled copy lost lifecycle ownership")
  client._reset_for_test()
  complete_copy = nil
  local copy_failure
  client.start(function(_, start_error) copy_failure = start_error end)
  assert(vim.wait(1000, function() return complete_copy ~= nil end, 5))
  complete_copy(nil, "copy denied")
  assert(copy_failure == "copy denied" and spawned == 0, "copy failure did not reach startup caller")
  client._reset_for_test()
  complete_copy = nil
  vim.system = function() error("fixture spawn failure") end
  local spawn_failure
  client.start(function(_, start_error) spawn_failure = start_error end)
  assert(vim.wait(1000, function() return complete_copy ~= nil end, 5))
  complete_copy({ path = "copy", release = function() released = released + 1 end })
  assert(spawn_failure and spawn_failure:find("fixture spawn failure", 1, true))
  assert(released == 2, "spawn failure retained executable copy")
end, debug.traceback)
builder.ensure, builder.acquire, vim.system = original_ensure, original_acquire, original_system
client._reset_for_test()
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("executable copy lifecycle passed")
vim.cmd("qa!")
