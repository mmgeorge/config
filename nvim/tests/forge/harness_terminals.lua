vim.opt.runtimepath:prepend("nvim")
local request, callback, notices, updates = 0, nil, {}, {}
local alive = true
package.loaded["forge.client"] = { request_for = function(session, method, params, receive)
  assert(session == "session" and method == "harness.document" and params.operation == "background_terminals")
  request, callback = request + 1, receive
end }
local watch = require("forge.views.harness.terminals").watch({
  session_id = "session", alive = function() return alive end,
  update = function(snapshot) updates[#updates + 1] = snapshot end,
  notice = function(message) notices[#notices + 1] = message end,
})
assert(vim.wait(100, function() return callback ~= nil end, 5))
watch.refresh()
assert(request == 1, "overlapping queries admitted")
callback({ supported = true, terminal = { { id = "1", command = "test" } } })
watch.refresh()
callback(nil, "offline")
assert(updates[#updates].unavailable and #notices == 1)
watch.refresh()
callback(nil, "offline")
assert(#notices == 1, "repeated failure spammed notifications")
watch.refresh()
callback({ supported = true, terminal = {} })
assert(#updates[#updates].terminal == 0, "terminal exit did not clear inventory")
watch.refresh()
local count = #updates
watch.close()
callback({ supported = true, terminal = { { id = "late" } } })
assert(#updates == count, "late response changed closed session")
watch.refresh()
assert(request == 5)
local unsupported = require("forge.views.harness.terminals").watch({
  session_id = "session", alive = function() return true end,
  update = function() end, notice = function() error("unexpected notice") end,
})
unsupported.refresh()
callback({ supported = false, terminal = {} })
unsupported.refresh()
assert(request == 6, "unsupported provider kept polling")
print("harness_terminals: passed")
vim.cmd("qa!")
