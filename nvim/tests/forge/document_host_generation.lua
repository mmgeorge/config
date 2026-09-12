local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local generation, accepting = 1, true
local requests, notices = {}, {}
local delayed
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
package.loaded["forge.client"] = {
  host_generation = function() return generation end,
  host_accepting = function() return accepting end,
  request_host = function(method, params, callback)
    assert(accepting, "document request resurrected a stopped host")
    requests[#requests + 1] = { method, params.operation }
    if params.operation == "open" then
      local snapshot = { document = params.document, revision = 1, block = {
        { id = "source", text = { "captured source" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
      } }
      if method == "status" then callback(fixture.snapshot(params.document, { revision = 1 }))
      else callback({ title = "HEAD", object = "HEAD", revision = "HEAD", snapshot = snapshot, more = false }) end
    elseif params.operation == "refresh" then delayed = callback
    elseif params.operation == "close_view" then callback(nil)
    else callback({}) end
  end,
}
local status = require("forge.status")
local source = require("forge.source_document")
local parent = status.open({ workspace = vim.fn.getcwd() })
assert(vim.wait(1000, function() return parent.replica.status == "Applied" and not parent.request_active end))
local child = source.open({ workspace = vim.fn.getcwd(), path = "history.txt", revision = "HEAD" })
assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == child.replica.buffer end))
accepting = false
local before = #requests
source.close(child)
assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == parent.replica.buffer end))
vim.wait(30, function() return false end)
assert(#requests == before, "source close or status redisplay contacted a dead host")
assert(#notices == 0, table.concat(notices, "\n"))
generation, accepting = 2, true
status.demand(parent)
status.refresh(parent)
assert(vim.wait(1000, function() return #notices > 0 end))
assert(notices[#notices]:find("refresh or reopen", 1, true))
assert(#requests == before, "stale status request reached the replacement host")
local notice_count = #notices
status.action(parent, "open")
assert(#notices == notice_count + 1 and notices[#notices]:find("refresh or reopen", 1, true))
status.close(parent)
assert(vim.wait(1000, function() return not parent.request_active end))
local replacement = status.open({ workspace = vim.fn.getcwd() })
assert(vim.wait(1000, function() return replacement.replica.status == "Applied" and not replacement.request_active end))
assert(replacement.host_generation == 2)
status.refresh(replacement)
assert(delayed)
generation = 3
delayed({ document = replacement.document, base = 1, next = 2 })
assert(vim.wait(1000, function() return not replacement.request_active end))
assert(replacement.replica.revision == 1, "old-generation response modified the replica")
status.close(replacement)
assert(vim.wait(1000, function() return not replacement.request_active end))
local late_source = source.open({ workspace = vim.fn.getcwd(), path = "late.txt", revision = "HEAD" })
generation = 4
assert(vim.wait(1000, function() return not late_source.active end))
assert(late_source.replica.status == "Closed", "late source open remained usable after host collection")
print("native document host generation lifecycle passed")
