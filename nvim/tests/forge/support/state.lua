-- Host fixtures read canonical session state through the controller's synchronization path.
--
-- `session.state.get` is the reconnect primitive, not a passive read. Its response carries a
-- retained `owner_publication` page, and the host records that page as delivered once the
-- response is admitted to output. A fixture that issues the request directly and discards the
-- page therefore consumes events, such as `activity_changed`, that the controller never sees.
-- `controller.synchronize_state` sends the client's publication cursor and replays the page
-- through `snapshot.apply`, so state polling cannot hide events from the presentation.
local M = {}

---Synchronize the Harness presentation with the host and return the applied snapshot.
---@param timeout? integer Milliseconds to wait for the snapshot; defaults to 10000.
---@return table snapshot
function M.get(timeout)
  local settled, snapshot, failure
  require("forge.views.harness.controller").synchronize_state(function(result, request_error)
    settled, snapshot, failure = true, result, request_error
  end)
  assert(vim.wait(timeout or 10000, function() return settled end, 10), "state synchronization timeout")
  assert(not failure, "session.state.get: " .. tostring(failure))
  return snapshot
end

return M
