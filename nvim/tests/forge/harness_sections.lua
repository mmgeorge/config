vim.loader.enable(false)
local client = require("forge.client")
local expanded, revision = false, 0
local requests, notices = {}, {}
local held, deferred = {}, false
local owner
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
local function snapshot(document)
  return { document = document, revision = revision, block = {
    { id = "heading", text = { "Tools" }, metadata = {
      target = {}, decoration = {}, editable_region = {},
      section = { { id = "tools", revision = revision, open = expanded, more = false } },
      fold = { { id = "tools", start = { row = 0, column = 0 },
        ["end"] = { block = "body", position = { row = 1, column = 0 } }, closed = true } },
    } },
    { id = "body", text = { expanded and "loaded only on demand" or "" },
      metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }
end
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  requests[#requests + 1] = params
  if deferred and (params.operation == "section_expansion" or params.operation == "sync") then
    held[#held + 1] = { params = params, callback = callback }
    return
  end
  if params.operation == "background_terminals" then callback({ supported = false })
  elseif params.operation == "open" then
    vim.schedule(function() callback({ transcript = snapshot(params.document) }) end)
  elseif params.operation == "section_expansion" then
    expanded = params.expanded
    revision = revision + 1
    callback({})
  elseif params.operation == "sync" then callback({ snapshot = snapshot(params.document) })
  else callback({}) end
end
local function respond(failure)
  local pending = assert(table.remove(held, 1), "no held request")
  if failure then pending.callback(nil, failure)
  elseif pending.params.operation == "section_expansion" then
    expanded = pending.params.expanded
    revision = revision + 1
    pending.callback({})
  else pending.callback({ snapshot = snapshot(pending.params.document) }) end
  return pending.params.operation
end
local function body_loaded()
  return table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n"):find("loaded only", 1, true) ~= nil
end
local ok, failure = xpcall(function()
  owner = require("forge.views.harness.presentation").open({
    session_id = "sections", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) notices[#notices + 1] = message end,
  }, function() end)
  assert(vim.wait(1000, function() return owner.ready end, 1))
  assert(vim.fn.foldclosed(1) == 1)
  assert(not table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n"):find("loaded only", 1, true))
  deferred = true
  assert(owner.toggle_heading(window))
  assert(vim.fn.foldclosed(1) == 1, "unloaded fold opened before delivery")
  assert(owner.transcript.fold_loading[window].tools, "opening did not publish loading state")
  assert(vim.fn.foldtextresult(1):find("Loading", 1, true), "closed heading omitted loading state")
  assert(not body_loaded())
  assert(respond() == "section_expansion")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(vim.fn.foldclosed(1) == 1, "fold opened on acknowledgement before content")
  assert(not body_loaded())
  assert(respond() == "sync")
  assert(vim.wait(1000, function()
    return body_loaded() and vim.fn.foldclosed(1) == -1
  end, 1), "opening the native fold did not demand its body")
  assert(not owner.transcript.fold_loading[window], "loaded fold retained loading state")
  deferred = false
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function()
    return not body_loaded()
  end, 1), "closing the native fold retained its body")
  deferred = true
  assert(owner.toggle_heading(window))
  assert(owner.toggle_heading(window))
  assert(not owner.transcript.fold_loading[window], "second toggle did not cancel loading")
  assert(respond() == "section_expansion")
  assert(respond() == "section_expansion")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(vim.wait(1000, function() return not owner.syncing and not owner.applying end, 1))
  assert(vim.fn.foldclosed(1) == 1 and not body_loaded(), "cancelled delivery reopened the fold")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(owner.toggle_heading(window))
  respond("fixture delivery failure")
  assert(vim.fn.foldclosed(1) == 1 and not body_loaded())
  assert(not owner.transcript.fold_loading[window], "failed request retained loading state")
  assert(#notices == 1 and notices[1]:find("fixture delivery failure", 1, true))
  deferred = false
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return body_loaded() and vim.fn.foldclosed(1) == -1 end, 1), "failed opening was not retryable")
  local sequence = 0
  for _, request in ipairs(requests) do
    if request.operation == "section_expansion" then
      assert(request.sequence > sequence)
      sequence = request.sequence
    end
  end
end, debug.traceback)
if owner then owner.close() end
assert(ok, failure)
print("harness_sections: passed")
vim.cmd("qa!")
