vim.loader.enable(false)
local client = require("forge.client")
local expanded, revision, generation = false, 0, 1
local requests, notices = {}, {}
local held, deferred = {}, false
local owner
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_buf(window, transcript)
local function snapshot(document)
  local value = { document = document, revision = revision, block = {
    { id = "heading", text = { "Tools" }, metadata = {
      node = { id = "tools", kind = "tool_group", lifecycle = "settled", generation = generation, order = 0, more = false,
        content_revision = revision, loaded_rows = expanded and 1 or 0, loaded_bytes = 0,
        display = expanded and "full" or "heading", default_display = "heading", expansion = expanded },
      target = {}, decoration = {}, editable_region = {},
      section = { { id = "tools", revision = revision, open = expanded, more = false } },
      fold = { { id = "tools", start = { row = 0, column = 0 },
        ["end"] = { block = expanded and "body" or "heading", position = { row = 1, column = 0 } }, closed = not expanded } },
    } },
    { id = "body", text = { expanded and "loaded only on demand" or "" },
      metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }
  if not expanded then table.remove(value.block) end
  return value
end
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  requests[#requests + 1] = params
  if deferred and (params.operation == "node" or params.operation == "sync") then
    held[#held + 1] = { params = params, callback = callback }
    return
  end
  if params.operation == "background_terminals" then callback({ supported = false })
  elseif params.operation == "open" then
    vim.schedule(function() callback({ transcript = snapshot(params.document) }) end)
  elseif params.operation == "node" then
    expanded = params.expanded
    revision = revision + 1
    callback({})
  elseif params.operation == "sync" then callback({ snapshot = snapshot(params.document) })
  else callback({}) end
end
local function respond(failure)
  local pending = assert(table.remove(held, 1), "no held request")
  if failure then pending.callback(nil, failure, { code = "request_failed" })
  elseif pending.params.operation == "node" then
    if pending.params.generation == generation then expanded = pending.params.expanded end
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
  assert(owner.transcript.block.heading.metadata.node.display == "heading")
  assert(not table.concat(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), "\n"):find("loaded only", 1, true))
  local loading_namespace = vim.api.nvim_get_namespaces()["ForgeHarnessLoading" .. transcript]
  local collapsed_lines = vim.api.nvim_buf_get_lines(transcript, 0, -1, false)
  deferred = true
  assert(owner.toggle_heading(window))
  assert(owner.transcript.block.heading.metadata.node.display == "heading", "unloaded fold opened before delivery")
  assert(owner.transcript.fold_loading[window].tools, "opening did not publish loading state")
  assert(#vim.api.nvim_buf_get_extmarks(transcript, loading_namespace, 0, -1, {}) == 0,
    "pending expansion displayed a loading marker")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(transcript, 0, -1, false), collapsed_lines),
    "pending expansion replaced the collapsed heading")
  assert(not body_loaded())
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(owner.transcript.block.heading.metadata.node.display == "heading", "fold opened on acknowledgement before content")
  assert(#vim.api.nvim_buf_get_extmarks(transcript, loading_namespace, 0, -1, {}) == 0,
    "acknowledged expansion displayed a loading marker before content arrived")
  assert(not body_loaded())
  assert(respond() == "sync")
  assert(vim.wait(1000, function()
    return body_loaded() and owner.transcript.block.heading.metadata.node.display == "full"
  end, 1), "opening the native fold did not demand its body")
  assert(not owner.transcript.fold_loading[window], "loaded fold retained loading state")
  assert(vim.api.nvim_win_get_cursor(window)[1] == 1,
    "section expansion followed the tail instead of retaining its heading")
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
  assert(respond() == "node")
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(vim.wait(1000, function() return not owner.syncing and not owner.applying end, 1))
  assert(owner.transcript.block.heading.metadata.node.display == "heading" and not body_loaded(), "cancelled delivery reopened the fold")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(owner.toggle_heading(window))
  respond("fixture delivery failure")
  assert(owner.transcript.block.heading.metadata.node.display == "heading" and not body_loaded())
  assert(not owner.transcript.fold_loading[window], "failed request retained loading state")
  local failure_marks = vim.api.nvim_buf_get_extmarks(transcript, loading_namespace, 0, -1, { details = true })
  assert(#failure_marks == 1 and failure_marks[1][4].virt_text[1][1]:find("Section unavailable", 1, true),
    "failed expansion lost its retry feedback")
  assert(#notices == 0 and owner.section_failure.tools:find("fixture delivery failure", 1, true))
  assert(not require("forge.views.harness.health").notice({ presentation = owner }), "section failure escaped its local scope")
  deferred = false
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return body_loaded() and owner.transcript.block.heading.metadata.node.display == "full" end, 1), "failed opening was not retryable")
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return not body_loaded() end, 1))
  deferred = true
  assert(owner.toggle_heading(window))
  generation = generation + 1
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(vim.wait(1000, function() return #held > 0 end, 1), "retired node generation did not reconcile the opening")
  assert(held[1].params.operation == "node" and held[1].params.generation == generation)
  assert(respond() == "node")
  assert(vim.wait(1000, function() return #held > 0 end, 1))
  assert(respond() == "sync")
  assert(body_loaded() and not owner.section_failure.tools, "generation replacement became a section failure")
  local sequence = 0
  for _, request in ipairs(requests) do
    if request.operation == "node" then
      assert(request.sequence > sequence)
      sequence = request.sequence
    end
  end
end, debug.traceback)
if owner then owner.close() end
assert(ok, failure)
print("harness_sections: passed")
vim.cmd("qa!")
