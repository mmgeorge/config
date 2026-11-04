vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
local client = require("forge.client")
local perf = require("forge.infra.perf")
local measured_patch
perf.enabled = function(scope) return scope == "harness" end
perf.event = function(_, event, fields)
  if event == "ui.buffer.patch" then measured_patch = perf.payload(fields) end
end
local requests, notices = {}, {}
client.host_is_accepting = function() return true end
client.request = function(request_specification, callback)
  local params = request_specification.params or {}

  callback = dofile("nvim/tests/forge/support/presentation_status.lua")(params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
local owner, publications
local function open()
  publications = 0
  local transcript, composer = vim.api.nvim_create_buf(false, true), vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  owner = require("forge.views.harness.presentation").open({ session_id = "scheduled",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) notices[#notices + 1] = message end,
    on_update = function() publications = publications + 1 end,
  }, function(value, failure) assert(value and not failure, failure) end)
  requests[#requests].callback({ transcript = { document = owner.document, revision = 0,
    block = { { id = "body", text = { "initial" },
      metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
end
local function refresh()
  local count = #requests
  owner.sync()
  assert(vim.wait(1000, function()
    return #requests > count and requests[#requests].params.operation == "sync"
  end, 1), "refresh did not dispatch after its publication interval")
  return requests[#requests]
end
local function snapshot(revision)
  local block = {}
  for index = 1, 2048 do
    block[index] = { id = "body:" .. index, text = { "prepared " .. index },
      metadata = { target = {}, decoration = {}, editable_region = {} } }
  end
  return { document = owner.document, revision = revision, block = block }
end
local function patch(base)
  return { document = owner.document, base = base, next = base + 1,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
    text_edit = { { start_row = 0, removed_rows = 1, text = { "update " .. (base + 1) } } },
    block_edit = {}, removed_block = {}, metadata_edit = {} }
end
local success, failure = xpcall(function()
  open()
  local pending = refresh()
  local observed_revision
  vim.schedule(function()
    observed_revision = owner.transcript.revision
    assert(vim.api.nvim_buf_get_lines(owner.transcript.buffer, 0, -1, false)[1] == "initial",
      "preparation exposed an unfinished publication")
  end)
  pending.callback({ snapshot = snapshot(50) })
  assert(owner.transcript.revision == 0 and owner.transcript.update_pending and owner.syncing,
    "large snapshot preparation did not yield before publication")
  local count = #requests
  owner.sync()
  assert(#requests == count, "a second sync overtook pending buffer updates")
  assert(vim.wait(3000, function() return owner.transcript.revision == 50 end, 1), "snapshot preparation stalled")
  assert(observed_revision == 0, "queued editor work waited for preparation to finish")
  assert(publications == 1, "preparation notified before its atomic publication")
  local timing = owner.transcript.update_timing
  assert(timing.slices > 1, "snapshot preparation used one unbounded callback")
  assert(timing.maximum_prepare_ms < 50, "snapshot preparation exceeded the per-slice budget")
  assert(timing.maximum_commit_ms < 250, "atomic snapshot publication exceeded the commit budget")
  assert(vim.wait(1000, function() return requests[#requests].params.revision == 50 end, 1),
    "trailing refresh did not dispatch")
  assert(requests[#requests].params.revision == 50, "follow-up sync used an intermediate revision")
  requests[#requests].callback({ patch = {} })
  assert(not owner.syncing and not owner.applying)
  owner.close()
  requests[#requests].callback({})

  open()
  refresh().callback({ snapshot = snapshot(50) })
  assert(owner.transcript.update_pending and owner.transcript.revision == 0)
  local closed = owner
  local closed_revision = owner.transcript.revision
  owner.close()
  assert(vim.wait(1000, function() return not closed.transcript.update_pending end, 1), "closed view retained an update job")
  assert(closed.transcript.revision == closed_revision, "preparation published after its view closed")
  assert(requests[#requests].params.operation == "close", "closing remained blocked behind buffer updates")
  requests[#requests].callback({})

  open()
  refresh().callback({ snapshot = snapshot(50) })
  assert(owner.transcript.update_pending and owner.transcript.revision == 0)
  local replaced_revision, previous_generation = owner.transcript.revision, client.host_get_generation
  local replaced_generation = client.host_get_generation() + 1
  client.host_get_generation = function() return replaced_generation end
  assert(vim.wait(1000, function() return not owner.transcript.update_pending end, 1), "replaced host retained an update job")
  assert(owner.transcript.revision == replaced_revision, "old host preparation reached the replacement view")
  assert(vim.api.nvim_buf_get_lines(owner.transcript.buffer, 0, -1, false)[1] == "initial",
    "host replacement changed the last committed text")
  owner.close()
  client.host_get_generation = previous_generation

  open()
  refresh().callback({ patch = { patch(0) } })
  for _, name in ipairs({ "preflight_ms", "text_ms", "metadata_ms", "view_capture_ms", "view_restore_ms" }) do
    assert(type(measured_patch[name]) == "number", "patch diagnostics omitted " .. name)
  end
  local failed_request, previous_notices = refresh(), #notices
  local invalid = patch(1)
  invalid.base = 99
  failed_request.callback({ patch = { invalid, patch(2) } })
  assert(vim.wait(1000, function() return not owner.applying end, 1), "failed patch retained an update job")
  assert(owner.transcript.status == "Desynchronized" and #notices == previous_notices + 1, "failed patch was not surfaced")
  assert(requests[#requests].params.operation == "snapshot", "failed patch did not request recovery")
  requests[#requests].callback({ document = owner.document, revision = 3,
    block = { { id = "body", text = { "recovered" },
      metadata = { target = {}, decoration = {}, editable_region = {} } } } })
  assert(owner.transcript.status == "Applied" and owner.transcript.revision == 3,
    "snapshot recovery did not restore the committed document")
  owner.close()
end, debug.traceback)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_update_scheduling: passed")
vim.cmd("qa!")
