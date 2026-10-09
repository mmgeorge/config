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
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  requests[#requests + 1] = { params = params, callback = callback }
end
local owner
local function open()
  local transcript, composer = vim.api.nvim_create_buf(false, true), vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  owner = require("forge.views.harness.presentation").open({ session_id = "scheduled",
    transcript_buffer = transcript, composer_buffer = composer, transcript_window = window,
    is_alive = function() return true end, notice = function(message) notices[#notices + 1] = message end,
  }, function(value, failure) assert(value and not failure, failure) end)
  requests[#requests].callback({ transcript = { document = owner.document, revision = 0,
    block = { { id = "body", text = { "initial" },
      metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
end
local function patch(base)
  return { document = owner.document, base = base, next = base + 1,
    base_rows = 1, next_rows = 1, base_blocks = 1, next_blocks = 1,
    text_edit = { { start_row = 0, removed_rows = 1, text = { "update " .. (base + 1) } } },
    block_edit = {}, removed_block = {}, metadata_edit = {} }
end
local success, failure = xpcall(function()
  open()
  owner.sync()
  local batch = {}
  for revision = 0, 49 do batch[#batch + 1] = patch(revision) end
  local observed_revision
  vim.schedule(function() observed_revision = owner.transcript.revision end)
  requests[#requests].callback({ patch = batch })
  assert(owner.transcript.revision > 0 and owner.transcript.revision <= 8 and owner.syncing,
    "one callback exceeded the patch budget")
  local count = #requests
  owner.sync()
  assert(#requests == count, "a second sync overtook pending buffer updates")
  assert(vim.wait(3000, function() return owner.transcript.revision == 50 end, 1), "patch batch stalled")
  assert(observed_revision and observed_revision < 50, "queued editor work waited for every patch")
  assert(requests[#requests].params.revision == 50, "follow-up sync used an intermediate revision")
  requests[#requests].callback({ patch = {} })
  assert(not owner.syncing and not owner.applying)
  for _, name in ipairs({ "preflight_ms", "text_ms", "metadata_ms", "fold_capture_ms", "fold_refresh_ms", "view_restore_ms" }) do
    assert(type(measured_patch[name]) == "number", "patch diagnostics omitted " .. name)
  end
  owner.close()
  requests[#requests].callback({})

  open()
  owner.sync()
  batch = {}
  for revision = 0, 49 do batch[#batch + 1] = patch(revision) end
  requests[#requests].callback({ patch = batch })
  local closed = owner
  local closed_revision = owner.transcript.revision
  owner.close()
  assert(vim.wait(1000, function() return not closed.applying end, 1), "closed view retained an update job")
  assert(closed.transcript.revision == closed_revision, "queued patches modified a closed view")
  assert(requests[#requests].params.operation == "close", "closing remained blocked behind buffer updates")
  requests[#requests].callback({})

  open()
  owner.sync()
  batch = {}
  for revision = 0, 49 do batch[#batch + 1] = patch(revision) end
  requests[#requests].callback({ patch = batch })
  local replaced_revision, previous_generation = owner.transcript.revision, client.host_generation
  local replaced_generation = client.host_generation() + 1
  client.host_generation = function() return replaced_generation end
  assert(vim.wait(1000, function() return not owner.applying end, 1), "replaced host retained an update job")
  assert(owner.transcript.revision == replaced_revision, "old host patches reached the replacement view")
  owner.close()
  client.host_generation = previous_generation

  open()
  owner.sync()
  local invalid = patch(1)
  invalid.base = 99
  requests[#requests].callback({ patch = { patch(0), invalid, patch(2) } })
  assert(vim.wait(1000, function() return not owner.applying end, 1), "failed patch retained an update job")
  assert(owner.transcript.status == "Desynchronized" and #notices == 1, "failed patch was not surfaced")
  assert(requests[#requests].params.operation == "snapshot", "failed patch did not request recovery")
  owner.close()
end, debug.traceback)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_update_scheduling: passed")
vim.cmd("qa!")
