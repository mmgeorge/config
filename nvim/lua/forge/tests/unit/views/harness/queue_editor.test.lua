-- Unit tests for `forge.views.harness.queue_editor` (queue_editor.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["input_reopen_edit"] = function()
  -- Reopening a queued message before the host publishes the previous edit must reserve the
  -- revision that edit produces, not the revision the editor displayed when it committed.
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  local state = { session = { id = "reopen-edit" }, queue = {},
    composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  local entry = { id = "queued", revision = 1, text = "original", attachments = {},
    purpose = { kind = "user" }, intent = "queue", delivery = "queued" }
  requests[1].callback({ revision = 1, messages = { entry }, suspended = false })
  local identity = state.queue[1]

  editor.move(state, "", -1)
  local reserve = requests[2]
  assert(reserve.spec.params.action.kind == "reserve" and reserve.spec.params.action.revision == 1)
  entry = vim.deepcopy(entry)
  entry.revision, entry.reservation = 2, reserve.spec.params.action.reservation
  reserve.callback({ revision = 2, operation_id = reserve.spec.params.id, upsert = { entry }, removed = {} })

  editor.finish(state, "first edit")
  local commit = requests[3]
  assert(commit and commit.spec.params.action.kind == "commit", "first edit did not commit")
  eq(commit.spec.params.action.edits[1].revision, 2)

  -- The host has not published the commit result yet, so the displayed revision is stale.
  local text, failure = editor.move(state, "", -1)
  assert(not failure and text == "original", "reopened edit did not show the queued entry")
  assert(#requests == 3, "reopened edit reserved before the previous edit was published: "
    .. vim.inspect(requests[4] and requests[4].spec.params.action))

  entry = vim.deepcopy(entry)
  entry.text, entry.revision, entry.reservation = "first edit", 3, nil
  commit.callback({ revision = 3, operation_id = commit.spec.params.id, upsert = { entry }, removed = {} })
  assert(#requests == 4 and requests[4].spec.params.action.kind == "reserve", "reopened edit never reserved")
  assert(requests[4].spec.params.action.revision == 3, "reopened edit reserved a stale revision: "
    .. tostring(requests[4].spec.params.action.revision))
  assert(state.queue[1] == identity and identity.text == "first edit")

  -- The reopened edit commits against the revision its own reservation produced.
  local reopened = requests[4]
  entry = vim.deepcopy(entry)
  entry.revision, entry.reservation = 4, reopened.spec.params.action.reservation
  reopened.callback({ revision = 4, operation_id = reopened.spec.params.id, upsert = { entry }, removed = {} })
  local restored, finish_failure = editor.finish(state, "second edit")
  assert(restored == "" and not finish_failure)
  assert(#requests == 5 and requests[5].spec.params.action.edits[1].revision == 4)
end

T["reopen_pending_removal"] = function()
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  local state = { session = { id = "remove-edit" }, queue = {}, composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  local entry = { id = "queued", revision = 1, text = "original", attachments = {}, delivery = "queued" }
  requests[1].callback({ revision = 1, messages = { entry }, suspended = false })
  editor.move(state, "original draft", -1)
  entry.revision, entry.reservation = 2, requests[2].spec.params.action.reservation
  requests[2].callback({ revision = 2, upsert = { entry }, removed = {} })
  eq(editor.finish(state, ""), "original draft")
  local removal = requests[3]
  eq(removal.spec.params.action.edits[1].text, "")
  -- The stale displayed entry remains while the host processes removal.
  eq(#state.queue, 1)
  local text, failure = editor.move(state, "new draft", -1)
  eq(text, nil)
  eq(type(failure), "string")
  eq(state.queue_edit, nil)
  eq(#requests, 3)
  removal.callback({ revision = 3, upsert = {}, removed = { entry.id } })
  eq(state.input.removing[entry.id], nil)
  eq(#state.queue, 0)
  eq(#requests, 3) -- No deferred reservation of the removed identity.
end

T["failed_removal_releases_edit_block"] = function()
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  require("forge.infra.notifications").error = function() end
  local state = { session = { id = "failed-removal" }, queue = {}, composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  requests[1].callback({ revision = 1, messages = {
    { id = "queued", revision = 1, text = "original", attachments = {}, delivery = "queued" },
  }, suspended = false })
  input.perform(state, { kind = "commit", reservation = "edit", edits = {
    { id = "queued", revision = 1, text = "", attachments = {} },
  } })
  requests[2].callback(nil, "removal rejected")
  eq(state.input.removing.queued, nil)
  eq(editor.move(state, "draft", -1), "original")
  eq(requests[3].spec.params.action.kind, "reserve")
end

T["pending_submission_edit"] = function()
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  local state = { session = { id = "pending-edit" }, queue = {}, composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  requests[1].callback({ revision = 0, messages = {}, suspended = false })
  input.submit(state, "pending message", {}, "queue")
  local submit = requests[2]
  local text, failure = editor.move(state, "unsent draft", -1)
  eq(text, nil)
  eq(type(failure), "string")
  eq(state.queue_edit, nil)
  eq(#requests, 2)
  local message = vim.deepcopy(submit.spec.params.action.message)
  message.delivery, message.revision = "queued", 1
  submit.callback({ revision = 1, upsert = { message }, removed = {} })
  text, failure = editor.move(state, "unsent draft", -1)
  eq({ text, failure }, { "pending message", nil })
  eq(requests[3].spec.params.action.kind, "reserve")
  eq(requests[3].spec.params.action.revision, 1)
end

T["unavailable_edit"] = function()
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  for _, action in ipairs({ editor.finish, function(state, draft) return editor.move(state, draft, -1) end }) do
    local state = { session = { id = "delivery-race" }, queue = {}, composer_buf = buffer.scratch() }
    input.attach(state, function() end)
    requests[#requests].callback({ revision = 1, messages = {
      { id = "queued", revision = 1, text = "original", delivery = "queued", attachments = {} },
    }, suspended = false })
    editor.move(state, "previous draft", -1)
    local reservation = requests[#requests]
    -- Delivery wins before the edit reservation is acknowledged.
    input.receive(state, { revision = 2, removed = { "queued" }, upsert = {} })
    local text, failure, attachments, recovered = action(state, "keep my edits")
    eq({ text, failure, attachments, recovered }, { "keep my edits", nil, {}, true })
    eq(state.queue_edit, nil)
    eq(requests[#requests].spec.params.action.kind, "release")
    -- A late reservation failure must not re-enter editing.
    require("forge.infra.notifications").error = function() end
    reservation.callback(nil, "Message no longer available")
    eq(state.queue_edit, nil)
    eq(state.failed_queue_edit, nil)
  end
end

return T
