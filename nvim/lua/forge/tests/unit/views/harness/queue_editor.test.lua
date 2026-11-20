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

  -- The host has not published the commit result yet, so the displayed revision is stale, while the
  -- composer shows the text the user saved.
  local text, failure = editor.move(state, "", -1)
  assert(not failure and text == "first edit", "reopened edit did not show the saved text: " .. tostring(text))
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
  -- The removed entry leaves the queue at once and cannot become an edit target meanwhile.
  eq(#state.queue, 0)
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
    eq({ text, failure, attachments, recovered }, { "keep my edits", nil, {}, "edited" })
    eq(state.queue_edit, nil)
    eq(requests[#requests].spec.params.action.kind, "release")
    -- A late reservation failure must not re-enter editing.
    require("forge.infra.notifications").error = function() end
    reservation.callback(nil, "Message no longer available")
    eq(state.queue_edit, nil)
    eq(state.failed_queue_edit, nil)
  end
end

T["delivered_unchanged_edit_restores_earlier_draft"] = function()
  -- Delivery can win the race with the reservation. The composer then holds text the host already
  -- sent, and keeping it as a draft invites sending it twice, so unchanged text gives way to the
  -- draft the composer held before editing began.
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  local state = { session = { id = "delivered-edit" }, queue = {}, composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  requests[1].callback({ revision = 1, messages = {
    { id = "queued", revision = 1, text = "original", delivery = "queued", attachments = {} },
  }, suspended = false })
  eq(editor.move(state, "earlier draft", -1), "original")
  input.receive(state, { revision = 2, removed = {}, upsert = {
    { id = "queued", revision = 2, text = "original", delivery = "delivering", route = "turn", attachments = {} },
  } })
  eq(editor.delivered(state), true)
  local text, failure, attachments, recovered = editor.finish(state, "original")
  eq({ text, failure, attachments, recovered }, { "earlier draft", nil, {}, "sent" })
  eq(state.queue_edit, nil)
  eq(requests[#requests].spec.params.action.kind, "release")
end

T["navigation_releases_unchanged_entries"] = function()
  -- Navigation reserves each message it visits. A message left unchanged is released alone, so
  -- an earlier message the user only looked at can still run while a later one is edited.
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  local state = { session = { id = "navigate-edit" }, queue = {}, composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  local two = { id = "two", revision = 1, text = "two", delivery = "queued", attachments = {} }
  local three = { id = "three", revision = 1, text = "three", delivery = "queued", attachments = {} }
  requests[1].callback({ revision = 1, messages = { two, three }, suspended = false })
  local function acknowledge(request, entry, revision)
    local reserved = vim.deepcopy(entry)
    reserved.revision, reserved.reservation = revision, request.spec.params.action.reservation
    request.callback({ revision = revision, operation_id = request.spec.params.id, upsert = { reserved }, removed = {} })
  end
  eq(editor.move(state, "", -1), "three")
  local reserve_three = requests[2]
  eq(reserve_three.spec.params.action.id, "three")
  acknowledge(reserve_three, three, 2)
  -- Leaving "three" unchanged for "two" releases "three" and reserves "two".
  eq(editor.move(state, "three", -1), "two")
  local actions = vim.tbl_map(function(request) return request.spec.params.action end, { unpack(requests, 3) })
  eq(vim.tbl_map(function(action) return { action.kind, action.id } end, actions),
    { { "release", "three" }, { "reserve", "two" } })
  eq(actions[1].reservation, reserve_three.spec.params.action.reservation)
  local released = vim.deepcopy(three)
  released.revision = 3
  requests[3].callback({ revision = 3, operation_id = requests[3].spec.params.id, upsert = { released }, removed = {} })
  -- Returning to "three" before the host acknowledges "two" reserves "three" at once, and releases
  -- the unchanged "two" only after its reservation lands.
  eq(editor.move(state, "two", 1), "three")
  eq(#requests, 5)
  eq({ requests[5].spec.params.action.kind, requests[5].spec.params.action.id, requests[5].spec.params.action.revision },
    { "reserve", "three", 3 })
  acknowledge(requests[4], two, 4)
  eq({ requests[6].spec.params.action.kind, requests[6].spec.params.action.id }, { "release", "two" })
  eq(state.queue_edit.drafts[state.queue[1]], nil)
  -- Only the edited entry is committed.
  local reserved = vim.deepcopy(released)
  reserved.revision, reserved.reservation = 5, requests[5].spec.params.action.reservation
  requests[5].callback({ revision = 5, upsert = { reserved }, removed = {} })
  editor.finish(state, "trois")
  local commit = requests[#requests].spec.params.action
  eq(commit.kind, "commit")
  eq(vim.tbl_map(function(edit) return { edit.id, edit.text } end, commit.edits), { { "three", "trois" } })
end

T["saved_edit_shows_saved_text_until_published"] = function()
  -- Saving ends the edit before the host answers its commit. The row keeps the saved text from that
  -- frame, through the reservation's late acknowledgement, until the host publishes the edit.
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  local state = { session = { id = "saved-edit" }, queue = {}, composer_buf = buffer.scratch() }
  local staged_rows = {}
  local function stage() staged_rows[#staged_rows + 1] = editor.staging(state, nil)[1].text end
  input.attach(state, stage)
  local queued = { id = "queued", revision = 1, text = "three", delivery = "queued", attachments = {} }
  requests[1].callback({ revision = 1, messages = { queued }, suspended = false })
  eq(editor.move(state, "", -1), "three")
  local reserve = requests[2]
  editor.finish(state, "four")
  staged_rows = {}
  stage()
  -- The reservation lands after the save, so the commit leaves only now.
  local reserved = vim.deepcopy(queued)
  reserved.revision, reserved.reservation = 2, reserve.spec.params.action.reservation
  reserve.callback({ revision = 2, operation_id = reserve.spec.params.id, upsert = { reserved }, removed = {} })
  local commit = requests[3]
  eq(commit.spec.params.action.kind, "commit")
  eq(commit.spec.params.action.edits[1].revision, 2)
  local published = vim.deepcopy(reserved)
  published.text, published.revision, published.reservation = "four", 3, nil
  commit.callback({ revision = 3, operation_id = commit.spec.params.id, upsert = { published }, removed = {} })
  stage()
  for index, text in ipairs(staged_rows) do eq({ index, text }, { index, "four" }) end
  eq(state.input.saved_edit.queued, nil)
end

T["rejected_saved_edit_shows_host_text"] = function()
  -- A commit the host rejects leaves the message as the host holds it, so the row shows that text.
  local input = require("forge.views.harness.input_state")
  local editor = require("forge.views.harness.queue_editor")
  local requests = mock_client.record()
  require("forge.infra.notifications").error = function() end
  local state = { session = { id = "rejected-save" }, queue = {}, composer_buf = buffer.scratch() }
  input.attach(state, function() end)
  local queued = { id = "queued", revision = 1, text = "three", delivery = "queued", attachments = {} }
  requests[1].callback({ revision = 1, messages = { queued }, suspended = false })
  editor.move(state, "", -1)
  local reserved = vim.deepcopy(queued)
  reserved.revision, reserved.reservation = 2, requests[2].spec.params.action.reservation
  requests[2].callback({ revision = 2, operation_id = requests[2].spec.params.id, upsert = { reserved }, removed = {} })
  editor.finish(state, "four")
  eq(editor.staging(state, nil)[1].text, "four")
  requests[3].callback(nil, "commit rejected")
  eq(editor.staging(state, nil)[1].text, "three")
  eq(state.input.saved_edit.queued, nil)
end

T["question_staging_preserves_typed_responses"] = function()
  local editor = require("forge.views.harness.queue_editor")
  local first = { question_id = "layout", revision = 1, message_id = "first", question = "Which layout?",
    response = { kind = "answer", answer = "Tabs", required_questions_remain = false } }
  local second = vim.deepcopy(first)
  second.question_id, second.message_id = "color", "second"
  local staged = editor.staging({}, nil, {
    { id = "answer", text = "plain display text", purpose = { kind = "question", reply = first, additional_replies = { second } } },
    { id = "user", text = "You answered: this is ordinary user text", purpose = { kind = "user" } },
  })
  eq(staged[1].content, { kind = "question_response", responses = { first, second } })
  eq(staged[2].content, { kind = "message" })
end

return T
