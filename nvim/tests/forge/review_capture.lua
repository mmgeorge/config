vim.loader.enable(false)

local review = require("forge.review_document")
local requests = {}
review._set_runner_for_test(function(method, params, callback)
  requests[#requests + 1] = { method = method, params = params, callback = callback }
end)
review.sync_editing = function() end
review.sync_dirty = function() end
review.refresh = function() end

local function field(text)
  return { revision = 0, pending = { sequence = 1, text = { text } } }
end
local state = {
  active = true,
  shown = true,
  explicit_repository = true,
  document = "review-capture",
  notice = function(message) error(message) end,
  replica = {
    editable = {
      document = "review-capture",
      suspended = true,
      region = {
        title = field("Unsaved title"),
        body = field("Unsaved description"),
        reviewers = field("Unsaved reviewers"),
        summary = field("Review summary"),
        ["comment-1/body"] = field("Selected comment\r\n\n"),
        ["comment-2/body"] = field("Other comment"),
      },
    },
  },
  comment_by_region = {
    ["comment-1/body"] = { comment = 1 },
    ["comment-2/body"] = { comment = 2 },
  },
}

assert(review.comment(state, { operation = "save", comment = 1, action = "save" }))
local saved = requests[1]
assert(saved.method == "review.comment")
assert(#saved.params.capture == 1 and saved.params.capture[1].region == "comment-1/body",
  "saving one comment must capture only that comment")
assert(saved.params.capture[1].text == "Selected comment\r\n\n",
  "explicit comment saves must preserve raw body bytes")
saved.callback({})
assert(vim.wait(1000, function() return not state.comment_running end))
assert(state.replica.editable.region.title.pending, "comment save must retain unsaved PR fields")
assert(state.replica.editable.region["comment-2/body"].pending,
  "comment save must retain other comment drafts")

state.verdict_provider = function(callback) callback("comment") end
assert(review.submit_batched(state))
local submitted = requests[2]
assert(submitted.method == "review.submit_batched")
assert(#submitted.params.capture == 2, "review submission must capture the summary and remaining comment")
for _, captured in ipairs(submitted.params.capture) do
  assert(captured.region ~= "title" and captured.region ~= "body" and captured.region ~= "reviewers",
    "review submission must retain unrelated PR fields")
end
state.submission_running = false
state.replica.editable.region["draft-comment/client-1/body"] = field("Local body\r\n\n")
local anchor = { revision = string.rep("a", 40), path = "src/lib.rs", side = "right", first_line = 4, last_line = 4 }
state.comment_by_region["draft-comment/client-1/body"] = {
  region = "draft-comment/client-1/body", local_draft = true, anchor = anchor,
}
assert(review.comment(state, { operation = "save_draft", region = "draft-comment/client-1/body", action = "save" }))
local created = requests[3]
assert(#created.params.capture == 1 and created.params.capture[1].text == "Local body\r\n\n")
assert(#created.params.draft_comment == 1)
assert(created.params.draft_comment[1].region == "draft-comment/client-1/body")
assert(vim.deep_equal(created.params.draft_comment[1].anchor, anchor))
anchor.last_line = 9
assert(created.params.draft_comment[1].anchor.last_line == 4,
  "later metadata changes must not mutate a submitted comment definition")
local editable = require("forge.editable")
state.replica.editable.sequence = 10
local region = "draft-comment/client-1/body"
editable.record(state.replica.editable, region, { "queued explicit body" })
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 3, "queued comment save bypassed active operation")
editable.record(state.replica.editable, region, { "newer unsaved body" })
created.callback({})
assert(vim.wait(1000, function() return #requests == 4 end), "completion stranded the queued explicit capture")
assert(requests[4].params.capture[1].text == "queued explicit body",
  "queued comment save recaptured later unsaved typing")
assert(requests[4].params.capture[1].base == 1)
requests[4].callback({})
assert(vim.wait(1000, function() return not state.comment_running end))
assert(editable.capture_draft(state.replica.editable, { [region] = true })[1].text == "newer unsaved body")
assert(state.replica.editable.region.title.pending, "queued saves cleared unrelated fields")
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 5)
editable.record(state.replica.editable, "summary", { "queued summary" })
assert(review.submit_batched(state))
assert(#requests == 5, "submission overlapped the active comment save")
editable.record(state.replica.editable, "summary", { "newer unsaved summary" })
requests[5].callback({})
assert(vim.wait(1000, function() return #requests == 6 end), "comment completion stranded submission")
assert(requests[6].method == "review.submit_batched")
local captured_summary
for _, capture in ipairs(requests[6].params.capture) do
  if capture.region == "summary" then captured_summary = capture.text end
end
assert(captured_summary == "queued summary", "submission recaptured newer unsaved text")
requests[6].callback({ outcome = "confirmed" })
assert(vim.wait(1000, function() return not state.submission_running end))
assert(editable.capture_draft(state.replica.editable, { summary = true })[1].text == "newer unsaved summary")
review.read_section = function() end
editable.record(state.replica.editable, region, { "lifecycle interleave body" })
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 7)
assert(review.transition(state, "draft"))
assert(#requests == 7, "lifecycle transition overlapped a comment save")
requests[7].callback({})
assert(vim.wait(1000, function() return #requests == 8 end))
assert(requests[8].method == "review.transition" and requests[8].params.desired == "DRAFT")
editable.record(state.replica.editable, region, { "captured during lifecycle" })
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 8, "comment save overlapped a lifecycle transition")
editable.record(state.replica.editable, region, { "newer lifecycle typing" })
requests[8].callback({ state = "OPEN", is_draft = true })
assert(vim.wait(1000, function() return #requests == 9 end))
assert(requests[9].params.capture[1].text == "captured during lifecycle")
requests[9].callback({})
assert(vim.wait(1000, function() return not state.comment_running end))
assert(editable.capture_draft(state.replica.editable, { [region] = true })[1].text == "newer lifecycle typing")
local failure_notice
state.notice = function(message) failure_notice = message end
assert(review.transition(state, "open"))
assert(#requests == 10)
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 10)
requests[10].callback(nil, "lifecycle fixture failure")
assert(vim.wait(1000, function() return #requests == 11 end), "lifecycle failure stranded queued capture")
assert(failure_notice == "lifecycle fixture failure")
requests[11].callback({})
assert(vim.wait(1000, function() return not state.comment_running end))
state.view = {}
state.comment_focus = { comment = 1 }
assert(not review.add_comment(state))
assert(not review.reply_comment(state))
assert(#requests == 11, "missing local presentation fell back to host draft creation")
local client = require("forge.client")
local original_generation = client.host_generation
local generation = original_generation()
client.host_generation = function() return generation end
editable.record(state.replica.editable, region, { "retained across restart" })
local accepted_revision = state.replica.editable.region[region].revision
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 12)
editable.record(state.replica.editable, region, { "queued before restart" })
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
editable.record(state.replica.editable, region, { "retained across restart" })
generation = generation + 1
requests[12].callback({})
assert(vim.wait(1000, function() return not state.comment_running end))
assert(state.replica.editable.region[region].revision == accepted_revision,
  "obsolete host response advanced the draft revision")
assert(editable.capture_draft(state.replica.editable, { [region] = true })[1].text == "retained across restart")
assert(failure_notice:find("host changed", 1, true))
assert(#requests == 12, "host loss dispatched a queued capture against the stale document")
assert(state.pending_operation.capture[1].text == "queued before restart")
state.host_lost, state.pending_operation = false, nil
state.generation = generation
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == 13)
generation = generation + 1
state.generation = generation
state.comment_running = "replacement operation"
requests[13].callback({})
vim.wait(50, function() return false end)
assert(not state.host_lost, "obsolete response invalidated the replacement host binding")
assert(state.comment_running == "replacement operation", "obsolete response settled a replacement operation")
assert(state.replica.editable.region[region].revision == accepted_revision)
state.host_lost, state.comment_running = false, true
state.submission_recovery = "recovery-operation"
local request_count = #requests
assert(review.reconcile_save(state) == false, "save recovery overlapped comment publication")
assert(not review.recover_batched_submission(state, { resolution = "not_dispatched" }),
  "submission recovery overlapped comment publication")
assert(#requests == request_count)
state.comment_running, state.save_recovering = false, true
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == request_count and state.pending_operation, "comment bypassed active save recovery")
assert(review.transition(state, "CLOSED"))
assert(#requests == request_count and state.pending_operation, "lifecycle bypassed active save recovery")
state.save_recovering, state.save_uncertain = false, true
assert(review.transition(state, "OPEN"))
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == request_count, "unresolved save outcome dispatched a later mutation")
assert(state.pending_operation.kind == "comment")
assert(state.pending_operation.capture[1].text == "retained across restart")
state.save_uncertain, state.rebinding = false, true
assert(review.transition(state, "CLOSED"))
assert(review.comment(state, { operation = "save_draft", region = region, action = "save" }))
assert(#requests == request_count, "rebinding dispatched a mutation against the obsolete document")
assert(state.pending_operation.kind == "comment")
client.host_generation = original_generation
print("review_capture: comment saves and review submissions preserve unrelated drafts")
