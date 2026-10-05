vim.opt.runtimepath:append("nvim")
local review = require("forge.review_document")
local editable = require("forge.editable")
local request = {}
review._set_runner_for_test(function(method, params, callback)
  request[#request + 1] = { method = method, params = vim.deepcopy(params), callback = callback }
end)
review.sync_editing = function() end
review.sync_dirty = function() end
review.refresh = function() end
review.read_section = function() end
local function field(text)
  return { revision = 0, pending = { sequence = 1, text = { text } } }
end
local comment_region = "comment-1/body"
local state = {
  active = true, explicit_repository = true, document = "queue-review", fields = {}, notice = error,
  replica = { buffer = vim.api.nvim_create_buf(false, true), editable = {
    document = "queue-review", sequence = 1, native = { anchor = {} }, suspended = true,
    region = { title = field("Title"), body = field("Description"), summary = field("Summary"),
      [comment_region] = field("Comment") },
  } },
  comment_by_region = { [comment_region] = { comment = 1 } },
  verdict_provider = function(callback) callback("comment") end,
}
review.save(state)
assert(#request == 1 and request[1].method == "review.save")
local superseded_comment, superseded_submission, superseded_lifecycle
review.comment(state, { operation = "save", comment = 1, action = "save" }, function(result, failure)
  assert(not result) superseded_comment = failure
end)
review.submit_batched(state, function(result, failure)
  assert(not result) superseded_submission = failure
end)
review.transition(state, "CLOSED", function(result, failure)
  assert(not result) superseded_lifecycle = failure
end)
review.save(state)
assert(superseded_comment and superseded_submission and superseded_lifecycle)
assert(state.pending_operation.kind == "save" and #request == 1)
editable.record(state.replica.editable, comment_region, { "Frozen comment" })
local callback_ran
review.comment(state, { operation = "save", comment = 1, action = "save" }, function(result, failure)
  assert(result and not failure, failure)
  review.transition(state, "OPEN")
  assert(#request == 2, "completion callback dispatched a concurrent operation")
  callback_ran = true
end)
editable.record(state.replica.editable, comment_region, { "Newer unsaved comment" })
assert(state.pending_operation.kind == "comment" and #request == 1)
request[1].callback({ snapshot = { uncertain = false, field = {} }, remote = { outcome = "confirmed" } })
assert(vim.wait(1000, function() return #request == 2 end))
assert(request[2].method == "review.comment")
assert(request[2].params.capture[1].text == "Frozen comment", "dispatch recaptured later typing")
local superseded_close
review.transition(state, "CLOSED", function(result, failure)
  assert(not result) superseded_close = failure
end)
request[2].callback({})
assert(vim.wait(1000, function() return #request == 3 end))
assert(callback_ran and superseded_close)
assert(request[3].method == "review.transition" and request[3].params.desired == "OPEN")
assert(editable.capture_draft(state.replica.editable, { [comment_region] = true })[1].text == "Newer unsaved comment")
request[3].callback({ lifecycle = { state = "OPEN" } })
assert(vim.wait(1000, function() return not state.lifecycle_running end))
state.submission_recovery = "unknown-review-submission"
review.transition(state, "CLOSED")
assert(#request == 3 and state.pending_operation.kind == "lifecycle",
  "unresolved review submission admitted a later publication")
local recovery_callback_ran
assert(review.recover_batched_submission(state, { resolution = "not_dispatched" }, function(result, failure)
  assert(result and not failure, failure)
  review.transition(state, "OPEN")
  assert(#request == 4, "recovery callback dispatched before completion settled")
  recovery_callback_ran = true
end))
assert(#request == 4 and request[4].method == "review.submit_batched_recover")
request[4].callback({ submission = {}, fresh_required = false })
assert(vim.wait(1000, function() return #request == 5 end))
assert(not state.submission_recovery and request[5].method == "review.transition")
assert(recovery_callback_ran and request[5].params.desired == "OPEN")
request[5].callback({ lifecycle = { state = "OPEN" } })
assert(vim.wait(1000, function() return not state.lifecycle_running end))
assert(not state.pending_operation)
state.submission_recovery = "failed-review-recovery"
state.notice = function() end
review.transition(state, "CLOSED")
local failed_recovery_callback
assert(review.recover_batched_submission(state, { resolution = "not_dispatched" }, function(result, failure)
  failed_recovery_callback = failure
end))
assert(#request == 6)
request[6].callback({})
assert(vim.wait(1000, function() return failed_recovery_callback ~= nil end))
assert(failed_recovery_callback == "Missing native review recovery delivery")
assert(state.submission_recovery == "failed-review-recovery")
assert(state.pending_operation.kind == "lifecycle" and #request == 6)
assert(editable.capture_draft(state.replica.editable, { [comment_region] = true })[1].text == "Newer unsaved comment")
failed_recovery_callback = nil
assert(review.recover_batched_submission(state, { resolution = "not_dispatched" }, function(result, failure)
  failed_recovery_callback = failure
end))
request[7].callback(nil, "Recovery transport failed")
assert(vim.wait(1000, function() return failed_recovery_callback ~= nil end))
assert(failed_recovery_callback == "Recovery transport failed")
assert(state.submission_recovery and state.pending_operation and #request == 7)
state.submission_recovery = nil
state.comment_by_region[comment_region].uncertain = true
review.transition(state, "OPEN")
assert(#request == 7 and state.pending_operation.params.desired == "OPEN")
local retained_operation = state.pending_operation
assert(review.comment(state, { operation = "recover", comment = 1 }))
assert(#request == 8 and state.pending_operation == retained_operation)
request[8].callback(nil, "Comment recovery failed")
assert(vim.wait(1000, function() return not state.comment_running end))
assert(#request == 8 and state.pending_operation == retained_operation)
local malformed_recovery_failure
assert(review.comment(state, { operation = "recover", comment = 1 }, function(result, failure)
  malformed_recovery_failure = failure
end))
request[9].callback({})
assert(vim.wait(1000, function() return malformed_recovery_failure ~= nil end))
assert(malformed_recovery_failure == "Missing native comment recovery snapshot")
assert(#request == 9 and state.pending_operation == retained_operation)
assert(review.comment(state, { operation = "recover", comment = 1 }))
request[10].callback({ snapshot = { region = comment_region, comment = 1, uncertain = false } })
assert(vim.wait(1000, function() return #request == 11 end))
assert(request[11].method == "review.transition" and request[11].params.desired == "OPEN")
assert(editable.capture_draft(state.replica.editable, { [comment_region] = true })[1].text == "Newer unsaved comment")
request[11].callback({ lifecycle = { state = "OPEN" } })
assert(vim.wait(1000, function() return not state.lifecycle_running end))
print("review_operation_queue: latest cross-action capture, supersession, callback exclusion, and recovery gates passed")
