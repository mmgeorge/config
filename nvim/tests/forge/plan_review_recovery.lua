vim.loader.enable(false)
local client = require("forge.client")
local pending = {}
local original_request = client.request_for
local original_generation = client.host_generation
local original_accepting = client.host_accepting
local generation = 1
client.host_generation = function() return generation end
client.host_accepting = function() return true end
client.request_for = function(_, method, params, callback)
  assert(method == "harness.document" or method == "plan.request_changes")
  pending[#pending + 1] = { method = method, params = vim.deepcopy(params), callback = callback }
end
local native_buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_name(native_buffer, vim.fn.tempname() .. ".md")
vim.api.nvim_win_set_buf(0, native_buffer)
vim.bo[native_buffer].bufhidden = "hide"
local owner
  owner = require("forge.views.plan_review.document").attach({ session_id = "session", buffer = native_buffer,
    window = vim.api.nvim_get_current_win(), plan = { id = "plan", review_digest = "canonical" },
    notice = error,
  }, function(value, error_message) assert(not error_message, error_message) owner = value end)
  local source = {}
  for row = 1, 105 do
    source[row] = { id = "source:" .. row, target = "source:" .. row, text = "Source row " .. row, source_line = row,
      block = "plan:source", position = { row = row - 1, column = 0 }, metadata = {} }
  end
  local text = {}
  for _, row in ipairs(source) do text[#text + 1] = row.text end
  pending[1].callback({ saved_source_digest = "saved", annotation = {}, source_row = source,
    snapshot = { document = owner.document, revision = 0, block = {
      { id = "plan:source", text = text, metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } })
  vim.api.nvim_win_set_cursor(0, { 44, 0 })
  owner.action("comment", function() end)

vim.cmd("stopinsert")
local comments = require("forge.draft_comments")
local body_row = vim.api.nvim_win_get_cursor(0)[1] - 1
vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 1, false, { "draft", "λ\r", "" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
local plan = { id = "plan", review_digest = "canonical", working_path = vim.api.nvim_buf_get_name(native_buffer) }
local harness = require("forge.session").harness
harness.session = { id = "replacement-session" }
local previous = { plan = plan, owner = owner, buf = native_buffer, win = vim.api.nvim_get_current_win(),
  tab = vim.api.nvim_get_current_tabpage() }
harness.plan_review = previous
local before = vim.api.nvim_buf_get_lines(native_buffer, 0, -1, false)
owner.saving = true
vim.cmd("write")
assert(owner.pending_operation and owner.pending_operation.capture[1].source.body == "draft\nλ\r\n")
generation = 2
require("forge.views.plan_review.native_controller").open(plan)
assert(#pending == 2)
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(native_buffer, 0, -1, false), before),
  "recovery replaced the retained draft before host validation")
assert(vim.bo[native_buffer].modified, "recovery cleared dirty state before host validation")
vim.bo[native_buffer].modifiable = true
vim.api.nvim_buf_set_lines(native_buffer, body_row, body_row + 3, false, { "newer draft", "λ\r", "" })
vim.api.nvim_exec_autocmds("TextChanged", { buffer = native_buffer })
local replacement = harness.plan_review
local response = { saved_source_digest = "saved", annotation = {}, source_row = source,
  public_only = true, snapshot = { document = pending[2].params.document, revision = 0, block = {
    { id = "plan:source", text = text, metadata = { target = {}, editable_region = {}, decoration = {} } },
  } } }
pending[2].callback(response)
assert(replacement.owner.ready)
assert(comments.capture(native_buffer)[1].source.body == "newer draft\nλ\r\n",
  "recovery restored an older capture instead of current typing")
assert(vim.bo[native_buffer].modified, "recovered draft became its own saved baseline")
assert(replacement.owner.document ~= owner.document)
assert(#pending == 3 and pending[3].params.operation == "plan_save_annotations", vim.inspect(pending))
assert(pending[3].params.annotation[1].source.body == "draft\nλ\r\n",
  "recovery recaptured newer typing for the pending save")
pending[3].callback({})
assert(comments.capture(native_buffer)[1].source.body == "newer draft\nλ\r\n")
assert(vim.bo[native_buffer].modified)
print("plan_review_recovery: host rebinding preserves the buffer, current text, and dirty baseline")

local retained_text = vim.api.nvim_buf_get_lines(native_buffer, 0, -1, false)
local failure_notice
vim.notify = function(message) failure_notice = tostring(message) end
generation = 3
require("forge.views.plan_review.native_controller").open(plan)
assert(#pending == 4)
pending[4].callback(nil, "physical source conflict")
assert(harness.plan_review == replacement, "failed rebind discarded the retained owner")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(native_buffer, 0, -1, false), retained_text), "failed rebind changed draft text")
assert(vim.bo[native_buffer].modified)
assert(failure_notice and failure_notice:find("physical source conflict", 1, true))
print("plan_review_recovery: conflicts preserve retained text and ownership")
generation = 2
replacement.owner.saving = true
local submitted
replacement.owner.submit("plan.request_changes", { comment = "Revision requested" }, function(result, failure)
  assert(result and not failure, failure)
  submitted = true
end)
assert(replacement.owner.pending_operation)
local frozen = vim.deepcopy(replacement.owner.pending_operation.params)
generation = 4
require("forge.views.plan_review.native_controller").open(plan)
assert(#pending == 5)
local submission_owner = harness.plan_review.owner
local reopened = vim.deepcopy(response)
reopened.snapshot.document = pending[5].params.document
pending[5].callback(reopened)
assert(#pending == 6 and pending[6].method == "plan.request_changes")
local recovered_submission = pending[6].params
assert(recovered_submission.review.document == submission_owner.document)
assert(recovered_submission.review.view == submission_owner.view.id)
assert(recovered_submission.review.revision == submission_owner.replica.revision)
assert(vim.deep_equal(recovered_submission.draft_annotation, frozen.draft_annotation))
assert(recovered_submission.comment == frozen.comment)
pending[6].callback({})
assert(submitted and not submission_owner.submission_pending)
print("plan_review_recovery: queued submission retains capture and rebinds host identity")
