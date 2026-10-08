vim.loader.enable(false)
local client = require("forge.client")
local harness = require("forge.session").harness
local pending = {}
local notice = {}
vim.notify = function(message) notice[#notice + 1] = tostring(message) end
client.host_generation = function() return 1 end
client.host_accepting = function() return true end
client.request_for = function(_, method, params, callback)
  pending[#pending + 1] = { method = method, params = params, callback = callback }
end
package.loaded["forge.views.harness.controller"] = {
  refresh_winbar = function() end,
  activate_snapshot = function() end,
  render = function() end,
  present_plan_question = function() end,
  task_transition = function(action) assert(action.action == "execute" and action.plan_id == "plan") end,
}
require("forge.infra.popup_window").input = function(_, callback) callback("") end
harness.session = { id = "session" }
harness.transcript_win = vim.api.nvim_get_current_win()
local controller = require("forge.views.plan_review.native_controller")
local comments = require("forge.draft_comments")

local function open_review()
  local plan = { id = "plan", review_digest = "canonical", working_path = vim.fn.tempname() .. ".md" }
  controller.open(plan)
  local review = harness.plan_review
  local request = pending[#pending]
  assert(request.params.operation == "plan_open")
  request.callback({ saved_source_digest = "saved", annotation = {}, public_only = true,
    source_row = { { id = "source:1", target = "source:1", text = "pub struct Sample;", source_line = 1,
      block = "plan:source", position = { row = 0, column = 0 }, metadata = {} } },
    snapshot = { document = review.owner.document, revision = 0, block = {
      { id = "plan:source", text = { "pub struct Sample;" },
        metadata = { target = {}, editable_region = {}, decoration = {} } },
    } } })
  comments.add(review.buf, { id = "single", source_line = 1, body = "Preserve this feedback" }, false)
  vim.cmd("stopinsert")
  assert(vim.bo[review.buf].modified)
  return review
end

for _, action in ipairs({ "request_changes", "accept" }) do
  local review = open_review()
  review.command_set.action_by_id[action].run({})
  local request = pending[#pending]
  assert(request.method == (action == "accept" and "plan.acceptance.begin" or "plan.request_changes"))
  assert(request.params.draft_annotation[1].source.body == "Preserve this feedback")
  assert(#vim.fn.win_findbuf(review.buf) == 0, action .. " kept the review visible while awaiting its response")
  assert(vim.api.nvim_get_current_win() == harness.transcript_win)
  assert(not review.owner.closed and review.owner.submission_pending, "hiding cancelled the pending submission")
  request.callback({}, nil)
  assert(review.owner.closed and not harness.plan_review, action .. " did not release its completed owner")
end

local review = open_review()
review.command_set.action_by_id.request_changes.run({})
local request = pending[#pending]
request.callback(nil, "Submission failed")
assert(#vim.fn.win_findbuf(review.buf) == 1, "failed submission did not restore the review")
assert(vim.bo[review.buf].modified and comments.capture(review.buf)[1].source.body == "Preserve this feedback",
  "failed submission lost the unsaved comment")
assert(not harness.busy and notice[#notice]:find("Submission failed", 1, true))
print("plan_review_submission_close: accept/reject hide immediately, completion closes, failure restores exact draft")
