vim.loader.enable(false)

local client = require("forge.client")
local session = require("forge.session").harness
local public_controller = require("forge.views.plan_review")
local native_controller = require("forge.views.plan_review.native_controller")
local original_request, original_accepting = client.request_for, client.host_accepting
local directory = vim.fn.tempname()
assert(vim.fn.mkdir(directory, "p") == 1)
local path = vim.fs.joinpath(directory, "working.md")
local source = { "# Physical plan", "", "# Tasks", "", "# Tests", "", "Preserve this source" }
vim.fn.writefile(source, path)
client.host_accepting = function() return true end
client.request_for = function(_, method, params, callback)
  assert(method == "harness.document")
  if params.operation == "plan_open" then
    callback({ path = path, version = 1, saved_source_digest = "saved", snapshot = {
      document = params.document, revision = 0, block = { {
        id = "plan:source", text = source, metadata = { decoration = {}, editable_region = {}, target = {} },
      } },
    } })
  else
    callback({})
  end
end

local success, failure = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" } })
  session.session = { id = "public-import" }
  session.transcript_win = vim.api.nvim_get_current_win()

  assert(public_controller == native_controller, "the public PlanReview import did not select the native controller")
  assert(type(public_controller.open) == "function")
  public_controller.open({ id = "public-import", working_path = path, review_digest = "saved-digest" })
  local review = assert(session.plan_review, "public PlanReview import did not open a review")
  assert(review.owner and review.owner.ready, "public PlanReview import did not attach the native document")
  assert(vim.fs.normalize(vim.api.nvim_buf_get_name(review.buf)) == vim.fs.normalize(path))
  assert(vim.deep_equal(vim.fn.readfile(path), source),
    "public PlanReview import changed the physical plan source")
  for _, command in ipairs({ "toggle", "open", "jump_entity", "entity_info", "rename_entity", "schema", "comment", "accept", "request_changes", "close", "help" }) do
    assert(review.command_set.action_by_id[command], "public PlanReview command is missing: " .. command)
  end

  review.command_set.action_by_id.close.run({})
  assert(session.plan_review == nil, "public PlanReview close did not release its session")
  assert(vim.deep_equal(vim.fn.readfile(path), source),
    "public PlanReview close changed the physical plan source")
end, debug.traceback)

if session.plan_review and session.plan_review.owner then session.plan_review.owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
vim.fn.delete(path)
vim.fn.delete(directory, "rf")
assert(success, failure)
print("plan_review_public_import: passed")
