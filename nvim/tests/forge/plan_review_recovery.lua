vim.loader.enable(false)
local client = require("forge.client")
local original_request, original_accepting, original_generation = client.request_for, client.host_accepting, client.host_generation
local generation = 1
client.host_accepting = function() return true end
client.host_generation = function() return generation end
local requests = {}
client.request_for = function(_, _, params, callback) requests[#requests + 1] = { params = params, callback = callback } end
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "# Plan" }, path)
vim.cmd("edit " .. vim.fn.fnameescape(path))
local native_buffer, window = vim.api.nvim_get_current_buf(), vim.api.nvim_get_current_win()
local module = require("forge.views.plan_review.document")
local options = { session_id = "session", buffer = native_buffer, window = window, plan = { id = "plan", review_digest = "canonical" } }
local function respond(open)
  open.callback({ path = path, version = 1, saved_source_digest = "saved", snapshot = { document = open.params.document, revision = 0,
    block = { { id = "comment", text = { "Comment", "saved" }, metadata = { target = {}, decoration = {}, editable_region = {
      { id = "note", revision = 0, range = { start = { row = 1, column = 0 }, ["end"] = { row = 1, column = 5 } } },
    } } } } } })
end
local owner
local success, failure = xpcall(function()
  owner = module.attach(options, function(_, error_message) assert(not error_message, error_message) end)
  respond(requests[1])
  vim.api.nvim_buf_set_text(native_buffer, 1, 0, 1, 5, { "unsent ** literal", "tail" })
  local recovery = assert(owner.recovery())
  assert(vim.deep_equal(recovery.draft.note, { "unsent ** literal", "tail" }))
  generation = 2
  assert(owner.close())
  options.recovery = recovery
  owner = module.attach(options, function(_, error_message) assert(not error_message, error_message) end)
  local reopened = requests[#requests]
  assert(reopened.params.saved_source_digest == "saved")
  respond(reopened)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(native_buffer, 0, -1, false), { "Comment", "unsent ** literal", "tail" }),
    "replacement host snapshot erased retained annotation typing")
  assert(require("forge.editable").suspend_generated_text(owner.replica.editable))
  generation = 3
  assert(owner.close())
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Plan" }))
end, debug.traceback)
if owner then generation = generation + 1 owner.close() end
client.request_for, client.host_accepting, client.host_generation = original_request, original_accepting, original_generation
vim.fn.delete(path)
assert(success, failure)
print("plan_review_recovery: passed")
