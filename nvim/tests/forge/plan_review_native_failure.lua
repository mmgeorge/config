vim.loader.enable(false)

local client = require("forge.client")
local session = require("forge.session").harness
local original_request, original_accepting = client.request_for, client.host_accepting
client.host_accepting = function() return true end
local path = vim.fn.tempname() .. ".md"
vim.fn.writefile({ "# Physical plan" }, path)
local source_window, source_tab = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_tabpage()
local success, failure = xpcall(function()
  session.session = { id = "plan-native-failure" }
  session.transcript_win = source_window
  client.request_for = function(_, method, params, callback)
    assert(method == "harness.document")
    if params.operation == "plan_open" then callback(nil, "host rejected plan attachment")
    else callback({}) end
  end
  require("forge.views.plan_review.native_controller").open({
    id = "plan-native-failure", working_path = path, review_digest = "digest",
  })
  assert(vim.wait(1000, function() return session.plan_review == nil end), "failed native attachment retained the session review")
  assert(vim.api.nvim_get_current_tabpage() == source_tab, "failed native attachment retained its review tab")
  assert(vim.api.nvim_win_get_buf(source_window) ~= -1, "failed native attachment invalidated the origin window")
  assert(vim.deep_equal(vim.fn.readfile(path), { "# Physical plan" }), "failed native attachment changed physical plan text")
end, debug.traceback)
if session.plan_review and session.plan_review.owner then session.plan_review.owner.close() end
client.request_for, client.host_accepting = original_request, original_accepting
vim.fn.delete(path)
assert(success, failure)
print("plan_review_native_failure: passed")
