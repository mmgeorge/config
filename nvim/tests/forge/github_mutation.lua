vim.loader.enable(false)
local mutation = require("github.mutation")
local notification = {}
vim.notify = function(message) notification[#notification + 1] = message end
local route, params, pending
mutation._set_runner_for_test(function(next_route, next_params, callback)
  route, params, pending = next_route, next_params, callback
end)
local result, failure, completed
local options = {
  directory = vim.fn.getcwd(), repo = "owner/repo", number = 7,
  mutation = { operation = "conversation_create", body = "captured\r\ntext" }, edit_sequence = 42,
  draft_target = "local-comment-1",
  settle = function(record)
    assert(record.capture.edit_sequence == 42)
    return { repo = "owner/repo", number = 7, review_comment_text = "newer unsent text" }
  end,
}
local function receive(record, diagnostic)
  result, failure, completed = record, diagnostic, (completed or 0) + 1
end
mutation.run(options, receive)
assert(route == "github.actor")
options.mutation.body = "newer unsent text"
pending({ login = "viewer", node_id = "ACTOR" })
assert(route == "github.review.mutate")
local capture = params.request
assert(capture.mutation.body == "captured\r\ntext" and #capture.operation_id == 64)
local record = { version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
  state = { phase = "confirmed", result = { id = 1 } } }
local mutation_reply = pending
pending(record)
assert(route == "github.recovery.settle_draft" and params.draft.review_comment_text == "newer unsent text")
assert(completed == nil)
pending({})
assert(completed == 1 and result == record and failure == nil)
mutation_reply(nil, "late duplicate")
assert(completed == 1)
mutation.run(options, receive)
pending({ login = "viewer", node_id = "ACTOR" })
capture = params.request
pending({ version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
  state = { phase = "outcome_unknown", diagnostic = "connection lost" } })
assert(route == "github.review.mutate", "unknown create was automatically reposted or acknowledged")
assert(completed == 2 and failure:find("durable recovery", 1, true))
assert(#notification == 1)
options.settle_async = function() error("injected publication failure") end
mutation.run(options, receive)
pending({ login = "viewer", node_id = "ACTOR" })
capture = params.request
pending({ version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
  state = { phase = "confirmed", result = { id = 2 } } })
assert(completed == 3 and failure:find("injected publication failure", 1, true))
assert(route == "github.review.mutate", "failed publication acknowledged its retained record")
options.settle_async = function(_, done) done() done("duplicate") end
mutation.run(options, receive)
pending({ login = "viewer", node_id = "ACTOR" })
capture = params.request
pending({ version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
  state = { phase = "confirmed", result = { id = 3 } } })
assert(completed == 4 and failure == nil)
mutation.run(options, receive)
pending({ login = "viewer", node_id = "ACTOR" })
capture = params.request
pending({ version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
  state = { phase = "rejected", diagnostic = "captured actor no longer authorized" } })
assert(completed == 5 and failure ~= nil and result.draft_acknowledged == true)
vim.ui.select = function() error("GitHub recovery must use the Forge choice popup") end
local origin = vim.api.nvim_get_current_win()
local recovery = { version = 1, resource = capture.resource,
  capture = { operation_id = "recovery-menu", actor = "ACTOR" }, state = { phase = "prepared" } }
local recovery_requests, recovery_result = {}, nil
mutation._set_runner_for_test(function(next_route, next_params, callback)
  recovery_requests[#recovery_requests + 1] = { route = next_route, params = next_params }
  callback(next_route == "github.recovery.resolve" and recovery or {})
end)
local function resolve()
  mutation.resolve(vim.fn.getcwd(), recovery, function() end, function(value, diagnostic) recovery_result = value or diagnostic end)
end
resolve()
assert(vim.bo.filetype == "ForgeChoicePopup")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), {
  "", "  [k]  Keep captured draft", "  [c]  Close without dispatch", "  [q]  cancel", "",
}))
vim.api.nvim_feedkeys("q", "x", false)
assert(#recovery_requests == 0 and recovery_result == "Recovery remains unresolved")
assert(vim.api.nvim_get_current_win() == origin)
resolve()
vim.api.nvim_feedkeys("c", "x", false)
assert(recovery_requests[1].route == "github.recovery.resolve")
assert(recovery_requests[1].params.resolution.resolution == "not_dispatched")
assert(recovery_requests[2].route == "github.recovery.ack" and recovery_result == recovery)
recovery.state.phase = "outcome_unknown"
resolve()
assert(vim.api.nvim_buf_get_lines(0, 2, 3, false)[1] == "  [l]  Link an existing remote result")
vim.api.nvim_feedkeys("k", "x", false)
assert(#recovery_requests == 2)
recovery.state.phase = "confirmed"
resolve()
assert(vim.api.nvim_buf_get_lines(0, 2, 3, false)[1] == "  [r]  Retry durable settlement")
vim.api.nvim_feedkeys("r", "x", false)
assert(#recovery_requests == 3 and recovery_requests[3].route == "github.recovery.ack")
mutation._set_runner_for_test(nil)
print("github mutation capture and durable acknowledgement tests passed")
