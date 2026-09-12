vim.loader.enable(false)
local pull_request = require("github.pull_request")
local mutation = require("github.mutation")
local notifications = {}
vim.notify = function(message) notifications[#notifications + 1] = message end
local route, params, pending
mutation._set_runner_for_test(function(next_route, next_params, callback)
  route, params, pending = next_route, next_params, callback
end)
local target = { repo = "owner/repo", number = 7, node_id = "PR_fixture" }
local result
pull_request.transition_async(vim.fn.getcwd(), target, "DRAFT", function(value) result = value end)
assert(route == "github.actor")
pending({ login = "viewer", node_id = "ACTOR" })
assert(route == "github.review.mutate")
local capture = params.request
assert(capture.mutation.node_id == "PR_fixture" and capture.mutation.desired == "DRAFT")
pending({ version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR" },
  state = { phase = "confirmed", result = { state = "OPEN", isDraft = true } },
  confirmed_steps = { { state = "OPEN", isDraft = false } } })
assert(route == "github.recovery.ack" and result == nil)
pending({})
assert(result.ok and result.state == "OPEN" and result.is_draft)
pull_request.transition_async(vim.fn.getcwd(), target, "DRAFT", function(value) result = value end)
pending({ login = "viewer", node_id = "ACTOR" })
capture = params.request
pending({ version = 1, resource = capture.resource,
  capture = { operation_id = capture.operation_id, actor = "ACTOR" },
  state = { phase = "outcome_unknown", diagnostic = "draft outcome unknown" },
  confirmed_steps = { { state = "OPEN", isDraft = false } } })
assert(not result.ok and result.outcome == "outcome_unknown")
assert(result.state == "OPEN" and result.is_draft == false and result.recovery)
assert(route == "github.review.mutate", "uncertain transition automatically retried")
pull_request.transition_async(vim.fn.getcwd(), target, "MERGED", function(value) result = value end)
assert(not result.ok and result.outcome == "rejected")
assert(#notifications == 2)
mutation._set_runner_for_test(nil)
print("github PR durable transition tests passed")