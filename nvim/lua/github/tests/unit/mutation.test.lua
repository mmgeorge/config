-- Unit tests for `github.mutation` (mutation.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["mutation"] = function()
  local mutation = require("github.mutation")
  local notification = notify.capture()
  local route, params, pending
  mutation._set_runner_for_test(function(next_route, next_params, callback)
    route, params, pending = next_route, next_params, callback
  end)
  local result, failure, completed
  local options = {
    repo = "owner/repo", number = 7,
    mutation = { operation = "conversation_create", body = "captured\r\ntext" }, edit_sequence = 42,
    draft_target = "local-comment-1",
    settle = function(record)
      eq(record.capture.edit_sequence, 42)
      return { repo = "owner/repo", number = 7, review_comment_text = "newer unsent text" }
    end,
  }
  local function receive(record, diagnostic)
    result, failure, completed = record, diagnostic, (completed or 0) + 1
  end
  mutation.run(options, receive)
  eq(route, "github.actor.get")
  options.mutation.body = "newer unsent text"
  pending({ login = "viewer", node_id = "ACTOR" })
  eq(route, "github.review.mutate")
  local capture = params.request
  assert(capture.mutation.body == "captured\r\ntext" and #capture.operation_id == 64)
  local record = { version = 1, resource = capture.resource,
    capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
    state = { phase = "confirmed", result = { id = 1 } } }
  local mutation_reply = pending
  pending(record)
  assert(route == "github.recovery.draft.settle" and params.draft.review_comment_text == "newer unsent text")
  eq(completed, nil)
  pending({})
  assert(completed == 1 and result == record and failure == nil)
  mutation_reply(nil, "late duplicate")
  eq(completed, 1)
  mutation.run(options, receive)
  pending({ login = "viewer", node_id = "ACTOR" })
  capture = params.request
  pending({ version = 1, resource = capture.resource,
    capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
    state = { phase = "outcome_unknown", diagnostic = "connection lost" } })
  eq(route, "github.review.mutate", { fail_reason = "unknown create was automatically reposted or acknowledged" })
  assert(completed == 2 and failure:find("durable recovery", 1, true))
  eq(#notification, 1)
  options.settle_async = function() error("injected publication failure") end
  mutation.run(options, receive)
  pending({ login = "viewer", node_id = "ACTOR" })
  capture = params.request
  pending({ version = 1, resource = capture.resource,
    capture = { operation_id = capture.operation_id, actor = "ACTOR", edit_sequence = 42 },
    state = { phase = "confirmed", result = { id = 2 } } })
  assert(completed == 3 and failure:find("injected publication failure", 1, true))
  eq(route, "github.review.mutate", { fail_reason = "failed publication acknowledged its retained record" })
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
    mutation.resolve(recovery, function() end, function(value, diagnostic) recovery_result = value or diagnostic end)
  end
  resolve()
  eq(vim.bo.filetype, "ForgeChoicePopup")
  assert(vim.deep_equal(buffer.lines(0), {
    "", "  [k]  Keep captured draft", "  [c]  Close without dispatch", "  [q]  cancel", "",
  }))
  keymap.feed("q", "x")
  assert(#recovery_requests == 0 and recovery_result == "Recovery remains unresolved")
  assert(vim.api.nvim_get_current_win() == origin)
  resolve()
  keymap.feed("c", "x")
  eq(recovery_requests[1].route, "github.recovery.resolve")
  eq(recovery_requests[1].params.resolution.resolution, "not_dispatched")
  assert(recovery_requests[2].route == "github.recovery.acknowledge" and recovery_result == recovery)
  recovery.state.phase = "outcome_unknown"
  resolve()
  eq(buffer.line(0, 3), "  [l]  Link an existing remote result")
  keymap.feed("k", "x")
  eq(#recovery_requests, 2)
  recovery.state.phase = "confirmed"
  resolve()
  eq(buffer.line(0, 3), "  [r]  Retry durable settlement")
  keymap.feed("r", "x")
  assert(#recovery_requests == 3 and recovery_requests[3].route == "github.recovery.acknowledge")
  mutation._set_runner_for_test(nil)
end

return T
