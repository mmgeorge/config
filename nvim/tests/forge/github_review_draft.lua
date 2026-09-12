vim.loader.enable(false)
local calls = {}
package.loaded["forge.client"] = { request_host = function(route, params, callback)
  calls[#calls + 1] = { route = route, params = vim.deepcopy(params), callback = callback }
end }
local draft = require("github.review_draft")
local state = { pr = { repo = "owner/repo", number = 7 }, body = "first", remote_id = nil }
local function build()
  return { repo = state.pr.repo, number = 7, review_comments = { { body = state.body, remote_id = state.remote_id } } }
end
draft.enqueue(state, build)
state.body = "second"
draft.enqueue(state, build)
local resource = { repository = { hostname = require("github.repo_cache").hostname(), owner = "owner", name = "repo" }, kind = "pull_request", number = 7 }
local settled = false
draft.enqueue(state, function()
  local payload = build()
  payload.review_comments[1].remote_id = 42
  return payload
end, function(failure)
  assert(not failure)
  state.remote_id = 42
  settled = true
end, { resource = resource, capture = { operation_id = "captured-operation" } })
state.body = "newer unsent body"
draft.enqueue(state, build)
assert(#calls == 1)
calls[1].callback({})
assert(calls[2].params.draft.review_comments[1].body == "newer unsent body")
calls[2].callback({})
assert(calls[3].route == "github.recovery.settle_draft")
assert(calls[3].params.draft.review_comments[1].body == "newer unsent body")
assert(not settled)
calls[3].callback({})
assert(settled and calls[4].params.draft.review_comments[1].remote_id == 42)
assert(calls[4].params.draft.review_comments[1].body == "newer unsent body")
calls[4].callback({})
draft.enqueue(state, build)
local second_view = { pr = { repo = "owner/repo", number = 7 } }
local loaded
draft.read(second_view, function(result, failure) assert(not failure) loaded = result end)
assert(#calls == 5, "second view read bypassed an admitted draft publication")
calls[5].callback({})
assert(calls[6].route == "github.review.draft")
calls[6].callback({ review_comments = { { body = "newer unsent body", remote_id = 42 } } })
assert(loaded.review_comments[1].body == "newer unsent body")
print("review draft publication preserves new edits across durable settlement")
