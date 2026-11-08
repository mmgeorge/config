-- Host fixtures that grant tool approvals answer each pending request once through the same
-- `session.approval.resolve` route the approval picker uses.
local client = require("forge.client")
local M = {}

---@class ForgeFixtureApprovalResponder
---@field answered table<string, true>
---@field choice_id string
---@field on_failure? fun(failure: string) Receives resolution failures instead of raising them.
---@field failure? string The first unhandled resolution failure, raised by the next `answer_pending`.

---Create a responder that answers every target of each newly pending approval request.
---@param choice_id? string Defaults to `allow_once`.
---@param on_failure? fun(failure: string)
---@return ForgeFixtureApprovalResponder
function M.responder(choice_id, on_failure)
  return { answered = {}, choice_id = choice_id or "allow_once", on_failure = on_failure }
end

---Answer approval requests that this responder has not answered yet.
---A resolution failure arrives in an asynchronous callback, where raising would only log it and leave
---the request unanswered until the fixture times out. The failure is therefore retained and raised
---here, inside the fixture's own wait loop, so the fixture fails with the host's diagnostic.
---@param responder ForgeFixtureApprovalResponder
---@param on_request? fun(request: ForgeApprovalRequest) Inspects each request before it is answered.
function M.answer_pending(responder, on_request)
  if responder.failure then error("approval resolution failed: " .. responder.failure, 0) end
  local state = require("forge.session").harness
  for _, approval in ipairs(state.approval or {}) do
    if not responder.answered[approval.request_id] then
      responder.answered[approval.request_id] = true
      if on_request then on_request(approval) end
      local answers = {}
      for _, target in ipairs(approval.targets) do
        answers[#answers + 1] = { item_id = target.target_id, choice_id = responder.choice_id }
      end
      client.request({ method = "session.approval.resolve", session_id = assert(state.session).id,
        params = { approval_id = approval.request_id, answer_list = answers } }, function(_, failure)
        if not failure then return end
        if responder.on_failure then
          responder.on_failure(tostring(failure))
        else
          responder.failure = responder.failure or (approval.request_id .. ": " .. tostring(failure))
        end
      end)
    end
  end
end

return M
