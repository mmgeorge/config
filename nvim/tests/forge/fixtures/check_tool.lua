local M = {}

---Start a saved check through a real invocation-owned check_run tool request.
---@param request fun(method: string, params?: table): table
---@param arguments table
---@return table
function M.start(request, arguments)
  local before = {}
  for _, run in ipairs(request("session.check.list").history or {}) do before[run.id] = true end
  request("session.provider.mock_response", { operation = "script", script = {
    steps = { { kind = "tool_call", name = "check_run", arguments = arguments } },
    terminal = "completed",
  } })
  request("session.prompt.submit", { text = "Run the selected fixture checks through check_run." })
  local run_id
  local answered = {}
  assert(vim.wait(10000, function()
    local state = require("forge.session").harness
    for _, approval in ipairs(state.approval or {}) do
      if not answered[approval.id] then
        answered[approval.id] = true
        local answers = {}
        for _, item in ipairs(approval.item_list) do
          answers[#answers + 1] = { item_id = item.id, choice_id = "allow_once" }
        end
        request("session.approval.resolve", { approval_id = approval.id, answer_list = answers })
      end
    end
    for _, run in ipairs(request("session.check.list").history or {}) do
      if not before[run.id] then run_id = run.id return true end
    end
    return false
  end, 10), "invocation-owned check_run did not admit a run")
  return { run_id = run_id }
end

return M
