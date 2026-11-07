-- Resolve support paths at load time; host fixtures change into their workspace afterwards.
local approval = dofile(vim.fs.dirname(vim.fs.dirname(vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")))
  .. "/support/approval.lua")
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
  local responder = approval.responder()
  assert(vim.wait(10000, function()
    approval.answer_pending(responder)
    for _, run in ipairs(request("session.check.list").history or {}) do
      if not before[run.id] then run_id = run.id return true end
    end
    return false
  end, 10), "invocation-owned check_run did not admit a run")
  return { run_id = run_id }
end

return M
