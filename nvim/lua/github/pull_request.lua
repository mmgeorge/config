---@class GithubPullRequestTarget
---@field repo string
---@field number integer
---@field node_id string

---@class GithubPullRequestResult
---@field ok boolean
---@field outcome? 'confirmed'|'rejected'|'outcome_unknown'
---@field state? 'OPEN'|'CLOSED'|'MERGED'
---@field is_draft? boolean
---@field message? string
---@field recovery? GithubMutationRecord

local M = {}

---@param cwd string
---@param target GithubPullRequestTarget
---@param desired 'DRAFT'|'OPEN'|'CLOSED'
---@param callback fun(result:GithubPullRequestResult)
function M.transition_async(cwd, target, desired, callback)
  if type(target.node_id) ~= "string" or target.node_id == ""
    or (desired ~= "DRAFT" and desired ~= "OPEN" and desired ~= "CLOSED") then
    local failure = "Invalid captured GitHub pull request transition"
    vim.notify(failure, vim.log.levels.ERROR, { title = "Forge GitHub" })
    callback({ ok = false, outcome = "rejected", message = failure })
    return
  end
  require("github.mutation").run({
    directory = cwd, repo = target.repo, number = target.number,
    mutation = { operation = "pull_request_transition", node_id = target.node_id, desired = desired },
  }, function(record, failure)
    local observed = record and record.state and record.state.result
    if not observed and record and record.confirmed_steps then observed = record.confirmed_steps[#record.confirmed_steps] end
    callback({
      ok = failure == nil,
      outcome = failure == nil and "confirmed" or (record and record.state.phase == "rejected" and "rejected" or "outcome_unknown"),
      state = observed and observed.state,
      is_draft = observed and observed.isDraft,
      message = failure,
      recovery = failure and record and not record.draft_acknowledged and record or nil,
    })
  end)
end

return M
