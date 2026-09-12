local M = {}
local runner_for_test

local function request(method, params, callback)
  local delivered = false
  local function receive(result, failure)
    if delivered then return end
    delivered = true
    callback(result, failure and tostring(failure) or nil)
  end
  local ok, failure = pcall(function()
    if runner_for_test then runner_for_test(method, params, receive)
    else require("forge.client").request_host(method, params, receive) end
  end)
  if not ok then receive(nil, tostring(failure)) end
end

function M.context(directory, callback)
  local hostname = require("github.repo_cache").hostname()
  request("github.creation.context", { directory = directory }, function(context, failure)
    if failure then callback(nil, failure) return end
    if type(context) ~= "table" or type(context.repository) ~= "table"
      or context.repository.hostname ~= hostname or type(context.repository.owner) ~= "string"
      or type(context.repository.name) ~= "string" or type(context.repository_node_id) ~= "string"
      or type(context.branch) ~= "string" or type(context.head_commit) ~= "string" then
      callback(nil, "Invalid native PR creation context")
      return
    end
    callback(context)
  end)
end

function M.create(directory, context, base, metadata, callback)
  local mutation = { operation = "pull_request_create", repository_node_id = context.repository_node_id,
    title = metadata.title, body = metadata.body, base = base, head = context.branch,
    head_commit = context.head_commit, draft = true }
  local resource = { repository = vim.deepcopy(context.repository), kind = "repository", number = 0 }
  local repo = resource.repository.owner .. "/" .. resource.repository.name
  request("github.review.draft", { resource = resource }, function(stored, read_failure)
    if read_failure then callback(nil, read_failure) return end
    stored = type(stored) == "table" and stored or {}
    local previous = type(stored.creation) == "table" and stored.creation.sequence or 0
    if type(previous) ~= "number" or previous < 0 or previous >= 9007199254740991 or previous % 1 ~= 0 then
      callback(nil, "PR creation draft sequence is invalid") return
    end
    local draft = { repo = repo, number = 0, creation = { sequence = previous + 1, mutation = vim.deepcopy(mutation) } }
    request("github.review.draft.write", { resource = resource, draft = draft }, function(_, write_failure)
      if write_failure then callback(nil, write_failure) return end
      local function settle()
        return draft
      end
      local function completed(record, failure)
        if failure then callback(nil, failure, record) return end
        local phase = record and record.state and record.state.phase
        if phase == "confirmed" or phase == "user_linked" then callback(record.state.result, nil, record)
        else callback(nil, "PR creation was resolved without a confirmed new pull request", record) end
      end
      request("github.recovery.inspect", { resource = resource }, function(record, inspection_failure)
        if inspection_failure then callback(nil, inspection_failure) return end
        local mutations = require("github.mutation")
        if type(record) == "table" then
          mutations.resolve(directory, record, settle, completed)
          return
        end
        mutations.run({ directory = directory, repo = repo, kind = "repository", number = 0,
          edit_sequence = draft.creation.sequence, draft_target = "pr:create", mutation = mutation, settle = settle }, completed)
      end)
    end)
  end)
end

function M._set_runner_for_test(runner)
  runner_for_test = runner
end

return M
