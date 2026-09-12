local M = {}

---@class GithubMutationResource
---@field repository {hostname:string, owner:string, name:string}
---@field kind 'pull_request'|'issue'|'notification'|'repository'
---@field number integer

---@class GithubMutationCapture
---@field resource GithubMutationResource
---@field operation_id string
---@field actor_node_id string
---@field edit_sequence integer|nil
---@field draft_target string|nil
---@field parent_node_id string|nil
---@field mutation table<string, any>

---@class GithubMutationRecord
---@field version integer
---@field resource GithubMutationResource
---@field capture {operation_id:string, actor:string, edit_sequence:integer|nil, submitted:GithubMutationCapture}
---@field state {phase:string, result:table|nil, diagnostic:string|nil}
---@field draft_acknowledged boolean|nil

---@class GithubMutationOptions
---@field directory string
---@field repo string
---@field number integer
---@field kind 'pull_request'|'issue'|'notification'|'repository'|nil
---@field mutation table<string, any>
---@field edit_sequence integer|nil
---@field draft_target string|nil
---@field parent_node_id string|nil
---@field settle fun(record:GithubMutationRecord):table|nil
---@field settle_async? fun(record:GithubMutationRecord, done:fun(failure:string|nil))

local sequence = 0
local runner_for_test

---@param route string
---@param params table
---@param callback fun(result:any, failure:string|nil)
local function request(route, params, callback)
  local delivered = false
  local function receive(result, failure)
    if delivered then return end
    delivered = true
    callback(result, failure and tostring(failure) or nil)
  end
  local ok, failure = pcall(function()
    if runner_for_test then runner_for_test(route, params, receive)
    else require("forge.client").request_host(route, params, receive) end
  end)
  if not ok then receive(nil, tostring(failure)) end
end

---@param message string
local function notify(message)
  vim.notify(message, vim.log.levels.ERROR, { title = "Forge GitHub" })
end

local function settle_capture(settle, record, callback)
  local delivered = false
  local function finish(failure)
    if delivered then return end
    delivered = true
    callback(failure)
  end
  local ok, failure = pcall(settle, record, finish)
  if not ok then finish("Captured draft settlement failed: " .. tostring(failure)) end
end

function M.submission_context(directory, repo, number, pending_review_id, expected, callback)
  local owner, name = tostring(repo):match("^([^/]+)/([^/]+)$")
  if not owner then callback(nil, "Invalid review repository") return end
  local hostname = require("github.repo_cache").hostname()
  request("github.review.submission_context", {
    directory = directory, repository = { hostname = hostname, owner = owner, name = name },
    number = number, pending_review_id = pending_review_id, expected = expected,
  }, function(context, failure)
    if require("github.repo_cache").hostname() ~= hostname then callback(nil, "GitHub hostname changed during review capture")
    else callback(context, failure) end
  end)
end

---@param options GithubMutationOptions
---@param callback fun(record:GithubMutationRecord|nil, failure:string|nil)
function M.run(options, callback)
  local owner, name = tostring(options.repo):match("^([^/]+)/([^/]+)$")
  local valid_number = type(options.number) == "number" and options.number % 1 == 0
    and (options.kind == "repository" and options.number == 0 or options.kind ~= "repository" and options.number >= 1)
  if not owner or not valid_number then
    local failure = "Invalid captured GitHub mutation resource"
    notify(failure)
    callback(nil, failure)
    return
  end
  local hostname = require("github.repo_cache").hostname()
  local repository = { hostname = hostname, owner = owner, name = name }
  sequence = sequence + 1
  local capture = {
    resource = { repository = repository, kind = options.kind or "pull_request", number = options.number },
    operation_id = vim.fn.sha256(table.concat({ vim.fn.getpid(), vim.uv.hrtime(), sequence }, ":")),
    mutation = vim.deepcopy(options.mutation),
    edit_sequence = options.edit_sequence,
    draft_target = options.draft_target,
    parent_node_id = options.parent_node_id,
  }
  local function fail(failure, record)
    notify(failure)
    callback(record, failure)
  end
  request("github.actor", { directory = options.directory, repository = repository }, function(actor, failure)
    if failure then fail(failure) return end
    if type(actor) ~= "table" or type(actor.node_id) ~= "string" or actor.node_id == "" then
      fail("Forge returned invalid authenticated GitHub identity")
      return
    end
    if require("github.repo_cache").hostname() ~= hostname then fail("GitHub hostname changed before mutation capture") return end
    capture.actor_node_id = actor.node_id
    request("github.review.mutate", { directory = options.directory, request = capture }, function(record, mutation_failure)
      if mutation_failure then
        request("github.recovery.inspect", { resource = capture.resource }, function(recovery, inspection_failure)
          fail(mutation_failure .. (inspection_failure and (". Recovery inspection failed: " .. inspection_failure)
            or ". Inspect the durable operation before retrying."), recovery)
        end)
        return
      end
      if type(record) ~= "table" or record.version ~= 1 or type(record.capture) ~= "table"
        or record.capture.operation_id ~= capture.operation_id or record.capture.actor ~= capture.actor_node_id
        or not vim.deep_equal(record.resource, capture.resource) or type(record.state) ~= "table" then
        fail("Forge returned an invalid durable operation receipt. Inspect recovery before retrying.")
        return
      end
      local phase = record.state.phase
      if phase == "outcome_unknown" or phase == "dispatch_possible" or phase == "prepared" then
        fail((record.state.diagnostic or "GitHub outcome is unknown") .. ". Captured text remains in durable recovery.", record)
        return
      end
      if phase ~= "confirmed" and phase ~= "rejected" then fail("Unexpected GitHub mutation settlement phase", record) return end
      if capture.edit_sequence ~= nil and options.settle_async then
        settle_capture(options.settle_async, record, function(settlement_failure)
          if settlement_failure then fail(settlement_failure, record)
          else
            record.draft_acknowledged = true
            if phase == "rejected" then fail(record.state.diagnostic or "GitHub rejected the mutation", record)
            else callback(record) end
          end
        end)
        return
      end
      local params = { resource = capture.resource, operation_id = capture.operation_id }
      local route = "github.recovery.ack"
      if capture.edit_sequence ~= nil then
        if type(options.settle) ~= "function" then fail("Editable mutation requires a durable draft settlement", record) return end
        local settled, draft = pcall(options.settle, record)
        if not settled or type(draft) ~= "table" then fail("Could not settle the captured draft: " .. tostring(draft), record) return end
        params.draft = draft
        route = "github.recovery.settle_draft"
      end
      request(route, params, function(_, acknowledgement_failure)
        if acknowledgement_failure then fail("GitHub completed but durable settlement failed: " .. acknowledgement_failure, record) return end
        record.draft_acknowledged = true
        if phase == "rejected" then fail(record.state.diagnostic or "GitHub rejected the mutation", record) return end
        callback(record)
      end)
    end)
  end)
end

---@param directory string
---@param record GithubMutationRecord
---@param settle fun(record:GithubMutationRecord):table|nil
---@param callback fun(record:GithubMutationRecord|nil, failure:string|nil)
function M.resolve(directory, record, settle, callback, settle_async)
  local terminal = record.state.phase == "confirmed" or record.state.phase == "rejected"
    or record.state.phase == "user_linked" or record.state.phase == "user_closed_unknown"
  local choices = terminal and { "Keep retained settlement", "Retry durable settlement" }
    or record.state.phase == "prepared" and { "Keep captured draft", "Close without dispatch" }
    or { "Keep outcome unresolved", "Link an existing remote result", "Close with unknown remote outcome" }
  local options = {}
  local keys = { "k", terminal and "r" or record.state.phase == "prepared" and "c" or "l", "c" }
  for index, label in ipairs(choices) do options[#options + 1] = { key = keys[index], value = label, label = label } end
  require("forge.infra.choice_popup").open({ title = "Resolve captured GitHub operation", options = options, on_choice = function(choice)
    if not choice or choice == choices[1] then callback(nil, "Recovery remains unresolved") return end
    local function resolve(resolution)
      local function finish_resolution(resolved, failure)
        if failure then notify(failure) callback(nil, failure) return end
        if type(resolved) ~= "table" or resolved.version ~= 1 or type(resolved.capture) ~= "table"
          or resolved.capture.operation_id ~= record.capture.operation_id
          or resolved.capture.actor ~= record.capture.actor
          or not vim.deep_equal(resolved.resource, record.resource) or type(resolved.state) ~= "table" then
          callback(record, "Recovery response does not identify the captured operation")
          return
        end
        if record.capture.edit_sequence ~= nil and settle_async then
          settle_capture(settle_async, resolved, function(settlement_failure)
            if settlement_failure then notify(settlement_failure) end
            callback(resolved, settlement_failure)
          end)
          return
        end
        local params = { resource = record.resource, operation_id = record.capture.operation_id }
        local route = "github.recovery.ack"
        if record.capture.edit_sequence ~= nil then
          local settled, draft = pcall(settle, resolved)
          if not settled or type(draft) ~= "table" then
            local diagnostic = "Resolved outcome remains retained because draft settlement failed: " .. tostring(draft)
            notify(diagnostic) callback(resolved, diagnostic) return
          end
          params.draft = draft
          route = "github.recovery.settle_draft"
        end
        request(route, params, function(_, acknowledgement_failure)
          if acknowledgement_failure then notify(acknowledgement_failure) end
          callback(resolved, acknowledgement_failure)
        end)
      end
      if resolution == nil then finish_resolution(record) return end
      request("github.recovery.resolve", {
        directory = directory, resource = record.resource, operation_id = record.capture.operation_id, resolution = resolution,
      }, finish_resolution)
    end
    if choice == "Retry durable settlement" then resolve(nil)
    elseif choice == "Link an existing remote result" then
      require("forge.infra.popup_window").input({ prompt = "Remote result numeric ID: " }, function(value)
        local id = tonumber(value)
        if not id or id < 1 or id % 1 ~= 0 then callback(nil, "Recovery remains unresolved") return end
        resolve({ resolution = "link", remote_id = id })
      end)
    else
      resolve({ resolution = choice == "Close without dispatch" and "not_dispatched" or "close_unknown" })
    end
  end })
end

---@param runner fun(route:string, params:table, callback:fun(result:any, failure:string|nil))|nil
function M._set_runner_for_test(runner)
  runner_for_test = runner
end

return M
