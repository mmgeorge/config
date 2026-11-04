-- Load only in a dedicated, visible `nvim -u NONE -i NONE --listen ...` process.
-- This driver opens real provider sessions. It never builds or starts model input automatically.
vim.loader.enable(false)

---@class ForgeProviderApiLiveOptions
---@field provider 'codex'|'copilot'
---@field case? string
---@field root? string Fresh artifact directory; must not already exist.
---@field binary? string Rebuilt development Forge executable.
---@field command? string[] Explicit installed runtime override; otherwise use the provider registry.
---@field workspace_files? table<string,string> Relative fixture paths and UTF-8 contents installed before provider open.
local options = assert(vim.g.forge_provider_api_live, "set vim.g.forge_provider_api_live before loading this driver")
assert(options.provider == "codex" or options.provider == "copilot", "select codex or copilot")
assert(#vim.api.nvim_list_uis() > 0, "live verification requires a visible Terminal MCP Neovim")
assert(vim.v.servername:find("forge-provider-api-live", 1, true), "use a dedicated --listen address containing forge-provider-api-live")
assert(not _G.ProviderApiLive, "one driver instance per fresh Neovim process")

local source = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p")
local repository = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(vim.fs.normalize(source)))))
vim.opt.runtimepath:prepend(vim.fs.joinpath(repository, "nvim"))
local original_builder = require("forge.builder").for_profile("dev")
local source_binary = vim.fn.fnamemodify(options.binary or original_builder.binary_path(), ":p")
assert(vim.fn.executable(source_binary) == 1, "rebuild the development Forge host before launching live verification: " .. source_binary)
-- Leave room for the production executable lease's nested hashes under Windows MAX_PATH.
local default_root = vim.fs.joinpath(vim.uv.os_tmpdir(),
  ("fpl-%d-%.0f"):format(vim.fn.getpid(), vim.uv.hrtime()))
local root = vim.fn.fnamemodify(options.root or default_root, ":p")
assert(not vim.uv.fs_stat(root), "artifact root must be fresh: " .. root)
local workspace, data, config = vim.fs.joinpath(root, "workspace"), vim.fs.joinpath(root, "data"), vim.fs.joinpath(root, "config")
local artifacts, hooks = vim.fs.joinpath(root, "artifacts"), vim.fs.joinpath(root, "empty-hooks")
for _, path in ipairs({ workspace, data, config, artifacts, hooks }) do assert(vim.fn.mkdir(path, "p") == 1, path) end
local binary = vim.fs.joinpath(root, "forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or ""))
assert(vim.uv.fs_copyfile(source_binary, binary))
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  if kind == "data" then return data end
  if kind == "config" then return config end
  if kind == "cache" or kind == "state" or kind == "log" then
    local path = vim.fs.joinpath(root, kind)
    vim.fn.mkdir(path, "p")
    return path
  end
  return original_stdpath(kind)
end
-- Pin the reviewed executable while keeping the production client and executable lease path.
package.loaded["forge.builder"] = setmetatable({
  ensure = function(callback)
    vim.schedule(function() callback({ ok = true, path = binary }) end)
    return function() end
  end,
  binary_path = function() return binary end,
}, { __index = original_builder })

local client = require("forge.client")
local contract = require("forge.event_contract")
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local requests, order, notices = {}, {}, {}
local phase, event_count, sequence, runtime_model = "preparing_workspace", 0, 0, nil
local pid, socket = vim.fn.getpid(), vim.v.servername

---@class ForgeLiveRequest
---@field id string
---@field method string
---@field status 'pending'|'completed'|'failed'
---@field request_id? integer
---@field session_id? string
---@field result? any
---@field error? string
---@field detail? table<string, any>
---@field started_ms number
---@field completed_ms? number

---@class ForgeProviderApiLive
local M = {}
_G.ProviderApiLive = M

---@return number
local function now() return math.floor(vim.uv.hrtime() / 1e6) end

---@param name string
---@param value any JSON-compatible artifact.
local function save(name, value)
  vim.fn.writefile({ vim.json.encode(value) }, vim.fs.joinpath(artifacts, name))
end

---@param name string
---@param value any JSON-compatible evidence record.
local function append(name, value)
  vim.fn.writefile({ vim.json.encode(value) }, vim.fs.joinpath(artifacts, name), "a")
end

---@return table<string, any>
function M.info()
  local state = session.harness
  return {
    pid = pid, socket = socket, repository = repository, root = root, workspace = workspace,
    data = data, data_root = vim.fs.joinpath(data, "forge", "harness"), config = config, artifacts = artifacts,
    source_binary = source_binary, binary = binary, case = options.case or "unspecified", provider = options.provider,
    requested_model = "gpt-6-luna", requested_effort = "medium", phase = phase,
    wire_version = contract.version,
    host_pid = client._client and client._client.process and client._client.process.pid,
    session_id = state.session and state.session.id, model = state.session and state.session.model,
    resolved_model = state.session and state.session.resolved_model, effort = state.session and state.session.effort,
    runtime_model = runtime_model, ready = state.ready == true, busy = state.busy == true,
    state_sync_pending = state.state_sync_pending == true, host_error = state.host_error,
    events = event_count, notices = #notices, composer_buffer = state.composer_buf, transcript_buffer = state.transcript_buf,
  }
end

---@param method string
---@param params? table<string, any>
---@param target? string Explicit session from this isolated host.
---@param completed? fun(record:ForgeLiveRequest)
---@return string ticket Local observation identity; pending never implies native failure.
local function request(method, params, target, completed)
  local policy = assert(contract.method_policy(method), "unknown production route: " .. method)
  sequence = sequence + 1
  local id = ("request-%04d"):format(sequence)
  local session_id
  if policy.session_context == "required" then session_id = target or assert(session.harness.session, "Harness is not initialized").id
  elseif policy.session_context == "optional" then session_id = target or session.harness.session and session.harness.session.id end
  local record = { id = id, method = method, status = "pending", session_id = session_id, started_ms = now() }
  requests[id], order[#order + 1] = record, id
  append("requests.jsonl", { kind = "started", id = id, method = method, session_id = session_id, params = params or {}, time_ms = now() })
  local ok, result = pcall(client.request, { method = method, params = params or {}, session_id = session_id }, function(value, failure, detail)
    record.result, record.error, record.detail = value, failure, detail
    record.status, record.completed_ms = failure and "failed" or "completed", now()
    save(id .. ".json", record)
    append("requests.jsonl", { kind = "finished", id = id, status = record.status, error = failure, detail = detail, time_ms = now() })
    if completed then completed(record) end
  end)
  if ok then record.request_id = result else
    record.status, record.error, record.completed_ms = "failed", tostring(result), now()
    save(id .. ".json", record)
    if completed then completed(record) end
  end
  return id
end

---@param method string
---@param params? table<string, any>
---@param target? string
---@return string ticket
function M.request(method, params, target) return request(method, params, target) end

---@param id string
---@return ForgeLiveRequest
function M.result(id) return vim.deepcopy(assert(requests[id], "unknown local ticket: " .. id)) end

---@param id string
---@param milliseconds? integer Maximum 15 seconds; a timeout retains the pending observation.
---@return ForgeLiveRequest
function M.await(id, milliseconds)
  local record = assert(requests[id], "unknown local ticket: " .. id)
  vim.wait(math.max(0, math.min(milliseconds or 1000, 15000)), function() return record.status ~= "pending" end, 20)
  return M.result(id)
end

---@return table<string, any>[]
function M.requests()
  local rows = {}
  for _, id in ipairs(order) do
    local record = requests[id]
    rows[#rows + 1] = { id = id, method = record.method, status = record.status, request_id = record.request_id,
      command_id = record.detail and record.detail.operation_id or type(record.result) == "table" and (record.result.id or record.result.operation_id), error = record.error }
  end
  return rows
end

---@param text string
function M.draft(text)
  local buffer = assert(session.harness.composer_buf, "composer is unavailable")
  assert(vim.api.nvim_buf_is_valid(buffer), "composer buffer was closed")
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, vim.split(text, "\n", { plain = true }))
end

---@param text string
---@return table<string, any> UI admission snapshot; inspect input/state for settlement.
function M.submit(text)
  M.draft(text)
  append("actions.jsonl", { action = "submit", text = text, time_ms = now() })
  controller.submit()
  return M.info()
end

---@param text string
---@return table<string, any>
function M.queue(text)
  M.draft(text)
  append("actions.jsonl", { action = "queue", text = text, time_ms = now() })
  controller.queue_submit()
  return M.info()
end

---@return table<string, any>
function M.cancel()
  append("actions.jsonl", { action = "cancel", time_ms = now() })
  controller.cancel_turn()
  return M.info()
end

---@param values table<string, any>
---@return string
function M.configure(values) return request("session.configure", values) end
---@param change table<string, any>
---@return string
function M.permission(change) return request("session.permission.set", { change = change }) end
---@param action table<string, any> Existing task action, such as goal, pause, resume, or clear.
---@return string
function M.task(action) return request("session.command.apply", action) end
---@return string
function M.state() return request("session.state.get") end
---@return string
function M.input() return request("session.input.get") end
---@return string
function M.models() return request("session.provider.model.list") end
---@return string
function M.inspect() return request("session.provider.setup") end
---@return string
function M.capabilities() return request("session.provider.capabilities.get") end
---@return string
function M.mcp() return request("session.provider.mcp.list") end
---@return string
function M.skills() return request("session.provider.skill.list") end
---@return string
function M.settings() return request("settings.get") end
---@return string
function M.exchanges() return request("session.exchange.list") end
---@return string
function M.plans() return request("session.plan.list") end
---@param plan_id string
---@return string
function M.plan(plan_id) return request("session.plan.get", { plan_id = plan_id }) end
---@param operation_id string Observed command identity, not the local request ticket.
---@return string
function M.command(operation_id) return request("session.command.operation.get", { operation_id = operation_id }) end
---@return string
function M.terminals() return request("session.document", { operation = "background_terminals" }) end
---@param id string Observed binding-scoped terminal identity.
---@return string
function M.terminate(id) return request("session.document", { operation = "terminate_terminal", id = id }) end
---@return string
function M.recap() return request("session.document", { operation = "recap", model = "gpt-6-luna" }) end
---@return table<string, any>[]
function M.approvals() return vim.deepcopy(session.harness.approval or {}) end
---@return table<string, any>
function M.questions() return vim.deepcopy(session.harness.questions or {}) end
---@param id string Observed approval identity.
---@param answers {item_id:string,choice_id:string}[] Choices captured from that approval.
---@return string
function M.approve(id, answers) return request("session.approval.resolve", { approval_id = id, answer_list = answers }) end
---@param command table<string, any> Exact observed question revision and typed answer action.
---@return string
function M.answer(command) return request("session.question.answer", { command = command }) end

---@param label? string
---@return string path Snapshot containing actual UI text, observed state, and request identities.
function M.capture(label)
  local name = (label or "snapshot"):gsub("[^%w_.-]", "_") .. "-" .. tostring(now()) .. ".json"
  local buffers = {}
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buffer) then
      buffers[#buffers + 1] = { id = buffer, name = vim.api.nvim_buf_get_name(buffer), filetype = vim.bo[buffer].filetype,
        lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false) }
    end
  end
  save(name, { info = M.info(), buffers = buffers, requests = M.requests(), approvals = M.approvals(), questions = M.questions(),
    session = session.harness.session, mode = vim.api.nvim_get_mode(), windows = vim.api.nvim_list_wins() })
  save("manifest.json", M.info())
  return vim.fs.joinpath(artifacts, name)
end

---@param quit? boolean Explicitly terminate only this driver's dedicated editor after host collection.
function M.stop(quit)
  M.capture("before-stop")
  client.host_stop("provider API live verification complete", function()
    phase = "stopped"
    save("manifest.json", M.info())
    if quit then assert(vim.fn.getpid() == pid and vim.v.servername == socket) vim.cmd("qa!") end
  end)
end

local notifications = require("forge.infra.notifications")
local original_error = notifications.error
notifications.error = function(message, title)
  notices[#notices + 1] = { message = tostring(message), title = title, time_ms = now() }
  append("notifications.jsonl", notices[#notices])
  return original_error(message, title)
end
client.session_events_subscribe(function(event, payload, session_id)
  event_count = event_count + 1
  if event == "provider_event" and payload.kind == "runtime_resolved" and payload.data then runtime_model = payload.data.model end
  append("events.jsonl", { event = event, payload = payload, session_id = session_id, time_ms = now() })
end)

local function start_harness()
  phase = "opening_harness"
  vim.fn.chdir(workspace)
  require("forge").setup({ harness_logging = true, text_correction = { enabled = false }, harness = {
    provider = options.provider, model = "gpt-6-luna", effort = "medium", config_dir = config,
    providers = { [options.provider] = { command = options.command, selectable = true } },
  } })
  require("forge").open_harness()
  local deadline = now() + 120000
  local function ready()
    if session.harness.host_error then phase = "startup_failed" save("manifest.json", M.info()) return end
    if session.harness.ready and session.harness.session then
      request("host.trace.configure", { enabled = true })
      request("session.state.get", {}, nil, function(record)
        local selected = record.result and record.result.session
        local correct = selected and (selected.resolved_model or selected.model) == "gpt-6-luna" and selected.effort == "medium"
        phase = record.error and "startup_failed" or correct and "ready" or "selection_mismatch"
        M.capture("startup")
        print("ProviderApiLive " .. options.provider .. ": " .. phase .. " (details in ProviderApiLive.info())")
      end)
      return
    end
    if now() >= deadline then phase = "startup_observation_timeout" M.capture("startup-timeout") return end
    vim.defer_fn(ready, 50)
  end
  ready()
end

local git_steps = {
  { "git", "init", "--quiet", workspace },
  { "git", "-C", workspace, "add", "source.txt", ".forge.json" },
  { "git", "-C", workspace, "-c", "core.hooksPath=" .. hooks, "-c", "user.name=Forge live fixture", "-c", "user.email=fixture@example.invalid", "-c", "commit.gpgsign=false", "commit", "--quiet", "-m", "Create isolated verification baseline" },
}
vim.fn.writefile({ "seed line" }, vim.fs.joinpath(workspace, "source.txt"))
vim.fn.writefile({ '{"checks":{}}' }, vim.fs.joinpath(workspace, ".forge.json"))
for path, contents in pairs(options.workspace_files or {}) do
  assert(type(path) == "string" and type(contents) == "string", "workspace fixtures require string paths and contents")
  local relative = path:gsub("\\", "/")
  assert(relative ~= "" and not relative:match("^/") and not relative:find(":", 1, true), "fixture paths must be relative")
  for component in relative:gmatch("[^/]+") do
    assert(component ~= "." and component ~= "..", "fixture paths must contain only normal components")
  end
  local target = vim.fs.joinpath(workspace, relative)
  vim.fn.mkdir(vim.fs.dirname(target), "p")
  assert(vim.fn.writefile(vim.split(contents, "\n", { plain = true }), target) == 0, "cannot create workspace fixture: " .. relative)
end
local function prepare(index)
  if index > #git_steps then start_harness() return end
  vim.system(git_steps[index], { text = true }, vim.schedule_wrap(function(result)
    append("preparation.jsonl", { command = git_steps[index], code = result.code, stdout = result.stdout, stderr = result.stderr })
    if result.code ~= 0 then phase = "preparation_failed" save("manifest.json", M.info()) vim.notify(result.stderr or "Git fixture preparation failed", vim.log.levels.ERROR) return end
    prepare(index + 1)
  end))
end
save("manifest.json", M.info())
print("ProviderApiLive " .. options.provider .. ": preparing workspace")
prepare(1)
