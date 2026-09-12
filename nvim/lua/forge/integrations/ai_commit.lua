---@class ForgeAICommitBackend
---@field request_async? fun(params: table, callback: fun(result: table?, failure: string?))
---@class ForgeAICommitState
---@field state "none"|"generating"|"ready"|"error"
---@field cwd? string
---@field ref? string
---@field message? string
---@field error? string
---@field waiters fun(result: ForgeAICommitState)[]

---@class ForgeAICommitModule
---@field _backend ForgeAICommitBackend?
---@field _state ForgeAICommitState?
---@field _states table<string, ForgeAICommitState>
---@field _request_ids table<string, integer>

local M = { _request_ids = {}, _states = {} }
local notifications = require("forge.infra.notifications")

---@param backend ForgeAICommitBackend?
function M.set_backend(backend) M._backend = backend end
function M.reset_backend()
  M._backend, M._state = nil, nil
  M._states, M._request_ids = {}, {}
end

local function text_to_lines(text)
  return vim.split(tostring(text or ""):gsub("\r\n", "\n"), "\n", { plain = true })
end

local function native_request(cwd, ref, ignored, callback, is_current)
  local resolved, failure = require("ai").resolve(require("ai.adapters").get().commit)
  if not resolved then callback(nil, failure) return end
  local model = { provider = resolved.provider.name, model = resolved.model, thinking = resolved.thinking }
  local params = { operation = "generate", workspace = cwd, comparison = ref == "staged" and "staged" or "head", ignored_paths = ignored or {}, model = model }
  local lifetime = M._states
  local function receive(result, failure)
    if M._states ~= lifetime or (is_current and not is_current()) then return end
    if result and result.state == "retry" then
      vim.defer_fn(function()
        if M._states == lifetime and (not is_current or is_current()) then
          native_request(cwd, ref, ignored, callback, is_current)
        end
      end, 120)
      return
    end
    callback(result, failure)
  end
  if M._backend and M._backend.request_async then M._backend.request_async(params, receive)
  else require("forge.client").request_host("repository.generate", params, receive) end
end

---@param state ForgeAICommitState
local function notify_waiters(state)
  local waiters = state.waiters or {}
  state.waiters = {}
  for _, waiter in ipairs(waiters) do
    local ok, failure = pcall(waiter, state)
    if not ok then notifications.error("Commit generation callback failed: " .. tostring(failure)) end
  end
end

---@param cwd string
---@param ref string
---@return string
local function state_key(cwd, ref)
  return cwd .. "\0" .. ref
end

---@param cwd string
---@param ref string
---@param state ForgeAICommitState
---@return boolean
local function set_state(cwd, ref, state)
  M._states = M._states or {}
  local key = state_key(cwd, ref)
  if not M._states[key] and vim.tbl_count(M._states) >= 64 then
    local evicted = false
    for previous_key, previous in pairs(M._states) do
      if previous.state ~= "generating" then
        M._states[previous_key] = nil
        M._request_ids[previous_key] = nil
        evicted = true
        break
      end
    end
    if not evicted then
      state.state, state.error = "error", "Commit generation state admission is full"
      return false
    end
  end
  M._states[key] = state
  if ref == "HEAD" then
    M._state = state
  end
  return true
end

---@param cwd string
---@param ref string
---@return ForgeAICommitState?
local function get_state(cwd, ref)
  local states = M._states or {}
  return states[state_key(cwd, ref)]
end

---@param cwd string
---@param opts? { force?: boolean, ref?: string, ignored_paths?: string[], on_start?: fun(state: ForgeAICommitState) }
---@param cb? fun(state: ForgeAICommitState)
function M.ensure(cwd, opts, cb)
  opts = opts or {}
  local ref = opts.ref or "HEAD"
  if cwd == nil or cwd == "" then
    local state = { state = "none", ref = ref, waiters = {} }
    if cb then cb(state) end
    return
  end

  local current = get_state(cwd, ref)
  if not opts.force and current then
    if opts.on_start and current.state == "generating" then opts.on_start(current) end
    if current.state == "generating" and cb then
      if #current.waiters >= 64 then cb({ state = "error", error = "Commit generation waiter admission is full", waiters = {} }) return end
      current.waiters[#current.waiters + 1] = cb
    elseif cb then cb(current) end
    return
  end
  local waiters = current and current.state == "generating" and current.waiters or {}
  if cb then waiters[#waiters + 1] = cb end
  local key = state_key(cwd, ref)
  M._request_ids[key] = (M._request_ids[key] or 0) + 1
  local request_id = M._request_ids[key]
  local state = { state = "generating", cwd = cwd, ref = ref, waiters = waiters }
  if not set_state(cwd, ref, state) then M._request_ids[key] = nil notify_waiters(state) return end
  if opts.on_start then opts.on_start(state) end
  native_request(cwd, ref, opts.ignored_paths, function(result, failure)
    if M._request_ids[key] ~= request_id or get_state(cwd, ref) ~= state then return end
    state.state = result and result.state or "error"
    state.message = result and result.message
    state.error = failure
    notify_waiters(state)
  end, function() return M._request_ids[key] == request_id and get_state(cwd, ref) == state end)
end

---@param cwd? string
---@param ref? string
---@return ForgeAICommitState?
function M.state(cwd, ref)
  if cwd and ref then return get_state(cwd, ref) end
  return M._state
end

---@param message string?
---@return string
function M.subject(message)
  for _, line in ipairs(text_to_lines(message or "")) do
    local trimmed = vim.trim(line)
    if trimmed ~= "" then return trimmed end
  end
  return ""
end

---@param buf integer
---@param cwd string
---@param notify? fun(message: string, level: integer)
---@param regenerate? boolean
function M.populate_commit_buffer_when_ready(buf, cwd, notify, regenerate)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if not regenerate and vim.b[buf].forge_ai_commit_populate_started then return end
  vim.b[buf].forge_ai_commit_populate_started = true
  vim.b[buf].ai_commit_generated = true
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  if not regenerate and lines[1] and lines[1] ~= "" then return end
  local revision = (vim.b[buf].forge_ai_commit_revision or 0) + 1
  vim.b[buf].forge_ai_commit_revision = revision
  local changedtick = vim.api.nvim_buf_get_changedtick(buf)
  local replace_end = 0
  if regenerate then
    replace_end = #lines
    for position, line in ipairs(lines) do
      if line:match("^#") then replace_end = position - 1 break end
    end
  end
  local function apply_message(state)
    if not vim.api.nvim_buf_is_valid(buf) or vim.b[buf].forge_ai_commit_revision ~= revision then return end
    if vim.api.nvim_buf_get_changedtick(buf) ~= changedtick then return end
    if state.state ~= "ready" or not state.message then
      if notify then
        if state.state == "error" then notify(state.error or "Unable to generate commit message", vim.log.levels.WARN)
        elseif regenerate and state.state == "none" then notify("No staged changes to describe", vim.log.levels.INFO) end
      end
      return
    end
    local message_lines = text_to_lines(state.message)
    if regenerate and replace_end < #lines then message_lines[#message_lines + 1] = "" end
    local was_modifiable = vim.bo[buf].modifiable
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, replace_end, false, message_lines)
    vim.bo[buf].modifiable = was_modifiable
    local window = vim.fn.bufwinid(buf)
    if window ~= -1 then vim.api.nvim_win_set_cursor(window, { 1, 0 }) end
    vim.b[buf].forge_ai_commit_populated = true
  end
  local ref = regenerate and "staged" or "HEAD"
  if not regenerate and not get_state(cwd, ref) then return end
  M.ensure(cwd, { ref = ref, force = regenerate == true,
    on_start = function()
      if notify then notify(regenerate and "Generating staged commit message..." or "Waiting for About draft...", vim.log.levels.INFO) end
    end,
  }, apply_message)
end

return M
