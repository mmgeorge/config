---@class ForgeAICommitBackend
---@field request_async? fun(params: table, callback: fun(result: table?, failure: string?)): fun()?
---@class ForgeAICommitState
---@field state "none"|"generating"|"ready"|"error"
---@field cwd? string
---@field ref? string
---@field message? string
---@field error? string
---@field model? string
---@field cancel? fun()
---@field waiters fun(result: ForgeAICommitState)[]

---@class ForgeAICommitModule
---@field _backend ForgeAICommitBackend?
---@field _state ForgeAICommitState?
---@field _states table<string, ForgeAICommitState>
---@field _request_ids table<string, integer>
---@field _buffer_generation table<integer, fun(supersede?: boolean)>

local M = { _request_ids = {}, _states = {}, _buffer_generation = {} }
local notifications = require("forge.infra.notifications")

---@param backend ForgeAICommitBackend?
function M.set_backend(backend) M._backend = backend end
function M.reset_backend()
  for _, cancel in pairs(M._buffer_generation) do cancel() end
  local previous = M._states
  M._backend, M._state = nil, nil
  M._states, M._request_ids = {}, {}
  for _, state in pairs(previous) do
    if state.cancel then state.cancel() end
  end
end

local function text_to_lines(text)
  return vim.split(tostring(text or ""):gsub("\r\n", "\n"), "\n", { plain = true })
end

local function native_request(cwd, ref, ignored, callback, is_current, on_model)
  local lifetime = M._states
  local cancelled, cancel_request = false, nil
  local function receive(result, failure)
    if cancelled or M._states ~= lifetime or (is_current and not is_current()) then return end
    if type(result) == "table" and result.state == "retry" then
      vim.defer_fn(function()
        if not cancelled and M._states == lifetime and (not is_current or is_current()) then
          local ok, failure = pcall(function()
            cancel_request = native_request(cwd, ref, ignored, callback, is_current, on_model)
          end)
          if not ok then callback(nil, tostring(failure)) end
        end
      end, 120)
      return
    end
    callback(result, failure)
  end
  local function resolved(snapshot, failure)
    if cancelled or M._states ~= lifetime or (is_current and not is_current()) then return end
    if not snapshot then callback(nil, failure) return end
    local client = require("forge.client")
    local generation, configuration_failure = client.generation_get_config()
    if not generation then callback(nil, configuration_failure) return end
    local choice = require("forge.integrations.background_settings").model(snapshot, generation.backend.kind)
    local function publish_model(label)
      local speed = choice.service_tier
      on_model((label:match("[^\r\n]*") or label) .. (speed and speed ~= "default" and (" · " .. speed) or ""))
    end
    publish_model(choice.model .. (choice.thinking ~= nil and (" · thinking " .. choice.thinking) or ""))
    local params = { operation = "generate", workspace = cwd,
      comparison = ref == "staged" and "staged" or ref == "latest_commit" and "latest_commit" or "status",
      ignored_paths = ignored or {} }
    if M._backend and M._backend.request_async then cancel_request = M._backend.request_async(params, receive)
    else
      cancel_request = client.generation_request(params, receive, publish_model, { model = choice.model,
        reasoning_effort = choice.thinking, service_tier = choice.service_tier,
        auto_compaction = snapshot.settings.auto_compaction })
    end
  end
  require("forge.integrations.background_settings").request(nil, nil, function(...)
    local ok, failure = pcall(resolved, ...)
    if not ok then callback(nil, tostring(failure)) end
  end)
  return function()
    if cancelled then return end
    cancelled = true
    if cancel_request then cancel_request() end
  end
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
      if #current.waiters >= 64 then
        local failure = "Commit generation waiter admission is full"
        notifications.error(failure, "Forge commit")
        cb({ state = "error", error = failure, waiters = {} })
        return
      end
      current.waiters[#current.waiters + 1] = cb
    elseif cb then cb(current) end
    return
  end
  local waiters = current and current.state == "generating" and current.waiters or {}
  if cb then waiters[#waiters + 1] = cb end
  local key = state_key(cwd, ref)
  M._request_ids[key] = (M._request_ids[key] or 0) + 1
  local request_id = M._request_ids[key]
  local state = { state = "generating", cwd = cwd, ref = ref, waiters = waiters,
    message = current and current.message, model = "resolving model" }
  if not set_state(cwd, ref, state) then
    M._request_ids[key] = nil
    notifications.error(state.error, "Forge commit")
    notify_waiters(state)
    return
  end
  if current and current.cancel then current.cancel() end
  if opts.on_start then opts.on_start(state) end
  local function complete(result, failure)
    if M._request_ids[key] ~= request_id or get_state(cwd, ref) ~= state or state.state ~= "generating" then return end
    if type(result) ~= "table" then result = nil end
    if not failure and result and result.state == "ready"
      and (type(result.message) ~= "string" or vim.trim(result.message) == "") then
      failure = "Commit generation returned no message"
    elseif not failure and (not result or (result.state ~= "ready" and result.state ~= "none")) then
      failure = result and result.error or "Commit generation returned an invalid response"
    end
    if failure then failure = tostring(failure) end
    state.state = failure and "error" or result.state
    state.error = failure
    if state.state == "ready" then state.message = result.message
    elseif state.state == "none" then state.message = nil end
    if failure then notifications.error("Commit generation failed: " .. failure, "Forge commit") end
    notify_waiters(state)
  end
  local ok, cancel_or_failure = pcall(native_request, cwd, ref, opts.ignored_paths, complete,
    function() return M._request_ids[key] == request_id and get_state(cwd, ref) == state and state.state == "generating" end,
    function(model) state.model = model end)
  if ok then state.cancel = cancel_or_failure else complete(nil, tostring(cancel_or_failure)) end
end

---@param cwd? string
---@param ref? string
---@return ForgeAICommitState?
function M.state(cwd, ref)
  if cwd and ref then return get_state(cwd, ref) end
  return M._state
end

---@param cwd string
---@param ref? string
function M.clear(cwd, ref)
  ref = ref or "HEAD"
  local key = state_key(cwd, ref)
  local current = get_state(cwd, ref)
  M._request_ids[key] = nil
  M._states[key] = nil
  if ref == "HEAD" and M._state == current then M._state = nil end
  if current and current.state == "generating" then
    if current.cancel then current.cancel() end
    current.state, current.message = "none", nil
    notify_waiters(current)
  end
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
---@param supersede? boolean Cancels the request supplying this editor before regeneration.
function M.cancel_buffer_generation(buf, supersede)
  local cancel = M._buffer_generation[buf]
  if cancel then cancel(supersede) end
  if vim.api.nvim_buf_is_valid(buf) then
    vim.b[buf].forge_ai_commit_revision = (vim.b[buf].forge_ai_commit_revision or 0) + 1
  end
end

---@param buf integer
---@param cwd string
---@param regenerate? boolean
---@param comparison? "latest_commit"
function M.populate_commit_buffer_when_ready(buf, cwd, regenerate, comparison)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if not regenerate and vim.b[buf].forge_ai_commit_populate_started then return end
  vim.b[buf].forge_ai_commit_populate_started = true
  vim.b[buf].ai_commit_generated = true
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  if not regenerate and lines[1] and lines[1] ~= "" then return end
  M.cancel_buffer_generation(buf, regenerate == true)
  if regenerate and not comparison then
    local about = get_state(cwd, "HEAD")
    if about and about.state == "generating" then M.clear(cwd, "HEAD") end
  end
  lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
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
  local abandoned, displayed, autocmd, replaced = false, nil, nil, false
  local stop_progress = function() end
  local view = { buffer = buf, preserve_view = true, sequence = { node = {}, locate = function() end } }
  local function replace(first, last, replacement)
    local position = require("forge.buffer_view")
    local retained = position.capture(view, function() return false end)
    local was_modifiable = vim.bo[buf].modifiable
    vim.bo[buf].modifiable = true
    if replaced and vim.api.nvim_buf_get_changedtick(buf) == changedtick then
      vim.api.nvim_buf_call(buf, function() vim.cmd("silent! undojoin") end)
    end
    vim.api.nvim_buf_set_lines(buf, first, last, false, replacement)
    replaced = true
    vim.bo[buf].modifiable = was_modifiable
    view.row_count = vim.api.nvim_buf_line_count(buf)
    position.restore(view, retained)
    changedtick = vim.api.nvim_buf_get_changedtick(buf)
  end
  local function cleanup()
    stop_progress()
    if autocmd then pcall(vim.api.nvim_del_autocmd, autocmd) autocmd = nil end
    M._buffer_generation[buf] = nil
  end
  local function restore_subject()
    if displayed and vim.api.nvim_buf_is_valid(buf)
      and vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1] == displayed then
      replace(0, 1, { lines[1] or "" })
    end
  end
  local function cancel(supersede)
    if abandoned then return end
    abandoned = true
    cleanup()
    restore_subject()
    if supersede then M.clear(cwd, comparison or (regenerate and "staged" or "HEAD")) end
  end
  local function current()
    return not abandoned and vim.api.nvim_buf_is_valid(buf)
      and vim.b[buf].forge_ai_commit_revision == revision
      and vim.api.nvim_buf_get_changedtick(buf) == changedtick
  end
  local function apply_message(state)
    if not current() then cancel() return end
    cleanup()
    if state.state ~= "ready" or not state.message then restore_subject() return end
    local message_lines = text_to_lines(state.message)
    if regenerate and replace_end < #lines then message_lines[#message_lines + 1] = "" end
    replace(0, replace_end == 0 and displayed and 1 or replace_end, message_lines)
    vim.b[buf].forge_ai_commit_populated = true
  end
  local ref = comparison or (regenerate and "staged" or "HEAD")
  if not regenerate and not get_state(cwd, ref) then return end
  M._buffer_generation[buf] = cancel
  autocmd = vim.api.nvim_create_autocmd({ "BufWritePre", "BufWipeout" }, { buffer = buf, once = true, callback = cancel })
  M.ensure(cwd, { ref = ref, force = regenerate == true, on_start = function(state)
    stop_progress = require("forge.render.generation_progress").watch(state, function()
      if current() then return true end
      cancel()
      return false
    end, function(text)
      displayed = text
      replace(0, 1, { text })
    end)
  end }, apply_message)
end

return M
