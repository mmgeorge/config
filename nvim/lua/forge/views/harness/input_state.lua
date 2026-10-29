local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local sequence = 0

---@class ForgeInputOperation
---@field id string
---@field action table
---@field callback? fun(failure: string?)
---@field sent? boolean

---@class ForgeInputViewState
---@field revision integer
---@field confirmed table[]
---@field pending ForgeInputOperation[]
---@field entry_by_id table<string, table>
---@field direct_submission table<string, boolean>
---@field sending? ForgeInputOperation
---@field refreshing? boolean
---@field suspended boolean
---@field pause_operation_id? string
---@field changed fun()

local function identity()
  sequence = sequence + 1
  return ("%s:%s:%s"):format(vim.fn.getpid(), vim.uv.hrtime(), sequence)
end

local function index_of(entries, id)
  for index, entry in ipairs(entries) do if entry.id == id then return index end end
end

local function apply(entries, action)
  if action.kind == "submit" then
    if not index_of(entries, action.message.id) then entries[#entries + 1] = vim.deepcopy(action.message) end
  elseif action.kind == "remove" then
    local index = index_of(entries, action.id)
    if index then table.remove(entries, index) end
  elseif action.kind == "reserve" then
    local index = index_of(entries, action.id)
    if index then entries[index].reservation = action.reservation end
  elseif action.kind == "release" then
    for _, entry in ipairs(entries) do if entry.reservation == action.reservation then entry.reservation = nil end end
  elseif action.kind == "commit" then
    for _, edit in ipairs(action.edits) do
      local index = index_of(entries, edit.id)
      if index then
        if not edit.text:find("%S") and not edit.config then table.remove(entries, index)
        else
          entries[index].text, entries[index].attachments = edit.text, vim.deepcopy(edit.attachments)
          if edit.config then entries[index].config = vim.deepcopy(edit.config) end
          entries[index].reservation = nil
        end
      end
    end
    for _, entry in ipairs(entries) do if entry.reservation == action.reservation then entry.reservation = nil end end
  end
end

local function render(state)
  local input = state.input
  local entries = vim.deepcopy(input.confirmed)
  for _, operation in ipairs(input.pending) do apply(entries, operation.action) end
  for _, message in ipairs(input.rejected) do
    if not index_of(entries, message.id) then entries[#entries + 1] = vim.deepcopy(message) end
  end
  local queue, steering, submitting = {}, {}, {}
  local retained = {}
  local present = {}
  for _, entry in ipairs(entries) do
    present[entry.id] = true
    local immediate = entry.intent == "auto" and (entry.config == nil or entry.config == vim.NIL)
      and entry.delivery ~= "failed" and entry.delivery ~= "unknown"
    if entry.delivery == "accepted" then input.accepted[entry.id] = true end
    if entry.delivery ~= "accepted" and not input.accepted[entry.id] then
      local target = input.entry_by_id[entry.id] or {}
      input.entry_by_id[entry.id] = target
      for key in pairs(target) do target[key] = nil end
      for key, value in pairs(entry) do if value ~= vim.NIL then target[key] = value end end
      local direct = entry.route == "turn" or (entry.route == nil or entry.route == vim.NIL) and input.direct_submission[entry.id]
      local destination = immediate and (direct and submitting or steering) or queue
      destination[#destination + 1] = target
      retained[entry.id] = true
    end
  end
  for id in pairs(input.direct_submission) do
    if not present[id] then input.direct_submission[id] = nil end
  end
  for id, entry in pairs(input.entry_by_id) do
    if not retained[id] and not (state.queue_edit and state.queue_edit.drafts[entry]) then input.entry_by_id[id] = nil end
  end
  state.queue, state.pending_steer, state.pending_submit = queue, steering, submitting
  input.changed()
end

local send_next

---@param state table
---@param update table
function M.receive(state, update)
  local input = state.input
  if not input then return end
  local current = update.revision >= input.revision
  local recovery = false
  if type(update.snapshot) == "table" and current
    and update.revision >= (input.required_revision or 0) then
    input.confirmed = vim.deepcopy(update.snapshot.messages)
    input.revision = update.snapshot.revision
    input.suspended = update.snapshot.suspended
    input.control_revision = update.snapshot.control_revision or input.control_revision
    input.required_revision = nil
  elseif input.required_revision or (update.revision > input.revision and update.revision ~= input.revision + 1) then
    input.required_revision = math.max(input.required_revision or 0, update.revision)
    recovery, current = true, false
  end
  if current and type(update.execution_busy) == "boolean" then
    state.execution_busy = update.execution_busy
    state.busy = state.request_busy == true or update.execution_busy
  end
  if current and update.revision > input.revision then
    for _, id in ipairs(update.removed or {}) do
      local index = index_of(input.confirmed, id)
      if index then table.remove(input.confirmed, index) end
      input.retraction[id] = nil
    end
    for _, message in ipairs(update.upsert or {}) do
      local index = index_of(input.confirmed, message.id)
      if index then input.confirmed[index] = vim.deepcopy(message)
      elseif update.prepend then table.insert(input.confirmed, 1, vim.deepcopy(message))
      else input.confirmed[#input.confirmed + 1] = vim.deepcopy(message) end
    end
    input.revision, input.suspended = update.revision, update.suspended
    input.control_revision = update.control_revision or input.control_revision
  end
  local resolved
  local failure = type(update.error) == "string" and update.error or nil
  for index, operation in ipairs(input.pending) do
    if operation.id == update.operation_id then
      resolved = operation
      if failure and operation.action.kind == "submit" then
        local message = vim.deepcopy(operation.action.message)
        message.delivery, message.error, message.local_only = "failed", failure, true
        input.rejected[#input.rejected + 1] = message
      end
      table.remove(input.pending, index)
      break
    end
  end
  if input.sending == resolved then input.sending = nil end
  render(state)
  if recovery then M.refresh(state) end
  if type(update.retracted) == "string" then
    local restore = input.retraction[update.retracted]
    input.retraction[update.retracted] = nil
    if restore and restore() then M.perform(state, { kind = "remove", id = update.retracted }) end
  end
  if resolved then
    if failure then notifications.error(failure, "Harness message") end
    if resolved.callback then resolved.callback(failure) end
  end
  send_next(state)
end

send_next = function(state)
  local input = state.input
  if not input or input.sending or input.refreshing or input.required_revision or state.host_error then return end
  local operation = input.pending[1]
  if not operation then return end
  input.sending = operation
  if not operation.sent then
    local function revision(id)
      local index = index_of(input.confirmed, id)
      return index and input.confirmed[index].revision or 0
    end
    if operation.action.kind == "reserve" or operation.action.kind == "remove" or operation.action.kind == "retry" then
      operation.action.revision = revision(operation.action.id)
    elseif operation.action.kind == "commit" then
      for _, edit in ipairs(operation.action.edits) do edit.revision = revision(edit.id) end
    end
    operation.sent = true
  end
  local session_id = state.session.id
  local sent, send_failure = pcall(client.request_for, session_id, "input.apply", { id = operation.id, action = operation.action,
    control_revision = operation.control_revision, after_control = operation.after_control }, function(result, failure, detail)
    if state.input ~= input or state.session.id ~= session_id then return end
    if not failure and result then M.receive(state, result) return end
    if input.sending ~= operation then return end
    if detail and detail.code == "not_admitted" then
      vim.defer_fn(function()
        if state.input ~= input or state.host_error or input.sending ~= operation then return end
        input.sending = nil
        send_next(state)
      end, 500)
      return
    end
    if detail and detail.code ~= "not_admitted" and detail.code ~= "outcome_unknown"
      and detail.code ~= "result_encoding_failed" then
      M.receive(state, { operation_id = operation.id, revision = input.revision,
        suspended = input.suspended, upsert = {}, removed = {}, error = failure })
      return
    end
    -- Retrying the same operation cannot duplicate a delivery already owned by Rust.
    notifications.error(failure or "Input acknowledgement is missing", "Harness message")
    vim.defer_fn(function()
      if state.input ~= input or state.host_error or input.sending ~= operation then return end
      client.request_for(session_id, "input.operation", { id = operation.id }, function(outcome, lookup_failure)
        if state.input ~= input or input.sending ~= operation then return end
        input.sending = nil
        if lookup_failure then notifications.error(lookup_failure, "Harness message recovery") return end
        if type(outcome) == "table" then M.receive(state, outcome)
        else send_next(state) end
      end)
    end, 500)
  end)
  if not sent then
    if input.sending ~= operation then error(send_failure, 0) end
    input.sending = nil
    notifications.error("Input remains pending: " .. tostring(send_failure), "Harness message")
  end
end

---@param state table
---@param changed fun()
function M.attach(state, changed)
  if not state.session then return end
  if state.input and state.input.session_id == state.session.id then
    state.input.changed = changed
    M.refresh(state)
    return
  end
  state.input = { session_id = state.session.id, runtime_epoch = state.runtime_epoch,
    revision = 0, control_revision = 0, confirmed = {}, pending = {}, rejected = {},
    entry_by_id = {}, direct_submission = {}, accepted = {}, retraction = {}, suspended = false, changed = changed }
  M.refresh(state)
end

---@param state table
function M.refresh(state)
  local input = state.input
  if not input then return end
  if input.runtime_epoch ~= state.runtime_epoch then
    local changed = input.changed
    local recovered = vim.deepcopy(input.confirmed)
    for _, operation in ipairs(input.pending) do apply(recovered, operation.action) end
    for _, entry in ipairs(input.rejected) do
      if not index_of(recovered, entry.id) then recovered[#recovered + 1] = vim.deepcopy(entry) end
    end
    local rejected = {}
    for _, entry in ipairs(recovered) do
      if entry.delivery ~= "accepted" then
        entry.delivery, entry.local_only = "unknown", true
        entry.error = "Host restarted. Delivery must be confirmed before resending."
        rejected[#rejected + 1] = entry
      end
    end
    if state.queue_edit then
      state.failed_queue_edit = state.queue_edit
      state.failed_queue_edit.failure = "The host restarted. Unqueue this draft to retain it separately."
      state.queue_edit = nil
    end
    state.input = nil
    M.attach(state, changed)
    state.input.rejected = rejected
    render(state)
    return
  end
  if input.refreshing then return end
  input.refreshing = true
  local session_id = state.session.id
  client.request_for(session_id, "input.get", {}, function(result, failure)
    if state.input ~= input or state.session.id ~= session_id then return end
    input.refreshing = false
    if failure or not result then
      notifications.error(failure or "Input state is missing", "Harness messages")
      if input.required_revision then
        vim.defer_fn(function() if state.input == input then M.refresh(state) end end, 500)
      end
      return
    end
    if result.revision < (input.required_revision or 0) then
      vim.schedule(function() if state.input == input then M.refresh(state) end end)
      return
    end
    if result.revision >= input.revision then
      input.required_revision = nil
      input.confirmed, input.revision, input.suspended = result.messages, result.revision, result.suspended
      input.control_revision = result.control_revision or input.control_revision
      state.execution_busy = result.execution_busy
      state.busy = state.request_busy == true or result.execution_busy == true
      render(state)
    end
    send_next(state)
  end)
end

---@param state table
---@param action table
---@param callback? fun(failure: string?)
function M.perform(state, action, callback)
  local operation = { id = identity(), action = vim.deepcopy(action), callback = callback,
    control_revision = state.input.control_revision, after_control = state.input.pause_operation_id }
  state.input.pending[#state.input.pending + 1] = operation
  render(state)
  send_next(state)
end

---@param state table
---@param id string
function M.discard_rejected(state, id)
  local index = index_of(state.input.rejected, id)
  if index then table.remove(state.input.rejected, index) end
  render(state)
end

---@param state table
---@param entry table
function M.retry(state, entry)
  if entry.local_only then
    if entry.delivery == "unknown" then
      notifications.error(entry.error, "Harness message recovery")
      return
    end
    M.discard_rejected(state, entry.id)
    local message = vim.deepcopy(entry)
    message.local_only, message.error, message.delivery = nil, nil, "queued"
    M.perform(state, { kind = "submit", message = message })
  else
    M.perform(state, { kind = "retry", id = entry.id, revision = entry.revision })
  end
end

---@param state table
---@param text string
---@param attachments table[]
---@param intent 'auto'|'queue'
---@param config? table
---@param callback? fun(failure: string?)
---@param retracted? fun(): boolean
function M.submit(state, text, attachments, intent, config, callback, retracted)
  local message = { id = identity(), revision = 0, text = text, attachments = attachments,
    purpose = { kind = "user" }, config = config, intent = intent, delivery = "queued" }
  if intent == "auto" and not config and not state.busy and not state.execution_busy
    and #(state.queue or {}) == 0 and #(state.pending_steer or {}) == 0 and #(state.pending_submit or {}) == 0 then
    state.input.direct_submission[message.id] = true
  end
  if retracted then state.input.retraction[message.id] = retracted end
  M.perform(state, { kind = "submit", message = message }, callback)
  return message.id
end

---@return string
function M.reservation()
  return identity()
end

return M
