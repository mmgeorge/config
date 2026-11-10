local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local command = require("forge.command")

---@class ForgeInputViewState
---@field session_id string
---@field runtime_epoch? string
---@field host_generation integer
---@field revision integer
---@field confirmed table[] Committed host input; never optimistically edited.
---@field drafts table[] Unacknowledged or rejected editor drafts, never eligible for host delivery.
---@field entry_by_id table<string, table> Stable display objects for queue editing.
---@field retraction table<string, fun(): boolean>
---@field committing table<string, integer> In-flight edit commits per message id.
---@field removing table<string, integer> In-flight edit commits that remove a message.
---@field commit_waiters table<string, fun()[]> Reservations waiting for a message's commit result.
---@field refreshing? boolean
---@field required_revision? integer
---@field suspended boolean
---@field control_revision integer
---@field pause_operation_id? string
---@field changed fun()

local function index_of(entries, id)
  for index, entry in ipairs(entries) do if entry.id == id then return index end end
end

local function owns_input(state, input)
  return state.input == input and not state.switching_provider and not state.host_error
    and state.session and state.session.id == input.session_id and state.runtime_epoch == input.runtime_epoch
    and client.host_get_generation() == input.host_generation
end

-- Preserve display identity for editor-owned drafts without mutating the host projection.
local function display_entry(input, entry)
  local target = input.entry_by_id[entry.id] or {}
  input.entry_by_id[entry.id] = target
  for key in pairs(target) do target[key] = nil end
  for key, value in pairs(entry) do if value ~= vim.NIL then target[key] = value end end
  return target
end

local function render(state)
  local input = state.input
  local queue, steering, submitting, retained, known = {}, {}, {}, {}, {}
  for _, entry in ipairs(input.confirmed) do
    known[entry.id] = true
    if entry.delivery ~= "accepted" then
      local destination = queue
      if entry.delivery == "delivering" then
        destination = entry.route == "steering" and steering or submitting
      end
      destination[#destination + 1] = display_entry(input, entry)
      retained[entry.id] = true
    end
  end
  for _, draft in ipairs(input.drafts) do
    if not known[draft.id] then
      local destination = draft.draft_state == "pending" and draft.intent ~= "queue" and submitting or queue
      destination[#destination + 1] = display_entry(input, draft)
      retained[draft.id] = true
    end
  end
  for id, entry in pairs(input.entry_by_id) do
    if not retained[id] and not (state.queue_edit and state.queue_edit.drafts[entry]) then
      input.entry_by_id[id] = nil
    end
  end
  state.queue, state.pending_steer, state.pending_submit = queue, steering, submitting
  input.changed()
end

local function discard_draft(input, id)
  local index = index_of(input.drafts, id)
  if index then table.remove(input.drafts, index) end
end

---Apply ordered host input observations; only a missing baseline requires a snapshot.
---@param state table
---@param update table
function M.receive(state, update)
  local input = state.input
  if not input or type(update.revision) ~= "number" then return end
  -- Retraction acknowledges an editor-owned callback even if a snapshot already superseded its delta.
  local restore = type(update.retracted) == "string" and input.retraction[update.retracted] or nil
  if restore then
    input.retraction[update.retracted] = nil
    restore()
  end
  if update.revision < input.revision then return end
  if type(update.snapshot) == "table" and update.snapshot.revision >= (input.required_revision or input.revision) then
    input.confirmed = vim.deepcopy(update.snapshot.messages)
    input.revision = update.snapshot.revision
    input.suspended = update.snapshot.suspended
    input.control_revision = update.snapshot.control_revision or input.control_revision
    input.required_revision = nil
    for _, entry in ipairs(input.confirmed) do discard_draft(input, entry.id) end
  elseif update.revision > input.revision then
    if input.required_revision or update.revision ~= input.revision + 1 then
      input.required_revision = math.max(input.required_revision or 0, update.revision)
      M.refresh(state)
      return
    end
    for _, id in ipairs(update.removed or {}) do
      local index = index_of(input.confirmed, id)
      if index then table.remove(input.confirmed, index) end
      discard_draft(input, id)
    end
    for _, message in ipairs(update.upsert or {}) do
      local index = index_of(input.confirmed, message.id)
      if index then input.confirmed[index] = vim.deepcopy(message)
      elseif update.prepend then table.insert(input.confirmed, 1, vim.deepcopy(message))
      else input.confirmed[#input.confirmed + 1] = vim.deepcopy(message) end
      discard_draft(input, message.id)
      -- A claimed or accepted submission stays retractable until its exchange proves output.
      -- Only the newest submission can be retracted, so a newer acceptance retires older callbacks.
      if message.delivery == "accepted" then
        for id in pairs(input.retraction) do
          if id ~= message.id then input.retraction[id] = nil end
        end
      end
    end
    input.revision, input.suspended = update.revision, update.suspended
    input.control_revision = update.control_revision or input.control_revision
  end
  render(state)
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
  state.input = {
    session_id = state.session.id, runtime_epoch = state.runtime_epoch,
    host_generation = client.host_get_generation(), revision = 0, control_revision = 0,
    confirmed = {}, drafts = {}, entry_by_id = {}, retraction = {}, suspended = false, changed = changed,
    committing = {}, removing = {}, commit_waiters = {},
  }
  M.refresh(state)
end

---Read a baseline for initial attachment, reconnect, or an observed stream gap.
---@param state table
function M.refresh(state)
  local input = state.input
  if not input or state.switching_provider or state.host_error then return end
  if input.runtime_epoch ~= state.runtime_epoch or input.host_generation ~= client.host_get_generation() then
    -- Keep editor content across host replacement, but never replay it automatically.
    local drafts = input.drafts
    for _, draft in ipairs(drafts) do
      draft.draft_state = "unknown"
      draft.error = "Host restarted. Confirm the original command before resending."
    end
    if state.queue_edit then
      state.failed_queue_edit = state.queue_edit
      state.failed_queue_edit.failure = "The host restarted. Unqueue this draft to retain it separately."
      state.queue_edit = nil
    end
    state.input = nil
    M.attach(state, input.changed)
    for _, draft in ipairs(drafts) do state.input.drafts[#state.input.drafts + 1] = draft end
    render(state)
    return
  end
  if input.refreshing then return end
  input.refreshing = true
  client.request({ session_id = input.session_id, method = "session.input.get", params = {} }, function(result, failure)
    if state.input ~= input then return end
    input.refreshing = false
    if not owns_input(state, input) then return end
    if failure or not result then
      notifications.error(failure or "Input state is missing", "Harness messages")
      return
    end
    if result.revision < (input.required_revision or input.revision) then return end
    M.receive(state, { revision = result.revision, snapshot = result })
  end)
end

---Submit one immutable command; shared command transport owns all observation and recovery.
---@param state table
---@param action table
---@param callback? fun(failure: string?)
function M.perform(state, action, callback, target)
  local input = state.input
  local id = command.identity()
  local draft
  if action.kind == "submit" then
    draft = vim.deepcopy(action.message)
    draft.local_only, draft.draft_state = true, "pending"
    draft.operation_id, draft.submission_target = id, vim.deepcopy(target)
    input.drafts[#input.drafts + 1] = draft
    render(state)
  end
  local completed = false
  local committing = {}
  local removing = {}
  for _, edit in ipairs(action.kind == "commit" and action.edits or {}) do
    committing[#committing + 1] = edit.id
    input.committing[edit.id] = (input.committing[edit.id] or 0) + 1
    if edit.text == "" and not edit.config then
      removing[#removing + 1] = edit.id
      input.removing[edit.id] = (input.removing[edit.id] or 0) + 1
    end
  end
  client.request({ session_id = input.session_id, method = "session.input.apply", params = {
    id = id, action = vim.deepcopy(action), control_revision = input.control_revision,
    after_control = input.pause_operation_id, target = vim.deepcopy(target),
  } }, function(result, failure, detail)
    if completed then return end
    completed = true
    local waiters = {}
    -- Release commit ownership only after the result updated the displayed revision, so waiters
    -- observe the revision the host published.
    local function settle_commits()
      for _, edit_id in ipairs(removing) do
        local remaining = input.removing[edit_id] - 1
        input.removing[edit_id] = remaining > 0 and remaining or nil
      end
      for _, edit_id in ipairs(committing) do
        local remaining = input.committing[edit_id] - 1
        input.committing[edit_id] = remaining > 0 and remaining or nil
        if not input.committing[edit_id] then
          for _, waiter in ipairs(input.commit_waiters[edit_id] or {}) do waiters[#waiters + 1] = waiter end
          input.commit_waiters[edit_id] = nil
        end
      end
      for _, waiter in ipairs(waiters) do waiter() end
    end
    if not owns_input(state, input) then settle_commits() return end
    local admitted = draft and not index_of(input.drafts, draft.id)
    if result then
      M.receive(state, result)
      failure = result.error or failure
    end
    if admitted then failure = nil end
    if draft then
      discard_draft(input, draft.id)
      if failure then
        draft.draft_state = detail and detail.code == "outcome_unknown" and "unknown" or "rejected"
        draft.error = failure
        input.drafts[#input.drafts + 1] = draft
      end
      render(state)
    end
    if failure then notifications.error(failure, "Harness message") end
    if failure and action.kind == "reserve" then
      require("forge.infra.perf").diagnostic("ui.queue.rejected", { reason = "reservation_failed",
        entry_id = action.id, operation_id = id, session_id = input.session_id,
        code = result and result.error_code or (detail and detail.code) })
    end
    if callback then callback(failure) end
    settle_commits()
  end)
end

---Run `continuation` once no edit commit for the message is awaiting its host result.
---A reservation must name the revision that result publishes, so it cannot read the displayed
---revision while a commit for the same message is still in flight.
---@param state table
---@param id string
---@param continuation fun()
function M.after_commit(state, id, continuation)
  local input = state.input
  if not input.committing[id] then continuation() return end
  local waiters = input.commit_waiters[id] or {}
  input.commit_waiters[id] = waiters
  waiters[#waiters + 1] = continuation
end

---Discard an editor-only recovery draft without sending a host input mutation.
function M.discard_rejected(state, id)
  discard_draft(state.input, id)
  render(state)
end

---Retry explicitly rejected content, keeping uncertain delivery blocked for inspection.
function M.retry(state, entry)
  if not entry.local_only then
    M.perform(state, { kind = "retry", id = entry.id, revision = entry.revision })
    return
  end
  if entry.draft_state == "unknown" then
    notifications.error(entry.error, "Harness message recovery")
    return
  end
  M.discard_rejected(state, entry.id)
  local message = { id = entry.id, text = entry.text, attachments = entry.attachments or {},
    purpose = entry.purpose, config = entry.config, intent = entry.intent }
  M.perform(state, { kind = "submit", message = message }, nil, entry.submission_target)
end

---Retain the editor draft until the host acknowledges this submission.
function M.submit(state, text, attachments, intent, config, callback, retracted, target)
  local message = { id = command.identity(), text = text, attachments = attachments,
    purpose = { kind = "user" }, config = config, intent = intent }
  if retracted then state.input.retraction[message.id] = retracted end
  M.perform(state, { kind = "submit", message = message }, callback, target)
  return message.id
end

---@return string
function M.reservation()
  return command.identity()
end

return M
