local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local command = require("forge.command")

---How long a submission may stay unsettled before the status split shows it as a waiting row.
M.submission_notice_delay_ms = 500

---@class ForgeInputViewState
---@field session_id string
---@field runtime_epoch? string
---@field host_generation integer
---@field revision integer
---@field confirmed table[] Committed host input; never optimistically edited.
---@field drafts table[] Unacknowledged or rejected editor drafts, never eligible for host delivery.
---@field entry_by_id table<string, table> Stable display objects for queue editing.
---@field retraction table<string, fun(): boolean>
---@field awaiting_claim table<string, true> Automatic submissions from this view that the host has
---  yet to claim, which no list draws until the notice delay passes.
---@field follow_up table<string, true> Messages the status split has shown waiting. Membership
---  lasts until the message is accepted or removed, so a shown row never hides and returns.
---@field running_steer table<string, true> Steering the running turn took, kept staged after its
---  acceptance until that turn ends.
---@field committing table<string, integer> In-flight edit commits per message id.
---@field removing table<string, integer> In-flight editor commands that remove a message, which no
---  list draws while the host has yet to answer.
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

-- Get whether an unclaimed message belongs in the queued follow-up list.
--
-- Only the host knows where an automatic submission goes: an idle session starts it as a turn, an
-- active turn takes it as steering, and other work makes it wait. Guessing from the editor's view
-- of the session drew such prompts as queued follow-ups for the moment before the host claimed them,
-- after a cancellation and for every steer. An automatic submission from this view therefore awaits
-- its claim undrawn, in `awaiting_claim`, until the notice delay shows that it waits. The queue draws
-- every other unsettled message: one the user queued, one that failed or is reserved for an edit,
-- any message while the queue is paused, and messages this view did not submit. An unanswered
-- draft ignores a pause, because the host resumes the queue when it admits an automatic one.
local function waits_in_queue(input, entry)
  if input.follow_up[entry.id] or not input.awaiting_claim[entry.id] then return true end
  if entry.local_only then return entry.draft_state ~= "pending" end
  return entry.error ~= nil or entry.reservation ~= nil or input.suspended or entry.delivery ~= "queued"
end

-- Place every unsettled message in exactly one list, moving it only forward.
--
-- An unclaimed message that does not wait stays in `pending_submit`, which no view stages, until
-- the host claims it into steering or a turn. A message shown in `queue` stays shown through its
-- claim, in `pending_handoff`, until the host accepts it; by then the transcript holds its prompt, so
-- the row hands over without a gap. Steering is different: the host reports a steer accepted from
-- its receipt before the exchange records the acknowledged copy that the status section lists
-- next, so an accepted steer stays in `pending_steer` while the turn that took it runs. The host
-- drops a staged copy once the exchange holds the message, so the section never lists it twice. A
-- message the editor is removing is drawn nowhere, so a deleted or retracted entry never
-- reappears while the host confirms the removal.
local function render(state)
  local input = state.input
  local queue, steering, handoff, submitting, retained, known = {}, {}, {}, {}, {}, {}
  local function place(entry, destination)
    if destination == queue then input.follow_up[entry.id] = true end
    if destination then destination[#destination + 1] = display_entry(input, entry) end
    retained[entry.id] = true
  end
  if not (input.running_steer and require("forge.views.harness.activity").busy(state)) then input.running_steer = {} end
  for _, entry in ipairs(input.confirmed) do
    known[entry.id] = true
    if entry.delivery == "accepted" then
      if input.running_steer[entry.id] and not input.removing[entry.id] then place(entry, steering) end
    else
      local destination
      if input.removing[entry.id] then destination = nil
      elseif entry.delivery == "delivering" then
        if entry.route == "steering" then
          destination = steering
          input.running_steer[entry.id] = true
        else destination = input.follow_up[entry.id] and handoff or submitting end
      else destination = waits_in_queue(input, entry) and queue or submitting end
      place(entry, destination)
    end
  end
  for _, draft in ipairs(input.drafts) do
    if not known[draft.id] then place(draft, waits_in_queue(input, draft) and queue or submitting) end
  end
  for id, entry in pairs(input.entry_by_id) do
    if not retained[id] and not (state.queue_edit and state.queue_edit.drafts[entry]) then
      input.entry_by_id[id] = nil
    end
  end
  for _, placement in ipairs({ input.awaiting_claim, input.follow_up, input.running_steer }) do
    for id in pairs(placement) do if not retained[id] then placement[id] = nil end end
  end
  state.queue, state.pending_steer, state.pending_handoff, state.pending_submit = queue, steering, handoff, submitting
  input.changed()
end

-- Show a submission that is still unsettled once the notice delay has passed.
--
-- The composer is cleared before the request leaves, and an owner that is holding commands, as it
-- does through a stop or a provider restart, answers minutes later. Until then the prompt would
-- exist nowhere on screen. The delay keeps an ordinary submission, answered and claimed at once,
-- from opening the status split for one frame.
local function notice_overdue(state, input, id)
  vim.defer_fn(function()
    if state.input ~= input or not owns_input(state, input) then return end
    local confirmed = input.confirmed[index_of(input.confirmed, id) or 0]
    local unsettled = index_of(input.drafts, id) ~= nil or (confirmed ~= nil and confirmed.delivery ~= "accepted")
    if not unsettled or not input.awaiting_claim[id] then return end
    input.follow_up[id], input.awaiting_claim[id] = true, nil
    render(state)
  end, M.submission_notice_delay_ms)
end

local function discard_draft(input, id)
  local index = index_of(input.drafts, id)
  if index then table.remove(input.drafts, index) end
end

---Apply ordered host input observations; only a missing baseline requires a snapshot.
---@param state table
---@param input ForgeInputViewState
---@param update table
local function project(state, input, update)
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

---Receive one host input observation and take back a retracted prompt.
---
---The host keeps a retracted prompt as an unsent queue entry, because the editor can exit before
---it restores the text. Once the composer holds the text, that entry would duplicate the draft, so
---the editor removes it. A composer holding newer work leaves the entry in the queue, where the
---user retries or edits it.
---@param state table
---@param update table
function M.receive(state, update)
  local input = state.input
  if not input or type(update.revision) ~= "number" then return end
  -- A retraction answers its editor-owned callback even if a snapshot already superseded its delta.
  local restore = type(update.retracted) == "string" and input.retraction[update.retracted] or nil
  local unsent
  if restore then
    input.retraction[update.retracted] = nil
    if restore() then unsent = (update.upsert or {})[1] end
  end
  -- The composer already shows the text, so the host's copy is withdrawn before it is ever drawn.
  if unsent then input.removing[unsent.id] = (input.removing[unsent.id] or 0) + 1 end
  project(state, input, update)
  if unsent then
    if state.input == input then
      M.perform(state, { kind = "remove", id = unsent.id, revision = unsent.revision })
    end
    M.release_removal(state, input, unsent.id)
  end
end

---Release one removal mark; a message whose removal did not happen is drawn again.
---@param state table
---@param input ForgeInputViewState
---@param id string
function M.release_removal(state, input, id)
  local remaining = input.removing[id] - 1
  input.removing[id] = remaining > 0 and remaining or nil
  if not input.removing[id] and state.input == input then render(state) end
end

---Place the messages again after the owner's activity changes phase: steering accepted by a
---running turn leaves the status section when that turn ends.
---@param state table
function M.activity_changed(state)
  if state.input then render(state) end
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
    awaiting_claim = {}, follow_up = {}, running_steer = {},
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
---@param target? table
---@param restore? fun(): boolean Returns a rejected submission's text to the composer when it is free.
function M.perform(state, action, callback, target, restore)
  local input = state.input
  local id = command.identity()
  local draft
  if action.kind == "submit" then
    draft = vim.deepcopy(action.message)
    draft.local_only, draft.draft_state = true, "pending"
    draft.operation_id, draft.submission_target = id, vim.deepcopy(target)
    if draft.intent ~= "queue" then
      input.awaiting_claim[draft.id] = true
      notice_overdue(state, input, draft.id)
    end
    input.drafts[#input.drafts + 1] = draft
    render(state)
  end
  local completed = false
  local committing = {}
  local removing = {}
  for _, edit in ipairs(action.kind == "commit" and action.edits or {}) do
    committing[#committing + 1] = edit.id
    input.committing[edit.id] = (input.committing[edit.id] or 0) + 1
    if edit.text == "" and not edit.config then removing[#removing + 1] = edit.id end
  end
  if action.kind == "remove" then removing[#removing + 1] = action.id end
  for _, removed_id in ipairs(removing) do input.removing[removed_id] = (input.removing[removed_id] or 0) + 1 end
  if #removing > 0 then render(state) end
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
      for _, removed_id in ipairs(removing) do M.release_removal(state, input, removed_id) end
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
    -- A rejected submission returns to the composer while it is free, and otherwise stays as a
    -- failed row carrying the host's reason, so its text is never left only in a pending row.
    local function reject(owner, reason)
      if restore and restore() then
        discard_draft(owner, draft.id)
        if state.input and state.input ~= owner then discard_draft(state.input, draft.id) end
        return
      end
      discard_draft(owner, draft.id)
      draft.draft_state = detail and detail.code == "outcome_unknown" and "unknown" or "rejected"
      draft.error = reason
      owner.drafts[#owner.drafts + 1] = draft
    end
    if not owns_input(state, input) then
      -- The view moved to another session, epoch, or host while the command was in flight. The
      -- caller still owns state tied to this command, so it hears the settlement as a failure.
      -- A replaced projection already carries its own copy of the draft.
      if draft and failure and state.input == input then
        reject(input, failure)
        render(state)
      end
      if failure then notifications.error(failure, "Harness message") end
      if callback then callback(failure or "Harness input changed before the host answered") end
      settle_commits()
      return
    end
    local admitted = draft and not index_of(input.drafts, draft.id)
    if result then
      M.receive(state, result)
      failure = result.error or failure
    end
    if admitted then failure = nil end
    if draft then
      if failure then reject(input, failure) else discard_draft(input, draft.id) end
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
  -- The failed row was on screen, so the retried message keeps its row instead of hiding until claimed.
  state.input.follow_up[entry.id] = true
  local message = { id = entry.id, text = entry.text, attachments = entry.attachments or {},
    purpose = entry.purpose, config = entry.config, intent = entry.intent }
  M.perform(state, { kind = "submit", message = message }, nil, entry.submission_target)
end

---Resume delivery of a queue an interrupt paused, keeping its order.
function M.resume(state)
  M.perform(state, { kind = "resume" })
end

---Retain the editor draft until the host acknowledges this submission.
---
---`restore` puts the text back into the composer when the composer is still free. It answers both a
---retraction and a rejected submission.
---@param restore? fun(): boolean
function M.submit(state, text, attachments, intent, config, callback, restore, target)
  local message = { id = command.identity(), text = text, attachments = attachments,
    purpose = { kind = "user" }, config = config, intent = intent }
  if restore then state.input.retraction[message.id] = restore end
  M.perform(state, { kind = "submit", message = message }, callback, target, restore)
  return message.id
end

---@return string
function M.reservation()
  return command.identity()
end

return M
