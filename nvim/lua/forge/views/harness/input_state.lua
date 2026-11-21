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
---@field retraction table<string, fun(check_only?: boolean): boolean>
---@field awaiting_claim table<string, true> Automatic submissions from this view that the host has
---  yet to claim, which no list draws until the notice delay passes.
---@field follow_up table<string, true> Messages the status split has shown waiting. Membership
---  lasts until the message is accepted or removed, so a shown row never hides and returns.
---@field waiting_steer table<string, true> Automatic submissions still unclaimed after the notice
---  delay while a provider turn ran, listed under steering as waiting until the host claims them.
---@field running_steer table<string, true> Steering the running turn took, kept staged after its
---  acceptance until that turn ends.
---@field committing table<string, integer> In-flight edit commits per message id.
---@field removing table<string, integer> In-flight editor commands that remove a message, which no
---  list draws while the host has yet to answer.
---@field commit_waiters table<string, fun()[]> Reservations waiting for a message's commit result.
---@field saved_edit table<string, ForgeSavedQueueEdit> Queue edits the user saved that the host has
---  yet to publish, drawn in place of the host's text so a saved row never shows its old text.
---@field refreshing? boolean
---@field required_revision? integer
---@field suspended boolean
---@field control_revision integer
---@field pause_operation_id? string
---@field submission_fence? ForgeInputControlFence Queue control that submissions made by the action
---  `under_fence` runs follow, captured when the user pressed the action's key.
---@field changed fun()

---@class ForgeInputControlFence
---@field input table Input projection the fence was captured from.
---@field control_revision integer Queue control revision the user had seen.
---@field pause_operation_id? string Newest interrupt the user had requested.

---@class ForgeSavedQueueEdit
---@field text string Draft the user saved, as the composer showed it.
---@field revision? integer Message revision the commit edits; the host publishes the edit at a
---  later revision. Unset while the commit waits for the edit's reservations.

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

-- Purposes of input the host produces for a task itself, never submitted by any view.
local host_produced_purpose = {
  task_entry = true, task_continuation = true, workflow_continuation = true,
  question_continuation = true, check_result = true,
}

---@param entry table
---@return boolean
local function host_produced(entry)
  local purpose = entry.purpose
  return type(purpose) == "table" and host_produced_purpose[purpose.kind] == true
end

local notice_overdue

-- Session phases in which the owner holds every automatic submission rather than steering it: an
-- open approval request, a parent waiting for its children, and a stop.
local holding_phase = { awaiting_approval = true, waiting_for_agent = true, stopping = true }

-- Get whether the running turn will take an unclaimed automatic submission as steering.
--
-- The host claims a steer only once the provider reports a running turn, so a steer sent while the
-- provider is still starting its turn stays unclaimed for as long as that start takes. Such a steer
-- waits for the active turn, not behind it, so it belongs in the steering section. A paused queue,
-- a failed or reserved message, and a phase in which the owner holds input keep it in the queue.
local function awaits_steering(state, input, entry)
  local status = state.status
  if input.suspended or entry.error ~= nil or entry.reservation ~= nil or entry.intent == "queue" then return false end
  if host_produced(entry) or type(status) ~= "table" or holding_phase[status.kind] then return false end
  local activity = require("forge.views.harness.activity")
  return activity.turn_running(state) and activity.turn_awaited(state)
end

-- Get whether an unclaimed message belongs in the queued follow-up list.
--
-- Only the host knows where an automatic submission goes: an idle session starts it as a turn, an
-- active turn takes it as steering, and other work makes it wait. Guessing from the editor's view
-- of the session drew such prompts as queued follow-ups for the moment before the host claimed them,
-- after a cancellation and for every steer. An automatic submission from this view therefore awaits
-- its claim undrawn, in `awaiting_claim`, until the notice delay shows that it waits. The queue draws
-- every other unsettled message: one the user queued, one that failed or is reserved for an edit,
-- any message while the queue is paused, and messages another view submitted. Input the host
-- produces for a task, such as its planning or continuation input, is claimed as promptly as an
-- automatic submission, so `render` enters it in `awaiting_claim` too. An unanswered
-- draft ignores a pause, because the host resumes the queue when it admits an automatic one.
local function waits_in_queue(input, entry)
  if input.follow_up[entry.id] or not input.awaiting_claim[entry.id] then return true end
  if entry.local_only then return entry.draft_state ~= "pending" end
  return entry.error ~= nil or entry.reservation ~= nil or input.suspended or entry.delivery ~= "queued"
end

-- Place every unsettled message in exactly one list, moving it only forward.
--
-- An automatic submission still unclaimed after the notice delay while a turn runs is listed in
-- `pending_steer` as waiting, because the host steers it into that turn once the provider can take
-- it. It moves to `queue` only when the owner starts holding input, and keeps its row through a
-- hand-off when the turn ended first and the host starts it as its own turn.
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
    -- A saved edit is drawn until the host publishes a revision past the one the commit edited.
    local saved = input.saved_edit[entry.id]
    if saved and saved.revision and (entry.revision or 0) > saved.revision then
      input.saved_edit[entry.id], saved = nil, nil
    end
    if destination then
      local displayed = display_entry(input, entry)
      displayed.saved_text = saved and saved.text
      destination[#destination + 1] = displayed
    end
    retained[entry.id] = true
  end
  -- A waiting steer leaves the steering section for the queue once the owner holds it; it never
  -- returns, so a held row does not move back and forth.
  local function unclaimed_destination(entry)
    if input.waiting_steer[entry.id] and not awaits_steering(state, input, entry) then
      input.waiting_steer[entry.id] = nil
      input.follow_up[entry.id] = true
    end
    if input.waiting_steer[entry.id] then return steering end
    return waits_in_queue(input, entry) and queue or submitting
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
        else
          destination = (input.follow_up[entry.id] or input.waiting_steer[entry.id]) and handoff or submitting
        end
      else
        if host_produced(entry) and entry.delivery == "queued" and not input.follow_up[entry.id]
          and not input.awaiting_claim[entry.id] then
          input.awaiting_claim[entry.id] = true
          notice_overdue(state, input, entry.id)
        end
        destination = unclaimed_destination(entry)
      end
      place(entry, destination)
      if destination == steering and entry.delivery == "queued" then input.entry_by_id[entry.id].awaiting_steer = true end
    end
  end
  for _, draft in ipairs(input.drafts) do
    if not known[draft.id] then place(draft, unclaimed_destination(draft)) end
  end
  for id, entry in pairs(input.entry_by_id) do
    if not retained[id] and not (state.queue_edit and state.queue_edit.drafts[entry]) then
      input.entry_by_id[id] = nil
    end
  end
  for _, placement in ipairs({ input.awaiting_claim, input.follow_up, input.waiting_steer, input.running_steer,
    input.saved_edit }) do
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
notice_overdue = function(state, input, id)
  vim.defer_fn(function()
    if state.input ~= input or not owns_input(state, input) then return end
    local confirmed = input.confirmed[index_of(input.confirmed, id) or 0]
    local unsettled = index_of(input.drafts, id) ~= nil or (confirmed ~= nil and confirmed.delivery ~= "accepted")
    if not unsettled or not input.awaiting_claim[id] then return end
    input.awaiting_claim[id] = nil
    local entry = confirmed or input.drafts[index_of(input.drafts, id)]
    if awaits_steering(state, input, entry) then input.waiting_steer[id] = true else input.follow_up[id] = true end
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
      local draft = input.drafts[index_of(input.drafts, id) or 0]
      if not (draft and draft.pickup_unconfirmed) then discard_draft(input, id) end
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
---the editor claims it by removing the host's copy first. A composer holding newer work leaves
---the entry in the queue. If the composer fills during the claim, the editor retains the removed
---text as an unsent draft.
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
    local candidate = (update.upsert or {})[1]
    local current = candidate and input.confirmed[index_of(input.confirmed, candidate.id) or 0]
    if candidate and (update.revision >= input.revision or current and current.revision == candidate.revision)
      and restore(true) then unsent = candidate end
  end
  -- Do not flash the host's copy while the editor is claiming it for the composer.
  if unsent then input.removing[unsent.id] = (input.removing[unsent.id] or 0) + 1 end
  project(state, input, update)
  if unsent and restore then
    if state.input == input then
      M.perform(state, { kind = "remove", id = unsent.id, revision = unsent.revision }, function(failure, detail)
        local uncertain = detail and detail.code == "outcome_unknown"
        if failure and not uncertain or not failure and restore() then return end
        local owner = state.input
        if not owner or owner.session_id ~= input.session_id then return end
        local draft = vim.deepcopy(unsent)
        draft.id = uncertain and unsent.id or command.identity()
        draft.local_only, draft.draft_state = true, uncertain and "unknown" or "rejected"
        draft.pickup_unconfirmed = uncertain
        if uncertain then draft.error = failure end
        -- If the host still owns the unsent entry, its row covers this recovery copy.
        owner.drafts[#owner.drafts + 1] = draft
        render(state)
      end)
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

---Place a waiting steer again after the session status changes: an approval, a wait for
---children, or a stop holds it in the queue.
---@param state table
function M.status_changed(state)
  if state.input and next(state.input.waiting_steer) then render(state) end
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
    awaiting_claim = {}, follow_up = {}, waiting_steer = {}, running_steer = {},
    committing = {}, removing = {}, commit_waiters = {}, saved_edit = {},
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
---@param callback? fun(failure: string?, detail: table?)
---@param target? table
---@param restore? fun(check_only?: boolean): boolean Returns a rejected submission's text to the composer when it is free.
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
  -- A submission follows the queue control of the moment its key was pressed: the host resumes a
  -- paused queue only for a submission that follows the pause.
  local control = action.kind == "submit" and input.submission_fence or input
  client.request({ session_id = input.session_id, method = "session.input.apply", params = {
    id = id, action = vim.deepcopy(action), control_revision = control.control_revision,
    after_control = control.pause_operation_id, target = vim.deepcopy(target),
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
      local uncertain = detail and detail.code == "outcome_unknown"
      if not uncertain and restore and restore() then
        discard_draft(owner, draft.id)
        if state.input and state.input ~= owner then discard_draft(state.input, draft.id) end
        return
      end
      discard_draft(owner, draft.id)
      draft.draft_state = uncertain and "unknown" or "rejected"
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
      if callback then callback(failure or "Harness input changed before the host answered", detail) end
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
    if callback then callback(failure, detail) end
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

---Draw `text` as the queued message `id` from the frame the user saves its edit.
---
---The edit ends in the editor before the host answers its commit, and the host's copy still holds
---the old text until then. Call again with the commit's base `revision` once the commit leaves; the
---row keeps `text` until the host publishes a later revision of the message.
---@param state table
---@param id string
---@param text string
---@param revision? integer
function M.hold_saved_edit(state, id, text, revision)
  state.input.saved_edit[id] = { text = text, revision = revision }
  render(state)
end

---Stop drawing a saved edit the host did not take, so the row shows the message the host holds.
---@param state table
---@param id string
function M.release_saved_edit(state, id)
  if not (state.input and state.input.saved_edit[id]) then return end
  state.input.saved_edit[id] = nil
  render(state)
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

---Capture the queue control the user sees, for a submission whose key runs its action later.
---
---A composer send waits one main-loop turn for a paste still arriving, while an interrupt runs at
---once, so `Ctrl-S Ctrl-C` reaches the host as the cancel followed by the submission. Stamped at
---send time, that submission claimed to follow the interrupt, and the host resumed the paused
---queue with it and ran it once the stop settled.
---@param state table
---@return ForgeInputControlFence?
function M.control_fence(state)
  local input = state.input
  if not input then return nil end
  return { input = input, control_revision = input.control_revision, pause_operation_id = input.pause_operation_id }
end

---Run `action` so that the submissions it makes follow `fence` rather than the current control.
---
---A fence from another input projection, such as one replaced by a session switch, is ignored.
---@param state table
---@param fence? ForgeInputControlFence
---@param action fun()
function M.under_fence(state, fence, action)
  local input = state.input
  if not (fence and input and fence.input == input) then return action() end
  local previous = input.submission_fence
  input.submission_fence = fence
  local ok, failure = pcall(action)
  input.submission_fence = previous
  if not ok then error(failure, 0) end
end

---Retain the editor draft until the host acknowledges this submission.
---
---`restore` puts the text back into the composer when the composer is still free. It answers both a
---retraction and a rejected submission.
---@param restore? fun(check_only?: boolean): boolean
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
