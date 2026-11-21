local M = {}
local attachment = require("forge.views.harness.attachment")
local input_state = require("forge.views.harness.input_state")

---@class ForgeQuestionReplyOption
---@field id string
---@field label string
---@field description string
---@field recommended boolean

---@class ForgeQuestionReplyDecision
---@field kind 'answer'|'correction'
---@field answer string
---@field required_questions_remain boolean

---@class ForgeQuestionReplyClarification
---@field kind 'clarification'
---@field options ForgeQuestionReplyOption[]
---@field text string
---@field reply_to? string

---@alias ForgeQuestionReplyResponse ForgeQuestionReplyDecision|ForgeQuestionReplyClarification

---@class ForgeQuestionReply
---@field question_id string
---@field revision integer
---@field message_id string
---@field question string
---@field response ForgeQuestionReplyResponse

---@class ForgeStagedMessageContent
---@field kind 'message'

---@class ForgeStagedQuestionContent
---@field kind 'question_response'
---@field responses ForgeQuestionReply[] Original structured replies from the host input purpose.

---@alias ForgeStagedContent ForgeStagedMessageContent|ForgeStagedQuestionContent

---@class ForgeStagedInput
---@field id string
---@field text string
---@field failed boolean
---@field failure_notice? string Recovery detail that does not repeat the prompt.
---@field content ForgeStagedContent

---@class ForgeQueuedPrompt
---@field text string
---@field purpose? table
---@field config? table
---@field attachments? ForgeAttachmentSelection[]

---@class ForgeQueueEdit
---@field entry ForgeQueuedPrompt Entry currently displayed in the composer.
---@field drafts table<ForgeQueuedPrompt, string> Edits retained while moving through the queue.
---@field original_draft string Composer draft restored when editing finishes.
---@field attachments table<ForgeQueuedPrompt, ForgeAttachmentSelection[]>
---@field original_attachments ForgeAttachmentSelection[]
---@field buffer? integer Composer buffer currently displaying the active edit.
---@field reservation string Edit identity the host records on every message this edit holds.
---@field reserving? table<string, true> Messages whose reservation awaits the host's answer.
---@field releasing? table<string, fun()|false> Messages whose release awaits the host's answer,
---  with the reservation to send once it lands when navigation returned to the message meanwhile.
---@field release_after? table<string, true> Messages left unchanged while their reservation was
---  still in flight, released once the host acknowledges it.
---@field delivered_entry? ForgeQueuedPrompt Entry that settled while its draft was open.
---@field failure? string Reservation failure awaiting recovery.
---@field failure_entry? ForgeQueuedPrompt Entry whose reservation failed.

---@param queue (string|ForgeQueuedPrompt)[]
---@param entry ForgeQueuedPrompt
---@return integer?
local function position(queue, entry)
  for index, candidate in ipairs(queue) do
    if candidate == entry then return index end
  end
end

---Get the text the queue shows for `entry`: a saved edit the host has yet to publish, or the queued
---text, with configuration-only entries exposed as model commands without changing queued payloads.
---@param entry ForgeQueuedPrompt
---@return string
local function queued_text(entry)
  if entry.saved_text then return entry.saved_text end
  if entry.text ~= "" or not (entry.config and entry.config.model) then return entry.text end
  local text = "/model " .. entry.config.model
  if entry.config.effort then text = text .. " " .. entry.config.effort end
  return text
end

---Get whether the composer still shows `entry` exactly as it is queued.
---@param entry ForgeQueuedPrompt
---@param draft string
---@param attachments? ForgeAttachmentSelection[]
---@return boolean
local function unchanged(entry, draft, attachments)
  return draft == queued_text(entry) and vim.deep_equal(attachments or {}, entry.attachments or {})
end

---Reserve `entry` for this edit once any earlier commit of it has settled.
---@param state table
---@param edit ForgeQueueEdit
---@param entry ForgeQueuedPrompt
local function reserve(state, edit, entry)
  edit.reserving = edit.reserving or {}
  edit.reserving[entry.id] = true
  -- An earlier edit of this message may still await its host result. Read the revision only
  -- after it settles, because the displayed revision is stale until then.
  input_state.after_commit(state, entry.id, function()
    input_state.perform(state, { kind = "reserve", id = entry.id, revision = entry.revision or 0,
      reservation = edit.reservation }, function(failure)
      edit.reserving[entry.id] = nil
      if edit.release_after and edit.release_after[entry.id] then
        edit.release_after[entry.id] = nil
        if not failure then M.release(state, edit, entry) end
      elseif failure and edit.delivered_entry ~= entry then
        edit.failure, edit.failure_entry = failure, entry
      end
      if edit.commit and not next(edit.reserving) then
        local commit = edit.commit
        edit.commit = nil
        commit()
      end
    end)
  end)
end

---Release one message from the edit while the edit keeps every other message it holds.
---
---Navigation reserves each message it visits, and a reserved message waits. Releasing a message the
---user left unchanged lets it run while a later message stays open in the composer.
---@param state table
---@param edit ForgeQueueEdit
---@param entry ForgeQueuedPrompt
function M.release(state, edit, entry)
  edit.drafts[entry], edit.attachments[entry] = nil, nil
  if edit.reserving and edit.reserving[entry.id] then
    edit.release_after = edit.release_after or {}
    edit.release_after[entry.id] = true
    return
  end
  edit.releasing = edit.releasing or {}
  edit.releasing[entry.id] = false
  input_state.perform(state, { kind = "release", reservation = edit.reservation, id = entry.id }, function()
    local returned = edit.releasing[entry.id]
    edit.releasing[entry.id] = nil
    if returned then returned() end
  end)
end

---Get whether the message being edited left the queue without this editor removing it, as it does
---when delivery wins the race with the edit's reservation.
---@param state table
---@return boolean
function M.delivered(state)
  local edit = state.queue_edit
  if not edit or edit.entry.local_only or position(state.queue, edit.entry) then return false end
  return not (state.input and state.input.removing[edit.entry.id])
end

---Move through queued entries without changing order, releasing an entry left unchanged.
---@param state table
---@param draft string
---@param direction integer
---@return string? text
---@return string? failure
---@return ForgeAttachmentSelection[]? attachments
---@return ("sent"|"edited")? recovered See `M.finish`.
function M.move(state, draft, direction)
  local edit = state.queue_edit
  if not edit and state.failed_queue_edit then
    edit = state.failed_queue_edit
    state.failed_queue_edit = nil
    state.queue_edit = edit
    edit.original_draft = draft
    edit.original_attachments = attachment.capture(state.composer_buf).attachments
    return edit.drafts[edit.entry] or queued_text(edit.entry), nil, edit.attachments[edit.entry] or edit.entry.attachments
  end
  if direction > 0 and not edit then return nil, "No queued prompt is being edited" end
  ---@type integer?
  local index = #state.queue + 1
  if edit then index = position(state.queue, edit.entry) end
  if not index then return M.finish(state, draft) end
  index = index + direction
  if index < 1 then return nil, edit and "Already editing the first queued prompt" or "No queued prompt to edit" end
  if index > #state.queue then return nil, "Already editing the last queued prompt" end
  local entry = state.queue[index]
  -- The confirmed projection keeps the entry until removal is acknowledged.
  -- It must not become a new edit target during that interval.
  if state.input and state.input.removing[entry.id] then
    require("forge.infra.perf").diagnostic("ui.queue.rejected", { reason = "removal_pending",
      entry_id = entry.id, session_id = state.input.session_id })
    return nil, "This queued message is being removed. Wait for acknowledgement before editing."
  end
  -- A pending submission is already immutable and may become deliverable at any
  -- moment. Editing it as a local draft would bypass the host reservation entirely.
  if entry.local_only and entry.draft_state == "pending" then
    return nil, "This message is still being submitted. Wait for acknowledgement before editing."
  end
  if entry.delivery == "delivering" or entry.delivery == "unknown" or entry.draft_state == "unknown" then
    return nil, "Delivery has started. The original message cannot be edited."
  end
  if edit then
    local left = edit.entry
    local left_attachments = attachment.capture(state.composer_buf).attachments
    if not left.local_only and unchanged(left, draft, left_attachments) then
      M.release(state, edit, left)
    else
      edit.drafts[left], edit.attachments[left] = draft, left_attachments
    end
  else edit = { entry = entry, drafts = {}, attachments = {}, original_draft = draft,
    reservation = input_state.reservation(),
    original_attachments = attachment.capture(state.composer_buf).attachments, buffer = state.composer_buf } end
  edit.entry = entry
  state.queue_edit = edit
  if edit.release_after then edit.release_after[entry.id] = nil end
  if not entry.local_only and not (edit.reserving and edit.reserving[entry.id]) then
    if edit.releasing and edit.releasing[entry.id] ~= nil then
      -- The release is still in flight; reserve again once the host has answered it.
      edit.releasing[entry.id] = function()
        if state.queue_edit == edit and edit.entry == entry then reserve(state, edit, entry) end
      end
    elseif entry.reservation ~= edit.reservation then
      reserve(state, edit, entry)
    end
  end
  return edit.drafts[entry] or queued_text(entry), nil, edit.attachments[entry] or entry.attachments
end

---Save the active draft without changing its reserved position.
---@param state table
---@param draft string
function M.remember(state, draft)
  if state.queue_edit then
    state.queue_edit.drafts[state.queue_edit.entry] = draft
    state.queue_edit.attachments[state.queue_edit.entry] = attachment.capture(state.composer_buf).attachments
  end
end

---Commit edited entries in place and remove cleared entries, preserving all remaining identities.
---@param state table
---@param draft string
---@param unqueue? boolean
---@return string? original_draft
---@return string? failure
---@return ForgeAttachmentSelection[]? attachments
---@return ("sent"|"edited")? recovered Why an edit whose message left the queue ended: its
---  unchanged text gave way to the earlier draft, or its changed text became an ordinary draft.
function M.finish(state, draft, unqueue)
  local edit = state.queue_edit
  if not edit then
    return nil, "The queued message being edited is no longer available"
  end
  local restored, restored_attachments = edit.original_draft, edit.original_attachments
  local recovered
  if not position(state.queue, edit.entry) then
    require("forge.infra.perf").diagnostic("ui.queue.recovered", { reason = "entry_missing",
      entry_id = edit.entry.id, reservation_id = edit.reservation, session_id = state.input and state.input.session_id })
    -- Delivery can win the race with this reservation while earlier edits still hold theirs.
    -- Retire the missing entry and commit the surviving reservations below.
    local attachments = attachment.capture(state.composer_buf).attachments
    recovered = "sent"
    if draft ~= "" and not unchanged(edit.entry, draft, attachments) then
      restored, restored_attachments, recovered = draft, attachments, "edited"
    end
    edit.delivered_entry = edit.entry
    if edit.reserving then edit.reserving[edit.entry.id] = nil end
    if edit.failure_entry == edit.entry then edit.failure, edit.failure_entry = nil, nil end
    edit.drafts[edit.entry], edit.attachments[edit.entry] = nil, nil
  else
    if not unqueue and edit.failure and draft ~= "" then
      return nil, edit.failure .. ". Unqueue to retain this draft separately."
    end
    edit.drafts[edit.entry] = unqueue and "" or draft
    edit.attachments[edit.entry] = attachment.capture(state.composer_buf).attachments
  end
  local entries = {}
  for _, entry in ipairs(state.queue) do
    if edit.drafts[entry] ~= nil then entries[#entries + 1] = entry end
  end
  state.queue_edit = nil
  if recovered and #entries == 0 then
    input_state.perform(state, { kind = "release", reservation = edit.reservation })
    return restored, nil, restored_attachments, recovered
  end
  -- The host answers the commit later, and later still when it waits for a reservation, so each
  -- saved row shows its saved text from this frame instead of the host's old copy.
  for _, entry in ipairs(entries) do
    if not entry.local_only and edit.drafts[entry] ~= "" then input_state.hold_saved_edit(state, entry.id, edit.drafts[entry]) end
  end
  local function failed(failure)
    if not failure then return end
    state.failed_queue_edit = edit
    edit.failure = failure
    for _, entry in ipairs(entries) do input_state.release_saved_edit(state, entry.id) end
  end
  local function commit()
    if edit.failure then failed(edit.failure) return end
    local edits = {}
    for _, entry in ipairs(entries) do
      local updated = edit.drafts[entry]
      if entry.local_only then
        if entry.draft_state == "unknown" and updated:find("%S") then
          failed("Confirm the original command before resubmitting this draft.")
          return
        end
      elseif entry.reservation ~= edit.reservation then
        failed("The queued message changed before its edit reservation completed.")
        return
      else
        edits[#edits + 1] = { id = entry.id, revision = entry.revision or 0,
          text = edit.commit_text and edit.commit_text[entry] or updated,
          config = edit.commit_config and edit.commit_config[entry],
          attachments = edit.attachments[entry] or entry.attachments or {} }
      end
    end
    for _, sent in ipairs(edits) do
      if state.input.saved_edit[sent.id] then
        input_state.hold_saved_edit(state, sent.id, state.input.saved_edit[sent.id].text, sent.revision)
      end
    end
    for _, entry in ipairs(entries) do
      local updated = edit.drafts[entry]
      if entry.local_only then
        input_state.discard_rejected(state, entry.id)
        if updated:find("%S") then
          input_state.submit(state, updated, edit.attachments[entry] or entry.attachments or {}, "queue", entry.config)
        end
      end
    end
    input_state.perform(state, { kind = "commit", reservation = edit.reservation, edits = edits }, failed)
  end
  if edit.reserving and next(edit.reserving) then edit.commit = commit else commit() end
  return restored, nil, restored_attachments, recovered
end

---Mark the reserved entry in the status split without changing queued payloads.
---@param state table
---@param draft? string
---@return ForgeStagedInput[]
function M.staging(state, draft, entries)
  local edit = state.queue_edit
  local staged = {}
  for _, entry in ipairs(entries or state.queue) do
    local text = edit and edit.drafts[entry] or queued_text(entry)
    local failure_notice
    if edit and entry == edit.entry then text = "[editing] " .. (draft or edit.drafts[entry] or queued_text(entry))
    elseif entry.local_only and entry.draft_state == "pending" then
      text = "[submitting] " .. text
    elseif entry.awaiting_steer then
      text = "[waiting] " .. text
    elseif entry.error then
      local keys = require("forge.shared.keymaps").view_keys_for("harness", "retry_input")
      local retry = keys[1] and (" " .. keys[1] .. " retry") or ""
      failure_notice = keys[1] and (keys[1] .. " retry · " .. entry.error) or entry.error
      text = "[" .. (entry.draft_state or entry.delivery) .. "]" .. retry .. " " .. text .. " — " .. entry.error
    end
    ---@type ForgeStagedContent
    local content = { kind = "message" }
    if entry.purpose and entry.purpose.kind == "question" then
      local responses = { entry.purpose.reply }
      vim.list_extend(responses, entry.purpose.additional_replies or {})
      content = { kind = "question_response", responses = responses }
    end
    staged[#staged + 1] = { id = entry.id, text = text, failed = entry.error ~= nil, failure_notice = failure_notice,
      content = content }
  end
  return staged
end

return M
