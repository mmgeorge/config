local M = {}
local attachment = require("forge.views.harness.attachment")
local input_state = require("forge.views.harness.input_state")

---@class ForgeQueuedPrompt
---@field text string
---@field purpose? table
---@field config? table
---@field attachments? ForgeAttachmentSelection[]

---@class ForgeQueueEdit
---@field entry ForgeQueuedPrompt Entry currently displayed in the composer.
---@field barrier ForgeQueuedPrompt Earliest unfinished edit, retained when validation focuses a later entry.
---@field drafts table<ForgeQueuedPrompt, string> Edits retained while moving through the queue.
---@field original_draft string Composer draft restored when editing finishes.
---@field attachments table<ForgeQueuedPrompt, ForgeAttachmentSelection[]>
---@field original_attachments ForgeAttachmentSelection[]
---@field buffer? integer Composer buffer currently displaying the active edit.

---@param queue (string|ForgeQueuedPrompt)[]
---@param entry ForgeQueuedPrompt
---@return integer?
local function position(queue, entry)
  for index, candidate in ipairs(queue) do
    if candidate == entry then return index end
  end
end

---Expose configuration-only entries as model commands without changing queued payloads.
---@param entry ForgeQueuedPrompt
---@return string
local function queued_text(entry)
  if entry.text ~= "" or not (entry.config and entry.config.model) then return entry.text end
  local text = "/model " .. entry.config.model
  if entry.config.effort then text = text .. " " .. entry.config.effort end
  return text
end

---Move through queued entries without changing order or advancing the earliest edit barrier.
---@param state table
---@param draft string
---@param direction integer
---@return string? text
---@return string? failure
---@return ForgeAttachmentSelection[]? attachments
---@return boolean? recovered
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
    return nil, "This queued message is being removed. Wait for acknowledgement before editing."
  end
  -- A pending submission is already immutable and may become deliverable at any
  -- moment. Editing it as a local draft would bypass the host reservation entirely.
  if entry.local_only and entry.draft_state == "pending" then
    return nil, "This message is still being submitted. Wait for acknowledgement before editing."
  end
  if entry.delivery == "delivering" or entry.delivery == "unknown" then
    return nil, "Delivery has started. The original message cannot be edited."
  end
  if edit then
    edit.drafts[edit.entry] = draft
    edit.attachments[edit.entry] = attachment.capture(state.composer_buf).attachments
  else edit = { entry = entry, barrier = entry, drafts = {}, attachments = {}, original_draft = draft,
    reservation = input_state.reservation(),
    original_attachments = attachment.capture(state.composer_buf).attachments, buffer = state.composer_buf } end
  if index < (position(state.queue, edit.barrier) or index) then edit.barrier = entry end
  edit.entry = entry
  state.queue_edit = edit
  if entry.reservation ~= edit.reservation and not entry.local_only then
    edit.reserving = edit.reserving or {}
    if edit.reserving[entry.id] then return edit.drafts[entry] or queued_text(entry), nil, edit.attachments[entry] or entry.attachments end
    edit.reserving[entry.id] = true
    -- An earlier edit of this message may still await its host result. Read the revision only
    -- after it settles, because the displayed revision is stale until then.
    input_state.after_commit(state, entry.id, function()
      input_state.perform(state, { kind = "reserve", id = entry.id, revision = entry.revision or 0,
        reservation = edit.reservation }, function(failure)
        edit.reserving[entry.id] = nil
        if failure then edit.failure = failure end
        if edit.commit and not next(edit.reserving) then
          local commit = edit.commit
          edit.commit = nil
          commit()
        end
      end)
    end)
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
---@return string? original_draft
---@return string? failure
---@return ForgeAttachmentSelection[]? attachments
---@return boolean? recovered Whether an unavailable edit became an ordinary draft.
function M.finish(state, draft)
  local edit = state.queue_edit
  if not edit then
    return nil, "The queued message being edited is no longer available"
  end
  if not position(state.queue, edit.entry) then
    -- Delivery can win the race with reservation. End editor ownership instead of
    -- trapping every subsequent submit behind an entry that cannot be edited.
    local attachments = attachment.capture(state.composer_buf).attachments
    state.queue_edit = nil
    input_state.perform(state, { kind = "release", reservation = edit.reservation })
    if draft ~= "" then return draft, nil, attachments, true end
    return edit.original_draft, nil, edit.original_attachments
  end
  if edit.failure and draft ~= "" then return nil, edit.failure .. ". Unqueue to retain this draft separately." end
  edit.drafts[edit.entry] = draft
  edit.attachments[edit.entry] = attachment.capture(state.composer_buf).attachments
  local entries = {}
  for _, entry in ipairs(state.queue) do
    if edit.drafts[entry] ~= nil then entries[#entries + 1] = entry end
  end
  state.queue_edit = nil
  local function failed(failure)
    if not failure then return end
    state.failed_queue_edit = edit
    edit.failure = failure
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
  return edit.original_draft, nil, edit.original_attachments
end

---Mark the reserved entry in the status split without changing queued payloads.
---@param state table
---@param draft? string
---@return (string|{text: string})[]
function M.staging(state, draft, entries)
  local edit = state.queue_edit
  local staged = {}
  for _, entry in ipairs(entries or state.queue) do
    local text = edit and edit.drafts[entry] or queued_text(entry)
    if edit and entry == edit.entry then text = "[editing] " .. (draft or edit.drafts[entry] or queued_text(entry))
    elseif entry.local_only and entry.draft_state == "pending" then
      text = "[submitting] " .. text
    elseif entry.error then
      local keys = require("forge.shared.keymaps").view_keys_for("harness", "retry_input")
      local retry = keys[1] and (" " .. keys[1] .. " retry") or ""
      text = "[" .. (entry.draft_state or entry.delivery) .. "]" .. retry .. " " .. text .. " — " .. entry.error
    end
    staged[#staged + 1] = { id = entry.id, text = text, failed = entry.error ~= nil }
  end
  return staged
end

return M
