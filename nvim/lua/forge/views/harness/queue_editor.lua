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

---Move through queued entries without changing order or advancing the earliest edit barrier.
---@param state table
---@param draft string
---@param direction integer
---@return string? text
---@return string? failure
---@return ForgeAttachmentSelection[]? attachments
function M.move(state, draft, direction)
  local edit = state.queue_edit
  if not edit and state.failed_queue_edit then
    edit = state.failed_queue_edit
    state.failed_queue_edit = nil
    state.queue_edit = edit
    edit.original_draft = draft
    edit.original_attachments = attachment.capture(state.composer_buf).attachments
    return edit.drafts[edit.entry] or edit.entry.text, nil, edit.attachments[edit.entry] or edit.entry.attachments
  end
  if direction > 0 and not edit then return nil, "No queued prompt is being edited" end
  ---@type integer?
  local index = #state.queue + 1
  if edit then index = position(state.queue, edit.entry) end
  if not index then return nil, "The queued message being edited is no longer available" end
  index = index + direction
  if index < 1 then return nil, edit and "Already editing the first queued prompt" or "No queued prompt to edit" end
  if index > #state.queue then return nil, "Already editing the last queued prompt" end
  local entry = state.queue[index]
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
    input_state.perform(state, { kind = "reserve", id = entry.id, revision = entry.revision or 0,
      reservation = edit.reservation }, function(failure)
      if failure and state.queue_edit == edit then edit.failure = failure end
    end)
  end
  return edit.drafts[entry] or entry.text, nil, edit.attachments[entry] or entry.attachments
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
function M.finish(state, draft)
  local edit = state.queue_edit
  if not edit then
    return nil, "The queued message being edited is no longer available"
  end
  if not position(state.queue, edit.entry) then
    if draft ~= "" then return nil, "This message was delivered. Unqueue to keep the edited draft separately." end
    state.queue_edit = nil
    input_state.perform(state, { kind = "release", reservation = edit.reservation })
    return edit.original_draft, nil, edit.original_attachments
  end
  if edit.failure and draft ~= "" then return nil, edit.failure .. ". Unqueue to retain this draft separately." end
  edit.drafts[edit.entry] = draft
  edit.attachments[edit.entry] = attachment.capture(state.composer_buf).attachments
  local edits = {}
  local local_edits = {}
  for _, entry in ipairs(state.queue) do
    local updated = edit.drafts[entry]
    if updated ~= nil then
      if entry.local_only then
        local_edits[#local_edits + 1] = { entry = entry, text = updated,
          attachments = edit.attachments[entry] or entry.attachments or {} }
      else
        edits[#edits + 1] = { id = entry.id, revision = entry.revision or 0,
          text = edit.commit_text and edit.commit_text[entry] or updated,
          config = edit.commit_config and edit.commit_config[entry],
          attachments = edit.attachments[entry] or entry.attachments or {} }
      end
    end
  end
  state.queue_edit = nil
  for _, change in ipairs(local_edits) do
    input_state.discard_rejected(state, change.entry.id)
    if change.text:find("%S") then input_state.submit(state, change.text, change.attachments, "queue", change.entry.config) end
  end
  input_state.perform(state, { kind = "commit", reservation = edit.reservation, edits = edits }, function(failure)
    if failure then
      state.failed_queue_edit = edit
      edit.failure = failure
    end
  end)
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
    local text = edit and edit.drafts[entry] or entry.text
    if edit and entry == edit.entry then text = "[editing] " .. (draft or edit.drafts[entry] or entry.text)
    elseif entry.error then
      local keys = require("forge.shared.keymaps").view_keys_for("harness", "retry_input")
      local retry = keys[1] and (" " .. keys[1] .. " retry") or ""
      text = "[" .. entry.delivery .. "]" .. retry .. " " .. text .. " — " .. entry.error
    end
    staged[#staged + 1] = { id = entry.id, text = text, failed = entry.error ~= nil }
  end
  return staged
end

return M
