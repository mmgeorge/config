local PromptSubmission = {}

local client = require("forge.client")
local attachment = require("forge.views.harness.attachment")

---@class ForgePromptContext
---@field document string
---@field buffer integer
---@field is_current fun(): boolean

---@class ForgePromptSubmission
---@field token integer
---@field context? ForgePromptContext
---@field source? string[]
---@field changedtick? integer
---@field cleared_tick? integer
---@field accepted? boolean
---@field attachments table[]

---Retain request completion in the session even when its composer presentation closes.
---@param state ForgeHarnessPresentationState
---@param prompt {text: string, attachments?: table[], context?: ForgePromptContext, source?: string[], changedtick?: integer}
---@param completed fun(result?: any, failure?: string, detail?: table)
function PromptSubmission.start(state, prompt, completed)
  if state.prompt_submission then
    completed(nil, "Composer submission is already active")
    return
  end
  local session_id = state.session and state.session.id
  if not session_id then completed(nil, "Harness session is not ready") return end
  local context = prompt.context
  local source = context and (prompt.source or vim.api.nvim_buf_get_lines(context.buffer, 0, -1, false)) or nil
  local text = source and table.concat(source, "\n") or prompt.text
  if not text:find("%S") then completed(nil, "Prompt requires nonempty text") return end
  state.prompt_sequence = (state.prompt_sequence or 0) + 1
  local submission = {
    token = state.prompt_sequence, context = context, source = source,
    changedtick = context and (prompt.changedtick or vim.api.nvim_buf_get_changedtick(context.buffer)) or nil,
    attachments = prompt.attachments or (context and attachment.capture(context.buffer).attachments) or {},
  }
  state.prompt_submission = submission
  local function settle(result, failure, detail)
    if state.prompt_submission ~= submission then return end
    state.prompt_submission = nil
    completed(result, failure, detail)
  end
  local sent, failure = pcall(client.request_for, session_id, "prompt.submit", {
    text = text, attachments = submission.attachments,
    submission = context and { document = context.document, token = submission.token } or nil,
  }, settle)
  if not sent then
    if state.prompt_submission ~= submission then error(failure, 0) end
    settle(nil, "Prompt request failed: " .. tostring(failure))
  end
end

---Apply acceptance and retraction only to the original, unchanged draft.
---@param state ForgeHarnessPresentationState
---@param event table
---@return boolean
function PromptSubmission.receive(state, event)
  local transition = event.data
  local submission = state.prompt_submission
  if not submission then return false end
  local context = submission.context
  local source = submission.source
  if event.kind ~= "prompt_submission" or type(transition) ~= "table" or not context
    or not source
    or transition.document ~= context.document or transition.token ~= submission.token
    or not context.is_current() or not vim.api.nvim_buf_is_valid(context.buffer) then return false end
  local changedtick = vim.api.nvim_buf_get_changedtick(context.buffer)
  if transition.state == "accepted" and not submission.accepted then
    submission.accepted = true
    if changedtick == submission.changedtick then
      vim.api.nvim_buf_set_lines(context.buffer, 0, -1, false, { "" })
      attachment.restore(context.buffer, {})
      submission.cleared_tick = vim.api.nvim_buf_get_changedtick(context.buffer)
    end
  elseif transition.state == "retracted" and submission.cleared_tick then
    if changedtick == submission.cleared_tick then
      vim.api.nvim_buf_set_lines(context.buffer, 0, -1, false, source)
      attachment.restore(context.buffer, submission.attachments)
    end
    submission.cleared_tick = nil
  end
  return true
end

return PromptSubmission
