local HarnessSnapshot = {}

local prompt_history = require("forge.views.harness.prompt_history")
local activity = require("forge.views.harness.activity")

---Convert nullable snapshot fields to absent Lua values without changing the wire response.
local function presentation_value(value)
  if value == vim.NIL then return nil end
  if type(value) ~= "table" then return value end
  local projected = {}
  for key, field in pairs(value) do projected[key] = presentation_value(field) end
  return projected
end

---Retain the receipt for a page the transport delivered, even when its state snapshot is stale.
---@param state table
---@param result table
function HarnessSnapshot.receive_publication(state, result)
  local publication = result.owner_publication
  if not publication then return end
  if state.publication_cursor ~= nil and publication.after ~= state.publication_cursor then return end
  state.publication_received = {
    receipt_id = publication.receipt_id, after = publication.after, through = publication.through,
    runtime_epoch = result.runtime_epoch,
  }
  state.publication_more = publication.more == true
end

---@param state table
---@param result table
function HarnessSnapshot.apply(state, result)
  result = assert(presentation_value(result), "Harness snapshot is missing")
  assert(type(result) == "table" and type(result.session) == "table" and type(result.session.id) == "string" and result.session.id ~= "",
    "Harness snapshot is missing its session identity")
  HarnessSnapshot.receive_publication(state, result)
  if state.session and result.session and state.session.id == result.session.id
    and state.runtime_epoch == result.runtime_epoch
    and (result.snapshot_revision or 0) < (state.snapshot_revision or 0) then return false end
  local session_changed = state.session and result.session and state.session.id ~= result.session.id
  local runtime_changed = state.runtime_epoch ~= result.runtime_epoch
  if state.session and (session_changed or runtime_changed) then
    state.model_catalog_bound = nil
    require("forge.views.harness.provider_catalog").invalidate_models(state)
  elseif state.session and state.model_catalog_bound and (state.session.model ~= result.session.model
    or state.session.effort ~= result.session.effort) then
    require("forge.views.harness.provider_catalog").invalidate_models(state)
  end
  if session_changed or state.runtime_epoch and state.runtime_epoch ~= result.runtime_epoch then
    state.questions = nil
    state.publication_cursor, state.publication_received, state.publication_more = nil, nil, nil
    if session_changed then
      local picker = require("forge.views.picker")
      if picker.is_open("questions") then
        state.question_picker_resume = picker.capture_state()
        picker.close(false)
      end
      state.question_view_by_session = state.question_view_by_session or {}
      state.question_view_by_session[state.session.id] = {
        drafts = state.question_drafts, resume = state.question_picker_resume, history = state.question_show_history,
        answers = state.question_answer_state,
      }
      local question_view = state.question_view_by_session[result.session.id] or {}
      state.question_drafts, state.question_picker_resume = question_view.drafts, question_view.resume
      state.question_show_history = question_view.history
      state.question_answer_state = question_view.answers
      state.question_saving, state.question_failure = nil, nil
    end
    state.restore_applying, state.activity = nil, nil
    require("forge.views.harness.cleanup_progress").settled(state)
    state.cancellation = nil
    if state.presentation and state.presentation.transcript then
      state.presentation.status_view:resize()
    end
  end
  state.runtime_epoch, state.snapshot_revision = result.runtime_epoch, result.snapshot_revision
  if session_changed or runtime_changed then HarnessSnapshot.receive_publication(state, result) end
  require("forge.views.harness.cleanup_progress").apply(state, result.cancellation)
  if result.activity then activity.receive(state, result.activity, result.runtime_epoch) end
  if state.host_error then state.host_error, state.execution_notice = nil, nil end
  local previous_session_id = state.session and state.session.id or nil
  local previous_context_usage = state.session and state.session.context_usage or nil
  local same_context = not runtime_changed and state.session and result.session
    and state.session.model == result.session.model
    and state.session.resolved_model == result.session.resolved_model
    and state.session.context_window == result.session.context_window
  state.session = result.session
  state.access_preference = result.access_preference
  state.access_abbreviation = result.access_abbreviation
  if same_context and previous_session_id == (result.session and result.session.id or nil)
    and previous_context_usage and not state.session.context_usage
  then
    state.session.context_usage = previous_context_usage
  end
  if previous_session_id ~= (result.session and result.session.id or nil) then
    state.restore_applying = nil
    state.activity_expanded = {}
    require("forge.views.harness.recap").clear(state)
  end
  state.capability = result.capability or {}
  state.subagent_tools = result.subagent_tools == true
  state.lsp_tools = result.lsp_tools == true
  state.session_tools = result.session_tools ~= false
  state.status_thoughts = result.status_thoughts == true
  state.ask_model = result.ask_model or {}
  state.check_editing = result.check_editing == true
  state.checks_running = result.checks_running == true
  state.check_timing = result.check_timing or "after_queue"
  state.automatic_checks = result.automatic_checks == true
  state.checks_deferred = result.checks_deferred == true
  state.active_check_run = result.active_check_run
  state.timeline_revision = result.timeline_revision or 0
  state.status = result.status or { kind = "idle" }
  state.artifact = vim.deepcopy(result.artifact or {})
  state.no_checkpoint = result.no_checkpoint == true
  state.restore_recovery = result.restore_recovery
  state.active_plan = result.active_plan
  if result.active_plan then
    -- The review module follows a newly submitted revision outside snapshot application: an open
    -- review reloads in place, and a closed one gets a single notice that the revision is ready.
    local plan = result.active_plan
    vim.schedule(function()
      if state.active_plan == plan then require("forge.views.plan_review.native_controller").observe(plan) end
    end)
  end
  require("forge.views.harness.questions").receive(state, result.questions)
  if result.active_elicitation and result.active_elicitation ~= vim.NIL then
    require("forge.views.harness.question_presentation").receive(state, result.active_elicitation, true)
  else
    state.active_elicitation = nil
  end
  state.active_wait = vim.deepcopy(result.active_wait)
  state.approval = vim.deepcopy(result.approval or {})
  -- Child list revisions count within one session's host run, so another session's list or a
  -- list from an earlier run never outranks this one.
  if session_changed or runtime_changed then state.agent = nil end
  require("forge.views.harness.agent_catalog").replace(state,
    result.agent or { definition = {}, run = {}, exchange = {} })
  if state.selected_agent_run_id then
    local selected_exists = vim.iter(state.agent.run or {}):any(function(run)
      return run.id == state.selected_agent_run_id
    end)
    if not selected_exists then
      -- The composer now addresses Main, so the transcript must stop showing the vanished child.
      state.selected_agent_run_id = nil
      if state.presentation and state.presentation.select_agent then state.presentation.select_agent(nil) end
    end
  end
  if result.prompt_history then prompt_history.replace(result.prompt_history) end
  if result.input and state.input and state.input.session_id == state.session.id
    and state.input.runtime_epoch == state.runtime_epoch
    and state.input.host_generation == require("forge.client").host_get_generation() then
    require("forge.views.harness.input_state").receive(state, { revision = result.input.revision, snapshot = result.input })
  end
  return true
end

---Promote only the exact received page that the owner durably confirmed after output admission.
---@param state table
---@param acknowledgment table
---@return boolean
function HarnessSnapshot.confirm_publication(state, acknowledgment)
  acknowledgment = presentation_value(acknowledgment)
  local received = state.publication_received
  if not received or received.receipt_id ~= acknowledgment.receipt_id
    or received.runtime_epoch ~= acknowledgment.runtime_epoch
    or state.runtime_epoch ~= acknowledgment.runtime_epoch
    or received.after ~= acknowledgment.after or received.through ~= acknowledgment.through
    or state.publication_cursor ~= nil and received.after ~= state.publication_cursor then return false end
  state.publication_cursor = received.through or received.after or state.publication_cursor
  state.publication_received = nil
  return true
end

return HarnessSnapshot
