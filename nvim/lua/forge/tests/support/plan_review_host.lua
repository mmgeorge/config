-- A mock plan host for PlanReview cases that need review state without the real Forge host. It
-- answers `forge.client` for the rest of the current case: `plan_open` presents the plan lines and
-- every annotation saved so far with its answer, `plan_save_annotations` stores the annotations,
-- and each `session.plan.question.answer` request waits in `answers` until the case completes it.
--
--   local plan_review_host = dofile("nvim/lua/forge/tests/support/plan_review_host.lua")
--   local plan_host = plan_review_host.start({ lines = { "pub fn requested_change();" } })
--   local review = plan_host.open()
--   ... save a question ...
--   plan_host.answer(1, "The caller owns it.")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
local encode_delivery = dofile("nvim/lua/forge/tests/support/plan_delivery.lua")

local plan_review_host = {}

---@class PlanReviewHostOptions
---@field lines string[] the plan's source lines, one presentation row each
---@field session_id? string defaults to "plan-session"

---@class PendingPlanAnswer
---@field question_id string
---@field progress fun(value: table)?
---@field callback fun(result: table?, failure: string?)

---@param options PlanReviewHostOptions
function plan_review_host.start(options)
  local session_id = options.session_id or "plan-session"
  local host = { annotation = {}, answers = {}, saves = 0 }
  local client = require("forge.client")
  stub(client, "host_get_generation", function() return 1 end)
  stub(client, "host_is_accepting", function() return true end)
  local state = require("forge.session").harness
  state.session = { id = session_id, model = "mock-model", effort = "high" }
  state.transcript_win = vim.api.nvim_get_current_win()
  stub(package.loaded, "forge.views.harness.controller", { refresh_winbar = function() end, render = function() end,
    synchronize_state = function() end, present_plan_question = function() end, task_transition = function() end })
  host.plan = { id = "plan", state = "awaiting_review", document_version = 1, review_digest = "digest:1",
    working_path = temp.file("plan.md", options.lines) }

  local function presentation(document)
    local source_row, block = {}, {}
    for index, text in ipairs(options.lines) do
      local id = "plan:" .. index
      local metadata = { decoration = {}, editable_region = {}, target = {
        { id = id, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } } }
      source_row[index] = { id = id, target = id, text = text, source_line = index, block = id,
        position = { row = 0, column = 0 }, metadata = metadata }
      block[index] = { id = id, text = { text }, metadata = metadata }
    end
    return { version = 1, saved_source_digest = "saved:1", annotation = vim.deepcopy(host.annotation),
      source_row = source_row, snapshot = { document = document, revision = 0, block = block } }
  end

  mock_client.respond({
    ["session.document"] = function(spec, callback)
      local params = spec.params or {}
      if params.operation == "plan_open" then callback(presentation(params.document)) return end
      if params.operation == "plan_close" or params.operation == "plan_view" then callback({}) return end
      assert(params.operation == "plan_save_annotations", "unexpected plan document operation " .. tostring(params.operation))
      local replies = {}
      for _, annotation in ipairs(host.annotation) do replies[tostring(annotation.id)] = annotation.reply end
      host.annotation = vim.deepcopy(params.annotation)
      for _, annotation in ipairs(host.annotation) do
        local reply = replies[tostring(annotation.id)]
        if reply and reply.question_body == annotation.source.body then annotation.reply = reply end
      end
      host.saves = host.saves + 1
      callback({ saved = true })
    end,
    ["session.plan.question.answer"] = function(spec, callback, progress)
      local question_id = spec.params.question_ids[1]
      host.answers[#host.answers + 1] = { question_id = question_id, callback = callback, progress = progress }
      if progress then progress({ question_id = question_id, model = state.session.model, effort = state.session.effort }) end
    end,
  }, { wrap_callback = encode_delivery, copy_params = true })

  ---Opens the review through the native controller and returns `session.harness.plan_review`.
  function host.open()
    require("forge.views.plan_review.native_controller").open(host.plan)
    return assert(state.plan_review, "PlanReview did not open")
  end

  ---Completes the `index`th answer request with `body`, storing the reply as the host would.
  ---@param index integer
  ---@param body string
  function host.answer(index, body)
    local pending = assert(host.answers[index], "no answer request " .. index)
    for _, annotation in ipairs(host.annotation) do
      if tostring(annotation.id) == pending.question_id then
        annotation.reply = { question_body = annotation.source.body, body = body, duration_ms = 1000 }
      end
    end
    pending.callback({ annotation = vim.deepcopy(host.annotation) })
  end

  ---Fails the `index`th answer request with `message`.
  ---@param index integer
  ---@param message string
  function host.fail(index, message)
    assert(host.answers[index], "no answer request " .. index).callback(nil, message)
  end

  return host
end

return plan_review_host
