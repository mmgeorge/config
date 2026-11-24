-- A mock plan host for PlanReview cases that need review state without the real Forge host. It
-- answers `forge.client` for the rest of the current case: `plan_open` presents the plan lines and
-- every annotation saved so far with its answer, `plan_save_annotations` stores the annotations,
-- each `session.plan.question.answer` request waits in `answers` until the case completes it, and
-- each `plan_action` visibility toggle waits in `actions` until `complete_action` answers it the way
-- the real host does: an input captured at an older document revision is rejected, and an
-- accepted toggle returns a patch that advances the revision. A `rename_entity` action answers at
-- once with `options.rename`, the host's rename proposal and its preview. Each review decision and
-- each review edit (`session.plan.accept`, `session.plan.changes.request`, `session.plan.cancel`,
-- `session.plan.check.update`, `session.plan.entity.rename`) waits in `requests` until the case
-- answers its callback.
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
---@field markdown? boolean mark the fixture's source rows as Markdown
---@field session_id? string defaults to "plan-session"
---@field rename? table the `rename` result of a `rename_entity` action

---@class PendingPlanAnswer
---@field question_id string
---@field progress fun(value: table)?
---@field callback fun(result: table?, failure: string?)

---@param options PlanReviewHostOptions
function plan_review_host.start(options)
  local session_id = options.session_id or "plan-session"
  local host = { annotation = {}, answers = {}, actions = {}, requests = {}, saves = 0, revision = 0, public_only = true,
    version = 1 }
  local client = require("forge.client")
  stub(client, "host_get_generation", function() return 1 end)
  stub(client, "host_is_accepting", function() return true end)
  local state = require("forge.session").harness
  state.session = { id = session_id, model = "mock-model", effort = "high" }
  state.transcript_win = vim.api.nvim_get_current_win()
  stub(package.loaded, "forge.views.harness.controller", { refresh_winbar = function() end, render = function() end,
    synchronize_state = function() end, present_plan_question = function() end, task_transition = function() end })
  host.plan = { id = "plan", state = "awaiting_review", model_revision = 1, document_version = 1, review_digest = "digest:1",
    working_path = temp.file("plan.md", options.lines) }

  local function presentation(document)
    local source_row, block = {}, {}
    for index, text in ipairs(options.lines) do
      local id = "plan:" .. index
      local metadata = { decoration = {}, editable_region = {}, markdown = options.markdown, target = {
        { id = id, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } } }
      source_row[index] = { id = id, target = id, text = text, source_line = index, block = id,
        position = { row = 0, column = 0 }, metadata = metadata }
      block[index] = { id = id, text = { text }, metadata = metadata }
    end
    return { version = host.version, saved_source_digest = "saved:" .. host.version, annotation = vim.deepcopy(host.annotation),
      public_only = host.public_only, source_row = source_row,
      snapshot = { document = document, revision = host.revision, block = block } }
  end

  local handler = {
    ["session.document"] = function(spec, callback)
      local params = spec.params or {}
      if params.operation == "plan_open" then
        host.revision = 0
        callback(presentation(params.document))
        return
      end
      if params.operation == "plan_action" and params.input.action == "rename_entity" and options.rename then
        callback({ rename = vim.tbl_extend("force", options.rename, { expected_version = host.version }) })
        return
      end
      if params.operation == "plan_action" then
        assert(params.input.action == "toggle_visibility", "unexpected plan action " .. tostring(params.input.action))
        host.actions[#host.actions + 1] = { input = params.input, callback = callback }
        return
      end
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
  }
  for _, method in ipairs({ "session.plan.accept", "session.plan.changes.request", "session.plan.cancel",
    "session.plan.check.update", "session.plan.entity.rename" }) do
    handler[method] = function(spec, callback)
      host.requests[#host.requests + 1] = { method = method, params = spec.params, callback = callback }
    end
  end
  mock_client.respond(handler, { wrap_callback = encode_delivery, copy_params = true })

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

  ---Answers the `index`th visibility toggle as the host would, rejecting an input captured at an
  ---older revision.
  ---@param index integer
  function host.complete_action(index)
    local pending = assert(host.actions[index], "no plan action " .. index)
    if pending.input.revision ~= host.revision then
      pending.callback(nil, "plan review input revision changed")
      return
    end
    host.public_only = not host.public_only
    host.revision = host.revision + 1
    local result = presentation(pending.input.document)
    result.patch = {}
    pending.callback(result)
  end

  ---Submits a new plan revision with `lines`, as a model revision would: the plan record gets a new
  ---review digest and the next `plan_open` presents the new lines. Saved annotations are kept, as
  ---the host keeps them on rows whose text survives.
  ---@param lines string[]
  ---@return table plan the new plan record
  function host.revise(lines)
    options.lines = lines
    host.version = host.version + 1
    host.plan = vim.tbl_extend("force", host.plan, { model_revision = host.plan.model_revision + 1,
      document_version = host.version, review_digest = "digest:" .. host.version })
    return host.plan
  end

  ---Answers the `index`th waiting decision or edit request. `failure` refuses it with that reason;
  ---otherwise an edit commits as `host.edit` does and replies with the edited plan and its review,
  ---and a decision replies with the plan.
  ---@param index integer
  ---@param failure? string
  function host.complete_request(index, failure)
    local pending = assert(host.requests[index], "no plan request " .. index)
    if failure then pending.callback(nil, failure) return end
    if pending.method == "session.plan.check.update" or pending.method == "session.plan.entity.rename" then
      pending.callback({ plan = host.edit(), review = presentation(pending.params.review.document) })
      return
    end
    pending.callback({ plan = host.plan })
  end

  ---Commits a review edit, such as a check toggle or a rename, as the host does: the plan record
  ---gets a new document version and review digest within the same model revision.
  ---@return table plan the edited plan record
  function host.edit()
    host.version = host.version + 1
    host.plan = vim.tbl_extend("force", host.plan, { document_version = host.version,
      review_digest = "digest:" .. host.version })
    return host.plan
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
