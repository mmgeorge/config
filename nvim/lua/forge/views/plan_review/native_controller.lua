local M = {}
local client = require("forge.client")
local session = require("forge.session")
local command_set = require("forge.shared.view_command_set")
local keymaps = require("forge.shared.keymaps")
local notifications = require("forge.infra.notifications")
local popup = require("forge.infra.popup_window")
local perf = require("forge.infra.perf")
local buffer = require("forge.buffer")

local function notice(message) notifications.error(message, "ForgePlanReview") end

---Explains why a review refused to close: `owner.close` refuses while a save, an answer, or a review
---edit it sent is still running, and closing then would drop that operation's result.
local busy_notice = "Plan review stays open until its running answers and edits finish."

---Reports the host's refusal of a review decision. The reason is the reviewer's only guide to what
---to change, so it is wrapped to the notifier's width rather than cut at the window edge, and it
---stays on screen longer than a routine notice; the notifier keeps it in its history.
---@param message string
local function report_refusal(message)
  local width = math.max(40, math.floor(vim.o.columns * 0.4) - 4)
  vim.notify(table.concat(require("forge.render.display_text").wrap(message, width), "\n"), vim.log.levels.ERROR,
    { title = "ForgePlanReview", timeout = 15000 })
end

---The newest model revision of each plan this editor has opened or announced, by session and plan.
---A plan record whose model revision is newer is a new submission; review edits keep the revision.
---@type table<string, integer>
local revision_seen = {}

---@param plan table
---@return string?
local function revision_key(plan)
  local current = session.harness.session
  return current and current.id and plan.id and (current.id .. "\0" .. plan.id) or nil
end

---@param review table
---@param on_projected fun()
local function toggle_fold(review, on_projected)
  local view = review.owner.current_view()
  if not view then return end
  if require("forge.draft_comments").toggle_thread_fold(review.buf, view.window) then return end
  require("forge.nodes").toggle_heading(review.owner.replica, view.window, {
    on_projected = on_projected,
  })
end

---@param review table
---@param focus_win? integer the window in the return tab to focus instead of the one the review left
---Show `review_buffer` in a new tab page whose window draws no number, sign, fold, or status
---column, and return that window. `tabnew` copies the window it runs in, usually a Harness transcript
---or composer whose columns belong to that view, and showing a buffer again restores the columns of
---the last window that showed it; either one placed a reopened review two columns to the right of
---where it first opened.
---@param review_buffer integer
---@return integer
local function open_review_tab(review_buffer)
  vim.cmd("tabnew")
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, review_buffer)
  require("forge.views.harness.layout").configure_gutterless_window(window)
  return window
end

local function hide_review(review, focus_win)
  if vim.api.nvim_tabpage_is_valid(review.tab) and review.tab ~= review.return_tab and vim.fn.tabpagenr("$") > 1 then
    vim.api.nvim_set_current_tabpage(review.tab)
    vim.cmd("tabclose")
  end
  if vim.api.nvim_tabpage_is_valid(review.return_tab) then
    if not (focus_win and vim.api.nvim_win_is_valid(focus_win) and vim.api.nvim_win_get_tabpage(focus_win) == review.return_tab) then
      focus_win = vim.api.nvim_win_is_valid(review.return_win) and review.return_win or nil
    end
    if focus_win then vim.api.nvim_set_current_win(focus_win) else vim.api.nvim_set_current_tabpage(review.return_tab) end
  end
end

---@param review table
---@param focus_win? integer
local function close_review(review, focus_win)
  if not review.owner.close() then notice(busy_notice) return false end
  hide_review(review, focus_win)
  if session.harness.plan_review == review
    and (not vim.api.nvim_buf_is_valid(review.buf) or not vim.bo[review.buf].modified) then
    session.harness.plan_review = nil
  end
  return true
end

local function discard_failed_attachment(review)
  if session.harness.plan_review == review then session.harness.plan_review = nil end
  if vim.api.nvim_tabpage_is_valid(review.tab) and review.tab ~= review.return_tab and vim.fn.tabpagenr("$") > 1 then
    vim.api.nvim_set_current_tabpage(review.tab)
    vim.cmd("tabclose")
  end
  if vim.api.nvim_tabpage_is_valid(review.return_tab) then
    vim.api.nvim_set_current_tabpage(review.return_tab)
    if vim.api.nvim_win_is_valid(review.return_win) then vim.api.nvim_set_current_win(review.return_win) end
  end
end

local function show_document(review, snapshot, title, filetype)
  if type(snapshot) ~= "table" then return end
  local view = review.owner.current_view()
  if not view then return end
  local rows = 0
  for _, block in ipairs(snapshot.block) do rows = rows + #block.text end
  local native_buffer, window = popup.open({ parent_win = view.window, width = math.max(20, math.min(80, vim.api.nvim_win_get_width(view.window) - 4)),
    height = math.max(1, math.min(rows, 24)), title = title, filetype = filetype or "markdown" })
  local replica = buffer.open(snapshot.document, { buffer = native_buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(native_buffer), filetype = filetype or "markdown", notice = notice })
  if buffer.apply_snapshot(replica, snapshot).kind ~= "Applied" then buffer.close(replica) return end
  local closed = false
  local function close()
    if closed then return end
    closed = true
    popup.close(window, true)
    buffer.close(replica)
  end
  for _, key in ipairs({ "q", "<Esc>" }) do vim.keymap.set("n", key, close, { buffer = native_buffer, silent = true }) end
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = native_buffer, once = true, callback = close })
end

local function effect(review, captured, fields)
  local view = review.owner.current_view()
  if not view or view.id ~= captured.view then return end
  require("forge.effects").apply(review.owner.replica, view, vim.tbl_extend("force", {
    id = "plan:" .. captured.action, document = captured.document, revision = captured.revision,
    view = captured.view, sequence = captured.sequence,
  }, fields))
end


local function refresh_winbar(review)
  local plan = review.plan
  local detail = plan.historical_revision and ("Revision " .. plan.historical_revision .. " • historical • read-only")
    or plan.state == "accepted" and "Accepted plan • read-only projection"
    or plan.state ~= "awaiting_review" and ("Revision " .. tostring(plan.model_revision or "?") .. " • "
      .. tostring(plan.state or "unavailable"):gsub("_", " ") .. " • decisions unavailable")
    or "Awaiting review"
  detail = detail .. " • Showing: " .. (review.public_only and "Public" or "All")
  if review.owner.show_tests then detail = detail .. " • Tests" end
  if review.owner.show_imports then detail = detail .. " • Imports" end
  local walkthrough = review.owner.walkthrough
  if walkthrough and walkthrough.total > 0 then
    detail = detail .. (" • Walkthrough %d/%d"):format(walkthrough.step, walkthrough.total)
  end
  keymaps.apply_view_winbar(review.win, "PlanReview", "plan_review", review.command_set, detail)
end

local function adopt_revision(review, result)
  if result.review_error then
    notice(result.review_error)
    -- The edit committed a new plan revision that this projection could not follow, so reload the
    -- review from the committed plan rather than leave it on the replaced revision.
    if type(result.plan) == "table" and result.plan.id == review.plan.id then
      vim.schedule(function()
        if session.harness.plan_review == review then M.open(result.plan) end
      end)
    end
    return false
  end
  local updated, failure = review.owner.update(result.plan, result.review)
  if not updated then notice(failure) return false end
  review.plan = result.plan
  refresh_winbar(review)
  return true
end

---Send one visibility action at a time and coalesce queued presses by action.
local function toggle_visibility(review, name)
  local pending = review.visibility_toggle or {}
  pending[name] = not pending[name] or nil
  if review.visibility_toggle then return end
  review.visibility_toggle = pending
  local function advance()
    local action = next(pending)
    if not action or review.owner.closed then review.visibility_toggle = nil return end
    pending[action] = nil
    local requested = review.owner.action(action, function(_, failure)
      if failure then notice(failure) end
    end, nil, function(_, failure)
      if failure then review.visibility_toggle = nil notice(failure) return end
      review.public_only = review.owner.public_only
      refresh_winbar(review)
      advance()
    end)
    if not requested then review.visibility_toggle = nil end
  end
  advance()
end

local function action(review, name)
  local walkthrough = name == "walkthrough_next" or name == "walkthrough_previous"
  local file_review = name == "viewed" or name == "unviewed"
  if review.cancel_definition then review.cancel_definition() review.cancel_definition = nil end
  if name == "toggle_visibility" or name == "toggle_tests" or name == "toggle_imports" then
    toggle_visibility(review, name) return
  end
  if name == "rename_entity" and (review.plan.historical_revision or review.plan.state ~= "awaiting_review") then
    notice("Only plans awaiting review can be renamed") return
  end
  if review.plan.historical_revision and (name == "comment" or name == "question" or name == "delete") then return end
  local opening_cursor = vim.api.nvim_win_get_cursor(review.win)
  local function receive(result, failure, captured)
    if (name == "rename_entity" or name == "jump_entity" or name == "open" or name:find("jump_symbol:", 1, true) == 1 or name == "references" or name:find("reveal_reference:", 1, true) == 1) and (not review.owner.is_current(captured)
        or not vim.api.nvim_win_is_valid(review.win)
        or vim.api.nvim_win_get_buf(review.win) ~= review.buf) then return end
    if failure then notice(failure) return end
    if walkthrough then
      if type(result.jump) == "table" then
        effect(review, captured, { kind = "cursor", jump = true, block = result.jump.block, position = result.jump.position })
      end
      refresh_winbar(review)
    elseif name == "toggle_declaration" then
      if result.jump then
        effect(review, captured, { kind = "cursor", block = result.jump.block, position = result.jump.position, viewport = result.viewport })
      end
    elseif file_review or name == "comment" or name == "question" or name == "delete" then
      return
    elseif name == "rename_entity" and type(result.rename) == "table" then
      require("forge.views.plan_review.entity_rename").proposed(review, result.rename, captured, function(name, finish)
        local queued = review.owner.submit("session.plan.entity.rename", { symbol = result.rename.symbol, name = name,
          expected_version = result.rename.expected_version }, function(renamed, failure)
          if failure then finish() notice(failure) return end
          local applied, update_failure = pcall(adopt_revision, review, renamed)
          finish()
          if not applied then notice(tostring(update_failure)) end
        end)
        if not queued then finish() end
      end)
    elseif name == "references" and type(result.references) == "table" then
      require("forge.views.plan_review.references").open(review, result.references, captured)
    elseif type(result.symbol_choices) == "table" then
      vim.ui.select(result.symbol_choices, { prompt = "Planned definition", format_item = function(choice) return choice.label end }, function(choice)
        if choice and review.owner.is_current(captured) and vim.api.nvim_win_is_valid(review.win)
          and vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), opening_cursor) then
          action(review, "jump_symbol:" .. choice.index)
        end
      end)
    elseif type(result.workspace_symbol) == "table" then
      review.cancel_definition = require("forge.views.plan_review.workspace_symbol").request(result.workspace_symbol,
        function() return review.owner.is_current(captured) and vim.api.nvim_win_is_valid(review.win)
          and vim.api.nvim_win_get_buf(review.win) == review.buf
          and vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), opening_cursor) end,
        function(target, lookup_failure, message)
          review.cancel_definition = nil
          if lookup_failure then notice(lookup_failure)
          elseif target then
            review.owner.highlight_symbol(captured, target.capture)
            effect(review, captured, { kind = "open_file", path = target.path, row = target.line - 1, column = target.column })
          elseif message and message ~= "cancelled" then notifications.info(message, "ForgePlanReview") end
        end)
    elseif type(result.workspace_definition) == "table" then
      review.cancel_definition = require("forge.views.plan_review.workspace_definition").request(result.workspace_definition,
        function() return review.owner.is_current(captured)
          and vim.api.nvim_win_is_valid(review.win)
          and vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), opening_cursor) end,
        function(target, lookup_failure, message)
          review.cancel_definition = nil
          if lookup_failure then notice(lookup_failure)
          elseif target then review.owner.definition(captured, result.workspace_definition, target, receive)
          elseif message and message ~= "cancelled" then notifications.info(message, "ForgePlanReview") end
        end)
    elseif type(result.message) == "string" then notifications.info(result.message, "ForgePlanReview")
    elseif name == "schema" then show_document(review, result.schema, "Canonical plan", "json")
    elseif name == "entity_info" then
      if type(result.info) == "table" then show_document(review, result.info, "Plan entity") else notifications.info("External references are unchecked during planning", "ForgePlanReview") end
    elseif (name == "jump_entity" or name == "open" or name:find("jump_symbol:", 1, true) == 1 or name:find("reveal_reference:", 1, true) == 1) and type(result.jump) == "table" then
      effect(review, captured, { kind = "cursor", jump = true, block = result.jump.block, position = result.jump.position })
    elseif name == "open" and type(result.anchor) == "table" and type(result.anchor.target) == "table"
      and result.anchor.target.target_type == "dependency" then
      require("forge.views.plan_review.dependency_browser").open(result.anchor.target.name)
    elseif type(result.source) == "table" then
      local file_heading = name == "open" and captured.block:match("^plan:design:file:[^:]+$") ~= nil
      effect(review, captured, { kind = "open_file", path = result.source.path, row = result.source.line - 1,
        column = result.source.column or 0, layout = file_heading and "tab" or nil })
    else notifications.info("This target has no captured source definition", "ForgePlanReview") end
  end
  review.owner.action(name, receive)
end

local function submit(review, method, params)
  if review.plan.historical_revision then return end
  local submitted_state = session.harness
  local controller = require("forge.views.harness.controller")
  controller.refresh_winbar()
  local queued = review.owner.submit(method, params, function(result, failure)
    if session.harness ~= submitted_state or not submitted_state.session
      or submitted_state.session.id ~= review.session_id then return end
    controller.refresh_winbar()
    -- The review stays open until the host answers, so a refusal leaves the reviewer where they
    -- were, with the reason.
    if failure then report_refusal(tostring(failure)) return end
    local current = session.harness.plan_review
    local completed = current and current.buf == review.buf and current.plan.id == review.plan.id
      and current.plan.review_digest == review.plan.review_digest
      and current.plan.historical_revision == review.plan.historical_revision and current or review
    local harness = session.harness
    local decision = method == "session.plan.accept" or method == "session.plan.changes.request"
    -- A decision starts a new exchange like a composer submission does, so it resumes following
    -- the transcript tail. Live follow is gated on composer focus, so the review hands focus
    -- straight to the composer rather than through the transcript window.
    if not completed.owner.closed then close_review(completed, decision and harness.composer_win or nil) end
    if decision and harness.presentation and harness.presentation.follow_tail then harness.presentation.follow_tail() end
    -- The read presents any question the decision raised. An acceptance made while the task is
    -- paused is only recorded (`awaiting_input` with the task still paused); once it leaves
    -- nothing to answer, the reviewer is told how to carry it out. Questions it raised are
    -- answered first, and the answer that completes them reports the same.
    local outcome = type(result) == "table" and type(result.outcome) == "table" and result.outcome.result
    local recorded = method == "session.plan.accept" and type(outcome) == "table" and outcome.disposition == "awaiting_input"
    controller.synchronize_state(recorded and function(_, sync_failure)
      if not sync_failure then require("forge.views.harness.task").report_recorded_acceptance(harness, harness.active_elicitation) end
    end or nil)
    controller.render()
  end)
end

local function update_checks(review, enabled, tests, receive, test_scopes)
  if review.plan.historical_revision or review.plan.state ~= "awaiting_review" then
    notice("Only plans awaiting review can change checks")
    if receive then receive(false) end
    return
  end
  -- The host decodes `enabled` and `test_scopes` as maps; `vim.json` encodes an empty table as an
  -- array, so an empty map must be sent as `vim.empty_dict()`.
  local function map(value) return value and next(value) and value or vim.empty_dict() end
  local queued = review.owner.submit("session.plan.check.update", { enabled = map(enabled), tests = tests or {}, test_scopes = map(test_scopes), expected_version = review.owner.version }, function(result, failure)
    local applied = false
    if failure then notice(failure) else applied = adopt_revision(review, result) end
    if receive then receive(applied) end
    return applied
  end)
end

local function delete_tests(review)
  if review.plan.historical_revision or review.plan.state ~= "awaiting_review" then
    notice("Only plans awaiting review can have tests deleted") return
  end
  local tests, captured, failure, checks = review.owner.selected_tests()
  if not tests or not captured then notice(failure or "Cannot capture selected tests") return end
  checks = checks or {}
  local title = next(checks) and ("Delete %d planned tests and disable %d checks?"):format(#tests, vim.tbl_count(checks))
    or ("Delete %d planned test%s?"):format(#tests, #tests == 1 and "" or "s")
  local lines = { title, "" }
  for index = 1, math.min(#tests, 20) do
    lines[#lines + 1] = vim.fn.strcharpart(tests[index].file .. ": " .. tests[index].name, 0, 110)
  end
  if #tests > 20 then lines[#lines + 1] = ("… and %d more"):format(#tests - 20) end
  for id in pairs(checks) do lines[#lines + 1] = "Check: " .. id end
  lines[#lines + 1] = ""
  lines[#lines + 1] = "Changes this plan only. Source files and project defaults are unchanged."
  require("forge.infra.confirm").open(lines, function()
    if not review.owner.is_current(captured, false) then notice("Plan changed while test deletion was awaiting confirmation") return end
    if next(checks) then update_checks(review, checks, tests) return end
    local queued = review.owner.submit("session.plan.test.delete", { tests = tests, expected_version = review.owner.version }, function(result, error)
      if error then notice(error) return end
      return adopt_revision(review, result)
    end)
  end, nil, { title = "Delete planned tests" })
end

local function prompt_decision(review, method, title)
  if review.plan.historical_revision or review.owner.closed then return end
  if review.plan.state ~= "awaiting_review" then
    -- A revision in progress replaces this document, so a decision here would
    -- address a plan the reviewer has not seen.
    notice("This plan revision is not awaiting review. Reopen the plan once the new revision is ready.")
    return
  end
  if review.decision_window and vim.api.nvim_win_is_valid(review.decision_window) then
    vim.api.nvim_set_current_win(review.decision_window)
    return
  end
  local tick = vim.api.nvim_buf_get_changedtick(review.buf)
  local digest, version = review.plan.review_digest, review.owner.version
  local plan_id, revision = review.plan.id, review.plan.model_revision
  local view = review.owner.current_view()
  if not view then return end
  local generation = client.host_get_generation()
  review.decision_window = require("forge.views.plan_review.decision").open({
    title = title, parent_win = view.window,
  }, function(comment)
    review.decision_window = nil
    if comment == nil then return end
    if session.harness.plan_review ~= review or review.owner.closed
      or client.host_get_generation() ~= generation or not session.harness.session
      or session.harness.session.id ~= review.session_id
      or not vim.api.nvim_buf_is_valid(review.buf)
      or tick ~= vim.api.nvim_buf_get_changedtick(review.buf)
      or digest ~= review.plan.review_digest or version ~= review.owner.version
      or plan_id ~= review.plan.id or revision ~= review.plan.model_revision
      or review.plan.historical_revision then
      notice("Plan changed while the review comment was open. Review it again before submitting.")
      return
    end
    submit(review, method, { comment = vim.trim(comment) })
  end)
end

local function commands(review)
  local set = command_set.new()
  command_set.register(set, "toggle", function() toggle_fold(review, function() action(review, "toggle_declaration") end) end)
  command_set.register(set, "visual_line_with_gutter", review.owner.gutter_selection.start)
  command_set.register(set, "delete_tests", function() delete_tests(review) end)
  command_set.register(set, "checks", function()
    require("forge.views.plan_review.checks").open(review, function(changes, receive) update_checks(review, changes.enabled or {}, nil, receive, changes.test_scopes) end)
  end)
  for _, name in ipairs({
    "open", "jump_entity", "references", "rename_entity", "entity_info", "schema",
    "comment", "question", "delete", "toggle_visibility", "toggle_tests", "toggle_imports",
    "viewed", "unviewed", "walkthrough_next", "walkthrough_previous",
  }) do
    command_set.register(set, name, function() action(review, name) end)
  end
  command_set.register(set, "save", function() if not review.plan.historical_revision then vim.cmd("write") end end)
  command_set.register(set, "accept", function()
    prompt_decision(review, "session.plan.accept", "Approval comment (optional)")
  end)
  command_set.register(set, "abort_plan", function()
    if not review.plan.historical_revision then submit(review, "session.plan.cancel", {}) end
  end)
  command_set.register(set, "request_changes", function()
    prompt_decision(review, "session.plan.changes.request", "Request changes comment (optional)")
  end)
  command_set.register(set, "close", function() close_review(review) end)
  command_set.register(set, "help", function() keymaps.show_view_help("plan_review", set, "PlanReview") end)
  return set
end

---Opens the review of `plan`. With `options.window`, the open review of the same plan reloads to
---`plan` in that window: the replaced owner hands its unsaved drafts to the new one, and no tab is
---created and focus does not move.
---@param plan table
---@param options? { window?: integer }
---@return boolean? opened false when the review could not be replaced
function M.open(plan, options)
  local open_started = perf.now()
  assert(type(plan) == "table" and type(plan.working_path) == "string", "PlanReview requires a physical plan path")
  local previous = session.harness.plan_review
  local same_plan = previous and previous.plan.id == plan.id
    and (previous.session_id == session.harness.session.id or previous.plan.working_path == plan.working_path)
  local recovery, rebase
  local window = options and options.window
  local key = revision_key(plan)
  if key and type(plan.model_revision) == "number" and not plan.historical_revision then
    revision_seen[key] = math.max(revision_seen[key] or 0, plan.model_revision)
  end
  if window then
    assert(previous and same_plan and vim.api.nvim_win_get_buf(window) == previous.buf,
      "an in-place reload replaces the open review of the same plan")
    if vim.bo[previous.buf].modified then
      local failure
      recovery, failure = previous.owner.recovery()
      if not recovery then notice(failure) return false end
      if not previous.owner.close(true) then return false end
      rebase = true
    elseif not previous.owner.close() then return false end
  elseif previous and same_plan and previous.plan.review_digest == plan.review_digest
    and previous.plan.historical_revision == plan.historical_revision and vim.api.nvim_buf_is_valid(previous.buf) then
    local refresh_execution = plan.state == "accepted" and not vim.bo[previous.buf].modified
    if refresh_execution and previous.owner.generation == client.host_get_generation() and previous.owner.attached() then
      if not close_review(previous) then return end
    end
    if not refresh_execution and previous.owner.generation == client.host_get_generation() and not previous.owner.closed and previous.owner.attached() then
      local window = vim.fn.win_findbuf(previous.buf)[1]
      if window then vim.api.nvim_set_current_win(window) else open_review_tab(previous.buf) end
      previous.win = vim.api.nvim_get_current_win()
      previous.tab = vim.api.nvim_get_current_tabpage()
      previous.owner.refresh_views()
      if not window then require("forge.draft_comments").fold_all_threads(previous.buf) end
      return
    end
    if not previous.owner.closed then
      local failure
      recovery, failure = previous.owner.recovery()
      if not recovery then notice(failure) return end
      previous.owner.close()
    end
  elseif previous and same_plan and not plan.historical_revision and not previous.plan.historical_revision
    and vim.api.nvim_buf_is_valid(previous.buf) and vim.bo[previous.buf].modified
    and previous.owner.generation == client.host_get_generation() and not previous.owner.closed and previous.owner.attached() then
    -- A newer revision replaces the reviewed one. Its unsaved drafts move onto the new revision
    -- by their source text, so the old owner releases the buffer once it has handed them over.
    local failure
    recovery, failure = previous.owner.recovery()
    if not recovery then notice(failure) return end
    if not previous.owner.close(true) then notice(busy_notice) return end
    rebase = true
    hide_review(previous)
  elseif previous and not close_review(previous) then return end
  local origin, origin_tab
  if window then
    origin, origin_tab = previous.return_win, previous.return_tab
  else
    origin = session.harness.transcript_win
    if not origin or not vim.api.nvim_win_is_valid(origin) then origin = vim.api.nvim_get_current_win() end
    origin_tab = vim.api.nvim_win_get_tabpage(origin)
  end
  local native_buffer = vim.fn.bufnr(plan.working_path)
  if native_buffer >= 0 and vim.api.nvim_buf_is_loaded(native_buffer) and vim.bo[native_buffer].modified and not recovery then
    notice("The physical plan buffer has unsaved changes") return
  end
  local retained = native_buffer >= 0 and vim.api.nvim_buf_is_loaded(native_buffer)
    and vim.bo[native_buffer].filetype == "ForgePlan"
  if native_buffer < 0 then
    native_buffer = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(native_buffer, plan.working_path)
  end
  -- The winbar announces loading while the next projection diffs against the retained text.
  if not recovery and not retained then
    vim.bo[native_buffer].buftype = "nofile"
    vim.bo[native_buffer].filetype = "ForgePlan"
    vim.bo[native_buffer].modifiable = true
    vim.api.nvim_buf_set_lines(native_buffer, 0, -1, false, { "Loading plan review…" })
    vim.bo[native_buffer].modified = false
  end
  if not recovery then vim.bo[native_buffer].modifiable = false end
  if window then
    if vim.api.nvim_win_get_buf(window) ~= native_buffer then vim.api.nvim_win_set_buf(window, native_buffer) end
  else
    window = open_review_tab(native_buffer)
  end
  vim.bo[native_buffer].bufhidden = "hide"
  vim.bo[native_buffer].swapfile = false
  local review = { plan = plan, buf = native_buffer, win = window, tab = vim.api.nvim_win_get_tabpage(window),
    return_win = origin, return_tab = origin_tab, session_id = session.harness.session.id }
  session.harness.plan_review = review
  local loading_commands = command_set.new()
  command_set.register(loading_commands, "close", function() close_review(review) end)
  keymaps.setup_view_keymaps(native_buffer, "plan_review", loading_commands)
  keymaps.apply_view_winbar(window, "PlanReview", "plan_review", loading_commands, "Loading review")
  review.owner = require("forge.views.plan_review.document").attach({ plan = plan, session_id = review.session_id,
    open_started = open_started, retained_buffer = retained,
    buffer = native_buffer, window = window, recovery = recovery, rebase = rebase, notice = notice,
    recovery_provider = recovery and not rebase and function() return assert(previous).owner.recovery() end or nil }, function(owner, failure)
    if failure then
      discard_failed_attachment(review)
      if recovery and not rebase then session.harness.plan_review = previous end
      if rebase and #recovery.annotation > 0 then
        -- The replaced review is closed, so the drafts survive only in this report.
        local lines = { failure, "", "Unsaved comments from the previous revision:" }
        for _, item in ipairs(recovery.annotation) do lines[#lines + 1] = "- " .. item.source.body end
        failure = table.concat(lines, "\n")
      end
      notice(failure)
      return
    end
    review.owner = owner
    review.public_only = owner.public_only
    local set = commands(review)
    review.command_set = set
    keymaps.setup_view_keymaps(native_buffer, "plan_review", set)
    refresh_winbar(review)
  end)
end

---Follows the session's plan record. Only a newer model revision, a new submission, concerns an
---open review: review edits keep the revision, and their own replies update the review. A review
---shown in a window reloads to the new revision there, carrying its unsaved drafts; a hidden or
---closed review stays out of the way, and one notice says the revision is ready to reopen, unless
---the task accepts plans automatically: the host then accepts the revision without the reviewer.
---Review items no prompt delivered before the revision replaced their document stay pending,
---and one notice names them: the host keeps those edits in `manual_changes` and the comments it
---could not place on the new revision in `unplaced_review_comments`.
---@param plan table?
function M.observe(plan)
  if type(plan) ~= "table" or type(plan.working_path) ~= "string" or plan.historical_revision
    or plan.state ~= "awaiting_review" or type(plan.model_revision) ~= "number" then return end
  local key = revision_key(plan)
  local seen = key and revision_seen[key]
  if not seen or plan.model_revision <= seen then return end
  revision_seen[key] = plan.model_revision
  local review = session.harness.plan_review
  local window = review and review.plan.id == plan.id and not review.plan.historical_revision and review.owner
    and not review.owner.closed and vim.api.nvim_buf_is_valid(review.buf)
    and (vim.api.nvim_win_is_valid(review.win) and vim.api.nvim_win_get_buf(review.win) == review.buf and review.win
      or vim.fn.win_findbuf(review.buf)[1])
  local task = require("forge.views.harness.task").current(session.harness)
  local accepted_automatically = task and task.auto_accept_plan and not (task.plan and task.plan.paused)
  if not (window and M.open(plan, { window = window }) ~= false) and not accepted_automatically then
    notifications.info(("Plan revision %d is ready · op"):format(plan.model_revision), "ForgePlanReview")
  end
  M.report_pending_review(plan)
end

local edit_description = {
  rename = function(edit) return ("Renamed %s to %s"):format(edit.from, edit.to) end,
  remove_test = function(edit) return ("Removed planned test %s::%s"):format(edit.file, edit.name) end,
  set_check = function(edit) return ("%s plan check %s"):format(edit.enabled and "Enabled" or "Disabled", edit.id) end,
  set_test_scope = function(edit) return ("Set plan check %s tests to %s"):format(edit.id, edit.scope) end,
}

---Names the review edits and comments a new model revision did not receive.
---@param plan table
function M.report_pending_review(plan)
  local lines = {}
  for _, edit in ipairs(type(plan.manual_changes) == "table" and plan.manual_changes or {}) do
    local describe = type(edit) == "table" and edit_description[edit.kind]
    lines[#lines + 1] = "- " .. (describe and describe(edit) or vim.inspect(edit))
  end
  for _, body in ipairs(type(plan.unplaced_review_comments) == "table" and plan.unplaced_review_comments or {}) do
    lines[#lines + 1] = "- Comment: " .. body
  end
  if #lines == 0 then return end
  notifications.warn(("Plan revision %d did not receive these review items, which stay pending and go with your next"
    .. " message or decision:\n%s"):format(plan.model_revision, table.concat(lines, "\n")), "ForgePlanReview")
end

return M
