local M = {}
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local input = require("forge.input")
local effects = require("forge.effects")
local runner_for_test
local finish_close

local function request(state, method, params, callback)
  local delivered = false
  local function receive(result, failure)
    if delivered then return end
    delivered = true
    vim.schedule(function()
      local current = state.active and (state.shown or not state.is_current or state.is_current())
      if not current and method ~= "review.close" then
        if state.active then finish_close(state) end
        if (method == "review.open_pr" or method == "review.open")
          and type(result) == "table" and type(result.document) == "string" and result.document ~= state.document then
          request(state, "review.close", { document = result.document }, function() end)
        end
        return
      end
      if method ~= "review.close" and not state.explicit_repository
        and require("github.repo_cache").hostname() ~= state.hostname then
        callback(nil, "GitHub hostname changed during review request")
      else callback(result, failure) end
    end)
  end
  local ok, failure = pcall(function()
    if runner_for_test then runner_for_test(method, params, receive)
    else require("forge.client").request_host(method, params, receive) end
  end)
  if not ok then receive(nil, tostring(failure)) end
end

local function failed(state, failure)
  state.failure = tostring(failure)
  state.notice(state.failure)
end

local function finish_refresh(state, ok)
  local waiter = state.refresh_waiter or {}
  state.refresh_waiter = {}
  for _, callback in ipairs(waiter) do callback(ok) end
end

local function load_next_initial_section(state)
  if not state.active or state.initial_section_loading then return end
  local section = table.remove(state.initial_sections or {}, 1)
  if not section then return end
  state.initial_section_loading = true
  request(state, "review.section", {
    document = state.document,
    directory = state.directory,
    section = section,
  }, function(_, failure)
    state.initial_section_loading = false
    if failure then
      failed(state, failure)
      load_next_initial_section(state)
      return
    end
    M.refresh(state, function(rendered)
      if rendered then load_next_initial_section(state) end
    end)
  end)
end

finish_close = function(state)
  state.active = false
  if state.commands then state.commands.close() state.commands = nil end
  if state.view then input.close(state.view) end
  if state.group then pcall(vim.api.nvim_del_augroup_by_id, state.group) end
  if state.replica then
    buffer.close(state.replica)
    if vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.replica.buffer
      and vim.api.nvim_buf_is_valid(state.origin) then vim.api.nvim_win_set_buf(state.window, state.origin) end
    if vim.api.nvim_buf_is_valid(state.replica.buffer) then pcall(vim.api.nvim_buf_delete, state.replica.buffer, { force = true }) end
  end
  if state.document then request(state, "review.close", { document = state.document }, function(_, failure)
    if failure then state.notice(failure) end
  end) end
end

local function settled(state)
  if not state.replica or editable.suspend_generated_text(state.replica.editable) then return end
  if state.effect_pending and state.replica.status == "Applied" then
    effects.apply(state.replica, state.view, state.effect_pending)
    state.effect_pending = nil
  end
  if state.closing then finish_close(state)
  elseif state.activation_pending and state.view_ready then M.activate(state)
  elseif state.comment_pending then M.comment(state)
  elseif state.save_pending then M.save(state)
  elseif state.refresh_pending then M.refresh(state) end
end

function M.attach_fold_window(state, window)
  if not state.active or not state.replica or not vim.api.nvim_win_is_valid(window)
    or vim.api.nvim_win_get_buf(window) ~= state.replica.buffer then return end
  require("forge.folds").attach(state.replica, window)
  state.fold_initialized = state.fold_initialized or {}
  state.fold_initialized[window] = state.fold_initialized[window] or {}
  for identity in pairs(state.replica.fold and state.replica.fold.record or {}) do
    state.fold_initialized[window][identity] = true
  end
end

function M.apply_new_default_folds(state)
  if not state.active or not state.replica or not state.replica.fold then return end
  state.fold_initialized = state.fold_initialized or {}
  for _, window in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do
    if vim.api.nvim_win_is_valid(window) then
      local known = state.fold_initialized[window] or {}
      state.fold_initialized[window] = known
      vim.api.nvim_win_call(window, function()
        for identity, record in pairs(state.replica.fold.record) do
          if not known[identity] and record.fold.closed then
            local _, start = state.replica.sequence:position(record.owner)
            vim.cmd(tostring(start + record.fold.start.row + 1) .. "foldclose")
          end
          known[identity] = true
        end
      end)
    end
  end
end

function M.refresh(state, callback)
  if not state.active or not state.document then return end
  state.refresh_waiter = state.refresh_waiter or {}
  if callback then state.refresh_waiter[#state.refresh_waiter + 1] = callback end
  if state.rendering then state.refresh_pending = true return end
  if state.replica and editable.suspend_generated_text(state.replica.editable) then
    state.refresh_pending = true
    return
  end
  state.refresh_pending, state.rendering = false, true
  local width = require("forge.width").capture(vim.api.nvim_win_is_valid(state.window) and state.window or vim.api.nvim_get_current_win())
  request(state, "review.materialize", { document = state.document, width = width }, function(delivery, failure)
    state.rendering = false
    if failure or type(delivery) ~= "table" then
      if not state.refresh_pending then
        failed(state, failure or "Missing native review presentation")
        finish_refresh(state, false)
      end
      settled(state)
      return
    end
    local applied
    if state.replica.revision == nil then applied = buffer.apply_snapshot(state.replica, delivery.snapshot)
    elseif delivery.patch and delivery.patch ~= vim.NIL then applied = buffer.apply_patch(state.replica, delivery.patch)
    elseif delivery.snapshot and delivery.snapshot.revision == state.replica.revision then
      finish_refresh(state, true)
      settled(state)
      return
    else
      failed(state, "Native review presentation has a missing revision")
      finish_refresh(state, false)
      return
    end
    if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then
      failed(state, applied.reason or applied.kind)
      finish_refresh(state, false)
      return
    end
    M.apply_new_default_folds(state)
    if not state.shown and applied.kind == "Applied" then
      state.shown = true
      if vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.origin then
        vim.api.nvim_win_set_buf(state.window, state.replica.buffer)
      end
      state.view = input.open(state.replica, state.window)
      M.attach_fold_window(state, state.window)
      M.resize(state)
      if state.on_open then state.on_open(state) end
    end
    finish_refresh(state, true)
    settled(state)
  end)
end

function M.resize(state)
  if not state.active or not state.view then return end
  local width = require("forge.width").capture(state.window)
  request(state, "review.view", { document = state.document, view = state.view.id, width = width }, function(delivery, failure)
    if failure then failed(state, failure) return end
    if type(delivery) == "table" and delivery.patch and delivery.patch ~= vim.NIL then
      local applied = buffer.apply_patch(state.replica, delivery.patch)
      if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then failed(state, applied.reason or applied.kind) return end
    end
    state.view_ready = true
    settled(state)
  end)
end

function M.activate(state, action)
  if not state.active or not state.view or state.action_running then return false end
  state.activation_pending = state.activation_pending or { cursor = vim.api.nvim_win_get_cursor(state.window), action = action or "activate" }
  if not editable.flush(state.replica.editable) then state.activation_pending = nil return false end
  if not state.view_ready or editable.suspend_generated_text(state.replica.editable) then return true end
  local pending = state.activation_pending
  state.activation_pending = nil
  if not vim.deep_equal(pending.cursor, vim.api.nvim_win_get_cursor(state.window)) then return false end
  local captured, failure = input.capture(state.replica, state.view, pending.action or "activate")
  if not captured then if failure then failed(state, failure) end return false end
  state.action_running = true
  request(state, "review.act", { input = captured, directory = state.directory }, function(delivery, action_failure)
    state.action_running = false
    if action_failure or type(delivery) ~= "table" or type(delivery.patch) ~= "table" then
      failed(state, action_failure or "Missing native review action delivery") return
    end
    for _, patch in ipairs(delivery.patch) do
      local applied = buffer.apply_patch(state.replica, patch)
      if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then failed(state, applied.reason or applied.kind) return end
    end
    if delivery.effect and delivery.effect ~= vim.NIL then state.effect_pending = delivery.effect end
    if delivery.comment and delivery.comment ~= vim.NIL then M.focus_comment(state, delivery.comment) end
    if delivery.diagnostic and delivery.diagnostic ~= vim.NIL then failed(state, delivery.diagnostic) end
    if delivery.refresh == true then M.read_section(state, "overview") end
    settled(state)
    if type(delivery.choice) == "table" and #delivery.choice > 0 then
      require("forge.infra.choice_popup").open({
        title = "PR #" .. tostring(state.number or "") .. " state", options = delivery.choice,
        on_choice = function(action)
          if action and state.active then M.activate(state, action) end
        end,
      })
    end
  end)
  return true
end

function M.save(state)
  if not state.active or state.saving or not state.replica then return end
  state.save_pending = true
  if not editable.flush(state.replica.editable) then failed(state, "Review edits could not be submitted") return end
  if editable.suspend_generated_text(state.replica.editable) then return end
  state.save_pending, state.saving = false, true
  request(state, "review.save", { document = state.document }, function(result, failure)
    state.saving = false
    if failure then failed(state, failure)
    elseif result and result.snapshot and result.snapshot.uncertain then
      failed(state, "Review save outcome requires reconciliation")
    elseif result and result.remote and result.remote ~= vim.NIL and result.remote.outcome ~= "confirmed" then
      failed(state, result.remote.message or "Review save was rejected")
    end
    M.refresh(state)
    settled(state)
  end)
end

local lifecycle_desired = { draft = "DRAFT", open = "OPEN", closed = "CLOSED" }

local function lifecycle_result(state, result, failure, callback)
  state.lifecycle_running = false
  if failure or type(result) ~= "table" then
    failed(state, failure or "Missing native lifecycle delivery")
    if callback then callback(nil, failure or state.failure) end
    return
  end
  state.lifecycle = result
  M.read_section(state, "overview")
  if callback then callback(result) end
  settled(state)
end

function M.transition(state, desired, callback)
  desired = lifecycle_desired[desired] or desired
  if not state.active or state.lifecycle_running or type(desired) ~= "string" then return false end
  if desired ~= "DRAFT" and desired ~= "OPEN" and desired ~= "CLOSED" then return false end
  if not editable.flush(state.replica.editable) then
    failed(state, "Review lifecycle change could not submit local edits")
    return false
  end
  if editable.suspend_generated_text(state.replica.editable) then return false end
  state.lifecycle_running = true
  request(state, "review.transition", { document = state.document, desired = desired }, function(result, failure)
    lifecycle_result(state, result, failure, callback)
  end)
  return true
end

function M.reconcile_lifecycle(state, callback)
  if not state.active or state.lifecycle_running then return false end
  state.lifecycle_running = true
  request(state, "review.lifecycle_reconcile", { document = state.document }, function(result, failure)
    lifecycle_result(state, result, failure, callback)
  end)
  return true
end

function M.resolve_lifecycle(state, operation_id, resolution, callback)
  if not state.active or state.lifecycle_running or type(operation_id) ~= "string" or type(resolution) ~= "table" then return false end
  state.lifecycle_running = true
  request(state, "review.lifecycle_resolve", {
    document = state.document, operation_id = operation_id, resolution = vim.deepcopy(resolution),
  }, function(result, failure)
    lifecycle_result(state, result, failure, callback)
  end)
  return true
end

function M.begin_batched(state, callback)
  if not state.active or not state.document or state.batched_running then return false end
  state.batched_running = true
  request(state, "review.begin_batched", { document = state.document }, function(delivery, failure)
    state.batched_running = false
    if failure or type(delivery) ~= "table" then
      failed(state, failure or "Missing native batched-review delivery")
      if callback then callback(nil, failure or state.failure) end
      return
    end
    state.mode, state.viewed_file = delivery.mode, delivery.viewed_file or {}
    if state.bind_commands then state.bind_commands() end
    M.refresh(state)
    if callback then callback(delivery) end
  end)
  return true
end

function M.set_viewed(state, path, viewed, callback)
  if not state.active or not state.document or state.batched_running or type(path) ~= "string" or type(viewed) ~= "boolean" then return false end
  state.batched_running = true
  request(state, "review.set_viewed", { document = state.document, path = path, viewed = viewed }, function(delivery, failure)
    state.batched_running = false
    if failure or type(delivery) ~= "table" then
      failed(state, failure or "Missing native viewed-state delivery")
      if callback then callback(nil, failure or state.failure) end
      return
    end
    state.mode, state.viewed_file = delivery.mode, delivery.viewed_file or {}
    M.refresh(state)
    if callback then callback(delivery) end
  end)
  return true
end

function M.set_viewed_active(state, viewed, callback)
  if not state.active_file_path then return false end
  return M.set_viewed(state, state.active_file_path, viewed, callback)
end

local function pick_verdict(state, callback)
  if state.verdict_provider then return state.verdict_provider(callback) end
  require("forge.infra.choice_popup").open({
    title = "Submit review",
    options = {
      { key = "c", value = "comment", label = "Comment (no verdict)" },
      { key = "a", value = "approve", label = "Approve" },
      { key = "r", value = "request_changes", label = "Request changes" },
    },
    on_choice = callback,
  })
end

function M.submit_batched(state, callback)
  if not state.active or not state.document or state.submission_running then return false end
  if not editable.flush(state.replica.editable) then return false end
  if editable.suspend_generated_text(state.replica.editable) then return false end
  pick_verdict(state, function(verdict)
    if not verdict or state.submission_running then return end
    state.submission_running = true
    request(state, "review.submit_batched", { document = state.document, verdict = verdict }, function(delivery, failure)
      state.submission_running = false
      if failure or type(delivery) ~= "table" then
        failed(state, failure or "Missing native review submission delivery")
      elseif delivery.outcome ~= "confirmed" then
        state.submission_recovery = delivery.operation_id
        failed(state, delivery.outcome == "outcome_unknown" and "Review outcome is unknown and requires recovery" or "Review submission was rejected")
      end
      M.refresh(state)
      if callback then callback(delivery, failure) end
    end)
  end)
  return true
end

function M.recover_batched_submission(state, resolution, callback)
  local operation_id = state.submission_recovery
  if not state.active or state.submission_running or type(operation_id) ~= "string" or type(resolution) ~= "table" then return false end
  state.submission_running = true
  request(state, "review.submit_batched_recover", {
    document = state.document, operation_id = operation_id, resolution = vim.deepcopy(resolution),
  }, function(delivery, failure)
    state.submission_running = false
    if failure or type(delivery) ~= "table" or type(delivery.submission) ~= "table" then
      failed(state, failure or "Missing native review recovery delivery")
    else
      state.submission_recovery = nil
      if delivery.fresh_required then failed(state, "Review outcome was closed as unknown. Refresh before submitting again.") end
      M.refresh(state)
    end
    if callback then callback(delivery, failure) end
  end)
  return true
end

local function pick_submission_recovery(state)
  if not state.submission_recovery then return end
  local popup = require("forge.infra.popup_window")
  local buffer, window = popup.open({ title = "Resolve review submission", relative = "editor", width = 42, height = 5, filetype = "ForgeReviewRecovery" })
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "l  Link confirmed review ID", "r  Mark not dispatched", "c  Close outcome as unknown", "q  Cancel" })
  vim.bo[buffer].modifiable = false
  local done = false
  local function finish(resolution)
    if done then return end
    done = true
    popup.close(window)
    if resolution then M.recover_batched_submission(state, resolution) end
  end
  vim.keymap.set("n", "l", function()
    popup.input({ prompt = "Confirmed review ID: " }, function(value)
      local remote_id = tonumber(value)
      if remote_id and remote_id > 0 and remote_id % 1 == 0 then finish({ resolution = "link", remote_id = remote_id }) end
    end)
  end, { buffer = buffer, nowait = true })
  vim.keymap.set("n", "r", function() finish({ resolution = "not_dispatched" }) end, { buffer = buffer, nowait = true })
  vim.keymap.set("n", "c", function() finish({ resolution = "close_unknown" }) end, { buffer = buffer, nowait = true })
  vim.keymap.set("n", { "q", "<Esc>" }, function() finish(nil) end, { buffer = buffer, nowait = true })
end

function M.focus_comment(state, comment)
  if type(comment) ~= "table" or type(comment.comment) ~= "number" or type(comment.region) ~= "string" then return false end
  state.comment_by_region = state.comment_by_region or {}
  state.comment_by_region[comment.region] = vim.deepcopy(comment)
  state.comment_focus = vim.deepcopy(comment)
  vim.schedule(function()
    if not state.active or not state.replica or state.comment_focus.region ~= comment.region then return end
    local native = state.replica.editable and state.replica.editable.native
    local anchor = native and native.anchor and native.anchor[comment.region]
    if not anchor or not vim.api.nvim_win_is_valid(state.window) then return end
    vim.api.nvim_win_call(state.window, function()
      if vim.fn.foldclosed(anchor.start.row + 1) ~= -1 then vim.cmd((anchor.start.row + 1) .. "foldopen") end
      vim.api.nvim_win_set_cursor(state.window, { anchor.start.row + 1, anchor.start.column })
    end)
  end)
  return true
end

function M.sync_comment_focus(state)
  if not state.active or not state.comment_by_region or not vim.api.nvim_win_is_valid(state.window) then return end
  local row = vim.api.nvim_win_get_cursor(state.window)[1] - 1
  local native = state.replica and state.replica.editable and state.replica.editable.native
  local anchor = native and native.anchor or {}
  for region, comment in pairs(state.comment_by_region) do
    local bounds = anchor[region]
    if bounds and row >= bounds.start.row and row <= bounds.finish.row then
      state.comment_focus = comment
      return
    end
  end
  state.comment_focus = nil
end

function M.save_comment(state, action, callback)
  local comment = state.comment_focus
  if not state.active or not comment or type(comment.comment) ~= "number" then return false end
  if action ~= "save" and action ~= "delete" then return false end
  return M.comment(state, { operation = "save", comment = comment.comment, action = action }, function(delivery, failure)
    if not failure and delivery and delivery.snapshot and delivery.snapshot ~= vim.NIL then
      if action == "delete" then
        state.comment_by_region[comment.region], state.comment_focus = nil, nil
      else M.focus_comment(state, delivery.snapshot) end
    end
    if callback then callback(delivery, failure) end
  end)
end

function M.reply_comment(state, callback)
  local comment = state.comment_focus
  if not state.active or not comment or type(comment.comment) ~= "number" then return false end
  return M.comment(state, { operation = "draft_reply", parent = comment.comment }, function(delivery, failure)
    if not failure and delivery and delivery.snapshot and delivery.snapshot ~= vim.NIL then M.focus_comment(state, delivery.snapshot) end
    if callback then callback(delivery, failure) end
  end)
end

function M.add_comment(state, callback)
  if not state.active or not state.replica or not state.view then return false end
  local cursor = vim.api.nvim_win_get_cursor(state.window)
  local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
  if location and location.target then return M.activate(state) end
  return M.comment(state, { operation = "draft_conversation" }, callback)
end

function M.comment(state, command, callback)
  if not state.active or not state.document or not state.replica then return false end
  if command then
    if state.comment_pending or state.comment_running then return false end
    state.comment_pending = { command = vim.deepcopy(command), callback = callback }
  end
  local pending = state.comment_pending
  if not pending or state.comment_running then return false end
  if not editable.flush(state.replica.editable) then
    state.comment_pending = nil
    failed(state, "Review comment edits could not be submitted")
    return false
  end
  if editable.suspend_generated_text(state.replica.editable) then return true end
  state.comment_pending, state.comment_running = nil, true
  request(state, "review.comment", { document = state.document, command = pending.command }, function(delivery, failure)
    state.comment_running = false
    if not failure and type(delivery) ~= "table" then failure = "Missing native comment delivery" end
    if not failure and delivery.patch and delivery.patch ~= vim.NIL then
      local applied = buffer.apply_patch(state.replica, delivery.patch)
      if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then failure = applied.reason or applied.kind end
    end
    if failure then failed(state, failure) end
    if not failure and delivery.snapshot and delivery.snapshot ~= vim.NIL
      and type(delivery.snapshot.region) == "string" then
      state.comment_by_region = state.comment_by_region or {}
      state.comment_by_region[delivery.snapshot.region] = vim.deepcopy(delivery.snapshot)
      if pending.command.operation == "draft_conversation" or pending.command.operation == "draft_inline"
        or pending.command.operation == "draft_reply" then
        M.focus_comment(state, delivery.snapshot)
      end
    end
    if pending.callback then pending.callback(delivery, failure) end
    settled(state)
  end)
    return true
end

function M.read_section(state, section, cursor)
  if not state.active or not state.document then return end
  request(state, "review.section", {
    document = state.document, directory = state.directory, section = section, cursor = cursor,
  }, function(_, failure)
    if failure then failed(state, failure) end
    M.refresh(state)
  end)
end

function M.read_file(state, path, continuation, callback)
  if not state.active or not state.document or not state.replica or not state.shown then return end
  local method = continuation and "review.file.more" or "review.file"
  local params = { document = state.document, path = path,
    directory = not continuation and state.directory or nil }
  request(state, method, params, function(delivery, failure)
    if failure or type(delivery) ~= "table" then
      failed(state, failure or "Missing native review file delivery")
      return
    end
    if delivery.patch and delivery.patch ~= vim.NIL then
      local applied = buffer.apply_patch(state.replica, delivery.patch)
      if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then
        failed(state, applied.reason or applied.kind)
        return
      end
    end
    if not continuation then state.active_file_path = path end
    if callback then callback(delivery) end
    settled(state)
  end)
end

function M.read_thread(state, thread_node_id, cursor, callback)
  if not state.active or not state.document or not state.replica then return end
  request(state, "review.thread", {
    document = state.document, directory = state.directory,
    thread_node_id = thread_node_id, cursor = cursor,
  }, function(delivery, failure)
    if failure or type(delivery) ~= "table" then
      failed(state, failure or "Missing native thread delivery")
      if callback then callback(nil, failure or state.failure) end
      return
    end
    if delivery.patch and delivery.patch ~= vim.NIL then
      local applied = buffer.apply_patch(state.replica, delivery.patch)
      if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then
        failed(state, applied.reason or applied.kind)
        if callback then callback(nil, state.failure) end
        return
      end
    else M.refresh(state) end
    if callback then callback(delivery) end
    settled(state)
  end)
end

function M.close(state)
  if not state.active or state.closing then return end
  state.closing = true
  if state.replica then
    editable.flush(state.replica.editable)
    if editable.suspend_generated_text(state.replica.editable) then return end
  end
  finish_close(state)
end

function M.open(options)
  local window = options.window or vim.api.nvim_get_current_win()
  local origin = vim.api.nvim_win_get_buf(window)
  local is_current = options.is_current
  local repository = options.repository or (options.target and options.target.repository)
  local state = {
    active = true, directory = assert(options.directory), window = window,
    mode = "overview", number = options.number or (options.target and options.target.number),
    origin = origin,
    hostname = repository and repository.hostname or require("github.repo_cache").hostname(),
    explicit_repository = repository ~= nil,
    is_current = function()
      return vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == origin
        and (not is_current or is_current())
    end,
    on_open = options.on_open,
    notice = options.on_error or function(message) vim.notify(message, vim.log.levels.ERROR, { title = "Forge review" }) end,
  }
  local method = options.target and "review.open_pr" or "review.open"
  local params = options.target and { directory = state.directory, target = vim.deepcopy(options.target) }
    or { directory = state.directory, repository = vim.deepcopy(assert(options.repository)), number = assert(options.number) }
  request(state, method, params, function(opened, failure)
    if failure or type(opened) ~= "table" or type(opened.document) ~= "string" then
      failed(state, failure or "Missing native review identity")
      finish_close(state)
      return
    end
    state.document = opened.document
    state.initial_sections = { "overview", "requested_reviewers", "files", "checks", "conversation", "commits" }
    state.replica = buffer.open(state.document, {
      filetype = "forge", notice = state.notice,
      recover = function() failed(state, "Review presentation requires explicit recovery") end,
      editable = { send = function(edit)
        local captured = { document = edit.document, region = edit.region, base = edit.base,
          sequence = edit.sequence, text = table.concat(edit.text, "\n") }
        request(state, "review.region_edit", captured, function(acknowledgement, edit_failure)
          if edit_failure or type(acknowledgement) ~= "table" or type(acknowledgement.patch) ~= "table" then
            failed(state, edit_failure or "Missing native review edit patch")
            return
          end
          local applied = buffer.acknowledge_edit(state.replica, acknowledgement, acknowledgement.patch)
          if applied.kind ~= "Applied" and applied.kind ~= "Deferred" then failed(state, applied.reason or applied.kind) return end
          settled(state)
        end)
        return true
      end, notice = state.notice },
    })
    vim.bo[state.replica.buffer].buftype = "acwrite"
    vim.bo[state.replica.buffer].buflisted = true
    state.group = vim.api.nvim_create_augroup("ForgeReviewDocument" .. state.replica.buffer, { clear = true })
    vim.api.nvim_create_autocmd("BufWriteCmd", { group = state.group, buffer = state.replica.buffer, callback = function() M.save(state) end })
    vim.api.nvim_create_autocmd("BufUnload", { group = state.group, buffer = state.replica.buffer, callback = function() M.close(state) end })
    vim.api.nvim_create_autocmd("BufWinEnter", { group = state.group, buffer = state.replica.buffer,
      callback = function() M.attach_fold_window(state, vim.api.nvim_get_current_win()) end })
    vim.api.nvim_create_autocmd("CursorMoved", { group = state.group, buffer = state.replica.buffer,
      callback = function() M.sync_comment_focus(state) end })
    vim.api.nvim_create_autocmd({ "VimResized", "WinResized" }, { group = state.group, callback = function() M.resize(state) end })
    state.bind_commands = function()
    if state.commands then state.commands.close() end
    local batched = state.mode == "batched"
    state.commands = require("forge.document_commands").attach(state.replica, {
      view = batched and "review" or "pr", title = (batched and "Review #" or "PR #") .. tostring(state.number or ""),
      narrow_title = "< #" .. tostring(state.number or ""),
      changed = function()
        local cursor = vim.api.nvim_win_get_cursor(state.window)
        local location = buffer.locate(state.replica, cursor[1] - 1, cursor[2])
        if vim.fn.foldclosed(".") < 0 and location and location.target then M.activate(state, "expand") end
      end,
      handler = {
        close = function() M.close(state) end,
        refresh = function() M.refresh(state) end,
        open = function() M.activate(state) end,
        browse = function() M.activate(state) end,
        review = function() M.begin_batched(state) end,
        reply = function() if not M.reply_comment(state) then M.refresh(state) end end,
        comment = function()
          M.add_comment(state, function(delivery, failure)
            if not failure and delivery and delivery.snapshot and delivery.snapshot ~= vim.NIL then M.focus_comment(state, delivery.snapshot) end
          end)
        end,
        sync = function()
          if not M.save_comment(state, "save") then M.save(state) end
        end,
        delete = function() M.save_comment(state, "delete") end,
        viewed = function() M.set_viewed_active(state, true) end,
        unviewed = function() M.set_viewed_active(state, false) end,
        submit = function() M.submit_batched(state) end,
      },
    })
    end
    state.bind_commands()
    vim.keymap.set("n", "R", function()
      if not M.reply_comment(state) then M.refresh(state) end
    end, { buffer = state.replica.buffer, desc = "Reply to selected review comment or refresh" })
    vim.keymap.set("n", "gR", function() M.reconcile_lifecycle(state) end,
      { buffer = state.replica.buffer, desc = "Reconcile pull request lifecycle" })
    vim.keymap.set("n", "gB", function() pick_submission_recovery(state) end,
      { buffer = state.replica.buffer, desc = "Resolve review submission" })
    M.refresh(state, function(rendered)
      if rendered then load_next_initial_section(state) end
    end)
  end)
  return state
end

function M._set_runner_for_test(runner)
  runner_for_test = runner
end

return M
