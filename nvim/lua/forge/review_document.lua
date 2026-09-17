local M = {}
local buffer = require("forge.buffer")
local editable = require("forge.editable")
local input = require("forge.input")
local effects = require("forge.effects")
local log = require("forge.startup_log")
local runner_for_test
local finish_close
local dispatch_save
local load_actor
---@class ForgeReviewSave
---@field text table<string, string> Native field values accepted by the latest save action.
---@type table<string, table>
local retained = {}
local CACHE_LIMIT = 8

---@param state table
local function retain(state)
  if not state.cache_key then return end
  if not retained[state.cache_key] and vim.tbl_count(retained) >= CACHE_LIMIT then
    local oldest
    for _, candidate in pairs(retained) do
      if candidate.hidden and (not oldest or candidate.load_started < oldest.load_started) then oldest = candidate end
    end
    if oldest then finish_close(oldest, true) else state.cache_key = nil return end
  end
  retained[state.cache_key] = state
end

local function request(state, method, params, callback)
  local started = vim.uv.hrtime()
  log.write("pr.request", { load_id = state.load_id, document = state.document, method = method })
  local delivered = false
  local function receive(result, failure)
    if delivered then return end
    delivered = true
    local received = vim.uv.hrtime()
    vim.schedule(function()
      log.write("pr.response", { load_id = state.load_id, document = state.document, method = method,
        elapsed_us = math.floor((received - started) / 1000), schedule_us = math.floor((vim.uv.hrtime() - received) / 1000),
        failed = failure ~= nil, timing = type(result) == "table" and result.timing or nil })
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

---@param state table
local function revalidate(state, initial)
  if state.revalidating or state.initial_section_loading then return end
  state.revalidating = true
  local pending = 2
  local function complete(result, failure)
    if failure then state.notice(tostring(failure)) end
    for _, diagnostic in ipairs(result and result.diagnostic or {}) do state.notice(diagnostic) end
    pending = pending - 1
    if pending == 0 then state.revalidating = false M.refresh(state) end
  end
  local params = { document = state.document, directory = state.directory }
  request(state, "review.header", vim.tbl_extend("force", params, { initial = initial }), complete)
  request(state, "review.load", params, complete)
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

local function load_initial_sections(state)
  if not state.active or state.initial_section_loading then return end
  state.initial_section_loading = true
  request(state, "review.load", {
    document = state.document,
    directory = state.directory,
  }, function(result, failure)
    state.initial_section_loading = false
    state.initial_sections_complete = true
    log.write("pr.sections.ready", { load_id = state.load_id, document = state.document,
      elapsed_us = math.floor((vim.uv.hrtime() - state.load_started) / 1000) })
    if failure then failed(state, failure) end
    for _, diagnostic in ipairs(result and result.diagnostic or {}) do failed(state, diagnostic) end
    M.refresh(state)
  end)
end

finish_close = function(state, dispose)
  if not dispose and state.cache_key and retained[state.cache_key] == state
    and state.replica and vim.api.nvim_buf_is_loaded(state.replica.buffer) then
    state.closing, state.hidden = false, true
    if state.view then state.closed_view = state.view input.close(state.view) state.view = nil end
    if vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.replica.buffer
      and vim.api.nvim_buf_is_valid(state.origin) then vim.api.nvim_win_set_buf(state.window, state.origin) end
    return
  end
  if state.cache_key and retained[state.cache_key] == state then retained[state.cache_key] = nil end
  state.active = false
  if state.commands then state.commands.close() state.commands = nil end
  if state.view then input.close(state.view) end
  if state.group then pcall(vim.api.nvim_del_augroup_by_id, state.group) end
  if state.replica then
    buffer.close(state.replica, { preserve_buffer = state.unloading })
    if vim.api.nvim_win_is_valid(state.window) and vim.api.nvim_win_get_buf(state.window) == state.replica.buffer
      and vim.api.nvim_buf_is_valid(state.origin) then vim.api.nvim_win_set_buf(state.window, state.origin) end
    if not state.unloading and vim.api.nvim_buf_is_valid(state.replica.buffer) then pcall(vim.api.nvim_buf_delete, state.replica.buffer, { force = true }) end
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
  elseif state.submission_pending then M.submit_batched(state)
  elseif state.save_pending then dispatch_save(state)
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

---@param state table
---@return string?
function M.editable_region(state)
  local replica = state.replica
  if not replica or not replica.editable.native or replica.editable.fault
    or vim.api.nvim_get_current_buf() ~= replica.buffer then return nil end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local position = { row = cursor[1] - 1, column = math.min(cursor[2], #vim.api.nvim_get_current_line()) }
  return editable.guard_region(replica.editable, position, position)
end

---@param state table
function M.sync_editing(state)
  if not state.active or not state.replica or vim.api.nvim_get_current_buf() ~= state.replica.buffer then return end
  local region = M.editable_region(state)
  vim.bo[state.replica.buffer].modifiable = region ~= nil
  vim.b[state.replica.buffer].forge_reviewer_input = region == "reviewers"
  if state.commands then state.commands.sync_editing() end
end

local dirty_namespace = vim.api.nvim_create_namespace("ForgeReviewDirty")

---@param replica {buffer: integer, namespace: integer}
---@param row integer
---@return any[]? Extmark identity, position, and presentation details.
local function heading_mark(replica, row)
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace,
    { row, 0 }, { row, -1 }, { details = true })) do
    local chunks = mark[4].virt_text
    if chunks and #chunks == 1 and chunks[1][1]:match("^.-%*?:%s*$") then return mark end
  end
end

---@param state table
function M.sync_dirty(state)
  local replica = state.replica
  if not replica or not replica.editable.native or not state.fields then return end
  vim.api.nvim_buf_clear_namespace(replica.buffer, dirty_namespace, 0, -1)
  local modified = false
  local fields = vim.list_extend({}, state.fields)
  for _, comment in pairs(state.comment_by_region or {}) do
    if not comment.deleted and type(comment.baseline) == "string" then fields[#fields + 1] = comment end
  end
  for _, field in ipairs(fields) do
    local anchor = replica.editable.native.anchor[field.region]
    if anchor then
      local text = table.concat(vim.api.nvim_buf_get_text(replica.buffer, anchor.start.row, anchor.start.column,
        anchor.finish.row, anchor.finish.column, {}), "\n")
      local submitted = state.save_pending or state.saving
      local baseline = submitted and submitted.text[field.region] or field.baseline
      local dirty = text ~= baseline
      local heading = heading_mark(replica, anchor.start.row)
      if heading then
        local details = heading[4]
        local label = details.virt_text[1][1]:gsub("%*?:%s*$", "")
        local marked = dirty or state.editing_region == field.region
        vim.api.nvim_buf_set_extmark(replica.buffer, replica.namespace, heading[2], heading[3], {
          id = heading[1], virt_text = { { label .. (marked and "*: " or ": "), details.virt_text[1][2] } },
          virt_text_pos = "inline", hl_mode = "combine", priority = details.priority, right_gravity = false,
        })
      end
      if dirty then
        modified = true
        if not heading then
          local row = field.region == "body" and math.max(0, anchor.start.row - 1) or anchor.start.row
          local label = vim.api.nvim_buf_get_lines(replica.buffer, row, row + 1, false)[1]
          local colon = field.region == "body" and label:find(":", 1, true)
          vim.api.nvim_buf_set_extmark(replica.buffer, dirty_namespace, row, colon and colon - 1 or 0,
            { virt_text = { { colon and "*" or " *", "DiagnosticWarn" } }, virt_text_pos = colon and "inline" or "eol" })
        end
      end
    end
  end
  vim.bo[replica.buffer].modified = modified
end

function M.refresh(state, callback)
  if not state.active or not state.document then return end
  state.refresh_waiter = state.refresh_waiter or {}
  if callback then state.refresh_waiter[#state.refresh_waiter + 1] = callback end
  if state.saving or state.save_pending then state.refresh_pending = true return end
  if state.rendering then state.refresh_pending = true return end
  if state.replica and editable.suspend_generated_text(state.replica.editable) then
    state.refresh_pending = true
    return
  end
  state.refresh_pending, state.rendering = false, true
  local save_generation = state.save_generation
  local width = require("forge.width").capture(vim.api.nvim_win_is_valid(state.window) and state.window or vim.api.nvim_get_current_win())
  request(state, "review.materialize", { document = state.document, width = width }, function(delivery, failure)
    local apply_started = vim.uv.hrtime()
    state.rendering = false
    if failure or type(delivery) ~= "table" then
      if not state.refresh_pending then
        failed(state, failure or "Missing native review presentation")
        finish_refresh(state, false)
      end
      settled(state)
      return
    end
    if delivery.field and save_generation == state.save_generation then state.fields = delivery.field end
    local applied
    if state.replica.revision == nil then applied = buffer.apply_snapshot(state.replica, delivery.snapshot)
    elseif delivery.patch and delivery.patch ~= vim.NIL then applied = buffer.apply_patch(state.replica, delivery.patch)
    elseif delivery.snapshot and delivery.snapshot.revision == state.replica.revision then
      M.sync_editing(state)
      M.sync_dirty(state)
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
    if not state.shown and applied.kind == "Applied" then
      vim.bo[state.replica.buffer].buftype = "acwrite"
      state.shown = true
      retain(state)
      if vim.api.nvim_win_is_valid(state.window) and (vim.api.nvim_win_get_buf(state.window) == state.origin
        or vim.api.nvim_win_get_buf(state.window) == state.replica.buffer) then
        vim.api.nvim_win_set_buf(state.window, state.replica.buffer)
      end
      state.view = input.open(state.replica, state.window, { margin = 0, virtualedit = "", conceal = { level = 2, cursor = "" } })
      M.attach_fold_window(state, state.window)
      M.resize(state)
      if state.on_open then state.on_open(state) end
    end
    M.apply_new_default_folds(state)
    M.sync_editing(state)
    M.sync_dirty(state)
    log.write("pr.presentation.applied", { load_id = state.load_id, document = state.document,
      apply_us = math.floor((vim.uv.hrtime() - apply_started) / 1000),
      elapsed_us = math.floor((vim.uv.hrtime() - state.load_started) / 1000), rows = state.replica.row_count })
    log.watch_redraw(state.replica.buffer, state.document, state.load_started, "pr.redraw",
      { load_id = state.load_id, phase = state.initial_sections_complete and "sections" or "header" })
    finish_refresh(state, true)
    settled(state)
    if state.pending_snapshot then M.refresh_snapshot(state, state.pending_snapshot) end
  end)
end

function M.refresh_snapshot(state, snapshot)
  if not state.active then return end
  state.pending_snapshot = snapshot
  if not state.shown or state.rendering or state.snapshot_refreshing or state.revalidating then return end
  state.pending_snapshot, state.snapshot_refreshing = nil, true
  request(state, "review.header", { document = state.document, directory = state.directory, initial = snapshot }, function(_, failure)
    state.snapshot_refreshing = false
    if failure then failed(state, failure) end
    M.refresh(state)
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
    M.sync_editing(state)
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
  if not captured.target and captured.action ~= "browse" then return false end
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

---@param state table
dispatch_save = function(state)
  if not state.active or state.saving or not state.save_pending or not state.replica then return end
  if state.repository and not state.actor then load_actor(state) return end
  for login in (state.save_pending.text.reviewers or ""):gmatch("[^,%s]+") do
    if state.actor and login:gsub("^@", ""):lower() == state.actor:lower() then
      state.save_pending = nil
      M.sync_dirty(state)
      failed(state, "You cannot request a review from yourself (@" .. state.actor .. ")")
      return
    end
  end
  if not editable.flush(state.replica.editable) then
    state.save_pending = nil
    M.sync_dirty(state)
    failed(state, "Review edits could not be submitted")
    return
  end
  if editable.suspend_generated_text(state.replica.editable) then return end
  state.saving, state.save_pending = state.save_pending, nil
  request(state, "review.save", { document = state.document }, function(result, failure)
    local submitted = state.saving
    state.saving = nil
    state.save_generation = (state.save_generation or 0) + 1
    local snapshot = type(result) == "table" and type(result.snapshot) == "table" and result.snapshot
    local remote = type(result) == "table" and type(result.remote) == "table" and result.remote
    if snapshot and snapshot.field then state.fields = snapshot.field end
    if not failure and not snapshot then
      failure = "Missing native review save result"
    elseif snapshot and snapshot.uncertain then
      failure = failure or "Review save outcome requires reconciliation"
    elseif remote and remote.outcome ~= "confirmed" then
      failure = failure or remote.message or "Review save was rejected"
    end
    local uncertain = snapshot and snapshot.uncertain
      or failure and failure:find("requires reconciliation", 1, true) ~= nil
    if failure then
      state.save_pending = nil
      if not uncertain then failed(state, failure) end
    end
    M.sync_dirty(state)
    if uncertain then
      state.recovery_capture = submitted.text
      state.save_uncertain = true
      M.reconcile_save(state)
    elseif state.save_pending then
      state.refresh_pending = true
    else
      state.reconciling = true
      M.refresh(state, function() state.reconciling = false end)
    end
    settled(state)
  end)
end

---@param state table
load_actor = function(state)
  if state.actor_loading or not state.repository then return end
  state.actor_loading = true
  request(state, "github.actor", { directory = state.directory, repository = state.repository }, function(actor, failure)
    state.actor_loading = false
    if failure or type(actor) ~= "table" or type(actor.login) ~= "string" or actor.login == "" then
      state.save_pending = nil
      M.sync_dirty(state)
      failed(state, failure or "Missing authenticated GitHub user")
      return
    end
    state.actor = actor.login
    vim.b[state.replica.buffer].forge_reviewer_actor = actor.login
    settled(state)
  end)
end

---@param state table
function M.reconcile_save(state)
  if not state.active or state.save_recovering or state.saving then return end
  state.save_pending, state.save_recovering = nil, true
  request(state, "review.reconcile", { document = state.document }, function(snapshot, failure)
    state.save_recovering = false
    if failure or type(snapshot) ~= "table" or type(snapshot.field) ~= "table" or snapshot.uncertain then
      state.save_uncertain = true
      failed(state, (failure or "Review save remains unresolved") .. ". Press gR to retry recovery. Your edits are retained.")
      M.sync_dirty(state)
      return
    end
    state.save_uncertain = false
    state.failure = nil
    state.fields = snapshot.field
    state.save_generation = (state.save_generation or 0) + 1
    for _, field in ipairs(state.fields) do
      local captured = state.recovery_capture and state.recovery_capture[field.region]
      local anchor = state.replica.editable.native.anchor[field.region]
      if captured and anchor and field.baseline ~= captured then
        local text = table.concat(vim.api.nvim_buf_get_text(state.replica.buffer, anchor.start.row, anchor.start.column,
          anchor.finish.row, anchor.finish.column, {}), "\n")
        if text == captured then
          local modifiable = vim.bo[state.replica.buffer].modifiable
          vim.bo[state.replica.buffer].modifiable = true
          local ok, message = pcall(vim.api.nvim_buf_set_text, state.replica.buffer, anchor.start.row, anchor.start.column,
            anchor.finish.row, anchor.finish.column, vim.split(field.baseline, "\n", { plain = true }))
          vim.bo[state.replica.buffer].modifiable = modifiable
          if not ok then failed(state, tostring(message)) end
        end
      end
    end
    state.recovery_capture = nil
    M.sync_dirty(state)
    M.refresh(state)
    settled(state)
  end)
end

---@param state table
function M.save(state)
  if not state.active or not state.replica or not state.replica.editable.native then return end
  for _, field in ipairs(state.fields or {}) do
    if field.uncertain then state.save_uncertain = true end
  end
  if state.save_uncertain or state.save_recovering then M.reconcile_save(state) return end
  ---@type ForgeReviewSave
  local submitted = { text = {} }
  for _, field in ipairs(state.fields or {}) do
    local anchor = state.replica.editable.native.anchor[field.region]
    if anchor then
      local text, failure = editable.trim_trailing_newlines(state.replica.editable, field.region)
      if not text then failed(state, failure) return end
      submitted.text[field.region] = text
    end
  end
  state.editing_region = nil
  if not state.saving or not vim.deep_equal(submitted, state.saving) or state.save_pending then
    state.save_pending = submitted
  end
  M.sync_dirty(state)
  dispatch_save(state)
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
  if not state.submission_pending then
    for region in pairs(state.replica.editable.region) do
      if region ~= "title" and region ~= "body" and region ~= "reviewers" then
        local text, failure = editable.trim_trailing_newlines(state.replica.editable, region)
        if not text then failed(state, failure) return false end
      end
    end
    state.submission_pending = { callback = callback }
  end
  if not editable.flush(state.replica.editable) then
    state.submission_pending = nil
    failed(state, "Review submission edits could not be submitted")
    return false
  end
  if editable.suspend_generated_text(state.replica.editable) then return true end
  local pending = state.submission_pending
  state.submission_pending = nil
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
      if pending.callback then pending.callback(delivery, failure) end
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
  if action == "save" then
    local text, failure = editable.trim_trailing_newlines(state.replica.editable, comment.region)
    if not text then failed(state, failure) return false end
  end
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
    M.sync_editing(state)
    M.sync_dirty(state)
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
  local started = options.started_at or vim.uv.hrtime()
  local window = options.window or vim.api.nvim_get_current_win()
  local origin = vim.api.nvim_win_get_buf(window)
  local is_current = options.is_current
  local repository = options.repository or (options.target and options.target.repository)
  local cache_key = repository and repository.hostname and repository.owner and repository.name
    and vim.json.encode({ options.directory, repository.hostname, repository.owner, repository.name,
      options.number or (options.target and options.target.number) }) or nil
  local cached = cache_key and retained[cache_key]
  if cached and cached.active and cached.replica.status ~= "Closed" and vim.api.nvim_buf_is_loaded(cached.replica.buffer) then
    cached.window = window
    if origin ~= cached.replica.buffer then cached.origin = origin end
    cached.load_started, cached.load_id = started, tostring(started)
    cached.hidden, cached.closing = false, false
    cached.on_open = options.on_open
    if options.on_error then cached.notice = options.on_error end
    vim.api.nvim_win_set_buf(window, cached.replica.buffer)
    local previous_view = cached.view or cached.closed_view
    if previous_view then input.close(previous_view) end
    cached.view = input.open(cached.replica, window, { margin = 0, virtualedit = "", conceal = { level = 2, cursor = "" } })
    if previous_view then cached.view.id, cached.view.sequence = previous_view.id, previous_view.sequence end
    cached.closed_view, cached.view_ready = nil, false
    M.attach_fold_window(cached, window)
    M.resize(cached)
    log.write("pr.cache.hit", { load_id = cached.load_id, document = cached.document,
      elapsed_us = math.floor((vim.uv.hrtime() - started) / 1000) })
    log.watch_redraw(cached.replica.buffer, cached.document, started, "pr.redraw",
      { load_id = cached.load_id, phase = "cached" })
    if cached.on_open then cached.on_open(cached) end
    M.sync_editing(cached)
    M.sync_dirty(cached)
    revalidate(cached, options.initial)
    return cached
  end
  if cached then finish_close(cached, true) end
  local state = {
    active = true, directory = assert(options.directory), window = window,
    mode = "overview", number = options.number or (options.target and options.target.number),
    origin = origin, cache_key = cache_key, load_started = started, load_id = tostring(started),
    repository = repository,
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
  log.write("pr.open", { load_id = state.load_id, elapsed_us = math.floor((vim.uv.hrtime() - started) / 1000) })
  local params = options.target and { directory = state.directory, target = vim.deepcopy(options.target) }
    or { directory = state.directory, repository = vim.deepcopy(assert(options.repository)), number = assert(options.number), initial = vim.deepcopy(options.initial) }
  request(state, method, params, function(opened, failure)
    if failure or type(opened) ~= "table" or type(opened.document) ~= "string" then
      failed(state, failure or "Missing native review identity")
      finish_close(state)
      return
    end
    state.document = opened.document
    state.fields = opened.field
    state.save_uncertain = opened.uncertain == true
    request(state, "review.header", { document = state.document, directory = state.directory }, function(_, header_failure)
    if header_failure then
      failed(state, header_failure)
      finish_close(state)
      return
    end
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
          if edit.region == "body" then state.refresh_pending = true end
          M.sync_editing(state)
          M.sync_dirty(state)
          settled(state)
        end)
        return true
      end, notice = state.notice },
    })
    vim.bo[state.replica.buffer].buftype = "acwrite"
    vim.bo[state.replica.buffer].buflisted = true
    vim.api.nvim_buf_set_name(state.replica.buffer, "forge://pull-request/" .. state.document)
    if state.repository then load_actor(state) end
    if repository and repository.owner and repository.name then
      local repo = repository.owner .. "/" .. repository.name
      local cache = require("github.repo_cache")
      if not repository.hostname or repository.hostname == cache.hostname() then
        cache.enable_user_completion(state.replica.buffer, repo)
        cache.ensure_metadata(state.directory, repo)
        require("github.issue_index").ensure_repo(state.directory, repo, { manual = false })
      end
    end
    state.group = vim.api.nvim_create_augroup("ForgeReviewDocument" .. state.replica.buffer, { clear = true })
    vim.api.nvim_create_autocmd("BufWriteCmd", { group = state.group, buffer = state.replica.buffer, callback = function() M.save(state) end })
    vim.api.nvim_create_autocmd("BufUnload", { group = state.group, buffer = state.replica.buffer, callback = function() state.unloading = true finish_close(state, true) end })
    vim.api.nvim_create_autocmd("BufWinEnter", { group = state.group, buffer = state.replica.buffer,
      callback = function() M.attach_fold_window(state, vim.api.nvim_get_current_win()) end })
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter", "InsertEnter", "InsertLeave", "TextChanged", "TextChangedI" }, {
      group = state.group, buffer = state.replica.buffer,
      callback = function(event)
        M.sync_comment_focus(state)
        M.sync_editing(state)
        if event.event == "InsertEnter" then state.editing_region = M.editable_region(state) end
        if event.event == "InsertLeave" then state.editing_region = nil end
        if event.event == "InsertEnter" or event.event == "InsertLeave"
          or event.event == "TextChanged" or event.event == "TextChangedI" then M.sync_dirty(state) end
      end })
    vim.keymap.set("n", "i", function()
      local region = M.editable_region(state)
      local anchor = region and state.replica.editable.native.anchor[region]
      if anchor and heading_mark(state.replica, anchor.start.row) then
        return "0" .. (anchor.start.column > 0 and tostring(anchor.start.column) .. "l" or "") .. "i"
      end
      return "i"
    end, { buffer = state.replica.buffer, expr = true, desc = "Insert at field input" })
    vim.keymap.set("i", "<CR>", function()
      local region = M.editable_region(state)
      return (region == "title" or region == "reviewers") and "" or "<CR>"
    end, { buffer = state.replica.buffer, expr = true })
    for _, key in ipairs({ "o", "O" }) do
      vim.keymap.set("n", key, function()
        local region = M.editable_region(state)
        if region == "title" or region == "reviewers" then return "" end
        local anchor = region and state.replica.editable.native.anchor[region]
        if key == "o" and anchor and vim.api.nvim_win_get_cursor(0)[1] - 1 == anchor.finish.row then
          return "A<CR>"
        end
        return key
      end, { buffer = state.replica.buffer, expr = true })
    end
    vim.api.nvim_create_autocmd({ "VimResized", "WinResized" }, { group = state.group, callback = function() M.resize(state) end })
    state.bind_commands = function()
    if state.commands then state.commands.close() end
    local batched = state.mode == "batched"
    state.commands = require("forge.document_commands").attach(state.replica, {
      editable = function() return M.editable_region(state) ~= nil end,
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
        open = function() M.activate(state, "open") end,
        browse = function() M.activate(state, "browse") end,
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
    vim.keymap.set("n", "gR", function()
      if state.save_uncertain or state.fields and vim.iter(state.fields):any(function(field) return field.uncertain end) then
        M.reconcile_save(state)
      else M.reconcile_lifecycle(state) end
    end, { buffer = state.replica.buffer, desc = "Recover pull request save or lifecycle" })
    vim.keymap.set("n", "gB", function() pick_submission_recovery(state) end,
      { buffer = state.replica.buffer, desc = "Resolve review submission" })
    M.refresh(state, function(rendered)
      if rendered then
        load_initial_sections(state)
        if state.save_uncertain then M.reconcile_save(state) end
      end
    end)
    end)
  end)
  return state
end

function M._set_runner_for_test(runner)
  for _, state in pairs(retained) do finish_close(state, true) end
  runner_for_test = runner
end

return M
