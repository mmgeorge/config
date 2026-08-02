--- Reconciles optimistic status projections with path-scoped authoritative Git snapshots.
local M = {}

local diff_buffer = require("diff_review.views.diff_buffer")
local diff_source_state = require("diff_review.views.status.diff_source_state")
local git_data = require("diff_review.git.git_data")
local mutation_coordinator = require("diff_review.git.mutation_coordinator")
local notifications = require("diff_review.infra.notifications")
local operation_journal = require("diff_review.views.status.operation_journal")
local paths = require("diff_review.infra.paths")
local section_map = require("diff_review.views.status.section_map")
local session = require("diff_review.session")
local source = require("diff_review.render.source")
local source_loader = require("diff_review.render.source_loader")
local status_snapshot = require("diff_review.git.status_snapshot")
local syntax_engine = require("diff_review.render.syntax_engine")

-- status_render reaches actions during module load, so keep this back edge lazy.
local function status_render() return require("diff_review.views.status.status_render") end

---@class DiffReviewStatusCacheSelection
---@field kind "file"|"hunk"
---@field filename string
---@field diff? string

---@class DiffReviewStatusCacheLayer
---@field burst_id integer
---@field target_section DiffReviewStatusStageSectionName
---@field path_list string[]
---@field selection_list DiffReviewStatusCacheSelection[]

---@class DiffReviewStatusSyncRootState
---@field root string
---@field cache_layer_list DiffReviewStatusCacheLayer[]
---@field configured boolean
---@field verification_stale boolean

---@type table<string, DiffReviewStatusSyncRootState>
local root_state_by_key = {}

local snapshot_attempt_limit = 2
local snapshot_retry_delay_ms = 120

---@param root string
---@return string
local function root_key(root)
  local normalized = paths.normalize_path(root)
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

---@param root string
---@return DiffReviewStatusSyncRootState
local function root_state(root)
  local key = root_key(root)
  local state = root_state_by_key[key]
  if state then return state end
  state = {
    root = root,
    cache_layer_list = {},
    configured = false,
    verification_stale = false,
  }
  root_state_by_key[key] = state
  return state
end

---@param root string
---@return table[]
local function status_state_list(root)
  local target_root = root_key(root)
  local state_list = {}
  local seen_state = {}
  for _, status in pairs(session.states or {}) do
    if status
      and status.sections
      and status.cwd
      and root_key(status.cwd) == target_root
      and (status.view_kind == "status" or status == session.main_status)
      and not seen_state[status]
    then
      seen_state[status] = true
      state_list[#state_list + 1] = status
    end
  end
  if session.main_status
    and session.main_status.sections
    and session.main_status.cwd
    and root_key(session.main_status.cwd) == target_root
    and not seen_state[session.main_status]
  then
    state_list[#state_list + 1] = session.main_status
  end
  return state_list
end

---@param status table
---@return DiffReviewOperationJournal
local function ensure_journal(status)
  status.operation_journal = status.operation_journal or operation_journal.new(status.sections or {})
  return status.operation_journal
end

---@param status table
local function render_current_projection(status)
  if not (status.buf and vim.api.nvim_buf_is_valid(status.buf)) then return end
  local previous_status = session.status
  session.status = status
  status_render().status_render_current_model(nil, { restore_cursor = false })
  session.status = previous_status
end

---@param root string
---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusStageSectionName
---@return DiffReviewStatusCacheLayer
local function capture_cache_layer(root, entries, target_section)
  local path_set = {}
  local selection_list = {}
  for _, entry in ipairs(entries or {}) do
    local filename = entry.file and entry.file.filename or nil
    if filename and filename ~= "" then
      local relpath = paths.repo_relative(filename, root)
      if relpath then path_set[(relpath:gsub("\\", "/"))] = true end
      selection_list[#selection_list + 1] = {
        kind = entry.kind == "hunk" and "hunk" or "file",
        filename = filename,
        diff = entry.kind == "hunk" and entry.hunk and entry.hunk.diff or nil,
      }
    end
  end
  local path_list = {}
  for path in pairs(path_set) do path_list[#path_list + 1] = path end
  table.sort(path_list)
  return {
    burst_id = 0,
    target_section = target_section,
    path_list = path_list,
    selection_list = selection_list,
  }
end

---@param layer DiffReviewStatusCacheLayer
local function apply_cache_layer(layer)
  session.file_diffs = session.file_diffs or {}
  session.file_hunk_staged = session.file_hunk_staged or {}
  local target_staged = layer.target_section == "staged"
  for _, selection in ipairs(layer.selection_list) do
    local diff_text = session.file_diffs[selection.filename]
    if diff_text and diff_text ~= "" then
      local hunk_list = git_data._parse_diff(diff_text, false)
      local staged_flag_list = vim.deepcopy(session.file_hunk_staged[selection.filename] or {})
      for hunk_index = 1, #hunk_list do
        if staged_flag_list[hunk_index] == nil then staged_flag_list[hunk_index] = false end
      end
      if selection.kind == "file" then
        for hunk_index = 1, #hunk_list do staged_flag_list[hunk_index] = target_staged end
      elseif selection.diff then
        for hunk_index, hunk in ipairs(hunk_list) do
          if hunk.diff == selection.diff and staged_flag_list[hunk_index] ~= target_staged then
            staged_flag_list[hunk_index] = target_staged
            break
          end
        end
      end
      session.file_hunk_staged[selection.filename] = #staged_flag_list > 0 and staged_flag_list or nil
    end
  end
end

---@param file_list string[]
---@return table<string, { diff: string|boolean|nil, flag_list: boolean[]? }>
local function capture_cache(file_list)
  local cache = {}
  for _, filename in ipairs(file_list or {}) do
    cache[filename] = {
      diff = session.file_diffs and session.file_diffs[filename] or nil,
      flag_list = vim.deepcopy(session.file_hunk_staged and session.file_hunk_staged[filename] or nil),
    }
  end
  return cache
end

---@param before table<string, { diff: string|boolean|nil, flag_list: boolean[]? }>
---@param file_list string[]
---@return string[]
local function changed_cache_file_list(before, file_list)
  local changed_file_list = {}
  for _, filename in ipairs(file_list or {}) do
    local previous = before[filename] or {}
    local next_diff = session.file_diffs and session.file_diffs[filename] or nil
    local next_flag_list = session.file_hunk_staged and session.file_hunk_staged[filename] or nil
    if previous.diff ~= next_diff or not vim.deep_equal(previous.flag_list, next_flag_list) then
      changed_file_list[#changed_file_list + 1] = filename
    end
  end
  return changed_file_list
end

---@param snapshot DiffReviewPathStatusSnapshot
local function apply_snapshot_cache(snapshot)
  session.file_diffs = session.file_diffs or {}
  session.file_hunk_staged = session.file_hunk_staged or {}
  session.untracked = session.untracked or {}
  if snapshot.full_repository then
    session.file_diffs = vim.deepcopy(snapshot.file_diffs or {})
    session.file_hunk_staged = vim.deepcopy(snapshot.file_hunk_staged or {})
    session.untracked = vim.deepcopy(snapshot.untracked_by_file or {})
    return
  end
  for _, filename in ipairs(snapshot.affected_file_list or {}) do
    session.file_diffs[filename] = snapshot.file_diffs[filename]
    if session.file_diffs[filename] == nil then session.file_diffs[filename] = false end
    session.file_hunk_staged[filename] = vim.deepcopy(snapshot.file_hunk_staged[filename])
    session.untracked[filename] = snapshot.untracked_by_file[filename]
  end
end

---@param state DiffReviewStatusSyncRootState
---@param resolved_burst_id_set table<integer, boolean>
local function retire_cache_layers(state, resolved_burst_id_set)
  local remaining_layer_list = {}
  for _, layer in ipairs(state.cache_layer_list) do
    if not resolved_burst_id_set[layer.burst_id] then
      remaining_layer_list[#remaining_layer_list + 1] = layer
    end
  end
  state.cache_layer_list = remaining_layer_list
end

---@param state DiffReviewStatusSyncRootState
local function replay_cache_layers(state)
  for _, layer in ipairs(state.cache_layer_list) do apply_cache_layer(layer) end
end

---@param state DiffReviewStatusSyncRootState
---@param burst_id integer
local function reverse_cache_layers_for_burst(state, burst_id)
  for layer_index = #state.cache_layer_list, 1, -1 do
    local layer = state.cache_layer_list[layer_index]
    if layer.burst_id == burst_id then
      apply_cache_layer({
        burst_id = layer.burst_id,
        target_section = layer.target_section == "staged" and "unstaged" or "staged",
        path_list = layer.path_list,
        selection_list = layer.selection_list,
      })
    end
  end
end

---@class DiffReviewCompletedMutationGroup
---@field target_section DiffReviewStatusStageSectionName
---@field entry_list DiffReviewStatusEntry[]

---@param burst DiffReviewMutationBurst
---@return DiffReviewCompletedMutationGroup[]
local function completed_mutation_group_list(burst)
  local group_list = {}
  for _, task in ipairs(burst.tasks or {}) do
    local metadata = task.metadata or {}
    local recovery_entry_list = metadata.recovery_entry_list or {}
    local result = task.result
    local completed_count = tonumber(result and result.count) or 0
    if result and result.ok then completed_count = #recovery_entry_list end
    if completed_count > 0 and metadata.target_section then
      local entry_list = {}
      for entry_index = 1, math.min(completed_count, #recovery_entry_list) do
        entry_list[#entry_list + 1] = recovery_entry_list[entry_index]
      end
      if #entry_list > 0 then
        group_list[#group_list + 1] = {
          target_section = metadata.target_section,
          entry_list = entry_list,
        }
      end
    end
  end
  return group_list
end

---@param state DiffReviewStatusSyncRootState
---@param burst DiffReviewMutationBurst
---@return string[] path_list
---@return table<integer, boolean> resolved_burst_id_set
local function synchronization_scope(state, burst)
  local path_set = {}
  local resolved_burst_id_set = {}
  for _, path in ipairs(mutation_coordinator.paths(burst)) do path_set[path] = true end
  resolved_burst_id_set[burst.id] = true
  for _, layer in ipairs(state.cache_layer_list) do
    if layer.burst_id <= burst.id then
      resolved_burst_id_set[layer.burst_id] = true
      for _, path in ipairs(layer.path_list) do path_set[path] = true end
    end
  end
  local path_list = {}
  for path in pairs(path_set) do path_list[#path_list + 1] = path end
  table.sort(path_list)
  return path_list, resolved_burst_id_set
end

---@param section_list DiffReviewStatusSection[]
---@param name string
---@return DiffReviewStatusSection?
local function section_named(section_list, name)
  for _, section in ipairs(section_list or {}) do
    if section.name == name then return section end
  end
  return nil
end

---@param status table
---@param projected_section_list DiffReviewStatusSection[]
---@param snapshot DiffReviewPathStatusSnapshot
local function apply_snapshot_sources(status, projected_section_list, snapshot)
  local registry = status.diff_source_registry
  if not registry then return end
  local source_id_list = { "unstaged", "staged" }
  for _, source_id in ipairs(source_id_list) do
    local source_state = registry.source_by_id[source_id]
    if source_state then
      for _, path in ipairs(snapshot.affected_path_list or {}) do source.remove_file(source_state, path) end
      local section = section_named(projected_section_list, source_id)
      local affected_file_list = {}
      local affected_file_set = {}
      for _, filename in ipairs(snapshot.affected_file_list or {}) do affected_file_set[paths.normalize_path(filename)] = true end
      for _, file in ipairs(section and section.files or {}) do
        if affected_file_set[paths.normalize_path(file.filename)] then
          affected_file_list[#affected_file_list + 1] = file
        end
      end
      diff_source_state._status_populate_reloaded_source_files(
        source_state,
        affected_file_list,
        status,
        source_id,
        source_id
      )
    end
  end
  source.clear_invalidated_path_list(registry, source_id_list, snapshot.affected_path_list or {})
  local later_path_set = {}
  for _, pending_path in ipairs(mutation_coordinator.pending_paths(status.cwd)) do later_path_set[pending_path] = true end
  for pending_file in pairs(operation_journal.pending_path_set(ensure_journal(status))) do
    local pending_path = paths.repo_relative(pending_file, status.cwd)
    if pending_path then later_path_set[(pending_path:gsub("\\", "/"))] = true end
  end
  local later_path_list = {}
  for path in pairs(later_path_set) do later_path_list[#later_path_list + 1] = path end
  if #later_path_list > 0 then source_loader.invalidate(registry, source_id_list, later_path_list) end
end

---@param status table
---@param snapshot DiffReviewPathStatusSnapshot
---@param snapshot_section_list DiffReviewStatusSection[]
---@param resolved_burst_id_set table<integer, boolean>
---@param force_render boolean
---@return boolean corrected
local function commit_status_snapshot(status, snapshot, snapshot_section_list, resolved_burst_id_set, force_render)
  local journal = ensure_journal(status)
  local authoritative_section_list
  if snapshot.full_repository then
    authoritative_section_list = snapshot_section_list
  else
    local affected_path_set = {}
    for _, filename in ipairs(snapshot.affected_file_list or {}) do affected_path_set[filename] = true end
    authoritative_section_list = section_map.replace_paths(
      operation_journal.confirmed(journal),
      snapshot_section_list,
      affected_path_set
    )
  end
  local committed_journal = journal
  for resolved_burst_id in pairs(resolved_burst_id_set) do
    committed_journal = operation_journal.commit(committed_journal, authoritative_section_list, resolved_burst_id)
  end
  status.operation_journal = committed_journal
  local projected_section_list = operation_journal.project(committed_journal)
  local corrected = not section_map.equivalent(status.sections or {}, projected_section_list)
  apply_snapshot_sources(status, projected_section_list, snapshot)
  status.verification_stale = false
  if force_render or corrected then
    status.sections = projected_section_list
    status_render().status_cancel_pending_enrichment(status)
    render_current_projection(status)
  end
  return corrected
end

---@param state DiffReviewStatusSyncRootState
---@param snapshot DiffReviewPathStatusSnapshot
---@param resolved_burst_id_set table<integer, boolean>
---@param force_render boolean
local function commit_snapshot(state, snapshot, resolved_burst_id_set, force_render)
  local affected_file_list = vim.deepcopy(snapshot.affected_file_list or {})
  local before_cache = capture_cache(affected_file_list)
  apply_snapshot_cache(snapshot)
  retire_cache_layers(state, resolved_burst_id_set)
  replay_cache_layers(state)
  local changed_file_list = changed_cache_file_list(before_cache, affected_file_list)
  local snapshot_section_list = section_map.sections_from_snapshot(snapshot)
  local corrected = false
  for _, status in ipairs(status_state_list(state.root)) do
    corrected = commit_status_snapshot(status, snapshot, snapshot_section_list, resolved_burst_id_set, force_render) or corrected
  end
  if corrected or #changed_file_list > 0 then
    syntax_engine.clear_context_cache()
    syntax_engine.clear_diff_syntax_cache()
  end
  local refresh_file_list = force_render and affected_file_list or changed_file_list
  for _, filename in ipairs(refresh_file_list) do diff_buffer.refresh_open_diff_buffer_from_cache(filename) end
  state.verification_stale = false
end

---@param burst DiffReviewMutationBurst
local function notify_burst_failure(burst)
  local task = burst.failed_task
  local result = burst.failure or { ok = false, error = "unknown Git failure" }
  local failure = result.failure or {}
  local target = failure.target or {}
  local detail = failure.message or failure.output or result.error or "Git mutation failed"
  local cancelled_suffix = #burst.cancelled_tasks > 0
      and ("\nCancelled %d queued action(s). Resynchronizing Git state."):format(#burst.cancelled_tasks)
    or "\nResynchronizing Git state."
  notifications.error(("%s failed for %s: %s%s"):format(
    task and task.label or "Git mutation",
    target.path or "repository",
    tostring(detail),
    cancelled_suffix
  ))
end

---@param root string
---@param path_list string[]
---@param done fun(snapshot: DiffReviewPathStatusSnapshot?, snapshot_error: DiffReviewPathStatusSnapshotError?)
local function collect_snapshot_with_retry(root, path_list, done)
  local attempt_count = 0
  local completed = false

  local function collect()
    if completed then return end
    attempt_count = attempt_count + 1
    local attempt_completed = false
    status_snapshot.collect_async(root, path_list, function(snapshot, snapshot_error)
      if completed or attempt_completed then return end
      attempt_completed = true
      if snapshot then
        completed = true
        done(snapshot, nil)
        return
      end
      if attempt_count >= snapshot_attempt_limit then
        completed = true
        done(nil, snapshot_error)
        return
      end
      vim.defer_fn(collect, snapshot_retry_delay_ms)
    end)
  end

  collect()
end

---@param state DiffReviewStatusSyncRootState
---@param burst DiffReviewMutationBurst
---@param force_render boolean
---@param done fun(ok: boolean)
local function synchronize_burst(state, burst, force_render, done)
  local path_list, resolved_burst_id_set = synchronization_scope(state, burst)
  collect_snapshot_with_retry(state.root, path_list, function(snapshot, snapshot_error)
    if not snapshot then
      state.verification_stale = true
      for _, status in ipairs(status_state_list(state.root)) do status.verification_stale = true end
      notifications.error(snapshot_error and snapshot_error.message or "Git status synchronization failed")
      done(false)
      return
    end
    commit_snapshot(state, snapshot, resolved_burst_id_set, force_render)
    done(true)
  end)
end

---@param state DiffReviewStatusSyncRootState
---@param burst DiffReviewMutationBurst
---@param done fun(ok: boolean)
local function recover_burst(state, burst, done)
  notify_burst_failure(burst)
  local path_list, resolved_burst_id_set = synchronization_scope(state, burst)
  collect_snapshot_with_retry(state.root, path_list, function(snapshot, snapshot_error)
    if snapshot then
      commit_snapshot(state, snapshot, resolved_burst_id_set, true)
      done(true)
      return
    end

    local completed_group_list = completed_mutation_group_list(burst)
    local refresh_file_set = {}
    for _, layer in ipairs(state.cache_layer_list) do
      if layer.burst_id == burst.id then
        for _, selection in ipairs(layer.selection_list) do refresh_file_set[selection.filename] = true end
      end
    end
    reverse_cache_layers_for_burst(state, burst.id)
    local remaining_cache_layer_list = {}
    for _, layer in ipairs(state.cache_layer_list) do
      if layer.burst_id ~= burst.id then remaining_cache_layer_list[#remaining_cache_layer_list + 1] = layer end
    end
    state.cache_layer_list = remaining_cache_layer_list
    for _, group in ipairs(completed_group_list) do
      local layer = capture_cache_layer(state.root, group.entry_list, group.target_section)
      layer.burst_id = burst.id
      state.cache_layer_list[#state.cache_layer_list + 1] = layer
      apply_cache_layer(layer)
      for _, selection in ipairs(layer.selection_list) do refresh_file_set[selection.filename] = true end
    end
    for _, status in ipairs(status_state_list(state.root)) do
      status.operation_journal = operation_journal.remove_burst(ensure_journal(status), burst.id)
      for _, group in ipairs(completed_group_list) do
        status.operation_journal = operation_journal.append(
          status.operation_journal,
          burst.id,
          group.entry_list,
          group.target_section
        )
      end
      status.operation_journal = operation_journal.mark_succeeded(status.operation_journal, burst.id)
      status.sections = operation_journal.project(status.operation_journal)
      status.verification_stale = true
      status_render().status_cancel_pending_enrichment(status)
      render_current_projection(status)
    end
    for filename in pairs(refresh_file_set) do diff_buffer.refresh_open_diff_buffer_from_cache(filename) end
    state.verification_stale = true
    notifications.error(
      "Git mutation recovery failed: " .. tostring(snapshot_error and snapshot_error.message or "unknown snapshot error")
    )
    done(false)
  end)
end

--- Configure one repository's quiet-sync and failure-recovery lifecycle.
---@param root string
function M.configure_root(root)
  local state = root_state(root)
  if state.configured then return end
  state.configured = true
  mutation_coordinator.set_handler(root, {
    settle = function(burst, done)
      for _, status in ipairs(status_state_list(root)) do
        status.operation_journal = operation_journal.mark_succeeded(ensure_journal(status), burst.id)
      end
      synchronize_burst(state, burst, false, done)
    end,
    recover = function(burst, done)
      recover_burst(state, burst, done)
    end,
  })
end

--- Apply one optimistic action before its Git process starts.
---@param root string
---@param burst_id integer
---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusStageSectionName
function M.apply_optimistic(root, burst_id, entries, target_section)
  local state = root_state(root)
  local layer = capture_cache_layer(root, entries, target_section)
  layer.burst_id = burst_id
  state.cache_layer_list[#state.cache_layer_list + 1] = layer
  apply_cache_layer(layer)

  for _, status in ipairs(status_state_list(root)) do
    status.request_id = (status.request_id or 0) + 1
    status_render().status_cancel_pending_enrichment(status)
    local journal = operation_journal.append(ensure_journal(status), burst_id, entries, target_section)
    status.operation_journal = journal
    status.sections = operation_journal.project(journal)
    if status.diff_source_registry and #layer.path_list > 0 then
      source_loader.invalidate(status.diff_source_registry, { "unstaged", "staged" }, layer.path_list)
    end
    render_current_projection(status)
  end
  for _, selection in ipairs(layer.selection_list) do
    diff_buffer.refresh_open_diff_buffer_from_cache(selection.filename)
  end
end

--- Reset one status journal after a full authoritative load.
---@param status table
---@param authoritative_section_list DiffReviewStatusSection[]
function M.reset_status(status, authoritative_section_list)
  status.operation_journal = operation_journal.reset(ensure_journal(status), authoritative_section_list)
  status.verification_stale = false
  if status.cwd then
    local state = root_state(status.cwd)
    state.cache_layer_list = {}
    state.verification_stale = false
  end
end

--- Reset private synchronization state for deterministic tests.
function M.reset_for_test()
  root_state_by_key = {}
end

return M
