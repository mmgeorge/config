--- Coordinates GitStatus actions across optimistic projection, repository mutation, and deferred
--- authoritative synchronization, keeping cursor policy outside stage and unstage operations.
local M = {}

---@class DiffReviewStatusIndexActionOptions
---@field ignored_path_list? string[]
---@field cursor_target? DiffReviewListCursorTarget

---@class DiffReviewStatusListActionOptions
---@field visual_selection? DiffReviewVisualSelection

local notifications = require("diff_review.infra.notifications")
local git_backend = require("diff_review.git.git_backend")
local index_mutation = require("diff_review.git.index_mutation")
local mutation_coordinator = require("diff_review.git.mutation_coordinator")
local paths = require("diff_review.infra.paths")
local source_loader = require("diff_review.render.source_loader")

-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
local diff_source_state = require("diff_review.views.status.diff_source_state")
local git_data = require("diff_review.git.git_data")
local ignored_path_store = require("diff_review.views.status.ignored_path_store")
local entry_nav = require("diff_review.views.status.entry_nav")
local status_helpers = require("diff_review.views.status.status_helpers")
local trace = require("diff_review.infra.perf_trace")
local session = require("diff_review.session")
local status_sync = require("diff_review.views.status.status_sync")

--- Displays an error notification through the notification subsystem.
---@param message string Error message text.
---@param title? string Notification title.
local function notify_error(message, title)
  return notifications.error(message, title)
end

--- Checks whether index mutation is blocked because a commit screen is currently open.
---@param action string Action name string.
---@return boolean blocked True if index mutation is blocked.
local function blocked_by_active_commit(action)
  if not session.suspend_preview then return false end
  notify_error(action .. " is unavailable while a commit is in progress", "DiffReview")
  return true
end

local repo_relative = paths.repo_relative

--- Refreshes the status buffer display after a discard operation.
---@param buf integer? Status buffer handle.
---@param target_id? string Optional entry ID to refocus.
---@param restore_cursor? boolean True to restore cursor position.
local function refresh_status_after_discard(buf, target_id, restore_cursor)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if restore_cursor == false then
    render_orchestrator().render_status_or_notify(buf, nil, nil, { restore_cursor = false })
  elseif target_id then
    render_orchestrator().render_status_or_notify(buf, target_id, vim.api.nvim_win_get_cursor(0)[1])
  else
    render_orchestrator().render_status_or_notify(buf)
  end
end

--- Checks whether background index mutation operations are currently pending.
---@return boolean pending True if index operations are running.
local function status_operations_pending()
  local status = session.status
  return status ~= nil and status.cwd ~= nil and mutation_coordinator.pending(status.cwd)
end

--- Resolves the repository-relative file path for a status entry.
---@param root string Git repository root path.
---@param entry DiffReviewStatusEntry Status entry descriptor.
---@return string? relpath Relative file path, or nil.
local function status_entry_relative_path(root, entry)
  if not (entry and entry.file) then return nil end
  local relpath = entry.file.relpath or repo_relative(entry.file.filename, root)
  return relpath and relpath:gsub("\\", "/") or nil
end

--- Converts an ignored status entry into an unstaged file descriptor for staging.
---@param entry DiffReviewStatusEntry Ignored status entry.
---@return DiffReviewStatusEntry entry Staging file entry.
local function status_ignored_file_stage_entry(entry)
  local file = vim.deepcopy(entry.file)
  file.section_name = "unstaged"
  for _, hunk in ipairs(file.hunks or {}) do
    hunk.section_name = "unstaged"
    hunk.staged = false
  end
  return {
    kind = "file",
    file = file,
  }
end

--- Filters entries for staging and separates virtual ignored file paths.
---@param root string Git repository root path.
---@param entries DiffReviewStatusEntry[] Array of selected status entries.
---@return DiffReviewStatusEntry[] action_entries Filtered staging entries.
---@return string[] ignored_path_list Array of staged ignored relative paths.
local function status_stage_action_entries(root, entries)
  local action_entry_list = {}
  local ignored_entry_by_filename = {}
  local ignored_path_set = {}
  for _, entry in ipairs(entries) do
    local file = entry.file
    if file and file.section_name == "ignored" then
      ignored_entry_by_filename[file.filename] = ignored_entry_by_filename[file.filename]
        or status_ignored_file_stage_entry(entry)
      local relpath = status_entry_relative_path(root, entry)
      if relpath then ignored_path_set[relpath] = true end
    elseif entry.kind == "file" and file and file.section_name == "unstaged" then
      action_entry_list[#action_entry_list + 1] = entry
    elseif entry.kind == "hunk" and entry.hunk and file and not entry.hunk.staged then
      action_entry_list[#action_entry_list + 1] = entry
    end
  end
  local ignored_filename_list = vim.tbl_keys(ignored_entry_by_filename)
  table.sort(ignored_filename_list)
  for _, filename in ipairs(ignored_filename_list) do
    action_entry_list[#action_entry_list + 1] = ignored_entry_by_filename[filename]
  end
  local ignored_path_list = entry_nav._status_files_from_set(ignored_path_set)
  return action_entry_list, ignored_path_list
end

--- Extracts relative repository paths for entries belonging to a given section.
---@param root string Git repository root path.
---@param entries DiffReviewStatusEntry[] Array of status entries.
---@param source_section "unstaged"|"ignored" Section filter name.
---@return string[] paths Array of relative file paths.
local function status_virtual_path_list(root, entries, source_section)
  local path_set = {}
  for _, entry in ipairs(entry_nav._status_action_entries(entries)) do
    if entry.file and entry.file.section_name == source_section then
      local relpath = status_entry_relative_path(root, entry)
      if relpath then path_set[relpath] = true end
    end
  end
  return entry_nav._status_files_from_set(path_set)
end

--- Resolves the diff source lookup path for a status entry.
---@param entry DiffReviewStatusEntry Status entry descriptor.
---@return string? source_path Source lookup path, or nil.
local function status_entry_source_path(entry)
  local status = session.status
  if not (entry and entry.file and status) then return nil end
  return diff_source_state._status_diff_file_path(entry.file, status)
end

--- Invalidates cached diff sources for modified status entries.
---@param entries DiffReviewStatusEntry[] Status entry array.
---@param source_ids string[] Array of diff source identifier keys.
local function status_mark_diff_paths_pending(entries, source_ids)
  local status = session.status
  if not (status and status.diff_source_registry) then return end
  local path_set = {}
  for _, entry in ipairs(entries or {}) do
    local path = status_entry_source_path(entry)
    if path and path ~= "" then path_set[path] = true end
  end
  local paths = entry_nav._status_files_from_set(path_set)
  if #paths == 0 then return end
  trace.event("source.invalidate_paths", status.buf, {
    source_ids = source_ids,
    paths = paths,
  })
  source_loader.invalidate(status.diff_source_registry, source_ids, paths)
end

--- Checks if a status entry represents an added file.
---@param entry DiffReviewStatusEntry Status entry descriptor.
---@return boolean added True if the file or hunk represents an addition.
local function status_entry_is_added(entry)
  return git_data._git_status_is_added((entry.file and entry.file.git_status) or (entry.hunk and entry.hunk.git_status))
end

--- Filters status entries to staged files and hunks for unstage actions.
---@param entries DiffReviewStatusEntry[] Selected status entry array.
---@return DiffReviewStatusEntry[] action_entries Staged entries ready for unstaging.
local function status_unstage_action_entries(entries)
  local action_entries = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "file" and entry.file and entry.file.section_name == "staged" then
      action_entries[#action_entries + 1] = entry
    elseif entry.kind == "hunk" and entry.hunk and entry.hunk.staged then
      action_entries[#action_entries + 1] = entry
    end
  end
  return action_entries
end

--- Builds index mutation target descriptors from status entries.
---@param root string Git repository root path.
---@param entries DiffReviewStatusEntry[] Status entry array.
---@param direction DiffReviewIndexMutationDirection Mutation direction (`"stage"` or `"unstage"`).
---@return DiffReviewIndexMutationTarget[] target_list Mutation target array.
---@return DiffReviewStatusEntry[] target_entry_list Corresponding status entry array.
local function status_mutation_target_list(root, entries, direction)
  local target_list = {}
  local target_entry_list = {}
  local seen_file = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" and entry.hunk and entry.file then
      target_list[#target_list + 1] = {
        kind = "hunk",
        path = entry.file.filename,
        diff = entry.hunk.diff,
      }
      target_entry_list[#target_entry_list + 1] = entry
    end
  end
  for _, entry in ipairs(entries) do
    if entry.kind == "file" and entry.file and not seen_file[entry.file.filename] then
      seen_file[entry.file.filename] = true
      local target_kind = "tracked_file"
      if direction == "stage" and entry.file.untracked then
        target_kind = "untracked_file"
      elseif direction == "unstage" and status_entry_is_added(entry) then
        target_kind = "added_file"
      end
      target_list[#target_list + 1] = {
        kind = target_kind,
        path = entry.file.filename,
        original_path = entry.file.path_change_kind == "renamed" and entry.file.original_relpath
            and paths.repo_file_path(root, entry.file.original_relpath)
          or nil,
      }
      target_entry_list[#target_entry_list + 1] = entry
    end
  end
  return target_list, target_entry_list
end

--- Extracts unique repository-relative paths for affected status entries.
---@param root string Git repository root path.
---@param entries DiffReviewStatusEntry[] Status entry array.
---@return string[] paths Array of affected relative file paths.
local function status_mutation_path_list(root, entries)
  local path_set = {}
  for _, entry in ipairs(entries) do
    local filename = entry.file and entry.file.filename or nil
    if filename then
      local relpath = repo_relative(filename, root)
      if relpath then path_set[(relpath:gsub("\\", "/"))] = true end
      if entry.file.path_change_kind == "renamed" and entry.file.original_relpath then
        path_set[(entry.file.original_relpath:gsub("\\", "/"))] = true
      end
    end
  end
  return entry_nav._status_files_from_set(path_set)
end

--- Enqueues an asynchronous index mutation action with optimistic UI updates and completion handling.
---@param label "Stage"|"Unstage" User-facing action verb.
---@param completed_label "Staged"|"Unstaged" User-facing completion verb.
---@param direction DiffReviewIndexMutationDirection Index mutation direction (`"stage"` or `"unstage"`).
---@param entries DiffReviewStatusEntry[] Target status entries array.
---@param target_section DiffReviewStatusStageSectionName Optimistic destination section name.
---@param root_override? string Explicit repository root path.
---@param opts? DiffReviewStatusIndexActionOptions Optional index action options.
local function status_enqueue_index_action(label, completed_label, direction, entries, target_section, root_override, opts)
  opts = opts or {}
  opts.ignored_path_list = opts.ignored_path_list or {}
  local status = session.status
  local root = root_override or (status and status.cwd or nil)
  if not root then
    notify_error(label .. " failed: missing Git root", "DiffReview")
    return
  end
  if mutation_coordinator.recovering(root) then
    notify_error(label .. " is unavailable while Git state recovers", "DiffReview")
    return
  end

  local target_list, target_entry_list = status_mutation_target_list(root, entries, direction)
  local path_list = status_mutation_path_list(root, entries)
  if #target_list == 0 or #path_list == 0 then return end
  status_sync.configure_root(root)
  local _, _, enqueue_error = mutation_coordinator.enqueue(root, {
    label = label,
    paths = path_list,
    metadata = {
      direction = direction,
      target_section = target_section,
      recovery_entry_list = vim.deepcopy(target_entry_list),
    },
    on_enqueue = function(task)
      if #opts.ignored_path_list > 0 then ignored_path_store.begin_stage(root, assert(task.id), opts.ignored_path_list) end
      local optimistic_ok, optimistic_error = pcall(
        status_sync.apply_optimistic,
        root,
        assert(task.burst_id),
        entries,
        target_section,
        opts.cursor_target
      )
      if not optimistic_ok then
        ignored_path_store.cancel_stage(root, assert(task.id))
        error(optimistic_error)
      end
    end,
    execute = function(done)
      index_mutation.execute_async(root, {
        direction = direction,
        target_list = target_list,
      }, done)
    end,
    on_complete = function(result, task)
      ---@cast result DiffReviewIndexMutationResult
      if #opts.ignored_path_list > 0 then
        local ignored_path_set = {}
        for _, path in ipairs(opts.ignored_path_list) do ignored_path_set[path] = true end
        local completed_path_list = {}
        for _, target in ipairs(result.completed_target_list or {}) do
          local completed_path = target.path and repo_relative(target.path, root) or nil
          if completed_path then
            completed_path = completed_path:gsub("\\", "/")
            if ignored_path_set[completed_path] then completed_path_list[#completed_path_list + 1] = completed_path end
          end
        end
        ignored_path_store.finish_stage(root, assert(task.id), completed_path_list)
      end
      if result.ok and result.count and result.count > 0 then
        entry_nav._status_notify_action(completed_label, result.hunk_count or 0, result.file_count or 0)
      end
    end,
    on_cancel = function(task)
      if #opts.ignored_path_list > 0 then ignored_path_store.cancel_stage(root, assert(task.id)) end
    end,
  })
  if enqueue_error then notify_error(label .. " failed: " .. enqueue_error, "DiffReview") end
end

---@class DiffReviewStatusDiscardOpts
---@field visual_selection? DiffReviewVisualSelection

--- Stages selected status entries including unstaged files, hunks, and ignored files.
---@param entries DiffReviewStatusEntry[] Array of selected status entries.
---@param opts? DiffReviewStatusListActionOptions Optional action parameters.
local function status_stage_entries(entries, opts)
  if #entries == 0 then return end
  if blocked_by_active_commit("Stage") then return end
  local action_selection = entry_nav._status_action_entries(entries)
  if #action_selection == 0 then return end

  local status = session.status
  local root = status and status.cwd or nil
  if not root then
    notify_error("Stage failed: missing Git root", "DiffReview")
    return
  end
  local action_entries, ignored_path_list = status_stage_action_entries(root, action_selection)
  if #action_entries == 0 then return end
  local cursor_target = opts and opts.visual_selection
      and entry_nav._status_visual_action_cursor_target(opts.visual_selection, action_entries) or nil
  status_enqueue_index_action("Stage", "Staged", "stage", action_entries, "staged", nil, {
    ignored_path_list = ignored_path_list,
    cursor_target = cursor_target,
  })
end

--- Stages a single status entry.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
local function status_stage(entry)
  if not entry then return end
  status_stage_entries({ entry })
end

--- Unstages selected status entries or unignores virtual entries.
---@param entries DiffReviewStatusEntry[] Array of selected status entries.
---@param opts? DiffReviewStatusListActionOptions Optional action parameters.
local function status_unstage_entries(entries, opts)
  if #entries == 0 then return end
  local action_selection = entry_nav._status_action_entries(entries)
  if #action_selection == 0 then return end

  local status = session.status
  local root = status and status.cwd or nil
  if not root then
    notify_error("Unstage failed: missing Git root", "DiffReview")
    return
  end
  local ignored_path_list = status_virtual_path_list(root, action_selection, "ignored")
  local virtual_changed = #ignored_path_list > 0 and ignored_path_store.unignore_paths(root, ignored_path_list)
  if virtual_changed then
    entry_nav._status_notify_action("Unignored", 0, #ignored_path_list)
  end

  local action_entries = status_unstage_action_entries(action_selection)
  local target_entries = #action_entries > 0 and action_entries or action_selection
  local cursor_target = opts and opts.visual_selection
      and entry_nav._status_visual_action_cursor_target(opts.visual_selection, target_entries) or nil
  if #action_entries == 0 then
    if virtual_changed then status_sync.reproject_ignored(root, cursor_target) end
    return
  end
  if blocked_by_active_commit("Unstage") then return end
  status_enqueue_index_action("Unstage", "Unstaged", "unstage", action_entries, "unstaged", nil, {
    cursor_target = cursor_target,
  })
end

--- Unstages a single status entry.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
local function status_unstage(entry)
  if not entry then return end
  status_unstage_entries({ entry })
end

--- Adds selected unstaged files to the session ignored path registry.
---@param entries DiffReviewStatusEntry[] Array of selected status entries.
---@param opts? DiffReviewStatusListActionOptions Optional action parameters.
local function status_ignore_entries(entries, opts)
  local status = session.status
  local root = status and status.cwd or nil
  if not root then
    notify_error("Ignore failed: missing Git root", "DiffReview")
    return
  end
  local ignored_path_list = status_virtual_path_list(root, entries, "unstaged")
  if #ignored_path_list == 0 then return end
  local action_selection = entry_nav._status_action_entries(entries)
  local cursor_target = opts and opts.visual_selection
      and entry_nav._status_visual_action_cursor_target(opts.visual_selection, action_selection) or nil
  if ignored_path_store.ignore_paths(root, ignored_path_list) then
    status_sync.reproject_ignored(root, cursor_target)
    entry_nav._status_notify_action("Ignored", 0, #ignored_path_list)
  end
end

--- Adds a single unstaged entry to the session ignored path registry.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
local function status_ignore(entry)
  if entry then status_ignore_entries({ entry }) end
end

--- Resolves the repository root path containing the specified filename.
---@param filename string Absolute or relative filename path.
---@return string? root Owning repository root path, or nil.
local function status_root_for_filename(filename)
  local seen_status = {}
  local candidate_list = {}
  if session.status then candidate_list[#candidate_list + 1] = session.status end
  if session.main_status then candidate_list[#candidate_list + 1] = session.main_status end
  for _, candidate in pairs(session.states or {}) do candidate_list[#candidate_list + 1] = candidate end
  for _, candidate in ipairs(candidate_list) do
    if candidate and candidate.cwd and not seen_status[candidate] then
      seen_status[candidate] = true
      local relpath = repo_relative(filename, candidate.cwd)
      if relpath then return candidate.cwd end
    end
  end
  return nil
end

--- Locates an existing hunk status entry in session state or constructs a synthetic hunk entry.
---@param root string Git repository root path.
---@param filename string Absolute file path.
---@param diff string Raw diff hunk patch string.
---@param staged boolean True if hunk is currently staged.
---@return DiffReviewStatusEntry entry Located or synthetic status entry.
local function status_hunk_entry(root, filename, diff, staged)
  for _, status in pairs(session.states or {}) do
    if status.cwd and paths.normalize_path(status.cwd) == paths.normalize_path(root) then
      for _, section in ipairs(status.sections or {}) do
        for _, file in ipairs(section.files or {}) do
          if paths.normalize_path(file.filename) == paths.normalize_path(filename) then
            for _, hunk in ipairs(file.hunks or {}) do
              if hunk.diff == diff and hunk.staged == staged then
                return { kind = "hunk", id = "", file = file, hunk = hunk }
              end
            end
          end
        end
      end
    end
  end
  local relpath = repo_relative(filename, root) or filename
  ---@type DiffReviewStatusFile
  local file = {
    filename = filename,
    relpath = relpath,
    section_name = staged and "staged" or "unstaged",
    added = 0,
    removed = 0,
    hunks = {},
    untracked = false,
    status = "",
    git_status = "M",
  }
  ---@type DiffReviewHunk
  local hunk = {
    file = relpath,
    filename = filename,
    section_name = file.section_name,
    pos = 1,
    diff = diff,
    staged = staged,
    context_text = "",
    git_status = "M",
    added = 0,
    removed = 0,
  }
  return { kind = "hunk", id = "", file = file, hunk = hunk }
end

--- Enqueues an index mutation action for an individual diff hunk patch.
---@param filename string Absolute file path.
---@param diff string Raw diff hunk patch string.
---@param direction DiffReviewIndexMutationDirection Mutation direction (`"stage"` or `"unstage"`).
local function status_mutate_diff_hunk(filename, diff, direction)
  local label = direction == "stage" and "Stage" or "Unstage"
  if blocked_by_active_commit(label) then return end
  local function enqueue_at_root(root)
    local source_staged = direction == "unstage"
    local target_section = direction == "stage" and "staged" or "unstaged"
    local completed_label = direction == "stage" and "Staged" or "Unstaged"
    local entry = status_hunk_entry(root, filename, diff, source_staged)
    status_enqueue_index_action(label, completed_label, direction, { entry }, target_section, root)
  end

  local root = status_root_for_filename(filename)
  if root then
    enqueue_at_root(root)
    return
  end
  git_backend.git_root_async(function(resolved_root, root_error)
    if not resolved_root then
      notify_error(root_error or label .. " failed: missing Git root", "DiffReview")
      return
    end
    enqueue_at_root(resolved_root)
  end)
end

--- Stages an individual diff hunk patch into the Git index.
---@param filename string Absolute file path.
---@param diff string Raw diff hunk patch string.
local function status_stage_diff_hunk(filename, diff)
  status_mutate_diff_hunk(filename, diff, "stage")
end

--- Unstages an individual diff hunk patch from the Git index.
---@param filename string Absolute file path.
---@param diff string Raw diff hunk patch string.
local function status_unstage_diff_hunk(filename, diff)
  status_mutate_diff_hunk(filename, diff, "unstage")
end

--- Discards uncommitted changes for a list of status entries asynchronously.
---@param entries DiffReviewStatusEntry[] Target status entries array.
---@param target_id? string Optional post-discard selection anchor ID.
---@param fallback_line? integer Optional fallback line number for cursor focus.
local function status_discard_entries(entries, target_id, fallback_line)
  if blocked_by_active_commit("Discard") then return end
  local status_buf = session.status and session.status.buf
  git_backend.git_root_async(function(cwd, root_err)
    if not cwd then
      notify_error(root_err or "Unable to find git root")
      return
    end

    local failures = {}
    if #entries == 0 then return end
    status_mark_diff_paths_pending(entries, { "unstaged", "staged" })

    local function finish_all()
      if #failures > 0 then notifications.git_failures("Discard failed", failures) end
      if fallback_line and status_buf and vim.api.nvim_buf_is_valid(status_buf) then
        render_orchestrator().render_status_or_notify(status_buf, target_id, fallback_line)
      else
        refresh_status_after_discard(status_buf, target_id)
      end
    end

    local function discard_at(index)
      local entry = entries[index]
      if not entry then
        finish_all()
        return
      end

      local function next_entry()
        discard_at(index + 1)
      end

      if entry.kind == "hunk" then
        local args = { "apply", "--reverse", "--whitespace=nowarn", "--unidiff-zero" }
        if entry.hunk.staged then args[#args + 1] = "--index" end
        args[#args + 1] = "-"
        git_backend.run_git_at_root_async(cwd, args, entry.hunk.diff .. "\n", function(result)
          if not result.ok then
            failures[#failures + 1] = { file = entry.file.filename, output = result.output, code = result.code }
          end
          next_entry()
        end)
      elseif entry.file.untracked then
        local delete_code = git_backend.delete_path(entry.file.filename)
        if delete_code ~= 0 then
          failures[#failures + 1] = {
            file = entry.file.filename,
            message = ("delete() failed with code %d"):format(delete_code),
          }
        end
        next_entry()
      else
        local relpath, rel_err = repo_relative(entry.file.filename, cwd)
        if not relpath then
          failures[#failures + 1] = { file = entry.file.filename, message = rel_err }
          next_entry()
        else
          ---@param result DiffReviewGitCommandResult
          local function add_failure(result)
            failures[#failures + 1] = {
              file = entry.file.filename,
              output = result.output,
              code = result.code,
            }
          end

          ---@param path string
          ---@param context string
          ---@return boolean
          local function delete_file(path, context)
            local delete_code = git_backend.delete_path(path)
            if delete_code == 0 then return true end
            failures[#failures + 1] = {
              file = path,
              message = ("delete() failed with code %d%s"):format(delete_code, context),
            }
            return false
          end

          if entry.file.section_name == "unstaged" then
            if git_data._git_status_is_added(entry.file.git_status) then
              git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
                if restore_result.ok then
                  delete_file(entry.file.filename, " after unstaging")
                else
                  add_failure(restore_result)
                end
                next_entry()
              end)
            else
              git_backend.run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end
          elseif git_data._git_status_is_added(entry.file.git_status) then
            git_backend.run_git_at_root_async(cwd, { "rm", "--cached", "--ignore-unmatch", "--", relpath }, nil, function(rm_result)
              if rm_result.ok then
                delete_file(entry.file.filename, " after unstaging")
              else
                add_failure(rm_result)
              end
              next_entry()
            end)
          elseif git_data._git_status_is_renamed(entry.file.git_status) then
            local original_relpath = entry.file.original_relpath
            if not original_relpath or original_relpath == "" then
              failures[#failures + 1] = { file = entry.file.filename, message = "Missing original path for renamed file" }
              next_entry()
            else
              git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath, original_relpath }, nil, function(restore_result)
                if not restore_result.ok then
                  add_failure(restore_result)
                  next_entry()
                  return
                end
                git_backend.run_git_at_root_async(cwd, { "checkout", "--", original_relpath }, nil, function(checkout_result)
                  if checkout_result.ok then
                    delete_file(entry.file.filename, " after restoring renamed file")
                  else
                    add_failure(checkout_result)
                  end
                  next_entry()
                end)
              end)
            end
          elseif git_data._git_status_is_deleted(entry.file.git_status) then
            git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
              if not restore_result.ok then
                add_failure(restore_result)
                next_entry()
                return
              end
              git_backend.run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end)
          else
            git_backend.run_git_at_root_async(cwd, { "restore", "--staged", "--", relpath }, nil, function(restore_result)
              if not restore_result.ok then
                add_failure(restore_result)
                next_entry()
                return
              end
              git_backend.run_git_at_root_async(cwd, { "checkout", "--", relpath }, nil, function(checkout_result)
                if not checkout_result.ok then add_failure(checkout_result) end
                next_entry()
              end)
            end)
          end
        end
      end
    end

    discard_at(1)
  end)
end

--- Prompts for user confirmation and initiates discard operations for selected entries.
---@param entries DiffReviewStatusEntry[] Target status entries array.
---@param target_id? string Optional post-discard entry anchor ID.
---@param opts? DiffReviewStatusDiscardOpts Optional discard action options.
local function status_discard_entry_list(entries, target_id, opts)
  opts = opts or {}
  local discard_entries = {}
  for _, entry in ipairs(entry_nav._status_action_entries(entries)) do
    if entry.kind == "hunk" or entry.kind == "file" then
      discard_entries[#discard_entries + 1] = entry
    end
  end
  if #discard_entries == 0 then return end
  local cursor_target = opts.visual_selection
      and entry_nav._status_visual_action_cursor_target(opts.visual_selection, discard_entries) or nil
  local action_target_id = cursor_target and cursor_target.id
    or entry_nav._status_action_target_id(entries, discard_entries)
    or target_id

  local message
  if #discard_entries == 1 then
    local first_entry = discard_entries[1]
    local prompt = first_entry.kind == "hunk" and "Discard this hunk?"
      or (first_entry.file.untracked and "Delete untracked file?" or "Discard ALL changes to file?")
    message = { prompt, "  " .. first_entry.file.relpath }
  else
    local files = {}
    for _, entry in ipairs(discard_entries) do
      files[entry.file.filename] = true
    end
    message = { ("Discard changes in %d file(s)?"):format(entry_nav._status_count_set(files)) }
  end
  status_helpers.confirm(message, function()
    status_discard_entries(discard_entries, action_target_id, cursor_target and cursor_target.fallback_line or nil)
  end)
end

--- Prompts for confirmation and discards changes for a single status entry.
---@param entry DiffReviewStatusEntry? Status entry descriptor.
local function status_discard(entry)
  if not entry then return end
  status_discard_entry_list({ entry }, entry.id)
end

M._status_stage_entries = status_stage_entries
M._status_stage = status_stage
M._status_stage_diff_hunk = status_stage_diff_hunk
M._status_unstage = status_unstage
M._status_unstage_entries = status_unstage_entries
M._status_unstage_diff_hunk = status_unstage_diff_hunk
M._status_ignore = status_ignore
M._status_ignore_entries = status_ignore_entries
M._status_discard_entry_list = status_discard_entry_list
M._status_discard = status_discard
M._status_operations_pending = status_operations_pending

return M
