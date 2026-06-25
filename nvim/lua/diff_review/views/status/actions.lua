--- Applies stage, unstage, and discard mutations for the status view, then reconciles the
--- diff sources. Owns the optimistic-move, operation-queue, and reconcile pipeline for the
--- GitStatus view, reaching shared state, git execution, and render through the init seam.
local M = {}

local notifications = require("diff_review.infra.notifications")
local git_backend = require("diff_review.git.git_backend")
local paths = require("diff_review.infra.paths")

local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")

local function notify_error(message, title)
  return notifications.error(message, title)
end

--- Block index-mutating actions while a commit is composing: `git commit` holds
--- `.git/index.lock` for the whole time the commit screen is open (commit.lua sets
--- session.suspend_preview), so a stage/unstage/discard would collide with the lock.
---@param action string
---@return boolean blocked
local function blocked_by_active_commit(action)
  if not session.suspend_preview then return false end
  notify_error(action .. " is unavailable while a commit is in progress", "DiffReview")
  return true
end

local repo_relative = paths.repo_relative

-- Defer reconcile a beat after the last queued mutation settles, coalescing rapid actions.
local status_reconcile_delay_ms = 120

---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusSectionName
---@param target_id? string
local function status_apply_optimistic_entries(entries, target_section, target_id)
  local status = session.status
  if not (status and status.sections) then return end
  local next_sections = dr()._status_apply_optimistic_move(status.sections, entries, target_section)
  if not next_sections then return end
  status.sections = next_sections
  dr()._status_render_current_model(target_id)
end

---@param buf integer
---@param target_id? string
local function refresh_status_after_action(buf, target_id)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  if target_id then
    dr()._render_status_or_notify(buf, target_id, vim.api.nvim_win_get_cursor(0)[1])
  else
    dr()._render_status_or_notify(buf)
  end
end

local function status_operations_pending()
  local status = session.status
  if not status then return false end
  if status.operation_queue_model then
    return dr()._diff_mutation_queue_model.pending(status.operation_queue_model)
  end
  return status.operation_running or #(status.operation_queue or {}) > 0
end

---@param status table
---@param done fun()
local function status_reload_invalidated_diff_sources(status, done)
  local registry = status and status.diff_source_registry or nil
  if not registry then
    done()
    return
  end
  local source_ids = {}
  local path_set = {}
  for source_id, invalidated in pairs(registry.invalidation_by_source or {}) do
    source_ids[#source_ids + 1] = source_id
    for path in pairs(invalidated or {}) do
      path_set[path] = true
    end
  end
  local paths = dr()._status_files_from_set(path_set)
  if #source_ids == 0 or #paths == 0 then
    done()
    return
  end
  dr()._status_perf_event("source.reload_invalidated.start", status.buf, {
    source_ids = source_ids,
    paths = paths,
  })
  dr()._diff_source_model.reload_paths(registry, source_ids, paths, function(ok, err)
    if not ok then
      notify_error("Diff source reload failed: " .. tostring(err or "unknown error"), "DiffReview")
    end
    dr()._status_perf_event("source.reload_invalidated.done", status.buf, {
      ok = ok,
      source_ids = source_ids,
      paths = paths,
    })
    done()
  end)
end

---@param buf integer?
---@param target_id? string
local function status_request_reconcile(buf, target_id)
  local status = session.status
  if not status then return end
  status.reconcile_buf = buf or status.buf
  status.reconcile_target_id = target_id
  if status_operations_pending() or not status.reconcile_buf then return end

  status.reconcile_generation = (status.reconcile_generation or 0) + 1
  local generation = status.reconcile_generation
  vim.defer_fn(function()
    local latest_status = session.status
    if not latest_status or latest_status.reconcile_generation ~= generation then return end
    if status_operations_pending() then return end
    local reconcile_buf = latest_status.reconcile_buf
    local reconcile_target_id = latest_status.reconcile_target_id
    latest_status.reconcile_buf = nil
    latest_status.reconcile_target_id = nil
    status_reload_invalidated_diff_sources(latest_status, function()
      if reconcile_buf then refresh_status_after_action(reconcile_buf, reconcile_target_id) end
    end)
  end, status_reconcile_delay_ms)
end

---@param operation fun(done: fun())
local function status_enqueue_operation(operation)
  session.status = session.status or {}
  local status = session.status
  status.reconcile_generation = (status.reconcile_generation or 0) + 1
  status.operation_queue_model = status.operation_queue_model or dr()._diff_mutation_queue_model.new()
  dr()._diff_mutation_queue_model.enqueue(status.operation_queue_model, operation)
  dr()._diff_mutation_queue_model.on_idle(status.operation_queue_model, function()
    status_request_reconcile(status.reconcile_buf, status.reconcile_target_id)
  end)
end
---@param entries DiffReviewStatusEntry[]
---@param target_section DiffReviewStatusSectionName
---@return DiffReviewStatusEntry[]
local function status_action_entries_for_target(entries, target_section)
  local action_entries = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "file" and entry.file and entry.file.section_name ~= target_section then
      action_entries[#action_entries + 1] = entry
    elseif entry.kind == "hunk" and entry.hunk and entry.file and entry.hunk.staged ~= (target_section == "staged") then
      action_entries[#action_entries + 1] = entry
    end
  end
  return action_entries
end

---@param entry DiffReviewStatusEntry
---@return string?
local function status_entry_source_path(entry)
  local status = session.status
  if not (entry and entry.file and status) then return nil end
  return dr()._status_diff_file_path(entry.file, status)
end

---@param entries DiffReviewStatusEntry[]
---@param source_ids string[]
local function status_mark_diff_paths_pending(entries, source_ids)
  local status = session.status
  if not (status and status.diff_source_registry) then return end
  local path_set = {}
  for _, entry in ipairs(entries or {}) do
    local path = status_entry_source_path(entry)
    if path and path ~= "" then path_set[path] = true end
  end
  local paths = dr()._status_files_from_set(path_set)
  if #paths == 0 then return end
  dr()._status_perf_event("source.invalidate_paths", status.buf, {
    source_ids = source_ids,
    paths = paths,
  })
  dr()._diff_source_loader_model.invalidate(status.diff_source_registry, source_ids, paths)
end

---@param entries DiffReviewStatusEntry[]
---@return string[] hunk_diffs
---@return string[] tracked_files
---@return string[] untracked_files
local function status_split_action_entries(entries)
  local hunk_diffs = {}
  local tracked_files = {}
  local untracked_files = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" and entry.hunk then
      hunk_diffs[#hunk_diffs + 1] = entry.hunk.diff
    elseif entry.kind == "file" and entry.file then
      if entry.file.untracked then
        untracked_files[entry.file.filename] = true
      else
        tracked_files[entry.file.filename] = true
      end
    end
  end
  return hunk_diffs, dr()._status_files_from_set(tracked_files), dr()._status_files_from_set(untracked_files)
end

---@param entry DiffReviewStatusEntry
---@return boolean
local function status_entry_is_added(entry)
  return dr()._git_status_is_added((entry.file and entry.file.git_status) or (entry.hunk and entry.hunk.git_status))
end

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusEntry[]
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

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusEntry[] tracked_entries
---@return DiffReviewStatusEntry[] added_entries
local function status_partition_unstage_entries(entries)
  local tracked_entries = {}
  local added_entries = {}
  for _, entry in ipairs(entries) do
    if status_entry_is_added(entry) then
      added_entries[#added_entries + 1] = entry
    else
      tracked_entries[#tracked_entries + 1] = entry
    end
  end
  return tracked_entries, added_entries
end

---@param entries DiffReviewStatusEntry[]
---@return string[] hunk_diffs
---@return string[] tracked_files
---@return string[] added_files
local function status_split_unstage_entries(entries)
  local hunk_diffs = {}
  local tracked_files = {}
  local added_files = {}
  for _, entry in ipairs(entries) do
    if entry.kind == "hunk" and entry.hunk then
      hunk_diffs[#hunk_diffs + 1] = entry.hunk.diff
    elseif entry.kind == "file" and entry.file then
      if status_entry_is_added(entry) then
        added_files[entry.file.filename] = true
      else
        tracked_files[entry.file.filename] = true
      end
    end
  end
  return hunk_diffs, dr()._status_files_from_set(tracked_files), dr()._status_files_from_set(added_files)
end

---@param entries DiffReviewStatusEntry[]
---@return DiffReviewStatusSectionName?
local function status_unstage_target_section(entries)
  local has_added = false
  for _, entry in ipairs(entries) do
    if status_entry_is_added(entry) then
      has_added = true
    else
      return "unstaged"
    end
  end
  if has_added then return "unstaged" end
  return nil
end

---@class DiffReviewStatusActionOpts
---@field preserve_cursor? boolean

---@param entries DiffReviewStatusEntry[]
---@param opts? DiffReviewStatusActionOpts
local function status_stage_entries(entries, opts)
  opts = opts or {}
  if #entries == 0 then return end
  if blocked_by_active_commit("Stage") then return end
  local expanded_entries = dr()._status_expanded_entries(entries)
  if #expanded_entries == 0 then return end

  local action_entries = status_action_entries_for_target(expanded_entries, "staged")
  if #action_entries == 0 then return end

  local target_id = nil
  if not opts.preserve_cursor then
    target_id = dr()._status_action_target_id(entries, action_entries, "staged", { file_target = "next" })
  end
  local hunk_diffs, tracked_files, untracked_files = status_split_action_entries(action_entries)
  local staged_hunks = 0
  local staged_files = 0
  local status_buf = session.status and session.status.buf

  status_mark_diff_paths_pending(action_entries, { "unstaged", "staged" })
  status_apply_optimistic_entries(action_entries, "staged", target_id)

  local function finish()
    if staged_hunks > 0 or staged_files > 0 then
      dr()._status_notify_action("Staged", staged_hunks, staged_files)
    end
    status_request_reconcile(status_buf, target_id)
  end

  local function stage_untracked_files()
    if #untracked_files == 0 then
      finish()
      return
    end
    dr().stage_files_async(untracked_files, function(result)
      staged_files = staged_files + #result.successes
      finish()
    end)
  end

  local function stage_files_after_hunks()
    if #tracked_files == 0 then
      stage_untracked_files()
      return
    end
    dr().stage_tracked_files_async(tracked_files, function(result)
      staged_files = staged_files + #result.successes
      stage_untracked_files()
    end)
  end

  local function stage_hunk_at(index)
    local diff = hunk_diffs[index]
    if not diff then
      stage_files_after_hunks()
      return
    end
    dr().stage_patch_async(diff, function(ok)
      if ok then staged_hunks = staged_hunks + 1 end
      stage_hunk_at(index + 1)
    end)
  end

  status_enqueue_operation(function(done)
    local original_finish = finish
    finish = function()
      original_finish()
      done()
    end
    stage_hunk_at(1)
  end)
end

---@param entry DiffReviewStatusEntry?
local function status_stage(entry)
  if not entry then return end
  status_stage_entries({ entry })
end

---@param entries DiffReviewStatusEntry[]
---@param opts? DiffReviewStatusActionOpts
local function status_unstage_entries(entries, opts)
  opts = opts or {}
  if #entries == 0 then return end
  if blocked_by_active_commit("Unstage") then return end
  local expanded_entries = dr()._status_expanded_entries(entries)
  if #expanded_entries == 0 then return end

  local action_entries = status_unstage_action_entries(expanded_entries)
  if #action_entries == 0 then return end

  local target_id = nil
  if not opts.preserve_cursor then
    target_id = dr()._status_action_target_id(entries, action_entries, status_unstage_target_section(action_entries))
  end
  local tracked_entries, added_entries = status_partition_unstage_entries(action_entries)
  local hunk_diffs, files, added_files = status_split_unstage_entries(action_entries)
  local unstaged_hunks = 0
  local unstaged_files = 0
  local status_buf = session.status and session.status.buf

  status_mark_diff_paths_pending(action_entries, { "staged", "unstaged" })
  status_apply_optimistic_entries(tracked_entries, "unstaged", target_id)
  status_apply_optimistic_entries(added_entries, "unstaged", target_id)

  local function finish()
    if unstaged_hunks > 0 or unstaged_files > 0 then
      dr()._status_notify_action("Unstaged", unstaged_hunks, unstaged_files)
    end
    status_request_reconcile(status_buf, target_id)
  end

  local function unstage_files_after_hunks()
    if #files == 0 then
      if #added_files == 0 then
        finish()
        return
      end
      dr().unstage_added_files_async(added_files, function(result)
        unstaged_files = unstaged_files + #result.successes
        finish()
      end)
      return
    end
    dr().unstage_files_async(files, function(result)
      unstaged_files = unstaged_files + #result.successes
      if #added_files == 0 then
        finish()
        return
      end
      dr().unstage_added_files_async(added_files, function(added_result)
        unstaged_files = unstaged_files + #added_result.successes
        finish()
      end)
    end)
  end

  local function unstage_hunk_at(index)
    local diff = hunk_diffs[index]
    if not diff then
      unstage_files_after_hunks()
      return
    end
    dr().unstage_patch_async(diff, function(ok)
      if ok then unstaged_hunks = unstaged_hunks + 1 end
      unstage_hunk_at(index + 1)
    end)
  end

  status_enqueue_operation(function(done)
    local original_finish = finish
    finish = function()
      original_finish()
      done()
    end
    unstage_hunk_at(1)
  end)
end

---@param entry DiffReviewStatusEntry?
local function status_unstage(entry)
  if not entry then return end
  status_unstage_entries({ entry })
end

---@param entries DiffReviewStatusEntry[]
---@param target_id? string
local function status_discard_entries(entries, target_id)
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
      if #failures > 0 then dr().notify_git_failures("Discard failed", failures) end
      refresh_status_after_action(status_buf, target_id)
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
            if dr()._git_status_is_added(entry.file.git_status) then
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
          elseif dr()._git_status_is_added(entry.file.git_status) then
            git_backend.run_git_at_root_async(cwd, { "rm", "--cached", "--ignore-unmatch", "--", relpath }, nil, function(rm_result)
              if rm_result.ok then
                delete_file(entry.file.filename, " after unstaging")
              else
                add_failure(rm_result)
              end
              next_entry()
            end)
          elseif dr()._git_status_is_renamed(entry.file.git_status) then
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
          elseif dr()._git_status_is_deleted(entry.file.git_status) then
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

---@param entries DiffReviewStatusEntry[]
---@param target_id? string
---@param opts? DiffReviewStatusActionOpts
local function status_discard_entry_list(entries, target_id, opts)
  opts = opts or {}
  local discard_entries = {}
  for _, entry in ipairs(dr()._status_expanded_entries(entries)) do
    if entry.kind == "hunk" or entry.kind == "file" then
      discard_entries[#discard_entries + 1] = entry
    end
  end
  if #discard_entries == 0 then return end
  local action_target_id = nil
  if not opts.preserve_cursor then
    action_target_id = dr()._status_action_target_id(entries, discard_entries) or target_id
  end

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
    message = { ("Discard changes in %d file(s)?"):format(dr()._status_count_set(files)) }
  end
  dr()._confirm(message, function()
    status_discard_entries(discard_entries, action_target_id)
  end)
end

---@param entry DiffReviewStatusEntry?
local function status_discard(entry)
  if not entry then return end
  status_discard_entry_list({ entry }, entry.id)
end

M._status_stage_entries = status_stage_entries
M._status_stage = status_stage
M._status_unstage = status_unstage
M._status_unstage_entries = status_unstage_entries
M._status_discard_entry_list = status_discard_entry_list
M._status_discard = status_discard
M._status_operations_pending = status_operations_pending
M._status_request_reconcile = status_request_reconcile

return M
