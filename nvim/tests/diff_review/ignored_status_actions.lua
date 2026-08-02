package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local actions = require("diff_review.views.status.actions")
local ignored_path_store = require("diff_review.views.status.ignored_path_store")
local index_mutation = require("diff_review.git.index_mutation")
local mutation_coordinator = require("diff_review.git.mutation_coordinator")
local session = require("diff_review.session")
local status_sync = require("diff_review.views.status.status_sync")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

---@param root string
---@param relative_path string
---@param section_name "unstaged"|"ignored"
---@return DiffReviewStatusEntry
local function hunk_entry(root, relative_path, section_name)
  local filename = vim.fs.joinpath(root, relative_path)
  local file = {
    filename = filename,
    relpath = relative_path,
    section_name = section_name,
    git_status = "M",
    hunks = {},
  }
  local first_hunk = {
    file = relative_path,
    abs_file = filename,
    section_name = section_name,
    staged = false,
    git_status = "M",
    diff = "@@ -1 +1 @@\n-old\n+new",
  }
  local second_hunk = vim.deepcopy(first_hunk)
  second_hunk.diff = "@@ -8 +8 @@\n-old two\n+new two"
  file.hunks = { first_hunk, second_hunk }
  return { kind = "hunk", file = file, hunk = first_hunk }
end

local function run()
  local root = "D:/repo"
  local writes = {} ---@type string[]
  ignored_path_store.reset_for_test()
  ignored_path_store.set_io_for_test({
    read_async = function(_, callback) callback(nil, nil) end,
    write_atomic_async = function(_, content, callback)
      writes[#writes + 1] = content
      callback(nil)
    end,
  })
  ignored_path_store.load_async(root, function() end)
  session.status = { cwd = root }

  local original_reproject = status_sync.reproject_ignored
  local original_configure_root = status_sync.configure_root
  local original_apply_optimistic = status_sync.apply_optimistic
  local original_recovering = mutation_coordinator.recovering
  local original_enqueue = mutation_coordinator.enqueue
  local original_execute_async = index_mutation.execute_async
  local reproject_count = 0
  local optimistic_entry_list = nil ---@type DiffReviewStatusEntry[]?
  local mutation_request = nil ---@type DiffReviewIndexMutationRequest?
  local next_completed_target_count = nil ---@type integer?

  status_sync.reproject_ignored = function() reproject_count = reproject_count + 1 end
  status_sync.configure_root = function() end
  status_sync.apply_optimistic = function(_, _, entry_list)
    optimistic_entry_list = entry_list
  end
  mutation_coordinator.recovering = function() return false end
  index_mutation.execute_async = function(_, request, callback)
    mutation_request = request
    local completed_target_list = {}
    local failed_target_list = {}
    local completed_target_count = next_completed_target_count or #request.target_list
    next_completed_target_count = nil
    for index, target in ipairs(request.target_list) do
      local target_list = index <= completed_target_count and completed_target_list or failed_target_list
      target_list[#target_list + 1] = target
    end
    callback({
      ok = #failed_target_list == 0,
      count = #completed_target_list,
      file_count = #completed_target_list,
      hunk_count = 0,
      completed_target_list = completed_target_list,
      failed_target_list = failed_target_list,
    })
  end
  mutation_coordinator.enqueue = function(_, task_spec)
    local task = { id = 11, burst_id = 22 }
    task_spec.on_enqueue(task)
    task_spec.execute(function(result)
      task_spec.on_complete(result, task)
    end)
    return task.id, task.burst_id, nil
  end

  local ignored_entry = hunk_entry(root, "src/alpha.lua", "unstaged")
  actions._status_ignore_entries({ ignored_entry })
  assert_true(ignored_path_store.contains(root, "src/alpha.lua"), "I did not create a virtual marker")
  assert_equal(reproject_count, 1, "I did not reproject exactly once")

  ignored_entry.file.section_name = "ignored"
  ignored_entry.hunk.section_name = "ignored"
  actions._status_unstage_entries({ ignored_entry })
  assert_true(not ignored_path_store.contains(root, "src/alpha.lua"), "U did not remove the virtual marker")
  assert_true(mutation_request == nil, "U from Ignored executed Git")

  actions._status_ignore_entries({ hunk_entry(root, "src/alpha.lua", "unstaged") })
  local stage_entry = hunk_entry(root, "src/alpha.lua", "ignored")
  actions._status_stage_entries({ stage_entry })
  assert_true(mutation_request ~= nil, "S from Ignored did not execute Git")
  assert_equal(#mutation_request.target_list, 1, "S from an ignored hunk produced multiple targets")
  assert_equal(mutation_request.target_list[1].kind, "tracked_file", "S from an ignored hunk did not stage the whole file")
  assert_equal(#optimistic_entry_list, 1, "S from Ignored produced multiple optimistic entries")
  assert_equal(optimistic_entry_list[1].kind, "file", "S from an ignored hunk projected a hunk move")
  assert_true(not ignored_path_store.contains(root, "src/alpha.lua"), "successful S retained the ignored marker")

  actions._status_ignore_entries({
    hunk_entry(root, "src/beta.lua", "unstaged"),
    hunk_entry(root, "src/gamma.lua", "unstaged"),
  })
  next_completed_target_count = 1
  actions._status_stage_entries({
    hunk_entry(root, "src/beta.lua", "ignored"),
    hunk_entry(root, "src/gamma.lua", "ignored"),
  })
  assert_true(not ignored_path_store.contains(root, "src/beta.lua"), "partial S retained its completed marker")
  assert_true(ignored_path_store.contains(root, "src/gamma.lua"), "partial S removed its failed marker")
  assert_true(#writes >= 3, "virtual transitions did not persist their final state")

  status_sync.reproject_ignored = original_reproject
  status_sync.configure_root = original_configure_root
  status_sync.apply_optimistic = original_apply_optimistic
  mutation_coordinator.recovering = original_recovering
  mutation_coordinator.enqueue = original_enqueue
  index_mutation.execute_async = original_execute_async
  ignored_path_store.reset_for_test()
  session.status = nil
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
