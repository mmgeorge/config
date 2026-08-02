package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local section_map = require("diff_review.views.status.section_map")
local git_data = require("diff_review.git.git_data")
local status_head = require("diff_review.views.status.status_head")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

---@param filename string
---@param label string
---@param staged boolean
---@param git_status string
---@param position integer
---@return DiffReviewHunk
local function status_hunk(filename, label, staged, git_status, position)
  return {
    file = filename:match("[^/]+$") or filename,
    filename = filename,
    section_name = staged and "staged" or "unstaged",
    pos = position,
    diff = "@@ -" .. position .. " +" .. position .. " @@\n-" .. label .. "-old\n+" .. label .. "-new",
    staged = staged,
    context_text = label,
    git_status = git_status,
    added = 1,
    removed = 1,
  }
end

---@param filename string
---@param section_name DiffReviewStatusStageSectionName
---@param git_status string
---@param untracked boolean
---@param hunk_list DiffReviewHunk[]
---@return DiffReviewStatusFile
local function status_file(filename, section_name, git_status, untracked, hunk_list)
  return {
    filename = filename,
    relpath = filename:match("[^/]+$") or filename,
    section_name = section_name,
    added = #hunk_list,
    removed = #hunk_list,
    hunks = hunk_list,
    untracked = untracked,
    status = "",
    git_status = git_status,
  }
end

---@param name DiffReviewStatusSectionName
---@param file_list DiffReviewStatusFile[]
---@return DiffReviewStatusSection
local function status_section(name, file_list)
  local title = name == "staged" and "Staged changes" or "Unstaged changes"
  local file_by_name = {}
  for _, file in ipairs(file_list) do file_by_name[file.filename] = file end
  return {
    name = name,
    title = title,
    default_folded = false,
    files = file_list,
    files_by_name = file_by_name,
  }
end

---@param sections DiffReviewStatusSection[]
---@param name string
---@return DiffReviewStatusSection?
local function find_section(sections, name)
  for _, section in ipairs(sections) do
    if section.name == name then return section end
  end
  return nil
end

---@param sections DiffReviewStatusSection[]
---@param section_name string
---@param filename string
---@return DiffReviewStatusFile?
local function find_file(sections, section_name, filename)
  local section = find_section(sections, section_name)
  return section and section.files_by_name[filename] or nil
end

---@param sections DiffReviewStatusSection[]
---@param section_name string
---@param filename string
---@return DiffReviewStatusFile
local function required_file(sections, section_name, filename)
  local file = find_file(sections, section_name, filename)
  if not file then error(("missing %s file %s"):format(section_name, filename), 2) end
  return file
end

---@param file_list DiffReviewPathStatusFileSnapshot[]
---@return DiffReviewPathStatusSnapshot
local function status_snapshot(file_list)
  local status_record_list = {}
  local status_record_by_path = {}
  local file_by_path = {}
  for _, file_snapshot in ipairs(file_list) do
    status_record_list[#status_record_list + 1] = file_snapshot.status_record
    status_record_by_path[file_snapshot.path] = file_snapshot.status_record
    file_by_path[file_snapshot.path] = file_snapshot
  end
  return {
    root = "D:/repo",
    full_repository = false,
    requested_path_list = {},
    affected_path_list = {},
    affected_file_list = {},
    status_record_list = status_record_list,
    status_record_by_path = status_record_by_path,
    file_list = file_list,
    file_by_path = file_by_path,
    file_diffs = {},
    file_hunk_staged = {},
    untracked_by_file = {},
    unstaged_diff_by_path = {},
    staged_diff_by_path = {},
    status_output = "",
    unstaged_output = "",
    staged_output = "",
  }
end

local function assert_semantic_hunk_move_uses_value_identity()
  local filename = "D:/repo/alpha.lua"
  local first_hunk = status_hunk(filename, "first", false, "M", 4)
  local second_hunk = status_hunk(filename, "second", false, "M", 20)
  local file = status_file(filename, "unstaged", "M", false, { first_hunk, second_hunk })
  local confirmed = { status_section("unstaged", { file }) }
  local detached_file = vim.deepcopy(file)
  local detached_hunk = vim.deepcopy(first_hunk)
  local move = section_map.capture_move({ { kind = "hunk", file = detached_file, hunk = detached_hunk } }, "staged")

  detached_file.filename = "D:/repo/mutated.lua"
  detached_hunk.diff = "mutated after capture"
  local projected = section_map.apply_move(confirmed, move)

  assert_true(find_file(confirmed, "unstaged", filename) == file, "reducer mutated its confirmed input")
  assert_equal(#required_file(confirmed, "unstaged", filename).hunks, 2, "confirmed hunks changed during projection")
  assert_equal(#required_file(projected, "unstaged", filename).hunks, 1, "projection did not remove the selected hunk")
  assert_equal(required_file(projected, "unstaged", filename).hunks[1].context_text, "second", "projection removed the wrong hunk")
  assert_equal(required_file(projected, "staged", filename).hunks[1].context_text, "first", "projection lost the selected hunk")
end

local function assert_added_file_unstages_as_untracked()
  local filename = "D:/repo/new.txt"
  local hunk = status_hunk(filename, "new", true, "A", 1)
  local file = status_file(filename, "staged", "A", false, { hunk })
  local confirmed = { status_section("staged", { file }) }
  local move = section_map.capture_move({ { kind = "file", file = vim.deepcopy(file) } }, "unstaged")
  local projected = section_map.apply_move(confirmed, move)
  local unstaged_file = required_file(projected, "unstaged", filename)

  assert_true(unstaged_file.untracked, "unstaging an added file did not restore untracked identity")
  assert_equal(unstaged_file.git_status, "??", "unstaging an added file did not render porcelain untracked status")
  assert_equal(unstaged_file.hunks[1].git_status, "??", "unstaged added hunk kept staged status")
end

local function assert_path_replacement_is_authoritative_and_local()
  local alpha = "D:/repo/alpha.lua"
  local beta = "D:/repo/beta.lua"
  local alpha_file = status_file(alpha, "unstaged", "M", false, { status_hunk(alpha, "alpha", false, "M", 2) })
  local beta_file = status_file(beta, "unstaged", "M", false, { status_hunk(beta, "beta", false, "M", 3) })
  local confirmed = { status_section("unstaged", { alpha_file, beta_file }) }
  local snapshot_alpha = status_file(alpha, "staged", "M", false, { status_hunk(alpha, "alpha", true, "M", 2) })
  local snapshot_sections = { status_section("staged", { snapshot_alpha }) }
  local replaced = section_map.replace_paths(confirmed, snapshot_sections, { [alpha] = true })

  assert_true(find_file(replaced, "unstaged", alpha) == nil, "path replacement retained stale source data")
  assert_true(find_file(replaced, "staged", alpha) ~= nil, "path replacement omitted authoritative target data")
  assert_true(find_file(replaced, "unstaged", beta) ~= nil, "path replacement removed an unrelated file")
  assert_true(find_file(confirmed, "unstaged", alpha) ~= nil, "path replacement mutated the confirmed baseline")

  local cleaned = section_map.replace_paths(replaced, {}, { [alpha] = true })
  assert_true(find_file(cleaned, "staged", alpha) == nil, "empty authoritative path snapshot did not remove a clean file")
  assert_true(find_file(cleaned, "unstaged", beta) ~= nil, "clean path replacement removed an unrelated file")
end

local function assert_snapshot_conversion_and_semantic_equivalence()
  local filename = "D:/repo/partial.lua"
  local unstaged_hunk = status_hunk(filename, "worktree", false, "M", 8)
  local staged_hunk = status_hunk(filename, "index", true, "M", 2)
  local snapshot = status_snapshot({
      {
        path = "partial.lua",
        abs_file = filename,
        status_record = {
          kind = "ordinary",
          path = "partial.lua",
          xy = "MM",
          index_status = "M",
          worktree_status = "M",
          staged = true,
          unstaged = true,
          untracked = false,
          added = false,
          deleted = false,
          renamed = false,
        },
        unstaged_hunk_list = { unstaged_hunk },
        staged_hunk_list = { staged_hunk },
        unstaged_diff = unstaged_hunk.diff,
        staged_diff = staged_hunk.diff,
        combined_diff = unstaged_hunk.diff .. "\n" .. staged_hunk.diff,
        staged_flag_list = { false, true },
      },
  })
  local sections = section_map.sections_from_snapshot(snapshot)

  assert_equal(required_file(sections, "unstaged", filename).relpath, "partial.lua", "snapshot conversion lost the repository path")
  assert_equal(required_file(sections, "staged", filename).hunks[1].staged, true, "snapshot conversion lost staged hunk state")

  local render_enriched = vim.deepcopy(sections)
  rawset(render_enriched[1].files[1], "diff_source_id", "render-source")
  rawset(render_enriched[1].files[1].hunks[1], "layout", { row = 200 })
  assert_true(section_map.equivalent(sections, render_enriched), "semantic comparison included render-owned metadata")
  render_enriched[1].files[1].hunks[1].diff = "different patch"
  assert_true(not section_map.equivalent(sections, render_enriched), "semantic comparison ignored visible hunk data")
end

local function assert_normal_stage_matches_authoritative_snapshot()
  local filename = "D:/repo/equivalent.lua"
  local hunk = status_hunk(filename, "equivalent", false, "M", 7)
  local file = status_file(filename, "unstaged", "M", false, { hunk })
  file.status = "-7 +7"
  local confirmed = { status_section("unstaged", { file }) }
  local move = section_map.capture_move({ { kind = "hunk", file = file, hunk = hunk } }, "staged")
  local optimistic = section_map.apply_move(confirmed, move)
  local authoritative = section_map.sections_from_snapshot(status_snapshot({
      {
        path = "equivalent.lua",
        abs_file = filename,
        status_record = {
          kind = "ordinary",
          path = "equivalent.lua",
          xy = "M.",
          index_status = "M",
          worktree_status = ".",
          staged = true,
          unstaged = false,
          untracked = false,
          added = false,
          deleted = false,
          renamed = false,
        },
        unstaged_hunk_list = {},
        staged_hunk_list = { vim.deepcopy(hunk) },
        unstaged_diff = false,
        staged_diff = hunk.diff,
        combined_diff = hunk.diff,
        staged_flag_list = { true },
      },
  }))

  assert_true(section_map.equivalent(optimistic, authoritative), "normal staged projection differed from authoritative status data")
end

local function assert_load_failure_completes_without_empty_sections()
  local original_collector = git_data._collect_items_from_git
  local original_head_loader = status_head._status_head_lines_async
  local expected_error = { kind = "command", message = "snapshot failed" } ---@type DiffReviewPathStatusSnapshotError
  local callback_count = 0
  local callback_result = nil ---@type DiffReviewStatusLoadResult?
  git_data._collect_items_from_git = function(_, callback) callback(nil, expected_error) end
  status_head._status_head_lines_async = function() end

  section_map._status_load_async("D:/repo", function(result)
    callback_count = callback_count + 1
    callback_result = result
  end)

  git_data._collect_items_from_git = original_collector
  status_head._status_head_lines_async = original_head_loader
  assert_equal(callback_count, 1, "status load failure did not complete exactly once")
  assert_true(callback_result and callback_result.error == expected_error, "status load failure lost the collector error")
  assert_true(callback_result and callback_result.sections == nil, "status load failure synthesized empty sections")
end

local function assert_ignored_overlay_moves_only_unstaged_whole_file()
  local ignored_filename = "D:/repo/ignored.lua"
  local staged_filename = "D:/repo/staged.lua"
  local visible_filename = "D:/repo/visible.lua"
  local ignored_file = status_file(ignored_filename, "unstaged", "M", false, {
    status_hunk(ignored_filename, "first", false, "M", 2),
    status_hunk(ignored_filename, "second", false, "M", 20),
  })
  ignored_file.relpath = "ignored.lua"
  local staged_file = status_file(staged_filename, "staged", "M", false, {
    status_hunk(staged_filename, "staged", true, "M", 4),
  })
  staged_file.relpath = "staged.lua"
  local visible_file = status_file(visible_filename, "unstaged", "M", false, {
    status_hunk(visible_filename, "visible", false, "M", 8),
  })
  visible_file.relpath = "visible.lua"

  local projected = section_map.apply_ignored_paths({
    status_section("unstaged", { ignored_file, visible_file }),
    status_section("staged", { staged_file }),
  }, { ["ignored.lua"] = true, ["staged.lua"] = true })

  assert_true(find_file(projected, "unstaged", ignored_filename) == nil, "ignored file remained in Unstaged")
  assert_true(find_section(projected, "ignored").default_folded, "ignored section did not default to folded")
  assert_equal(#required_file(projected, "ignored", ignored_filename).hunks, 2, "ignored overlay lost whole-file hunks")
  assert_true(find_file(projected, "unstaged", visible_filename) ~= nil, "ignored overlay moved an unrelated file")
  assert_true(find_file(projected, "staged", staged_filename) ~= nil, "ignored overlay moved a staged file")
  assert_true(find_file(projected, "ignored", staged_filename) == nil, "ignored overlay shadowed staged Git state")
  assert_true(find_file({ status_section("unstaged", { ignored_file }) }, "unstaged", ignored_filename) ~= nil, "ignored overlay mutated its input")
end

local function run()
  assert_semantic_hunk_move_uses_value_identity()
  assert_added_file_unstages_as_untracked()
  assert_path_replacement_is_authoritative_and_local()
  assert_snapshot_conversion_and_semantic_equivalence()
  assert_normal_stage_matches_authoritative_snapshot()
  assert_ignored_overlay_moves_only_unstaged_whole_file()
  assert_load_failure_completes_without_empty_sections()
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
