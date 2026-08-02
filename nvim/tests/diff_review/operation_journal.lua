package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local journal_model = require("diff_review.views.status.operation_journal")

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
  local file_by_name = {}
  for _, file in ipairs(file_list) do file_by_name[file.filename] = file end
  return {
    name = name,
    title = name == "staged" and "Staged changes" or "Unstaged changes",
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

---@param file DiffReviewStatusFile
---@param label string
---@return DiffReviewStatusEntry
local function hunk_entry(file, label)
  for _, hunk in ipairs(file.hunks) do
    if hunk.context_text == label then return { kind = "hunk", file = file, hunk = hunk } end
  end
  error("missing hunk " .. label)
end

local function assert_success_waits_for_authoritative_commit()
  local filename = "D:/repo/shared.lua"
  local first_hunk = status_hunk(filename, "first", false, "M", 3)
  local second_hunk = status_hunk(filename, "second", false, "M", 14)
  local file = status_file(filename, "unstaged", "M", false, { first_hunk, second_hunk })
  local confirmed = { status_section("unstaged", { file }) }
  local initial_journal = journal_model.new(confirmed)
  local first_journal = journal_model.append(initial_journal, "burst-a", { hunk_entry(file, "first") }, "staged")
  local after_first = journal_model.project(first_journal)
  local projected_unstaged = required_file(after_first, "unstaged", filename)
  local layered_journal = journal_model.append(first_journal, "burst-b", { hunk_entry(projected_unstaged, "second") }, "staged")
  local succeeded_journal = journal_model.mark_succeeded(layered_journal, "burst-a")

  assert_true(find_file(journal_model.project(succeeded_journal), "staged", filename) ~= nil, "successful burst vanished before snapshot commit")
  assert_equal(#initial_journal.layer_list, 0, "append mutated the prior journal")
  assert_equal(#succeeded_journal.layer_list, 2, "success removed its optimistic layer")

  local committed_journal = journal_model.commit(succeeded_journal, after_first, "burst-a")
  local committed_projection = journal_model.project(committed_journal)
  assert_equal(#committed_journal.layer_list, 1, "commit removed a later optimistic burst")
  assert_equal(committed_journal.layer_list[1].burst_id, "burst-b", "commit retained the wrong burst")
  assert_equal(#required_file(committed_projection, "staged", filename).hunks, 2, "later burst did not replay over committed baseline")
  assert_true(journal_model.pending_path_set(committed_journal)[filename], "remaining same-path burst was not re-invalidated")
end

local function assert_failed_burst_replays_survivors()
  local filename = "D:/repo/survivor.lua"
  local first_hunk = status_hunk(filename, "first", false, "M", 3)
  local second_hunk = status_hunk(filename, "second", false, "M", 14)
  local file = status_file(filename, "unstaged", "M", false, { first_hunk, second_hunk })
  local journal = journal_model.new({ status_section("unstaged", { file }) })
  journal = journal_model.append(journal, 1, { hunk_entry(file, "first") }, "staged")
  local after_first = journal_model.project(journal)
  journal = journal_model.append(journal, 2, { hunk_entry(required_file(after_first, "unstaged", filename), "second") }, "staged")
  journal = journal_model.remove_burst(journal, 1)
  local projected = journal_model.project(journal)

  assert_equal(#required_file(projected, "staged", filename).hunks, 1, "surviving burst did not replay")
  assert_equal(required_file(projected, "staged", filename).hunks[1].context_text, "second", "failure replay staged the removed burst")
  assert_equal(required_file(projected, "unstaged", filename).hunks[1].context_text, "first", "failure replay lost confirmed data")
end

local function assert_stage_then_unstage_preserves_untracked_status()
  local filename = "D:/repo/new.txt"
  local hunk = status_hunk(filename, "new", false, "??", 1)
  local file = status_file(filename, "unstaged", "??", true, { hunk })
  local journal = journal_model.new({ status_section("unstaged", { file }) })
  journal = journal_model.append(journal, "stage", { { kind = "file", file = file } }, "staged")
  local staged_projection = journal_model.project(journal)
  local staged_file = required_file(staged_projection, "staged", filename)
  assert_equal(staged_file.git_status, "A", "staging an untracked file did not project added status")

  journal = journal_model.append(journal, "unstage", { { kind = "file", file = staged_file } }, "unstaged")
  local unstaged_projection = journal_model.project(journal)
  local unstaged_file = required_file(unstaged_projection, "unstaged", filename)
  assert_true(unstaged_file.untracked, "stage then unstage lost untracked identity")
  assert_equal(unstaged_file.git_status, "??", "stage then unstage did not restore porcelain untracked status")
end

local function assert_manual_baseline_keeps_active_layers()
  local filename = "D:/repo/manual.lua"
  local hunk = status_hunk(filename, "manual", false, "M", 5)
  local file = status_file(filename, "unstaged", "M", false, { hunk })
  local confirmed = { status_section("unstaged", { file }) }
  local journal = journal_model.new(confirmed)
  journal = journal_model.append(journal, "active", { hunk_entry(file, "manual") }, "staged")
  local detached_confirmed = journal_model.confirmed(journal)
  detached_confirmed[1].files[1].filename = "D:/repo/mutated.lua"
  assert_true(find_file(journal_model.confirmed(journal), "unstaged", filename) ~= nil, "confirmed accessor exposed mutable baseline state")
  local refreshed = journal_model.commit(journal, vim.deepcopy(confirmed), nil)

  assert_true(journal_model.has_layers(refreshed), "manual baseline replacement discarded an active layer")
  assert_true(find_file(journal_model.project(refreshed), "staged", filename) ~= nil, "active layer did not replay over manual baseline")

  local reset = journal_model.reset(refreshed, confirmed)
  assert_true(not journal_model.has_layers(reset), "full authoritative reset retained stale operation layers")
  assert_true(find_file(journal_model.project(reset), "unstaged", filename) ~= nil, "full authoritative reset lost its baseline")
end

local function run()
  assert_success_waits_for_authoritative_commit()
  assert_failed_burst_replays_survivors()
  assert_stage_then_unstage_preserves_untracked_status()
  assert_manual_baseline_keeps_active_layers()
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
