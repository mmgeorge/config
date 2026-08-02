vim.loader.enable(false)

local git_backend = require("diff_review.git.git_backend")
local git_data = require("diff_review.git.git_data")
local session = require("diff_review.session")
local status_snapshot = require("diff_review.git.status_snapshot")
local section_map = require("diff_review.views.status.section_map")

local original_backend = git_backend.current
local original_notify = vim.notify
local scratch_root = vim.fn.tempname()
local call_list = {}
local notification_list = {}
local response_mode = "success"
local status_output_override = nil
local unstaged_output_override = nil
local staged_output_override = nil

local zero_oid = string.rep("0", 40)
local first_oid = string.rep("1", 40)
local second_oid = string.rep("2", 40)
local third_oid = string.rep("3", 40)

local status_output = table.concat({
  "1 .M N... 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " modified file.txt",
  "1 A. N... 000000 100644 100644 " .. zero_oid .. " " .. second_oid .. " added-empty.txt",
  "1 D. N... 100644 000000 000000 " .. first_oid .. " " .. zero_oid .. " deleted.txt",
  "2 R. N... 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " R100 renamed.txt",
  "original.txt",
  "? untracked file.txt",
  "? second untracked.txt",
  "1 MM N... 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " mixed.txt",
  "u UU N... 100644 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " " .. third_oid .. " conflict.txt",
}, "\0") .. "\0"

local unstaged_output = table.concat({
  "diff --git a/modified file.txt b/modified file.txt",
  "index 1111111..2222222 100644",
  "--- a/modified file.txt",
  "+++ b/modified file.txt",
  "@@ -1 +1 @@",
  "-old modified",
  "+new modified",
  "diff --git a/mixed.txt b/mixed.txt",
  "index 1111111..2222222 100644",
  "--- a/mixed.txt",
  "+++ b/mixed.txt",
  "@@ -1 +1 @@",
  "-mixed base",
  "+mixed-worktree",
}, "\n")

local staged_output = table.concat({
  "diff --git a/deleted.txt b/deleted.txt",
  "deleted file mode 100644",
  "index 1111111..0000000",
  "--- a/deleted.txt",
  "+++ /dev/null",
  "@@ -1 +0,0 @@",
  "-deleted content",
  "diff --git a/original.txt b/renamed.txt",
  "similarity index 50%",
  "rename from original.txt",
  "rename to renamed.txt",
  "index 1111111..2222222 100644",
  "--- a/original.txt",
  "+++ b/renamed.txt",
  "@@ -1 +1 @@",
  "-old name",
  "+new name",
  "diff --git a/mixed.txt b/mixed.txt",
  "index 0000000..1111111 100644",
  "--- a/mixed.txt",
  "+++ b/mixed.txt",
  "@@ -1 +1 @@",
  "-mixed head",
  "+mixed-index",
}, "\n")

local requested_path_list = {
  "modified file.txt",
  "added-empty.txt",
  "deleted.txt",
  "renamed.txt",
  "untracked file.txt",
  "second untracked.txt",
  "mixed.txt",
  "conflict.txt",
}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

local function command_has(command, argument)
  for _, command_argument in ipairs(command) do
    if command_argument == argument then return true end
  end
  return false
end

local function response_for(command)
  if response_mode == "failure" then
    if command_has(command, "status") then
      return { ok = false, code = 128, stdout = "", stderr = "fatal: status failed", output = "fatal: status failed" }
    end
    if command_has(command, "--cached") then
      return {
        ok = false,
        code = 129,
        stdout = "partial staged output",
        stderr = "fatal: staged failed",
        output = "partial staged output\nfatal: staged failed",
      }
    end
    return { ok = true, code = 0, stdout = "", stderr = "", output = "" }
  end

  if response_mode == "empty" then
    return { ok = true, code = 0, stdout = "", stderr = "", output = "" }
  end
  if command_has(command, "status") then
    local output = status_output_override or status_output
    return { ok = true, code = 0, stdout = output, stderr = "", output = output }
  end
  if command_has(command, "--cached") then
    local output = staged_output_override or staged_output
    return { ok = true, code = 0, stdout = output, stderr = "", output = output }
  end
  local output = unstaged_output_override or unstaged_output
  return { ok = true, code = 0, stdout = output, stderr = "", output = output }
end

---@type DiffReviewGitBackend
local backend = {}

function backend.system_async(command, _, callback)
  call_list[#call_list + 1] = vim.deepcopy(command)
  local result = response_for(command)
  vim.schedule(function() callback(result) end)
end

local function collect(path_list)
  local collected_snapshot = nil
  local collected_error = nil
  local finished = false
  status_snapshot.collect_async(scratch_root, path_list, function(snapshot, snapshot_error)
    collected_snapshot = snapshot
    collected_error = snapshot_error
    finished = true
  end)
  assert_true(vim.wait(3000, function() return finished end, 10), "status snapshot collection timed out")
  return collected_snapshot, collected_error
end

local function collect_items()
  local collected_item_list = nil
  local collected_error = nil
  local collected_snapshot = nil
  local finished = false
  git_data._collect_items_from_git(scratch_root, function(item_list, snapshot_error, snapshot)
    collected_item_list = item_list
    collected_error = snapshot_error
    collected_snapshot = snapshot
    finished = true
  end, { skip_pre_render = true, skip_ts_context = true })
  assert_true(vim.wait(3000, function() return finished end, 10), "full-load snapshot adapter timed out")
  return collected_item_list, collected_error, collected_snapshot
end

local function assert_path_scoped_commands()
  assert_equals(#call_list, 3, "path snapshot must issue exactly three Git commands")
  assert_equals(call_list[1], vim.list_extend({
    "git", "--no-optional-locks", "-C", scratch_root,
    "status", "--porcelain=v2", "-z", "--untracked-files=all", "--",
  }, vim.deepcopy(requested_path_list)), "status command must use porcelain v2 with the complete path set")
  assert_equals(call_list[2], vim.list_extend({
    "git", "--no-optional-locks", "-C", scratch_root,
    "-c", "core.quotepath=false", "diff", "--no-color", "--no-ext-diff", "--unified=0", "--",
  }, vim.deepcopy(requested_path_list)), "unstaged command must request zero-context paths")
  assert_equals(call_list[3], vim.list_extend({
    "git", "--no-optional-locks", "-C", scratch_root,
    "-c", "core.quotepath=false", "diff", "--no-color", "--no-ext-diff", "--unified=0", "--cached", "--",
  }, vim.deepcopy(requested_path_list)), "staged command must request zero-context cached paths")
end

local function assert_snapshot_content(snapshot)
  assert_true(snapshot ~= nil, "successful collection must return a snapshot")
  assert_true(not snapshot.full_repository, "non-empty paths must produce a path-scoped snapshot")
  assert_equals(snapshot.requested_path_list, requested_path_list, "snapshot must preserve normalized path scope")
  assert_equals(#snapshot.status_record_list, 8, "snapshot must retain every porcelain record")

  local modified = snapshot.file_by_path["modified file.txt"]
  assert_equals(modified.status_record.kind, "ordinary", "ordinary record kind was lost")
  assert_equals(modified.status_record.index_status, ".", "ordinary index status was parsed incorrectly")
  assert_equals(modified.status_record.worktree_status, "M", "ordinary worktree status was parsed incorrectly")
  assert_true(modified.unstaged_diff:find("new modified", 1, true) ~= nil, "ordinary unstaged diff was lost")
  assert_equals(modified.staged_diff, false, "ordinary unstaged file must not gain a staged source")
  assert_equals(modified.unstaged_hunk_list[1].filename, modified.abs_file, "hunks must carry the absolute filename")

  local added = snapshot.file_by_path["added-empty.txt"]
  assert_true(added.status_record.added and added.status_record.staged, "empty added file must survive without a hunk")
  assert_equals(added.combined_diff, false, "empty added file must use the checked-empty diff sentinel")
  assert_equals(snapshot.file_diffs[added.abs_file], false, "session cache must retain the empty-file sentinel")

  local deleted = snapshot.file_by_path["deleted.txt"]
  assert_true(deleted.status_record.deleted, "deleted status was not parsed")
  assert_equals(deleted.staged_hunk_list[1].git_status, "D", "deleted hunk lost its staged Git status")

  local renamed = snapshot.file_by_path["renamed.txt"]
  assert_equals(renamed.status_record.kind, "renamed", "rename record kind was lost")
  assert_equals(renamed.original_path, "original.txt", "rename source path was not paired from the next NUL field")
  assert_equals(renamed.status_record.score, "R100", "rename score was not parsed")
  assert_equals(renamed.staged_hunk_list[1].git_original_file, "original.txt", "rename hunk lost its source path")
  assert_true(vim.tbl_contains(snapshot.affected_path_list, "original.txt"), "rename source must join the clear boundary")

  local untracked = snapshot.file_by_path["untracked file.txt"]
  assert_true(untracked.status_record.untracked, "untracked record was not parsed")
  assert_true(untracked.unstaged_diff:find("+untracked first", 1, true) ~= nil, "untracked source was not synthesized")
  assert_equals(untracked.staged_flag_list, { false }, "untracked synthetic hunk must remain unstaged")
  assert_equals(snapshot.untracked_by_file[untracked.abs_file], "untracked file.txt", "untracked cache mapping was lost")
  local second_untracked = snapshot.file_by_path["second untracked.txt"]
  assert_true(
    second_untracked.unstaged_diff:find("+second untracked content", 1, true) ~= nil,
    "parallel untracked reads crossed path content"
  )

  local mixed = snapshot.file_by_path["mixed.txt"]
  assert_equals(mixed.staged_flag_list, { false, true }, "same-position hunks must keep unstaged before staged")
  local unstaged_position = mixed.combined_diff:find("mixed-worktree", 1, true)
  local staged_position = mixed.combined_diff:find("mixed-index", 1, true)
  assert_true(unstaged_position < staged_position, "combined diff order must match staged flags")
  assert_equals(snapshot.file_hunk_staged[mixed.abs_file], { false, true }, "session staged flags must be snapshot-ready")

  local conflict = snapshot.file_by_path["conflict.txt"]
  assert_equals(conflict.status_record.kind, "unmerged", "unmerged porcelain record was not parsed")
  assert_equals(conflict.status_record.xy, "UU", "unmerged status pair was not preserved")
  assert_equals(snapshot.status_output, status_output, "raw status source must remain available")
  assert_equals(snapshot.unstaged_output, unstaged_output, "raw unstaged source must remain available")
  assert_equals(snapshot.staged_output, staged_output, "raw staged source must remain available")
end

local function assert_full_repository_scope()
  call_list = {}
  response_mode = "empty"
  local snapshot, snapshot_error = collect({})
  assert_true(snapshot_error == nil, "full repository collection failed: " .. vim.inspect(snapshot_error))
  assert_true(snapshot.full_repository, "empty path list must mean full repository")
  assert_equals(snapshot.requested_path_list, {}, "full repository snapshot must retain an empty path scope")
  assert_equals(#call_list, 3, "full repository snapshot must still issue exactly three commands")
  for _, command in ipairs(call_list) do
    assert_true(not command_has(command, "--"), "full repository commands must omit the pathspec separator")
  end
end

local function assert_command_failure_detail()
  call_list = {}
  response_mode = "failure"
  local snapshot, snapshot_error = collect({ "mixed.txt" })
  assert_true(snapshot == nil, "failed Git commands must not produce an authoritative snapshot")
  assert_equals(snapshot_error.kind, "command", "Git failures must remain command errors")
  assert_equals(#snapshot_error.failure_list, 2, "each failed Git source must remain distinct")
  assert_equals(snapshot_error.failure_list[1].source, "status", "status failure order changed")
  assert_equals(snapshot_error.failure_list[1].code, 128, "status failure code was lost")
  assert_equals(snapshot_error.failure_list[1].stderr, "fatal: status failed", "status stderr was lost")
  assert_equals(snapshot_error.failure_list[2].source, "staged_diff", "staged failure order changed")
  assert_equals(snapshot_error.failure_list[2].stdout, "partial staged output", "staged stdout was lost")
  assert_equals(snapshot_error.failure_list[2].stderr, "fatal: staged failed", "staged stderr was lost")
  assert_true(snapshot_error.message:find("status: fatal: status failed", 1, true) ~= nil, "aggregate error omitted status")
  assert_true(snapshot_error.message:find("staged_diff: fatal: staged failed", 1, true) ~= nil, "aggregate error omitted staged diff")
  assert_equals(#call_list, 3, "one failure must not prevent the remaining snapshot commands")
end

local function assert_full_load_adapter()
  call_list = {}
  response_mode = "success"
  session.file_diffs = { sentinel = "preserve" }
  session.file_hunk_staged = { sentinel = { true } }
  session.untracked = { sentinel = "preserve" }
  local item_list, snapshot_error, snapshot = collect_items()
  assert_true(snapshot_error == nil, "full-load adapter failed: " .. vim.inspect(snapshot_error))
  assert_true(snapshot and snapshot.full_repository, "full-load adapter must expose its repository snapshot")
  assert_equals(#call_list, 3, "full-load adapter must replace the former five Git reads")

  local saw_added_placeholder = false
  local saw_untracked_item = false
  for _, collected_item in ipairs(item_list) do
    if collected_item.filename == snapshot.file_by_path["added-empty.txt"].abs_file
        and collected_item.item.git_status == "A"
        and collected_item.item.diff == nil then
      saw_added_placeholder = true
    end
    if collected_item.filename == snapshot.file_by_path["untracked file.txt"].abs_file
        and collected_item.item.category == "Untracked Files" then
      saw_untracked_item = true
    end
  end
  assert_true(saw_added_placeholder, "full-load adapter dropped the empty added-file placeholder")
  assert_true(saw_untracked_item, "full-load adapter changed untracked item semantics")

  assert_equals(session.file_diffs, { sentinel = "preserve" }, "collector adopted diff cache before request acceptance")
  assert_equals(
    session.file_hunk_staged,
    { sentinel = { true } },
    "collector adopted staged flags before request acceptance"
  )
  assert_equals(session.untracked, { sentinel = "preserve" }, "collector adopted untracked cache before request acceptance")

  session.file_diffs = { sentinel = "preserve" }
  session.file_hunk_staged = { sentinel = { true } }
  session.untracked = { sentinel = "preserve" }
  notification_list = {}
  call_list = {}
  response_mode = "failure"
  local failed_item_list, failed_error = collect_items()
  assert_true(failed_item_list == nil, "failed full-load adapter must not return an empty-success item list")
  assert_equals(failed_error.kind, "command", "full-load adapter lost the snapshot error kind")
  assert_equals(session.file_diffs, { sentinel = "preserve" }, "failed snapshot must not replace file diff cache")
  assert_equals(session.file_hunk_staged, { sentinel = { true } }, "failed snapshot must not replace staged flags")
  assert_equals(session.untracked, { sentinel = "preserve" }, "failed snapshot must not replace untracked cache")
  assert_equals(#notification_list, 1, "failed full-load adapter must notify exactly once")
  assert_true(
    notification_list[1]:find("status", 1, true) ~= nil and notification_list[1]:find("staged_diff", 1, true) ~= nil,
    "failure notification must distinguish each failed source"
  )
end

local function assert_parse_error_detail()
  local record_list, parse_error = status_snapshot.parse_status(
    "2 R. N... 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " R100 renamed.txt\0"
  )
  assert_true(record_list == nil, "rename without its original path must fail parsing")
  assert_true(parse_error:find("Malformed renamed status record", 1, true) ~= nil, "rename parse error lost its record kind")

  local copy_record_list, copy_error = status_snapshot.parse_status(
    "2 C. N... 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " C100 copied.txt\0source.txt\0"
  )
  assert_true(copy_error == nil, "copy status record failed to parse: " .. tostring(copy_error))
  assert_equals(copy_record_list[1].kind, "copied", "copy record must not masquerade as a rename")
  assert_true(copy_record_list[1].copied, "copy record lost its copy flag")
  assert_true(not copy_record_list[1].renamed, "copy record gained a rename flag")
  assert_true(copy_record_list[1].added, "copy destination must retain added-file semantics")

  local copy_snapshot, copy_snapshot_error = status_snapshot.build(
    scratch_root,
    { "copied.txt" },
    "2 C. N... 100644 100644 100644 " .. first_oid .. " " .. second_oid .. " C100 copied.txt\0source.txt\0",
    "",
    ""
  )
  assert_true(copy_snapshot_error == nil, "copy snapshot failed: " .. vim.inspect(copy_snapshot_error))
  assert_true(
    not vim.tbl_contains(copy_snapshot.affected_path_list, "source.txt"),
    "copy source joined the replacement boundary"
  )
  local copy_section_list = section_map.sections_from_snapshot(copy_snapshot)
  assert_equals(copy_section_list[1].files[1].path_change_kind, "copied", "section model lost copy identity")
end

local function assert_untracked_byte_semantics()
  local crlf_diff = status_snapshot._build_untracked_diff_from_bytes("crlf.txt", "first\r\nsecond\r\n")
  assert_true(crlf_diff:find("+first\r\n+second\r", 1, true) ~= nil, "untracked synthesis normalized CRLF bytes")
  assert_true(
    crlf_diff:find("\\ No newline at end of file", 1, true) == nil,
    "terminated CRLF content gained a missing-newline marker"
  )

  local unterminated_diff = status_snapshot._build_untracked_diff_from_bytes("unterminated.txt", "first\nsecond")
  assert_true(
    unterminated_diff:find("+first\n+second\n\\ No newline at end of file", 1, true) ~= nil,
    "unterminated content lost its Git patch marker"
  )
  assert_equals(status_snapshot._build_untracked_diff_from_bytes("empty.txt", ""), nil, "empty file gained a hunk")
  assert_equals(
    status_snapshot._build_untracked_diff_from_bytes("binary.dat", "first\0second"),
    nil,
    "binary file gained a text hunk"
  )
end

local function assert_untracked_read_is_async_and_one_shot()
  local original_reader = status_snapshot._read_untracked_file_async
  response_mode = "success"
  local read_finished = false
  local reader_returned = false
  original_reader(scratch_root .. "/untracked file.txt", function(content)
    assert_true(reader_returned, "untracked file callback ran synchronously")
    assert_true(content:find("untracked first", 1, true) ~= nil, "async untracked read lost file content")
    read_finished = true
  end)
  reader_returned = true
  assert_true(vim.wait(3000, function() return read_finished end, 10), "async untracked read timed out")

  status_snapshot._read_untracked_file_async = function(_, callback)
    vim.schedule(function()
      callback(nil)
      callback("late duplicate")
    end)
  end
  local callback_count = 0
  local collected_snapshot = nil
  status_snapshot.collect_async(scratch_root, { "untracked file.txt" }, function(snapshot)
    callback_count = callback_count + 1
    collected_snapshot = snapshot
  end)
  assert_true(vim.wait(3000, function() return callback_count > 0 end, 10), "mocked untracked read timed out")
  vim.wait(50)
  status_snapshot._read_untracked_file_async = original_reader

  assert_equals(callback_count, 1, "duplicate disk callback completed the snapshot twice")
  assert_true(collected_snapshot ~= nil, "read failure discarded authoritative status")
  assert_equals(
    collected_snapshot.file_by_path["untracked file.txt"].combined_diff,
    false,
    "failed disk read fell back to a blocking file read"
  )
end

local function assert_untracked_read_concurrency_is_bounded()
  local original_reader = status_snapshot._read_untracked_file_async
  local record_text_list = {}
  local path_list = {}
  for file_index = 1, 20 do
    local relpath = ("bulk-%02d.txt"):format(file_index)
    record_text_list[#record_text_list + 1] = "? " .. relpath .. "\0"
    path_list[#path_list + 1] = relpath
  end
  status_output_override = table.concat(record_text_list)
  unstaged_output_override = ""
  staged_output_override = ""
  response_mode = "success"

  local active_read_count = 0
  local maximum_read_count = 0
  local completed_read_count = 0
  status_snapshot._read_untracked_file_async = function(_, callback)
    active_read_count = active_read_count + 1
    maximum_read_count = math.max(maximum_read_count, active_read_count)
    vim.defer_fn(function()
      active_read_count = active_read_count - 1
      completed_read_count = completed_read_count + 1
      callback("content\n")
    end, 1)
  end

  local snapshot, snapshot_error = collect(path_list)
  status_snapshot._read_untracked_file_async = original_reader
  status_output_override = nil
  unstaged_output_override = nil
  staged_output_override = nil

  assert_true(snapshot_error == nil, "bounded untracked collection failed: " .. vim.inspect(snapshot_error))
  assert_equals(completed_read_count, 20, "bounded pool dropped an untracked read")
  assert_equals(#snapshot.file_list, 20, "bounded pool dropped an untracked snapshot file")
  assert_true(maximum_read_count <= 16, "untracked reads exceeded the file-descriptor bound")
  assert_true(maximum_read_count > 1, "untracked reads became serial")
end

local function run()
  assert_equals(vim.fn.mkdir(scratch_root, "p"), 1, "failed to create snapshot scratch root")
  assert_equals(
    vim.fn.writefile({ "untracked first", "untracked second" }, scratch_root .. "/untracked file.txt"),
    0,
    "failed to create untracked fixture"
  )
  assert_equals(
    vim.fn.writefile({ "second untracked content" }, scratch_root .. "/second untracked.txt"),
    0,
    "failed to create second untracked fixture"
  )
  git_backend.set_backend(backend)
  vim.notify = function(message)
    notification_list[#notification_list + 1] = tostring(message)
  end

  response_mode = "success"
  call_list = {}
  local snapshot, snapshot_error = collect(requested_path_list)
  assert_true(snapshot_error == nil, "path snapshot failed: " .. vim.inspect(snapshot_error))
  assert_path_scoped_commands()
  assert_snapshot_content(snapshot)
  assert_full_repository_scope()
  assert_command_failure_detail()
  assert_full_load_adapter()
  assert_parse_error_detail()
  assert_untracked_byte_semantics()
  assert_untracked_read_is_async_and_one_shot()
  assert_untracked_read_concurrency_is_bounded()
end

local ok, error_message = xpcall(run, debug.traceback)
git_backend.set_backend(original_backend)
vim.notify = original_notify
vim.fn.delete(scratch_root, "rf")
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit")
end
vim.cmd("qa!")
