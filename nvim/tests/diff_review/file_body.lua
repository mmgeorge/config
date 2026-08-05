vim.loader.enable(false)

local config = require("diff_review.infra.config")
local file_body = require("diff_review.git.file_body")
local git_backend = require("diff_review.git.git_backend")

local original_backend = git_backend.current
local original_options = config.options
local command_list = {}
local response_list = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error(("%s\nexpected: %s\nactual: %s"):format(message, vim.inspect(expected), vim.inspect(actual)), 2)
  end
end

---@type DiffReviewGitBackend
local backend = {}

function backend.system_async(command, _, callback)
  command_list[#command_list + 1] = vim.deepcopy(command)
  local response = table.remove(response_list, 1)
  assert_true(response ~= nil, "file body issued an unexpected Git command: " .. vim.inspect(command))
  vim.schedule(function() callback(response) end)
end

local function load(file)
  local result = nil
  file_body.load_async("D:/repo", file, function(loaded_result) result = loaded_result end)
  assert_true(vim.wait(1000, function() return result ~= nil end, 5), "file body load timed out")
  return result
end

local function staged_file(source, oid)
  return {
    filename = "D:/repo/example.txt",
    relpath = "example.txt",
    section_name = "staged",
    added = 0,
    removed = 0,
    hunks = {},
    untracked = false,
    status = source == "index_added" and "A." or "D.",
    git_status = source == "index_added" and "A" or "D",
    preview_state = "unloaded",
    preview_source = source,
    preview_oid = oid,
    preview_mode = "100755",
    preview_binary = false,
    line_stats_complete = false,
  }
end

local function success(stdout)
  return { code = 0, stdout = stdout, stderr = "", output = stdout }
end

local function assert_staged_addition_uses_index_blob()
  command_list = {}
  response_list = { success("first\nsecond\n") }
  local file = staged_file("index_added", string.rep("2", 40))
  local result = load(file)
  assert_equal(#command_list, 1, "staged addition issued more than one Git command")
  assert_equal(command_list[1], {
    "git", "--no-optional-locks", "-C", "D:/repo", "cat-file", "blob", string.rep("2", 40),
  }, "staged addition did not use its porcelain index object ID")
  assert_equal(result.state, "loaded", "staged addition did not load")
  assert_equal(result.added, 2, "staged addition line count changed")
  assert_equal(result.removed, 0, "staged addition gained removed lines")
  assert_equal(#result.hunks, 1, "staged addition did not synthesize one canonical hunk")
  assert_true(result.hunks[1].diff:find("new file mode 100755", 1, true) ~= nil, "staged addition lost its index mode")
end

local function assert_large_deletion_stops_before_blob_read()
  command_list = {}
  response_list = { success("0\t1001\texample.txt\0") }
  local file = staged_file("head_deleted", string.rep("1", 40))
  local result = load(file)
  assert_equal(#command_list, 1, "large deletion read a blob after crossing the preview limit")
  assert_true(vim.tbl_contains(command_list[1], "--numstat"), "large deletion skipped its count query")
  assert_true(vim.tbl_contains(command_list[1], "--cached"), "staged deletion count did not target the index diff")
  assert_equal(result.state, "omitted", "large deletion did not enter the omitted state")
  assert_equal(result.removed, 1001, "large deletion lost its exact line count")
end

local function assert_small_deletion_uses_head_blob()
  command_list = {}
  response_list = {
    success("0\t2\texample.txt\0"),
    success("first\nsecond\n"),
  }
  local file = staged_file("head_deleted", string.rep("1", 40))
  local result = load(file)
  assert_equal(#command_list, 2, "small deletion did not perform count and blob reads")
  assert_equal(command_list[2], {
    "git", "--no-optional-locks", "-C", "D:/repo", "cat-file", "blob", string.rep("1", 40),
  }, "staged deletion did not use its porcelain HEAD object ID")
  assert_equal(result.removed, 2, "small deletion line count changed")
  assert_equal(#result.hunks, 1, "small deletion did not synthesize one canonical hunk")
  assert_true(result.hunks[1].diff:find("deleted file mode 100755", 1, true) ~= nil, "small deletion lost its HEAD mode")
end

local function assert_unstaged_deletion_uses_index_blob()
  command_list = {}
  response_list = {
    success("0\t1\texample.txt\0"),
    success("index content\n"),
  }
  local file = staged_file("index_deleted", string.rep("3", 40))
  file.section_name = "unstaged"
  file.status = ".D"
  local result = load(file)
  assert_true(not vim.tbl_contains(command_list[1], "--cached"), "unstaged deletion count targeted the cached diff")
  assert_equal(command_list[2], {
    "git", "--no-optional-locks", "-C", "D:/repo", "cat-file", "blob", string.rep("3", 40),
  }, "unstaged deletion did not use its porcelain index object ID")
  assert_equal(result.removed, 1, "unstaged deletion line count changed")
end

local function assert_binary_addition_never_reads_content()
  command_list = {}
  response_list = {}
  local file = staged_file("index_added", string.rep("2", 40))
  file.preview_binary = true
  file.added = 0
  file.line_stats_complete = false
  local result = load(file)
  assert_equal(command_list, {}, "known binary addition read its blob")
  assert_true(result.binary, "known binary addition lost its binary state")
  assert_equal(result.hunks, {}, "known binary addition gained text hunks")
end

local function run()
  git_backend.set_backend(backend)
  config.options = vim.tbl_deep_extend("force", vim.deepcopy(config.defaults), {
    status_deleted_file_preview_line_limit = 1000,
  })
  assert_staged_addition_uses_index_blob()
  assert_large_deletion_stops_before_blob_read()
  assert_small_deletion_uses_head_blob()
  assert_unstaged_deletion_uses_index_blob()
  assert_binary_addition_never_reads_content()
end

local ok, error_message = xpcall(run, debug.traceback)
git_backend.set_backend(original_backend)
config.options = original_options
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit")
end
vim.cmd("qa!")
