local index_mutation = require("diff_review.git.index_mutation")
local git_backend = require("diff_review.git.git_backend")
local status_snapshot = require("diff_review.git.status_snapshot")

local test_root = vim.fn.tempname() .. "-diff-review-index-mutation-real-git"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if actual ~= expected then
    error(("%s\nexpected: %q\nactual:   %q"):format(message, expected, actual), 2)
  end
end

---@param path string
---@param content string
local function write_bytes(path, content)
  local file_handle, open_error = io.open(path, "wb")
  assert_true(file_handle ~= nil, "open failed for " .. path .. ": " .. tostring(open_error))
  local write_ok, write_error = file_handle:write(content)
  file_handle:close()
  assert_true(write_ok ~= nil, "write failed for " .. path .. ": " .. tostring(write_error))
end

---@param path string
---@return string
local function read_bytes(path)
  local file_handle, open_error = io.open(path, "rb")
  assert_true(file_handle ~= nil, "open failed for " .. path .. ": " .. tostring(open_error))
  local content = file_handle:read("*a")
  file_handle:close()
  assert_true(content ~= nil, "read failed for " .. path)
  return content
end

---@param argument_list string[]
---@return string
local function run_git_text(argument_list)
  local command = { "git", "-C", test_root }
  vim.list_extend(command, argument_list)
  local output = vim.fn.system(command)
  local exit_code = vim.v.shell_error
  assert_true(
    exit_code == 0,
    ("git %s failed with %d:\n%s"):format(table.concat(argument_list, " "), exit_code, output)
  )
  return output
end

---@param diff_text string
---@param hunk_number integer
---@return string
local function select_hunk_patch(diff_text, hunk_number)
  local line_list = vim.split(diff_text, "\n", { plain = true })
  local hunk_start_list = {}
  for line_index, line in ipairs(line_list) do
    if line:sub(1, 3) == "@@ " then hunk_start_list[#hunk_start_list + 1] = line_index end
  end
  local selected_start = hunk_start_list[hunk_number]
  assert_true(selected_start ~= nil, "missing hunk " .. hunk_number .. " in:\n" .. diff_text)
  local selected_end = (hunk_start_list[hunk_number + 1] or (#line_list + 1)) - 1
  while selected_end >= selected_start and line_list[selected_end] == "" do
    selected_end = selected_end - 1
  end

  local patch_line_list = {}
  for line_index = 1, selected_start - 1 do
    patch_line_list[#patch_line_list + 1] = line_list[line_index]
  end
  for line_index = selected_start, selected_end do
    patch_line_list[#patch_line_list + 1] = line_list[line_index]
  end
  return table.concat(patch_line_list, "\n")
end

---@param diff_text string
---@return string[]
local function changed_body_line_list(diff_text)
  local result = {}
  for _, line in ipairs(vim.split(diff_text, "\n", { plain = true })) do
    local prefix = line:sub(1, 1)
    local header_prefix = line:sub(1, 3)
    if (prefix == "+" or prefix == "-") and header_prefix ~= "+++" and header_prefix ~= "---" then
      result[#result + 1] = line
    end
  end
  return result
end

---@param actual string[]
---@param expected string[]
---@param message string
local function assert_line_list(actual, expected, message)
  assert_equal(#actual, #expected, message .. " line count")
  for line_index = 1, #expected do
    assert_equal(actual[line_index], expected[line_index], message .. " line " .. line_index)
  end
end

---@param direction DiffReviewIndexMutationDirection
---@param target DiffReviewIndexMutationTarget
---@return DiffReviewIndexMutationResult
local function execute_target(direction, target)
  local mutation_result
  index_mutation.execute_async(test_root, {
    direction = direction,
    target_list = { target },
  }, function(result)
    mutation_result = result
  end)
  assert_true(vim.wait(5000, function() return mutation_result ~= nil end, 10), "index mutation timed out")
  assert_true(mutation_result.ok, "index mutation failed: " .. tostring(mutation_result.error))
  assert_equal(mutation_result.count, 1, "completed mutation count")
  assert_equal(mutation_result.hunk_count, target.kind == "hunk" and 1 or 0, "completed hunk count")
  assert_equal(mutation_result.file_count, target.kind == "hunk" and 0 or 1, "completed file count")
  return mutation_result
end

local function initialize_repository()
  assert_true(vim.fn.mkdir(test_root, "p") == 1, "mkdir failed: " .. test_root)
  run_git_text({ "init" })
  run_git_text({ "config", "user.email", "diff-review-test@example.com" })
  run_git_text({ "config", "user.name", "DiffReview Test" })
  run_git_text({ "config", "core.autocrlf", "false" })
  run_git_text({ "config", "core.safecrlf", "true" })
  run_git_text({ "config", "core.eol", "lf" })
  run_git_text({ "config", "filter.diff-review-clean.clean", "git stripspace" })
  run_git_text({ "config", "filter.diff-review-clean.required", "true" })

  write_bytes(test_root .. "/.gitattributes", table.concat({
    "*crlf*.txt text eol=crlf",
    "*filtered*.txt text eol=lf filter=diff-review-clean",
    "",
  }, "\n"))
  write_bytes(test_root .. "/basic.txt", table.concat({
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "",
  }, "\n"))
  write_bytes(test_root .. "/crlf.txt", "alpha\r\nbeta\r\ngamma\r\n")
  write_bytes(test_root .. "/filtered.txt", "alpha   \n")
  run_git_text({ "add", "--", ".gitattributes", "basic.txt", "crlf.txt", "filtered.txt" })
  run_git_text({ "commit", "-m", "baseline" })

  assert_equal(run_git_text({ "show", ":crlf.txt" }), "alpha\nbeta\ngamma\n", "CRLF baseline index blob")
  assert_equal(run_git_text({ "show", ":filtered.txt" }), "alpha\n", "clean-filter baseline index blob")
  assert_equal(read_bytes(test_root .. "/crlf.txt"), "alpha\r\nbeta\r\ngamma\r\n", "CRLF baseline worktree")
  assert_equal(read_bytes(test_root .. "/filtered.txt"), "alpha   \n", "clean-filter baseline worktree")
  assert_equal(run_git_text({ "status", "--porcelain" }), "", "baseline repository status")
end

local function assert_basic_hunk_stage_and_reverse_unstage()
  write_bytes(test_root .. "/basic.txt", table.concat({
    "one",
    "TWO",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "NINE",
    "ten",
    "",
  }, "\n"))
  local full_diff = run_git_text({ "diff", "--no-color", "--no-ext-diff", "--unified=0", "--", "basic.txt" })
  local first_hunk_patch = select_hunk_patch(full_diff, 1)
  assert_true(select_hunk_patch(full_diff, 2):find("NINE", 1, true) ~= nil, "second hunk fixture missing")

  execute_target("stage", { kind = "hunk", path = test_root .. "/basic.txt", diff = first_hunk_patch })
  local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", "basic.txt" })
  local unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "basic.txt" })
  assert_line_list(changed_body_line_list(staged_diff), { "-two", "+TWO" }, "staged first hunk")
  assert_line_list(changed_body_line_list(unstaged_diff), { "-nine", "+NINE" }, "remaining unstaged hunk")

  execute_target("unstage", { kind = "hunk", path = test_root .. "/basic.txt", diff = first_hunk_patch })
  assert_equal(run_git_text({ "diff", "--cached", "--", "basic.txt" }), "", "reverse unstage clears cached diff")
  local restored_unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "basic.txt" })
  assert_line_list(
    changed_body_line_list(restored_unstaged_diff),
    { "-two", "+TWO", "-nine", "+NINE" },
    "reverse unstage restores both worktree hunks"
  )
end

local function assert_crlf_policy_preserves_semantic_hunk()
  local worktree_content = "alpha\r\nBETA\r\ngamma\r\n"
  write_bytes(test_root .. "/crlf.txt", worktree_content)
  local full_diff = run_git_text({ "diff", "--no-color", "--no-ext-diff", "--unified=0", "--", "crlf.txt" })
  local patch = select_hunk_patch(full_diff, 1)
  assert_true(patch:find("\r", 1, true) == nil, "CRLF diff patch was not normalized")
  assert_line_list(changed_body_line_list(patch), { "-beta", "+BETA" }, "normalized CRLF patch")

  execute_target("stage", { kind = "hunk", path = test_root .. "/crlf.txt", diff = patch })
  local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", "crlf.txt" })
  assert_line_list(changed_body_line_list(staged_diff), { "-beta", "+BETA" }, "CRLF cached diff")
  assert_equal(run_git_text({ "show", ":crlf.txt" }), "alpha\nBETA\ngamma\n", "CRLF staged index blob")
  assert_equal(read_bytes(test_root .. "/crlf.txt"), worktree_content, "CRLF worktree after stage")
  assert_equal(run_git_text({ "diff", "--", "crlf.txt" }), "", "CRLF path after stage")

  execute_target("unstage", { kind = "hunk", path = test_root .. "/crlf.txt", diff = patch })
  assert_equal(run_git_text({ "diff", "--cached", "--", "crlf.txt" }), "", "CRLF reverse unstage clears cached diff")
  local unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "crlf.txt" })
  assert_line_list(changed_body_line_list(unstaged_diff), { "-beta", "+BETA" }, "CRLF diff after reverse unstage")
  assert_equal(run_git_text({ "show", ":crlf.txt" }), "alpha\nbeta\ngamma\n", "CRLF index blob after reverse unstage")
  assert_equal(read_bytes(test_root .. "/crlf.txt"), worktree_content, "CRLF worktree after reverse unstage")
end

local function assert_clean_filter_preserves_canonical_hunk()
  local worktree_content = "BETA   \n"
  write_bytes(test_root .. "/filtered.txt", worktree_content)
  local full_diff = run_git_text({ "diff", "--no-color", "--no-ext-diff", "--unified=0", "--", "filtered.txt" })
  local patch = select_hunk_patch(full_diff, 1)
  assert_line_list(changed_body_line_list(patch), { "-alpha", "+BETA" }, "clean-filter patch")

  execute_target("stage", { kind = "hunk", path = test_root .. "/filtered.txt", diff = patch })
  local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", "filtered.txt" })
  assert_line_list(changed_body_line_list(staged_diff), { "-alpha", "+BETA" }, "clean-filter cached diff")
  assert_equal(run_git_text({ "show", ":filtered.txt" }), "BETA\n", "clean-filter staged index blob")
  assert_equal(read_bytes(test_root .. "/filtered.txt"), worktree_content, "clean-filter worktree after stage")
  assert_equal(run_git_text({ "diff", "--", "filtered.txt" }), "", "clean-filter path after stage")

  execute_target("unstage", { kind = "hunk", path = test_root .. "/filtered.txt", diff = patch })
  assert_equal(run_git_text({ "diff", "--cached", "--", "filtered.txt" }), "", "clean-filter reverse unstage clears cached diff")
  local unstaged_diff = run_git_text({ "diff", "--no-color", "--unified=0", "--", "filtered.txt" })
  assert_line_list(changed_body_line_list(unstaged_diff), { "-alpha", "+BETA" }, "clean-filter diff after reverse unstage")
  assert_equal(run_git_text({ "show", ":filtered.txt" }), "alpha\n", "clean-filter index blob after reverse unstage")
  assert_equal(read_bytes(test_root .. "/filtered.txt"), worktree_content, "clean-filter worktree after reverse unstage")
end

---@param relpath string
---@param raw_content string
---@param expected_synthetic_line_list string[]
---@param expected_staged_line_list string[]
---@param expected_index_content string
local function assert_untracked_whole_file_canonicalization(
  relpath,
  raw_content,
  expected_synthetic_line_list,
  expected_staged_line_list,
  expected_index_content
)
  local path = test_root .. "/" .. relpath
  write_bytes(path, raw_content)
  assert_equal(run_git_text({ "status", "--porcelain", "--", relpath }), "?? " .. relpath .. "\n", relpath .. " starts untracked")

  local synthetic_patch = status_snapshot._build_untracked_diff_from_bytes(relpath, raw_content)
  assert_true(type(synthetic_patch) == "string", "missing synthetic untracked patch for " .. relpath)
  ---@cast synthetic_patch string
  local synthetic_line_list = changed_body_line_list(synthetic_patch)
  assert_line_list(synthetic_line_list, expected_synthetic_line_list, relpath .. " raw synthetic patch")

  execute_target("stage", { kind = "untracked_file", path = path })
  assert_equal(run_git_text({ "status", "--porcelain", "--", relpath }), "A  " .. relpath .. "\n", relpath .. " staged status")
  assert_equal(read_bytes(path), raw_content, relpath .. " raw worktree after whole-file stage")
  assert_equal(run_git_text({ "show", ":" .. relpath }), expected_index_content, relpath .. " canonical index blob")

  local staged_diff = run_git_text({ "diff", "--cached", "--no-color", "--unified=0", "--", relpath })
  local staged_line_list = changed_body_line_list(staged_diff)
  assert_line_list(staged_line_list, expected_staged_line_list, relpath .. " staged canonical diff")
  assert_true(
    table.concat(synthetic_line_list, "\n") ~= table.concat(staged_line_list, "\n"),
    relpath .. " canonicalization should differ from the raw synthetic patch"
  )

  execute_target("unstage", { kind = "added_file", path = path })
  assert_equal(run_git_text({ "diff", "--cached", "--", relpath }), "", relpath .. " cached diff after unstage")
  assert_equal(run_git_text({ "ls-files", "--stage", "--", relpath }), "", relpath .. " index entry after unstage")
  assert_equal(run_git_text({ "status", "--porcelain", "--", relpath }), "?? " .. relpath .. "\n", relpath .. " returns untracked")
  assert_equal(read_bytes(path), raw_content, relpath .. " raw worktree after unstage")
end

local function assert_untracked_whole_file_stage_and_unstage()
  assert_untracked_whole_file_canonicalization(
    "untracked-crlf.txt",
    "first\r\nsecond\r\n",
    { "+first\r", "+second\r" },
    { "+first", "+second" },
    "first\nsecond\n"
  )
  assert_untracked_whole_file_canonicalization(
    "untracked-filtered.txt",
    "first   \nsecond\t\n",
    { "+first   ", "+second\t" },
    { "+first", "+second" },
    "first\nsecond\n"
  )
end

local function cleanup()
  local normalized_root = vim.fs.normalize(test_root)
  local normalized_parent = vim.fs.normalize(vim.fn.fnamemodify(test_root, ":h"))
  local parent_prefix = normalized_parent:gsub("[/\\]+$", "") .. "/"
  assert_true(normalized_root:sub(1, #parent_prefix) == parent_prefix, "refusing cleanup outside temp parent")
  if vim.fn.isdirectory(test_root) == 1 then vim.fn.delete(test_root, "rf") end
end

local function run()
  git_backend.reset_backend()
  initialize_repository()
  assert_basic_hunk_stage_and_reverse_unstage()
  assert_crlf_policy_preserves_semantic_hunk()
  assert_clean_filter_preserves_canonical_hunk()
  assert_untracked_whole_file_stage_and_unstage()
end

local ok, error_message = xpcall(run, debug.traceback)
local cleanup_ok, cleanup_error = pcall(cleanup)
git_backend.reset_backend()
if not cleanup_ok then
  vim.api.nvim_err_writeln(tostring(cleanup_error))
  vim.cmd("cquit")
end
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit")
end
vim.cmd("qa!")
