package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local git_backend = require("diff_review.git.git_backend")
local index_mutation = require("diff_review.git.index_mutation")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if actual ~= expected then
    error((message or "values differ") .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual), 2)
  end
end

local root = "D:/mutation-root"
local command_list = {}
local fail_path = "fail.txt"
local duplicate_backend_callback = false

---@type DiffReviewGitBackend
local backend = {}

function backend.system_async(command, input, callback)
  command_list[#command_list + 1] = { command = vim.deepcopy(command), input = input }
  local failed = command[#command] == fail_path
  local result = {
    code = failed and 1 or 0,
    stdout = "",
    stderr = failed and "rejected" or "",
    output = failed and "rejected" or "",
  }
  callback(result)
  if duplicate_backend_callback then callback(result) end
end

git_backend.set_backend(backend)

local patch = table.concat({
  "diff --git a/first.txt b/first.txt",
  "--- a/first.txt",
  "+++ b/first.txt",
  "@@ -1 +1 @@",
  "-old",
  "+new",
}, "\n")

local result = nil
index_mutation.execute_async(root, {
  direction = "stage",
  target_list = {
    { kind = "hunk", path = root .. "/first.txt", diff = patch },
    { kind = "tracked_file", path = root .. "/" .. fail_path },
    { kind = "untracked_file", path = root .. "/skipped.txt" },
  },
}, function(next_result)
  result = next_result
end)

assert_true(result ~= nil and not result.ok, "the failed file should fail the ordered action")
assert_equal(#command_list, 2, "execution should stop at the first failure")
assert_equal(result.hunk_count, 1, "the successful hunk should remain recorded")
assert_equal(result.file_count, 0, "the failed file should not count as complete")
assert_equal(result.failure.target.path, root .. "/" .. fail_path, "failure should retain its semantic target")
assert_true(
  table.concat(command_list[1].command, "\t"):find("apply\t--cached", 1, true) ~= nil,
  "hunk staging should use cached apply"
)
assert_true(
  table.concat(command_list[2].command, "\t"):find("add\t-u\t--\t" .. fail_path, 1, true) ~= nil,
  "tracked staging should use git add -u"
)

command_list = {}
result = nil
index_mutation.execute_async(root, {
  direction = "unstage",
  target_list = {
    { kind = "added_file", path = root .. "/new.txt" },
  },
}, function(next_result)
  result = next_result
end)
assert_true(result ~= nil and result.ok, "added-file unstage should succeed")
assert_true(
  table.concat(command_list[1].command, "\t"):find("rm\t--cached\t--ignore-unmatch\t--\tnew.txt", 1, true) ~= nil,
  "added-file unstage should use rm --cached"
)

command_list = {}
result = nil
index_mutation.execute_async(root, {
  direction = "unstage",
  target_list = {
    {
      kind = "tracked_file",
      path = root .. "/renamed.txt",
      original_path = root .. "/original.txt",
    },
  },
}, function(next_result)
  result = next_result
end)
assert_true(result ~= nil and result.ok, "renamed-file unstage should succeed")
assert_true(
  table.concat(command_list[1].command, "\t"):find(
    "restore\t--staged\t--\trenamed.txt\toriginal.txt",
    1,
    true
  ) ~= nil,
  "renamed-file unstage should restore both sides of the rename"
)

command_list = {}
result = nil
local completion_count = 0
duplicate_backend_callback = true
index_mutation.execute_async(root, {
  direction = "stage",
  target_list = {
    { kind = "tracked_file", path = root .. "/duplicate-first.txt" },
    { kind = "tracked_file", path = root .. "/duplicate-second.txt" },
  },
}, function(next_result)
  completion_count = completion_count + 1
  result = next_result
end)
duplicate_backend_callback = false
assert_true(result ~= nil and result.ok, "duplicate backend callback changed the mutation result")
assert_equal(#command_list, 2, "duplicate backend callback issued a target twice")
assert_equal(completion_count, 1, "duplicate backend callback completed the mutation twice")

git_backend.reset_backend()
print("index_mutation OK")
vim.cmd("qa!")
