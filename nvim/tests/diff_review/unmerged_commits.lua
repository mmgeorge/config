vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local root = "D:/diffreview-unmerged-root"
local calls = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(kind, command, input)
  calls[#calls + 1] = {
    kind = kind,
    command = vim.deepcopy(command),
    key = command_key(command),
    input = input,
  }
end

local function output_lines(text)
  if text == "" then return {} end
  return vim.split(text, "\n", { plain = true })
end

local function wait_for(predicate, message)
  assert_true(vim.wait(3000, predicate, 10), message)
end

local function status_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  for index, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(status_lines(buf), "\n"), 2)
end

local function find_row_after(buf, needle, start_row)
  local lines = status_lines(buf)
  for index = start_row + 1, #lines do
    if lines[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. tostring(start_row) .. ": " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function saw_call_containing(needle)
  for _, call in ipairs(calls) do
    if call.key:find(needle, 1, true) then return true end
  end
  return false
end

local function count_calls_containing(needle)
  local count = 0
  for _, call in ipairs(calls) do
    if call.key:find(needle, 1, true) then count = count + 1 end
  end
  return count
end

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local commit_diff = table.concat({
  "diff --git a/foo/bar.js b/foo/bar.js",
  "index 1111111..2222222 100644",
  "--- a/foo/bar.js",
  "+++ b/foo/bar.js",
  "@@ -1 +1 @@",
  "-old",
  "+new",
  "diff --git a/baz.txt b/baz.txt",
  "index 3333333..4444444 100644",
  "--- a/baz.txt",
  "+++ b/baz.txt",
  "@@ -2 +2 @@",
  "-left",
  "+right",
}, "\n")

local staged_diff = table.concat({
  "diff --git a/staged.txt b/staged.txt",
  "index 5555555..6666666 100644",
  "--- a/staged.txt",
  "+++ b/staged.txt",
  "@@ -1 +1 @@",
  "-before",
  "+after",
}, "\n")

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  record("systemlist", command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "45806b8" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "feat: add or mapping and guard ai generation" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{upstream}" then return { "origin/master" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{upstream}" then return { "365f098" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{upstream}" then return { "refactor: update shader types" }, 0 end
  if key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%s\torigin/master..HEAD" then
    return {
      "45806b8123456789\t45806b8\tfeat: add or mapping and guard ai generation",
      "748971a123456789\t748971a\tfeat: add debug notifications and AI commit flag",
      "f7930f2123456789\tf7930f2\tfeat: add ai commit message generation",
      "c8f1018123456789\tc8f1018\tfeat: preserve cursor position during visual staging",
    }, 0
  end
  if key == "git\t-C\t" .. root .. "\tshow\t--format=\t--no-color\t--no-ext-diff\t45806b8123456789" then
    return output_lines(commit_diff), 0
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return { "M\tstaged.txt" }, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then return output_lines(staged_diff), 0 end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  record("systemlist_async", command)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function backend.system(command, input)
  record("system", command, input)
  return "unexpected command: " .. command_key(command), 1
end

function backend.system_async(command, input, cb)
  record("system_async", command, input)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "unexpected command", output = "unexpected command" })
  end, 5)
end

local function run()
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup()
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()

  wait_for(function() return buffer_contains(buf, "Unmerged into origin/master (4)") end, "unmerged section did not render")
  assert_true(find_row(buf, "Unmerged into origin/master (4)") > find_row(buf, "Staged changes"), "unmerged section should render after local change sections")
  assert_true(buffer_contains(buf, "45806b8 master feat: add or mapping and guard ai generation"), "first unmerged commit did not include branch")
  assert_true(buffer_contains(buf, "748971a feat: add debug notifications and AI commit flag"), "second unmerged commit missing")
  assert_true(not buffer_contains(buf, "foo/bar.js +1 -1"), "commit files loaded before commit was expanded")
  assert_true(count_calls_containing("\tshow\t--format=\t--no-color\t--no-ext-diff\t45806b8123456789") == 0, "commit diff loaded eagerly")

  local unmerged_heading = find_row(buf, "Unmerged into origin/master (4)")
  trigger_normal_mapping("<Tab>", find_row_after(buf, "45806b8 master", unmerged_heading))
  wait_for(function() return buffer_contains(buf, "foo/bar.js +1 -1") end, "expanded commit did not render changed file\n" .. table.concat(status_lines(buf), "\n"))
  assert_true(buffer_contains(buf, "baz.txt +1 -1"), "expanded commit missing second changed file")
  assert_true(count_calls_containing("\tshow\t--format=\t--no-color\t--no-ext-diff\t45806b8123456789") == 2, "commit diff was not loaded exactly once through async backend")

  trigger_normal_mapping("<Tab>", find_row(buf, "foo/bar.js +1 -1"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "commit file did not unfold hunks")
  trigger_normal_mapping("S", find_row(buf, "foo/bar.js +1 -1"))
  vim.wait(50)
  assert_true(not saw_call_containing("\tadd\t"), "stage command ran on read-only commit diff")
  assert_true(not saw_call_containing("\tapply\t--cached\t"), "stage patch ran on read-only commit hunk")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
