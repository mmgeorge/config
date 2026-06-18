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

local function set_datetime_now(value)
  local epoch = diff_review._datetime.parse(value)
  assert_true(type(epoch) == "number", "test datetime did not parse: " .. tostring(value))
  diff_review._datetime.now_override = function() return epoch end
end

local function recent_commit_date(index)
  if index <= 14 then return ("2026-06-%02dT12:00:00Z"):format(15 - index) end
  return ("2026-05-%02dT12:00:00Z"):format(46 - index)
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

local function buffer_contains_after(buf, needle, start_row)
  local lines = status_lines(buf)
  for index = start_row + 1, #lines do
    if lines[index]:find(needle, 1, true) then return true end
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

local function line_has_highlight_prefix(buf, row, hl_prefix)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if type(details.hl_group) == "string" and details.hl_group:sub(1, #hl_prefix) == hl_prefix then return true end
  end
  return false
end

local function line_has_highlight(buf, row, hl_group, start_col, end_col)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group == hl_group
      and (start_col == nil or mark[3] == start_col)
      and (end_col == nil or details.end_col == end_col) then
      return true
    end
  end
  return false
end

local function line_highlights(buf, row)
  local groups = {}
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group then groups[#groups + 1] = details.hl_group end
  end
  return table.concat(groups, ",")
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
  "diff --git a/src/commit.rs b/src/commit.rs",
  "index 7777777..8888888 100644",
  "--- a/src/commit.rs",
  "+++ b/src/commit.rs",
  "@@ -1 +1 @@",
  "-fn old_commit() {}",
  "+pub fn from_commit() {}",
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
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\torigin/master..HEAD" then
    return {
      "45806b8123456789\t45806b8\t2026-06-14T12:00:00Z\tfeat: add or mapping and guard ai generation",
      "748971a123456789\t748971a\t2026-06-13T12:00:00Z\tfeat: add debug notifications and AI commit flag",
      "f7930f2123456789\tf7930f2\t2026-06-12T12:00:00Z\tfeat: add ai commit message generation",
      "c8f1018123456789\tc8f1018\t2026-06-11T12:00:00Z\tfeat: preserve cursor position during visual staging",
    }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20\torigin/master" then
    local lines = {}
    for index = 1, 21 do
      local oid = ("%040d"):format(index)
      local short_oid = ("recent%02d"):format(index)
      lines[#lines + 1] = ("%s\t%s\t%s\tdocs: recent commit %02d"):format(oid, short_oid, recent_commit_date(index), index)
    end
    return lines, 0
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
  diff_review.setup({ about_auto_generate = false })
  assert_true(diff_review._status_conventional_commit_type_end("feat: add row highlighting") == #"feat", "plain conventional type did not parse")
  assert_true(diff_review._status_conventional_commit_type_end("fix(parser)!: handle bang") == #"fix", "scoped breaking conventional type did not parse")
  assert_true(diff_review._status_conventional_commit_type_end("Fix: uppercase is ambiguous") == nil, "uppercase type should not parse")
  assert_true(diff_review._status_conventional_commit_type_end("docs:missing space") == nil, "missing post-colon space should not parse")
  assert_true(diff_review._status_conventional_commit_type_end("build(scope with space): no") == nil, "scope with spaces should not parse")
  set_datetime_now("2026-06-15T12:00:00Z")
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()

  wait_for(function() return buffer_contains(buf, "Unmerged into origin/master (4)") end, "unmerged section did not render")
  assert_true(find_row(buf, "Unmerged into origin/master (4)") > find_row(buf, "Staged changes"), "unmerged section should render after local change sections")
  wait_for(function() return buffer_contains(buf, "Recent Commits (20)") end, "recent commits section did not render")
  assert_true(find_row(buf, "Recent Commits (20)") > find_row(buf, "Unmerged into origin/master (4)"), "recent commits should render below unmerged commits")
  assert_true(saw_call_containing("\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20\torigin/master"), "recent commits did not use a 20-commit upstream git log")
  assert_true(not buffer_contains(buf, "recent01  1 day ago   docs: recent commit 01"), "recent commits should start folded")
  assert_true(buffer_contains(buf, "45806b8  1 day ago  feat: add or mapping and guard ai generation"), "first unmerged commit missing")
  assert_true(buffer_contains(buf, "748971a  2 days ago feat: add debug notifications and AI commit flag"), "second unmerged commit missing")
  local first_unmerged_row = find_row(buf, "45806b8  1 day ago  feat: add or mapping and guard ai generation")
  local first_unmerged_line = status_lines(buf)[first_unmerged_row] or ""
  local feat_start = (first_unmerged_line:find("feat:", 1, true) or 1) - 1
  assert_true(
    line_has_highlight(buf, first_unmerged_row, "DiffReviewStatusCommitType", feat_start, feat_start + #"feat"),
    "first unmerged conventional commit type was not highlighted: " .. line_highlights(buf, first_unmerged_row)
  )
  assert_true(not buffer_contains(buf, "foo/bar.js +1 -1"), "commit files loaded before commit was expanded")
  assert_true(count_calls_containing("\tshow\t--format=\t--no-color\t--no-ext-diff\t45806b8123456789") == 0, "commit diff loaded eagerly")

  local unmerged_heading = find_row(buf, "Unmerged into origin/master (4)")
  trigger_normal_mapping("<Tab>", find_row_after(buf, "45806b8  1 day ago  feat", unmerged_heading))
  wait_for(function() return buffer_contains(buf, "foo/bar.js +1 -1") end, "expanded commit did not render changed file\n" .. table.concat(status_lines(buf), "\n"))
  assert_true(buffer_contains(buf, "baz.txt +1 -1"), "expanded commit missing second changed file")
  assert_true(buffer_contains(buf, "src/commit.rs +1 -1"), "expanded commit missing Rust changed file")
  assert_true(count_calls_containing("\tshow\t--format=\t--no-color\t--no-ext-diff\t45806b8123456789") == 2, "commit diff was not loaded exactly once through async backend")

  trigger_normal_mapping("<Tab>", find_row(buf, "foo/bar.js +1 -1"))
  wait_for(function() return buffer_contains(buf, "@@ +1 -1") end, "commit file did not unfold hunks")
  trigger_normal_mapping("S", find_row(buf, "foo/bar.js +1 -1"))
  vim.wait(50)
  assert_true(not saw_call_containing("\tadd\t"), "stage command ran on read-only commit diff")
  assert_true(not saw_call_containing("\tapply\t--cached\t"), "stage patch ran on read-only commit hunk")

  trigger_normal_mapping("<Tab>", find_row(buf, "src/commit.rs +1 -1"))
  wait_for(function() return buffer_contains(buf, "pub fn from_commit") end, "Rust commit file did not unfold hunks")
  wait_for(function()
    local row = find_row(buf, "pub fn from_commit")
    return line_has_highlight_prefix(buf, row, "@keyword")
  end, "commit-only Rust hunk body row did not get Tree-sitter keyword highlight: " .. line_highlights(buf, find_row(buf, "pub fn from_commit")))

  local show_calls_before_recent = count_calls_containing("\tshow\t--format=\t--no-color\t--no-ext-diff")
  trigger_normal_mapping("<Tab>", find_row(buf, "Recent Commits (20)"))
  wait_for(function() return buffer_contains(buf, "recent01  1 day ago   docs: recent commit 01") end, "recent commits did not unfold")
  local recent_row = find_row(buf, "recent01  1 day ago   docs: recent commit 01")
  local recent_line = status_lines(buf)[recent_row] or ""
  local docs_start = (recent_line:find("docs:", 1, true) or 1) - 1
  assert_true(
    line_has_highlight(buf, recent_row, "DiffReviewStatusCommitType", docs_start, docs_start + #"docs"),
    "recent conventional commit type was not highlighted: " .. line_highlights(buf, recent_row)
  )
  assert_true(buffer_contains(buf, "recent20  20 days ago docs: recent commit 20"), "recent commits did not include the 20th commit")
  assert_true(not buffer_contains(buf, "recent21  21 days ago docs: recent commit 21"), "recent commits rendered more than 20 commits")
  assert_true(
    find_row_after(buf, "recent01  1 day ago   docs: recent commit 01", find_row(buf, "Recent Commits (20)"))
      > find_row(buf, "Recent Commits (20)"),
    "recent commits did not render under the recent section"
  )
  assert_true(
    not buffer_contains_after(buf, "45806b8  1 day ago  feat: add or mapping and guard ai generation", find_row(buf, "Recent Commits (20)")),
    "recent commits included an unmerged commit"
  )
  assert_true(
    count_calls_containing("\tshow\t--format=\t--no-color\t--no-ext-diff") == show_calls_before_recent,
    "expanding recent commits section loaded commit diffs eagerly"
  )
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
diff_review._datetime.now_override = nil
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
