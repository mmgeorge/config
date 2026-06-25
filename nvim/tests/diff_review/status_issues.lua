vim.loader.enable(false)

local diff_review = require("diff_review")
local session = require("diff_review.session")
local ai_commit = require("diff_review.integrations.ai_commit")
local gh = require("diff_review.integrations.gh")
local issue_index = require("github.issue_index")
local repo_cache = require("github.repo_cache")

local original_notify = vim.notify
local original_cwd = vim.fn.getcwd():gsub("\\", "/")
local root = vim.fs.joinpath(original_cwd, ".tmp-gitstatus-file-test"):gsub("\\", "/")
local cache_root = vim.fn.tempname()
local notifications = {}
local calls = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(command)
  calls[#calls + 1] = command_key(command)
end

local git_backend = {}

function git_backend.systemlist(command)
  record(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "feature/session" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "mock subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{upstream}" then return { "origin/feature/session" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{upstream}" then return { "def5678" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{upstream}" then return { "upstream subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{push}" then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{push}" then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{push}" then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return { "M\tlua/diff_review/init.lua" }, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return {
      "diff --git a/lua/diff_review/init.lua b/lua/diff_review/init.lua",
      "--- a/lua/diff_review/init.lua",
      "+++ b/lua/diff_review/init.lua",
      "@@ -1 +1 @@",
      "-old",
      "+new",
    }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then return {}, 0 end
  if key:find("\tlog\t--no%-color\t", 1, false) then return {}, 1 end
  return {}, 1
end

function git_backend.systemlist_async(command, cb)
  local output, code = git_backend.systemlist(command)
  cb(output, code, "")
end

local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = command_key(command)
  if key == "gh\tpr\tview\t--json\tnumber,id,title,body,url,headRefName,headRefOid,headRepositoryOwner,baseRefName,state,isDraft,reviewDecision,reviewRequests,reviews,comments,commits,files,changedFiles,additions,deletions,milestone" then
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
    return
  end
  if key == "gh\trepo\tview\t--json\tnameWithOwner" then
    cb({ code = 0, stdout = vim.json.encode({ nameWithOwner = "org/repo" }), stderr = "", output = vim.json.encode({ nameWithOwner = "org/repo" }) })
    return
  end
  if key == "gh\tapi\t/repos/org/repo/contributors\t--paginate\t--slurp" then
    cb({ code = 0, stdout = "[]", stderr = "", output = "[]" })
    return
  end
  cb({ code = 1, stdout = "", stderr = "unexpected gh command: " .. key, output = "unexpected gh command: " .. key })
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

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function move_cursor(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
end

local function edit_line(buf, row, text)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, row - 1, row, false, { text })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
end

local function write_issue_snapshot(repo_name)
  local path = issue_index.snapshot_path(repo_name)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = repo_name,
    state = "open",
    issue_count = 2,
    issues = {
      {
        repo = repo_name,
        number = 42,
        title = "Test issue match",
        state = "OPEN",
        url = "https://github.example.test/org/repo/issues/42",
        labels = { { name = "test" } },
      },
      {
        repo = repo_name,
        number = 44,
        title = "Second test issue",
        state = "OPEN",
        url = "https://github.example.test/org/repo/issues/44",
        labels = { { name = "test" } },
      },
    },
  }) }, path)
  assert_true(result == 0, "issue snapshot write failed")
end

local function issue_completion_labels(buf, row, text)
  edit_line(buf, row, text)
  move_cursor(buf, row)
  vim.b[buf].github_repo = "org/repo"
  local old_virtualedit = vim.o.virtualedit
  vim.o.virtualedit = "onemore"
  vim.api.nvim_win_set_cursor(0, { row, #text })
  local issue_source = require("github.issue_source").new({ debounce_ms = 0 })
  assert_true(issue_source:enabled(), "GitStatus issue completion did not enable on: " .. text)
  local completion_result
  issue_source:get_completions({}, function(result) completion_result = result end)
  assert_true(vim.wait(1000, function() return completion_result ~= nil end, 10), "GitStatus issue completion did not return")
  vim.o.virtualedit = old_virtualedit
  local labels = {}
  for _, item in ipairs(completion_result.items or {}) do
    labels[item.label] = item
  end
  return labels
end

local function run()
  vim.fn.delete(root, "rf")
  vim.fn.mkdir(root, "p")
  vim.notify = function(message, level, opts)
    notifications[#notifications + 1] = { message = tostring(message), level = level, opts = opts }
  end
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  ai_commit.reset_backend()
  repo_cache.set_data_dir_for_test(cache_root)
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  repo_cache.remember_cwd_repo(root, "org/repo")
  write_issue_snapshot("org/repo")
  diff_review.setup({ about_auto_generate = false })

  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(buf, "init.lua +1 -1") end, "GitStatus changes did not render:\n" .. table.concat(status_lines(buf), "\n") .. "\nCalls:\n" .. vim.inspect(calls))
  wait_for(function() return buffer_contains(buf, "Issues: none") end, "Issues row did not render none state")

  local issues_row = find_row(buf, "Issues:")
  move_cursor(buf, issues_row)
  assert_true(vim.bo[buf].modifiable, "GitStatus must unlock on the Issues row")

  local issue_labels = issue_completion_labels(buf, issues_row, "Issues: #test")
  assert_true(issue_labels["#42 Test issue match"] ~= nil, "GitStatus issue completion did not include #42")
  assert_true(issue_labels["#42 Test issue match"].textEdit.newText == "#42", "GitStatus issue completion should insert only the issue id")

  edit_line(buf, issues_row, "Issues: #42 #44")
  assert_true(vim.bo[buf].modified, "editing GitStatus issues must mark the buffer modified")
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  assert_true(not vim.bo[buf].modified, "saving GitStatus issues must clear the modified flag")
  assert_true(buffer_contains(buf, "Issues: #42 #44"), "GitStatus issues row did not normalize after save")

  local gitstatus_path = vim.fs.joinpath(root, ".gitstatus")
  assert_true(vim.fn.filereadable(gitstatus_path) == 1, "GitStatus save did not create .gitstatus")
  local gitstatus = vim.json.decode(table.concat(vim.fn.readfile(gitstatus_path), "\n"))
  assert_true(vim.deep_equal(gitstatus.issues, { 42, 44 }), "GitStatus .gitstatus issues were wrong: " .. vim.inspect(gitstatus))

  session.states[buf].issues = nil
  diff_review.render_status(buf)
  wait_for(function() return buffer_contains(buf, "Issues: #42 #44") end, "GitStatus did not reload issues from .gitstatus")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
ai_commit.reset_backend()
repo_cache.set_data_dir_for_test(nil)
issue_index._reset_for_test()
vim.notify = original_notify
vim.fn.delete(cache_root, "rf")
vim.fn.delete(root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
