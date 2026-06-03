vim.loader.enable(false)

local gh = require("github.gh")
local notifications = require("github.notifications")

local root = "D:/mock/github"
local calls = {}
local opened_urls = {}
local captured_picker = nil
local original_snacks = _G.Snacks
local original_picker_pick = original_snacks and original_snacks.picker and original_snacks.picker.pick
local original_notify = vim.notify

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(command)
  calls[#calls + 1] = {
    command = vim.deepcopy(command),
    key = command_key(command),
  }
end

local function reset()
  calls = {}
  opened_urls = {}
  captured_picker = nil
  notifications._reset_for_tests()
end

local function wait_for(predicate, message)
  local ok = vim.wait(1000, predicate, 10)
  assert_true(ok, message)
end

local function current_lines()
  return vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false)
end

local function find_line(needle)
  for index, line in ipairs(current_lines()) do
    if line:find(needle, 1, true) then return index end
  end
  return nil
end

local function encoded_issue(number)
  return vim.json.encode({
    number = number,
    title = "Fix command parser",
    body = "Issue body\n\nDetails.",
    url = "https://github.com/org/repo/issues/" .. tostring(number),
    state = "OPEN",
    author = { login = "alice" },
    assignees = { { login = "bob" } },
    labels = { { name = "bug" } },
    comments = {
      {
        body = "First comment",
        author = { login = "carol" },
        createdAt = "2026-06-01T00:00:00Z",
        updatedAt = "2026-06-01T00:00:00Z",
        url = "https://github.com/org/repo/issues/12#issuecomment-1",
      },
    },
    createdAt = "2026-06-01T00:00:00Z",
    updatedAt = "2026-06-02T00:00:00Z",
  })
end

local function encoded_pr(number)
  return vim.json.encode({
    number = number,
    title = "Improve review flow",
    body = "PR body",
    url = "https://github.com/org/repo/pull/" .. tostring(number),
    state = "OPEN",
    author = { login = "alice" },
    assignees = {},
    labels = {},
    comments = {},
    createdAt = "2026-06-01T00:00:00Z",
    updatedAt = "2026-06-02T00:00:00Z",
    headRefName = "feature/review",
    baseRefName = "main",
    isDraft = false,
  })
end

---@type GithubGhBackend
local backend = {}

function backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

function backend.system_async(command, _, callback, cwd)
  record(command)
  assert_true(cwd == root or cwd == vim.fn.getcwd() or cwd == nil, "unexpected cwd: " .. tostring(cwd))
  local key = command_key(command)
  local stdout = ""
  local code = 0

  if key:find("gh\tsearch\tissues", 1, true) then
    stdout = vim.json.encode({
      {
        number = 12,
        title = "Fix command parser",
        url = "https://github.com/org/repo/issues/12",
        repository = { nameWithOwner = "org/repo" },
        author = { login = "alice" },
        commentsCount = 1,
        updatedAt = "2026-06-02T00:00:00Z",
        state = "OPEN",
      },
    })
  elseif key:find("gh\tsearch\tprs\t--author", 1, true) then
    stdout = vim.json.encode({
      {
        number = 44,
        title = "Improve review flow",
        url = "https://github.com/org/repo/pull/44",
        repository = { nameWithOwner = "org/repo" },
        author = { login = "alice" },
        commentsCount = 0,
        updatedAt = "2026-06-02T00:00:00Z",
        state = "OPEN",
        isDraft = false,
      },
    })
  elseif key:find("gh\tsearch\tprs\t--review-requested", 1, true) then
    stdout = vim.json.encode({
      {
        number = 45,
        title = "Review requested",
        url = "https://github.com/org/repo/pull/45",
        repository = { nameWithOwner = "org/repo" },
        author = { login = "dana" },
        commentsCount = 2,
        updatedAt = "2026-06-02T00:00:00Z",
        state = "OPEN",
      },
    })
  elseif key:find("gh\tissue\tview\t12", 1, true) then
    stdout = encoded_issue(12)
  elseif key:find("gh\tissue\tview\t34", 1, true) then
    stdout = encoded_issue(34)
  elseif key:find("gh\tpr\tview\t44", 1, true) or key:find("gh\tpr\tview\t45", 1, true) then
    stdout = encoded_pr(44)
  elseif key:find("gh\tapi\t/notifications", 1, true) then
    stdout = table.concat({
      vim.json.encode({
        id = "thread-1",
        unread = true,
        reason = "subscribed",
        updated_at = "2026-06-02T00:00:00Z",
        last_read_at = vim.NIL,
        repository = { full_name = "org/repo" },
        subject = {
          title = "Fix command parser",
          type = "Issue",
          url = "https://api.github.com/repos/org/repo/issues/12",
          latest_comment_url = "https://api.github.com/repos/org/repo/issues/comments/100",
        },
      }),
      vim.json.encode({
        id = "thread-2",
        unread = false,
        reason = "mention",
        updated_at = "2026-06-01T00:00:00Z",
        repository = { full_name = "org/repo" },
        subject = {
          title = "Improve review flow",
          type = "PullRequest",
          url = "https://api.github.com/repos/org/repo/pulls/44",
        },
      }),
    }, "\n")
  elseif key == "gh\tapi\t/repos/org/repo/issues/comments/100" then
    stdout = vim.json.encode({
      body = "Last notification comment",
      user = { login = "carol" },
    })
  elseif key == "gh\tapi\t/repos/org/repo/issues/12" then
    stdout = vim.json.encode({
      body = "Issue body",
      comments = 1,
    })
  elseif key == "gh\tapi\t/repos/org/repo/pulls/44" then
    stdout = vim.json.encode({
      body = "PR body",
      comments = 2,
    })
  elseif key == "gh\tapi\t-X\tPATCH\t/notifications/threads/thread-1" then
    stdout = "{}"
  elseif key == "gh\tapi\t-X\tDELETE\t/notifications/threads/thread-1" then
    stdout = "{}"
  elseif key == "gh\tapi\t-X\tDELETE\t/notifications/threads/thread-2" then
    stdout = "{}"
  else
    code = 1
  end

  vim.defer_fn(function()
    callback({
      code = code,
      stdout = stdout,
      stderr = code == 0 and "" or "unexpected command: " .. key,
      output = code == 0 and stdout or "unexpected command: " .. key,
    })
  end, 5)
end

gh.set_backend(backend)
vim.notify = function() end
if not _G.Snacks then _G.Snacks = {} end
if not Snacks.picker then Snacks.picker = {} end
Snacks.picker.pick = function(opts)
  captured_picker = opts
  return opts
end

local plugin_spec = require("plugins.github")[1]
plugin_spec.init()

local function cleanup()
  notifications._reset_for_tests()
  if original_snacks then
    _G.Snacks = original_snacks
    if original_snacks.picker then original_snacks.picker.pick = original_picker_pick end
  else
    _G.Snacks = nil
  end
  vim.notify = original_notify
  gh.reset_backend()
end

local function run_tests()
  reset()
  vim.cmd.GithubIssuse()
  wait_for(function() return captured_picker ~= nil end, "issue picker did not open")
  assert_true(calls[1].key:find("gh\tsearch\tissues", 1, true) ~= nil, "issues command was not used")
  assert_true(#captured_picker.items == 1, "issue picker missing items")
  captured_picker.confirm({ close = function() end }, captured_picker.items[1])
  wait_for(function() return find_line("#12 Fix command parser") ~= nil end, "issue view did not render selected issue")
  assert_true(find_line("Comments (1)") ~= nil, "issue comments did not render")

  reset()
  vim.cmd.GithubPR()
  wait_for(function() return captured_picker ~= nil end, "PR picker did not open")
  assert_true(calls[1].key:find("gh\tsearch\tprs\t--author\t@me", 1, true) ~= nil, "PR author search command was not used")

  reset()
  vim.cmd.GithubReview()
  wait_for(function() return captured_picker ~= nil end, "review picker did not open")
  assert_true(
    calls[1].key:find("gh\tsearch\tprs\t--review-requested\t@me", 1, true) ~= nil,
    "review request search command was not used"
  )

  reset()
  vim.cmd("GithubIssueOpen 34")
  wait_for(function() return find_line("#34 Fix command parser") ~= nil end, "GithubIssueOpen did not open issue buffer")

  reset()
  vim.cmd.GithubNotifications()
  wait_for(function() return find_line("#12 Fix command parser") ~= nil end, "notifications did not render unread item")
  wait_for(function() return find_line("#12 Fix command parser (1)") ~= nil end, "notification comment count did not load")
  local unread_line = find_line("#12 Fix command parser")
  assert_true(unread_line ~= nil, "unread notification line missing")
  vim.api.nvim_win_set_cursor(0, { unread_line, 0 })
  notifications.toggle_expand()
  wait_for(function() return find_line("Last notification comment") ~= nil end, "notification expansion did not load latest comment")
  notifications.save_current()
  wait_for(function() return find_line("Saved:") ~= nil and find_line("#12 Fix command parser") ~= nil end, "saved notification missing")
  assert_true(calls[#calls].key == "gh\tapi\t-X\tPATCH\t/notifications/threads/thread-1", "save did not mark thread read")
  notifications.unread_current()
  assert_true(find_line("Unread:") ~= nil and find_line("#12 Fix command parser") ~= nil, "unread action did not restore item locally")
  notifications.done_current()
  assert_true(find_line("Done:") ~= nil and find_line("#12 Fix command parser") ~= nil, "done action did not move item")
  notifications.unread_current()
  assert_true(find_line("Unread:") ~= nil and find_line("#12 Fix command parser") ~= nil, "done item did not move back to unread")
end

local ok, err = xpcall(run_tests, debug.traceback)
cleanup()
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("github_integration: ok")
vim.cmd("qa!")
