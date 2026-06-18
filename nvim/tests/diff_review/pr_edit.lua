vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local github_gh = require("github.gh")
local issue_index = require("github.issue_index")
local repo_cache = require("github.repo_cache")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local original_notify = vim.notify
local captured_notifications = {}
local repo_cache_dir = vim.fn.tempname()
local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = {
    message = tostring(message),
    level = level,
    opts = opts,
  }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

local pr_diff_text = table.concat({
  "diff --git a/src/a.txt b/src/a.txt",
  "index 1111111..2222222 100644",
  "--- a/src/a.txt",
  "+++ b/src/a.txt",
  "@@ -1,9 +1,9 @@",
  " far context before",
  " alpha",
  " beta",
  " gamma",
  "-old line",
  "+NEW LINE",
  " omega",
  " delta",
  " far context after",
}, "\n")

---@type { command: string[], input: string? }[]
local edit_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local standalone_comment_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local issue_comment_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local reviewer_request_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local reviewer_remove_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local milestone_create_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local milestone_set_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local draft_status_calls = {}
---@type string[]
local user_lookup_calls = {}
local contributor_calls = 0
local collaborator_calls = 0
local milestone_list_calls = 0
local issue_reference_search_calls = {}
local edit_should_fail = false
local opened_urls = {}

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = table.concat(command, " ")
  vim.defer_fn(function()
    if key == "gh api graphql --input -" then
      local graphql_payload = vim.json.decode(input or "{}")
      local query = tostring(graphql_payload.query or "")
      if query:find("markPullRequestReadyForReview", 1, true) then
        draft_status_calls[#draft_status_calls + 1] = { command = command, input = input, payload = graphql_payload }
        local stdout = vim.json.encode({
          data = {
            markPullRequestReadyForReview = {
              pullRequest = { id = "PR_kwTEST7", isDraft = false },
            },
          },
        })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      if query:find("convertPullRequestToDraft", 1, true) then
        draft_status_calls[#draft_status_calls + 1] = { command = command, input = input, payload = graphql_payload }
        local stdout = vim.json.encode({
          data = {
            convertPullRequestToDraft = {
              pullRequest = { id = "PR_kwTEST7", isDraft = true },
            },
          },
        })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      if query:find("statusCheckRollup", 1, true) then
        local stdout = vim.json.encode({
          data = {
            repository = {
              pullRequest = {
                commits = {
                  nodes = {
                    {
                      commit = {
                        statusCheckRollup = {
                          contexts = {
                            nodes = {
                              {
                                __typename = "CheckRun",
                                name = "Dummy Lint",
                                status = "COMPLETED",
                                conclusion = "SUCCESS",
                                detailsUrl = "https://github.com/owner/repo/actions/runs/123/job/456",
                                startedAt = "2026-06-14T18:00:00Z",
                                completedAt = "2026-06-14T18:01:00Z",
                                checkSuite = {
                                  workflowRun = {
                                    workflow = { name = "PR Dummy Checks" },
                                  },
                                },
                              },
                              {
                                __typename = "CheckRun",
                                name = "Dummy Unit Tests",
                                status = "COMPLETED",
                                conclusion = "FAILURE",
                                detailsUrl = "https://github.com/owner/repo/actions/runs/123/job/789",
                                startedAt = "2026-06-14T18:00:00Z",
                                completedAt = "2026-06-14T18:02:00Z",
                                checkSuite = {
                                  workflowRun = {
                                    workflow = { name = "PR Dummy Checks" },
                                  },
                                },
                              },
                            },
                          },
                        },
                      },
                    },
                  },
                },
              },
            },
          },
        })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      local stdout = vim.json.encode({
        data = {
          repository = {
            pullRequest = {
              reviews = {
                nodes = {
                  {
                    id = "PRR_1",
                    databaseId = 4493241278,
                    state = "CHANGES_REQUESTED",
                    body = "This requires a few changes...",
                    createdAt = "2026-06-14T17:13:00Z",
                    updatedAt = "2026-06-14T17:15:00Z",
                    submittedAt = "2026-06-14T17:15:00Z",
                    url = "https://github.com/owner/repo/pull/7#pullrequestreview-4493241278",
                    author = { login = "foo" },
                    commit = { oid = "abc1234def5678abc1234def5678abc1234def56" },
                  },
                  {
                    id = "PRR_2",
                    databaseId = 4493241279,
                    state = "APPROVED",
                    body = "LGTM!",
                    createdAt = "2026-06-14T17:16:00Z",
                    updatedAt = "2026-06-14T17:16:00Z",
                    submittedAt = "2026-06-14T17:16:00Z",
                    url = "https://github.com/owner/repo/pull/7#pullrequestreview-4493241279",
                    author = { login = "mgeorge" },
                    commit = { oid = "abc1234def5678abc1234def5678abc1234def56" },
                  },
                  {
                    id = "PRR_3",
                    databaseId = 4493241280,
                    state = "COMMENTED",
                    body = "",
                    createdAt = "2026-06-14T17:18:00Z",
                    updatedAt = "2026-06-14T17:18:00Z",
                    submittedAt = "2026-06-14T17:18:00Z",
                    url = "https://github.com/owner/repo/pull/7#pullrequestreview-4493241280",
                    author = { login = "me" },
                    commit = { oid = "abc1234def5678abc1234def5678abc1234def56" },
                  },
                  {
                    id = "PRR_4",
                    databaseId = 4493241281,
                    state = "COMMENTED",
                    body = "Needs a follow-up",
                    createdAt = "2026-06-14T17:19:00Z",
                    updatedAt = "2026-06-14T17:19:00Z",
                    submittedAt = "2026-06-14T17:19:00Z",
                    url = "https://github.com/owner/repo/pull/7#pullrequestreview-4493241281",
                    author = { login = "mgeorge" },
                    commit = { oid = "abc1234def5678abc1234def5678abc1234def56" },
                  },
                },
              },
              comments = {
                nodes = {
                  {
                    id = "IC_1",
                    databaseId = 4702465966,
                    body = "This is a regular comment",
                    createdAt = "2026-06-14T17:12:24Z",
                    updatedAt = "2026-06-14T17:12:24Z",
                    url = "https://github.com/owner/repo/pull/7#issuecomment-4702465966",
                    author = { login = "me" },
                  },
                  {
                    id = "IC_2",
                    databaseId = 4702465967,
                    body = "Lorem Ipsum is the ubiquitous placeholder\nSecond full line for expansion",
                    createdAt = "2026-06-14T22:00:00Z",
                    updatedAt = "2026-06-14T22:00:00Z",
                    url = "https://github.com/owner/repo/pull/7#issuecomment-4702465967",
                    author = { login = "me" },
                  },
                },
              },
              reviewThreads = {
                nodes = {
                  {
                    isResolved = false,
                    isOutdated = false,
                    path = "src/a.txt",
                    line = 5,
                    startLine = 5,
                    diffSide = "RIGHT",
                    comments = {
                      nodes = {
                        {
                          id = "PRRC_1",
                          databaseId = 3409923137,
                          body = "This is inline comment without review",
                          createdAt = "2026-06-14T17:14:07Z",
                          updatedAt = "2026-06-14T17:14:07Z",
                          url = "https://github.com/owner/repo/pull/7#discussion_r3409923137",
                          author = { login = "me" },
                          pullRequestReview = { id = "PRR_1", databaseId = 4493241278, state = "CHANGES_REQUESTED" },
                        },
                        {
                          id = "PRRC_REPLY_1",
                          databaseId = 3409923138,
                          body = "Oh good point! fixed",
                          createdAt = "2026-06-14T17:00:00Z",
                          updatedAt = "2026-06-14T17:00:00Z",
                          url = "https://github.com/owner/repo/pull/7#discussion_r3409923138",
                          author = { login = "foo" },
                          pullRequestReview = { id = "PRR_1", databaseId = 4493241278, state = "CHANGES_REQUESTED" },
                        },
                      },
                    },
                  },
                  {
                    isResolved = false,
                    isOutdated = false,
                    path = "src/a.txt",
                    line = 8,
                    startLine = 8,
                    diffSide = "RIGHT",
                    comments = {
                      nodes = {
                        {
                          id = "PRRC_2",
                          databaseId = 3409923139,
                          body = "Single inline review shell",
                          createdAt = "2026-06-14T17:18:00Z",
                          updatedAt = "2026-06-14T17:18:00Z",
                          url = "https://github.com/owner/repo/pull/7#discussion_r3409923139",
                          author = { login = "me" },
                          pullRequestReview = { id = "PRR_3", databaseId = 4493241280, state = "COMMENTED" },
                        },
                      },
                    },
                  },
                },
              },
            },
          },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key:find("gh pr diff", 1, true) then
      cb({ code = 0, stdout = pr_diff_text, stderr = "", output = pr_diff_text })
      return
    end
    if key == "gh api --method POST /repos/owner/repo/pulls/7/comments --input -" then
      local payload = vim.json.decode(input or "{}")
      standalone_comment_calls[#standalone_comment_calls + 1] = { command = command, input = input, payload = payload }
      local stdout = vim.json.encode({
        id = 3409923999,
        node_id = "PRRC_STANDALONE_1",
        body = payload.body,
        path = payload.path,
        line = payload.line,
        position = payload.position,
        side = payload.side,
        url = "https://api.github.com/repos/owner/repo/pulls/comments/3409923999",
        created_at = "2026-06-14T18:00:00Z",
        updated_at = "2026-06-14T18:00:00Z",
        html_url = "https://github.com/owner/repo/pull/7#discussion_r3409923999",
        user = { login = "me" },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api --method POST /repos/owner/repo/issues/7/comments --input -" then
      local payload = vim.json.decode(input or "{}")
      local call_index = #issue_comment_calls + 1
      local remote_id = 4702465998 + call_index
      issue_comment_calls[#issue_comment_calls + 1] = { command = command, input = input, payload = payload }
      local stdout = vim.json.encode({
        id = remote_id,
        node_id = "IC_NEW_" .. tostring(call_index),
        body = payload.body,
        html_url = "https://github.com/owner/repo/pull/7#issuecomment-" .. tostring(remote_id),
        created_at = "2026-06-14T18:30:00Z",
        updated_at = "2026-06-14T18:30:00Z",
        user = { login = "me" },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api /repos/owner/repo/contributors --paginate --slurp" then
      contributor_calls = contributor_calls + 1
      local stdout = vim.json.encode({
        {
          { login = "alice-dev" },
          { login = "bobtown" },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api /repos/owner/repo/collaborators --paginate --slurp" then
      collaborator_calls = collaborator_calls + 1
      local stdout = vim.json.encode({
        {
          { login = "mgeorge-esri" },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api /users/alice-dev" or key == "gh api /users/bobtown" or key == "gh api /users/mgeorge-esri" then
      local login = key:match("/users/(.+)$")
      user_lookup_calls[#user_lookup_calls + 1] = login
      local stdout = vim.json.encode({
        login = login,
        name = login == "alice-dev" and "Alice Developer" or (login == "bobtown" and "Bob Town" or "M George"),
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api --method POST /repos/owner/repo/pulls/7/requested_reviewers --input -" then
      local payload = vim.json.decode(input or "{}")
      reviewer_request_calls[#reviewer_request_calls + 1] = { command = command, input = input, payload = payload }
      local stdout = vim.json.encode({ requested_reviewers = payload.reviewers })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api --method DELETE /repos/owner/repo/pulls/7/requested_reviewers --input -" then
      local payload = vim.json.decode(input or "{}")
      reviewer_remove_calls[#reviewer_remove_calls + 1] = { command = command, input = input, payload = payload }
      cb({ code = 0, stdout = vim.json.encode({ requested_reviewers = {} }), stderr = "", output = "" })
      return
    end
    if key == "gh api /repos/owner/repo/milestones?state=all&per_page=100 --paginate --slurp" then
      milestone_list_calls = milestone_list_calls + 1
      local stdout = vim.json.encode({
        {
          { number = 100, title = "Backlog", state = "open" },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api --method POST /repos/owner/repo/milestones --input -" then
      local payload = vim.json.decode(input or "{}")
      milestone_create_calls[#milestone_create_calls + 1] = { command = command, input = input, payload = payload }
      local stdout = vim.json.encode({
        number = 501,
        title = payload.title,
        state = "open",
        html_url = "https://github.com/owner/repo/milestone/501",
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api --method PATCH /repos/owner/repo/issues/7 --input -" then
      local payload = vim.json.decode(input or "{}")
      milestone_set_calls[#milestone_set_calls + 1] = { command = command, input = input, payload = payload }
      local milestone = payload.milestone == 501 and {
        number = 501,
        title = "5.1",
        state = "open",
        html_url = "https://github.com/owner/repo/milestone/501",
      } or vim.NIL
      local stdout = vim.json.encode({ milestone = milestone })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key:find("gh search issues", 1, true) then
      issue_reference_search_calls[#issue_reference_search_calls + 1] = key
      local stdout = vim.json.encode({
        {
          number = 42,
          title = "Test issue match",
          url = "https://github.com/owner/repo/issues/42",
          repository = { nameWithOwner = "owner/repo" },
          author = { login = "me" },
          commentsCount = 0,
          updatedAt = "2026-06-14T20:00:00Z",
          state = "open",
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key:find("gh pr edit", 1, true) then
      edit_calls[#edit_calls + 1] = { command = command, input = input }
      if edit_should_fail then
        cb({ code = 1, stdout = "", stderr = "mock edit failure", output = "mock edit failure" })
      else
        cb({ code = 0, stdout = "", stderr = "", output = "" })
      end
      return
    end
    cb({ code = 1, stdout = "", stderr = "unexpected gh command: " .. key, output = "unexpected gh command: " .. key })
  end, 3)
end

function gh_backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

---@type DiffReviewGitBackend
local git_backend = {}

function git_backend.systemlist(command)
  return {}, 1
end

function git_backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    cb({}, 1)
  end, 3)
end

function git_backend.system(command)
  return "unexpected git command: " .. table.concat(command, " "), 1
end

function git_backend.system_async(command, input, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "", output = "" })
  end, 3)
end

function git_backend.delete()
  return 0
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function set_datetime_now(value)
  local epoch = diff_review._datetime.parse(value)
  assert_true(type(epoch) == "number", "test datetime did not parse: " .. tostring(value))
  diff_review._datetime.now_override = function() return epoch end
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function find_row(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function find_row_after(buf, needle, after_row)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index = after_row + 1, #lines do
    if lines[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. tostring(after_row) .. ": " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function line_has_highlight(buf, row, hl_group, start_col, end_col)
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
    local details = mark[4] or {}
    if details.hl_group == hl_group
      and (start_col == nil or mark[3] == start_col)
      and (end_col == nil or details.end_col == end_col) then
      return true
    end
  end
  return false
end

local function assert_cursor_clamped_to_line(buf, row, label)
  vim.api.nvim_win_set_buf(0, buf)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  vim.fn.setpos(".", { 0, row, #line + 1, 40 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  local cursor = vim.api.nvim_win_get_cursor(0)
  local pos = vim.fn.getcurpos()
  assert_true(cursor[1] == row, label .. " cursor moved rows: " .. vim.inspect(cursor))
  assert_true(cursor[2] == #line, label .. " cursor was not clamped to line end: " .. vim.inspect({ cursor = cursor, line = line }))
  assert_true((pos[4] or 0) == 0, label .. " cursor kept virtual columns: " .. vim.inspect(pos))
end

local pr_edit_ns = vim.api.nvim_create_namespace("diff_review_pr_edit")

--- 1-based rows that carry the "*" out-of-sync marker.
---@param buf integer
---@return integer[]
local function marker_rows(buf)
  local rows = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, pr_edit_ns, 0, -1, { details = true })) do
    local details = mark[4] or {}
    local virt = details.virt_text
    if virt and virt[1] and virt[1][1] == "*" then rows[#rows + 1] = mark[2] + 1 end
  end
  table.sort(rows)
  return rows
end

--- Move the cursor and fire the CursorMoved autocmd that re-syncs
--- 'modifiable' (nvim_win_set_cursor does not fire it on its own).
local function move_cursor(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
end

local function move_cursor_to_text(buf, row, text)
  vim.api.nvim_win_set_buf(0, buf)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local start_col = line:find(text, 1, true)
  assert_true(start_col ~= nil, "line did not contain cursor target " .. text .. ": " .. line)
  vim.api.nvim_win_set_cursor(0, { row, start_col - 1 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
end

--- Edit one buffer line the way a gated insert would, then leave insert.
local function edit_line(buf, row, text)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, row - 1, row, false, { text })
  vim.api.nvim_exec_autocmds("InsertLeave", { buffer = buf })
end

local function trigger_buf_mapping(buf, key)
  vim.api.nvim_win_set_buf(0, buf)
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

local function trigger_current_mapping(key)
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing current mapping for " .. key)
  mapping.callback()
end

local function assert_browse_url(buf, row, expected, label)
  move_cursor(buf, row)
  trigger_buf_mapping(buf, "b")
  assert_true(
    opened_urls[#opened_urls] == expected,
    ("%s opened %s instead of %s"):format(label, tostring(opened_urls[#opened_urls]), expected)
  )
end

local function reviewer_completion_labels(buf, row, text)
  edit_line(buf, row, text)
  move_cursor(buf, row)
  vim.api.nvim_win_set_cursor(0, { row, #text })
  local reviewer_source = require("diff_review.reviewer_source").new({})
  assert_true(reviewer_source:enabled(), "reviewer completion source did not enable on: " .. text)
  local completion_result
  reviewer_source:get_completions({}, function(result) completion_result = result end)
  local labels = {}
  for _, item in ipairs(completion_result.items or {}) do
    labels[item.label] = true
  end
  return labels
end

local function assert_contributor_completion(buf, row, text)
  local labels = reviewer_completion_labels(buf, row, text)
  assert_true(labels["@alice-dev"], "GitHub user completion did not include @alice-dev on: " .. text)
  assert_true(labels["@bobtown"], "GitHub user completion did not include @bobtown on: " .. text)
  assert_true(labels["@mgeorge-esri"], "GitHub user completion did not include collaborator @mgeorge-esri on: " .. text)
  assert_true(not labels["@foobar"], "GitHub user completion still included placeholder @foobar")
end

local function assert_issue_completion(buf, row, text)
  edit_line(buf, row, text)
  move_cursor(buf, row)
  local old_virtualedit = vim.o.virtualedit
  vim.o.virtualedit = "onemore"
  vim.api.nvim_win_set_cursor(0, { row, #text })
  local issue_source = require("github.issue_source").new({ debounce_ms = 0 })
  assert_true(issue_source:enabled(), "GitHub issue completion did not enable on: " .. text)
  local completion_result
  issue_source:get_completions({}, function(result) completion_result = result end)
  assert_true(vim.wait(1000, function() return completion_result ~= nil end, 10), "GitHub issue completion did not return on: " .. text)
  vim.o.virtualedit = old_virtualedit
  local labels = {}
  for _, item in ipairs(completion_result.items or {}) do
    labels[item.label] = item
  end
  assert_true(labels["#42 Test issue match"] ~= nil, "GitHub issue completion did not include #42 on: " .. text)
  assert_true(labels["#42 Test issue match"].textEdit.newText == "#42", "GitHub issue completion should insert only the issue id")
end

local function write_issue_snapshot(repo_name)
  local path = issue_index.snapshot_path(repo_name)
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = repo_name,
    state = "open",
    issue_count = 1,
    issues = {
      {
        repo = repo_name,
        number = 42,
        title = "Test issue match",
        state = "OPEN",
        url = "https://github.com/owner/repo/issues/42",
        labels = { { name = "test" }, { name = "z" } },
      },
    },
  }) }, path)
  assert_true(result == 0, "issue snapshot write failed")
end

local pr = {
  id = "PR_kwTEST7",
  number = 7,
  title = "Old title",
  body = "Line one\nLine two",
  url = "https://github.com/owner/repo/pull/7",
  repo = "owner/repo",
  headRefName = "feature",
  headRefOid = "abc1234def5678abc1234def5678abc1234def56",
  commits = {
    {
      oid = "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      messageHeadline = "feat: base commit",
      committedDate = "2026-06-13T03:20:00Z",
    },
    {
      oid = "abc1234def5678abc1234def5678abc1234def56",
      messageHeadline = "chore: head commit",
      committedDate = "2026-06-14T03:20:00Z",
    },
  },
  files = { { path = "src/a.txt", additions = 1, deletions = 1 } },
  changedFiles = 1,
  additions = 1,
  deletions = 1,
  isDraft = true,
}

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  github_gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  repo_cache.set_data_dir_for_test(repo_cache_dir)
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  set_datetime_now("2026-06-15T12:00:00Z")
  assert_true(diff_review._datetime.relative("2026-06-15T11:55:00Z") == "5 minutes ago", "minute date label failed")
  assert_true(diff_review._datetime.relative("2026-06-15T10:00:00Z") == "2 hours ago", "hour date label failed")
  assert_true(diff_review._datetime.relative("2026-06-14T12:00:00Z") == "Yesterday", "yesterday date label failed")
  assert_true(diff_review._datetime.relative("2026-06-14T12:00:00Z", { yesterday = false }) == "1 day ago", "one-day date label failed")
  assert_true(diff_review._datetime.relative("2026-06-12T12:00:00Z") == "3 days ago", "day date label failed")
  assert_true(diff_review._datetime.relative("2026-06-07T12:00:00Z") == "Last week", "last week date label failed")
  assert_true(diff_review._datetime.relative("2026-05-01T12:00:00Z") == "Last month", "last month date label failed")
  assert_true(diff_review._datetime.relative("2026-04-01T12:00:00Z") == "April 1, 2026", "absolute date label failed")
  assert_true(
    diff_review._datetime.action_phrase("mmgeorge", "commented", "2026-06-15T11:55:00Z") == "mmgeorge commented 5 minutes ago",
    "action phrase date label failed"
  )
  set_datetime_now("2026-06-15T03:20:00Z")
  issue_index._set_runner_for_test(function(command, _, callback)
    local key = table.concat(command, " ")
    if key:find(" state ", 1, true) then
      local stdout = vim.json.encode({
        repo = pr.repo,
        open_historical_complete = true,
        last_open_checked_at = os.time(),
      })
      callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    callback({ code = 0, stdout = "{}", stderr = "", output = "{}" })
  end)
  write_issue_snapshot(pr.repo)

  local draft_codeowner_text = diff_review._pr_overview.pending_review_text({
    isDraft = true,
    requestedReviewers = {
      { login = "codeowners", is_code_owner = true },
      { login = "alice-dev" },
    },
  }, nil)
  assert_true(
    draft_codeowner_text == diff_review._pending_review_icon .. " " .. diff_review._codeowner_review_icon .. "@codeowners @alice-dev",
    "draft codeowner reviewers should render with a warning icon: " .. tostring(draft_codeowner_text)
  )
  local ready_codeowner_text = diff_review._pr_overview.pending_review_text({
    isDraft = false,
    requestedReviewers = {
      { login = "codeowners", is_code_owner = true },
    },
  }, nil)
  assert_true(
    ready_codeowner_text == diff_review._pending_review_icon .. " @codeowners",
    "ready codeowner reviewers should render without a draft warning icon: " .. tostring(ready_codeowner_text)
  )

  local buf = diff_review.open_pr(pr, { cwd = "D:/diffreview-pr-edit-root" })
  assert_true(buf ~= nil, "open_pr did not return a buffer")
  wait_for(function() return buffer_contains(buf, "Title:  Old title") end, "PR view did not render the title")
  assert_true(vim.wo[0].wrap, "PR buffer should enable soft wrap")
  assert_true(vim.wo[0].linebreak, "PR buffer should wrap on word boundaries")
  assert_true(vim.wo[0].breakindent, "PR buffer should preserve indent on wrapped screen lines")
  assert_true(buffer_contains(buf, "Line one"), "PR body did not render")
  wait_for(function() return buffer_contains(buf, "This is a regular comment") end, "PR conversation comment did not render")
  assert_true(buffer_contains(buf, "Comments (2):"), "PR comments heading did not use section heading format")
  local first_regular_comment_row = find_row(buf, "This is a regular comment")
  local first_regular_comment_line = vim.api.nvim_buf_get_lines(buf, first_regular_comment_row - 1, first_regular_comment_row, false)[1] or ""
  assert_true(
    first_regular_comment_line:find("me 10 hours ago  This is a regular comment", 1, true) ~= nil,
    "regular PR comment row did not align metadata without action text: " .. first_regular_comment_line
  )
  assert_true(not first_regular_comment_line:find("|", 1, true), "regular PR comment row kept a pipe: " .. first_regular_comment_line)
  assert_true(not first_regular_comment_line:find("commented", 1, true), "regular PR comment row kept action text: " .. first_regular_comment_line)
  assert_true(
    buffer_contains(buf, "Lorem Ipsum is the ubiquitous placeholder"),
    "long PR conversation comment preview did not render"
  )
  local long_regular_preview_row = find_row(buf, "Lorem Ipsum is the ubiquitous placeholder")
  local long_regular_preview_line = vim.api.nvim_buf_get_lines(buf, long_regular_preview_row - 1, long_regular_preview_row, false)[1] or ""
  assert_true(
    long_regular_preview_line:find("me 5 hours ago   Lorem Ipsum is the ubiquitous placeholder", 1, true) ~= nil,
    "long PR comment row did not align the shorter date column: " .. long_regular_preview_line
  )
  assert_true(
    not buffer_contains(buf, "Second full line for expansion"),
    "long PR conversation comment should render collapsed by default"
  )
  wait_for(function() return buffer_contains(buf, "Reviews (3):") end, "submitted reviews section did not render")
  assert_true(
    not buffer_contains(buf, "Single inline review shell"),
    "single inline-comment review shell should not render in Reviews"
  )
  assert_true(buffer_contains(buf, "Changes (1):"), "PR changes heading did not use section heading format")
  assert_true(
    line_has_highlight(buf, find_row(buf, "Description:"), "DiffReviewStatusHeader", 0, #"Description:"),
    "PR description heading did not use header highlight"
  )
  wait_for(function() return buffer_contains(buf, "Dummy Lint") end, "PR checks section did not render")
  local description_row = find_row(buf, "Description:")
  local lint_check_row = find_row(buf, "Dummy Lint")
  local lint_check_line = vim.api.nvim_buf_get_lines(buf, lint_check_row - 1, lint_check_row, false)[1] or ""
  local checks_status_row = lint_check_row - 1
  local checks_status_line = vim.api.nvim_buf_get_lines(buf, checks_status_row - 1, checks_status_row, false)[1] or ""
  assert_true(checks_status_line == "Status:", "PR checks heading did not render above the check rows")
  assert_true(checks_status_row < description_row, "PR checks heading did not render before Description")
  assert_true(lint_check_row < description_row, "PR check row did not render before Description")
  assert_true(lint_check_line:match("^%S") ~= nil, "PR check row should not be indented: " .. lint_check_line)
  assert_true(
    lint_check_line:find("✓ Dummy Lint | PR Dummy Checks", 1, true) ~= nil,
    "PR check row did not render without state text: " .. lint_check_line
  )
  assert_true(not lint_check_line:find("SUCCESS", 1, true), "PR check row should not render state text: " .. lint_check_line)
  assert_true(buffer_contains(buf, "Dummy Unit Tests"), "non-green PR check did not render")
  local unit_check_row = find_row(buf, "Dummy Unit Tests")
  local unit_check_line = vim.api.nvim_buf_get_lines(buf, unit_check_row - 1, unit_check_row, false)[1] or ""
  assert_true(unit_check_line:match("^%S") ~= nil, "failing PR check row should not be indented: " .. unit_check_line)
  assert_true(
    unit_check_line:find("✗ Dummy Unit Tests | PR Dummy Checks", 1, true) ~= nil,
    "failing PR check row did not render without state text: " .. unit_check_line
  )
  assert_true(not unit_check_line:find("FAILURE", 1, true), "failing PR check row should not render state text: " .. unit_check_line)
  assert_true(
    line_has_highlight(buf, checks_status_row, "DiffReviewStatusHeader", 0, #"Status:"),
    "PR checks heading did not use header highlight"
  )
  wait_for(function()
    return buffer_contains(buf, "foo     10 hours ago  This requires a few changes...")
  end, "rejected review summary did not render")
  assert_true(buffer_contains(buf, "mgeorge 10 hours ago  LGTM!"), "approved review summary did not render")
  assert_true(buffer_contains(buf, "mgeorge 10 hours ago  Needs a follow-up"), "commented review summary did not render")
  assert_true(not buffer_contains(buf, "REJECTED by"), "review summary should not render rejected state text")
  assert_true(not buffer_contains(buf, "APPROVED by"), "review summary should not render approved state text")
  assert_true(not buffer_contains(buf, "COMMENTED by"), "review summary should not render commented state text")
  local rejected_summary_row = find_row(buf, "foo     10 hours ago")
  local approved_summary_row = find_row(buf, "mgeorge 10 hours ago  LGTM!")
  local commented_summary_row = find_row(buf, "mgeorge 10 hours ago  Needs a follow-up")
  for _, review_summary_row in ipairs({ rejected_summary_row, approved_summary_row, commented_summary_row }) do
    local review_summary_line = vim.api.nvim_buf_get_lines(buf, review_summary_row - 1, review_summary_row, false)[1] or ""
    assert_true(not review_summary_line:find("|", 1, true), "review summary kept a pipe: " .. review_summary_line)
    assert_true(not review_summary_line:find("requested changes", 1, true), "review summary kept requested-changes action text: " .. review_summary_line)
    assert_true(not review_summary_line:find("approved", 1, true), "review summary kept approved action text: " .. review_summary_line)
    assert_true(not review_summary_line:find("commented", 1, true), "review summary kept commented action text: " .. review_summary_line)
    assert_true(not line_has_highlight(buf, review_summary_row, "DiffReviewReviewCommentHeader"), "review summary should not use blue header highlight: " .. review_summary_line)
  end
  assert_true(line_has_highlight(buf, rejected_summary_row, "DiffReviewDeleteRange"), "rejected review summary did not highlight status red")
  assert_true(line_has_highlight(buf, approved_summary_row, "DiffReviewAddRange"), "approved review summary did not highlight status green")
  assert_true(not line_has_highlight(buf, commented_summary_row, "DiffReviewAddRange"), "commented review summary should not be green")
  assert_true(not line_has_highlight(buf, commented_summary_row, "DiffReviewDeleteRange"), "commented review summary should not be red")
  local changes_file_row = find_row(buf, "src/a.txt +1 -1")
  local recent_commits_row = find_row(buf, "Recent Commits (2):")
  assert_true(recent_commits_row > changes_file_row, "PR recent commits section did not render at the end")
  assert_true(
    not pcall(find_row_after, buf, "abc1234  1 day ago  chore: head commit", recent_commits_row),
    "PR recent commits should start folded"
  )
  move_cursor(buf, recent_commits_row)
  trigger_buf_mapping(buf, "<Tab>")
  local head_commit_row = find_row_after(buf, "abc1234  1 day ago  chore: head commit", recent_commits_row)
  local base_commit_row = find_row_after(buf, "1111111  2 days ago feat: base commit", head_commit_row)
  assert_true(head_commit_row > recent_commits_row, "PR head commit did not render under Recent Commits")
  assert_true(base_commit_row > head_commit_row, "PR commits did not render newest first")
  move_cursor(buf, changes_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return buffer_contains(buf, "NEW LINE") end, "PR changed file did not expand")
  wait_for(function() return buffer_contains(buf, "This is inline comment without review") end, "PR inline code comment did not render")
  wait_for(function() return buffer_contains(buf, "Oh good point! fixed") end, "PR inline reply did not render")
  local changed_code_row = find_row(buf, "NEW LINE")
  local inline_comment_row = find_row_after(buf, "This is inline comment without review", changes_file_row)
  assert_true(
    inline_comment_row > changed_code_row,
    "inline code comment rendered before its code row"
  )
  local inline_reply_row = find_row_after(buf, "Oh good point! fixed", inline_comment_row)
  assert_true(
    inline_reply_row > inline_comment_row,
    "inline reply did not render under its parent comment"
  )
  move_cursor(buf, inline_comment_row)
  trigger_buf_mapping(buf, "<Tab>")
  local expected_folded_preview = diff_review._review.comment_icon
    .. " me commented 10 hours ago | This is inline comment without review"
  wait_for(function()
    local ok, folded_row = pcall(find_row_after, buf, "This is inline comment without review", changes_file_row)
    if not ok then return false end
    local folded_line = vim.api.nvim_buf_get_lines(buf, folded_row - 1, folded_row, false)[1] or ""
    return folded_line:find(expected_folded_preview, 1, true) ~= nil
      and not buffer_contains(buf, "Oh good point! fixed")
  end, "PR inline code comment did not fold")
  inline_comment_row = find_row_after(buf, "This is inline comment without review", changes_file_row)
  move_cursor(buf, inline_comment_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return buffer_contains(buf, "Oh good point! fixed") end, "PR inline code comment did not unfold")
  inline_comment_row = find_row_after(buf, "This is inline comment without review", changes_file_row)
  inline_reply_row = find_row_after(buf, "Oh good point! fixed", inline_comment_row)
  local rejected_review_row = find_row(buf, "foo     10 hours ago")
  local regular_comment_row = find_row(buf, "This is a regular comment")
  local long_regular_comment_row = find_row(buf, "Lorem Ipsum is the ubiquitous placeholder")
  move_cursor(buf, long_regular_comment_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return buffer_contains(buf, "Second full line for expansion")
  end, "regular PR conversation comment did not expand")
  assert_browse_url(
    buf,
    find_row(buf, "Second full line for expansion"),
    "https://github.com/owner/repo/pull/7#issuecomment-4702465967",
    "expanded regular PR comment body"
  )
  long_regular_comment_row = find_row(buf, "Lorem Ipsum is the ubiquitous placeholder")
  move_cursor(buf, long_regular_comment_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return not buffer_contains(buf, "Second full line for expansion")
  end, "regular PR conversation comment did not collapse")
  opened_urls = {}
  assert_browse_url(buf, lint_check_row, "https://github.com/owner/repo/actions/runs/123/job/456", "PR check row")
  assert_browse_url(buf, regular_comment_row, "https://github.com/owner/repo/pull/7#issuecomment-4702465966", "regular PR comment")
  assert_browse_url(buf, rejected_review_row, "https://github.com/owner/repo/pull/7#pullrequestreview-4493241278", "review summary")
  assert_browse_url(buf, inline_comment_row, "https://github.com/owner/repo/pull/7#discussion_r3409923137", "inline code comment")
  assert_browse_url(buf, inline_reply_row, "https://github.com/owner/repo/pull/7#discussion_r3409923138", "inline code reply")

  move_cursor(buf, regular_comment_row)
  trigger_buf_mapping(buf, "C")
  wait_for(function() return buffer_contains(buf, "Comments (3):") end, "regular PR comment draft did not update the comments count")
  local regular_draft_header_row = find_row_after(buf, diff_review._review.comment_icon .. " you", regular_comment_row)
  assert_true(
    not line_has_highlight(buf, regular_draft_header_row, "DiffReviewReviewCommentHeader"),
    "new regular PR comment used the blue review-comment header highlight"
  )
  assert_true(
    line_has_highlight(buf, regular_draft_header_row, "DiffReviewReviewComment"),
    "new regular PR comment did not use the regular comment highlight"
  )
  local regular_draft_body_row = regular_draft_header_row + 1
  edit_line(buf, regular_draft_body_row, "Regular from PR overview")
  assert_true(vim.bo[buf].modified, "regular PR comment edit must mark the PR buffer modified")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #issue_comment_calls == 1 end, "regular PR comment was not posted")
  local issue_payload = issue_comment_calls[1].payload
  assert_true(issue_payload.body == "Regular from PR overview", "wrong regular PR comment body: " .. vim.inspect(issue_payload))
  wait_for(function() return saw_notification_containing("PR comment synced") end, "successful regular PR comment sync was not notified")
  assert_true(buffer_contains(buf, "Regular from PR overview"), "posted regular PR comment disappeared from the PR overview")
  local synced_regular_row = find_row(buf, "Regular from PR overview")
  assert_true(
    not line_has_highlight(buf, synced_regular_row, "DiffReviewReviewCommentHeader"),
    "synced regular PR comment used the blue review-comment header highlight"
  )
  assert_browse_url(
    buf,
    synced_regular_row,
    "https://github.com/owner/repo/pull/7#issuecomment-4702465999",
    "fresh regular PR comment"
  )

  changes_file_row = find_row(buf, "src/a.txt +1 -1")
  changed_code_row = find_row(buf, "NEW LINE")
  move_cursor(buf, changed_code_row)
  trigger_buf_mapping(buf, "C")
  local standalone_header_row = find_row_after(buf, "you commented", changed_code_row)
  local standalone_body_row = standalone_header_row + 1
  assert_issue_completion(buf, standalone_body_row, "Standalone #t")
  assert_contributor_completion(buf, standalone_body_row, "Standalone @")
  edit_line(buf, standalone_body_row, "Standalone from PR overview")
  assert_true(vim.bo[buf].modified, "standalone inline comment edit must mark the PR buffer modified")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #standalone_comment_calls == 1 end, "standalone inline comment was not posted")
  local standalone_payload = standalone_comment_calls[1].payload
  assert_true(standalone_payload.body == "Standalone from PR overview", "wrong standalone comment body: " .. vim.inspect(standalone_payload))
  assert_true(standalone_payload.commit_id == pr.headRefOid, "standalone comment did not target the PR head commit")
  assert_true(standalone_payload.path == "src/a.txt", "standalone comment path was wrong")
  assert_true(standalone_payload.line == 5, "standalone comment line was wrong")
  assert_true(standalone_payload.side == "RIGHT", "standalone comment side was wrong")
  wait_for(function() return saw_notification_containing("Inline comment synced") end, "successful standalone sync was not notified")
  assert_true(buffer_contains(buf, "Standalone from PR overview"), "posted standalone comment disappeared from the PR overview")
  assert_browse_url(
    buf,
    find_row(buf, "Standalone from PR overview"),
    "https://github.com/owner/repo/pull/7#discussion_r3409923999",
    "fresh standalone inline code comment"
  )

  rejected_review_row = find_row(buf, "foo     10 hours ago")
  move_cursor(buf, rejected_review_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    local ok, review_comment_row = pcall(find_row_after, buf, "This is inline comment without review", rejected_review_row)
    if not ok then return false end
    return review_comment_row < find_row_after(buf, "Changes", rejected_review_row)
  end, "expanded review did not show inline comment context")
  local changes_heading_row = find_row_after(buf, "Changes", rejected_review_row)
  local expanded_file_row = find_row_after(buf, "src/a.txt +1 -1", rejected_review_row)
  assert_true(
    expanded_file_row < changes_heading_row,
    "expanded review did not render the commented file"
  )
  local expanded_code_row = find_row_after(buf, "NEW LINE", expanded_file_row)
  assert_true(expanded_code_row < changes_heading_row, "expanded review did not render diff context")
  local expanded_review_lines = table.concat(vim.api.nvim_buf_get_lines(buf, rejected_review_row - 1, changes_heading_row - 1, false), "\n")
  assert_true(
    not expanded_review_lines:find("far context before", 1, true),
    "expanded review rendered leading hunk content outside the comment context"
  )
  assert_true(
    not expanded_review_lines:find("far context after", 1, true),
    "expanded review rendered trailing hunk content outside the comment context"
  )
  local expanded_parent_row = find_row_after(buf, "This is inline comment without review", expanded_code_row)
  local expanded_reply_row = find_row_after(buf, "Oh good point! fixed", expanded_parent_row)
  assert_true(expanded_reply_row > expanded_parent_row, "expanded review reply did not render under the parent comment")
  assert_true(expanded_reply_row < changes_heading_row, "expanded review reply rendered in the wrong section")
  assert_true(buffer_contains(buf, "foo replied 10 hours ago"), "expanded review reply header did not render author and timestamp")

  move_cursor(buf, expanded_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    local current_review_row = find_row(buf, "foo     10 hours ago")
    local current_changes_row = find_row_after(buf, "Changes", current_review_row)
    local review_block = table.concat(vim.api.nvim_buf_get_lines(buf, current_review_row - 1, current_changes_row - 1, false), "\n")
    return review_block:find("src/a.txt +1 -1", 1, true)
      and not review_block:find("NEW LINE", 1, true)
      and not review_block:find("This is inline comment without review", 1, true)
      and not review_block:find("Oh good point! fixed", 1, true)
  end, "expanded review file did not fold its diff context")

  rejected_review_row = find_row(buf, "foo     10 hours ago")
  expanded_file_row = find_row_after(buf, "src/a.txt +1 -1", rejected_review_row)
  move_cursor(buf, expanded_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    local current_changes_row = find_row_after(buf, "Changes", rejected_review_row)
    local ok, current_code_row = pcall(find_row_after, buf, "NEW LINE", expanded_file_row)
    return ok and current_code_row < current_changes_row
  end, "expanded review file did not reopen after folding")
  changes_heading_row = find_row_after(buf, "Changes", rejected_review_row)
  expanded_code_row = find_row_after(buf, "NEW LINE", expanded_file_row)
  expanded_parent_row = find_row_after(buf, "This is inline comment without review", expanded_code_row)
  expanded_reply_row = find_row_after(buf, "Oh good point! fixed", expanded_parent_row)
  assert_true(expanded_reply_row < changes_heading_row, "expanded review reply rendered in the wrong section after reopening")
  local previous_issue_comment_count = #issue_comment_calls
  local previous_standalone_comment_count = #standalone_comment_calls
  move_cursor(buf, expanded_code_row)
  trigger_buf_mapping(buf, "C")
  wait_for(function() return buffer_contains(buf, "Comments (4):") end, "review-context C did not create a regular PR comment")
  local context_regular_header_row = find_row_after(buf, diff_review._review.comment_icon .. " you", find_row(buf, "Regular from PR overview"))
  local context_regular_body_row = context_regular_header_row + 1
  edit_line(buf, context_regular_body_row, "Regular from review context")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #issue_comment_calls == previous_issue_comment_count + 1 end, "review-context regular PR comment was not posted")
  assert_true(
    #standalone_comment_calls == previous_standalone_comment_count,
    "review-context C must not create an inline code comment"
  )
  assert_true(
    issue_comment_calls[#issue_comment_calls].payload.body == "Regular from review context",
    "wrong review-context regular PR comment body: " .. vim.inspect(issue_comment_calls[#issue_comment_calls].payload)
  )
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(vim.bo[buf].buftype == "acwrite", "PR buffer must be acwrite")
  assert_true(not vim.bo[buf].modifiable, "PR buffer must start nomodifiable")

  local title_row = find_row(buf, "Title:  Old title")
  local milestone_row = find_row(buf, "Milestone:")
  local review_row = find_row(buf, "Review:")
  local status_row = find_row(buf, "Status: DRAFT")
  local body_row = find_row(buf, "Line one")
  assert_true(milestone_row < review_row, "Milestone row should render above Review row")
  assert_true(review_row < status_row, "Status row should render below Review row")
  assert_true(
    line_has_highlight(buf, review_row, "DiffReviewStatusLabel", 0, #"Review: "),
    "PR review request row did not use label highlight"
  )
  assert_true(
    line_has_highlight(buf, milestone_row, "DiffReviewStatusLabel", 0, #"Milestone: "),
    "PR milestone row did not use label highlight"
  )
  assert_true(
    line_has_highlight(buf, status_row, "DiffReviewStatusLabel", 0, #"Status: "),
    "PR status row did not use label highlight"
  )

  -- ── the buffer unlocks exactly on the editable regions ─────────────────────
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(not vim.bo[buf].modifiable, "buffer must stay locked outside the editable regions")
  move_cursor(buf, title_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on the title row")
  assert_cursor_clamped_to_line(buf, title_row, "PR title")
  move_cursor(buf, review_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on the review request row")
  assert_cursor_clamped_to_line(buf, review_row, "PR review request")
  move_cursor(buf, milestone_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on the milestone row")
  assert_cursor_clamped_to_line(buf, milestone_row, "PR milestone")
  move_cursor(buf, status_row)
  assert_true(not vim.bo[buf].modifiable, "PR status row must stay locked")
  move_cursor(buf, find_row(buf, "Description:"))
  assert_true(not vim.bo[buf].modifiable, "the Description: label itself must stay locked")
  move_cursor(buf, body_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on description rows")
  assert_cursor_clamped_to_line(buf, body_row, "PR description")
  issue_reference_search_calls = {}
  assert_issue_completion(buf, body_row, "Line #z")
  assert_true(
    #issue_reference_search_calls == 0,
    "PR description issue completion should not call gh search: " .. vim.inspect(issue_reference_search_calls)
  )

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, body_row - 1, body_row, false, { "Line one INSERT EDIT" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
  local insert_rows = marker_rows(buf)
  assert_true(
    #insert_rows == 1 and insert_rows[1] == find_row(buf, "Description:"),
    "insert-mode description edit did not immediately mark Description dirty: " .. vim.inspect(insert_rows)
  )
  vim.api.nvim_buf_set_lines(buf, body_row - 1, body_row, false, { "Line one" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = buf })
  assert_true(#marker_rows(buf) == 0, "restoring PR description during insert mode did not clear dirty marker")

  edit_line(buf, body_row, "Line one")
  assert_true(#marker_rows(buf) == 0, "restoring PR description after issue completion left dirty markers")
  vim.bo[buf].modified = false
  move_cursor(buf, regular_comment_row)
  assert_true(not vim.bo[buf].modifiable, "regular PR comment row must not be editable as description text")
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(not vim.bo[buf].modifiable, "buffer must relock after leaving the regions")

  -- ── Status: Enter confirms draft/ready transitions ─────────────────────────
  move_cursor_to_text(buf, status_row, "DRAFT")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return buffer_contains(vim.api.nvim_get_current_buf(), "Mark PR #7 ready for review?")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Status will change from DRAFT to READY.")
  end, "ready-for-review confirmation did not render")
  trigger_current_mapping("y")
  wait_for(function() return #draft_status_calls == 1 end, "mark ready mutation was not sent")
  assert_true(
    draft_status_calls[1].payload.variables.input.pullRequestId == "PR_kwTEST7",
    "mark ready mutation used wrong PR node id: " .. vim.inspect(draft_status_calls[1].payload)
  )
  wait_for(function() return buffer_contains(buf, "Status: READY") end, "PR status row did not switch to READY")
  assert_true(pr.isDraft == false, "PR cache did not switch to ready")

  status_row = find_row(buf, "Status: READY")
  move_cursor_to_text(buf, status_row, "READY")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return buffer_contains(vim.api.nvim_get_current_buf(), "Move PR #7 back to draft?")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Status will change from READY to DRAFT.")
  end, "draft confirmation did not render")
  trigger_current_mapping("y")
  wait_for(function() return #draft_status_calls == 2 end, "convert-to-draft mutation was not sent")
  assert_true(
    draft_status_calls[2].payload.variables.input.pullRequestId == "PR_kwTEST7",
    "convert draft mutation used wrong PR node id: " .. vim.inspect(draft_status_calls[2].payload)
  )
  wait_for(function() return buffer_contains(buf, "Status: DRAFT") end, "PR status row did not switch back to DRAFT")
  assert_true(pr.isDraft == true, "PR cache did not switch back to draft")

  -- ── Review: completes cached repo contributors and sends reviewer requests ──
  wait_for(function()
    return contributor_calls == 1 and collaborator_calls == 1 and #repo_cache.contributors("owner/repo") == 3
  end, "repo contributors were not cached")
  assert_contributor_completion(buf, review_row, "Review: @")

  edit_line(buf, review_row, "Review: @alice-dev @bobtown")
  assert_true(vim.bo[buf].modified, "reviewer request edit must mark the PR buffer modified")
  local rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == review_row, "reviewer request edit did not mark the Review row dirty: " .. vim.inspect(rows))
  captured_notifications = {}
  trigger_buf_mapping(buf, "<C-s>")
  assert_true(#marker_rows(buf) == 0, "reviewer request save must clear markers before confirmation")
  wait_for(function()
    return #user_lookup_calls == 2
      and buffer_contains(vim.api.nvim_get_current_buf(), "@alice-dev (Alice Developer)")
      and buffer_contains(vim.api.nvim_get_current_buf(), "@bobtown (Bob Town)")
  end, "reviewer request confirmation did not resolve display names")
  trigger_current_mapping("y")
  wait_for(function() return #reviewer_request_calls == 1 end, "reviewer request was not sent")
  local reviewer_payload = reviewer_request_calls[1].payload
  assert_true(vim.deep_equal(reviewer_payload.reviewers, { "alice-dev", "bobtown" }), "wrong reviewers payload: " .. vim.inspect(reviewer_payload))
  wait_for(function() return saw_notification_containing("Review requests updated: requested @alice-dev, @bobtown") end, "successful reviewer request was not notified")
  assert_true(
    vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: " .. diff_review._pending_review_icon .. " @alice-dev @bobtown",
    "Review row did not show pending reviewers after reviewer request"
  )
  assert_true(not vim.bo[buf].modified, "reviewer request sync must clear the modified flag")

  edit_line(buf, review_row, "Review: " .. diff_review._pending_review_icon .. " @alice-dev")
  assert_true(vim.bo[buf].modified, "reviewer removal edit must mark the PR buffer modified")
  rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == review_row, "reviewer removal edit did not mark the Review row dirty")
  captured_notifications = {}
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function()
    return #user_lookup_calls == 3
      and buffer_contains(vim.api.nvim_get_current_buf(), "Confirm review request changes:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Remove review request:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "@bobtown (Bob Town)")
  end, "reviewer removal confirmation did not describe the pending removal")
  trigger_current_mapping("n")
  wait_for(function()
    return vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: " .. diff_review._pending_review_icon .. " @alice-dev @bobtown"
  end, "cancelled reviewer removal did not restore the Review row")
  assert_true(#reviewer_remove_calls == 0, "cancelled reviewer removal still called GitHub")
  assert_true(#marker_rows(buf) == 0, "cancelled reviewer removal did not clear the marker")
  assert_true(not vim.bo[buf].modified, "cancelled reviewer removal did not clear modified state")

  -- ── edits flag the field with a "*" marker ──────────────────────────────────
  edit_line(buf, title_row, "Title:  New title")
  rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == title_row, "title edit did not add exactly one marker on the title row")
  assert_true(vim.bo[buf].modified, "title edit must mark the buffer modified")

  edit_line(buf, body_row, "Line one EDITED")
  rows = marker_rows(buf)
  assert_true(#rows == 2, "description edit did not add a second marker")
  assert_true(rows[2] == find_row(buf, "Description:"), "description marker is not on the Description: label")

  -- ── re-renders are blocked while dirty ──────────────────────────────────────
  captured_notifications = {}
  trigger_buf_mapping(buf, "R")
  vim.wait(120, function() return false end, 10)
  assert_true(buffer_contains(buf, "New title"), "refresh clobbered unsynced edits")
  assert_true(saw_notification_containing("Unsynced PR edits"), "blocked refresh did not explain itself")

  -- ── :w clears markers immediately and syncs through the queue ──────────────
  captured_notifications = {}
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  assert_true(#marker_rows(buf) == 0, "markers must clear immediately on save")
  assert_true(not vim.bo[buf].modified, "save must clear the modified flag")
  wait_for(function() return #edit_calls == 1 end, "gh pr edit was not invoked")
  local call = edit_calls[1]
  local call_key = table.concat(call.command, " ")
  assert_true(call_key:find("gh pr edit 7", 1, true) ~= nil, "wrong gh edit command: " .. call_key)
  assert_true(call_key:find("--title New title", 1, true) ~= nil, "title missing from gh edit: " .. call_key)
  assert_true(call_key:find("--body-file -", 1, true) ~= nil, "body flag missing from gh edit: " .. call_key)
  assert_true(call.input == "Line one EDITED\nLine two", "wrong body sent: " .. tostring(call.input))
  wait_for(function() return saw_notification_containing("PR #7 updated") end, "successful sync was not notified")
  assert_true(pr.title == "New title", "PR cache title was not updated")
  assert_true(pr.body == "Line one EDITED\nLine two", "PR cache body was not updated")
  assert_true(#marker_rows(buf) == 0, "markers must stay clear after a successful sync")
  assert_true(buffer_contains(buf, "Title:  New title"), "post-sync re-render lost the new title")
  assert_true(
    buffer_contains(buf, "Review: " .. diff_review._pending_review_icon .. " @alice-dev @bobtown"),
    "post-sync re-render lost pending reviewers"
  )

  -- ── unchanged save is a no-op ───────────────────────────────────────────────
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  vim.wait(120, function() return false end, 10)
  assert_true(#edit_calls == 1, "no-op save must not call gh")

  -- ── Review: mixed removals and new requests are confirmed together ─────────
  edit_line(buf, review_row, "Review: " .. diff_review._pending_review_icon .. " @bobtown @mgeorge-esri")
  assert_true(vim.bo[buf].modified, "mixed reviewer edit must mark the PR buffer modified")
  rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == review_row, "mixed reviewer edit did not mark the Review row dirty")
  captured_notifications = {}
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function()
    return #user_lookup_calls == 5
      and buffer_contains(vim.api.nvim_get_current_buf(), "Confirm review request changes:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Remove review request:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "@alice-dev (Alice Developer)")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Request review:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "@mgeorge-esri (M George)")
  end, "mixed reviewer confirmation did not describe removals and requests")
  trigger_current_mapping("y")
  wait_for(function() return #reviewer_remove_calls == 1 and #reviewer_request_calls == 2 end, "mixed reviewer change did not call GitHub")
  assert_true(
    vim.deep_equal(reviewer_remove_calls[1].payload.reviewers, { "alice-dev" }),
    "wrong reviewer removal payload: " .. vim.inspect(reviewer_remove_calls[1].payload)
  )
  assert_true(
    vim.deep_equal(reviewer_request_calls[2].payload.reviewers, { "mgeorge-esri" }),
    "wrong mixed reviewer request payload: " .. vim.inspect(reviewer_request_calls[2].payload)
  )
  wait_for(function() return saw_notification_containing("Review requests updated: removed @alice-dev; requested @mgeorge-esri") end, "mixed reviewer update was not notified")
  assert_true(
    vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: " .. diff_review._pending_review_icon .. " @bobtown @mgeorge-esri",
    "mixed reviewer update did not render the desired pending reviewers"
  )
  assert_true(not vim.bo[buf].modified, "mixed reviewer sync must clear the modified flag")

  -- ── Milestone: missing milestone is confirmed, created, then assigned ──────
  edit_line(buf, milestone_row, "Milestone: 5.1")
  assert_true(vim.bo[buf].modified, "milestone edit must mark the PR buffer modified")
  rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == milestone_row, "milestone edit did not mark the Milestone row dirty")
  captured_notifications = {}
  trigger_buf_mapping(buf, "<C-s>")
  assert_true(#marker_rows(buf) == 0, "milestone save must clear markers before confirmation")
  wait_for(function()
    return milestone_list_calls == 1
      and buffer_contains(vim.api.nvim_get_current_buf(), "Milestone not found:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "5.1")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Create it now?")
  end, "missing milestone confirmation did not render")
  trigger_current_mapping("y")
  wait_for(function()
    return #milestone_create_calls == 1 and #milestone_set_calls == 1
  end, "missing milestone was not created and assigned")
  assert_true(milestone_create_calls[1].payload.title == "5.1", "wrong milestone create payload: " .. vim.inspect(milestone_create_calls[1].payload))
  assert_true(milestone_set_calls[1].payload.milestone == 501, "wrong milestone set payload: " .. vim.inspect(milestone_set_calls[1].payload))
  wait_for(function() return saw_notification_containing("Milestone updated: " .. diff_review._milestone_icon .. " 5.1") end, "successful milestone update was not notified")
  assert_true(
    vim.api.nvim_buf_get_lines(buf, milestone_row - 1, milestone_row, false)[1] == "Milestone: " .. diff_review._milestone_icon .. " 5.1",
    "Milestone row did not show the assigned milestone"
  )
  assert_true(pr.milestone and pr.milestone.title == "5.1", "PR cache milestone was not updated")
  assert_true(not vim.bo[buf].modified, "milestone sync must clear the modified flag")

  -- ── failed sync restores the markers and notifies ───────────────────────────
  edit_should_fail = true
  captured_notifications = {}
  edit_line(buf, find_row(buf, "Title:  New title"), "Title:  Broken title")
  assert_true(#marker_rows(buf) == 1, "second title edit did not mark dirty")
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  assert_true(#marker_rows(buf) == 0, "markers must clear immediately on save even when sync later fails")
  wait_for(function() return saw_notification_containing("GitHub PR update failed") end, "failed sync was not notified")
  assert_true(#marker_rows(buf) == 1, "failed sync must restore the dirty marker")
  assert_true(vim.bo[buf].modified, "failed sync must restore the modified flag")
  assert_true(pr.title == "New title", "failed sync must not update the PR cache")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
diff_review.reset_git_backend()
gh.reset_backend()
github_gh.reset_backend()
issue_index._reset_for_test()
repo_cache.set_data_dir_for_test(nil)
diff_review._datetime.now_override = nil
vim.fn.delete(repo_cache_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
