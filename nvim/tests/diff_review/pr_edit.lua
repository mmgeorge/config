vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
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
---@type string[]
local user_lookup_calls = {}
local contributor_calls = 0
local edit_should_fail = false
local opened_urls = {}

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = table.concat(command, " ")
  vim.defer_fn(function()
    if key == "gh api graphql --input -" then
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
    if key == "gh api /users/alice-dev" or key == "gh api /users/bobtown" then
      local login = key:match("/users/(.+)$")
      user_lookup_calls[#user_lookup_calls + 1] = login
      local stdout = vim.json.encode({
        login = login,
        name = login == "alice-dev" and "Alice Developer" or "Bob Town",
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

local pr = {
  number = 7,
  title = "Old title",
  body = "Line one\nLine two",
  url = "https://github.com/owner/repo/pull/7",
  repo = "owner/repo",
  headRefName = "feature",
  headRefOid = "abc1234def5678abc1234def5678abc1234def56",
  commits = {},
  files = { { path = "src/a.txt", additions = 1, deletions = 1 } },
  changedFiles = 1,
  additions = 1,
  deletions = 1,
}

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  repo_cache.set_data_dir_for_test(repo_cache_dir)

  local buf = diff_review.open_pr(pr, { cwd = "D:/diffreview-pr-edit-root" })
  assert_true(buf ~= nil, "open_pr did not return a buffer")
  wait_for(function() return buffer_contains(buf, "Title:  Old title") end, "PR view did not render the title")
  assert_true(buffer_contains(buf, "Line one"), "PR body did not render")
  wait_for(function() return buffer_contains(buf, "This is a regular comment") end, "PR conversation comment did not render")
  assert_true(buffer_contains(buf, "Comments (1):"), "PR comments heading did not use section heading format")
  wait_for(function() return buffer_contains(buf, "Reviews (2):") end, "submitted reviews section did not render")
  assert_true(
    not buffer_contains(buf, "COMMENTED by me | 2026-06-14 17:18"),
    "single inline-comment review shell should not render in Reviews"
  )
  assert_true(buffer_contains(buf, "Changes (1):"), "PR changes heading did not use section heading format")
  assert_true(
    line_has_highlight(buf, find_row(buf, "Description:"), "DiffReviewStatusHeader", 0, #"Description:"),
    "PR description heading did not use header highlight"
  )
  wait_for(function()
    return buffer_contains(buf, "REJECTED by foo | 2026-06-14 17:15 | This requires a few changes...")
  end, "rejected review summary did not render")
  assert_true(buffer_contains(buf, "APPROVED by mgeorge | 2026-06-14 17:16 | LGTM!"), "approved review summary did not render")
  local changes_file_row = find_row(buf, "src/a.txt +1 -1")
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
    .. " me | 2026-06-14 17:14 | This is inline comment without review"
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
  local rejected_review_row = find_row(buf, "REJECTED by foo")
  local regular_comment_row = find_row(buf, "This is a regular comment")
  opened_urls = {}
  assert_browse_url(buf, regular_comment_row, "https://github.com/owner/repo/pull/7#issuecomment-4702465966", "regular PR comment")
  assert_browse_url(buf, rejected_review_row, "https://github.com/owner/repo/pull/7#pullrequestreview-4493241278", "review summary")
  assert_browse_url(buf, inline_comment_row, "https://github.com/owner/repo/pull/7#discussion_r3409923137", "inline code comment")
  assert_browse_url(buf, inline_reply_row, "https://github.com/owner/repo/pull/7#discussion_r3409923138", "inline code reply")

  move_cursor(buf, regular_comment_row)
  trigger_buf_mapping(buf, "C")
  wait_for(function() return buffer_contains(buf, "Comments (2):") end, "regular PR comment draft did not update the comments count")
  local regular_draft_header_row = find_row_after(buf, "you |", regular_comment_row)
  local regular_draft_body_row = regular_draft_header_row + 1
  edit_line(buf, regular_draft_body_row, "Regular from PR overview")
  assert_true(vim.bo[buf].modified, "regular PR comment edit must mark the PR buffer modified")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #issue_comment_calls == 1 end, "regular PR comment was not posted")
  local issue_payload = issue_comment_calls[1].payload
  assert_true(issue_payload.body == "Regular from PR overview", "wrong regular PR comment body: " .. vim.inspect(issue_payload))
  wait_for(function() return saw_notification_containing("PR comment synced") end, "successful regular PR comment sync was not notified")
  assert_true(buffer_contains(buf, "Regular from PR overview"), "posted regular PR comment disappeared from the PR overview")
  assert_browse_url(
    buf,
    find_row(buf, "Regular from PR overview"),
    "https://github.com/owner/repo/pull/7#issuecomment-4702465999",
    "fresh regular PR comment"
  )

  changes_file_row = find_row(buf, "src/a.txt +1 -1")
  changed_code_row = find_row(buf, "NEW LINE")
  move_cursor(buf, changed_code_row)
  trigger_buf_mapping(buf, "C")
  local standalone_header_row = find_row_after(buf, "you |", changed_code_row)
  local standalone_body_row = standalone_header_row + 1
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

  rejected_review_row = find_row(buf, "REJECTED by foo")
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
  assert_true(buffer_contains(buf, "foo | 2026-06-14 17:00"), "expanded review reply header did not render author and timestamp")

  move_cursor(buf, expanded_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    local current_review_row = find_row(buf, "REJECTED by foo")
    local current_changes_row = find_row_after(buf, "Changes", current_review_row)
    local review_block = table.concat(vim.api.nvim_buf_get_lines(buf, current_review_row - 1, current_changes_row - 1, false), "\n")
    return review_block:find("src/a.txt +1 -1", 1, true)
      and not review_block:find("NEW LINE", 1, true)
      and not review_block:find("This is inline comment without review", 1, true)
      and not review_block:find("Oh good point! fixed", 1, true)
  end, "expanded review file did not fold its diff context")

  rejected_review_row = find_row(buf, "REJECTED by foo")
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
  wait_for(function() return buffer_contains(buf, "Comments (3):") end, "review-context C did not create a regular PR comment")
  local context_regular_header_row = find_row_after(buf, "you |", find_row(buf, "Regular from PR overview"))
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
  local review_row = find_row(buf, "Review:")
  local body_row = find_row(buf, "Line one")
  assert_true(
    line_has_highlight(buf, review_row, "DiffReviewStatusLabel", 0, #"Review: "),
    "PR review request row did not use label highlight"
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
  move_cursor(buf, find_row(buf, "Description:"))
  assert_true(not vim.bo[buf].modifiable, "the Description: label itself must stay locked")
  move_cursor(buf, body_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on description rows")
  assert_cursor_clamped_to_line(buf, body_row, "PR description")
  move_cursor(buf, regular_comment_row)
  assert_true(not vim.bo[buf].modifiable, "regular PR comment row must not be editable as description text")
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(not vim.bo[buf].modifiable, "buffer must relock after leaving the regions")

  -- ── Review: completes cached repo contributors and sends reviewer requests ──
  wait_for(function()
    return contributor_calls == 1 and #repo_cache.contributors("owner/repo") == 2
  end, "repo contributors were not cached")
  edit_line(buf, review_row, "Review: @")
  move_cursor(buf, review_row)
  vim.api.nvim_win_set_cursor(0, { review_row, #"Review: @" })
  local reviewer_source = require("diff_review.reviewer_source").new({})
  assert_true(reviewer_source:enabled(), "reviewer completion source did not enable on the Review row")
  local completion_result
  reviewer_source:get_completions({}, function(result) completion_result = result end)
  local completion_labels = {}
  for _, item in ipairs(completion_result.items or {}) do
    completion_labels[item.label] = true
  end
  assert_true(completion_labels["@alice-dev"], "reviewer completion did not include @alice-dev")
  assert_true(completion_labels["@bobtown"], "reviewer completion did not include @bobtown")
  assert_true(not completion_labels["@foobar"], "reviewer completion still included placeholder @foobar")

  edit_line(buf, review_row, "Review: @alice-dev @bobtown")
  assert_true(vim.bo[buf].modified, "reviewer request edit must mark the PR buffer modified")
  local rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == review_row, "reviewer request edit did not mark the Review row dirty")
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
  wait_for(function() return saw_notification_containing("Requested review from @alice-dev, @bobtown") end, "successful reviewer request was not notified")
  assert_true(vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: ", "Review row was not cleared after reviewer request")
  assert_true(not vim.bo[buf].modified, "reviewer request sync must clear the modified flag")

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

  -- ── unchanged save is a no-op ───────────────────────────────────────────────
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  vim.wait(120, function() return false end, 10)
  assert_true(#edit_calls == 1, "no-op save must not call gh")

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
repo_cache.set_data_dir_for_test(nil)
vim.fn.delete(repo_cache_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
