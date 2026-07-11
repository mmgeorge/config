vim.loader.enable(false)

local render_markdown_ns = vim.api.nvim_create_namespace("render-markdown.nvim")
local render_markdown_calls = {}
local render_markdown_parser_results = {}
local render_markdown_outside_rows_during_render = {}
local language_register_calls = {}
local original_language_register = vim.treesitter.language.register
vim.treesitter.language.register = function(language, filetype)
  language_register_calls[#language_register_calls + 1] = {
    language = language,
    filetype = filetype,
  }
end
package.loaded["render-markdown.core.ui"] = { ns = render_markdown_ns }
package.loaded["render-markdown"] = {
  render = function(ctx)
    render_markdown_calls[#render_markdown_calls + 1] = ctx
    local parser_ok, parser_or_err = pcall(vim.treesitter.get_parser, ctx.buf)
    render_markdown_parser_results[#render_markdown_parser_results + 1] = {
      ok = parser_ok,
      value = parser_or_err,
      filetype = vim.bo[ctx.buf].filetype,
      included_regions = parser_ok
        and type(parser_or_err) == "table"
        and type(parser_or_err.included_regions) == "function"
        and parser_or_err:included_regions()
        or nil,
    }
    vim.api.nvim_buf_clear_namespace(ctx.buf, render_markdown_ns, 0, -1)
    pcall(vim.api.nvim_buf_set_extmark, ctx.buf, render_markdown_ns, 0, 0, {
      virt_text = { { "outside", "Comment" } },
      virt_text_pos = "eol",
    })
    local saw_outside_row = false
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(ctx.buf, render_markdown_ns, 0, -1, {})) do
      if mark[2] == 0 then saw_outside_row = true end
    end
    render_markdown_outside_rows_during_render[#render_markdown_outside_rows_during_render + 1] = saw_outside_row
    local lines = vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false)
    for row, line in ipairs(lines) do
      if line == "Description:" then
        local body_row = row + 1
        local body_line = lines[body_row] or ""
        pcall(vim.api.nvim_buf_set_extmark, ctx.buf, render_markdown_ns, body_row - 1, 0, {
          hl_group = "RenderMarkdownParagraph",
          end_col = #body_line,
        })
        break
      end
    end
    for row, line in ipairs(lines) do
      if line == "```ts" or line == "interface CommentBlock {" or line == "```" then
        pcall(vim.api.nvim_buf_set_extmark, ctx.buf, render_markdown_ns, row - 1, 0, {
          hl_group = "RenderMarkdownCode",
          end_col = #line,
        })
      end
    end
    if ctx.config and ctx.config.on and ctx.config.on.render then ctx.config.on.render({ buf = ctx.buf, win = ctx.win }) end
  end,
}

local diff_review = require("diff_review")
local datetime = require("diff_review.integrations.datetime")
local gh = require("diff_review.integrations.gh")
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
standalone_comment_calls.updates = {}
standalone_comment_calls.deletes = {}
standalone_comment_calls.replies = {}
standalone_comment_calls.reply_should_fail = false
---@type { command: string[], input: string?, payload: table }[]
local issue_comment_calls = {}
---@type { command: string[], input: string?, payload: table }[]
local issue_comment_update_calls = {}
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
      if draft_status_calls.should_fail
        and (query:find("PullRequestReadyForReview", 1, true)
          or query:find("PullRequestToDraft", 1, true)
          or query:find("closePullRequest", 1, true)
          or query:find("reopenPullRequest", 1, true)) then
        cb({ code = 1, stdout = "", stderr = "mock PR state failure", output = "mock PR state failure" })
        return
      end
      if query:find("updatePullRequestReviewComment", 1, true) then
        local mutation_input = graphql_payload.variables and graphql_payload.variables.input or {}
        standalone_comment_calls.updates[#standalone_comment_calls.updates + 1] = {
          command = command,
          input = input,
          payload = mutation_input,
        }
        local stdout = vim.json.encode({
          data = {
            updatePullRequestReviewComment = {
              pullRequestReviewComment = {
                id = mutation_input.pullRequestReviewCommentId,
                databaseId = 3409923137,
                body = mutation_input.body,
                path = "src/a.txt",
                line = 5,
                createdAt = "2026-06-14T17:14:07Z",
                updatedAt = "2026-06-14T19:00:00Z",
                author = { login = "me" },
              },
            },
          },
        })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      if query:find("deletePullRequestReviewComment", 1, true) then
        local mutation_input = graphql_payload.variables and graphql_payload.variables.input or {}
        standalone_comment_calls.deletes[#standalone_comment_calls.deletes + 1] = {
          command = command,
          input = input,
          payload = mutation_input,
        }
        local stdout = vim.json.encode({ data = { deletePullRequestReviewComment = { clientMutationId = vim.NIL } } })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      if query:find("markPullRequestReadyForReview", 1, true) then
        draft_status_calls[#draft_status_calls + 1] = { command = command, input = input, payload = graphql_payload }
        local stdout = vim.json.encode({
          data = {
            markPullRequestReadyForReview = {
              pullRequest = { id = "PR_kwTEST7", state = "OPEN", isDraft = false },
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
              pullRequest = { id = "PR_kwTEST7", state = "OPEN", isDraft = true },
            },
          },
        })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      if query:find("closePullRequest", 1, true) then
        draft_status_calls[#draft_status_calls + 1] = { command = command, input = input, payload = graphql_payload }
        local stdout = vim.json.encode({
          data = {
            closePullRequest = {
              pullRequest = { id = "PR_kwTEST7", state = "CLOSED", isDraft = false },
            },
          },
        })
        cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
        return
      end
      if query:find("reopenPullRequest", 1, true) then
        draft_status_calls[#draft_status_calls + 1] = { command = command, input = input, payload = graphql_payload }
        local stdout = vim.json.encode({
          data = {
            reopenPullRequest = {
              pullRequest = { id = "PR_kwTEST7", state = "OPEN", isDraft = draft_status_calls.reopen_as_draft == true },
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
                    body = "Lorem Ipsum is the ubiquitous placeholder\r\n```ts\r\ninterface CommentBlock {\r\n  value: number;\r\n}\r\n```\r\nSecond full line for expansion",
                    viewerDidAuthor = true,
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
                          viewerDidAuthor = true,
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
    if key == "gh api --method POST /repos/owner/repo/pulls/7/comments/3409923139/replies --input -" then
      local payload = vim.json.decode(input or "{}")
      standalone_comment_calls.replies[#standalone_comment_calls.replies + 1] = { command = command, input = input, payload = payload }
      if standalone_comment_calls.reply_should_fail then
        cb({ code = 1, stdout = "", stderr = "mock reply failure", output = "mock reply failure" })
        return
      end
      local stdout = vim.json.encode({
        id = 3409923140,
        node_id = "PRRC_REPLY_2",
        in_reply_to_id = 3409923139,
        body = payload.body,
        created_at = "2026-06-15T03:20:00Z",
        updated_at = "2026-06-15T03:20:00Z",
        html_url = "https://github.com/owner/repo/pull/7#discussion_r3409923140",
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
    if key == "gh api --method PATCH /repos/owner/repo/issues/comments/4702465967 --input -" then
      local payload = vim.json.decode(input or "{}")
      issue_comment_update_calls[#issue_comment_update_calls + 1] = { command = command, input = input, payload = payload }
      local stdout = vim.json.encode({
        id = 4702465967,
        node_id = "IC_2",
        body = payload.body,
        html_url = "https://github.com/owner/repo/pull/7#issuecomment-4702465967",
        created_at = "2026-06-14T22:00:00Z",
        updated_at = "2026-06-14T23:00:00Z",
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
  local epoch = datetime.parse(value)
  assert_true(type(epoch) == "number", "test datetime did not parse: " .. tostring(value))
  datetime.now_override = function() return epoch end
end

--- Concatenated text of every compact comment box with its first cursorable row.
local function box_text_lines(buf)
  local out = {}
  local state = require("diff_review.views.pr.review").state(buf)
  local buffer_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local current
  for row = 1, #buffer_lines do
    local entry = state and state.entries and state.entries[row] or nil
    if entry and entry.kind == "comment_box" then
      if entry.comment_box_boundary == "header" or not current then
        current = { lines = {}, anchor_row = row, descriptor = entry.comment_box }
      end
      current.lines[#current.lines + 1] = buffer_lines[row] or ""
      if entry.comment_box_boundary == "footer" then
        current.text = table.concat(current.lines, "\n")
        current.lines = nil
        out[#out + 1] = current
        current = nil
      end
    end
  end
  return out
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  for _, box in ipairs(box_text_lines(buf)) do
    if box.text:find(needle, 1, true) then return true end
  end
  return false
end

local function buffer_has_carriage_return(buf)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find("\r", 1, true) then return true end
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

local function find_box_at_or_after(buf, needle, start_row)
  for _, box in ipairs(box_text_lines(buf)) do
    if box.anchor_row >= start_row and box.text:find(needle, 1, true) then return box end
  end
  error("missing comment box at or after " .. tostring(start_row) .. ": " .. needle, 2)
end

local function find_row_after(buf, needle, after_row)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index = after_row + 1, #lines do
    if lines[index]:find(needle, 1, true) then return index end
  end
  error("missing row after " .. tostring(after_row) .. ": " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function row_is_folded(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  return vim.fn.foldclosed(row) ~= -1
end

local function fold_text_at(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  local text = vim.fn.foldtextresult(row)
  if text ~= "" then return text end
  return vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
end

local function find_status_entry_row(buf, predicate, label)
  local session = require("diff_review.session")
  local state = session.states and session.states[buf] or nil
  assert_true(state ~= nil, "missing status state for " .. tostring(label))
  for row = 1, vim.api.nvim_buf_line_count(buf) do
    local entry = state.entries and state.entries[row] or nil
    if entry and predicate(entry, row) then return row end
  end
  error("missing status entry: " .. tostring(label), 2)
end

local function line_has_highlight(buf, row, hl_group, start_col, end_col)
  local ui = require("diff_review.infra.ui")
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ui.status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })) do
    local details = mark[4] or {}
    if details.hl_group == hl_group
      and (start_col == nil or mark[3] == start_col)
      and (end_col == nil or details.end_col == end_col) then
      return true
    end
  end
  return false
end

local function line_has_substring_highlight(buf, row, text, hl_group)
  local line = vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local start_index = line:find(text, 1, true)
  if not start_index then return false end
  return line_has_highlight(buf, row, hl_group, start_index - 1, start_index - 1 + #text)
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

local pr_edit_ns = vim.api.nvim_create_namespace("diff_review.views.pr.pr_edit")

local function render_markdown_mark_rows(buf)
  local rows = {}
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, render_markdown_ns, 0, -1, {})) do
    rows[#rows + 1] = mark[2] + 1
  end
  table.sort(rows)
  return rows
end

local function row_list_contains(rows, row)
  for _, value in ipairs(rows) do
    if value == row then return true end
  end
  return false
end

local function code_block_highlighted(buf, row)
  local ns = require("diff_review.views.pr.pr_edit").code_block_ns
  return #vim.api.nvim_buf_get_extmarks(buf, ns, { row - 1, 0 }, { row - 1, -1 }, {}) > 0
end

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
  local reviewer_source = require("diff_review.views.pr.reviewer_source").new({})
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
  state = "OPEN",
}

local function run()
  local review = require("diff_review.views.pr.review")
  local ui = require("diff_review.infra.ui")
  local session = require("diff_review.session")
  vim.notify = capture_notify
  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  github_gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  local date_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewStatusDate", link = true })
  assert_true(date_hl.link == "Comment", "DiffReviewStatusDate should link to Comment")
  repo_cache.set_data_dir_for_test(repo_cache_dir)
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  set_datetime_now("2026-06-15T12:00:00Z")
  assert_true(datetime.relative("2026-06-15T11:55:00Z") == "5 minutes ago", "minute date label failed")
  assert_true(datetime.relative("2026-06-15T10:00:00Z") == "2 hours ago", "hour date label failed")
  assert_true(datetime.relative("2026-06-14T12:00:00Z") == "Yesterday", "yesterday date label failed")
  assert_true(datetime.relative("2026-06-14T12:00:00Z", { yesterday = false }) == "1 day ago", "one-day date label failed")
  assert_true(datetime.relative("2026-06-12T12:00:00Z") == "3 days ago", "day date label failed")
  assert_true(datetime.relative("2026-06-07T12:00:00Z") == "Last week", "last week date label failed")
  assert_true(datetime.relative("2026-05-01T12:00:00Z") == "Last month", "last month date label failed")
  assert_true(datetime.relative("2026-04-01T12:00:00Z") == "April 1, 2026", "absolute date label failed")
  assert_true(
    datetime.action_phrase("mmgeorge", "commented", "2026-06-15T11:55:00Z") == "mmgeorge commented 5 minutes ago",
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

  local draft_codeowner_text = require("diff_review.views.pr.pr_overview").pending_review_text({
    isDraft = true,
    requestedReviewers = {
      { login = "codeowners", is_code_owner = true },
      { login = "alice-dev" },
    },
  }, nil)
  assert_true(
    draft_codeowner_text == ui.pending_review_icon .. " " .. ui.codeowner_review_icon .. "@codeowners @alice-dev",
    "draft codeowner reviewers should render with a warning icon: " .. tostring(draft_codeowner_text)
  )
  local ready_codeowner_text = require("diff_review.views.pr.pr_overview").pending_review_text({
    isDraft = false,
    requestedReviewers = {
      { login = "codeowners", is_code_owner = true },
    },
  }, nil)
  assert_true(
    ready_codeowner_text == ui.pending_review_icon .. " @codeowners",
    "ready codeowner reviewers should render without a draft warning icon: " .. tostring(ready_codeowner_text)
  )

  local buf = diff_review.open_pr(pr, { cwd = "D:/diffreview-pr-edit-root" })
  assert_true(buf ~= nil, "open_pr did not return a buffer")
  wait_for(function() return buffer_contains(buf, "Title:  Old title") end, "PR view did not render the title")
  assert_true(
    buffer_contains(buf, "Head:   abc1234 1 day ago feature chore: head commit"),
    "PR head row did not render the head commit date"
  )
  assert_true(
    line_has_substring_highlight(buf, find_row(buf, "Head:   abc1234"), "1 day ago", "DiffReviewStatusDate"),
    "PR head date did not use date highlight"
  )
  assert_true(vim.wo[0].wrap, "PR overview should reuse GitStatus soft-wrap formatting")
  assert_true(vim.wo[0].linebreak, "PR overview should enable linebreak for word-aware wrap")
  assert_true(not vim.wo[0].breakindent, "PR overview should keep breakindent off so wrapped diff rows have no unpainted background notch")
  assert_true(vim.wo[0].conceallevel == 0, "PR overview should not conceal code rows")
  assert_true(vim.wo[0].fillchars:find("fold: ", 1, true) ~= nil, "PR overview should not draw fold filler dots")
  assert_true(vim.wo[0].winhighlight:find("Folded:Normal", 1, true) ~= nil, "PR overview should not recolor folded rows")
  assert_true(buffer_contains(buf, "Line one"), "PR body did not render")
  wait_for(function() return buffer_contains(buf, "This is a regular comment") end, "PR conversation comment did not render")
  wait_for(function() return buffer_contains(buf, "Activity: 5 hours ago") end, "PR activity row did not use the newest comment/review activity")
  assert_true(
    line_has_substring_highlight(buf, find_row(buf, "Activity: 5 hours ago"), "5 hours ago", "DiffReviewStatusDate"),
    "PR activity date did not use date highlight"
  )
  assert_true(
    find_row(buf, "Status: DRAFT") < find_row(buf, "Activity: 5 hours ago"),
    "PR activity row should render after Status"
  )
  assert_true(buffer_contains(buf, "Comments (2):"), "PR comments heading did not use section heading format")
  local first_regular_comment_row = find_status_entry_row(buf, function(entry)
    return entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and entry.pr_comment.body == "This is a regular comment"
  end, "first regular PR comment")
  local first_regular_comment_line = fold_text_at(buf, first_regular_comment_row)
  assert_true(
    first_regular_comment_line:find("me 10 hours ago  This is a regular comment", 1, true) ~= nil,
    "regular PR comment row did not align metadata without action text: " .. first_regular_comment_line
  )
  assert_true(not first_regular_comment_line:find("|", 1, true), "regular PR comment row kept a pipe: " .. first_regular_comment_line)
  assert_true(not first_regular_comment_line:find("commented", 1, true), "regular PR comment row kept action text: " .. first_regular_comment_line)
  assert_true(
    line_has_substring_highlight(buf, first_regular_comment_row, "10 hours ago", "DiffReviewStatusDate"),
    "regular PR comment date did not use date highlight"
  )
  -- The dark gray comment-box background is scoped to inline diff comments only; the PR
  -- conversation/list rows must keep the plain comment highlight, not the box groups.
  assert_true(
    not line_has_highlight(buf, first_regular_comment_row, "DiffReviewReviewCommentBox")
      and not line_has_highlight(buf, first_regular_comment_row, "DiffReviewReviewCommentBoxHeader"),
    "PR conversation comment list row must not carry the inline comment box highlight"
  )
  -- The shared comment groups (list rows, review summary, standalone PR comments) must carry
  -- no background; only the dedicated *Box groups do. Pins the box bg from leaking back here.
  assert_true(vim.api.nvim_get_hl(0, { name = "DiffReviewReviewComment" }).bg == nil,
    "DiffReviewReviewComment must not have a background (box bg belongs to DiffReviewReviewCommentBox)")
  assert_true(vim.api.nvim_get_hl(0, { name = "DiffReviewReviewCommentHeader" }).bg == nil,
    "DiffReviewReviewCommentHeader must not have a background (box bg belongs to DiffReviewReviewCommentBoxHeader)")
  assert_true(vim.api.nvim_get_hl(0, { name = "DiffReviewReviewCommentBox" }).bg ~= nil,
    "DiffReviewReviewCommentBox must carry the dark gray box background")
  assert_true(
    buffer_contains(buf, "Lorem Ipsum is the ubiquitous placeholder"),
    "long PR conversation comment preview did not render"
  )
  assert_true(not buffer_has_carriage_return(buf), "PR comments rendered raw carriage returns")
  local long_regular_preview_row = find_status_entry_row(buf, function(entry)
    return entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and tostring(entry.pr_comment.body or ""):find("Second full line for expansion", 1, true) ~= nil
  end, "long regular PR comment")
  local long_regular_preview_line = fold_text_at(buf, long_regular_preview_row)
  assert_true(
    long_regular_preview_line:find("me 5 hours ago   Lorem Ipsum is the ubiquitous placeholder", 1, true) ~= nil,
    "long PR comment row did not align the shorter date column: " .. long_regular_preview_line
  )
  assert_true(
    line_has_substring_highlight(buf, long_regular_preview_row, "5 hours ago", "DiffReviewStatusDate"),
    "long PR comment date did not use date highlight"
  )
  assert_true(not buffer_contains(buf, "Second full line for expansion"), "long PR conversation comment body should not render while folded")
  assert_true(not buffer_contains(buf, "```ts"), "long PR conversation comment code block should not render while folded")
  wait_for(function() return buffer_contains(buf, "Reviews (3):") end, "submitted reviews section did not render")
  local has_single_inline_row, single_inline_row = pcall(find_row, buf, "Single inline review shell")
  assert_true(
    not has_single_inline_row or row_is_folded(buf, single_inline_row),
    "single inline-comment review shell should stay hidden unless its diff is expanded"
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
  assert_true(checks_status_line == "Checks:", "PR checks heading did not render above the check rows")
  assert_true(description_row < checks_status_row, "Description did not render before PR checks heading")
  assert_true(description_row < lint_check_row, "Description did not render before PR check rows")
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
    line_has_highlight(buf, checks_status_row, "DiffReviewStatusHeader", 0, #"Checks:"),
    "PR checks heading did not use header highlight"
  )
  local reviews_heading_row = find_row(buf, "Reviews (3):")
  local comments_heading_row = find_row(buf, "Comments (2):")
  local changes_heading_row = find_row(buf, "Changes (1):")
  local initial_recent_commits_row = find_row(buf, "Recent Commits (2):")
  assert_true(
    description_row < checks_status_row
      and checks_status_row < reviews_heading_row
      and reviews_heading_row < comments_heading_row
      and comments_heading_row < changes_heading_row
      and changes_heading_row < initial_recent_commits_row,
    "PR overview sections did not render in requested order"
  )
  move_cursor(buf, checks_status_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return row_is_folded(buf, find_row(buf, "Dummy Lint")) and row_is_folded(buf, find_row(buf, "Dummy Unit Tests"))
  end, "PR checks heading did not fold check rows")
  assert_true(buffer_contains(buf, "Description:"), "folding PR checks hid the Description heading")
  move_cursor(buf, checks_status_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return not row_is_folded(buf, find_row(buf, "Dummy Lint")) end, "PR checks heading did not unfold check rows")
  lint_check_row = find_row(buf, "Dummy Lint")
  checks_status_row = lint_check_row - 1
  description_row = find_row(buf, "Description:")

  move_cursor(buf, description_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return row_is_folded(buf, find_row(buf, "Line one")) and row_is_folded(buf, find_row(buf, "Line two"))
  end, "PR Description heading did not fold the description body")
  local folded_description_row = find_row(buf, "Description:")
  local folded_description_entry = session.states[buf].entries[folded_description_row]
  assert_true(
    folded_description_entry and folded_description_entry.id == "pr-head-section:description",
    "folded Description row did not keep its fold entry: " .. vim.inspect(folded_description_entry)
  )
  move_cursor(buf, folded_description_row)
  captured_notifications = {}
  trigger_buf_mapping(buf, "<Tab>")
  assert_true(
    session.states[buf].folds["pr-head-section:description"] == false,
    "Description fold state did not reopen: " .. vim.inspect(session.states[buf].folds)
  )
  assert_true(
    not saw_notification_containing("Unsynced PR edits"),
    "Description unfold render was blocked as dirty: " .. vim.inspect(captured_notifications)
  )
  wait_for(function()
    return not row_is_folded(buf, find_row(buf, "Line one")) and not row_is_folded(buf, find_row(buf, "Line two"))
  end, "PR Description heading did not unfold the description body")
  description_row = find_row(buf, "Description:")
  wait_for(function()
    local rows = render_markdown_mark_rows(buf)
    return row_list_contains(rows, find_row(buf, "Line one")) and not row_list_contains(rows, 1)
  end, "PR description markdown marks did not return after unfolding")
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
    assert_true(
      line_has_substring_highlight(buf, review_summary_row, "10 hours ago", "DiffReviewStatusDate"),
      "review summary date did not use date highlight: " .. review_summary_line
    )
  end
  assert_true(line_has_highlight(buf, rejected_summary_row, "DiffReviewDeleteRange"), "rejected review summary did not highlight status red")
  assert_true(line_has_highlight(buf, approved_summary_row, "DiffReviewAddRange"), "approved review summary did not highlight status green")
  assert_true(not line_has_highlight(buf, commented_summary_row, "DiffReviewAddRange"), "commented review summary should not be green")
  assert_true(not line_has_highlight(buf, commented_summary_row, "DiffReviewDeleteRange"), "commented review summary should not be red")
  local changes_file_row = find_row_after(buf, "src/a.txt +1 -1", changes_heading_row)
  local recent_commits_row = find_row(buf, "Recent Commits (2):")
  assert_true(recent_commits_row > changes_file_row, "PR recent commits section did not render at the end")
  assert_true(not buffer_contains(buf, "abc1234  1 day ago  chore: head commit"), "PR recent commits should start folded")
  move_cursor(buf, recent_commits_row)
  trigger_buf_mapping(buf, "<Tab>")
  local head_commit_row = find_row_after(buf, "abc1234  1 day ago  chore: head commit", recent_commits_row)
  local base_commit_row = find_row_after(buf, "1111111  2 days ago feat: base commit", head_commit_row)
  assert_true(head_commit_row > recent_commits_row, "PR head commit did not render under Recent Commits")
  assert_true(base_commit_row > head_commit_row, "PR commits did not render newest first")
  assert_true(
    line_has_substring_highlight(buf, head_commit_row, "1 day ago", "DiffReviewStatusDate"),
    "PR recent commit date did not use date highlight"
  )
  assert_true(
    line_has_substring_highlight(buf, base_commit_row, "2 days ago", "DiffReviewStatusDate"),
    "PR base commit date did not use date highlight"
  )
  move_cursor(buf, changes_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return buffer_contains(buf, "NEW LINE") end, "PR changed file did not expand")
  wait_for(function() return buffer_contains(buf, "This is inline comment without review") end, "PR inline code comment did not render")
  wait_for(function() return buffer_contains(buf, "Oh good point! fixed") end, "PR inline reply did not render")
  -- Inline code comments render as compact real rows below their changed diff row. The
  -- comment and reply live in the same box and fold with the hunk.
  local inline_comment_row = find_row_after(buf, "This is inline comment without review", changes_file_row)
  assert_true(inline_comment_row > changes_file_row, "inline comment box did not anchor under the changed file")
  local function inline_boxes_fit(width)
    local state = review.state(buf)
    local rendered_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    for row = 1, #rendered_lines do
      local entry = state and state.entries and state.entries[row] or nil
      if entry and entry.kind == "comment_box" and vim.fn.strdisplaywidth(rendered_lines[row] or "") > width then return false end
    end
    return true
  end
  local original_columns = vim.o.columns
  vim.o.columns = 48
  vim.api.nvim_exec_autocmds("VimResized", {})
  wait_for(function() return inline_boxes_fit(48) end, "PR inline comment boxes did not shrink with the terminal")
  assert_true(buffer_contains(buf, "This is inline comment without review"), "PR resize dropped the inline comment body")
  assert_true(buffer_contains(buf, "Oh good point! fixed"), "PR resize dropped the inline comment reply")
  vim.o.columns = original_columns
  vim.api.nvim_exec_autocmds("VimResized", {})
  wait_for(function() return inline_boxes_fit(original_columns) end, "PR inline comment boxes did not restore after resize")
  local readonly_inline_box = find_box_at_or_after(buf, "Single inline review shell", changes_file_row)
  local editable_count_before_readonly_c = #review.state(buf).review_comments
  move_cursor(buf, readonly_inline_box.anchor_row)
  local focused_comment_before_readonly_c = review.state(buf).focused_comment_id
  local readonly_entry = review.state(buf).entries[readonly_inline_box.anchor_row]
  local readonly_resolved = readonly_entry.comment_box_source
  assert_true(readonly_resolved and readonly_resolved.remote_node_id == "PRRC_2",
    "readonly box row resolved the wrong comment: " .. vim.inspect(readonly_resolved))
  assert_true(not require("diff_review.views.pr.pr_overview").is_standalone_comment(review.state(buf), readonly_resolved),
    "readonly box entered the editable PR-owned store: " .. vim.inspect(review.state(buf).review_comments))
  assert_true(readonly_entry.comment_box.readonly == true, "another author's compact comment became editable")
  assert_true(not vim.bo[buf].modifiable, "another author's compact comment unlocked the buffer")
  trigger_buf_mapping(buf, "C")
  assert_true(find_box_at_or_after(buf, "Single inline review shell", changes_file_row) ~= nil,
    "C promoted another author's inline comment out of its readonly box")
  assert_true(review.state(buf).focused_comment_id == focused_comment_before_readonly_c,
    "C focused another author's inline comment: " .. vim.inspect({
      before = focused_comment_before_readonly_c,
      after = review.state(buf).focused_comment_id,
    }))
  assert_true(
    #review.state(buf).review_comments == editable_count_before_readonly_c,
    "C created a replacement comment for another author's inline box"
  )
  local pr_window_count = #vim.api.nvim_list_wins()
  trigger_buf_mapping(buf, "R")
  local reply_draft = review.state(buf).pr_reply_draft
  assert_true(reply_draft ~= nil, "R did not create an inline reply draft")
  assert_true(vim.api.nvim_get_current_buf() == buf, "R switched away from the PR buffer")
  assert_true(#vim.api.nvim_list_wins() == pr_window_count, "R opened a bespoke reply popup")
  local reply_draft_row = review.comment_body_rows(buf, reply_draft)
  assert_true(reply_draft_row ~= nil, "R did not render an editable reply body below the comment")
  assert_true(vim.api.nvim_win_get_cursor(0)[1] == reply_draft_row, "R did not focus the inline reply body")
  assert_true(vim.bo[buf].modifiable, "inline reply body did not unlock the PR buffer")
  edit_line(buf, reply_draft_row, "Reply created from the PR view")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #standalone_comment_calls.replies == 1 end, "R did not create a PR review reply")
  assert_true(
    standalone_comment_calls.replies[1].payload.body == "Reply created from the PR view",
    "R sent the wrong reply body: " .. vim.inspect(standalone_comment_calls.replies[1].payload)
  )
  wait_for(function() return buffer_contains(buf, "Reply created from the PR view") end, "created reply did not render")
  local replied_box = find_box_at_or_after(buf, "Single inline review shell", changes_file_row)
  assert_true(replied_box.text:find("Reply created from the PR view", 1, true) ~= nil, "created reply left its parent block")
  assert_true(replied_box.text:find("├─", 1, true) ~= nil, "created reply lacks a merged divider")
  assert_true(select(2, replied_box.text:gsub("╭", "")) == 1, "created reply added another top border")
  assert_true(select(2, replied_box.text:gsub("╰", "")) == 1, "created reply added another bottom border")
  assert_true(saw_notification_containing("Reply posted"), "successful reply was not notified")
  standalone_comment_calls.reply_should_fail = true
  captured_notifications = {}
  move_cursor(buf, replied_box.anchor_row)
  trigger_buf_mapping(buf, "R")
  local failed_reply_draft = review.state(buf).pr_reply_draft
  local failed_reply_row = review.comment_body_rows(buf, failed_reply_draft)
  assert_true(failed_reply_row ~= nil, "failed-reply setup did not create an inline editor")
  edit_line(buf, failed_reply_row, "Reply that should fail")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return saw_notification_containing("mock reply failure") end, "failed reply did not notify")
  assert_true(#(readonly_resolved.replies or {}) == 1, "failed reply mutated the posted comment thread")
  assert_true(
    review.state(buf).pr_reply_draft == failed_reply_draft
      and failed_reply_draft.body == "Reply that should fail",
    "failed reply discarded the inline draft"
  )
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(not failed_reply_draft.focused, "leaving the inline reply body did not collapse its editor")
  local failed_reply_box = find_box_at_or_after(buf, "Single inline review shell", changes_file_row)
  assert_true(
    failed_reply_box.text:find("Reply that should fail", 1, true) ~= nil,
    "collapsed reply draft left the merged comment thread"
  )
  move_cursor(buf, failed_reply_box.anchor_row)
  wait_for(function()
    return failed_reply_draft.focused
      and review.reply_draft_body_at_row(buf, vim.api.nvim_win_get_cursor(0)[1]) == failed_reply_draft
  end, "cursor entry did not reopen the inline reply draft")
  trigger_buf_mapping(buf, "J")
  assert_true(review.state(buf).pr_reply_draft == nil, "J did not discard the failed inline reply draft")
  standalone_comment_calls.reply_should_fail = false
  local rejected_review_row = find_row(buf, "foo     10 hours ago")
  local regular_comment_row = find_row(buf, "This is a regular comment")
  local long_regular_comment_row = find_status_entry_row(buf, function(entry)
    return entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and tostring(entry.pr_comment.body or ""):find("Second full line for expansion", 1, true) ~= nil
  end, "long regular PR comment")
  move_cursor(buf, long_regular_comment_row)
  assert_true(
    not buffer_contains(buf, "Second full line for expansion"),
    "cursor entry auto-opened the regular PR conversation comment"
  )
  assert_true(not buffer_contains(buf, "```ts"), "cursor entry auto-opened the regular PR comment code block")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return not row_is_folded(buf, find_row(buf, "```ts"))
      and not row_is_folded(buf, find_row(buf, "interface CommentBlock {"))
      and not row_is_folded(buf, find_row(buf, "Second full line for expansion"))
  end, "open did not expand the regular PR conversation comment")
  assert_true(not buffer_has_carriage_return(buf), "expanded PR comment rendered raw carriage returns")
  local comment_code_fence_row = find_row(buf, "```ts")
  local comment_code_row = find_row(buf, "interface CommentBlock {")
  wait_for(function()
    local mark_rows = render_markdown_mark_rows(buf)
    return row_list_contains(mark_rows, find_row(buf, "Line one"))
      and row_list_contains(mark_rows, comment_code_fence_row)
      and row_list_contains(mark_rows, comment_code_row)
  end, "expanded regular PR comment code block was not kept as a markdown region")
  wait_for(function()
    return code_block_highlighted(buf, find_row(buf, "interface CommentBlock {"))
  end, "expanded regular PR comment code block was not treesitter-highlighted")
  assert_browse_url(
    buf,
    find_row(buf, "Second full line for expansion"),
    "https://github.com/owner/repo/pull/7#issuecomment-4702465967",
    "expanded regular PR comment body"
  )
  move_cursor(buf, rejected_review_row)
  assert_true(
    not row_is_folded(buf, find_row(buf, "Second full line for expansion")),
    "leaving a manually opened PR conversation comment collapsed it like an annotation"
  )
  long_regular_comment_row = find_status_entry_row(buf, function(entry)
    return entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and tostring(entry.pr_comment.body or ""):find("Second full line for expansion", 1, true) ~= nil
  end, "long regular PR comment")
  move_cursor(buf, long_regular_comment_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return row_is_folded(buf, find_row(buf, "Second full line for expansion"))
  end, "regular PR conversation comment did not collapse")
  assert_true(row_is_folded(buf, find_row(buf, "```ts")), "collapsed regular PR comment kept its code block visible")
  long_regular_comment_row = find_status_entry_row(buf, function(entry)
    return entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and tostring(entry.pr_comment.body or ""):find("Second full line for expansion", 1, true) ~= nil
  end, "long regular PR comment")
  move_cursor(buf, long_regular_comment_row)
  wait_for(function()
    return row_is_folded(buf, find_row(buf, "Second full line for expansion"))
  end, "cursor entry reopened the viewer-authored regular PR comment")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return not row_is_folded(buf, find_row(buf, "Second full line for expansion"))
  end, "open did not reopen the viewer-authored regular PR comment")
  local long_regular_comment_body_row = find_row(buf, "Second full line for expansion")
  move_cursor(buf, long_regular_comment_body_row)
  assert_true(vim.bo[buf].modifiable, "viewer-authored regular PR comment body must be editable")
  edit_line(buf, long_regular_comment_body_row, "Edited existing regular comment")
  assert_true(vim.bo[buf].modified, "editing an existing regular PR comment must mark the PR buffer modified")
  local previous_issue_comment_update_count = #issue_comment_update_calls
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function()
    return #issue_comment_update_calls == previous_issue_comment_update_count + 1
  end, "viewer-authored regular PR comment was not updated")
  local issue_update_payload = issue_comment_update_calls[#issue_comment_update_calls].payload
  assert_true(
    issue_update_payload.body == "Lorem Ipsum is the ubiquitous placeholder\n```ts\ninterface CommentBlock {\n  value: number;\n}\n```\nEdited existing regular comment",
    "wrong regular PR comment update body: " .. vim.inspect(issue_update_payload)
  )
  wait_for(function() return saw_notification_containing("PR comment synced") end, "successful regular PR comment update was not notified")
  assert_true(buffer_contains(buf, "Edited existing regular comment"), "updated regular PR comment disappeared from the PR overview")
  regular_comment_row = find_status_entry_row(buf, function(entry)
    return entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and entry.pr_comment.body == "This is a regular comment"
  end, "first regular PR comment")
  changes_file_row = find_row_after(buf, "src/a.txt +1 -1", find_row(buf, "Changes (1):"))
  inline_comment_row = find_row_after(buf, "This is inline comment without review", changes_file_row)
  rejected_review_row = find_row(buf, "foo     10 hours ago")
  opened_urls = {}
  assert_browse_url(buf, lint_check_row, "https://github.com/owner/repo/actions/runs/123/job/456", "PR check row")
  assert_browse_url(buf, regular_comment_row, "https://github.com/owner/repo/pull/7#issuecomment-4702465966", "regular PR comment")
  assert_browse_url(buf, rejected_review_row, "https://github.com/owner/repo/pull/7#pullrequestreview-4493241278", "review summary")
  -- The inline comment + its reply share one box on the anchor diff row; b browses the comment.
  assert_browse_url(buf, inline_comment_row, "https://github.com/owner/repo/pull/7#discussion_r3409923137", "inline code comment box")

  move_cursor(buf, find_row(buf, "Comments (2):"))
  trigger_buf_mapping(buf, "C")
  wait_for(function() return buffer_contains(buf, "Comments (3):") end, "regular PR comment draft did not update the comments count")
  local regular_draft_header_row = find_row_after(buf, require("diff_review.views.pr.review").comment_icon .. " you", regular_comment_row)
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

  changes_file_row = find_row_after(buf, "src/a.txt +1 -1", find_row(buf, "Changes (1):"))
  changed_code_row = find_row_after(buf, "old line", changes_file_row)
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
  assert_true(standalone_payload.side == "LEFT", "standalone comment side was wrong")
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
  local expanded_comment_box = find_box_at_or_after(buf, "This is inline comment without review", expanded_code_row)
  assert_true(expanded_comment_box.anchor_row < changes_heading_row, "expanded review comment box rendered in the wrong section")
  assert_true(
    expanded_comment_box.text:find("Oh good point! fixed", 1, true) ~= nil,
    "expanded review reply did not reuse the parent comment box: " .. vim.inspect(expanded_comment_box)
  )
  assert_true(
    expanded_comment_box.text:find("foo replied 10 hours ago", 1, true) ~= nil,
    "expanded review reply header did not render author and timestamp"
  )
  assert_true(
    expanded_comment_box.text:find("├─", 1, true) ~= nil,
    "expanded review reply did not render as a merged block divider"
  )
  assert_true(
    expanded_review_lines:find("This is inline comment without review", 1, true) ~= nil,
    "expanded review compact comment was not emitted as real buffer rows"
  )
  assert_true(
    review.state(buf).entries[find_row_after(buf, "This is inline comment without review", expanded_code_row)].kind == "comment_box",
    "expanded review comment did not use the shared compact-row renderer"
  )

  move_cursor(buf, expanded_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return row_is_folded(buf, find_row_after(buf, "NEW LINE", expanded_file_row))
      and row_is_folded(buf, find_row_after(buf, "Oh good point! fixed", expanded_file_row))
  end, "collapsed review file did not hide its materialized diff context")
  assert_true(
    row_is_folded(buf, find_row_after(buf, "src/a.txt +1 -1", rejected_review_row)),
    "collapsed review file did not close its native fold range"
  )

  rejected_review_row = find_row(buf, "foo     10 hours ago")
  expanded_file_row = find_row_after(buf, "src/a.txt +1 -1", rejected_review_row)
  move_cursor(buf, expanded_file_row)
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function()
    return not row_is_folded(buf, expanded_file_row)
      and not row_is_folded(buf, find_row_after(buf, "NEW LINE", expanded_file_row))
  end, "expanded review file did not reopen after folding")
  changes_heading_row = find_row_after(buf, "Changes", rejected_review_row)
  expanded_code_row = find_row_after(buf, "NEW LINE", expanded_file_row)
  expanded_comment_box = find_box_at_or_after(buf, "This is inline comment without review", expanded_code_row)
  assert_true(expanded_comment_box.anchor_row < changes_heading_row, "expanded review comment box rendered in the wrong section after reopening")
  local previous_issue_comment_count = #issue_comment_calls
  local previous_standalone_comment_count = #standalone_comment_calls
  local review_context_comment_row = find_row_after(buf, "gamma", expanded_file_row)
  move_cursor(buf, review_context_comment_row)
  trigger_buf_mapping(buf, "C")
  wait_for(function() return buffer_contains(buf, "Comments (4):") end, "review-context C did not create a regular PR comment")
  local context_regular_header_row = find_row_after(buf, require("diff_review.views.pr.review").comment_icon .. " you", find_row(buf, "Regular from PR overview"))
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

  rejected_review_row = find_row(buf, "foo     10 hours ago")
  changes_heading_row = find_row_after(buf, "Changes", rejected_review_row)
  expanded_comment_box = find_box_at_or_after(buf, "This is inline comment without review", rejected_review_row)
  assert_true(expanded_comment_box.anchor_row < changes_heading_row, "review comment box disappeared before edit")
  local editable_comment_count = #review.state(buf).review_comments
  move_cursor(buf, expanded_comment_box.anchor_row)
  wait_for(function()
    local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
    local cursor_line = vim.api.nvim_buf_get_lines(buf, cursor_row - 1, cursor_row, false)[1] or ""
    return cursor_line:find("This is inline comment without review", 1, true) ~= nil
  end, "cursor entry did not focus the review comment's editable body")
  assert_true(review.state(buf).view_kind == "pr", "editing a review comment left the PR surface")
  assert_true(vim.bo[buf].modifiable, "focused review comment body must be editable")
  assert_true(
    #review.state(buf).review_comments == editable_comment_count,
    "cursor entry on a review comment box created a duplicate comment"
  )
  local focused_inline_row = vim.api.nvim_win_get_cursor(0)[1]
  local focused_inline_comment = review.comment_body_at_row(buf, focused_inline_row)
  local focused_reply_mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg("R", "n", false, true)
  end)
  assert_true(type(focused_reply_mapping.callback) == "function", "R fell through to Replace mode from the focused inline comment")
  local focused_start_row, focused_end_row = review.comment_body_rows(buf, focused_inline_comment)
  edit_line(buf, focused_inline_row, "Edited inline comment from Reviews")
  assert_true(
    focused_inline_comment.body == "Edited inline comment from Reviews",
    "inline comment edit captured non-body rows: "
      .. vim.inspect({
        body = focused_inline_comment.body,
        cursor_row = focused_inline_row,
        body_rows_before = { focused_start_row, focused_end_row },
        lines = vim.api.nvim_buf_get_lines(buf, focused_start_row - 1, focused_end_row + 2, false),
      })
  )
  local body_text_after_header_refresh = review.comment_body_text_from_buffer(buf, focused_inline_comment)
  local refreshed_start_row, refreshed_end_row = review.comment_body_rows(buf, focused_inline_comment)
  assert_true(
    body_text_after_header_refresh == "Edited inline comment from Reviews",
    "inline header refresh moved the editable region: "
      .. vim.inspect({
        body_text = body_text_after_header_refresh,
        body_rows = { refreshed_start_row, refreshed_end_row },
        lines = vim.api.nvim_buf_get_lines(buf, refreshed_start_row - 1, refreshed_end_row + 2, false),
      })
  )
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #standalone_comment_calls.updates == 1 end, "edited review comment was not updated")
  assert_true(
    standalone_comment_calls.updates[1].payload.pullRequestReviewCommentId == "PRRC_1",
    "inline comment update targeted the wrong node: " .. vim.inspect(standalone_comment_calls.updates[1].payload)
  )
  assert_true(
    standalone_comment_calls.updates[1].payload.body == "Edited inline comment from Reviews",
    "inline comment update sent the wrong body: " .. vim.inspect(standalone_comment_calls.updates[1].payload)
  )
  wait_for(function() return saw_notification_containing("Inline comment synced") end, "edited review comment sync was not notified")
  assert_true(
    review.state(buf).focused_comment_id == focused_inline_comment.local_id
      and review.comment_body_at_row(buf, vim.api.nvim_win_get_cursor(0)[1]) == focused_inline_comment,
    "saving a PR inline comment collapsed its active editor"
  )

  local changes_row_for_delete = find_row(buf, "Changes (1):")
  local standalone_box = find_box_at_or_after(buf, "Standalone from PR overview", changes_row_for_delete)
  move_cursor(buf, standalone_box.anchor_row)
  local standalone_comment = standalone_box.descriptor.source
  assert_true(
    review.state(buf).focused_comment_id == standalone_comment.local_id,
    "entering the next PR comment did not transfer editor focus"
  )
  assert_true(find_box_at_or_after(buf, "Edited inline comment from Reviews", rejected_review_row) ~= nil,
    "leaving a PR inline comment did not restore its box")
  wait_for(function()
    local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
    local cursor_line = vim.api.nvim_buf_get_lines(buf, cursor_row - 1, cursor_row, false)[1] or ""
    return cursor_line:find("Standalone from PR overview", 1, true) ~= nil
  end, "cursor entry on the standalone box did not focus its editable body")
  assert_true(buffer_contains(buf, "Edited inline comment from Reviews"), "saved review edit disappeared when its box collapsed")
  assert_true(buffer_contains(buf, "Oh good point! fixed"), "saved review edit dropped the existing reply thread")
  move_cursor(buf, review.comment_header_row0(buf, standalone_comment) + 1)
  trigger_buf_mapping(buf, "J")
  assert_true(not buffer_contains(buf, "Standalone from PR overview"), "J did not hide the deleted inline PR comment")
  assert_true(vim.bo[buf].modified, "deleted inline PR comment did not mark the buffer modified")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #standalone_comment_calls.deletes == 1 end, "deleted inline PR comment was not synced")
  assert_true(
    standalone_comment_calls.deletes[1].payload.id == "PRRC_STANDALONE_1",
    "inline comment delete targeted the wrong node: " .. vim.inspect(standalone_comment_calls.deletes[1].payload)
  )
  wait_for(function() return saw_notification_containing("Inline comment deleted") end, "inline comment deletion was not notified")

  local edited_review_box = find_box_at_or_after(buf, "Edited inline comment from Reviews", rejected_review_row)
  move_cursor(buf, edited_review_box.anchor_row)
  move_cursor(buf, review.comment_header_row0(buf, edited_review_box.descriptor.source) + 1)
  trigger_buf_mapping(buf, "J")
  assert_true(not buffer_contains(buf, "Edited inline comment from Reviews"), "J did not hide the submitted-review comment box")
  trigger_buf_mapping(buf, "<C-s>")
  wait_for(function() return #standalone_comment_calls.deletes == 2 end, "submitted-review comment deletion was not synced")
  assert_true(
    standalone_comment_calls.deletes[2].payload.id == "PRRC_1",
    "submitted-review delete targeted the wrong node: " .. vim.inspect(standalone_comment_calls.deletes[2].payload)
  )
  wait_for(function() return not buffer_contains(buf, "Edited inline comment from Reviews") end,
    "deleted submitted-review comment reappeared from the remote snapshot")
  move_cursor(buf, find_row(buf, "URL:"))
  assert_true(vim.bo[buf].buftype == "acwrite", "PR buffer must be acwrite")
  assert_true(not vim.bo[buf].modifiable, "PR buffer must start nomodifiable")

  local title_row = find_row(buf, "Title:  Old title")
  local milestone_row = find_row(buf, "Release:")
  local review_row = find_row(buf, "Review:")
  local status_row = find_row(buf, "Status: DRAFT")
  local body_row = find_row(buf, "Line one")
  wait_for(function() return #render_markdown_calls > 0 end, "PR description did not invoke render-markdown")
  wait_for(function() return #render_markdown_parser_results > 0 end, "PR description did not ask for a markdown parser")
  assert_true(
    render_markdown_parser_results[1].ok,
    "PR description render-markdown could not get a parser: " .. tostring(render_markdown_parser_results[1].value)
  )
  assert_true(
    render_markdown_parser_results[1].filetype == "GitStatus",
    "PR description parser shim changed the buffer filetype: " .. tostring(render_markdown_parser_results[1].filetype)
  )
  local included_regions = render_markdown_parser_results[1].included_regions or {}
  local body_region_seen = false
  local first_row_region_seen = false
  for _, region in ipairs(included_regions) do
    for _, range in ipairs(region) do
      local first_row = range[1]
      local after_row = #range == 6 and range[4] or range[3]
      if first_row and after_row and first_row <= body_row - 1 and after_row > body_row - 1 then body_region_seen = true end
      if first_row and after_row and first_row <= 0 and after_row > 0 then first_row_region_seen = true end
    end
  end
  assert_true(body_region_seen, "PR markdown parser regions did not include the description body")
  assert_true(not first_row_region_seen, "PR markdown parser regions included non-markdown header rows")
  assert_true(
    render_markdown_outside_rows_during_render[1] == false,
    "PR markdown placed marks outside the registered markdown ranges during render"
  )
  for _, call in ipairs(language_register_calls) do
    assert_true(call.filetype ~= "GitStatus", "PR description must not register GitStatus as markdown")
  end
  local markdown_rows = render_markdown_mark_rows(buf)
  assert_true(
    row_list_contains(markdown_rows, body_row) and not row_list_contains(markdown_rows, 1),
    "PR markdown marks should include the description body and prune outside marks: " .. vim.inspect(markdown_rows)
  )
  local markdown_config = render_markdown_calls[#render_markdown_calls].config or {}
  assert_true(
    not (markdown_config.anti_conceal and markdown_config.anti_conceal.enabled == false),
    "PR description markdown render should leave render-markdown anti-conceal enabled"
  )
  assert_true(milestone_row < review_row, "Release row should render above Review row")
  assert_true(review_row < status_row, "Status row should render below Review row")
  assert_true(
    line_has_highlight(buf, review_row, "DiffReviewStatusLabel", 0, #"Review: "),
    "PR review request row did not use label highlight"
  )
  assert_true(
    line_has_highlight(buf, milestone_row, "DiffReviewStatusLabel", 0, #"Release: "),
    "PR release row did not use label highlight"
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
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on the release row")
  assert_cursor_clamped_to_line(buf, milestone_row, "PR release")
  move_cursor(buf, status_row)
  assert_true(not vim.bo[buf].modifiable, "PR status row must stay locked")
  move_cursor(buf, find_row(buf, "Description:"))
  assert_true(not vim.bo[buf].modifiable, "the Description: label itself must stay locked")
  move_cursor(buf, body_row)
  assert_true(vim.bo[buf].modifiable, "buffer must unlock on description rows")
  markdown_rows = render_markdown_mark_rows(buf)
  assert_true(
    row_list_contains(markdown_rows, body_row) and not row_list_contains(markdown_rows, 1),
    "PR markdown should stay managed by render-markdown while editing the body: " .. vim.inspect(markdown_rows)
  )
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
  wait_for(function()
    local rows = render_markdown_mark_rows(buf)
    return row_list_contains(rows, body_row) and not row_list_contains(rows, 1)
  end, "PR description markdown did not re-render after leaving the editable body")

  -- ── Status: Enter chooses between the other lifecycle states ───────────────
  move_cursor_to_text(buf, status_row, "DRAFT")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return buffer_contains(vim.api.nvim_get_current_buf(), "[o]  OPEN")
      and buffer_contains(vim.api.nvim_get_current_buf(), "[c]  CLOSED")
      and not buffer_contains(vim.api.nvim_get_current_buf(), "[d]  DRAFT")
  end, "draft state chooser did not render the other two states")
  trigger_current_mapping("q")
  assert_true(#draft_status_calls == 0, "q changed PR state instead of cancelling")

  move_cursor_to_text(buf, status_row, "DRAFT")
  trigger_buf_mapping(buf, "<CR>")
  trigger_current_mapping("o")
  wait_for(function() return #draft_status_calls == 1 end, "mark ready mutation was not sent")
  assert_true(
    draft_status_calls[1].payload.variables.input.pullRequestId == "PR_kwTEST7",
    "mark ready mutation used wrong PR node id: " .. vim.inspect(draft_status_calls[1].payload)
  )
  wait_for(function() return buffer_contains(buf, "Status: OPEN") end, "PR status row did not switch to OPEN")
  assert_true(pr.isDraft == false and pr.state == "OPEN", "PR cache did not switch to open")

  status_row = find_row(buf, "Status: OPEN")
  move_cursor_to_text(buf, status_row, "OPEN")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return buffer_contains(vim.api.nvim_get_current_buf(), "[d]  DRAFT")
      and buffer_contains(vim.api.nvim_get_current_buf(), "[c]  CLOSED")
      and not buffer_contains(vim.api.nvim_get_current_buf(), "[o]  OPEN")
  end, "open state chooser did not render the other two states")
  trigger_current_mapping("c")
  wait_for(function() return #draft_status_calls == 2 end, "close mutation was not sent")
  assert_true(
    draft_status_calls[2].payload.variables.input.pullRequestId == "PR_kwTEST7",
    "close mutation used wrong PR node id: " .. vim.inspect(draft_status_calls[2].payload)
  )
  wait_for(function() return buffer_contains(buf, "Status: CLOSED") end, "PR status row did not switch to CLOSED")
  assert_true(pr.state == "CLOSED", "PR cache did not switch to closed")

  status_row = find_row(buf, "Status: CLOSED")
  move_cursor_to_text(buf, status_row, "CLOSED")
  trigger_buf_mapping(buf, "<CR>")
  wait_for(function()
    return buffer_contains(vim.api.nvim_get_current_buf(), "[d]  DRAFT")
      and buffer_contains(vim.api.nvim_get_current_buf(), "[o]  OPEN")
      and not buffer_contains(vim.api.nvim_get_current_buf(), "[c]  CLOSED")
  end, "closed state chooser did not render the other two states")
  trigger_current_mapping("d")
  wait_for(function() return #draft_status_calls == 4 end, "closed-to-draft transition did not reopen then convert")
  assert_true(
    tostring(draft_status_calls[3].payload.query):find("reopenPullRequest", 1, true) ~= nil
      and tostring(draft_status_calls[4].payload.query):find("convertPullRequestToDraft", 1, true) ~= nil,
    "closed-to-draft transition used the wrong mutation order: " .. vim.inspect(draft_status_calls)
  )
  wait_for(function() return buffer_contains(buf, "Status: DRAFT") end, "PR status row did not switch from CLOSED to DRAFT")
  assert_true(pr.state == "OPEN" and pr.isDraft == true, "PR cache did not switch from closed to draft")

  draft_status_calls.should_fail = true
  captured_notifications = {}
  status_row = find_row(buf, "Status: DRAFT")
  move_cursor_to_text(buf, status_row, "DRAFT")
  trigger_buf_mapping(buf, "<CR>")
  trigger_current_mapping("c")
  wait_for(function() return saw_notification_containing("mock PR state failure") end, "failed PR state mutation was not notified")
  assert_true(buffer_contains(buf, "Status: DRAFT"), "failed PR state mutation changed the rendered status")
  assert_true(pr.state == "OPEN" and pr.isDraft == true, "failed PR state mutation changed the PR cache")
  draft_status_calls.should_fail = false

  move_cursor_to_text(buf, status_row, "DRAFT")
  trigger_buf_mapping(buf, "<CR>")
  trigger_current_mapping("c")
  wait_for(function() return #draft_status_calls == 5 and buffer_contains(buf, "Status: CLOSED") end,
    "draft-to-closed setup transition did not complete")
  draft_status_calls.reopen_as_draft = true
  status_row = find_row(buf, "Status: CLOSED")
  move_cursor_to_text(buf, status_row, "CLOSED")
  trigger_buf_mapping(buf, "<CR>")
  trigger_current_mapping("o")
  wait_for(function() return #draft_status_calls == 7 end, "closed draft did not reopen then mark ready")
  assert_true(
    tostring(draft_status_calls[6].payload.query):find("reopenPullRequest", 1, true) ~= nil
      and tostring(draft_status_calls[7].payload.query):find("markPullRequestReadyForReview", 1, true) ~= nil,
    "closed-draft-to-open transition used the wrong mutation order: " .. vim.inspect(draft_status_calls)
  )
  wait_for(function() return buffer_contains(buf, "Status: OPEN") end, "closed draft did not switch to OPEN")
  assert_true(pr.state == "OPEN" and pr.isDraft == false, "closed draft cache did not switch to open")
  draft_status_calls.reopen_as_draft = false

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
    vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: " .. ui.pending_review_icon .. " @alice-dev @bobtown",
    "Review row did not show pending reviewers after reviewer request"
  )
  assert_true(not vim.bo[buf].modified, "reviewer request sync must clear the modified flag")

  edit_line(buf, review_row, "Review: " .. ui.pending_review_icon .. " @alice-dev")
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
    return vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: " .. ui.pending_review_icon .. " @alice-dev @bobtown"
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
    buffer_contains(buf, "Review: " .. ui.pending_review_icon .. " @alice-dev @bobtown"),
    "post-sync re-render lost pending reviewers"
  )

  -- ── unchanged save is a no-op ───────────────────────────────────────────────
  vim.api.nvim_buf_call(buf, function() vim.cmd("write") end)
  vim.wait(120, function() return false end, 10)
  assert_true(#edit_calls == 1, "no-op save must not call gh")

  -- ── Review: mixed removals and new requests are confirmed together ─────────
  edit_line(buf, review_row, "Review: " .. ui.pending_review_icon .. " @bobtown @mgeorge-esri")
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
    vim.api.nvim_buf_get_lines(buf, review_row - 1, review_row, false)[1] == "Review: " .. ui.pending_review_icon .. " @bobtown @mgeorge-esri",
    "mixed reviewer update did not render the desired pending reviewers"
  )
  assert_true(not vim.bo[buf].modified, "mixed reviewer sync must clear the modified flag")

  -- ── Release: missing milestone is confirmed, created, then assigned ────────
  edit_line(buf, milestone_row, "Release: 5.1")
  assert_true(vim.bo[buf].modified, "milestone edit must mark the PR buffer modified")
  rows = marker_rows(buf)
  assert_true(#rows == 1 and rows[1] == milestone_row, "milestone edit did not mark the Release row dirty")
  captured_notifications = {}
  trigger_buf_mapping(buf, "<C-s>")
  assert_true(#marker_rows(buf) == 0, "milestone save must clear markers before confirmation")
  wait_for(function()
    return milestone_list_calls == 1
      and buffer_contains(vim.api.nvim_get_current_buf(), "Release not found:")
      and buffer_contains(vim.api.nvim_get_current_buf(), "5.1")
      and buffer_contains(vim.api.nvim_get_current_buf(), "Create it now?")
  end, "missing milestone confirmation did not render")
  trigger_current_mapping("y")
  wait_for(function()
    return #milestone_create_calls == 1 and #milestone_set_calls == 1
  end, "missing milestone was not created and assigned")
  assert_true(milestone_create_calls[1].payload.title == "5.1", "wrong milestone create payload: " .. vim.inspect(milestone_create_calls[1].payload))
  assert_true(milestone_set_calls[1].payload.milestone == 501, "wrong milestone set payload: " .. vim.inspect(milestone_set_calls[1].payload))
  wait_for(function() return saw_notification_containing("Release updated: " .. ui.milestone_icon .. " 5.1") end, "successful release update was not notified")
  assert_true(
    vim.api.nvim_buf_get_lines(buf, milestone_row - 1, milestone_row, false)[1] == "Release: " .. ui.milestone_icon .. " 5.1",
    "Release row did not show the assigned milestone"
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
vim.treesitter.language.register = original_language_register
diff_review.reset_git_backend()
gh.reset_backend()
github_gh.reset_backend()
issue_index._reset_for_test()
repo_cache.set_data_dir_for_test(nil)
datetime.now_override = nil
vim.fn.delete(repo_cache_dir, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
