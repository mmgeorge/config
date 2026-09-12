vim.loader.enable(false)

local gh = require("github.gh")
local issue_index = require("github.issue_index")
local repo_cache = require("github.repo_cache")

local root = "D:/mock/github"
local cache_root = vim.fs.joinpath(vim.fn.getcwd(), ".tmp-github-integration-test")
local calls = {}
local captured_picker = nil
local opened_pr_numbers = {}
local defer_next_issue_view = false
local deferred_issue_view_callback = nil
local issue_detail_cache = {}
local original_snacks = _G.Snacks
local original_picker_pick = original_snacks and original_snacks.picker and original_snacks.picker.pick
local original_notify = vim.notify
local original_forge = package.loaded["forge"]
local original_pr_overview = package.loaded["forge.views.pr.pr_overview"]
local original_pr_edit = package.loaded["forge.views.pr.pr_edit"]
local render_markdown_ns = vim.api.nvim_create_namespace("render-markdown.nvim")
local render_markdown_calls = {}

package.loaded["render-markdown.core.ui"] = { ns = render_markdown_ns }
package.loaded["render-markdown"] = {
  render = function(ctx)
    render_markdown_calls[#render_markdown_calls + 1] = ctx
    vim.api.nvim_buf_clear_namespace(ctx.buf, render_markdown_ns, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false)
    for row, line in ipairs(lines) do
      if line == "Issue body" then
        vim.api.nvim_buf_set_extmark(ctx.buf, render_markdown_ns, row - 1, 0, {
          virt_text = { { "rendered issue body", "Comment" } },
          virt_text_pos = "eol",
        })
      elseif line == "Author:       alice" then
        vim.api.nvim_buf_set_extmark(ctx.buf, render_markdown_ns, row - 1, 0, {
          virt_text = { { "metadata should be pruned", "Comment" } },
          virt_text_pos = "eol",
        })
      end
    end
    if ctx.config and ctx.config.on and ctx.config.on.render then ctx.config.on.render({ buf = ctx.buf, win = ctx.win }) end
  end,
}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function record(command, input)
  calls[#calls + 1] = {
    command = vim.deepcopy(command),
    key = command_key(command),
    input = input,
  }
end

local function reset()
  calls = {}
  captured_picker = nil
  opened_pr_numbers = {}
  defer_next_issue_view = false
  deferred_issue_view_callback = nil
  issue_detail_cache = {}
  issue_index._clear_detail_memory_for_test()
  render_markdown_calls = {}
  repo_cache.remember_cwd_repo(vim.fn.getcwd(), "org/repo")
end

local function write_issue_snapshot()
  local path = issue_index.snapshot_path("org/repo")
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = "org/repo",
    state = "open",
    issue_count = 1,
    revision = 0,
    issues = {
      {
        repo = "org/repo",
        number = 12,
        title = "Fix command parser",
        state = "OPEN",
        url = "https://github.com/org/repo/issues/12",
        body = "Indexed body\n\nIndexed details.",
        updated_at = "2026-06-02T00:00:00Z",
        labels = { { name = "bug" } },
        comments_count = 1,
      },
    },
  }) }, path)
  assert_true(result == 0, "issue snapshot write failed")
  local loaded
  issue_index.reload_snapshot("org/repo", function(result) loaded = result end, true)
  assert_true(vim.wait(2000, function() return loaded ~= nil end, 5), "issue snapshot preload did not finish")
  assert_true(loaded.ok, loaded.message)
end

local function preview_contains(preview, needle)
  local lines = preview and preview.lines or {}
  return table.concat(lines, "\n"):find(needle, 1, true) ~= nil
end

local function new_preview()
  local preview = { buf = vim.api.nvim_create_buf(false, true) }
  function preview:set_title(title)
    self.title = title
  end
  function preview:set_lines(lines)
    self.lines = lines
    vim.bo[self.buf].modifiable = true
    vim.api.nvim_buf_set_lines(self.buf, 0, -1, false, lines)
    vim.bo[self.buf].modifiable = false
  end
  function preview:highlight() end
  function preview:notify(message)
    self.lines = { tostring(message) }
  end
  return preview
end

local function wait_for(predicate, message)
  local ok = vim.wait(1000, predicate, 10, false)
  assert_true(ok, message)
end

local function buffer_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function find_buffer_line(buf, needle)
  for index, line in ipairs(buffer_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  return nil
end

local function line_has_namespace_highlight(buf, namespace_name, row, hl_group)
  local ns = vim.api.nvim_get_namespaces()[namespace_name]
  if not ns then return false end
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, { details = true })) do
    local details = mark[4] or {}
    if mark[2] == row - 1 and details.hl_group == hl_group then return true end
  end
  return false
end

local function find_call(key)
  for _, call in ipairs(calls) do
    if call.key == key then return call end
  end
  return nil
end

local function find_call_containing(needle)
  for _, call in ipairs(calls) do
    if call.key:find(needle, 1, true) then return call end
  end
  return nil
end

local function count_calls_containing(needle)
  local count = 0
  for _, call in ipairs(calls) do
    if call.key:find(needle, 1, true) then count = count + 1 end
  end
  return count
end

local function cached_issue_detail(body, fetched_at)
  return {
    repo = "org/repo",
    number = 12,
    found = true,
    fetched_at = fetched_at or os.time(),
    item = {
      kind = "issue",
      repo = "org/repo",
      number = 12,
      title = "Fix command parser",
      body = body,
      url = "https://github.com/org/repo/issues/12",
      state = "OPEN",
      author = "alice",
      comments_count = 0,
      created_at = "2026-06-01T00:00:00Z",
      updated_at = "2026-06-10T12:00:00Z",
      labels = { "bug" },
      assignees = { "bob" },
      milestone = "v1.0",
      projects = { "Roadmap" },
      subscription = "Subscribed",
      comments = {},
    },
  }
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

function backend.system_async(command, input, callback, cwd)
  record(command, input)
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

  callback({
    code = code,
    stdout = stdout,
    stderr = code == 0 and "" or "unexpected command: " .. key,
    output = code == 0 and stdout or "unexpected command: " .. key,
  })
end

gh.set_backend(backend)
vim.fn.delete(cache_root, "rf")
repo_cache.set_data_dir_for_test(cache_root)
require("github.repo_users")._set_runner_for_test(function(params, callback)
  local identity = params.request.repository
  local metadata = { repo = identity.owner .. "/" .. identity.name, hostname = identity.hostname,
    fetched_at = os.time(), contributors = { { login = "alice" }, { login = "bob" }, { login = "carol" } }, failure = {} }
  vim.fn.mkdir(params.cache_directory, "p")
  vim.fn.writefile({ vim.json.encode(metadata) }, vim.fs.joinpath(params.cache_directory, "metadata.json"))
  callback(metadata, nil)
end)
issue_index._reset_for_test()
issue_index._set_progress_enabled_for_test(false)
  issue_index._set_sync_runner_for_test(function(_, callback)
    callback({ refreshed = false, fetched = 0, pages = 0 }, nil)
  end)
issue_index._set_storage_runner_for_test(function(params, callback)
  if params.request.operation == "reconcile_snapshot" then
    callback({ ready = true, republished = false, state = { repo = params.repo, revision = 0 } }, nil)
    return
  end
  local request = params.request
  if request.operation == "state" then
    callback({ repo = "org/repo", open_historical_complete = true, last_open_checked_at = os.time() }, nil)
  elseif request.operation == "detail" then
    local number = tostring(request.number)
    callback(issue_detail_cache[number] or { repo = "org/repo", number = request.number, found = false }, nil)
  elseif request.operation == "details" then
    local details = {}
    for _, number in ipairs(request.number) do
      details[#details + 1] = issue_detail_cache[tostring(number)] or { repo = "org/repo", number = number, found = false }
    end
    callback({ repo = "org/repo", details = details }, nil)
  elseif request.operation == "upsert_detail" then
    local detail = request.detail
    issue_detail_cache[tostring(request.number)] = {
      repo = "org/repo", number = request.number, found = true, fetched_at = detail.fetched_at, item = detail.item,
    }
    callback(issue_detail_cache[tostring(request.number)], nil)
  else
    callback({}, nil)
  end
end)
issue_index._set_detail_runner_for_test(function(params, callback)
  local identity = params.request.repository
  local repo = identity.owner .. "/" .. identity.name
  local record = cached_issue_detail("Issue body\n\n## Foobar\n\nDetails.")
  record.repo, record.number, record.item.repo, record.item.number = repo, params.request.number, repo, params.request.number
  record.item.comments_count = 1
  record.item.comments = { { author = "carol", body = "Comment body", created_at = "2026-06-09T00:00:00Z", url = "https://github.com/org/repo/issues/12#issuecomment-1" } }
  local function complete()
    issue_detail_cache[tostring(params.request.number)] = record
    callback(record, nil)
  end
  if defer_next_issue_view then
    defer_next_issue_view = false
    deferred_issue_view_callback = complete
  else complete() end
end)
repo_cache.remember_cwd_repo(vim.fn.getcwd(), "org/repo")
write_issue_snapshot()
vim.notify = function() end
if not _G.Snacks then _G.Snacks = {} end
if not Snacks.picker then Snacks.picker = {} end
Snacks.picker.pick = function(opts)
  captured_picker = opts
  return opts
end
package.loaded["forge"] = {
  open_pr_number = function(number, opts)
    opened_pr_numbers[#opened_pr_numbers + 1] = {
      number = number,
      opts = opts,
    }
  end,
  _milestone_icon = "◆",
  _pr_overview = {
    reviewer_token = function(username)
      return "@" .. tostring(username or ""):gsub("^@", "")
    end,
    reviewer_login = function(reviewer)
      if type(reviewer) == "table" then reviewer = reviewer.login or reviewer.slug or reviewer.name end
      return tostring(reviewer or ""):gsub("^@", "")
    end,
    milestone_text = function(pr)
      local milestone = pr and pr.milestone or nil
      if type(milestone) == "table" then milestone = milestone.title or milestone.name end
      milestone = vim.trim(tostring(milestone or ""))
      return milestone ~= "" and ("◆ " .. milestone) or ""
    end,
    comment_datetime = function(value)
      if value == "2026-06-12T12:00:00Z" then return "3 days ago" end
      if value == "2026-06-10T12:00:00Z" then return "5 days ago" end
      return tostring(value or "")
    end,
  },
  _pr_edit = {
    reviewer_usernames = function(text)
      local usernames = {}
      local seen = {}
      for token in tostring(text or ""):gmatch("@?[%w][%w_-]*") do
        local username = token:gsub("^@", "")
        local key = username:lower()
        if username ~= "" and not seen[key] then
          seen[key] = true
          usernames[#usernames + 1] = username
        end
      end
      return usernames
    end,
  },
}

package.loaded["forge.views.pr.pr_overview"] = package.loaded["forge"]._pr_overview
package.loaded["forge.views.pr.pr_edit"] = package.loaded["forge"]._pr_edit
package.loaded["forge"]._pr_overview = nil
package.loaded["forge"]._pr_edit = nil

local plugin_spec = require("plugins.github")[1]
plugin_spec.init()

local function cleanup()
  if original_snacks then
    _G.Snacks = original_snacks
    if original_snacks.picker then original_snacks.picker.pick = original_picker_pick end
  else
    _G.Snacks = nil
  end
  vim.notify = original_notify
  package.loaded["forge"] = original_forge
  package.loaded["forge.views.pr.pr_overview"] = original_pr_overview
  package.loaded["forge.views.pr.pr_edit"] = original_pr_edit
  gh.reset_backend()
  require("github.repo_users")._set_runner_for_test(nil)
  issue_index._reset_for_test()
  repo_cache.set_data_dir_for_test(nil)
  vim.fn.delete(cache_root, "rf")
end

local function run_tests()
  reset()
  issue_detail_cache["12"] = cached_issue_detail("Cached prefetch body", os.time())
  vim.cmd.ForgeGithubIssue()
  wait_for(function() return captured_picker ~= nil end, "issue picker did not open")
  assert_true(#calls == 0, "issue picker should use the synced index instead of gh search: " .. vim.inspect(calls))
  assert_true(#captured_picker.items == 1, "issue picker missing items")
  assert_true(captured_picker.items[1].item.repo == "org/repo", "issue picker item was not repo scoped")
  assert_true(captured_picker.items[1].item.body == "Indexed body\n\nIndexed details.", "issue picker did not load synced body")
  assert_true(type(captured_picker.preview) == "function", "issue picker did not install an issue preview")
  wait_for(function() return issue_index.cached_detail("org/repo", 12) ~= nil end, "issue picker did not prefetch redb detail")
  local preview = new_preview()
  captured_picker.preview({ item = captured_picker.items[1], preview = preview })
  assert_true(preview_contains(preview, "Cached prefetch body"), "issue preview should paint prefetched redb detail first")
  assert_true(not preview_contains(preview, "Indexed details."), "issue preview should not paint the indexed fallback over prefetched detail")
  assert_true(not preview_contains(preview, "Loading description"), "issue preview should not render the old description placeholder")
  assert_true(count_calls_containing("gh\tissue\tview\t12") == 0, "fresh prefetched detail should not trigger a GitHub fetch")

  reset()
  vim.cmd.ForgeGithubIssue()
  wait_for(function() return captured_picker ~= nil end, "issue picker did not open for stale preview")
  preview = new_preview()
  issue_detail_cache["12"] = cached_issue_detail("Cached stale body", os.time() - 180)
  defer_next_issue_view = true
  captured_picker.preview({ item = captured_picker.items[1], preview = preview })
  assert_true(preview_contains(preview, "Cached stale body"), "issue preview should display stale cached detail immediately")
  assert_true(deferred_issue_view_callback ~= nil, "stale issue preview did not start a deferred detail refresh")
  deferred_issue_view_callback()
  deferred_issue_view_callback = nil
  wait_for(function() return preview_contains(preview, "Issue body") end, "issue preview did not fetch the issue body")
  assert_true(count_calls_containing("gh\tissue\tview\t12") == 0, "issue preview should not restore Lua gh detail reads")
  assert_true(preview_contains(preview, "Title:  Fix command parser"), "issue preview should use issue-view title layout")
  assert_true(preview_contains(preview, "Author:       alice"), "issue preview should use issue-view metadata layout")
  assert_true(preview_contains(preview, "Description:"), "issue preview should render the issue description heading")
  assert_true(preview_contains(preview, "Comments (1):"), "issue preview should render issue comments with the shared component")
  assert_true(not preview_contains(preview, "#12 Fix command parser"), "issue preview should not use the legacy picker-only layout")
  local preview_state_line = find_buffer_line(preview.buf, "State:        Open")
  assert_true(preview_state_line ~= nil, "issue preview should use the shared issue metadata layout")
  assert_true(
    line_has_namespace_highlight(preview.buf, "github.issue_preview.decorations", preview_state_line, "ForgeStatusOpen"),
    "issue preview should use shared issue state highlighting"
  )
  local preview_heading_line = find_buffer_line(preview.buf, "## Foobar")
  assert_true(preview_heading_line ~= nil, "issue preview should render markdown body content")
  assert_true(
    line_has_namespace_highlight(preview.buf, "github.issue_preview.decorations", preview_heading_line, "GithubIssueMarkdownHeading"),
    "issue preview should use shared markdown heading highlighting"
  )
  assert_true(
    captured_picker.items[1].item.body == "Issue body\n\n## Foobar\n\nDetails.",
    "issue preview should update the selected picker item with fetched detail before confirm"
  )
  reset()
  vim.cmd.ForgeGithubPR()
  wait_for(function() return captured_picker ~= nil end, "PR picker did not open")
  assert_true(calls[1].key:find("gh\tsearch\tprs\t--author\t@me", 1, true) ~= nil, "PR author search command was not used")

  reset()
  vim.cmd("ForgeGithubPR 44")
  assert_true(#opened_pr_numbers == 1, "ForgeGithubPR with a number did not open PR by number")
  assert_true(opened_pr_numbers[1].number == "44", "ForgeGithubPR passed the wrong PR number")

  reset()
  vim.cmd.ForgeGithubReview()
  wait_for(function() return captured_picker ~= nil end, "review picker did not open")
  assert_true(
    calls[1].key:find("gh\tsearch\tprs\t--review-requested\t@me", 1, true) ~= nil,
    "review request search command was not used"
  )


end

local ok, err = xpcall(run_tests, debug.traceback)
cleanup()
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("github_integration: ok")
vim.cmd("qa!")
