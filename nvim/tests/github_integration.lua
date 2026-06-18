vim.loader.enable(false)

local gh = require("github.gh")
local issue_index = require("github.issue_index")
local notifications = require("github.notifications")
local repo_cache = require("github.repo_cache")

local root = "D:/mock/github"
local cache_root = vim.fs.joinpath(vim.fn.getcwd(), ".tmp-github-integration-test")
local calls = {}
local opened_urls = {}
local captured_picker = nil
local opened_pr_numbers = {}
local defer_next_issue_view = false
local deferred_issue_view_callback = nil
local issue_detail_cache = {}
local original_snacks = _G.Snacks
local original_picker_pick = original_snacks and original_snacks.picker and original_snacks.picker.pick
local original_notify = vim.notify
local original_diff_review = package.loaded["diff_review"]
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

local function command_json_fields(command)
  for index, value in ipairs(command) do
    if value == "--json" then return command[index + 1] or "" end
  end
  return ""
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
  opened_urls = {}
  captured_picker = nil
  opened_pr_numbers = {}
  defer_next_issue_view = false
  deferred_issue_view_callback = nil
  issue_detail_cache = {}
  render_markdown_calls = {}
  notifications._reset_for_tests()
  repo_cache.remember_cwd_repo(vim.fn.getcwd(), "org/repo")
end

local function write_issue_snapshot()
  local path = issue_index.snapshot_path("org/repo")
  vim.fn.mkdir(vim.fs.dirname(path), "p")
  local result = vim.fn.writefile({ vim.json.encode({
    repo = "org/repo",
    state = "open",
    issue_count = 1,
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
end

local function preview_contains(preview, needle)
  local lines = preview and preview.lines or {}
  return table.concat(lines, "\n"):find(needle, 1, true) ~= nil
end

local function wait_for(predicate, message)
  local ok = vim.wait(1000, predicate, 10, false)
  assert_true(ok, message)
end

local function current_lines()
  return vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false)
end

local function buffer_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function find_line(needle)
  for index, line in ipairs(current_lines()) do
    if line:find(needle, 1, true) then return index end
  end
  return nil
end

local function find_buffer_line(buf, needle)
  for index, line in ipairs(buffer_lines(buf)) do
    if line:find(needle, 1, true) then return index end
  end
  return nil
end

local function buffer_contains(needle)
  return table.concat(current_lines(), "\n"):find(needle, 1, true) ~= nil
end

local function buffer_has_exact_line(needle)
  for _, line in ipairs(current_lines()) do
    if line == needle then return true end
  end
  return false
end

local function plain_winbar()
  return (vim.wo[0].winbar or ""):gsub("%%#.-#", ""):gsub("%%%*", ""):gsub("%%=", " ")
end

local function trigger_buf_mapping(buf, key)
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing current mapping for " .. key)
  mapping.callback()
end

local function move_cursor(buf, row)
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
end

local function edit_line(buf, row, text)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, row - 1, row, false, { text })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
end

local function reviewer_completion_labels(buf, row, text)
  edit_line(buf, row, text)
  move_cursor(buf, row)
  local old_virtualedit = vim.o.virtualedit
  vim.o.virtualedit = "onemore"
  vim.api.nvim_win_set_cursor(0, { row, #text })
  local reviewer_source = require("diff_review.reviewer_source").new({})
  assert_true(reviewer_source:enabled(), "reviewer completion source did not enable on: " .. text)
  local completion_result
  reviewer_source:get_completions({}, function(result) completion_result = result end)
  vim.o.virtualedit = old_virtualedit
  local labels = {}
  for _, item in ipairs(completion_result.items or {}) do
    labels[item.label] = item
  end
  return labels
end

local function assert_contributor_completion(buf, row, text)
  local labels = reviewer_completion_labels(buf, row, text)
  assert_true(labels["@alice"], "GitHub user completion did not include contributor @alice on: " .. text)
  assert_true(labels["@bob"], "GitHub user completion did not include collaborator @bob on: " .. text)
  assert_true(labels["@carol"], "GitHub user completion did not include contributor @carol on: " .. text)
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
  assert_true(vim.wait(1000, function() return completion_result ~= nil end, 10), "GitHub issue completion did not return")
  vim.o.virtualedit = old_virtualedit
  local labels = {}
  for _, item in ipairs(completion_result.items or {}) do
    labels[item.label] = item
  end
  assert_true(labels["#12 Fix command parser"] ~= nil, "GitHub issue completion did not include #12 on: " .. text)
  assert_true(labels["#12 Fix command parser"].textEdit.newText == "#12", "GitHub issue completion should insert only the issue id")
end

local function issue_dirty_marker_count(buf)
  local ns = vim.api.nvim_get_namespaces()["github.issue_view"]
  local count = 0
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, { details = true })) do
    local details = mark[4] or {}
    local virt_text = details.virt_text
    if virt_text and virt_text[1] and virt_text[1][1] == "*" then count = count + 1 end
  end
  return count
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

local function encoded_issue(number)
  local issue_state = number == 34 and "CLOSED" or "OPEN"
  return vim.json.encode({
    number = number,
    title = "Fix command parser",
    body = "Issue body\n\n## Foobar\n\nDetails.",
    url = "https://github.com/org/repo/issues/" .. tostring(number),
    state = issue_state,
    author = { login = "alice" },
    assignees = { { login = "bob" } },
    labels = { { name = "bug" } },
    milestone = { title = "v1.0" },
    viewerSubscription = "SUBSCRIBED",
    projectItems = {
      {
        title = "Roadmap",
        status = { name = "In progress" },
      },
      {
        project = { title = "Backlog" },
      },
    },
    comments = {
      {
        body = "First comment with enough preview text to fill the collapsed row before later body content appears in the buffer\n\nSecond comment line",
        author = { login = "carol" },
        createdAt = "2026-06-01T00:00:00Z",
        updatedAt = "2026-06-12T12:00:00Z",
        url = "https://github.com/org/repo/issues/12#issuecomment-1",
      },
    },
    createdAt = "2026-06-01T00:00:00Z",
    updatedAt = "2026-06-10T12:00:00Z",
  })
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

function backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

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
  elseif key:find("gh\tissue\tview\t12", 1, true) then
    assert_true(
      not command_json_fields(command):find("viewerSubscription", 1, true),
      "issue view requested unsupported viewerSubscription field"
    )
    if defer_next_issue_view then
      defer_next_issue_view = false
      deferred_issue_view_callback = function()
        callback({
          code = 0,
          stdout = encoded_issue(12),
          stderr = "",
          output = encoded_issue(12),
        })
      end
      return
    end
    stdout = encoded_issue(12)
  elseif key:find("gh\tissue\tview\t34", 1, true) then
    assert_true(
      not command_json_fields(command):find("viewerSubscription", 1, true),
      "issue view requested unsupported viewerSubscription field"
    )
    stdout = encoded_issue(34)
  elseif key:find("gh\tissue\tview\t99", 1, true) then
    assert_true(
      not command_json_fields(command):find("viewerSubscription", 1, true),
      "issue view requested unsupported viewerSubscription field"
    )
    stdout = encoded_issue(99)
  elseif key == "gh\tissue\tcreate\t--title\tCreated issue\t--body\t" then
    stdout = "https://github.com/org/repo/issues/99\n"
  elseif key:find("gh\tissue\tedit\t12\t--repo\torg/repo", 1, true) then
    stdout = ""
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
  elseif key == "gh\tapi\t/repos/org/repo/contributors\t--paginate\t--slurp" then
    stdout = vim.json.encode({ { login = "alice" }, { login = "carol" } })
  elseif key == "gh\tapi\t/repos/org/repo/collaborators\t--paginate\t--slurp" then
    stdout = vim.json.encode({ { login = "bob" } })
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
issue_index._reset_for_test()
issue_index._set_progress_enabled_for_test(false)
issue_index._set_runner_for_test(function(command, input, callback)
  local action = command[2]
  if action == "state" then
    local stdout = vim.json.encode({
      repo = "org/repo",
      open_historical_complete = true,
      last_open_checked_at = os.time(),
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
    return
  end
  if action == "detail" then
    local number = command[#command]
    local cached = issue_detail_cache[number]
    local stdout = vim.json.encode(cached or {
      repo = "org/repo",
      number = tonumber(number),
      found = false,
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
    return
  end
  if action == "upsert-detail" then
    local number = command[#command]
    local decoded = vim.json.decode(input or "{}")
    issue_detail_cache[number] = {
      repo = "org/repo",
      number = tonumber(number),
      found = true,
      fetched_at = decoded.fetched_at,
      item = decoded.item,
    }
    local stdout = vim.json.encode(issue_detail_cache[number])
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
    return
  end
  callback({ code = 0, stdout = "{}", stderr = "", output = "{}" })
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
package.loaded["diff_review"] = {
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
  package.loaded["diff_review"] = original_diff_review
  gh.reset_backend()
  issue_index._reset_for_test()
  repo_cache.set_data_dir_for_test(nil)
  vim.fn.delete(cache_root, "rf")
end

local function run_tests()
  reset()
  vim.cmd.GithubIssue()
  wait_for(function() return captured_picker ~= nil end, "issue picker did not open")
  assert_true(#calls == 0, "issue picker should use the synced index instead of gh search: " .. vim.inspect(calls))
  assert_true(#captured_picker.items == 1, "issue picker missing items")
  assert_true(captured_picker.items[1].item.repo == "org/repo", "issue picker item was not repo scoped")
  assert_true(captured_picker.items[1].item.body == "Indexed body\n\nIndexed details.", "issue picker did not load synced body")
  assert_true(type(captured_picker.preview) == "function", "issue picker did not install an issue preview")
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
  issue_detail_cache["12"] = cached_issue_detail("Cached stale body", os.time() - 180)
  defer_next_issue_view = true
  captured_picker.preview({ item = captured_picker.items[1], preview = preview })
  assert_true(preview_contains(preview, "Cached stale body"), "issue preview should display stale cached detail immediately")
  assert_true(deferred_issue_view_callback ~= nil, "stale issue preview did not start a deferred detail refresh")
  deferred_issue_view_callback()
  deferred_issue_view_callback = nil
  wait_for(function() return preview_contains(preview, "Issue body") end, "issue preview did not fetch the issue body")
  assert_true(count_calls_containing("gh\tissue\tview\t12") == 1, "issue preview should fetch detail exactly once")
  assert_true(preview_contains(preview, "Title:  Fix command parser"), "issue preview should use issue-view title layout")
  assert_true(preview_contains(preview, "Author:       alice"), "issue preview should use issue-view metadata layout")
  assert_true(preview_contains(preview, "Description:"), "issue preview should render the issue description heading")
  assert_true(preview_contains(preview, "Comments (1):"), "issue preview should render issue comments with the shared component")
  assert_true(not preview_contains(preview, "#12 Fix command parser"), "issue preview should not use the legacy picker-only layout")
  local preview_state_line = find_buffer_line(preview.buf, "State:        Open")
  assert_true(preview_state_line ~= nil, "issue preview should use the shared issue metadata layout")
  assert_true(
    line_has_namespace_highlight(preview.buf, "github.issue_view.decorations", preview_state_line, "DiffReviewStatusOpen"),
    "issue preview should use shared issue state highlighting"
  )
  local preview_heading_line = find_buffer_line(preview.buf, "## Foobar")
  assert_true(preview_heading_line ~= nil, "issue preview should render markdown body content")
  assert_true(
    line_has_namespace_highlight(preview.buf, "github.issue_view.decorations", preview_heading_line, "GithubIssueMarkdownHeading"),
    "issue preview should use shared markdown heading highlighting"
  )
  assert_true(
    captured_picker.items[1].item.body == "Issue body\n\n## Foobar\n\nDetails.",
    "issue preview should update the selected picker item with fetched detail before confirm"
  )
  captured_picker.confirm({ close = function() end }, captured_picker.items[1])
  wait_for(function() return find_line("Title:  Fix command parser") ~= nil end, "issue view did not render selected issue")
  assert_true(count_calls_containing("gh\tissue\tview\t12") == 1, "issue buffer open should reuse cached issue detail")
  local buf = vim.api.nvim_get_current_buf()
  assert_true(vim.wo[0].wrap, "issue buffer should enable word wrap")
  assert_true(vim.wo[0].linebreak, "issue buffer should wrap at word boundaries")
  assert_true(vim.wo[0].breakindent, "issue buffer should preserve indentation on wrapped lines")
  defer_next_issue_view = true
  require("github.issue_view").refresh({ force = true })
  assert_true(deferred_issue_view_callback ~= nil, "issue refresh did not start the deferred detail request")
  assert_true(buffer_contains("Title:  Fix command parser"), "issue refresh should keep existing content while loading")
  assert_true(
    not buffer_contains("Loading GitHub issue..."),
    "issue refresh should not replace existing content with a loading placeholder"
  )
  deferred_issue_view_callback()
  deferred_issue_view_callback = nil
  wait_for(function() return buffer_contains("Issue body") end, "issue refresh did not render the deferred response")
  assert_true(not buffer_contains("Hint:"), "issue buffer still rendered the legacy Hint line")
  local issue_winbar = plain_winbar()
  assert_true(issue_winbar:find("GitHub Issue #12", 1, true) ~= nil, "issue winbar title missing")
  assert_true(issue_winbar:find("b browse", 1, true) ~= nil, "issue winbar missing browse command")
  assert_true(issue_winbar:find("R refresh", 1, true) ~= nil, "issue winbar missing refresh command")
  assert_true(issue_winbar:find("<C-s> sync", 1, true) ~= nil, "issue winbar missing sync command")
  assert_true(issue_winbar:find("q close", 1, true) ~= nil, "issue winbar missing close command")
  assert_true(issue_winbar:find("? help", 1, true) ~= nil, "issue winbar missing help command")
  local help_map = vim.fn.maparg("?", "n", false, true)
  assert_true(type(help_map.callback) == "function", "issue help keymap did not install a callback")
  help_map.callback()
  assert_true(vim.bo.filetype == "GithubIssueHelp", "issue command popup did not open")
  assert_true(buffer_contains("Refresh issue"), "issue command popup missing refresh command")
  vim.api.nvim_win_close(0, true)
  vim.api.nvim_set_current_buf(buf)
  local title_line = find_line("Title:  Fix command parser")
  local author_line = find_line("Author:       alice")
  local state_line = find_line("State:        Open")
  assert_true(title_line ~= nil and author_line ~= nil and state_line ~= nil, "issue header fields missing")
  assert_true(title_line + 1 == author_line, "issue title should be followed immediately by author")
  assert_true(author_line < state_line, "issue state should render after author")
  local activity_line = find_line("Activity:     3 days ago")
  assert_true(activity_line ~= nil, "issue activity row did not render latest activity")
  assert_true(state_line < activity_line, "issue activity should render after state")
  assert_true(
    line_has_namespace_highlight(buf, "github.issue_view.decorations", activity_line, "DiffReviewStatusDate"),
    "issue activity did not receive the date highlight"
  )
  assert_true(
    line_has_namespace_highlight(buf, "github.issue_view.decorations", state_line, "DiffReviewStatusOpen"),
    "open issue state did not receive the open highlight"
  )
  local open_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewStatusOpen", link = false })
  assert_true(open_hl.fg == 0x50fa7b, "DiffReviewStatusOpen should be green")
  local closed_hl = vim.api.nvim_get_hl(0, { name = "DiffReviewStatusClosed", link = false })
  assert_true(closed_hl.fg == 0x6b7280, "DiffReviewStatusClosed should be gray")
  assert_true(not buffer_contains("Repo:"), "issue buffer should not render Repo metadata")
  assert_true(not buffer_contains("URL:"), "issue buffer should not render URL metadata")
  wait_for(function() return find_line("Release:      ◆ v1.0") ~= nil end, "issue release did not render")
  assert_true(find_line("Projects:     Roadmap (In progress), Backlog") ~= nil, "issue projects did not render")
  assert_true(find_line("Subscription: Subscribed") ~= nil, "issue subscription did not render")
  assert_true(find_line("Labels:       bug") ~= nil, "issue labels did not render")
  assert_true(find_line("Assignees:    @bob") ~= nil, "issue assignees did not render reviewer-style tokens")
  assert_true(find_line("Description:") ~= nil, "issue description heading did not render")
  assert_true(not buffer_contains("Opening Comment:"), "issue buffer still rendered the old opening comment heading")
  local comments_heading_line = find_line("Comments (1):")
  assert_true(comments_heading_line ~= nil, "issue comments did not render")
  local line_after_comments_heading = current_lines()[comments_heading_line + 1] or ""
  assert_true(
    line_after_comments_heading:find("carol", 1, true) ~= nil,
    "issue comments rendered an extra blank line after the comments heading"
  )
  assert_true(find_line("carol") ~= nil, "issue comments did not use the PR comment summary format")
  assert_true(not buffer_has_exact_line("Second comment line"), "issue comments should reuse collapsed PR comment rendering by default")
  vim.api.nvim_win_set_cursor(0, { comments_heading_line + 1, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  opened_urls = {}
  trigger_buf_mapping(buf, "b")
  assert_true(
    opened_urls[1] == "https://github.com/org/repo/issues/12#issuecomment-1",
    "issue comment browse did not use the comment URL"
  )
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return buffer_has_exact_line("Second comment line") end, "issue comment fold did not expand through shared PR rows")
  local expanded_comment_line = find_line("Second comment line")
  assert_true(expanded_comment_line ~= nil, "expanded issue comment body did not render")
  vim.api.nvim_win_set_cursor(0, { expanded_comment_line, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  trigger_buf_mapping(buf, "<Tab>")
  wait_for(function() return not buffer_has_exact_line("Second comment line") end, "issue comment fold did not collapse from body row")
  wait_for(function() return #render_markdown_calls > 0 end, "issue opening comment did not invoke markdown renderer")
  assert_true(vim.bo.filetype == "GithubIssue", "issue buffer did not use the GithubIssue filetype")
  local markdown_heading_line = find_line("## Foobar")
  assert_true(markdown_heading_line ~= nil, "issue markdown heading did not render")
  assert_true(
    line_has_namespace_highlight(buf, "github.issue_view.decorations", markdown_heading_line, "GithubIssueMarkdownHeading"),
    "issue markdown heading did not receive the issue heading highlight"
  )
  local markdown_heading_hl = vim.api.nvim_get_hl(0, { name = "GithubIssueMarkdownHeading", link = false })
  assert_true(markdown_heading_hl.bold == true, "issue markdown heading highlight was not bold")
  assert_true(markdown_heading_hl.fg == 0xffffff, "issue markdown heading highlight was not white")
  local body_line = find_line("Details.")
  assert_true(title_line ~= nil and body_line ~= nil, "issue editable fields missing")
  assert_true(issue_dirty_marker_count(buf) == 0, "issue dirty markers were present before edits")
  wait_for(function()
    return #repo_cache.contributors("org/repo") == 3
  end, "repo contributors were not cached for issue assignee completion")
  local assignees_line = find_line("Assignees:    @bob")
  assert_true(assignees_line ~= nil, "issue assignees row missing")
  move_cursor(buf, assignees_line)
  assert_true(vim.bo[buf].modifiable, "issue assignees were not editable")
  assert_contributor_completion(buf, assignees_line, "Assignees:    @")
  edit_line(buf, assignees_line, "Assignees:    @bob")
  assert_issue_completion(buf, body_line, "Details. #Fi")
  edit_line(buf, body_line, "Details.")
  wait_for(function() return issue_dirty_marker_count(buf) == 0 end, "issue completion probes left dirty markers")
  vim.api.nvim_win_set_cursor(0, { title_line, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "issue title was not editable")
  vim.api.nvim_buf_set_lines(buf, title_line - 1, title_line, false, { "Title:  Better parser" })
  edit_line(buf, assignees_line, "Assignees:    @alice @carol")
  vim.api.nvim_win_set_cursor(0, { body_line, 0 })
  vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
  assert_true(vim.bo[buf].modifiable, "issue body was not editable")
  vim.api.nvim_buf_set_lines(buf, body_line - 1, body_line, false, { "Updated details." })
  vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
  wait_for(function() return issue_dirty_marker_count(buf) == 3 end, "issue dirty markers did not render")
  vim.api.nvim_buf_call(buf, function()
    vim.cmd("write")
  end)
  local edit_key = "gh\tissue\tedit\t12\t--repo\torg/repo\t--title\tBetter parser\t--body-file\t-"
  wait_for(function()
    return find_call_containing(edit_key) ~= nil
  end, "issue write did not update issue")
  local edit_call = find_call_containing(edit_key)
  assert_true(edit_call.key:find("--add-assignee\talice", 1, true) ~= nil, "issue write did not add alice assignee")
  assert_true(edit_call.key:find("--add-assignee\tcarol", 1, true) ~= nil, "issue write did not add carol assignee")
  assert_true(edit_call.key:find("--remove-assignee\tbob", 1, true) ~= nil, "issue write did not remove bob assignee")
  assert_true(
    edit_call.input == "Issue body\n\n## Foobar\n\nUpdated details.",
    "issue write sent the wrong body"
  )
  wait_for(function() return not vim.bo[buf].modified end, "issue save did not clear the modified flag")
  wait_for(function() return issue_dirty_marker_count(buf) == 0 end, "issue save did not clear dirty markers")

  reset()
  vim.cmd.GithubPR()
  wait_for(function() return captured_picker ~= nil end, "PR picker did not open")
  assert_true(calls[1].key:find("gh\tsearch\tprs\t--author\t@me", 1, true) ~= nil, "PR author search command was not used")

  reset()
  vim.cmd("GithubPR 44")
  assert_true(#opened_pr_numbers == 1, "GithubPR with a number did not open PR by number")
  assert_true(opened_pr_numbers[1].number == "44", "GithubPR passed the wrong PR number")

  reset()
  vim.cmd.GithubReview()
  wait_for(function() return captured_picker ~= nil end, "review picker did not open")
  assert_true(
    calls[1].key:find("gh\tsearch\tprs\t--review-requested\t@me", 1, true) ~= nil,
    "review request search command was not used"
  )

  reset()
  vim.cmd("GithubIssue 34")
  wait_for(function() return find_line("Title:  Fix command parser") ~= nil end, "GithubIssue with a number did not open issue buffer")
  wait_for(function() return find_line("State:        Closed") ~= nil end, "closed issue state did not render as title case")
  local closed_state_line = find_line("State:        Closed")
  assert_true(
    line_has_namespace_highlight(vim.api.nvim_get_current_buf(), "github.issue_view.decorations", closed_state_line, "DiffReviewStatusClosed"),
    "closed issue state did not receive the closed highlight"
  )

  reset()
  vim.cmd("GithubIssueCreate Created issue")
  wait_for(function() return find_line("Title:  Fix command parser") ~= nil end, "GithubIssueCreate did not open created issue")
  assert_true(calls[1].key == "gh\tissue\tcreate\t--title\tCreated issue\t--body\t", "GithubIssueCreate did not create issue")

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
