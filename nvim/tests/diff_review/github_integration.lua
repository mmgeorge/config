vim.loader.enable(false)

local diff_review = require("diff_review")
local ai_commit = require("diff_review.ai_commit")
local commit = require("diff_review.commit")
local gh = require("diff_review.gh")
local issue_index = require("github.issue_index")
local original_notify = vim.notify
local original_system = vim.system

local root = "D:/mock/github-project"
local calls = {}
local opened_urls = {}
local pr_mode = "ready"
local pr_title = "Improve DiffReview"
local gh_host_mismatch = table.concat({
  "none of the git remotes configured for this repository correspond to the GH_HOST environment variable.",
  "Try adding a matching remote or unsetting the variable",
}, " ")
local generated_commit = "feat: add diff review ai summary\n\nAdd generated commit metadata."
local staged_generated_commit = "fix: update staged selection\n\nUse the staged diff."
local has_changes = true
local generate_count = 0
local notifications = {}
local staged_mode = "same"
local hold_pr_lookup = false
local release_pr_lookup = nil
local hold_push = false
local release_push = nil

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

---@type DiffReviewGitBackend
local git_backend = {}

function git_backend.systemlist(command)
  record("systemlist", command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "feature/pr-view" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "mock subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{upstream}" then return { "origin/feature/pr-view" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{upstream}" then return { "def5678" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{upstream}" then return { "upstream subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\t@{push}" then return { "origin/feature/pr-view" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\t@{push}" then return { "def5678" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s\t@{push}" then return { "push subject" }, 0 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return has_changes and { "M\tlua/diff_review/init.lua" } or {}, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff" then
    if not has_changes then return {}, 0 end
    return {
      "diff --git a/lua/diff_review/init.lua b/lua/diff_review/init.lua",
      "--- a/lua/diff_review/init.lua",
      "+++ b/lua/diff_review/init.lua",
      "@@ -1 +1 @@",
      "-old",
      "+new",
    }, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--cached" then return {}, 0 end
  return {}, 1
end

function git_backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = git_backend.systemlist(command)
    cb(output, code)
  end, 5)
end

function git_backend.system(command, input)
  record("system", command, input)
  local key = command_key(command)
  if key == "git\t-C\t" .. root .. "\tpush\t--progress" then return "", 0 end
  if key == "git\t-C\t" .. root .. "\tpull\t--progress" then return "", 0 end
  return "unexpected command: " .. key, 1
end

function git_backend.system_async(command, input, cb)
  if command_key(command) == "git\t-C\t" .. root .. "\tpush\t--progress" and hold_push then
    local output, code = git_backend.system(command, input)
    release_push = function()
      cb({
        code = code,
        stdout = output,
        stderr = "",
        output = output,
      })
    end
    return
  end

  vim.defer_fn(function()
    local output, code = git_backend.system(command, input)
    cb({
      code = code,
      stdout = output,
      stderr = "",
      output = output,
    })
  end, 5)
end

function git_backend.system_stream_async(command, input, on_line, cb)
  if command_key(command) == "git\t-C\t" .. root .. "\tpush\t--progress" and hold_push then
    local output, code = git_backend.system(command, input)
    on_line("Enumerating objects: 3, done.")
    release_push = function()
      cb({
        code = code,
        stdout = output,
        stderr = "",
        output = output,
      })
    end
    return
  end

  vim.defer_fn(function()
    local output, code = git_backend.system(command, input)
    if output ~= "" then on_line(output) end
    cb({
      code = code,
      stdout = output,
      stderr = "",
      output = output,
    })
  end, 5)
end

---@type DiffReviewGhBackend
local gh_backend = {}

local function pr_json()
  return vim.json.encode({
    number = 42,
    id = "PR_kwTEST42",
    title = pr_title,
    body = "This PR adds GitHub integration.\n\n- status row\n- PR view",
    url = "https://github.example.test/org/repo/pull/42",
    headRefName = "feature/pr-view",
    headRefOid = "abc123456789",
    commits = {
      {
        oid = "abc123456789",
        messageHeadline = "mock subject",
      },
    },
    files = {
      {
        path = "lua/diff_review/init.lua",
        additions = 8,
        deletions = 2,
        changeType = "MODIFIED",
      },
      {
        path = "lua/diff_review/gh.lua",
        additions = 100,
        deletions = 0,
        changeType = "ADDED",
      },
    },
    changedFiles = 2,
    additions = 108,
    deletions = 2,
    reviewRequests = {
      {
        asCodeOwner = true,
        requestedReviewer = {
          login = "platform-team",
          __typename = "Team",
        },
      },
    },
    milestone = vim.NIL,
    isDraft = true,
  })
end

local function respond_current_pr_lookup(cb)
  if pr_mode == "none" then
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  elseif pr_mode == "unavailable" then
    cb({ code = 1, stdout = "", stderr = gh_host_mismatch, output = gh_host_mismatch })
  else
    cb({ code = 0, stdout = pr_json(), stderr = "", output = pr_json() })
  end
end

function gh_backend.system_async(command, input, cb, cwd)
  record("gh_system_async", command, cwd)
  vim.defer_fn(function()
    local key = command_key(command)
    if key == "gh\tpr\tview\t--json\tid,number,title,body,url,headRefName,headRefOid,commits,files,changedFiles,additions,deletions,reviewRequests,milestone,isDraft" then
      if hold_pr_lookup then
        release_pr_lookup = function()
          release_pr_lookup = nil
          respond_current_pr_lookup(cb)
        end
        return
      end
      respond_current_pr_lookup(cb)
      return
    end
    if key == "gh\tpr\tdiff\t42\t--patch\t--color\tnever" then
      local diff = table.concat({
        "diff --git a/lua/diff_review/gh.lua b/lua/diff_review/gh.lua",
        "index 1111111..2222222 100644",
        "--- a/lua/diff_review/gh.lua",
        "+++ b/lua/diff_review/gh.lua",
        "@@ -0,0 +1 @@",
        "+new gh wrapper",
        " .../sources/strategies/chunks/ASourceChunk.ts        | 12 ++++++++++--",
        " .../strategies/chunks/ByReferenceTileSourceChunk.ts  |  6 +++---",
        " 8 files changed, 37 insertions(+), 23 deletions(-)",
      }, "\n")
      cb({ code = 0, stdout = diff, stderr = "", output = diff })
      return
    end
    if key == "gh\tapi\tgraphql\t--input\t-" then
      local graphql_payload = vim.json.decode(input or "{}")
      local query = tostring(graphql_payload.query or "")
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
                                name = "Dummy Typecheck",
                                status = "COMPLETED",
                                conclusion = "SUCCESS",
                                detailsUrl = "https://github.example.test/org/repo/actions/runs/42/job/101",
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
      cb({ code = 0, stdout = vim.json.encode({ data = { repository = { pullRequest = {} } } }), stderr = "", output = "" })
      return
    end
    cb({ code = 1, stdout = "", stderr = "unexpected gh command: " .. key, output = "unexpected gh command: " .. key })
  end, 120)
end

function gh_backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

---@type DiffReviewAICommitBackend
local ai_backend = {}

function ai_backend.systemlist_async(command, cb)
  record("ai_systemlist_async", command)
  local key = command_key(command)
  vim.defer_fn(function()
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\tHEAD\t--stat\t--summary" then
      if not has_changes then
        cb({}, 0)
        return
      end
      cb({ " lua/diff_review/init.lua | 2 +-" }, 0)
      return
    end
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\tHEAD" then
      if not has_changes then
        cb({}, 0)
        return
      end
      cb({
        "diff --git a/lua/diff_review/init.lua b/lua/diff_review/init.lua",
        "--- a/lua/diff_review/init.lua",
        "+++ b/lua/diff_review/init.lua",
        "@@ -1 +1 @@",
        "-old",
        "+new",
      }, 0)
      return
    end
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\t--staged\t--stat\t--summary" then
      if staged_mode == "none" then
        cb({}, 0)
      elseif staged_mode == "different" then
        cb({ " staged_only.rs | 2 +-" }, 0)
      else
        cb({ " lua/diff_review/init.lua | 2 +-" }, 0)
      end
      return
    end
    if key == "git\t-C\t" .. root .. "\tdiff\t--no-ext-diff\t--no-color\t--staged" then
      if staged_mode == "none" then
        cb({}, 0)
      elseif staged_mode == "different" then
        cb({
          "diff --git a/staged_only.rs b/staged_only.rs",
          "--- a/staged_only.rs",
          "+++ b/staged_only.rs",
          "@@ -1 +1 @@",
          "-old_staged",
          "+new_staged",
        }, 0)
      else
        cb({
          "diff --git a/lua/diff_review/init.lua b/lua/diff_review/init.lua",
          "--- a/lua/diff_review/init.lua",
          "+++ b/lua/diff_review/init.lua",
          "@@ -1 +1 @@",
          "-old",
          "+new",
        }, 0)
      end
      return
    end
    cb({}, 1)
  end, 5)
end

function ai_backend.generate_async(context, cb)
  generate_count = generate_count + 1
  vim.defer_fn(function()
    if context:find("staged_only.rs", 1, true) then
      cb({ ok = true, message = staged_generated_commit })
      return
    end
    cb({ ok = true, message = generated_commit })
  end, 80)
end

local function status_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function plain_winbar()
  return (vim.wo.winbar or ""):gsub("%%#[^#]+#", ""):gsub("%%%*", ""):gsub("%%=", " "):gsub("%%%%", "%%")
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(status_lines(buf)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function count_lines_equal(buf, expected)
  local count = 0
  for _, line in ipairs(status_lines(buf)) do
    if line == expected then count = count + 1 end
  end
  return count
end

local function reset_notifications()
  notifications = {}
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

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

---@param buf integer
---@param keys string[]
local function assert_buffer_contains_all(buf, keys)
  for _, key in ipairs(keys) do
    assert_true(buffer_contains(buf, key), "buffer missing " .. key)
  end
end

local function run()
  diff_review.set_git_backend(git_backend)
  ai_commit.set_backend(ai_backend)
  gh.set_backend(gh_backend)
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  issue_index._set_runner_for_test(function(command, _, callback)
    local action = command[2]
    if action == "state" then
      callback({ code = 0, stdout = vim.json.encode({}), stderr = "", output = "{}" })
      return
    end
    if action == "upsert-page" or action == "snapshot" then
      callback({ code = 0, stdout = vim.json.encode({}), stderr = "", output = "{}" })
      return
    end
    callback({ code = 1, stdout = "", stderr = "unexpected issue-index action: " .. tostring(action), output = "unexpected issue-index action: " .. tostring(action) })
  end)
  issue_index._set_gh_runner_for_test(function(_, _, callback)
    local stdout = vim.json.encode({
      data = {
        rateLimit = { remaining = 5000 },
        repository = {
          issues = {
            totalCount = 0,
            pageInfo = { hasNextPage = false, endCursor = vim.NIL },
            nodes = {},
          },
        },
      },
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)
  vim.notify = function(message, level, opts)
    notifications[#notifications + 1] = {
      message = tostring(message),
      level = level,
      opts = opts,
    }
  end
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest", about_auto_generate = true, about_auto_generate_delay_ms = 0 })
  diff_review.open()
  local status_buf = vim.api.nvim_get_current_buf()

  wait_for(function() return buffer_contains(status_buf, "PR:") and buffer_contains(status_buf, "...fetching...") end, "PR row did not show fetching state")
  wait_for(function() return buffer_contains(status_buf, "About:  ...generating...") end, "About row did not show generating state")
  wait_for(function()
    local state = ai_commit.state()
    return state and state.state == "generating"
  end, "AI commit generation did not enter generating state")

  local commit_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(commit_buf, 0, -1, false, { "" })
  vim.bo[commit_buf].modifiable = true
  local waiting_messages = {}
  ai_commit.populate_commit_buffer_when_ready(commit_buf, root, function(message)
    waiting_messages[#waiting_messages + 1] = message
  end)
  ai_commit.populate_commit_buffer_when_ready(commit_buf, root)
  assert_true(
    waiting_messages[1] == "Waiting for generated commit message...",
    "waiting commit buffer did not report that it joined existing About generation"
  )

  wait_for(function() return buffer_contains(status_buf, "Improve DiffReview") end, "PR row did not show fetched PR title")
  assert_true(diff_review._status.pr.pr.repo == "org/repo", "current PR repo was not normalized from its URL")
  wait_for(function() return buffer_contains(status_buf, "feat: add diff review ai summary") end, "About row did not show generated commit subject")
  wait_for(function()
    local lines = vim.api.nvim_buf_get_lines(commit_buf, 0, -1, false)
    return lines[1] == "feat: add diff review ai summary"
  end, "generated commit was not applied to commit buffer after waiting")
  assert_true(count_lines_equal(commit_buf, "feat: add diff review ai summary") == 1, "generated commit duplicated in waiting buffer")
  assert_true(generate_count == 1, "waiting commit buffer started another AI generation")
  assert_true(vim.bo[commit_buf].modifiable, "commit buffer modifiable state was not preserved")

  local commit_host_buf = vim.api.nvim_get_current_buf()
  local commit_console = vim.api.nvim_create_buf(false, true)
  local commit_tmp = vim.fn.tempname()
  vim.fn.mkdir(commit_tmp, "p")
  local commit_editmsg = commit_tmp .. "/COMMIT_EDITMSG"
  local saw_unmarked_editmsg = false
  local mark_group = vim.api.nvim_create_augroup("DiffReviewCommitBufferMarkTest", { clear = true })
  vim.api.nvim_create_autocmd({ "BufReadPost", "BufEnter" }, {
    group = mark_group,
    pattern = "COMMIT_EDITMSG",
    callback = function(args)
      if not vim.b[args.buf].diff_review_commit_buffer or not vim.b[args.buf].ai_commit_generated then
        saw_unmarked_editmsg = true
      end
    end,
  })
  commit._active = {
    win = vim.api.nvim_get_current_win(),
    prev_buf = commit_host_buf,
    prev_winbar = "",
    console = commit_console,
    root = root,
  }
  reset_notifications()
  commit.editor(commit_editmsg, "")
  local editmsg_buf = vim.api.nvim_get_current_buf()
  pcall(vim.api.nvim_del_augroup_by_id, mark_group)
  assert_true(not saw_unmarked_editmsg, "DiffReview commit buffer was visible to autocmds before AI opt-out marks")
  wait_for(function() return buffer_contains(editmsg_buf, "feat: add diff review ai summary") end, "commit editor did not reuse About message")
  assert_true(count_lines_equal(editmsg_buf, "feat: add diff review ai summary") == 1, "generated commit duplicated in editmsg buffer")
  assert_true(generate_count == 1, "commit editor started another AI generation")
  assert_true(#notifications == 0, "ready About message should not notify when entering commit editor")
  assert_true(vim.b[editmsg_buf].ai_commit_generated == true, "DiffReview commit buffer did not suppress global AI commit autocmd")
  assert_true(vim.b[editmsg_buf].diff_review_commit_buffer == true, "DiffReview commit buffer did not set its opt-out marker")
  local submit_mapping = vim.fn.maparg("<C-c><C-c>", "n", false, true)
  assert_true(type(submit_mapping.callback) == "function", "commit submit mapping missing")
  submit_mapping.callback()
  assert_true(vim.api.nvim_get_current_buf() == commit_console, "commit submit did not immediately return to console")
  assert_true(buffer_contains(commit_console, "Finalizing commit..."), "commit console did not show finalizing state")
  commit._active = nil
  if vim.api.nvim_buf_is_valid(editmsg_buf) then vim.bo[editmsg_buf].modified = false end
  vim.api.nvim_win_set_buf(0, commit_host_buf)
  if vim.api.nvim_buf_is_valid(editmsg_buf) then
    vim.api.nvim_buf_delete(editmsg_buf, { force = true })
  end
  if vim.api.nvim_buf_is_valid(commit_console) then
    vim.api.nvim_buf_delete(commit_console, { force = true })
  end
  vim.fn.delete(commit_tmp, "rf")

  trigger_normal_mapping("<CR>", find_row(status_buf, "About:"))
  local about_buf = vim.api.nvim_get_current_buf()
  assert_true(about_buf ~= status_buf, "About view did not open a new buffer")
  assert_true(buffer_contains(about_buf, "feat: add diff review ai summary"), "About view missing commit subject")
  assert_true(buffer_contains(about_buf, "Add generated commit metadata."), "About view missing commit body")
  vim.api.nvim_win_set_buf(0, status_buf)

  trigger_normal_mapping("<CR>", find_row(status_buf, "PR:"))
  local pr_buf = vim.api.nvim_get_current_buf()
  assert_true(pr_buf ~= status_buf, "PRView did not open a new buffer")
  assert_true(vim.bo[pr_buf].filetype == "GitStatus", "PRView is not a GitStatus buffer")
  local pr_hint = plain_winbar()
  assert_true(pr_hint:find("PR #42", 1, true) ~= nil, "PRView missing title: " .. pr_hint)
  assert_true(pr_hint:find("toggle", 1, true) ~= nil, "PRView missing toggle hint: " .. pr_hint)
  assert_true(pr_hint:find("b browse", 1, true) ~= nil, "PRView missing browse hint: " .. pr_hint)
  assert_true(pr_hint:find("q close", 1, true) ~= nil, "PRView missing close hint: " .. pr_hint)
  assert_true(not buffer_contains(pr_buf, "Hint:"), "PRView hint should be a sticky winbar, not buffer text")
  assert_true(buffer_contains(pr_buf, "Title:  Improve DiffReview"), "PRView missing title")
  assert_true(
    buffer_contains(pr_buf, "Review: " .. diff_review._pending_review_icon .. " " .. diff_review._codeowner_review_icon .. "@platform-team"),
    "PRView missing draft codeowner reviewer warning"
  )
  assert_true(buffer_contains(pr_buf, "Description:"), "PRView missing description heading")
  assert_true(buffer_contains(pr_buf, "- status row"), "PRView missing markdown body")
  assert_true(buffer_contains(pr_buf, "Checks:"), "PRView missing checks heading")
  wait_for(function() return buffer_contains(pr_buf, "Dummy Typecheck") end, "PRView missing check status row")
  local typecheck_row = find_row(pr_buf, "Dummy Typecheck")
  local typecheck_line = vim.api.nvim_buf_get_lines(pr_buf, typecheck_row - 1, typecheck_row, false)[1] or ""
  assert_true(
    typecheck_line:find("✓ Dummy Typecheck | PR Dummy Checks", 1, true) ~= nil,
    "PRView check status row should render icon, name, and description only: " .. typecheck_line
  )
  assert_true(not typecheck_line:find("SUCCESS", 1, true), "PRView check status row should not render state text: " .. typecheck_line)
  assert_true(buffer_contains(pr_buf, "Head:"), "PRView missing head row")
  assert_true(buffer_contains(pr_buf, "Changes (2):"), "PRView missing changes heading")
  assert_true(buffer_contains(pr_buf, "lua/diff_review/gh.lua +100 -0"), "PRView missing file change row")
  assert_true(buffer_contains(pr_buf, "Recent Commits (1):"), "PRView missing recent commits section")
  assert_true(
    find_row(pr_buf, "Description:") < find_row(pr_buf, "Checks:")
      and find_row(pr_buf, "Checks:") < find_row(pr_buf, "Changes (2):")
      and find_row(pr_buf, "Changes (2):") < find_row(pr_buf, "Recent Commits (1):"),
    "PRView did not render Description, Checks, Changes, and Recent Commits in order"
  )
  trigger_normal_mapping("<Tab>", find_row(pr_buf, "lua/diff_review/gh.lua"))
  wait_for(function() return buffer_contains(pr_buf, "@@ +1 -0") or buffer_contains(pr_buf, "No textual diff") end, "PR file did not expand")
  assert_true(not buffer_contains(pr_buf, "ASourceChunk.ts"), "PR diffstat leaked into expanded hunk")
  assert_true(not buffer_contains(pr_buf, "files changed"), "PR diff summary leaked into expanded hunk")

  trigger_normal_mapping("b", 1)
  assert_true(opened_urls[#opened_urls] == "https://github.example.test/org/repo/pull/42", "browse did not open PR URL")
  trigger_normal_mapping("q", 1)
  assert_true(not vim.api.nvim_buf_is_valid(pr_buf), "q did not close PRView")

  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_normal_mapping("?", find_row(status_buf, "Head:"))
  local help_buf = vim.api.nvim_get_current_buf()
  assert_true(help_buf ~= status_buf, "help did not open a popup buffer")
  assert_buffer_contains_all(help_buf, { "<Tab>", "N", "S", "U", "j", "cc", "opP", "opp", "ogp", "o", "<CR>", "R", "or", "q", "?" })
  assert_true(not buffer_contains(help_buf, "ogc"), "help should not list removed ogc mapping")
  pcall(vim.api.nvim_win_close, 0, true)

  vim.api.nvim_win_set_buf(0, status_buf)
  reset_notifications()
  pr_mode = "ready"
  pr_title = "PR after queued ogp"
  hold_pr_lookup = true
  release_pr_lookup = nil
  diff_review.render_status(status_buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(status_buf, "...fetching...") end, "PR row did not refetch for queued ogp test")
  trigger_normal_mapping("ogp", find_row(status_buf, "Head:"))
  assert_true(vim.api.nvim_get_current_buf() == status_buf, "ogp while fetching should wait in GitStatus")
  for _, notification in ipairs(notifications) do
    assert_true(
      not notification.message:find("GitHub PR is still loading", 1, true),
      "ogp while fetching should not dead-end with a loading notification"
    )
  end
  wait_for(function() return type(release_pr_lookup) == "function" end, "queued ogp test did not capture held PR lookup")
  hold_pr_lookup = false
  release_pr_lookup()
  wait_for(function()
    return vim.api.nvim_get_current_buf() ~= status_buf and buffer_contains(vim.api.nvim_get_current_buf(), "PR after queued ogp")
  end, "queued ogp did not open PRView after PR lookup resolved")
  local queued_pr_buf = vim.api.nvim_get_current_buf()
  trigger_normal_mapping("q", 1)
  assert_true(not vim.api.nvim_buf_is_valid(queued_pr_buf), "q did not close queued PRView")

  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_normal_mapping("ogp", find_row(status_buf, "Head:"))
  assert_true(vim.api.nvim_get_current_buf() ~= status_buf, "ogp did not open PRView")

  vim.api.nvim_win_set_buf(0, status_buf)
  pr_mode = "none"
  diff_review.render_status(status_buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(status_buf, "PR:     none") end, "PR row did not render none state")

  local open_pr = require("github.open_pr")
  local original_open_pr = open_pr.open
  local create_pr_count = 0
  open_pr.open = function()
    create_pr_count = create_pr_count + 1
  end

  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_normal_mapping("ogp", find_row(status_buf, "Head:"))
  local create_prompt_buf = vim.api.nvim_get_current_buf()
  assert_true(create_prompt_buf ~= status_buf, "ogp with no PR did not open create prompt")
  assert_true(buffer_contains(create_prompt_buf, "No GitHub PR found for this branch."), "create prompt missing no-PR message")
  assert_true(buffer_contains(create_prompt_buf, "Create a draft PR now?"), "create prompt missing create question")
  trigger_normal_mapping("n", 1)
  assert_true(create_pr_count == 0, "declining create prompt should not create a PR")

  vim.api.nvim_win_set_buf(0, status_buf)
  trigger_normal_mapping("ogp", find_row(status_buf, "Head:"))
  trigger_normal_mapping("y", 1)
  assert_true(create_pr_count == 1, "accepting create prompt should start GithubPRCreate flow")
  open_pr.open = original_open_pr

  reset_notifications()
  local count_before_unavailable = generate_count
  pr_mode = "unavailable"
  diff_review.render_status(status_buf, nil, nil, { refresh_pr = true })
  wait_for(function() return buffer_contains(status_buf, "PR:     unavailable") end, "PR row did not render unavailable state")
  assert_true(#notifications == 0, "unavailable PR lookup should not emit error notifications: " .. vim.inspect(notifications))
  assert_true(generate_count == count_before_unavailable, "unavailable PR lookup should not restart AI generation")

  gh.reset_backend()
  gh.set_timeout_ms(20)
  local timeout_result = nil
  local killed = false
  vim.system = function()
    return {
      kill = function()
        killed = true
      end,
    }
  end
  gh.current_pr_async(root, function(result)
    timeout_result = result
  end)
  wait_for(function() return timeout_result ~= nil end, "gh PR lookup did not time out")
  assert_true(timeout_result.ok == false, "timed out PR lookup should fail")
  assert_true((timeout_result.message or ""):find("timed out", 1, true) ~= nil, "timeout result should explain the timeout")
  assert_true(killed, "timed out gh process should be killed")
  vim.system = original_system
  gh.reset_timeout_ms()
  gh.set_backend(gh_backend)

  pr_mode = "ready"
  pr_title = "PR after manual refresh"
  assert_true(type(vim.fn.maparg("R", "n", false, true).callback) == "function", "R refresh mapping missing")
  trigger_normal_mapping("R", find_row(status_buf, "Head:"))
  wait_for(function() return buffer_contains(status_buf, "PR after manual refresh") end, "R did not force-refresh PR state")

  pr_title = "PR after push"
  hold_push = true
  release_push = nil
  trigger_normal_mapping("opp", find_row(status_buf, "Head:"))
  wait_for(function()
    return buffer_contains(status_buf, "Push:   Enumerating objects: 3, done.")
  end, "push did not render streamed git output")
  wait_for(function()
    for _, call in ipairs(calls) do
      if call.kind == "system" and call.key == "git\t-C\t" .. root .. "\tpush\t--progress" then return true end
    end
    return false
  end, "push command did not run")
  assert_true(buffer_contains(status_buf, "PR after manual refresh"), "push should not force-refresh PR state")
  assert_true(not buffer_contains(status_buf, "PR after push"), "push unexpectedly refreshed PR state before completion")

  hold_push = false
  assert_true(type(release_push) == "function", "push completion was not captured")
  release_push()
  wait_for(function() return not buffer_contains(status_buf, "Pushing...") end, "push pending state did not clear")
  assert_true(buffer_contains(status_buf, "PR after manual refresh"), "push completion should keep cached PR state")
  assert_true(not buffer_contains(status_buf, "PR after push"), "push completion unexpectedly refreshed PR state")

  trigger_normal_mapping("opP", find_row(status_buf, "Head:"))
  wait_for(function()
    for _, call in ipairs(calls) do
      if call.kind == "system" and call.key == "git\t-C\t" .. root .. "\tpull\t--progress" then return true end
    end
    return false
  end, "pull command did not run")

  if vim.api.nvim_buf_is_valid(status_buf) then
    vim.api.nvim_buf_delete(status_buf, { force = true })
  end
  diff_review._status = nil
  diff_review.setup({
    pr_buffer_name = "DiffReviewPRTest",
    keymaps = {
      status = {
        commit = "zc",
        push = "zpP",
        pull = "zpp",
        pr = "zgp",
        help = "zh",
      },
    },
  })
  diff_review.open()
  local override_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(override_buf, "PR:") end, "override status buffer did not render")
  assert_true(type(vim.fn.maparg("zc", "n", false, true).callback) == "function", "custom commit mapping missing")
  assert_true(type(vim.fn.maparg("zpP", "n", false, true).callback) == "function", "custom push mapping missing")
  assert_true(type(vim.fn.maparg("zpp", "n", false, true).callback) == "function", "custom pull mapping missing")
  assert_true(type(vim.fn.maparg("zgp", "n", false, true).callback) == "function", "custom PR mapping missing")
  trigger_normal_mapping("zh", find_row(override_buf, "Head:"))
  local override_help_buf = vim.api.nvim_get_current_buf()
  assert_buffer_contains_all(override_help_buf, { "zc", "zpP", "zpp", "zgp", "zh" })
  assert_true(not buffer_contains(override_help_buf, "zgc"), "custom help should not list removed current PR mapping")

  if vim.api.nvim_win_is_valid(0) then
    pcall(vim.api.nvim_win_close, 0, true)
  end
  if vim.api.nvim_buf_is_valid(override_buf) then
    vim.api.nvim_buf_delete(override_buf, { force = true })
  end
  has_changes = true
  ai_commit.reset_backend()
  ai_commit.set_backend(ai_backend)
  local count_before_default_about = generate_count
  diff_review._status = nil
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest" })
  diff_review.open()
  local default_about_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(default_about_buf, "lua/diff_review/init.lua +1 -1") end, "default status with changes did not render")
  wait_for(function() return buffer_contains(default_about_buf, "About:  ...generating...") end, "default About row did not auto-generate")
  vim.wait(120)
  assert_true(generate_count == count_before_default_about, "default About generation started before the idle delay")
  wait_for(function() return buffer_contains(default_about_buf, "feat: add diff review ai summary") end, "default About generation did not update status row")
  assert_true(generate_count == count_before_default_about + 1, "default status open did not start exactly one generation")
  if vim.api.nvim_buf_is_valid(default_about_buf) then
    vim.api.nvim_buf_delete(default_about_buf, { force = true })
  end

  ai_commit.reset_backend()
  ai_commit.set_backend(ai_backend)
  local count_before_opt_out_about = generate_count
  diff_review._status = nil
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest", about_auto_generate = false })
  diff_review.open()
  local opt_out_about_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(opt_out_about_buf, "lua/diff_review/init.lua +1 -1") end, "opt-out status with changes did not render")
  assert_true(buffer_contains(opt_out_about_buf, "About:  none"), "About row should show none when auto-generation is disabled")
  vim.wait(120)
  assert_true(generate_count == count_before_opt_out_about, "opt-out status open started AI generation")
  trigger_normal_mapping("<CR>", find_row(opt_out_about_buf, "About:"))
  wait_for(function() return buffer_contains(opt_out_about_buf, "About:  ...generating...") end, "manual opt-out About open did not start generation")
  wait_for(function() return buffer_contains(opt_out_about_buf, "feat: add diff review ai summary") end, "manual opt-out About generation did not update status row")
  assert_true(generate_count == count_before_opt_out_about + 1, "manual opt-out About open did not start exactly one generation")
  if vim.api.nvim_buf_is_valid(opt_out_about_buf) then
    vim.api.nvim_buf_delete(opt_out_about_buf, { force = true })
  end

  has_changes = false
  local count_before_no_changes = generate_count
  diff_review._status = nil
  diff_review.setup({ pr_buffer_name = "DiffReviewPRTest" })
  diff_review.open()
  local no_changes_buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(no_changes_buf, "No changes") end, "no changes status did not render")
  assert_true(buffer_contains(no_changes_buf, "About:  none"), "About row should show none with no changes")
  vim.wait(120)
  assert_true(generate_count == count_before_no_changes, "AI generation started despite no changes")

  has_changes = true
  ai_commit.reset_backend()
  ai_commit.set_backend(ai_backend)
  staged_mode = "same"
  local count_before_direct_generate = generate_count
  local direct_commit_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(direct_commit_buf, 0, -1, false, { "" })
  vim.bo[direct_commit_buf].modifiable = true
  local generating_messages = {}
  ai_commit.populate_commit_buffer_when_ready(direct_commit_buf, root, function(message)
    generating_messages[#generating_messages + 1] = message
  end)
  assert_true(
    generating_messages[1] == "Generating commit message...",
    "commit buffer did not report new AI commit generation"
  )
  wait_for(function() return buffer_contains(direct_commit_buf, "feat: add diff review ai summary") end, "direct commit generation did not populate buffer")
  assert_true(generate_count == count_before_direct_generate + 1, "direct commit buffer did not start exactly one AI generation")

  ai_commit.reset_backend()
  ai_commit.set_backend(ai_backend)
  staged_mode = "same"
  local count_before_head_generate = generate_count
  ai_commit.ensure(root, nil, function() end)
  wait_for(function()
    local state = ai_commit.state()
    return state and state.state == "ready"
  end, "HEAD About generation did not complete before staged mismatch test")
  assert_true(generate_count == count_before_head_generate + 1, "HEAD About generation did not start for staged mismatch test")

  staged_mode = "different"
  local mismatch_commit_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(mismatch_commit_buf, 0, -1, false, { "" })
  vim.bo[mismatch_commit_buf].modifiable = true
  local mismatch_messages = {}
  ai_commit.populate_commit_buffer_when_ready(mismatch_commit_buf, root, function(message)
    mismatch_messages[#mismatch_messages + 1] = message
  end)
  wait_for(function()
    return mismatch_messages[1] == "Generating commit message..."
  end, "staged mismatch did not notify when replacement generation started")
  wait_for(function() return buffer_contains(mismatch_commit_buf, "fix: update staged selection") end, "staged mismatch did not generate a staged commit message")
  assert_true(generate_count == count_before_head_generate + 2, "staged mismatch did not start exactly one replacement generation")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
ai_commit.reset_backend()
gh.reset_backend()
gh.reset_timeout_ms()
issue_index._reset_for_test()
vim.system = original_system
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
