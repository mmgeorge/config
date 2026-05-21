local M = {}

local base_branch = "main"
local pr_context_limit = 60000
local pr_adapter = {
  name = "copilot",
  model = "gpt-4.1",
}

local pr_system_prompt = [[You generate GitHub pull request metadata from git commit messages.
Return ONLY valid JSON with this exact shape:
{"title":"<title>","body":"<markdown body>"}

Title rules:
- Must be a Conventional Commit subject.
- Format: <type>: <description>
- Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore, revert
- Imperative mood.
- Lowercase first letter after the colon.
- No trailing period.
- Prefer concrete nouns from the commit messages.

Body rules:
Use this template:

Related <issue number list>

<summary>

# Changes
<major changes>

## Testing
- [x] Automated (integration, performance, screenshot, unit)
<test files>
- [ ] Manual (test app)

For the above template, follow these rules:
- <issue number list>
  - If the issue numbers are known, put them here. Otherwise just write # and the user will fill in the rest.
- <summary>
  - Include a brief paragraph description of the PR that is succinct an factual.
  - The first couples senteces state what major feature was added or bug fixed, a restatement of the title.
    - "This PR adds support for X..."
  - Then follow up with details about what major changes were needed to add the feature or fix the bug
    - "A new source type ParquetSourceWorker handles querying files"
- <major changes>
  - A bulleted list of the major changes of the PR. The new files added and structure changes.
  - Look at all commits and then created this bulleted summary.
  - Do *not* just restate the commit history. Look at changes as a whole and then determine aggregate changes.
- <test files>
  - A bulleted list of added tests.
]]

local progress = {
  id = "github_open_pr",
  timer = nil,
  spinner_index = 1,
  spinner = { "|", "/", "-", "\\" },
}

local function notify(message, level, opts)
  if type(level) == "string" then
    level = ({
      info = vim.log.levels.INFO,
      warn = vim.log.levels.WARN,
      error = vim.log.levels.ERROR,
    })[level] or vim.log.levels.INFO
  end

  opts = vim.tbl_extend("force", {
    id = progress.id,
    title = "GithubOpenPR",
  }, opts or {})
  vim.notify(message, level or vim.log.levels.INFO, opts)
end

local function stop_progress()
  if progress.timer and not progress.timer:is_closing() then
    progress.timer:stop()
    progress.timer:close()
  end
  progress.timer = nil
end

local function show_progress(message)
  stop_progress()
  progress.spinner_index = 1

  local function render()
    notify(message, "info", {
      icon = progress.spinner[progress.spinner_index],
      timeout = false,
    })
  end

  render()
  progress.timer = vim.uv.new_timer()
  progress.timer:start(120, 120, vim.schedule_wrap(function()
    progress.spinner_index = (progress.spinner_index % #progress.spinner) + 1
    render()
  end))
end

local function complete_progress(message)
  stop_progress()
  notify(message, "info", {
    icon = "OK",
    timeout = 3000,
  })
end

local function fail_progress(message)
  stop_progress()
  notify(message, "error", {
    icon = "!!",
    timeout = 8000,
  })
end

local function truncate_at_line(text, limit)
  if #text <= limit then
    return text, false
  end

  local truncated = text:sub(1, limit)
  local last_newline = truncated:match(".*()\n")
  if last_newline and last_newline > 1 then
    truncated = truncated:sub(1, last_newline - 1)
  end
  return truncated, true
end

local function command_error(result)
  local message = result.stderr
  if not message or vim.trim(message) == "" then
    message = result.stdout
  end
  if not message or vim.trim(message) == "" then
    message = "command exited with code " .. tostring(result.code)
  end
  return vim.trim(message)
end

local function run(args, opts, callback)
  vim.system(args, {
    cwd = opts and opts.cwd or nil,
    text = true,
  }, function(result)
    vim.schedule(function()
      if result.code ~= 0 then
        callback(nil, command_error(result))
        return
      end

      callback(result.stdout or "")
    end)
  end)
end

local function git(args, cwd, callback)
  run(vim.list_extend({ "git" }, args), { cwd = cwd }, callback)
end

local function model_content(result)
  local content = result and result.output
  if type(content) == "table" then
    content = content.content
  end
  if type(content) ~= "string" then
    return nil
  end

  content = vim.trim(content)
  content = content:gsub("^```%w*\n", ""):gsub("\n```$", "")
  return vim.trim(content)
end

local function decode_pr_metadata(content)
  if not content or content == "" then
    return nil, "No response from model"
  end

  local ok, metadata = pcall(vim.json.decode, content)
  if not ok or type(metadata) ~= "table" then
    return nil, "Model returned invalid JSON"
  end

  local title = type(metadata.title) == "string" and vim.trim(metadata.title) or ""
  local body = type(metadata.body) == "string" and vim.trim(metadata.body) or ""
  if title == "" or body == "" then
    return nil, "Model response must include title and body"
  end

  return {
    title = vim.trim(title:gsub("\n.*", "")),
    body = body,
  }
end

local function safe_inline_output(background)
  local inline_output = background
      and background.adapter
      and background.adapter.handlers
      and background.adapter.handlers.inline_output

  if type(inline_output) == "function" and not background.adapter.handlers._commit_safe_inline_output then
    background.adapter.handlers.inline_output = function(self, data, context)
      if type(data) == "string" then
        data = { body = data }
      end

      return inline_output(self, data, context)
    end
    background.adapter.handlers._commit_safe_inline_output = true
  end
end

local function build_context(branch, base_ref, commits)
  local truncated_commits, commits_truncated = truncate_at_line(commits, pr_context_limit)
  local sections = {
    "Generate metadata for a draft GitHub PR.",
    "Base branch: " .. base_branch,
    "Comparison ref used for commit messages: " .. base_ref,
    "Head branch: " .. branch,
  }

  if commits_truncated then
    table.insert(sections, "Some commit message context was truncated to stay within the model request size.")
  end

  table.insert(sections, "Commit messages:\n```text\n" .. truncated_commits .. "\n```")
  return table.concat(sections, "\n\n")
end

local function generate_metadata(context, callback)
  local ok, Background = pcall(require, "codecompanion.interactions.background")
  if not ok then
    callback(nil, "Could not load CodeCompanion background interactions")
    return
  end

  local background = Background.new({ adapter = pr_adapter })
  safe_inline_output(background)

  show_progress("Generating PR title and description with " .. pr_adapter.model .. "...")
  background:ask({
    { role = "system", content = pr_system_prompt },
    { role = "user", content = context },
  }, {
    method = "async",
    parse_handler = "parse_inline",
    silent = true,
    on_done = function(result)
      vim.schedule(function()
        local metadata, err = decode_pr_metadata(model_content(result))
        callback(metadata, err)
      end)
    end,
    on_error = function(err)
      vim.schedule(function()
        local message = type(err) == "string" and err or vim.inspect(err)
        callback(nil, message)
      end)
    end,
  })
end

local function push_branch(cwd, branch, callback)
  git({ "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}" }, cwd, function(upstream)
    local args = { "push" }
    if upstream and vim.trim(upstream) ~= "" then
      show_progress("Pushing current branch...")
    else
      show_progress("Pushing current branch and setting upstream...")
      vim.list_extend(args, { "-u", "origin", branch })
    end

    git(args, cwd, callback)
  end)
end

local function create_pr(cwd, branch, metadata)
  show_progress("Creating draft PR...")
  run({
    "gh",
    "pr",
    "create",
    "--draft",
    "--base",
    base_branch,
    "--head",
    branch,
    "--title",
    metadata.title,
    "--body",
    metadata.body,
  }, { cwd = cwd }, function(output, err)
    if err then
      fail_progress("Failed to create PR: " .. err)
      return
    end

    local pr_url = output:match("https?://%S+") or vim.trim(output)
    show_progress("Opening PR in browser...")
    run({ "gh", "pr", "view", pr_url, "--web" }, { cwd = cwd }, function(_, open_err)
      if open_err then
        if vim.ui and vim.ui.open then
          vim.ui.open(pr_url)
          complete_progress("Created draft PR: " .. pr_url)
          return
        end

        fail_progress("Created PR, but failed to open browser: " .. open_err)
        return
      end

      complete_progress("Created draft PR: " .. pr_url)
    end)
  end)
end

function M.open()
  git({ "rev-parse", "--show-toplevel" }, nil, function(root, root_err)
    if root_err then
      fail_progress("Not in a git repository: " .. root_err)
      return
    end

    local cwd = vim.trim(root)
    git({ "branch", "--show-current" }, cwd, function(branch, branch_err)
      if branch_err then
        fail_progress("Could not determine current branch: " .. branch_err)
        return
      end

      branch = vim.trim(branch)
      if branch == "" then
        fail_progress("Cannot create a PR from a detached HEAD")
        return
      end
      if branch == base_branch then
        fail_progress("Cannot create a PR from " .. base_branch)
        return
      end

      git({ "rev-parse", "--verify", "origin/" .. base_branch }, cwd, function(_, origin_err)
        local base_ref = origin_err and base_branch or "origin/" .. base_branch
        git({ "log", "--reverse", "--format=%h %s%n%b%n---END-COMMIT---", base_ref .. "..HEAD" }, cwd,
          function(commits, commits_err)
            if commits_err then
              fail_progress("Could not read PR commit messages: " .. commits_err)
              return
            end

            if vim.trim(commits) == "" then
              stop_progress()
              notify("No commits found on " .. branch .. " compared with " .. base_ref, "warn")
              return
            end

            generate_metadata(build_context(branch, base_ref, commits), function(metadata, metadata_err)
              if metadata_err then
                fail_progress("Failed to generate PR metadata: " .. metadata_err)
                return
              end

              push_branch(cwd, branch, function(_, push_err)
                if push_err then
                  fail_progress("Failed to push branch: " .. push_err)
                  return
                end

                create_pr(cwd, branch, metadata)
              end)
            end)
          end)
      end)
    end)
  end)
end

return M
