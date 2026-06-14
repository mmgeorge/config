local M = {}
local repo_cache = require("github.repo_cache")

local default_base_branch = "main"
local state_cache = nil
local state_path_for_test = nil
local pr_context_limit = 60000

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

local function state_path()
  return state_path_for_test
end

---@param cwd string
---@return string
local function repo_key(cwd)
  local key = vim.fs.normalize(cwd)
  if vim.fn.has("win32") == 1 then
    key = key:lower()
  end
  return key
end

---@return table
local function read_state()
  if state_cache then
    return state_cache
  end

  local path = state_path()
  if not path then
    state_cache = { repos = {} }
    return state_cache
  end
  if vim.uv.fs_stat(path) == nil then
    state_cache = { repos = {} }
    return state_cache
  end

  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then
    state_cache = { repos = {} }
    return state_cache
  end

  local decoded_ok, decoded = pcall(vim.json.decode, table.concat(lines, "\n"))
  if not decoded_ok or type(decoded) ~= "table" then
    state_cache = { repos = {} }
    return state_cache
  end

  if type(decoded.repos) ~= "table" then
    decoded.repos = {}
  end
  state_cache = decoded
  return state_cache
end

---@param state table
---@return string?
local function write_state(state)
  local path = state_path()
  if not path then return "No GitHubOpenPR test state path configured" end
  local directory = vim.fs.dirname(path)
  local mkdir_ok, mkdir_err = pcall(vim.fn.mkdir, directory, "p")
  if not mkdir_ok or mkdir_err == 0 then
    return "Could not create state directory: " .. directory
  end

  local encode_ok, encoded = pcall(vim.json.encode, state)
  if not encode_ok then
    return "Could not encode state"
  end

  local write_ok, write_err = pcall(vim.fn.writefile, { encoded }, path)
  if not write_ok or write_err ~= 0 then
    return "Could not write state: " .. path
  end

  return nil
end

---@param cwd string
---@return string?
local function get_stored_base_branch(cwd)
  if not state_path_for_test then
    return repo_cache.get_base_branch(cwd)
  end
  local state = read_state()
  local branch = state.repos[repo_key(cwd)]
  if type(branch) == "string" and branch ~= "" then
    return branch
  end
  return nil
end

---@param cwd string
---@param branch string
local function set_base_branch(cwd, branch)
  if not state_path_for_test then
    local err = repo_cache.set_base_branch(cwd, branch)
    if err then notify(err, "warn") end
    return
  end
  local state = read_state()
  state.repos[repo_key(cwd)] = branch
  local err = write_state(state)
  if err then
    notify(err, "warn")
  end
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

---@param content string?
---@return string?
local function normalize_pr_metadata_content(content)
  if type(content) ~= "string" then return nil end
  content = vim.trim(content):gsub("\r\n", "\n")
  content = content:gsub("^```%w*\n", ""):gsub("\n```$", "")
  return vim.trim(content)
end

local function decode_pr_metadata(content)
  content = normalize_pr_metadata_content(content)
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

local function build_context(branch, base_ref, commits)
  local truncated_commits, commits_truncated = truncate_at_line(commits, pr_context_limit)
  local sections = {
    "Generate metadata for a draft GitHub PR.",
    "Base branch: " .. base_ref:gsub("^origin/", ""),
    "Comparison ref used for commit messages: " .. base_ref,
    "Head branch: " .. branch,
  }

  if commits_truncated then
    table.insert(sections, "Some commit message context was truncated to stay within the model request size.")
  end

  table.insert(sections, "Commit messages:\n```text\n" .. truncated_commits .. "\n```")
  return table.concat(sections, "\n\n")
end

---@param raw_branch string
---@return string?
local function normalized_base_branch(raw_branch)
  local branch = vim.trim(raw_branch)
  branch = branch:gsub("^remotes/", ""):gsub("^origin/", "")
  if branch == "" or branch == "HEAD" or branch:match("^HEAD%s*%->") then
    return nil
  end
  return branch
end

---@param output string
---@param current_branch string
---@return string[]
local function parse_base_branches(output, current_branch)
  local seen = {}
  local branches = {}

  for line in output:gmatch("[^\r\n]+") do
    local branch = normalized_base_branch(line)
    if branch and branch ~= current_branch and not seen[branch] then
      seen[branch] = true
      branches[#branches + 1] = branch
    end
  end

  table.sort(branches)
  return branches
end

---@param branches string[]
---@return string?
local function default_base_branch_from_branches(branches)
  for _, branch in ipairs(branches) do
    if branch == default_base_branch then return branch end
  end
  for _, branch in ipairs(branches) do
    if branch == "master" then return branch end
  end
  return nil
end

---@param lines string[]
---@param on_yes fun()
---@param on_no fun()
local function confirm(lines, on_yes, on_no)
  local body = vim.list_extend({}, lines)
  body[#body + 1] = ""
  body[#body + 1] = "  [y] yes    [n] no"
  local width = 32
  for _, line in ipairs(body) do
    width = math.max(width, #line + 4)
  end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, body)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = #body,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - #body) / 2),
    style = "minimal",
    border = "rounded",
    title = " GithubOpenPR ",
    title_pos = "center",
  })

  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end

  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true, desc = "Use base branch" })

  vim.keymap.set("n", "n", function()
    close()
    on_no()
  end, { buffer = buf, nowait = true, silent = true, desc = "Select base branch" })

  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true, desc = "Cancel PR creation" })
  end
end

---@param branches string[]
---@param current_base_branch string
---@param callback fun(branch: string?)
local function pick_base_branch(branches, current_base_branch, callback)
  if #branches == 0 then
    notify("No base branches available to select", "warn")
    callback(nil)
    return
  end

  if _G.Snacks and Snacks.picker and type(Snacks.picker.pick) == "function" then
    Snacks.picker.pick({
      title = "Select PR base branch",
      items = vim.tbl_map(function(branch)
        return {
          text = branch,
          branch = branch,
        }
      end, branches),
      format = function(item)
        return {
          { item.text, item.branch == current_base_branch and "Function" or "Identifier" },
        }
      end,
      confirm = function(picker, item)
        if picker and picker.close then picker:close() end
        callback(item and item.branch or nil)
      end,
    })
    return
  end

  vim.ui.select(branches, {
    prompt = "Select PR base branch",
  }, callback)
end

---@param cwd string
---@param current_branch string
---@param callback fun(branch: string?)
local function choose_base_branch(cwd, current_branch, callback)
  local stored_base_branch = get_stored_base_branch(cwd)
  if stored_base_branch then
    confirm({ "Base: " .. stored_base_branch }, function()
      callback(stored_base_branch)
    end, function()
      git({ "branch", "--list", "--all", "--format=%(refname:short)" }, cwd, function(output, branches_err)
        if branches_err then
          fail_progress("Could not list base branches: " .. branches_err)
          callback(nil)
          return
        end

        pick_base_branch(parse_base_branches(output, current_branch), stored_base_branch, function(selected)
          if not selected or selected == "" then
            return callback(nil)
          end

          set_base_branch(cwd, selected)
          callback(selected)
        end)
      end)
    end)
    return
  end

  git({ "branch", "--list", "--all", "--format=%(refname:short)" }, cwd, function(output, branches_err)
    if branches_err then
      fail_progress("Could not list base branches: " .. branches_err)
      callback(nil)
      return
    end

    local branches = parse_base_branches(output, current_branch)
    local current_base_branch = default_base_branch_from_branches(branches)
    confirm({ "Base: " .. (current_base_branch or "") }, function()
      callback(current_base_branch)
    end, function()
      pick_base_branch(branches, current_base_branch or "", function(selected)
        if not selected or selected == "" then
          return callback(nil)
        end

        set_base_branch(cwd, selected)
        callback(selected)
      end)
    end)
  end)
end

local function generate_metadata(context, callback)
  local ok_adapters, adapters = pcall(require, "ai.adapters")
  local selected = ok_adapters and adapters.get() or {}
  local model = selected.pr_create or selected.commit or "copilot,model=gpt-4.1"

  show_progress("Generating PR title and description with " .. model .. "...")
  require("ai").generate({
    model = model,
    system = pr_system_prompt,
    prompt = context,
  }, function(result)
    if not result.ok then
      callback(nil, result.error or "Unable to generate PR metadata")
      return
    end
    local metadata, err = decode_pr_metadata(result.content)
    callback(metadata, err)
  end)
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

local function create_pr(cwd, branch, selected_base_branch, metadata)
  show_progress("Creating draft PR...")
  run({
    "gh",
    "pr",
    "create",
    "--draft",
    "--base",
    selected_base_branch,
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

    local pr_ref = output:match("https?://%S+") or vim.trim(output)
    show_progress("Opening PR in nvim...")
    require("diff_review.gh").pr_async(cwd, pr_ref, nil, function(result)
      if not result.ok or not result.pr then
        fail_progress("Created PR, but failed to open PR view: " .. (result.message or "Unable to load GitHub pull request"))
        return
      end

      require("diff_review").open_pr(result.pr, { cwd = cwd })
      complete_progress("Created draft PR: " .. pr_ref)
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

      choose_base_branch(cwd, branch, function(selected_base_branch)
        if not selected_base_branch then return end

        if branch == selected_base_branch then
          fail_progress("Cannot create a PR from " .. selected_base_branch)
          return
        end

        git({ "rev-parse", "--verify", "origin/" .. selected_base_branch }, cwd, function(_, origin_err)
          local base_ref = origin_err and selected_base_branch or "origin/" .. selected_base_branch
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

                  create_pr(cwd, branch, selected_base_branch, metadata)
                end)
              end)
            end)
        end)
      end)
    end)
  end)
end

M._parse_base_branches_for_test = parse_base_branches
M._set_base_branch_for_test = function(branch, cwd)
  if cwd then
    set_base_branch(cwd, branch)
  end
end
M._get_base_branch_for_test = function(cwd)
  return get_stored_base_branch(cwd)
end
M._default_base_branch_from_branches_for_test = default_base_branch_from_branches
M._set_state_path_for_test = function(path)
  state_path_for_test = path
  state_cache = nil
end
M._choose_base_branch_for_test = choose_base_branch

return M
