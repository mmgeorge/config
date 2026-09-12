--- Public entry points for forge: the open* commands that create or reuse a view buffer
--- and route to the status, PR, review, branch-diff, file-revision, and walkthrough views.
--- Re-exported by the plugin entry point, reaching shared state and render through the init seam.
local M = {}

local config = require("forge.infra.config")
local notifications = require("forge.infra.notifications")
local gh = require("forge.integrations.gh")
local git_backend = require("forge.git.git_backend")
local paths = require("forge.infra.paths")

local file_revision = require("forge.views.file_revision")
local session = require("forge.session")
local native_main_status

local function review_document() return require("forge.review_document") end

--- Emits an error notification with title through the notifications system.
---@param message string Error description message string.
---@param title? string Optional notification title string.
local function notify_error(message, title)
  return notifications.error(message, title)
end

local repo_relative = paths.repo_relative

--- Opens a read-only buffer with `file` as it exists at git revision `rev`.
---@param file string Repository file path string.
---@param rev string Git revision or commit hash string.
function M.open_file_revision(file, rev)
  file = vim.trim(tostring(file or ""))
  rev = vim.trim(tostring(rev or ""))
  if file == "" or rev == "" then
    notify_error("ForgeFileRevision requires a file and a revision", "Forge")
    return
  end
  git_backend.git_root_async(function(root, root_err)
    if not root then
      notify_error(root_err or "Not a git repository", "Forge")
      return
    end
    local relpath, rel_err = repo_relative(file, root)
    if not relpath then
      notify_error(rel_err or ("Path is outside the git root: " .. file), "Forge")
      return
    end
    file_revision.open({
      rev = rev,
      path = relpath,
      cwd = root,
      on_error = function(message)
        notify_error(message ~= "" and message or ("Git show failed for %s:%s"):format(rev, relpath), "Forge")
      end,
    })
  end)
end

---@class ForgeOpenPROptions
---@field cwd? string
---@field repo? string
---@field window? integer

--- Resolves the repository name string on a pull request descriptor.
---@param pr ForgeGhPR Pull request descriptor.
---@param opts? ForgeOpenPROptions Open PR options table.
---@return ForgeGhPR pr Updated pull request descriptor.
local function pr_with_resolved_repo(pr, opts)
  if pr.repo and pr.repo ~= "" then return pr end
  opts = opts or {}
  local repo = opts.repo and opts.repo ~= "" and opts.repo or gh.repo_from_pr_url(pr.url)
  if not repo then return pr end
  pr.repo = repo
  return pr
end

--- Converts a GitHub repository slug to the native review repository identity.
---@param pr ForgeGhPR Pull request descriptor.
---@param opts? ForgeOpenPROptions Open PR options table.
---@return table? repository
local function native_pr_repository(pr, opts)
  pr = pr_with_resolved_repo(pr, opts)
  local owner, name = tostring(pr.repo or ""):match("^([^/]+)/([^/]+)$")
  if not owner or not name then return nil end
  return { hostname = require("github.repo_cache").hostname(), owner = owner, name = name }
end

--- Opens the public pull-request overview through the native document owner.
---@param pr ForgeGhPR Pull request descriptor.
---@param opts? ForgeOpenPROptions Open PR options table.
---@return table? state Native review document state.
function M.open_pr(pr, opts)
  opts = opts or {}
  if not pr or not tonumber(pr.number) then return nil end
  local repository = native_pr_repository(pr, opts)
  if not repository then
    notify_error("Forge PR open requires an owner/repository identity", "Forge")
    return nil
  end
  local cwd = opts.cwd or (session.status and session.status.cwd) or vim.fn.getcwd()
  return review_document().open({ directory = cwd, repository = repository, number = tonumber(pr.number), window = opts.window })
end

--- Fetches pull request metadata by number and opens the PR overview buffer.
---@param number integer|string Pull request number or identifier.
---@param opts? ForgeOpenPROptions Open PR options table.
function M.open_pr_number(number, opts)
  opts = opts or {}
  local cwd = opts.cwd or vim.fn.getcwd()
  gh.pr_async(cwd, number, opts.repo, function(result)
    if not result.ok or not result.pr then
      notify_error(result.message or "Unable to load GitHub pull request", "Forge")
      return
    end
    M.open_pr(result.pr, { cwd = cwd, repo = opts.repo })
  end)
end

--- Opens a native batched pull-request review document.
---@param pr ForgeGhPR Pull request descriptor.
---@param opts? ForgeOpenPROptions Open PR options table.
---@return table? state Native review document state.
function M.open_review(pr, opts)
  opts = opts or {}
  if not pr or not tonumber(pr.number) then return nil end
  local repository = native_pr_repository(pr, opts)
  if not repository then
    notify_error("Forge review open requires an owner/repository identity", "Forge")
    return nil
  end
  local cwd = opts.cwd or (session.status and session.status.cwd) or vim.fn.getcwd()
  local state
  state = review_document().open({ directory = cwd, repository = repository, number = tonumber(pr.number), window = opts.window,
    on_open = function(opened)
      if opened == state and opened.active then review_document().begin_batched(opened) end
    end })
  return state
end

---@class ForgeBranchDiffOptions
---@field cwd? string
---@field file? string limit the diff to one repo-relative file

--- Opens a read-only diff comparison view between a branch and the working tree.
---@param branch string Comparison branch name or revision string.
---@param opts? ForgeBranchDiffOptions Options table with optional cwd or file filter.
function M.open_branch_diff(branch, opts)
  opts = opts or {}
  branch = vim.trim(branch or "")
  local file = opts.file and vim.trim(opts.file) or nil
  if file == "" then file = nil end
  if branch == "" then
    notify_error("ForgeBranchDiff requires a branch or revision", "ForgeBranchDiff")
    return
  end

  local function open_for_root(root, root_err)
    if not root then
      notify_error(root_err or "Not a git repository", "ForgeBranchDiff")
      return
    end

    local path = file
    if path and (path:match("^[/\\]") or path:match("^%a:[/\\]")) then
      local failure
      path, failure = repo_relative(path, root)
      if not path then notify_error(failure or "Path is outside the repository", "ForgeBranchDiff") return end
    end
    require("forge.status").open_comparison({ workspace = root, reference = branch, worktree = true,
      path = path, name = path and "ForgeBranchDiffFile" or "ForgeBranchDiff", filetype = "ForgeStatus" })
  end

  if opts.cwd then
    open_for_root(opts.cwd, nil)
    return
  end
  git_backend.git_root_async(open_for_root)
end

---@class ForgeCompactPreviewOptions
---@field cwd? string
---@field staged? boolean

--- Opens a compact or full unified diff preview buffer for staged or unstaged changes.
---@param opts? ForgeCompactPreviewOptions Compact preview options table.
function M.open_compact_preview(opts)
  opts = opts or {}
  local origin_window = vim.api.nvim_get_current_win()
  local origin_buffer = vim.api.nvim_win_get_buf(origin_window)
  local function origin_current()
    return vim.api.nvim_win_is_valid(origin_window)
      and vim.api.nvim_win_get_buf(origin_window) == origin_buffer
  end
  local function open_for_root(root, err)
    if not origin_current() then return end
    local cwd = root
    if not cwd then
      notify_error(err or "Not a git repository", "ForgeDiffCompactPreview")
      return
    end

    local command = git_backend.git_diff_command(cwd)
    if opts.staged then command[#command + 1] = "--cached" end
    git_backend.systemlist_async(command, function(output, code, stderr)
      if not origin_current() then return end
      if code ~= 0 then
        local message = vim.trim(stderr or "")
        notify_error(message ~= "" and message or "Unable to read git diff", "ForgeDiffCompactPreview")
        return
      end

      local compacted, was_compacted, metrics = require("git.diff").compact_lines(output or {})
      local lines = compacted == "" and { "No diff." } or vim.split(compacted, "\n", { plain = true })
      local buf = vim.api.nvim_create_buf(true, true)
      vim.bo[buf].bufhidden = "wipe"
      vim.bo[buf].buftype = "nofile"
      vim.bo[buf].swapfile = false
      vim.bo[buf].filetype = "diff"
      vim.bo[buf].modifiable = true
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
      vim.bo[buf].modifiable = false

      local name = ("ForgeDiffCompactPreview://%s/%s"):format(
        opts.staged and "staged" or "unstaged",
        was_compacted and "compact" or "full"
      )
      pcall(vim.api.nvim_buf_set_name, buf, name)

      require("forge.window_presentation").attach(buf, {
        number = false, relativenumber = false, signcolumn = "no", foldcolumn = "0", statuscolumn = " ",
        virtualedit = "all", wrap = true, linebreak = true, breakindent = false,
        conceallevel = 0, concealcursor = "", foldenable = true, foldlevel = 99, foldmethod = "manual",
      })

      local win = origin_window
      local ok, set_err = pcall(vim.api.nvim_win_set_buf, win, buf)
      if not ok then
        notify_error("ForgeDiffCompactPreview open failed: " .. tostring(set_err), "ForgeDiffCompactPreview")
        return
      end
      vim.b[buf].git_diff_compact_metrics = metrics
      vim.b[buf].git_diff_compacted = was_compacted
    end)
  end

  if opts.cwd then
    open_for_root(opts.cwd)
  else
    git_backend.git_root_async(open_for_root)
  end
end

--- Opens or focuses the primary Git status review buffer and triggers rendering.
---@param started_at? integer Command-entry monotonic time before lazy plugin loading, in nanoseconds.
function M.open(started_at)
  local command_started = started_at or vim.uv.hrtime()
  require("forge.startup_log").write("status.command.enter")
  local native = require("forge.status")
  local workspace, window = vim.fn.getcwd(), vim.api.nvim_get_current_win()
  if native_main_status and native_main_status.workspace == workspace and native_main_status.state.active
    and vim.api.nvim_buf_is_valid(native_main_status.state.replica.buffer)
  then
    vim.api.nvim_win_set_buf(window, native_main_status.state.replica.buffer)
    require("forge.startup_log").write("status.buffer.reused", {
      elapsed_ms = math.floor((vim.uv.hrtime() - command_started) / 1e6),
    })
    native.refresh(native_main_status.state)
    return native_main_status.state
  end
  if native_main_status and native_main_status.state.active then native.close(native_main_status.state) end
  local state = native.open({ workspace = workspace, window = window, started_at = command_started,
    name = config.options.status_buffer_name or "ForgeStatus", filetype = "ForgeStatus" })
  native_main_status = { workspace = workspace, state = state }
  return state
end

return M
