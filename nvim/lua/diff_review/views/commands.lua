--- Public entry points for diff_review: the open* commands that create or reuse a view buffer
--- and route to the status, PR, review, branch-diff, file-revision, and walkthrough views.
--- Re-exported by the plugin entry point, reaching shared state and render through the init seam.
local M = {}

local config = require("diff_review.infra.config")
local notifications = require("diff_review.infra.notifications")
-- keymaps edge kept lazy to avoid a load-time cycle.
local function keymaps() return require("diff_review.shared.keymaps") end
local gh = require("diff_review.integrations.gh")
local git_backend = require("diff_review.git.git_backend")
local paths = require("diff_review.infra.paths")

-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
-- pr_overview edge kept lazy to avoid a load-time cycle.
local function pr_overview() return require("diff_review.views.pr.pr_overview") end
-- state edge kept lazy to avoid a load-time cycle.
local function state_mod() return require("diff_review.views.status.state") end
local pr_edit = require("diff_review.views.pr.pr_edit")
-- review edge kept lazy to avoid a load-time cycle.
local function review() return require("diff_review.views.pr.review") end
local file_revision = require("diff_review.views.file_revision")
local branch_diff = require("diff_review.views.branch_diff")
local status_keys = require("diff_review.views.status.status_keys")
local window_options = require("diff_review.views.status.window_options")
-- status_head edge kept lazy to avoid a load-time cycle.
local function status_head() return require("diff_review.views.status.status_head") end
-- entry_nav edge kept lazy to avoid a load-time cycle.
local function entry_nav() return require("diff_review.views.status.entry_nav") end
-- fold_state edge kept lazy to avoid a load-time cycle.
local function fold_state() return require("diff_review.views.status.fold_state") end
local status_helpers = require("diff_review.views.status.status_helpers")
local inventory = require("diff_review.infra.inventory")
local session = require("diff_review.session")

local function notify_error(message, title)
  return notifications.error(message, title)
end

local repo_relative = paths.repo_relative

---Open a read-only buffer with `file` as it exists at git revision `rev`.
---Entry point for :GitFileRevision.
---@param file string
---@param rev string
function M.open_file_revision(file, rev)
  file = vim.trim(tostring(file or ""))
  rev = vim.trim(tostring(rev or ""))
  if file == "" or rev == "" then
    notify_error("GitFileRevision requires a file and a revision", "DiffReview")
    return
  end
  git_backend.git_root_async(function(root, root_err)
    if not root then
      notify_error(root_err or "Not a git repository", "DiffReview")
      return
    end
    local relpath, rel_err = repo_relative(file, root)
    if not relpath then
      notify_error(rel_err or ("Path is outside the git root: " .. file), "DiffReview")
      return
    end
    file_revision.open({
      rev = rev,
      path = relpath,
      cwd = root,
      on_error = function(message)
        notify_error(message ~= "" and message or ("Git show failed for %s:%s"):format(rev, relpath), "DiffReview")
      end,
    })
  end)
end
--- Narrow interface handed to diff_review.walkthrough so the module never
--- reaches into init internals.
---@param buf integer
---@return DiffReviewWalkthroughHost
function M._walkthrough_host(buf)
  local resolved_root

  local function status_state()
    return session.states and session.states[buf] or session.status
  end

  --- Resolve and cache the root shared by walkthrough artifact and inventory reads.
  ---@param cb fun(root?: string, err?: string)
  local function resolve_root_async(cb)
    if resolved_root then
      cb(resolved_root, nil)
      return
    end
    local state = status_state()
    local cwd = state and state.cwd
    if not cwd or cwd == "" then
      cb(nil, "No repository loaded")
      return
    end
    git_backend.git_root_at_async(cwd, function(root, root_err)
      if root then resolved_root = root end
      cb(root, root_err)
    end)
  end

  return {
    buf = buf,
    cwd = function()
      local state = status_state()
      return resolved_root or (state and state.cwd)
    end,
    resolve_root_async = resolve_root_async,
    get_state = function()
      return status_state()
    end,
    file_key = status_keys.file_key,
    hunk_key = status_keys.hunk_key,
    set_folded = function(key, folded)
      local state = session.states and session.states[buf] or session.status
      if state then session.status = state end
      fold_state()._set_status_folded(key, folded, state)
    end,
    rerender = function(target_id, fallback_line)
      render_orchestrator().render_status_or_notify(buf, target_id, fallback_line, { reuse_sections = true })
    end,
    git_list_async = git_backend.systemlist_async,
    inventory_async = config.options.walkthrough_inventory == "sem" and function(cb)
      resolve_root_async(function(root, root_err)
        if not root then
          if root_err and root_err ~= "" then
            notify_error("Walkthrough inventory failed: " .. root_err, "DiffReview")
          end
          cb({ rows = {} })
          return
        end
        local state = status_state()
        if not state then
          cb({ rows = {} })
          return
        end
        inventory.compute_async({
          cwd = root,
          sections = state.sections or {},
          run_text_async = git_backend.system_text_async,
          sem_available = function()
            return vim.fn.executable("sem") == 1
          end,
          repo_relative = function(filename)
            return repo_relative(filename, root)
          end,
        }, function(result)
          if result and result.error and result.error ~= "" then
            notify_error("Walkthrough inventory failed: " .. result.error, "DiffReview")
          end
          cb(result or { rows = {} })
        end)
      end)
    end or nil,
  }
end

---@class DiffReviewOpenPROptions
---@field cwd? string
---@field repo? string

---@param pr DiffReviewGhPR
---@param opts? DiffReviewOpenPROptions
---@return DiffReviewGhPR
local function pr_with_resolved_repo(pr, opts)
  if pr.repo and pr.repo ~= "" then return pr end
  opts = opts or {}
  local repo = opts.repo and opts.repo ~= "" and opts.repo or gh.repo_from_pr_url(pr.url)
  if not repo then return pr end
  pr.repo = repo
  return pr
end

---@param pr DiffReviewGhPR
---@param opts? DiffReviewOpenPROptions
---@return integer? buf
function M.open_pr(pr, opts)
  opts = opts or {}
  if not pr then return nil end
  status_helpers.setup_bg_highlights()

  pr = pr_with_resolved_repo(pr, opts)
  local cwd = opts.cwd or (session.status and session.status.cwd) or vim.fn.getcwd()
  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "GitStatus"
  local options = config.options or config.options or config.defaults
  local name = ("%s://%s"):format(options.pr_buffer_name, pr.number or "current")
  if pr.repo and pr.repo ~= "" then name = ("%s/%s"):format(name, pr.repo:gsub("/", "%%2F")) end
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  local state = {
    view_kind = "pr",
    buf = buf,
    cwd = cwd,
    pr = pr,
    folds = {},
    lines = {},
    entries = {},
    highlights = {},
    line_highlights = {},
    extmarks = {},
    boundary_lines = {},
    sections = nil,
    fancy_rows = {},
  }
  state.pr_standalone_comments = {}
  state.pr_regular_comments = {}
  state.review_comments = pr_overview().editable_comments(state)
  if pr.repo and pr.repo ~= "" then vim.b[buf].github_repo = pr.repo end
  session.status = state
  state_mod().attach(buf, state)
  keymaps().setup_status_keymaps(buf)
  pr_edit.attach(buf)
  status_head().github_load_repo_metadata(cwd, pr.repo)

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview PR open failed: " .. tostring(err))
    return nil
  end
  window_options.apply(win, state)
  vim.wo[win].foldcolumn = "0"

  render_orchestrator().render_pr_status(pr, cwd, buf)
  render_orchestrator().load_pr_diff(pr, cwd, buf)
  pr_overview().load_comments(pr, cwd, buf)
  pr_overview().load_checks(pr, cwd, buf)
  return buf
end

---@param number integer|string
---@param opts? DiffReviewOpenPROptions
function M.open_pr_number(number, opts)
  opts = opts or {}
  local cwd = opts.cwd or vim.fn.getcwd()
  gh.pr_async(cwd, number, opts.repo, function(result)
    if not result.ok or not result.pr then
      notify_error(result.message or "Unable to load GitHub pull request", "DiffReview")
      return
    end
    M.open_pr(result.pr, { cwd = cwd, repo = opts.repo })
  end)
end


--- Open a PR review buffer (":or"): the PR title, an editable review summary,
--- and the changed files split into Unviewed/Viewed sections.
---@param pr DiffReviewGhPR
---@param opts? DiffReviewOpenPROptions
---@return integer? buf
function M.open_review(pr, opts)
  opts = opts or {}
  if not pr then return nil end
  status_helpers.setup_bg_highlights()
  pr = pr_with_resolved_repo(pr, opts)
  local cwd = opts.cwd or (session.status and session.status.cwd) or vim.fn.getcwd()
  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "GitStatus"
  local options = config.options or config.options or config.defaults
  local name = ("%sReview://%s"):format(options.pr_buffer_name, pr.number or "current")
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  local state = {
    view_kind = "review",
    buf = buf,
    cwd = cwd,
    pr = pr,
    commit_id = pr.headRefOid or "",
    diff_text = "",
    review_viewed = {},
    review_viewed_hunks = {},
    review_comment_text = "",
    review_comments = {},
    folds = {},
    lines = {},
    entries = {},
    highlights = {},
    line_highlights = {},
    extmarks = {},
    boundary_lines = {},
    sections = nil,
    fancy_rows = {},
  }
  if pr.repo and pr.repo ~= "" then vim.b[buf].github_repo = pr.repo end
  review().load_draft(state)
  review().normalize_comments(state)
  session.status = state
  state_mod().attach(buf, state)
  keymaps().setup_status_keymaps(buf)
  review().attach(buf)
  status_head().github_load_repo_metadata(cwd, pr.repo)

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview review open failed: " .. tostring(err))
    return nil
  end
  window_options.apply(win, state)
  vim.wo[win].foldcolumn = "0"

  entry_nav()._status_set_plain_lines(buf, { "Loading GitHub pending review()..." })
  review().load_remote_before_open(buf, function()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    review().render(buf)
    review().load_diff(pr, cwd, buf)
  end)
  return buf
end



---@class DiffReviewBranchDiffOptions
---@field cwd? string
---@field file? string limit the diff to one repo-relative file

--- Open a read-only diff of the working tree against a branch or revision
--- (":GitBranchDiff <branch>", ":GitBranchDiffFile <file> <branch>"): the
--- diff part of the status view without staging, commit, or remote actions.
---@param branch string
---@param opts? DiffReviewBranchDiffOptions
function M.open_branch_diff(branch, opts)
  opts = opts or {}
  branch = vim.trim(branch or "")
  local file = opts.file and vim.trim(opts.file) or nil
  if file == "" then file = nil end
  if branch == "" then
    notify_error("GitBranchDiff requires a branch or revision", "GitBranchDiff")
    return
  end
  status_helpers.setup_bg_highlights()

  local function open_for_root(root, root_err)
    if not root then
      notify_error(root_err or "Not a git repository", "GitBranchDiff")
      return
    end

    local buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    vim.bo[buf].filetype = "GitStatus"
    local name = "GitBranchDiff"
    if not pcall(vim.api.nvim_buf_set_name, buf, name) then
      pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
    end

    local state = {
      view_kind = "diff",
      buf = buf,
      cwd = root,
      diff_branch = branch,
      diff_file = file,
      folds = {},
      lines = {},
      entries = {},
      highlights = {},
      line_highlights = {},
      extmarks = {},
      boundary_lines = {},
      sections = nil,
      fancy_rows = {},
    }
    session.status = state
    state_mod().attach(buf, state)
    keymaps().setup_status_keymaps(buf)

    local win = vim.api.nvim_get_current_win()
    local ok, set_err = pcall(vim.api.nvim_win_set_buf, win, buf)
    if not ok then
      notify_error("GitBranchDiff open failed: " .. tostring(set_err), "GitBranchDiff")
      return
    end
    window_options.apply(win, state)
    vim.wo[win].foldcolumn = "0"

    branch_diff.render(branch, root, buf, nil, file)
    branch_diff.load(branch, root, buf, file)
  end

  if opts.cwd then
    open_for_root(opts.cwd, nil)
    return
  end
  git_backend.git_root_async(open_for_root)
end

---@class DiffReviewCompactPreviewOptions
---@field cwd? string
---@field staged? boolean

---@param opts? DiffReviewCompactPreviewOptions
function M.open_compact_preview(opts)
  opts = opts or {}
  local function open_for_root(root, err)
    local cwd = root
    if not cwd then
      notify_error(err or "Not a git repository", "GitDiffCompactPreview")
      return
    end

    local command = git_backend.git_diff_command(cwd)
    if opts.staged then command[#command + 1] = "--cached" end
    git_backend.systemlist_async(command, function(output, code, stderr)
      if code ~= 0 then
        local message = vim.trim(stderr or "")
        notify_error(message ~= "" and message or "Unable to read git diff", "GitDiffCompactPreview")
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

      local name = ("GitDiffCompactPreview://%s/%s"):format(
        opts.staged and "staged" or "unstaged",
        was_compacted and "compact" or "full"
      )
      pcall(vim.api.nvim_buf_set_name, buf, name)

      local win = vim.api.nvim_get_current_win()
      local ok, set_err = pcall(vim.api.nvim_win_set_buf, win, buf)
      if not ok then
        notify_error("GitDiffCompactPreview open failed: " .. tostring(set_err), "GitDiffCompactPreview")
        return
      end
      window_options.apply(win, nil)
      vim.wo[win].foldcolumn = "0"
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

--- Open a standalone, Neogit-style DiffReview status buffer.
function M.open()
  status_helpers.setup_bg_highlights()
  local buf = session.status and session.status.buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then
    buf = vim.api.nvim_create_buf(true, false)
    vim.bo[buf].bufhidden = "hide"
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].swapfile = false
    vim.bo[buf].filetype = "GitStatus"
    pcall(vim.api.nvim_buf_set_name, buf, (config.options or config.options).status_buffer_name)
    session.main_status = {
      view_kind = "status",
      buf = buf,
      folds = {},
      lines = {},
      entries = {},
      highlights = {},
      line_highlights = {},
      extmarks = {},
      boundary_lines = {},
      sections = nil,
      fancy_rows = {},
    }
    session.status = session.main_status
    state_mod().attach(buf, session.main_status)
    keymaps().setup_status_keymaps(buf)
  else
    session.main_status = session.states and session.states[buf] or session.main_status or session.status
    session.status = session.main_status
  end

  local win = vim.api.nvim_get_current_win()
  local ok, err = pcall(vim.api.nvim_win_set_buf, win, buf)
  if not ok then
    notify_error("DiffReview open failed: " .. tostring(err))
    return
  end
  window_options.apply(win, session.main_status)
  vim.wo[win].foldcolumn = "0"
  render_orchestrator().render_status_or_notify(buf)
end

return M
