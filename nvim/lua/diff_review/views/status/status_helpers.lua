--- Provides shared status-view helpers for notifications, git command building,
--- commit metadata, branch creation, and small popup/diff-mapping utilities.
--- Functions live here, off init.lua's local budget, and reach the active
--- status state via session.lua and sibling modules via direct requires.

local M = {}

local config = require("diff_review.infra.config")
local highlights = require("diff_review.infra.highlights")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local datetime = require("diff_review.integrations.datetime")
local git_backend = require("diff_review.git.git_backend")

local status_buffer = require("diff_review.views.status.status_buffer")
local source = require("diff_review.render.source")
-- diff_source_state edge kept lazy to avoid a load-time cycle.
local function diff_source_state() return require("diff_review.views.status.diff_source_state") end
-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
local repo_config = require("diff_review.git.repo_config")
local ui = require("diff_review.infra.ui")
local session = require("diff_review.session")

local function setup_bg_highlights()
  highlights.setup()
end

---@param message string
---@param title? string
local function notify_error(message, title)
  notifications.error(message, title)
end

---@return boolean
local function debug_notifications_enabled()
  local options = config.options or config.options or config.defaults
  return options.debug_notifications == true
end

---@param message string
---@param level? integer
---@param opts? table
local function notify_debug(message, level, opts)
  if not debug_notifications_enabled() then return end
  vim.notify(message, level or vim.log.levels.INFO, opts)
end

---@param title string
---@param failures DiffReviewGitFailure[]
local function notify_git_failures(title, failures)
  notifications.git_failures(title, failures)
end


---@param buf integer
---@param entry_id string
---@param head_line DiffReviewStatusHeadLine
---@return boolean
local function status_patch_head_line(buf, entry_id, head_line)
  local status = session.states and session.states[buf] or session.status
  if not (
    status
    and status.entries
    and head_line
    and head_line.segments
    and vim.api.nvim_buf_is_valid(buf)
  ) then return false end

  local target_line = nil
  for line, entry in pairs(status.entries) do
    if entry and entry.id == entry_id then
      target_line = line
      break
    end
  end
  if not target_line then return false end

  local text, segment_highlights = status_buffer.segment_line_parts(head_line.segments)
  status_buffer.with_writable(buf, function()
    vim.api.nvim_buf_set_lines(buf, target_line - 1, target_line, false, { text })
  end)
  status.entries[target_line] = head_line.entry
  if status.lines then status.lines[target_line] = text end

  vim.api.nvim_buf_clear_namespace(buf, ui.status_ns, target_line - 1, target_line)
  for _, highlight in ipairs(segment_highlights) do
    pcall(vim.api.nvim_buf_set_extmark, buf, ui.status_ns, target_line - 1, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    })
  end
  return true
end

---@param commit DiffReviewStatusCommit
---@return string?
local function status_commit_relative_date(commit)
  local value = commit.committed_at or commit.authored_at
  local relative = datetime.relative(value, { yesterday = false })
  if relative == "" then return nil end
  return relative
end

---@param commit DiffReviewStatusCommit
local function status_load_commit_files(commit)
  local status = session.status
  local cwd = status and status.cwd
  local buf = status and status.buf
  if not (cwd and buf and vim.api.nvim_buf_is_valid(buf)) then return end

  local source_state = diff_source_state()._status_ensure_commit_source_state(commit)
  if not source_state then return end
  local source_id = source_state.handle.id
  status.commit_file_cache = status.commit_file_cache or {}
  local cached = status.commit_file_cache[commit.oid]
  if cached and (cached.files_loaded or cached.files_loading) then
    commit.files = cached.files
    commit.files_loaded = cached.files_loaded
    commit.files_loading = cached.files_loading
    commit.files_error = cached.files_error
    return
  end

  local request_id = (status.commit_file_request_id or 0) + 1
  status.commit_file_request_id = request_id
  commit.files_loading = true
  commit.files_error = nil
  status.commit_file_cache[commit.oid] = {
    files = nil,
    files_loaded = false,
    files_loading = true,
    files_error = nil,
  }

  source.ensure_loaded(source_state, function(ok, err)
    local latest_status = session.status
    if not (latest_status and latest_status.buf and vim.api.nvim_buf_is_valid(latest_status.buf)) then return end
    if latest_status.cwd ~= cwd then return end

    latest_status.commit_file_cache = latest_status.commit_file_cache or {}
    local next_cache = {
      files = nil,
      files_loaded = ok == true,
      files_loading = false,
      files_error = nil,
    }
    local latest_source = latest_status.diff_source_registry
      and latest_status.diff_source_registry.source_by_id
      and latest_status.diff_source_registry.source_by_id[source_id]
      or source_state
    if not ok then
      next_cache.files_error = "Unable to load commit diff" .. (err and err ~= "" and (": " .. tostring(err)) or "")
    else
      next_cache.files = latest_source.metadata and latest_source.metadata.files or {}
    end
    latest_status.commit_file_cache[commit.oid] = next_cache

    for _, section in ipairs(latest_status.sections or {}) do
      for _, current_commit in ipairs(section.commits or {}) do
        if current_commit.oid == commit.oid then
          current_commit.files = next_cache.files
          current_commit.files_loaded = next_cache.files_loaded
          current_commit.files_loading = false
          current_commit.files_error = next_cache.files_error
        end
      end
    end

    render_orchestrator().render_status(latest_status.buf, nil, nil, { reuse_sections = true })
  end)
end

local function confirm(lines, on_yes, on_no)
  local body = vim.list_extend({}, lines)
  body[#body + 1] = ""
  body[#body + 1] = "  [y] yes    [n] no"
  local width = 32
  for _, line in ipairs(body) do
    width = math.max(width, #line + 4)
  end
  local buf, win = popup_window.open({
    relative = "editor",
    width = width,
    height = #body,
    title = "Confirm",
    filetype = "DiffReviewConfirm",
  })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, body)
  vim.bo[buf].modifiable = false
  local function close()
    popup_window.close(win)
  end
  local function cancel()
    close()
    if on_no then on_no() end
  end
  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true })
  for _, key in ipairs({ "n", "q", "<Esc>" }) do
    vim.keymap.set("n", key, cancel, { buffer = buf, nowait = true, silent = true })
  end
end

--- Centered single-line input popup (not vim.ui.input / snacks). `<CR>`
--- submits, `<Esc>`/`q` cancels (calling back with nil).
---@param prefix string prefilled text
---@param on_submit fun(name: string?)
local function prompt_branch_name(prefix, on_submit)
  local width = math.min(60, math.max(30, math.floor(vim.o.columns * 0.4)))
  local buf, win = popup_window.open({
    relative = "editor",
    width = width,
    height = 1,
    title = "New branch",
    filetype = "DiffReviewBranchPrompt",
  })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { prefix })
  local done = false
  local function finish(value)
    if done then return end
    done = true
    popup_window.close(win)
    on_submit(value)
  end
  vim.keymap.set({ "i", "n" }, "<CR>", function()
    vim.cmd("stopinsert")
    finish(vim.api.nvim_buf_get_lines(buf, 0, -1, false)[1] or "")
  end, { buffer = buf })
  vim.keymap.set("i", "<C-c>", function() vim.cmd("stopinsert"); finish(nil) end, { buffer = buf })
  vim.keymap.set("n", "<Esc>", function() finish(nil) end, { buffer = buf, nowait = true })
  vim.keymap.set("n", "q", function() finish(nil) end, { buffer = buf, nowait = true })
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buf, once = true, callback = function() finish(nil) end })
end

--- `bc`: prompt for a name (prefilled with the repo's branch prefix) and
--- create + switch to the new branch, then refresh the status view.
---@param buf integer
local function create_branch(buf)
  local status = session.states and session.states[buf] or session.status
  local cwd = status and status.cwd
  if not cwd then
    notify_error("Not a git repository", "DiffReview")
    return
  end
  local prefix = repo_config.branch_prefix(cwd)
  M.prompt_branch_name(prefix, function(name)
    if not name then return end
    name = vim.trim(name)
    if name == "" or name == prefix then return end
    git_backend.run_git_at_root_async(cwd, { "switch", "-c", name }, nil, function(result)
      if not result.ok then
        notify_error(
          "Create branch failed: " .. (result.output ~= "" and result.output or ("git exited " .. tostring(result.code))),
          "DiffReview"
        )
        return
      end
      vim.notify("Created branch " .. name, vim.log.levels.INFO, { title = "DiffReview" })
      if vim.api.nvim_buf_is_valid(buf) then
        session.status = status
        status.pr = nil
        status.about = nil
        render_orchestrator().render_status_or_notify(buf, nil, nil, { restore_initial_folds = true, refresh_pr = true, refresh_about = true })
      end
    end)
  end)
end

---@param diff_text string
---@param old_target_line integer
---@return integer?
local function closest_current_line_for_deleted_diff_line(diff_text, old_target_line)
  local old_line
  local new_line
  local in_hunk = false
  for diff_line in diff_text:gmatch("[^\n]+") do
    if diff_line:match("^@@") then
      old_line = tonumber(diff_line:match("%-(%d+)"))
      new_line = tonumber(diff_line:match("%+(%d+)"))
      in_hunk = old_line ~= nil and new_line ~= nil
    elseif in_hunk then
      local prefix = diff_line:sub(1, 1)
      if prefix == "-" then
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
      elseif prefix == "+" then
        new_line = new_line + 1
      else
        if old_line == old_target_line then
          return new_line
        end
        old_line = old_line + 1
        new_line = new_line + 1
      end
    end
  end
end

M.setup_bg_highlights = setup_bg_highlights
M.notify_error = notify_error
M.debug_notifications_enabled = debug_notifications_enabled
M.notify_debug = notify_debug
M.notify_git_failures = notify_git_failures
M.status_patch_head_line = status_patch_head_line
M.status_commit_relative_date = status_commit_relative_date
M.status_load_commit_files = status_load_commit_files
M.confirm = confirm
M.prompt_branch_name = prompt_branch_name
M.create_branch = create_branch
M.closest_current_line_for_deleted_diff_line = closest_current_line_for_deleted_diff_line

return M
