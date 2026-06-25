--- Coordinates GitHub pull-request and AI about-summary lookup state for the status view.
--- Owns the async PR/about request lifecycle, the open-PR action, and the request-id race
--- guards that keep stale callbacks from clobbering a newer status snapshot.

local config = require("diff_review.infra.config")
local ai_commit = require("diff_review.integrations.ai_commit")
local gh = require("diff_review.integrations.gh")

local M = {}

local function dr() return require("diff_review") end
local session = require("diff_review.session")

-- Forward-declare so status_start_pr_lookup can capture the open action as an upvalue.
local status_open_pr

local function status_start_pr_lookup(cwd, buf, request_id)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local status = session.states and session.states[buf] or session.status
  if not (status and status.pr_request_id == request_id and status.pr_root == cwd and status.pr) then return end
  if status.pr.lookup_started then return end
  status.pr.lookup_started = true

  local function complete_pr_lookup(result)
    local latest_status = session.states and session.states[buf] or session.status
    if not (latest_status and latest_status.pr_request_id == request_id and latest_status.pr_root == cwd) then return end
    session.status = latest_status
    local pending_open = latest_status.pr and latest_status.pr.open_when_ready
    local pending_open_win = latest_status.pr and latest_status.pr.open_when_ready_win

    if result.ok and result.pr then
      latest_status.pr = { state = "ready", pr = result.pr, lookup_started = true }
    elseif result.ok and result.unavailable then
      latest_status.pr = { state = "unavailable", message = result.message, lookup_started = true }
    elseif result.ok then
      latest_status.pr = { state = "none", lookup_started = true }
    else
      local message = result.message or "Unable to fetch GitHub pull request"
      latest_status.pr = { state = "error", message = message, lookup_started = true }
      dr()._notify_error("GitHub PR lookup failed: " .. message)
    end

    if latest_status.head_values then
      latest_status.head_lines = dr()._status_build_head_lines(
        latest_status.head_values,
        latest_status.pr,
        latest_status.about,
        latest_status.issues
      )
    end
    if vim.api.nvim_buf_is_valid(buf) and latest_status.head_lines and latest_status.sections then
      if not dr()._status_patch_head_line(buf, "pr", dr()._status_pr_head_line(latest_status.pr)) then
        dr().render_status(buf, nil, nil, { reuse_sections = true })
      end
    end
    if pending_open then
      vim.schedule(function()
        if not (status_open_pr and vim.api.nvim_buf_is_valid(buf)) then return end
        local current_status = session.states and session.states[buf] or session.status
        if not (current_status and current_status.pr_request_id == request_id and current_status.pr_root == cwd) then return end
        session.status = current_status

        local function open_pending_pr()
          status_open_pr(nil)
        end

        if pending_open_win and vim.api.nvim_win_is_valid(pending_open_win) then
          local ok, win_buf = pcall(vim.api.nvim_win_get_buf, pending_open_win)
          if ok and win_buf == buf then
            vim.api.nvim_win_call(pending_open_win, open_pending_pr)
            return
          end
        end
        if vim.api.nvim_get_current_buf() == buf then open_pending_pr() end
      end)
    end
  end

  local options = dr().config or config.options or config.defaults
  if options.pr_lookup_mode == "mock-delay" then
    vim.defer_fn(function()
      complete_pr_lookup({ ok = true })
    end, math.max(1, tonumber(options.pr_mock_delay_ms) or 5000))
    return
  end

  gh.current_pr_async(cwd, complete_pr_lookup)
end

---@param cwd string
---@param buf integer
---@param force? boolean
---@return integer? request_id
local function status_ensure_pr_state(cwd, buf, force)
  session.status = session.status or {}
  local status = session.status
  if not force and status.pr_root == cwd and status.pr then return nil end

  status.pr_root = cwd
  status.pr = { state = "fetching" }
  status.pr_request_id = (status.pr_request_id or 0) + 1
  return status.pr_request_id
end

---@param sections DiffReviewStatusSection[]?
---@return boolean
local function status_has_changes(sections)
  for _, section in ipairs(sections or {}) do
    if #(section.files or {}) > 0 then return true end
  end
  return false
end

---@param cwd string
---@param buf integer
---@param has_changes boolean
---@param force? boolean
---@param allow_generation? boolean
local function status_ensure_about_state(cwd, buf, has_changes, force, allow_generation)
  session.status = session.status or {}
  local status = session.status
  if not has_changes then
    status.about_root = cwd
    status.about = { state = "none", waiters = {} }
    return
  end

  if not allow_generation and (dr().config or config.options or config.defaults).about_auto_generate == false then
    status.about_root = cwd
    status.about = { state = "none", waiters = {} }
    status.about_pending = nil
    return
  end

  local current = ai_commit.state()
  if not force and not allow_generation and status.about_root == cwd and status.about_pending and status.about then return end
  if not force and status.about_root == cwd and status.about and current == status.about then return end

  status.about_root = cwd
  status.about = current and current.cwd == cwd and current or { state = "generating", cwd = cwd, waiters = {} }
  status.about_request_id = (status.about_request_id or 0) + 1
  local request_id = status.about_request_id
  if allow_generation then
    dr()._status_patch_about_line(buf)
  end

  local function start_generation()
    local latest_status = session.states and session.states[buf] or session.status
    if not (latest_status and latest_status.about_request_id == request_id and latest_status.about_root == cwd) then return end
    latest_status.about_pending = nil
    ai_commit.ensure(cwd, { force = force }, function(result)
      latest_status = session.states and session.states[buf] or session.status
      if not (latest_status and latest_status.about_request_id == request_id and latest_status.about_root == cwd) then return end
      session.status = latest_status
      latest_status.about = result
      latest_status.about_pending = nil
      if latest_status.head_values then
        latest_status.head_lines = dr()._status_build_head_lines(
          latest_status.head_values,
          latest_status.pr,
          latest_status.about,
          latest_status.issues
        )
      end
      if vim.api.nvim_buf_is_valid(buf) and latest_status.head_lines and latest_status.sections then
        if not dr()._status_patch_about_line(buf) then
          dr().render_status(buf, nil, nil, { reuse_sections = true })
        end
      end
    end)
  end

  local delay = allow_generation and 0 or ((dr().config or config.options or config.defaults).about_auto_generate_delay_ms or 0)
  if delay > 0 then
    status.about_pending = true
    vim.defer_fn(start_generation, delay)
    return
  end

  start_generation()
end

status_open_pr = function(entry)
  local pr = entry and entry.pr
  if not pr and session.status and session.status.pr and session.status.pr.state == "ready" then
    pr = session.status.pr.pr
  end
  if not pr then
    local pr_state = session.status and session.status.pr
    if pr_state and pr_state.state == "fetching" then
      pr_state.open_when_ready = true
      pr_state.open_when_ready_win = vim.api.nvim_get_current_win()
      return
    end
    if pr_state and pr_state.state == "error" then
      dr()._notify_error("GitHub PR lookup failed: " .. (pr_state.message or "Unable to fetch GitHub pull request"), "DiffReview")
      return
    end
    if pr_state and pr_state.state == "unavailable" then
      vim.notify(pr_state.message or "GitHub PR lookup is unavailable", vim.log.levels.INFO, { title = "DiffReview" })
      return
    end
    dr()._status_confirm_create_pr(function()
      require("github.open_pr").open()
    end)
    return
  end
  dr().open_pr(pr, { cwd = session.status and session.status.cwd or nil })
end

M.status_start_pr_lookup = status_start_pr_lookup
M.status_ensure_pr_state = status_ensure_pr_state
M.status_has_changes = status_has_changes
M.status_ensure_about_state = status_ensure_about_state
M.open_pr = status_open_pr

return M
