--- Installs the GitStatus/PR/review/diff buffer keymaps and the sticky hint-bar winbar,
--- resolving per-view command visibility and key bindings from the command vocabulary
--- and dispatching to init-owned actions.
---@class DiffReviewKeymapsModule
local M = {}

local config = require("diff_review.infra.config")
local gh = require("diff_review.integrations.gh")
local notifications = require("diff_review.infra.notifications")
local command_specs = require("diff_review.shared.command_specs")
local status_command_specs = command_specs.specs
local status_command_specs_by_id = command_specs.by_id

local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")

local function notify_error(message, title) return notifications.error(message, title) end
local function status_entry_under_cursor(...) return dr()._status_entry_under_cursor(...) end
local function render_pr_status(...) return dr()._render_pr_status(...) end
local function status_open_pr(...) return dr()._status_open_pr(...) end
local function status_show_help(...) return dr()._status_show_help(...) end

-- Forward-declared: status_command_visible references it before its definition.
local status_command_visible_for_view
local function render_status_or_notify(...) return dr()._render_status_or_notify(...) end
local function status_leave_visual_mode(...) return dr()._status_leave_visual_mode(...) end
local function status_entries_from_visual_selection(...) return dr()._status_entries_from_visual_selection(...) end
local function status_jump(...) return dr()._status_jump(...) end
local function status_remote_action(...) return dr()._status_remote_action(...) end
local function status_defer_prewarm_under_cursor(...) return dr()._status_defer_prewarm_under_cursor(...) end
local function status_toggle(...) return dr()._status_toggle(...) end
local function status_stage_entries(...) return dr()._status_stage_entries(...) end
local function status_stage(...) return dr()._status_stage(...) end
local function status_primary_key(...) return dr()._status_primary_key(...) end
local function status_open_about(...) return dr()._status_open_about(...) end
local function status_discard_entry_list(...) return dr()._status_discard_entry_list(...) end
local function status_discard(...) return dr()._status_discard(...) end
local function load_pr_diff(...) return dr()._load_pr_diff(...) end
local function status_unstage(...) return dr()._status_unstage(...) end
local function status_unstage_entries(...) return dr()._status_unstage_entries(...) end

local function status_keymap_config()
  local options = M.config or config.options or config.defaults
  local keymaps = options.keymaps or config.defaults.keymaps
  return vim.tbl_deep_extend("force", vim.deepcopy(config.defaults.keymaps.status), keymaps.status or {})
end

local function status_command_visible(spec)
  local view_kind = session.status and session.status.view_kind or "status"
  return status_command_visible_for_view(spec, view_kind)
end

function status_command_visible_for_view(spec, view_kind)
  return not spec.views or spec.views[view_kind] == true
end

local function status_keys_for(command_id)
  local spec = status_command_specs_by_id[command_id]
  local keymaps = spec and spec.keymap == "review" and dr()._review.keymap_config() or status_keymap_config()
  local key = keymaps[command_id]
  if key == false or key == nil then return {} end
  if type(key) == "table" then return key end
  return { key }
end

local function status_hint_segments(state)
  local view_kind = state and state.view_kind or (session.status and session.status.view_kind) or "status"
  local segments = {}
  local first = true
  local hint_command_ids = dr()._status_hint_command_ids_by_view[view_kind] or dr()._status_hint_command_ids_by_view.status
  for _, command_id in ipairs(hint_command_ids) do
    local spec = status_command_specs_by_id[command_id]
    if spec and spec.pinned and status_command_visible_for_view(spec, view_kind) then
      local key = status_primary_key(spec.id)
      if key ~= "" then
        if not first then
          segments[#segments + 1] = { " | ", "DiffReviewStatusHint" }
        end
        first = false
        segments[#segments + 1] = { key, "DiffReviewStatusHintKey" }
        segments[#segments + 1] = { " " .. spec.label, "DiffReviewStatusHint" }
      end
    end
  end
  return segments
end

local function status_hint_title(state)
  local view_kind = state.view_kind or "status"
  local options = M.config or config.options or config.defaults
  if view_kind == "pr" then
    local number = state.pr and state.pr.number
    return number and ("PR #" .. tostring(number)) or options.pr_buffer_name
  end
  if view_kind == "review" then
    local number = state.pr and state.pr.number
    return number and ("Review #" .. tostring(number)) or "Review"
  end
  if view_kind == "diff" then
    return state.diff_file and "GitBranchDiffFile" or "GitBranchDiff"
  end
  return options.status_buffer_name or config.defaults.status_buffer_name or "GitStatus"
end

local function status_hint_winbar(segments, title)
  local parts = { ("%%#DiffReviewStatusLabel#%s%%*"):format(tostring(title or ""):gsub("%%", "%%%%")) }
  if #segments > 0 then
    parts[#parts + 1] = "%="
  end
  for _, segment in ipairs(segments) do
    local text = tostring(segment[1] or ""):gsub("%%", "%%%%")
    local highlight = segment[2]
    if highlight and text ~= "" then
      parts[#parts + 1] = ("%%#%s#%s%%*"):format(highlight, text)
    else
      parts[#parts + 1] = text
    end
  end
  return table.concat(parts)
end

local function status_apply_hint_bar(buf, win)
  local state = session.states and session.states[buf] or (session.status and session.status.buf == buf and session.status) or nil
  if not state then return end
  local winbar = status_hint_winbar(status_hint_segments(state), status_hint_title(state))
  if win then
    if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf then
      vim.wo[win].winbar = winbar
    end
    return
  end
  for _, status_win in ipairs(vim.fn.win_findbuf(buf)) do
    if vim.api.nvim_win_is_valid(status_win) then
      vim.wo[status_win].winbar = winbar
    end
  end
end

local function status_clear_hint_bar(win)
  if vim.api.nvim_win_is_valid(win) then
    vim.wo[win].winbar = ""
  end
end

local function setup_status_keymaps(buf)
  require("github.repo_cache").enable_user_completion(buf)
  local opts = { buffer = buf, silent = true, nowait = true }
  local view_kind = session.status and session.status.view_kind or "status"
  local is_pr_view = view_kind == "pr"
  local function map(command_id, mode, callback, desc)
    local spec = status_command_specs_by_id[command_id]
    if spec and not status_command_visible(spec) then return end
    for _, key in ipairs(status_keys_for(command_id)) do
      local map_opts = vim.tbl_extend("force", opts, {
        desc = desc or (spec and spec.desc) or command_id,
      })
      local mapped
      mapped = function(...)
        if session.states and session.states[buf] then session.status = session.states[buf] end
        callback(...)
      end
      if view_kind == "review" then
        dr()._review.register_command_map(buf, command_id, mode, key, mapped, map_opts)
      else
        vim.keymap.set(mode, key, mapped, map_opts)
      end
    end
  end


  map("close", "n", function()
    local state = session.states and session.states[buf] or session.status
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
    if session.states then session.states[buf] = nil end
    if session.main_status == state then session.main_status = nil end
    if session.status == state then session.status = session.main_status end
  end)

  map("refresh", "n", function()
    if view_kind == "diff" then
      local status = session.states and session.states[buf] or session.status
      if status and status.diff_branch and status.cwd then
        session.status = status
        dr()._branch_diff.load(status.diff_branch, status.cwd, buf, status.diff_file)
      end
      return
    end
    if is_pr_view then
      local status = session.states and session.states[buf] or session.status
      if status and status.pr and status.cwd then
        session.status = status
        render_pr_status(status.pr, status.cwd, buf)
        load_pr_diff(status.pr, status.cwd, buf)
        dr()._pr_overview.load_comments(status.pr, status.cwd, buf)
        dr()._pr_overview.load_checks(status.pr, status.cwd, buf)
      end
      return
    end
    if session.status then
      session.status.pr = nil
      session.status.about = nil
    end
    render_status_or_notify(buf, nil, nil, {
      restore_initial_folds = true,
      refresh_pr = true,
      refresh_about = true,
    })
  end)

  map("toggle", "n", function()
    local status = session.states and session.states[buf] or session.status
    if status then session.status = status end
    if dr()._review.toggle_comment_fold(buf) then return end
    status_toggle(status_entry_under_cursor(status), status)
  end)

  map("collapse_parent", "n", function()
    if session.states and session.states[buf] then session.status = session.states[buf] end
    dr()._status_collapse_parent()
  end)

  map("visual_line_with_gutter", "n", function()
    dr()._start_diff_gutter_visual_line(buf)
  end)

  -- The review view gets its own action set (S/U/C/y/n/submit); the read-only
  -- PR and branch-diff views only get navigation commands.
  if view_kind == "review" then
    dr()._review.setup_keymaps(buf)
    map("browse", { "n", "x" }, function()
      local status = session.states and session.states[buf] or session.status
      local fragment = dr()._review.browse_fragment_under_cursor(buf)
      dr()._review.leave_visual()
      if not gh.browse_pr_changes(status and status.pr, fragment) then
        notify_error("Unable to open PR URL", "DiffReview")
      end
    end)
    local function review_open_action()
      if session.states and session.states[buf] then session.status = session.states[buf] end
      local entry = status_entry_under_cursor()
      if dr()._status_open_commit_message_under_cursor(entry) then return end
      status_jump(entry)
    end
    map("open", "n", review_open_action, "Jump to file")
    vim.schedule(function()
      if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
      if session.states and session.states[buf] then session.status = session.states[buf] end
      map("open", "n", review_open_action, "Jump to file")
    end)
    map("help", "n", status_show_help)
    return
  end
  if view_kind ~= "status" then
    map("browse", "n", function()
      local status = session.states and session.states[buf] or session.status
      if status and status.view_kind == "pr" then
        local url = dr()._pr_overview.url_under_cursor(buf)
        if url and gh.browse_url(url) then return end
      end
      local pr = status and status.pr
      if not gh.browse_pr(pr) then
        notify_error("Unable to open PR URL", "DiffReview")
      end
    end)
    if is_pr_view then
      map("comment", { "n", "x" }, function()
        dr()._pr_overview.add_standalone_comment(buf)
      end)
      map("sync", { "n", "i" }, function()
        dr()._review.sync_inline_comment_text(buf)
        dr()._pr_overview.sync_standalone_comments(buf)
        dr()._pr_edit.sync(buf)
      end)
    end
    map("review", "n", function()
      dr()._review.start(buf)
    end)
    map("open", "n", function()
      if session.states and session.states[buf] then session.status = session.states[buf] end
      if is_pr_view and dr()._pr_edit.toggle_draft_status_under_cursor(buf) then return end
      local entry = status_entry_under_cursor()
      if dr()._status_open_commit_message_under_cursor(entry) then return end
      status_jump(entry)
    end, "Jump to file")
    map("help", "n", status_show_help)
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = buf,
      callback = function()
        dr()._status_perf_span("status.autocmd_cursor_prewarm", buf, nil, function()
          if (M.config or config.options or config.defaults).status_cursor_prewarm == false then return end
          if session.states and session.states[buf] then session.status = session.states[buf] end
          status_defer_prewarm_under_cursor(buf)
        end)
      end,
    })
    return
  end

  map("stage", "n", function()
    status_stage(status_entry_under_cursor())
  end, "Stage hunk/file")
  map("stage", "x", function()
    local entries = status_entries_from_visual_selection()
    status_leave_visual_mode()
    status_stage_entries(entries, { preserve_cursor = true })
  end, "Stage selection")

  map("unstage", "n", function()
    status_unstage(status_entry_under_cursor())
  end, "Unstage hunk/file")
  map("unstage", "x", function()
    local entries = status_entries_from_visual_selection()
    status_leave_visual_mode()
    status_unstage_entries(entries, { preserve_cursor = true })
  end, "Unstage selection")

  map("discard", "n", function()
    status_discard(status_entry_under_cursor())
  end, "Discard hunk/file")
  map("discard", "x", function()
    local entries = status_entries_from_visual_selection()
    status_leave_visual_mode()
    status_discard_entry_list(entries, nil, { preserve_cursor = true })
  end, "Discard selection")

  map("open", "n", function()
    local entry = status_entry_under_cursor()
    if require("diff_review.views.walkthrough").jump_status_step(buf) then
      return
    elseif dr()._status_open_commit_message_under_cursor(entry) then
      return
    elseif entry and entry.kind == "pr" then
      status_open_pr(entry)
    elseif entry and entry.kind == "about" then
      status_open_about(entry)
    elseif entry and entry.kind == "load_more" then
      local status = session.states and session.states[buf] or session.status
      if status then
        -- Force-render the next chunk of deferred hunks from where the gate stopped, so
        -- each activation always reveals more even when a single hunk exceeds the budget.
        local chunk = 6
        local target = (tonumber(entry.load_more_from) or 1) + chunk - 1
        status.file_render_limits = status.file_render_limits or {}
        status.file_render_limits[entry.file_key] = math.max(status.file_render_limits[entry.file_key] or 0, target)
        session.status = status
        render_status_or_notify(buf, nil, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
      end
    else
      status_jump(entry)
    end
  end)

  local function jump_back_with_saved_view()
    local walkthrough = require("diff_review.views.walkthrough")
    walkthrough._debug_jump_event("status_jump_back_start", {
      buf = buf,
      view = walkthrough._jump_debug_snapshot(vim.fn.bufwinid(buf), buf),
    })
    vim.cmd("normal! \15")
    walkthrough._debug_jump_event("status_jump_back_after_ctrl_o", {
      buf = buf,
      view = walkthrough._jump_debug_snapshot(vim.fn.bufwinid(buf), buf),
    })
    local restored = walkthrough.restore_jump_return_view(buf, true)
    walkthrough._debug_jump_event("status_jump_back_after_restore", {
      buf = buf,
      restored = restored,
      view = walkthrough._jump_debug_snapshot(vim.fn.bufwinid(buf), buf),
    })
    vim.schedule(function()
      if vim.api.nvim_buf_is_valid(buf) then
        local scheduled_restored = walkthrough.restore_jump_return_view(buf, true)
        walkthrough._debug_jump_event("status_jump_back_scheduled_restore", {
          buf = buf,
          restored = scheduled_restored,
          view = walkthrough._jump_debug_snapshot(vim.fn.bufwinid(buf), buf),
        })
      end
    end)
  end
  for _, key in ipairs({ ",", "<C-o>" }) do
    vim.keymap.set("n", key, jump_back_with_saved_view,
      vim.tbl_extend("force", opts, { desc = "Jump back" }))
  end

  local function commit()
    require("diff_review.integrations.commit").commit({
      win = vim.api.nvim_get_current_win(),
      on_done = function()
        if vim.api.nvim_buf_is_valid(buf) then render_status_or_notify(buf) end
      end,
    })
  end
  map("commit", "n", commit)

  map("push", "n", function()
    status_remote_action(buf, "push")
  end)

  map("pull", "n", function()
    status_remote_action(buf, "pull")
  end)

  map("pr", "n", function()
    status_open_pr(status_entry_under_cursor())
  end)

  map("branch_create", "n", function()
    dr()._create_branch(buf)
  end)

  map("walkthrough", "n", function()
    require("diff_review.views.walkthrough").start(dr()._walkthrough_host(buf))
  end)

  map("review", "n", function()
    dr()._review.start(buf)
  end)

  map("help", "n", status_show_help)

  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      dr()._status_perf_span("status.autocmd_cursor_prewarm", buf, nil, function()
        if (M.config or config.options or config.defaults).status_cursor_prewarm == false then return end
        if session.states and session.states[buf] then session.status = session.states[buf] end
        status_defer_prewarm_under_cursor(buf)
      end)
    end,
  })
end

-- Public surface (init reaches these via seams).
M.status_command_visible_for_view = status_command_visible_for_view
M.status_hint_segments = status_hint_segments
M.status_hint_title = status_hint_title
M.status_hint_winbar = status_hint_winbar
M.status_apply_hint_bar = status_apply_hint_bar
M.status_clear_hint_bar = status_clear_hint_bar
M.status_keymap_config = status_keymap_config
M.status_command_visible = status_command_visible
M.status_keys_for = status_keys_for
M.setup_status_keymaps = setup_status_keymaps

return M
