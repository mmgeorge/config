--- Installs the GitStatus/PR/review/diff buffer keymaps and the sticky hint-bar winbar,
--- resolving per-view command visibility and key bindings from the command vocabulary
--- and dispatching to init-owned actions().
---@class DiffReviewKeymapsModule
local M = {}

local config = require("diff_review.infra.config")
local gh = require("diff_review.integrations.gh")
local notifications = require("diff_review.infra.notifications")
local command_specs = require("diff_review.shared.command_specs")
local status_command_specs = command_specs.specs
local status_command_specs_by_id = command_specs.by_id

-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
-- review edge kept lazy to avoid a load-time cycle.
local function review() return require("diff_review.views.pr.review") end
-- pr_overview edge kept lazy to avoid a load-time cycle.
local function pr_overview() return require("diff_review.views.pr.pr_overview") end
local pr_edit = require("diff_review.views.pr.pr_edit")
-- commit_view edge kept lazy to avoid a load-time cycle.
local function commit_view() return require("diff_review.views.status.commit_view") end
-- actions edge kept lazy to avoid a load-time cycle.
local function actions() return require("diff_review.views.status.actions") end
local branch_diff = require("diff_review.views.branch_diff")
local diff_buffer = require("diff_review.views.diff_buffer")
local commands = require("diff_review.views.commands")
local status_keys = require("diff_review.views.status.status_keys")
local pr_state = require("diff_review.views.status.pr_state")
-- entry_nav requires keymaps back for the hint bar, so this edge stays lazy to avoid
-- a load-time circular require (shared -> views).
local function entry_nav() return require("diff_review.views.status.entry_nav") end
local status_helpers = require("diff_review.views.status.status_helpers")
local trace = require("diff_review.infra.perf_trace")
local session = require("diff_review.session")

local function notify_error(message, title) return notifications.error(message, title) end
local function status_entry_under_cursor(...) return entry_nav()._status_entry_under_cursor(...) end
local function render_pr_status(...) return render_orchestrator().render_pr_status(...) end
local function status_open_pr(...) return pr_state.open_pr(...) end
local function status_show_help(...) return commit_view()._status_show_help(...) end

-- Forward-declared: status_command_visible references it before its definition.
local status_command_visible_for_view
local function render_status_or_notify(...) return render_orchestrator().render_status_or_notify(...) end
local function status_leave_visual_mode(...) return entry_nav()._status_leave_visual_mode(...) end
local function status_entries_from_visual_selection(...) return entry_nav()._status_entries_from_visual_selection(...) end
local function status_jump(...) return commit_view()._status_jump(...) end
local function status_remote_action(...) return commit_view()._status_remote_action(...) end
local function status_defer_prewarm_under_cursor(...) return entry_nav()._status_defer_prewarm_under_cursor(...) end
local function status_toggle(...) return commit_view()._status_toggle(...) end
local function status_stage_entries(...) return actions()._status_stage_entries(...) end
local function status_stage(...) return actions()._status_stage(...) end
local function status_primary_key(...) return status_keys.primary_key(...) end
local function status_open_about(...) return commit_view()._status_open_about(...) end
local function status_discard_entry_list(...) return actions()._status_discard_entry_list(...) end
local function status_discard(...) return actions()._status_discard(...) end
local function load_pr_diff(...) return render_orchestrator().load_pr_diff(...) end
local function status_unstage(...) return actions()._status_unstage(...) end
local function status_unstage_entries(...) return actions()._status_unstage_entries(...) end

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
  local keymaps = spec and spec.keymap == "review" and review().keymap_config() or status_keymap_config()
  local key = keymaps[command_id]
  if key == false or key == nil then return {} end
  if type(key) == "table" then return key end
  return { key }
end

local function status_hint_segments(state)
  local view_kind = state and state.view_kind or (session.status and session.status.view_kind) or "status"
  local segments = {}
  local first = true
  local hint_command_ids = command_specs.hint_command_ids_by_view[view_kind] or command_specs.hint_command_ids_by_view.status
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

-- ===========================================================================
-- Buffer-local keymaps for the status-family views (status / pr / diff / review).
--
-- Data-driven: each command's per-view *behavior* lives in COMMON_KEYMAPS (bound in
-- every view) or VIEW_KEYMAPS[view_kind] (one entry per binding). Per-view *visibility*
-- and the *keys* still come from shared/command_specs.lua (the `views` field) and
-- status_keys_for() (config.defaults.keymaps, overridable via setup{ keymaps = ... });
-- these tables own only the action callback. Adding a command to a view = one entry
-- here + its spec in command_specs.lua. Non-keymap per-view setup (the cursor-prewarm
-- autocmd, the review action set, the jump-back keys) lives in VIEW_SETUP_HOOKS.
-- ===========================================================================

local function close_view(buf)
  local state = session.states and session.states[buf] or session.status
  if vim.api.nvim_buf_is_valid(buf) then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end
  if session.states then session.states[buf] = nil end
  if session.main_status == state then session.main_status = nil end
  if session.status == state then session.status = session.main_status end
end

local function refresh_status(buf)
  if session.status then
    session.status.pr = nil
    session.status.about = nil
  end
  render_status_or_notify(buf, nil, nil, {
    restore_initial_folds = true,
    refresh_pr = true,
    refresh_about = true,
  })
end

local function refresh_pr(buf)
  local status = session.states and session.states[buf] or session.status
  if status and status.pr and status.cwd then
    session.status = status
    render_pr_status(status.pr, status.cwd, buf)
    load_pr_diff(status.pr, status.cwd, buf)
    pr_overview().load_comments(status.pr, status.cwd, buf)
    pr_overview().load_checks(status.pr, status.cwd, buf)
  end
end

local function reply_or_refresh_pr(buf)
  if pr_overview().reply_to_comment(buf) then return end
  refresh_pr(buf)
end

local function refresh_diff(buf)
  local status = session.states and session.states[buf] or session.status
  if status and status.diff_branch and status.cwd then
    session.status = status
    branch_diff.load(status.diff_branch, status.cwd, buf, status.diff_file)
  end
end

local function toggle_fold(buf)
  local status = session.states and session.states[buf] or session.status
  if status then session.status = status end
  if review().toggle_comment_fold(buf) then return end
  status_toggle(status_entry_under_cursor(status), status)
end

local function collapse_parent(buf)
  if session.states and session.states[buf] then session.status = session.states[buf] end
  commit_view()._status_collapse_parent()
end

local function start_visual_line_gutter(buf)
  diff_buffer._start_diff_gutter_visual_line(buf)
end

local function stage_under_cursor(buf)
  status_stage(status_entry_under_cursor())
end

local function stage_selection(buf)
  local entries = status_entries_from_visual_selection()
  status_leave_visual_mode()
  status_stage_entries(entries, { preserve_cursor = true })
end

local function unstage_under_cursor(buf)
  status_unstage(status_entry_under_cursor())
end

local function unstage_selection(buf)
  local entries = status_entries_from_visual_selection()
  status_leave_visual_mode()
  status_unstage_entries(entries, { preserve_cursor = true })
end

local function discard_under_cursor(buf)
  status_discard(status_entry_under_cursor())
end

local function discard_selection(buf)
  local entries = status_entries_from_visual_selection()
  status_leave_visual_mode()
  status_discard_entry_list(entries, nil, { preserve_cursor = true })
end

local function status_open(buf)
  local entry = status_entry_under_cursor()
  if require("diff_review.views.walkthrough").jump_status_step(buf) then
    return
  elseif commit_view()._status_open_commit_message_under_cursor(entry) then
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
end

local function pr_open(buf)
  local is_pr_view = session.status and session.status.view_kind == "pr"
  if is_pr_view and pr_edit.choose_pr_state_under_cursor(buf) then return end
  if is_pr_view and pr_overview().open_issue_comment_under_cursor(buf) then return end
  local entry = status_entry_under_cursor()
  if commit_view()._status_open_commit_message_under_cursor(entry) then return end
  status_jump(entry)
end

local function review_open(buf)
  local entry = status_entry_under_cursor()
  if commit_view()._status_open_commit_message_under_cursor(entry) then return end
  status_jump(entry)
end

local function browse_pr(buf)
  local status = session.states and session.states[buf] or session.status
  if status and status.view_kind == "pr" then
    local url = pr_overview().url_under_cursor(buf)
    if url and gh.browse_url(url) then return end
  end
  local pr = status and status.pr
  if not gh.browse_pr(pr) then
    notify_error("Unable to open PR URL", "DiffReview")
  end
end

local function review_browse(buf)
  local status = session.states and session.states[buf] or session.status
  local fragment = review().browse_fragment_under_cursor(buf)
  review().leave_visual()
  if not gh.browse_pr_changes(status and status.pr, fragment) then
    notify_error("Unable to open PR URL", "DiffReview")
  end
end

local function pr_add_comment(buf)
  pr_overview().add_standalone_comment(buf)
end

local function pr_delete_comment(buf)
  pr_overview().delete_standalone_comment(buf)
end

local function pr_sync_comments(buf)
  review().sync_inline_comment_text(buf)
  pr_overview().sync_reply_draft(buf)
  pr_overview().sync_standalone_comments(buf)
  pr_edit.sync(buf)
end

local function start_review(buf)
  review().start(buf)
end

local function commit_changes(buf)
  require("diff_review.integrations.commit").commit({
    win = vim.api.nvim_get_current_win(),
    on_done = function()
      if vim.api.nvim_buf_is_valid(buf) then render_status_or_notify(buf) end
    end,
  })
end

local function push_remote(buf)
  status_remote_action(buf, "push")
end

local function pull_remote(buf)
  status_remote_action(buf, "pull")
end

local function open_pr_action(buf)
  status_open_pr(status_entry_under_cursor())
end

local function create_branch(buf)
  status_helpers.create_branch(buf)
end

local function start_walkthrough(buf)
  require("diff_review.views.walkthrough").start(commands._walkthrough_host(buf))
end

local function show_help(buf)
  status_show_help()
end

local function jump_back_with_saved_view(buf)
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

local function install_cursor_prewarm(buf)
  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buf,
    callback = function()
      trace.span("status.autocmd_cursor_prewarm", buf, nil, function()
        if (M.config or config.options or config.defaults).status_cursor_prewarm == false then return end
        if session.states and session.states[buf] then session.status = session.states[buf] end
        status_defer_prewarm_under_cursor(buf)
      end)
    end,
  })
end

--- Bindings installed in every status-family view. `mode` mirrors vim.keymap.set;
--- `handler(buf)` is the action. Keys + visibility come from command_specs.
local COMMON_KEYMAPS = {
  { id = "close", mode = "n", handler = close_view },
  { id = "toggle", mode = "n", handler = toggle_fold },
  { id = "collapse_parent", mode = "n", handler = collapse_parent },
  { id = "visual_line_with_gutter", mode = "n", handler = start_visual_line_gutter },
}

--- Per-view command behavior. A command only installs where command_specs marks it
--- visible (status_command_visible), so an entry listed in a view it is hidden from is
--- a harmless no-op; keep these lists aligned with command_specs.views for clarity.
local VIEW_KEYMAPS = {
  status = {
    { id = "refresh", mode = "n", handler = refresh_status },
    { id = "stage", mode = "n", handler = stage_under_cursor, desc = "Stage hunk/file" },
    { id = "stage", mode = "x", handler = stage_selection, desc = "Stage selection" },
    { id = "unstage", mode = "n", handler = unstage_under_cursor, desc = "Unstage hunk/file" },
    { id = "unstage", mode = "x", handler = unstage_selection, desc = "Unstage selection" },
    { id = "discard", mode = "n", handler = discard_under_cursor, desc = "Discard hunk/file" },
    { id = "discard", mode = "x", handler = discard_selection, desc = "Discard selection" },
    { id = "open", mode = "n", handler = status_open },
    { id = "commit", mode = "n", handler = commit_changes },
    { id = "push", mode = "n", handler = push_remote },
    { id = "pull", mode = "n", handler = pull_remote },
    { id = "pr", mode = "n", handler = open_pr_action },
    { id = "branch_create", mode = "n", handler = create_branch },
    { id = "walkthrough", mode = "n", handler = start_walkthrough },
    { id = "review", mode = "n", handler = start_review },
    { id = "help", mode = "n", handler = show_help },
  },
  pr = {
    { id = "reply", mode = "n", handler = reply_or_refresh_pr },
    { id = "browse", mode = "n", handler = browse_pr },
    { id = "comment", mode = { "n", "x" }, handler = pr_add_comment },
    { id = "delete", mode = "n", handler = pr_delete_comment },
    { id = "sync", mode = { "n", "i" }, handler = pr_sync_comments },
    { id = "review", mode = "n", handler = start_review },
    { id = "open", mode = "n", handler = pr_open, desc = "Open comment or jump to file" },
    { id = "help", mode = "n", handler = show_help },
  },
  diff = {
    { id = "refresh", mode = "n", handler = refresh_diff },
    { id = "open", mode = "n", handler = pr_open, desc = "Jump to file" },
    { id = "help", mode = "n", handler = show_help },
  },
  review = {
    { id = "refresh", mode = "n", handler = refresh_status },
    { id = "browse", mode = { "n", "x" }, handler = review_browse },
    { id = "open", mode = "n", handler = review_open, desc = "Jump to file" },
    { id = "help", mode = "n", handler = show_help },
  },
}

--- Per-view setup that is not a single command keymap: the review action set + the
--- scheduled open re-map, the jump-back keys, and the cursor-prewarm autocmd. Receives
--- the buffer and the view's `map` helper (for re-mapping).
local VIEW_SETUP_HOOKS = {
  status = function(buf, _map)
    local opts = { buffer = buf, silent = true, nowait = true }
    for _, key in ipairs({ ",", "<C-o>" }) do
      vim.keymap.set("n", key, function() jump_back_with_saved_view(buf) end,
        vim.tbl_extend("force", opts, { desc = "Jump back" }))
    end
    install_cursor_prewarm(buf)
  end,
  pr = function(buf, _map)
    install_cursor_prewarm(buf)
  end,
  diff = function(buf, _map)
    install_cursor_prewarm(buf)
  end,
  review = function(buf, map)
    review().setup_keymaps(buf)
    vim.schedule(function()
      if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
      if session.states and session.states[buf] then session.status = session.states[buf] end
      map("open", "n", function() review_open(buf) end, "Jump to file")
    end)
  end,
}

local function setup_status_keymaps(buf)
  require("github.repo_cache").enable_user_completion(buf)
  local opts = { buffer = buf, silent = true, nowait = true }
  local view_kind = session.status and session.status.view_kind or "status"
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
      if view_kind == "review" or view_kind == "pr" then
        review().register_command_map(buf, command_id, mode, key, mapped, map_opts)
      else
        vim.keymap.set(mode, key, mapped, map_opts)
      end
    end
  end

  for _, entry in ipairs(COMMON_KEYMAPS) do
    map(entry.id, entry.mode, function() entry.handler(buf) end, entry.desc)
  end
  for _, entry in ipairs(VIEW_KEYMAPS[view_kind] or {}) do
    map(entry.id, entry.mode, function() entry.handler(buf) end, entry.desc)
  end
  local hook = VIEW_SETUP_HOOKS[view_kind]
  if hook then hook(buf, map) end
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

---@param group string
---@param command_id string
---@return string[]
function M.view_keys_for(group, command_id)
  local options = M.config or config.options or config.defaults
  local keymap_group = options.keymaps and options.keymaps[group] or {}
  local key = keymap_group and keymap_group[command_id]
  if key == false or key == nil then return {} end
  if type(key) == "table" then return vim.deepcopy(key) end
  return { key }
end

---@param buf integer
---@param group string
---@param command_set DiffReviewViewCommandSet
---@param context? table
function M.setup_view_keymaps(buf, group, command_set, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    if action and spec and (not action.enabled or action.enabled(context)) then
      local modes = type(spec.modes) == "table" and spec.modes or { spec.modes or "n" }
      for _, key in ipairs(M.view_keys_for(group, command_id)) do
        vim.keymap.set(modes, key, function()
          require("diff_review.shared.view_command_set").dispatch(command_set, command_id, context)
        end, { buffer = buf, silent = true, nowait = true, desc = spec.desc })
      end
    end
  end
end

---@param group string
---@param command_set DiffReviewViewCommandSet
---@param context? table
---@return string
function M.view_hint(group, command_set, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  local segment = {}
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    local key = M.view_keys_for(group, command_id)[1]
    if action and spec and spec.pinned and key and (not action.enabled or action.enabled(context)) then
      segment[#segment + 1] = key .. " " .. spec.label
    end
  end
  return table.concat(segment, " | ")
end

---@param win integer
---@param title string
---@param group string
---@param command_set DiffReviewViewCommandSet
---@param status? string|{ text: string, group: string }[]
---@param context? table
function M.apply_view_winbar(win, title, group, command_set, status, context)
  if not (win and vim.api.nvim_win_is_valid(win)) then return end
  local title_text = tostring(title or "")
  local status_segment_list = type(status) == "table" and status or nil
  local status_text = status_segment_list and table.concat(vim.tbl_map(function(segment)
    return segment.text
  end, status_segment_list)) or tostring(status or "")
  local status_rendered = status_segment_list and table.concat(vim.tbl_map(function(segment)
    return ("%%#%s#%s%%*"):format(segment.group, segment.text:gsub("%%", "%%%%"))
  end, status_segment_list)) or nil
  local hint_text = M.view_hint(group, command_set, context)
  local full_width = vim.fn.strdisplaywidth(title_text) + vim.fn.strdisplaywidth(status_text) + vim.fn.strdisplaywidth(hint_text) + 4
  local left_text = title_text ~= "" and title_text or status_text
  local center_text = title_text ~= "" and status_text or ""
  local left = left_text:gsub("%%", "%%%%")
  local center = center_text:gsub("%%", "%%%%")
  if full_width <= vim.api.nvim_win_get_width(win) then
    local hint = hint_text:gsub("%%", "%%%%")
    if status_rendered then
      local rendered_left = title_text ~= "" and ("%%#DiffReviewStatusLabel#%s%%*"):format(left) or status_rendered
      local rendered_center = title_text ~= "" and status_rendered or ""
      vim.wo[win].winbar = ("%s%%=%s%%=%%#DiffReviewStatusHint#%s%%*"):format(rendered_left, rendered_center, hint)
      return
    end
    vim.wo[win].winbar = ("%%#DiffReviewStatusLabel#%s%%*%%=%%#DiffReviewStatusHint#%s%%*%%=%%#DiffReviewStatusHint#%s%%*")
      :format(left, center, hint)
    return
  end
  local help_action = command_set.action_by_id.help
  local help_key = help_action and M.view_keys_for(group, "help")[1] or nil
  local help_hint = help_key and (help_key .. " help"):gsub("%%", "%%%%") or ""
  local help_section = help_hint ~= "" and ("%%=%%#DiffReviewStatusHint#%s%%*"):format(help_hint) or ""
  if status_rendered then
    local rendered_left = title_text ~= "" and ("%%#DiffReviewStatusLabel#%s%%*"):format(left) or status_rendered
    local rendered_center = title_text ~= "" and status_rendered or ""
    vim.wo[win].winbar = ("%s  %s%s"):format(rendered_left, rendered_center, help_section)
    return
  end
  vim.wo[win].winbar = ("%%#DiffReviewStatusLabel#%s%%*  %%#DiffReviewStatusHint#%s%%*%s"):format(left, center, help_section)
end

---@param group string
---@param command_set DiffReviewViewCommandSet
---@param title string
---@param context? table
function M.show_view_help(group, command_set, title, context)
  context = context or {}
  local spec_by_id = command_specs.view_spec_by_id[group] or {}
  local line = { title, "" }
  for _, command_id in ipairs(command_set.order or {}) do
    local action = command_set.action_by_id[command_id]
    local spec = spec_by_id[command_id]
    local keys = M.view_keys_for(group, command_id)
    if action and spec and #keys > 0 and (not action.enabled or action.enabled(context)) then
      line[#line + 1] = ("  %-12s %s"):format(table.concat(keys, ", "), spec.desc)
    end
  end
  local width = math.min(80, math.max(40, vim.o.columns - 8))
  local height = math.min(#line + 2, math.max(6, vim.o.lines - 6))
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, line)
  vim.bo[buf].modifiable = false
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    row = math.floor((vim.o.lines - height) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    width = width,
    height = height,
    border = "rounded",
    style = "minimal",
    title = " " .. title .. " ",
    title_pos = "center",
  })
  vim.keymap.set("n", "q", function() if vim.api.nvim_win_is_valid(win) then vim.api.nvim_win_close(win, true) end end,
    { buffer = buf, silent = true, nowait = true, desc = "Close help" })
end

return M
