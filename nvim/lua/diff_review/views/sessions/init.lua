local M = {}

local client = require("diff_review.harness.client")
local command_set = require("diff_review.shared.view_command_set")
local config = require("diff_review.infra.config")
local keymaps = require("diff_review.shared.keymaps")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local session = require("diff_review.session")
local interaction_state = require("diff_review.views.harness.interaction_state")

local namespace = vim.api.nvim_create_namespace("DiffReviewSessions")
local fork
local sync_fork_action

---@return table?
local function state() return session.harness.sessions_view end

---@return table?
local function selected_session()
  local current = assert(state())
  if not current or not current.win or not vim.api.nvim_win_is_valid(current.win) then return nil end
  return current.row[vim.api.nvim_win_get_cursor(current.win)[1]]
end

---@param item table[]
local function render(item)
  local current = state()
  if not current or not vim.api.nvim_buf_is_valid(current.buf) then return end
  local current_tab = current.scope == "repo" and "[Current Repo]" or " Current Repo "
  local all_tab = current.scope == "all" and "[All Repos]" or " All Repos "
  local line = { current_tab .. "   " .. all_tab, "" }
  local row = {}
  local last_workspace = nil
  for _, entry in ipairs(item or {}) do
    if current.scope == "all" and entry.workspace ~= last_workspace then
      last_workspace = entry.workspace
      line[#line + 1] = entry.workspace
    end
    local active = session.harness.session and session.harness.session.id == entry.id and "*" or " "
    local name = type(entry.name) == "string" and vim.trim(entry.name) or ""
    if name == "" then name = "[unnamed]" end
    local raw_mode = entry.execution_mode or "read"
    local mode = raw_mode == "yolo" and "YOLO" or (raw_mode:sub(1, 1):upper() .. raw_mode:sub(2))
    local provider = entry.backend_session_id and "provider linked" or "not started"
    line[#line + 1] = ("%-28s %s %-7s %-8s %-14s %s/%s"):format(
      name, active, mode, entry.backend or "", provider, entry.model or "", entry.effort or ""
    )
    row[#line] = entry
  end
  if #item == 0 then line[#line + 1] = "No Harness sessions in this scope." end
  current.row = row
  vim.bo[current.buf].modifiable = true
  vim.api.nvim_buf_set_lines(current.buf, 0, -1, false, line)
  vim.api.nvim_buf_clear_namespace(current.buf, namespace, 0, -1)
  vim.api.nvim_buf_add_highlight(current.buf, namespace, "DiffReviewStatusSection", 0, 0, -1)
  for line_number, entry in pairs(row) do
    if entry.workspace ~= (session.harness.session and session.harness.session.workspace) then
      vim.api.nvim_buf_add_highlight(current.buf, namespace, "Comment", line_number - 1, 0, -1)
    end
  end
  vim.bo[current.buf].modifiable = false
  sync_fork_action()
  keymaps.apply_view_winbar(current.win, config.options.harness.sessions_buffer_name, "sessions", current.command_set,
    current.scope == "repo" and "Current Repo" or "All Repos", current)
end

function M.refresh()
  local current = state()
  if not current then return end
  client.request("session.list", { scope = current.scope, workspace = vim.fn.getcwd() }, function(result, request_error)
    if request_error then notifications.error(request_error, "Sessions") return end
    render(result or {})
  end)
end

local function toggle_scope()
  local current = state()
  if not current then return end
  current.scope = current.scope == "repo" and "all" or "repo"
  M.refresh()
end

local function resume()
  local entry = selected_session()
  if not entry then return end
  local current_session = session.harness.session or {}
  if entry.workspace ~= current_session.workspace then
    notifications.warn("Cross-worktree sessions are history-only. Use native fork from that worktree.", "Sessions")
    return
  end
  if entry.backend ~= current_session.backend then
    notifications.warn("This session uses a different configured backend.", "Sessions")
    return
  end
  client.request("session.resume", { session_id = entry.id }, function(result, request_error)
    if request_error then notifications.error(request_error, "Sessions") return end
    session.harness.session = result.session
    interaction_state.replace(session.harness, result.interaction or {})
    session.harness.capability = result.capability or {}
    session.harness.queue = {}
    session.harness.goal = result.goal
    session.harness.active_plan = result.active_plan
    session.harness.active_elicitation = result.active_elicitation
    session.harness.timeline = vim.deepcopy(result.timeline or {})
    session.harness.artifact = vim.deepcopy(result.artifact or {})
    require("diff_review.views.harness.controller").render(true)
    M.refresh()
  end)
end

fork = function()
  local entry = selected_session()
  local current_session = session.harness.session or {}
  if not entry or entry.native_fork ~= true or entry.backend ~= current_session.backend then return end
  client.request("session.fork", { session_id = entry.id }, function(result, request_error)
    if request_error then notifications.error(request_error, "Sessions fork") return end
    session.harness.session = result.session
    interaction_state.replace(session.harness, result.interaction or {})
    session.harness.capability = result.capability or {}
    session.harness.queue = {}
    session.harness.goal = result.goal
    session.harness.active_plan = result.active_plan
    session.harness.active_elicitation = result.active_elicitation
    session.harness.timeline = vim.deepcopy(result.timeline or {})
    session.harness.artifact = vim.deepcopy(result.artifact or {})
    require("diff_review.views.harness.controller").render(true)
    M.refresh()
  end)
end

sync_fork_action = function()
  local current = state()
  if not current or not current.command_set then return end
  local entry = selected_session()
  local active = session.harness.session or {}
  local supported = entry ~= nil and entry.native_fork == true and entry.backend == active.backend
  if supported and not current.command_set.action_by_id.fork then
    command_set.register(current.command_set, "fork", fork)
    keymaps.setup_view_keymaps(current.buf, "sessions", current.command_set)
  elseif not supported and current.command_set.action_by_id.fork then
    command_set.unregister(current.command_set, "fork")
  end
end

local function rename()
  local entry = selected_session()
  if not entry then return end
  popup_window.input({ prompt = "Session name: ", default = entry.name or "" }, function(name)
    if name == nil then return end
    client.request("session.rename", { session_id = entry.id, name = vim.trim(name) }, function(_, request_error)
      if request_error then notifications.error(request_error, "Sessions") return end
      M.refresh()
    end)
  end)
end

local function delete()
  local entry = selected_session()
  if not entry then return end
  popup_window.select({ "Cancel", "Delete Harness state" }, {
    prompt = "Delete this Harness session? The native provider session remains intact.",
  }, function(choice)
    if choice ~= "Delete Harness state" then return end
    client.request("session.delete", { session_id = entry.id }, function(_, request_error)
      if request_error then notifications.error(request_error, "Sessions") return end
      M.refresh()
    end)
  end)
end

local function close()
  if vim.fn.tabpagenr("$") > 1 then vim.cmd("tabclose") else vim.cmd("enew") end
end

---@return DiffReviewViewCommandSet
local function commands()
  local set = command_set.new()
  command_set.register(set, "open", resume)
  command_set.register(set, "tab_next", toggle_scope)
  command_set.register(set, "rename", rename)
  command_set.register(set, "delete", delete)
  command_set.register(set, "refresh", M.refresh)
  command_set.register(set, "close", close)
  command_set.register(set, "help", function() keymaps.show_view_help("sessions", set, "Sessions") end)
  return set
end

local function open_ready()
  local name = config.options.harness.sessions_buffer_name
  local buf = vim.fn.bufnr(name)
  local existing_win = buf >= 0 and vim.fn.win_findbuf(buf)[1] or nil
  if existing_win and vim.api.nvim_win_is_valid(existing_win) then
    vim.api.nvim_set_current_tabpage(vim.api.nvim_win_get_tabpage(existing_win))
    vim.api.nvim_set_current_win(existing_win)
    M.refresh()
    return
  end
  vim.cmd("tabnew")
  if buf < 0 or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  local win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win, buf)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "DiffReviewSessions"
  vim.bo[buf].modifiable = false
  session.harness.sessions_view = { buf = buf, win = win, scope = "repo", row = {} }
  local current = assert(state())
  current.command_set = commands()
  keymaps.setup_view_keymaps(buf, "sessions", current.command_set)
  local group = vim.api.nvim_create_augroup("DiffReviewHarnessSessions" .. buf, { clear = true })
  vim.api.nvim_create_autocmd("CursorMoved", {
    group = group,
    buffer = buf,
    callback = function()
      sync_fork_action()
      local active = state()
      if active then
        keymaps.apply_view_winbar(active.win, config.options.harness.sessions_buffer_name, "sessions",
          active.command_set, active.scope == "repo" and "Current Repo" or "All Repos", active)
      end
    end,
  })
  M.refresh()
end

function M.open()
  client.start(function(_, start_error)
    if start_error then notifications.error(start_error, "Sessions") return end
    open_ready()
  end)
end

return M
