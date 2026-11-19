local SessionPicker = {}

local client = require("forge.client")
local datetime = require("forge.integrations.datetime")
local notifications = require("forge.infra.notifications")
local picker = require("forge.views.picker")
local session = require("forge.session")
local session_preview = require("forge.views.harness.session_preview")
local session_navigation = require("forge.views.harness.session_navigation")

local active = nil

---@class ForgeHarnessSessionPickerOptions
---@field provider? string
---@field on_select? fun(entry: table)
---@field on_cancel? fun()

---@param entry table
---@param scope "repo"|"all"
---@return table
local function option(entry, scope)
  local name = type(entry.name) == "string" and vim.trim(entry.name) or ""
  if name == "" then name = "[unnamed]" end
  local current_session = session.harness.session or {}
  local label = current_session.id == entry.id and (name .. " (current)") or name
  local mode = entry.approval_mode or "ask"
  local columns = { datetime.relative_ms(entry.created_at_ms), label, entry.provider or "", mode }
  if scope == "all" then columns[#columns + 1] = entry.workspace or "" end
  return {
    id = entry.id,
    section = entry.active and "Active" or "Inactive",
    label = ("%-24s  %s"):format(datetime.relative_ms(entry.created_at_ms), label),
    columns = columns,
    search_text = name,
    value = entry,
  }
end

---Preview `entry`, or invalidate the preview in flight when there is none.
---The calling session's own transcript is already on screen beneath the picker, so its entry
---removes the preview instead of drawing a copy of that transcript over it.
---@param instance table
---@param entry? table
---@param visible_height? integer Rows of the transcript window that the picker leaves uncovered.
local function request_preview(instance, entry, visible_height)
  instance.preview_height = visible_height
  instance.preview_generation = instance.preview_generation + 1
  local generation = instance.preview_generation
  if not entry then return end
  if entry.id == instance.owner_session_id then
    session_preview.hide()
    return
  end
  if not instance.owner_session_id then
    notifications.warn("Wait for the calling session to initialize before previewing saved sessions.", "ForgeSessions")
    return
  end
  client.request({ session_id = instance.owner_session_id, method = "session.preview", params = { session_id = entry.id } }, function(result, request_error)
    if active ~= instance or generation ~= instance.preview_generation then return end
    if request_error then
      notifications.error(request_error, "Session preview")
      return
    end
    session_preview.render(result or {}, instance.preview_height)
  end)
end

---@param left? string
---@param right? string
local function same_workspace(left, right)
  if left == right then return true end
  local resolved = left and vim.uv.fs_realpath(left)
  return resolved ~= nil and right ~= nil and resolved == vim.uv.fs_realpath(right)
end

---@param instance table
local function dismiss(instance)
  picker.close(false)
  session_preview.close()
  if active == instance then active = nil end
end

---@param instance table
---@param entry table
local function resume_session(instance, entry)
  local current_session = session.harness.session or {}
  if not same_workspace(entry.workspace, current_session.workspace) then
    notifications.warn("Cross-worktree sessions are history-only. Open Harness from that worktree to resume.", "ForgeSessions")
    return
  end
  if entry.provider ~= (instance.provider or current_session.provider) then
    -- The host serves one provider, so it cannot resume this session beside the current one. The
    -- provider switch replaces the host and resumes the session there. Choosing a session is not
    -- choosing a default provider, so the switch leaves the saved default alone. A switch that
    -- cannot start reports its own reason, and the picker stays open on the selection.
    local started = require("forge.views.harness").switch_provider(entry.provider,
      { kind = "resume", session_id = entry.id, persist_default = false })
    if started then dismiss(instance) end
    return
  end
  dismiss(instance)
  if instance.on_select then instance.on_select(entry)
  else session_navigation.resume(entry.id, { open_mode = "current", insert = instance.composer_insert }) end
end

local build_spec
local load_session_list

---@param instance table
---@param entry table
local function delete_session(instance, entry)
  if not entry then return end
  if not instance.owner_session_id then
    notifications.warn("Wait for the calling session to initialize before deleting saved sessions.", "ForgeSessions")
    return
  end
  if instance.owner_session_id == entry.id then
    notifications.warn("The active session cannot be deleted.", "ForgeSessions")
    return
  end
  client.request({ session_id = instance.owner_session_id, method = "session.delete", params = { session_id = entry.id } }, function(_, request_error)
    if request_error then
      if active == instance then notifications.error(request_error, "ForgeSessions") end
      return
    end
    -- Views of the deleted session close even when the picker closed before the host replied.
    session_navigation.session_deleted(entry.id)
    if active ~= instance then return end
    load_session_list(instance)
  end)
end

---@param instance table
---@return table
build_spec = function(instance)
  local page_list = {}
  for _, scope in ipairs(instance.provider and { "repo" } or { "repo", "all" }) do
    local option_list = {}
    local entry_list = instance.entry_list_by_scope[scope] or {}
    for _, is_active in ipairs({ true, false }) do
      for _, entry in ipairs(entry_list) do
        if (entry.active == true) == is_active then option_list[#option_list + 1] = option(entry, scope) end
      end
    end
    local column_headers = { "Created", "Session", "Harness", "Mode" }
    if scope == "all" then column_headers[#column_headers + 1] = "Workspace" end
    page_list[#page_list + 1] = {
      id = scope,
      title = instance.provider and "Resume Session" or (scope == "repo" and "Workspace" or "All"),
      column_headers = column_headers,
      header_right = instance.provider,
      option_list = option_list,
      wrap_selection = false,
      empty_text = instance.entry_list_by_scope[scope] and "No matching Harness sessions." or "Loading sessions...",
      search = { start_in_normal = true, preserve_sections = true },
      confirm_label = instance.provider and "resume" or "open",
    }
  end
  local function switch_scope()
    local spec = build_spec(instance)
    spec.initial_page = instance.scope == "repo" and 2 or 1
    picker.update(spec)
  end
  return {
    owner = "sessions",
    host = instance.host,
    title = instance.provider and "Resume Session" or "Select Session",
    size_ratio = 0.35,
    width_ratio = 1,
    placement = "bottom",
    page_navigation = not instance.provider and "tabs" or nil,
    -- The footer derives its hints from the effective bindings, which tabs switch between scopes.
    command_footer = true,
    page_hint = "scope",
    wrap_pages = true,
    page_list = page_list,
    action_list = instance.provider and {} or {
      {
        id = "scope",
        key = "<Tab>",
        label = "scope",
        modes = { "n", "i" },
        desc = "Switch Workspace and All sessions",
        callback = switch_scope,
      },
      {
        id = "delete",
        key = "<C-j>",
        label = "delete",
        modes = { "n", "i" },
        desc = "Delete selected Harness session",
        callback = function(context) delete_session(instance, context.option and context.option.value) end,
      },
      {
        id = "scope-alternate",
        key = "<C-o>",
        modes = { "n", "i" },
        desc = "Switch Workspace and All sessions",
        callback = switch_scope,
      },
    },
    on_change = function(context)
      if instance.scope ~= context.page.id then
        instance.scope = context.page.id
        request_preview(instance, nil, context.preview_height)
        load_session_list(instance)
        return
      end
      request_preview(instance, context.option and context.option.value, context.preview_height)
    end,
    on_confirm = function(result)
      resume_session(instance, result.option.value)
      return false
    end,
    on_close = function()
      local cancelled = active == instance
      if cancelled then active = nil end
      session_preview.close()
      if cancelled and instance.on_cancel then vim.schedule(instance.on_cancel) end
    end,
  }
end

---@param instance table
load_session_list = function(instance)
  instance.list_generation = instance.list_generation + 1
  local generation = instance.list_generation
  client.request({ method = "session.list", params = { scope = instance.scope, workspace = vim.fn.getcwd() } }, function(result, request_error)
    if active ~= instance or generation ~= instance.list_generation then return end
    if request_error then
      notifications.error(request_error, "ForgeSessions")
      if instance.provider then
        active = nil
        if instance.on_cancel then vim.schedule(instance.on_cancel) end
      end
      return
    end
    instance.entry_list_by_scope[instance.scope] = vim.tbl_filter(function(entry)
      return not instance.provider or (entry.provider == instance.provider and same_workspace(entry.workspace, instance.workspace))
    end, result or {})
    if picker.is_open("sessions") then
      picker.update(build_spec(instance))
    else
      session_preview.open(instance.host.transcript_win, instance.host.status_view)
      picker.open(build_spec(instance))
    end
  end)
end

---@param host table
---@param options? ForgeHarnessSessionPickerOptions
function SessionPicker.open(host, options)
  options = options or {}
  if active then
    picker.close(true)
    session_preview.close()
  end
  local instance = {
    host = host,
    owner_session_id = session.harness.session and session.harness.session.id,
    scope = "repo",
    entry_list_by_scope = {},
    list_generation = 0,
    preview_generation = 0,
    provider = options.provider,
    workspace = session.harness.session and session.harness.session.workspace,
    on_select = options.on_select,
    on_cancel = options.on_cancel,
    -- The picker returns focus to the composer in this mode, and a chosen session's composer
    -- takes it over.
    composer_insert = vim.fn.mode(1):sub(1, 1) == "i",
  }
  active = instance
  load_session_list(instance)
end

function SessionPicker.close()
  picker.close(true)
  session_preview.close()
  active = nil
end

function SessionPicker._state_for_test()
  return active
end

return SessionPicker
