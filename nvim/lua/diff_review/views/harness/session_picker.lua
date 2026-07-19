local SessionPicker = {}

local client = require("diff_review.harness.client")
local config = require("diff_review.infra.config")
local notifications = require("diff_review.infra.notifications")
local picker = require("diff_review.views.picker")
local session = require("diff_review.session")
local session_preview = require("diff_review.views.harness.session_preview")
local session_navigation = require("diff_review.views.harness.session_navigation")

local active = nil

---@param entry table
---@param scope "repo"|"all"
---@return table
local function option(entry, scope)
  local name = type(entry.name) == "string" and vim.trim(entry.name) or ""
  if name == "" then name = "[unnamed]" end
  local current_session = session.harness.session or {}
  local active_marker = current_session.id == entry.id and "current" or nil
  local mode = entry.execution_mode == "yolo" and "YOLO"
    or ((entry.execution_mode or "read"):sub(1, 1):upper() .. (entry.execution_mode or "read"):sub(2))
  local detail_list = vim.tbl_filter(function(value) return value and value ~= "" end, {
    active_marker,
    mode,
    entry.backend,
    entry.model,
    entry.effort,
    scope == "all" and entry.workspace or nil,
  })
  return {
    id = entry.id,
    label = name,
    detail = table.concat(detail_list, " · "),
    search_text = name,
    value = entry,
  }
end

---@param instance table
local function request_preview(instance, entry)
  instance.preview_generation = instance.preview_generation + 1
  local generation = instance.preview_generation
  if not entry then return end
  client.request("session.preview", { session_id = entry.id }, function(result, request_error)
    if active ~= instance or generation ~= instance.preview_generation then return end
    if request_error then
      notifications.error(request_error, "Session preview")
      return
    end
    session_preview.render(result or {})
  end)
end

---@param instance table
---@param entry table
local function resume_session(instance, entry)
  local current_session = session.harness.session or {}
  if entry.workspace ~= current_session.workspace then
    notifications.warn("Cross-worktree sessions are history-only. Open Harness from that worktree to resume.", "Sessions")
    return
  end
  if entry.backend ~= current_session.backend then
    notifications.warn("This session uses a different configured backend.", "Sessions")
    return
  end
  picker.close(false)
  session_preview.close()
  active = nil
  session_navigation.resume(entry.id, { open_mode = instance.open_mode })
end

local build_spec
local load_session_list

---@param instance table
---@param entry table
local function delete_session(instance, entry)
  if not entry then return end
  if session.harness.session and session.harness.session.id == entry.id then
    notifications.warn("The active session cannot be deleted.", "Sessions")
    return
  end
  client.request("session.delete", { session_id = entry.id }, function(_, request_error)
    if active ~= instance then return end
    if request_error then
      notifications.error(request_error, "Sessions")
      return
    end
    load_session_list(instance)
  end)
end

---@param instance table
---@return table
build_spec = function(instance)
  local scope_label = instance.scope == "repo" and "current repository" or "all repositories"
  local open_label = instance.open_mode == "tab" and "new tab" or "current tab"
  local option_list = vim.tbl_map(function(entry) return option(entry, instance.scope) end, instance.entry_list)
  return {
    owner = "sessions",
    host = instance.host,
    page_list = {
      {
        id = "sessions",
        title = "Harness sessions",
        subtitle = "Search " .. scope_label .. ". Open in " .. open_label .. ".",
        option_list = option_list,
        empty_text = "No matching Harness sessions.",
        search = { choice_keys = config.options.picker.session_keys },
        footer = "↑↓ select  Enter open  Tab toggle tab  C-j delete  C-o scope  Esc normal",
      },
    },
    action_list = {
      {
        id = "open-mode",
        key = "<Tab>",
        modes = { "n", "i" },
        desc = "Toggle current-tab and new-tab opening",
        callback = function()
          instance.open_mode = instance.open_mode == "current" and "tab" or "current"
          picker.update(build_spec(instance))
        end,
      },
      {
        id = "delete",
        key = "<C-j>",
        modes = { "n", "i" },
        desc = "Delete selected Harness session",
        callback = function(context) delete_session(instance, context.option and context.option.value) end,
      },
      {
        id = "scope",
        key = "<C-o>",
        modes = { "n", "i" },
        desc = "Toggle repository session scope",
        callback = function()
          instance.scope = instance.scope == "repo" and "all" or "repo"
          load_session_list(instance)
        end,
      },
    },
    on_change = function(context) request_preview(instance, context.option and context.option.value) end,
    on_confirm = function(result)
      resume_session(instance, result.option.value)
      return false
    end,
    on_close = function()
      if active == instance then active = nil end
      session_preview.close()
    end,
  }
end

---@param instance table
load_session_list = function(instance)
  instance.list_generation = instance.list_generation + 1
  local generation = instance.list_generation
  client.request("session.list", { scope = instance.scope, workspace = vim.fn.getcwd() }, function(result, request_error)
    if active ~= instance or generation ~= instance.list_generation then return end
    if request_error then
      notifications.error(request_error, "Sessions")
      return
    end
    instance.entry_list = result or {}
    if picker.is_open("sessions") then
      picker.update(build_spec(instance))
    else
      session_preview.open(instance.host.transcript_win)
      picker.open(build_spec(instance))
    end
  end)
end

---@param host table
function SessionPicker.open(host)
  if active then
    picker.close(true)
    session_preview.close()
  end
  local instance = {
    host = host,
    scope = "repo",
    open_mode = "current",
    entry_list = {},
    list_generation = 0,
    preview_generation = 0,
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
