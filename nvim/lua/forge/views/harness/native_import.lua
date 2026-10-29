local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local picker = require("forge.views.picker")
local provider_catalog = require("forge.views.harness.provider_catalog")

local M = {}
local sequence = 0

---@class ForgeNativeImportCandidate
---@field id string
---@field name string
---@field kind "skill"|"mcp"
---@field source string
---@field transport? string
---@field blocked? string
---@field conflict string[]

---@class ForgeNativeImportPreview
---@field source string
---@field fingerprint string
---@field revision integer
---@field candidate ForgeNativeImportCandidate[]

---@param state ForgeHarnessPresentationState
---@param host {window_list: integer[], control_win?: integer}
function M.open(state, host)
  if not state.session then notifications.warn("Harness is still initializing", "Import configuration") return end
  sequence = sequence + 1
  local instance = sequence
  local session_id = state.session.id
  local kind, scope, source
  local loading = false
  local generation_request = 0
  ---@type ForgeNativeImportPreview?
  local preview
  local owner = "harness-native-import"
  local review
  local refresh
  local function current()
    return sequence == instance and state.session and state.session.id == session_id
  end
  local function unavailable(candidate)
    if type(candidate.blocked) == "string" then return candidate.blocked end
    if #(candidate.conflict or {}) > 0 then return "Disable the selected definition with this name before importing" end
  end
  review = function()
    local rows = {}
    if preview then
      for _, candidate in ipairs(preview.candidate) do
        local failure = unavailable(candidate)
        rows[#rows + 1] = { id = candidate.id, value = candidate,
          label = candidate.name,
          disabled_reason = loading and "Import configuration is still loading" or failure,
          detail = failure or ((type(candidate.transport) == "string" and candidate.transport .. " · " or "") .. candidate.source) }
      end
      rows[#rows + 1] = { id = "apply", action = "submit", label = "Import selected", detail = "Save entries disabled in " .. scope .. " settings",
        disabled_reason = loading and "Import configuration is still loading" or nil }
      rows[#rows + 1] = { id = "refresh", action = "submit", label = "Refresh preview", detail = "Read the source and check current conflicts again",
        disabled_reason = loading and "Import configuration is still loading" or nil }
    end
    return { owner = owner, host = host,
      page_list = { { id = "preview", title = "Import Configuration",
        subtitle = loading and "Applying / reading native configuration..."
          or "Entries start disabled. MCP values remain referenced from the native source.",
        column_headers = { "Entry", "Source / outcome" }, option_list = rows, selection_mode = "multiple",
        empty_text = "Reading native configuration...", footer = "Enter select/apply  q close" } },
      on_confirm = function(result)
        if loading or not current() then return false end
        local option = result.option
        if option.id == "refresh" then refresh() return false end
        if option.id ~= "apply" then return false end
        if state.host_error then notifications.error(state.host_error, "Import configuration") return false end
        local reviewed = preview
        if not reviewed then notifications.error("Import preview is unavailable. Refresh it before applying.", "Import configuration") return false end
        local selection = {}
        for _, selected in ipairs(result.selected_option_list or {}) do
          local failure = unavailable(selected.value)
          if failure then notifications.warn(failure, "Import configuration") return false end
          selection[#selection + 1] = selected.value.id
        end
        if #selection == 0 then notifications.warn("Select at least one entry to import", "Import configuration") return false end
        loading = true
        picker.update(review())
        require("forge.views.harness.controller").task_transition({ action = "configure", kind = kind,
          scope = scope, source = reviewed.source, fingerprint = reviewed.fingerprint,
          expected_revision = reviewed.revision, selected = selection }, nil, function(applied)
          loading = false
          provider_catalog.clear()
          if not current() then return end
          if applied then
            notifications.info("Imported entries are disabled. Enable them in /skills or /mcp.", "Import configuration")
            if picker.is_open(owner) then picker.close() end
          elseif picker.is_open(owner) then refresh() end
        end, "settings.import.apply")
        return false
      end,
    }
  end
  refresh = function()
    if not current() then return end
    generation_request = generation_request + 1
    local generation = generation_request
    loading = true
    if picker.is_open(owner) then picker.update(review()) else picker.open(review()) end
    client.request({ session_id = session_id, method = "settings.import.preview", params = { kind = kind, scope = scope, source = source } }, function(value, failure)
      if generation ~= generation_request or not current() then return end
      loading = false
      if failure then
        notifications.error(failure, "Import configuration")
        if picker.is_open(owner) then picker.update({ owner = owner, host = host,
          page_list = { { id = "failed", title = "Import Configuration", subtitle = failure,
            option_list = { { id = "retry", label = "Retry preview" } } } },
          on_confirm = function() refresh() return false end }) end
        return
      end
      preview = value
      if picker.is_open(owner) then picker.open(review()) end
    end)
  end
  local function choose_scope()
    picker.open({ owner = owner, host = host,
      page_list = { { id = "scope", title = "Import Scope", option_list = {
        { label = "This workspace", value = "workspace", detail = "Available to all providers in this workspace" },
        { label = "Global", value = "global", detail = "Available to all workspaces and providers" },
      } } },
      on_confirm = function(result)
        if not current() then return false end
        scope = result.option.value
        picker.close()
        local directory = ({ codex = ".codex", copilot = ".copilot", claude = ".claude", antigravity = ".gemini" })[state.session.backend]
        local default = ""
        if directory then
          local relative = kind == "skills" and ({ codex = ".agents/skills", copilot = ".copilot/skills", claude = ".claude/skills", antigravity = ".gemini/config/skills" })[state.session.backend]
            or ({ codex = ".codex/config.toml", copilot = ".copilot/mcp-config.json", claude = ".claude.json", antigravity = ".gemini/config/mcp_config.json" })[state.session.backend]
          default = vim.fs.joinpath(vim.fn.expand("~"), relative)
        end
        vim.ui.input({ prompt = kind == "skills" and "Native skill directory: " or "Native MCP configuration file: ",
          default = default, completion = kind == "skills" and "dir" or "file" }, function(value)
          if not value or value == "" or not current() then return end
          source = vim.fn.fnamemodify(value, ":p")
          refresh()
        end)
        return false
      end,
    })
  end
  picker.open({ owner = owner, host = host,
    page_list = { { id = "kind", title = "Import Native Configuration", option_list = {
      { label = "MCP servers", value = "mcp", detail = "Preview JSON or TOML server definitions" },
      { label = "Skills", value = "skills", detail = "Preview a native skill directory" },
    } } },
    on_confirm = function(result)
      if not current() then return false end
      kind = result.option.value
      choose_scope()
      return false
    end,
  })
end

return M
