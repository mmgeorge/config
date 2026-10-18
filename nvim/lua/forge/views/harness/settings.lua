local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local picker_field = require("forge.views.picker.field")
local duration = require("forge.views.harness.duration")
local notifications = require("forge.infra.notifications")
local log_buffer_by_path = {}
local log_revision_by_buffer = {}

---@param value any
---@return string[]
local function formatted_json(value)
  local encoded = vim.json.encode(value)
  local lines = {}
  local current = {}
  local depth = 0
  local in_string = false
  local escaped = false
  local function emit()
    lines[#lines + 1] = string.rep("  ", depth) .. table.concat(current)
    current = {}
  end
  for index = 1, #encoded do
    local character = encoded:sub(index, index)
    if in_string then
      current[#current + 1] = character
      if escaped then
        escaped = false
      elseif character == "\\" then
        escaped = true
      elseif character == '"' then
        in_string = false
      end
    elseif character == '"' then
      in_string = true
      current[#current + 1] = character
    elseif character == "{" or character == "[" then
      current[#current + 1] = character
      emit()
      depth = depth + 1
    elseif character == "}" or character == "]" then
      if #current > 0 then emit() end
      depth = depth - 1
      current[#current + 1] = character
    elseif character == "," then
      current[#current + 1] = character
      emit()
    elseif character == ":" then
      current[#current + 1] = ": "
    else
      current[#current + 1] = character
    end
  end
  if #current > 0 then emit() end
  return lines
end

---@param path string
---@param buffer integer
---@param force? boolean
local function refresh_log(path, buffer, force)
  local metadata, stat_error = vim.uv.fs_stat(path)
  if not metadata and stat_error and not stat_error:find("ENOENT", 1, true) then
    notifications.error(stat_error, "Harness logging")
    return
  end
  local revision = metadata and string.format("%d:%d:%d", metadata.size, metadata.mtime.sec, metadata.mtime.nsec) or "missing"
  if not force and revision == log_revision_by_buffer[buffer] then return end
  local read_ok, source = pcall(function() return metadata and vim.fn.readfile(path) or {} end)
  if not read_ok then
    notifications.error(tostring(source), "Harness logging")
    return
  end
  local lines = {}
  for index, source_line in ipairs(source) do
    local decode_ok, record = pcall(vim.json.decode, source_line)
    if decode_ok and type(record) == "table" then
      local timestamp = type(record.timestamp_ms) == "number"
          and os.date("%Y-%m-%d %H:%M:%S", math.floor(record.timestamp_ms / 1000))
          or "unknown time"
      local event = type(record.event) == "string" and record.event or "unknown event"
      lines[#lines + 1] = string.format("%s  %s  #%d", timestamp, event, index)
      local payload = record.payload == nil and vim.NIL or record.payload
      for _, payload_line in ipairs(formatted_json(payload)) do
        lines[#lines + 1] = "  " .. payload_line
      end
      lines[#lines + 1] = ""
    elseif source_line ~= "" then
      lines[#lines + 1] = string.format("Invalid JSONL record #%d", index)
      lines[#lines + 1] = source_line
      lines[#lines + 1] = ""
      notifications.error(string.format("Invalid JSONL record #%d in %s", index, path), "Harness logging")
    end
  end
  if #lines == 0 then lines = { "No log events yet" } end
  vim.bo[buffer].readonly = false
  vim.bo[buffer].modifiable = true
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  vim.bo[buffer].modifiable = false
  vim.bo[buffer].readonly = true
  vim.bo[buffer].modified = false
  log_revision_by_buffer[buffer] = revision
end

---@param status { enabled: boolean }
function M.adopt(status)
  require("forge.infra.perf").setup({ harness = { enabled = status.enabled } })
end

local function request(session_id, method, params, callback)
  client.request_for(session_id, method, params, function(status, failure)
    if failure then notifications.error(failure, "Harness logging") return end
    M.adopt(status)
    if callback then callback(status) end
  end)
end

local function open_log(status)
  local title = "Harness log • " .. (status.enabled and "On" or "Off") .. " • R refresh • q close"
  local existing = log_buffer_by_path[status.path]
  if existing and vim.api.nvim_buf_is_valid(existing) then
    local windows = vim.fn.win_findbuf(existing)
    if #windows > 0 then
      vim.api.nvim_set_current_win(windows[1])
      vim.wo.winbar = title
      refresh_log(status.path, existing)
      return
    end
  end
  vim.cmd.tabnew()
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(0, buffer)
  log_buffer_by_path[status.path] = buffer
  vim.bo[buffer].readonly = true
  vim.bo[buffer].modifiable = false
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].filetype = "text"
  vim.keymap.set("n", "q", "<Cmd>tabclose<CR>", { buffer = buffer, silent = true })
  vim.wo.winbar = title
  vim.keymap.set("n", "R", function() refresh_log(status.path, buffer) end, { buffer = buffer, desc = "Refresh Harness log" })
  vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
    buffer = buffer,
    callback = function() refresh_log(status.path, buffer) end,
  })
  refresh_log(status.path, buffer)
end

---@param session_id string
---@param action? string
function M.log(session_id, action)
  action = action or "open"
  if action == "open" or action == "" then
    request(session_id, "trace.status", {}, open_log)
  elseif action == "on" or action == "off" then
    request(session_id, "trace.configure", { enabled = action == "on" })
  elseif action == "clear" then
    request(session_id, "trace.session.clear", {}, function(status)
      local buffer = log_buffer_by_path[status.path]
      if buffer and vim.api.nvim_buf_is_valid(buffer) then refresh_log(status.path, buffer, true) end
    end)
  else
    notifications.error("Use /log [on|off|open|clear]", "Harness logging")
  end
end

---@param state ForgeHarnessPresentationState
---@param host table
function M.open(state, host)
  if not state.session then return end
  local session_id = state.session.id
  M._revision = (M._revision or 0) + 1
  local owner_revision = M._revision
  request(session_id, "trace.status", {}, function(status)
    if not state.session or state.session.id ~= session_id then return end
    local pending = false
    local active = vim.tbl_extend("force", assert(state.session), state.configuring_config or {},
      state.pending_config or {}, state.task_config or {})
    active.status_thoughts = state.status_thoughts == true
    local configuration_revision = 0
    local configuration_pending = false
    local cli = active.provider_label or ({ codex = "Codex CLI", copilot = "Copilot CLI" })[active.backend] or active.backend
    local build_spec
    local commit
    local timeout_focus = { approval_timeout = "amount", answer_timeout = "amount" }
    local access = require("forge.views.harness.access")
    local function configure(params, completed)
      if not state.session or state.session.id ~= session_id then return end
      configuration_revision = configuration_revision + 1
      local revision = configuration_revision
      configuration_pending = true
      active = vim.tbl_extend("force", active, params)
      local session = require("forge.session")
      local previous = session.harness
      session.activate_harness(state)
      require("forge.views.harness.controller").configure(params, false, function(applied)
        if not state.session or state.session.id ~= session_id then return end
        if revision ~= configuration_revision then return end
        configuration_pending = false
        active = vim.tbl_extend("force", assert(state.session), state.configuring_config or {},
          state.pending_config or {}, state.task_config or {})
        active.status_thoughts = state.status_thoughts == true
        if completed then completed(applied) end
        if picker.is_open("harness-config") then picker.update(build_spec()) end
      end)
      session.activate_harness(previous)
      if picker.is_open("harness-config") then picker.update(build_spec()) end
    end
    local function apply_access(policy, completed)
      configure({ access = policy }, function(applied) if applied and completed then completed() end end)
    end
    local function toggle(context, direction)
      if pending or not context.option then return end
      local id = context.option.id
      if commit and commit:change(id, direction or 1) then return end
      if id == "auto-compact" or id == "compact-threshold" then
        local policy = vim.deepcopy(active.auto_compaction or { enabled = false, threshold_tokens = 128000 })
        if id == "auto-compact" then
          if not (state.capability and state.capability.native_compact) then notifications.error("This provider does not support compaction", "Harness") return end
          policy.enabled = not policy.enabled
          configure({ auto_compaction = policy })
        else
          vim.ui.input({ prompt = "Compact at tokens: ", default = tostring(policy.threshold_tokens) }, function(value)
            if not value or not state.session or state.session.id ~= session_id then return end
            local amount = tonumber(value)
            if not amount or amount <= 0 or amount % 1 ~= 0 then notifications.error("Enter a positive whole token count", "Harness") return end
            policy.threshold_tokens = amount
            configure({ auto_compaction = policy })
          end)
        end
        return
      end
      if timeout_focus[id] then
        configure({ [id] = duration.change(duration.value(active[id]), timeout_focus[id], direction or 1) })
        return
      end
      if id == "sandbox" or id == "read-access" or id == "write-access" or id == "windows-sandbox" then
        local policy = access.policy(active)
        local supported = state.capability and state.capability.os_sandbox == true
        if not supported and (id == "sandbox" or id == "windows-sandbox") then
          notifications.error("This provider does not expose sandbox configuration", "Harness access")
          return
        end
        if id == "sandbox" then policy.sandbox = not policy.sandbox
        elseif id == "read-access" then policy.read_access = policy.read_access == "workspace" and "full" or "workspace"
        elseif id == "write-access" then policy.write_access = policy.write_access == "workspace" and "full" or "workspace"
        else
          local values = { "elevated", "unelevated", "mxc" }
          local position = 1
          for index, value in ipairs(values) do if value == policy.windows_sandbox then position = index end end
          policy.windows_sandbox = values[(position - 1 + (direction or 1)) % #values + 1]
        end
        apply_access(policy)
        return
      elseif id == "readable-directories" or id == "writable-directories" then
        picker.close(false)
        access.directories(state, host, apply_access, function() M.open(state, host) end,
          id == "readable-directories" and "read" or "write")
        return
      end
      if id == "default-write-permission" or id == "plan-permission" then
        local field = id == "default-write-permission" and "default_write_permission" or "plan_permission"
        local values = field == "plan_permission" and { "keep", "read", "write", "yolo" } or { "write", "yolo" }
        local selected = active[field]
        if not selected or selected == vim.NIL then selected = "keep" end
        local position = 1
        for index, value in ipairs(values) do if value == selected then position = index end end
        local next_value = values[(position - 1 + (direction or 1)) % #values + 1]
        configure({ [field] = next_value == "keep" and vim.NIL or next_value })
      elseif id == "shell" then
        local values = { "system" }
        if vim.fn.executable("nu") == 1 then values[#values + 1] = "nushell" end
        local position = active.shell == "nushell" and #values or 1
        configure({ shell = values[(position - 1 + (direction or 1)) % #values + 1] })
      elseif id == "plan-revisions" then
        configure({ plan_auto_approve_revisions = active.plan_auto_approve_revisions == false })
      elseif id == "subagent-tools" then
        configure({ subagent_tools = (active.subagent_tools == nil and state.subagent_tools or active.subagent_tools) ~= true })
      elseif id == "lsp-tools" then
        configure({ lsp_tools = (active.lsp_tools == nil and state.lsp_tools or active.lsp_tools) ~= true })
      elseif id == "status-thoughts" then
        configure({ status_thoughts = active.status_thoughts ~= true })
      elseif id == "logging" then
        pending = true
        client.request_for(session_id, "trace.configure", { enabled = not status.enabled }, function(result, failure)
          pending = false
          if failure then notifications.error(failure, "Harness logging") return end
          status = result
          M.adopt(status)
          if picker.is_open("harness-config") then picker.update(build_spec()) end
        end)
      end
    end
    build_spec = function()
      local policy = access.policy(active)
      local supported = state.capability and state.capability.os_sandbox == true
      local commit_rows = commit and commit:rows() or {}
      local provider_setup = (active.backend == "claude" or active.backend == "antigravity") and {
        id = "provider-setup", label = "Provider setup",
        columns = { "Provider setup", "Install / sign in", "Manage the ACP runtime and OAuth login" },
      } or nil
      return {
        owner = "harness-config", host = host,
        page_list = { {
          id = "config", title = "Configuration", subtitle = "CLI: " .. (cli or "Unknown")
            .. (configuration_pending and " · Changes pending" or ""),
          column_headers = { "Setting", "Value", "Description" },
          option_list = {
            commit_rows[1], commit_rows[2], commit_rows[3], commit_rows[4],
            { id = "auto-compact", label = "Auto compact", columns = { "Auto compact", picker_field.render(active.auto_compaction and active.auto_compaction.enabled and "On" or "Off", true), state.capability and state.capability.native_compact and (active.context_usage and active.context_usage ~= vim.NIL and "Current session · after completed turns" or "Waiting for provider context usage") or "Unavailable for this provider" } },
            { id = "compact-threshold", label = "Compact at tokens", columns = { "Compact at tokens", picker_field.render(tostring(active.auto_compaction and active.auto_compaction.threshold_tokens or 128000), true), "Current session · provider-reported context tokens" } },
            { id = "approval_timeout", label = "Approval timeout", columns = { "Approval timeout", duration.render(duration.value(active.approval_timeout), timeout_focus.approval_timeout), "Automatically reject unanswered approvals · 0 disables" } },
            { id = "answer_timeout", label = "Answer timeout", columns = { "Answer timeout", duration.render(duration.value(active.answer_timeout), timeout_focus.answer_timeout), "Automatically answer questions using recommended defaults · 0 disables" } },
            { id = "shell", label = "Shell", columns = { "Shell", picker_field.render(active.shell == "nushell" and "Nushell" or "System", true), "Accepted checks run locally · Nushell available when installed" } },
            { id = "default-write-permission", label = "Default write permission", columns = { "Default write permission", picker_field.render(active.default_write_permission or "write", true), "Applied to new goals and accepted plans" } },
            { id = "plan-permission", label = "Plan permission", columns = { "Plan permission", picker_field.render(type(active.plan_permission) == "string" and active.plan_permission or "Keep current", true), "Applied when starting a planning task" } },
            { id = "plan-revisions", label = "Auto-approve plan revisions", columns = { "Auto-approve plan revisions", picker_field.render(active.plan_auto_approve_revisions ~= false and "On" or "Off", true), "Accept validated execution revisions automatically" } },
            { id = "subagent-tools", label = "Subagent tools", columns = { "Subagent tools", picker_field.render((active.subagent_tools == nil and state.subagent_tools or active.subagent_tools) == true and "On" or "Off",true), "Root model delegation · manual spawning remains available" } },
            { id = "lsp-tools", label = "LSP tools", columns = { "LSP tools", picker_field.render((active.lsp_tools == nil and state.lsp_tools or active.lsp_tools) == true and "On" or "Off", true), "Read-only agent queries through editor language servers" } },
            { id = "status-thoughts", label = "Status thoughts", columns = { "Status thoughts", picker_field.render(active.status_thoughts and "On" or "Off", true), "Show the latest thought under live status · timeline unchanged" } },
            { id = "sandbox", label = "Sandbox", columns = { "Sandbox", supported and picker_field.render(policy.sandbox and "Enabled" or "Disabled", true)
              or "Disabled (unsupported)", "OS isolation, independent of approval mode" } },
            { id = "read-access", label = "Read access", columns = { "Read access", picker_field.render(policy.read_access, true), "Approval scope for reported file reads · not OS confinement" } },
            { id = "readable-directories", label = "Additional readable directories", columns = { "Additional readable directories", tostring(#policy.readable_directory), "Enter to add, edit, or remove" } },
            { id = "write-access", label = "Write access", columns = { "Write access", picker_field.render(policy.write_access, true), "Approval scope for reported file writes · independent of sandbox" } },
            { id = "writable-directories", label = "Additional writable directories", columns = { "Additional writable directories", tostring(#policy.writable_directory), "Enter to add, edit, or remove" } },
            { id = "windows-sandbox", label = "Windows sandbox", columns = { "Windows sandbox", picker_field.render(policy.windows_sandbox, true), "Windows only · MXC requires host support" } },
            { id = "logging", label = "Logging", columns = { "Logging", picker_field.render(status.enabled and "On" or "Off", true), "Save session log" } },
            { id = "provider", label = "Provider", columns = { "Provider", cli or "Unknown",
              require("forge.views.harness").backend_switch_available() and "Choose a new chat or resume a session" or "Finish pending work to switch" } },
            { id = "native-import", label = "Import native configuration", columns = { "Import native configuration", "Preview / select", "Import skills or MCP definitions into Forge settings" } },
            provider_setup,
          },
          footer = "←→ change setting  Tab amount/unit  Enter edit/select  q close",
        } },
        action_list = {
          { id = "previous-value", command = "previous_value", callback = function(context) toggle(context, -1) end },
          { id = "next-value", command = "next_value", callback = function(context) toggle(context, 1) end },
          { key = "<Tab>", modes = { "n", "i" }, desc = "Select timeout amount or unit", callback = function(context)
            local id = context.option and context.option.id
            if not timeout_focus[id] or pending then return end
            timeout_focus[id] = timeout_focus[id] == "amount" and "unit" or "amount"
            picker.update(build_spec())
          end },
        },
        on_confirm = function(result)
          local id = result.option.id
          if commit and commit:open(id) then return false end
          if timeout_focus[id] and timeout_focus[id] == "amount" then
            if pending then return false end
            vim.ui.input({ prompt = result.option.label .. " amount (0 = Off): ", default = tostring(duration.value(active[id]).amount) }, function(text)
              if text == nil then return end
              local amount = tonumber(text)
              if not text:match("^%d+$") or not amount or amount > 4294967295 then
                notifications.error("Enter a whole number from 0 to 4294967295", "Harness timeout")
                return
              end
              local value = duration.value(active[id])
              value.amount = amount
              configure({ [id] = value })
            end)
            return false
          end
          if result.option.id == "native-import" then
            require("forge.views.harness.native_import").open(state, host)
            return false
          end
          if result.option.id == "provider-setup" then
            require("forge.views.harness.provider_setup").open({ host = host, session_id = session_id })
            return false
          end
          if result.option.id == "provider" then
            require("forge.views.harness.controller").select_backend()
            return false
          end
          toggle({ option = result.option })
          return false
        end,
      }
    end
    commit = require("forge.views.harness.background_configuration").new({
      backend = active.backend, session_id = session_id, host = host,
      current = function() return M._revision == owner_revision and state.session ~= nil and state.session.id == session_id end,
      changed = function() if commit and picker.is_open("harness-config") then picker.update(build_spec()) end end,
      reopen = function() M.open(state, host) end,
    })
    picker.open(build_spec())
  end)
end

return M
