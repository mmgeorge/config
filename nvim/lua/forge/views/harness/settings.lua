local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
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
    ---@type table<string, any>
    local active = vim.tbl_extend("force", assert(state.session), state.configuring_config or {},
      state.task_config or {})
    active.status_thoughts = state.status_thoughts == true
    active.check_timing = state.check_timing or "after_queue"
    active.automatic_checks = state.automatic_checks ~= false
    local configuration_revision = 0
    local cli = active.provider_label or ({ codex = "Codex CLI", copilot = "Copilot CLI" })[active.backend] or active.backend
    local build_spec
    local commit
    local timeout_focus = { approval_timeout = "amount", answer_timeout = "amount" }
    local access = require("forge.views.harness.access")
    local function configure(params, completed)
      if not state.session or state.session.id ~= session_id then return end
      configuration_revision = configuration_revision + 1
      local revision = configuration_revision
      active = vim.tbl_extend("force", active, params)
      local session = require("forge.session")
      local previous = session.harness
      session.activate_harness(state)
      require("forge.views.harness.controller").configure(params, false, function(applied)
        if not state.session or state.session.id ~= session_id then return end
        if revision ~= configuration_revision then return end
        active = vim.tbl_extend("force", assert(state.session), state.configuring_config or {},
          state.task_config or {})
        active.status_thoughts = state.status_thoughts == true
        active.check_timing = state.check_timing or "after_queue"
        active.automatic_checks = state.automatic_checks ~= false
        if applied and params.session_tools ~= nil then active.session_tools = params.session_tools end
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
        elseif id == "write-access" then
          local values = { "off", "workspace", "full" }
          local position = 1
          for index, value in ipairs(values) do if value == policy.write_access then position = index end end
          policy.write_access = values[(position - 1 + (direction or 1)) % #values + 1]
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
      if id == "default-approval" or id == "plan-approval" then
        local field = id == "default-approval" and "default_approval" or "plan_approval"
        local values = field == "plan_approval" and { "keep", "ask", "deny", "allow" } or { "ask", "deny", "allow" }
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
      elseif id == "session-tools" then
        configure({ session_tools = not (active.session_tools ~= false and (active.session_tools ~= nil or state.session_tools ~= false)) })
      elseif id == "status-thoughts" then
        configure({ status_thoughts = active.status_thoughts ~= true })
      elseif id == "check-timing" then
        configure({ check_timing = active.check_timing == "between_work" and "after_queue" or "between_work" })
      elseif id == "automatic-checks" then
        configure({ automatic_checks = active.automatic_checks == false })
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
    local function current()
      return M._revision == owner_revision and state.session ~= nil and state.session.id == session_id
    end
    local function redraw()
      if current() and picker.is_open("harness-config") then picker.update(build_spec()) end
    end
    local function model_descriptor()
      return require("forge.views.harness.model_options").find(commit and commit.model_list, active.model)
    end
    local function change_main(id, direction)
      local function cycle(values, value)
        if #values == 0 then return value end
        local position = math.max(1, vim.fn.index(values, value) + 1)
        return values[(position - 1 + direction) % #values + 1]
      end
      if id == "main-model" then
        local models = commit and commit.model_list or {}
        local values = vim.tbl_map(function(model) return model.id end, models)
        local selected = cycle(values, active.model)
        for _, model in ipairs(models) do
          if model.id == selected then
            local speeds = require("forge.views.harness.model_options").speeds(model, state.capability or {})
            configure({ model = selected, effort = model.default_reasoning or (model.reasoning or {})[1] or active.effort,
              service_tier = vim.tbl_contains(speeds, active.service_tier or "default") and active.service_tier or "default" })
            return
          end
        end
      elseif id == "main-thinking" then
        local model = model_descriptor()
        configure({ effort = cycle(model and model.reasoning or {}, active.effort) })
      elseif id == "main-speed" then
        local values = require("forge.views.harness.model_options").speeds(model_descriptor(), state.capability or {})
        configure({ service_tier = cycle(values, active.service_tier or "default") })
      end
    end
    local function activate(id)
      if commit and id:match("^background%-") and commit:open(id) then return end
      if id == "main-model" then
        local models = vim.deepcopy(commit and commit.model_list or {})
        for _, model in ipairs(models) do
          if model.id == active.model then model.selected_reasoning = active.effort end
        end
        picker.close(false)
        require("forge.views.harness.model_picker").open({ host = host, model_list = models,
          backend = active.backend, current_model = active.model, is_current = current,
          on_close = function() if current() then M.open(state, host) end end,
          on_confirm = function(value)
            picker.close(false)
            local model = require("forge.views.harness.model_options").find(models, value.model)
            local speeds = require("forge.views.harness.model_options").speeds(model, state.capability or {})
            configure({ model = value.model, effort = value.effort, context_window = value.context_window,
              service_tier = vim.tbl_contains(speeds, active.service_tier or "default") and active.service_tier or "default" },
              function() if current() then M.open(state, host) end end)
          end })
      elseif id == "native-import" then
        require("forge.views.harness.native_import").open(state, host)
      elseif id == "provider-setup" then
        require("forge.views.harness.provider_setup").open({ host = host, session_id = session_id })
      elseif id == "provider" then
        require("forge.views.harness.controller").select_backend()
      elseif id:match("^main%-") then
        change_main(id, 1)
      else
        toggle({ option = { id = id } }, 1)
      end
    end
    build_spec = function()
      local policy = access.policy(active)
      local supported = state.capability and state.capability.os_sandbox == true
      local background = commit and commit:fields() or {}
      local background_fields = { background.model, background.thinking }
      if background.speed then background_fields[#background_fields + 1] = background.speed end
      local capability = state.capability or {}
      ---@param id string
      ---@param kind 'choice'|'toggle'|'input'|'action'
      ---@param value string|boolean
      ---@param label string?
      ---@param description string
      ---@param disabled string?
      ---@param muted boolean?
      ---@return ForgePickerField
      local function field(id, kind, value, label, description, disabled, muted)
        return { id = id, kind = kind, value = value, label = label, description = description,
          disabled_reason = disabled, muted = muted }
      end
      local catalog_disabled = not (commit and commit.model_list) and (commit and commit.failure or "Loading models") or nil
      local main = {
        field("main-model", "choice", active.model or "Default", nil, "Model for the main conversation and task execution.",
          not capability.model_selection and "Model selection is unavailable for this provider" or catalog_disabled),
        field("main-thinking", "choice", active.effort or "Default", "Thinking", "Reasoning effort for the selected main model.",
          not capability.effort_selection and "Reasoning effort selection is unavailable for this provider" or catalog_disabled),
      }
      if #require("forge.views.harness.model_options").speeds(model_descriptor(), capability) > 1
        or active.service_tier and active.service_tier ~= "default" then
        main[#main + 1] = field("main-speed", "choice", active.service_tier or "default", "Speed", "Provider service tier for the main conversation.")
      end
      local compaction = active.auto_compaction or { enabled = false, threshold_tokens = 128000 }
      local sandbox = {
        field("sandbox", "toggle", policy.sandbox, "Enabled", "OS isolation, independent of approval mode.",
          not supported and "OS sandbox configuration is unavailable for this provider" or nil),
      }
      if vim.fn.has("win32") == 1 then
        sandbox[#sandbox + 1] = field("windows-sandbox", "choice", policy.windows_sandbox, "Windows mode", "Windows sandbox implementation. MXC requires host support.",
          not supported and "OS sandbox configuration is unavailable for this provider" or nil)
      end
      local approval, answer = duration.value(active.approval_timeout), duration.value(active.answer_timeout)
      local approval_unit = field("approval_timeout:unit", "choice", approval.unit, nil, "Approval timeout unit: seconds, minutes, or hours.")
      local answer_unit = field("answer_timeout:unit", "choice", answer.unit, nil, "Answer timeout unit: seconds, minutes, or hours.")
      approval_unit.join_previous, answer_unit.join_previous = true, true
      local provider_configuration = {
        field("native-import", "action", "Import…", nil, "Preview and select native skills or MCP definitions to import into Forge settings."),
      }
      if active.backend == "claude" or active.backend == "antigravity" then
        provider_configuration[#provider_configuration + 1] = field("provider-setup", "action", "Install / sign in…", nil, "Manage the ACP runtime and OAuth login.")
      end
      ---@param id string
      ---@param section string
      ---@param label string
      ---@param fields ForgePickerField[]
      ---@param inline_fields? boolean
      ---@return table
      local function row(id, section, label, fields, inline_fields)
        return { id = id, section = section, label = label, fields = fields, inline_fields = inline_fields }
      end
      return {
        owner = "harness-config", host = host, height_ratio = 0.9, fit_content = true,
        page_list = { {
          id = "config", title = "Configuration",
          field_navigation = true, highlight_selected_text = false, compact_sections = true,
          option_list = {
            row("main-model", "Models", "Main", main),
            row("background-model", "Models", "Background", background_fields),
            row("main-compaction", "Compaction", "Main", {
              field("auto-compact", "toggle", compaction.enabled, "Automatic", "Compact the main conversation after completed turns.",
                not capability.native_compact and "Compaction is unavailable for this provider" or nil),
              field("compact-threshold", "input", tostring(compaction.threshold_tokens), "Threshold", "Main compaction threshold in provider-reported context tokens.", nil, not compaction.enabled),
            }),
            row("background-compaction", "Compaction", "Background", { background.compact, background.threshold }),
            row("check-execution", "Checks", "Execution", {
              field("shell", "choice", active.shell == "nushell" and "Nushell" or "System", "Shell", "Shell used for saved checks. Nushell is available when installed."),
            }),
            row("automatic-checks", "Checks", "Automatic", {
              field("automatic-checks", "toggle", active.automatic_checks ~= false, "Enabled", "Run applicable checks when work completes. Explicit runs and plan Checking remain available when disabled."),
              field("check-timing", "choice", active.check_timing == "between_work" and "Between queued work" or "After queue", "Timing", "Run checks after each work item or once the queue finishes."),
            }),
            row("check-editing", "Checks", "Setup", {
              field("check-editing", "toggle", state.check_editing == true, "Enabled", "Allow the agent to edit project checks. Changes permission without sending a prompt."),
            }),
            row("plan-approval", "Planning", "Approval", {
              field("plan-approval", "choice", ({ ask = "ask", deny = "deny", allow = "allow" })[active.plan_approval] or "Keep current", nil, "Approval mode applied when starting a planning task."),
            }),
            row("plan-revisions", "Planning", "Auto-approve", {
              field("plan-revisions", "toggle", active.plan_auto_approve_revisions ~= false, "Auto-approve", "Automatically accept validated execution plan revisions."),
            }, true),
            row("execute-approval", "Execute", "Approval", {
              field("default-approval", "choice", active.default_approval or "ask", nil, "Approval mode applied to new goals and accepted plan execution. Choose ask, deny, or allow without changing filesystem access."),
            }),
            row("read-access", "Permissions", "Read", {
              field("read-access", "choice", policy.read_access, nil, "Filesystem scope for reported file reads. This does not configure OS read confinement."),
              field("readable-directories", "action", tostring(#policy.readable_directory) .. "…", "Extra directories", "Add, edit, or remove additional readable directories."),
            }),
            row("write-access", "Permissions", "Write", {
              field("write-access", "choice", policy.write_access, nil, "Filesystem scope for reported file writes. Off rejects writes in every approval mode."),
              field("writable-directories", "action", tostring(#policy.writable_directory) .. "…", "Extra directories", "Add, edit, or remove additional writable directories."),
            }),
            row("sandbox", "Permissions", "Sandbox", sandbox),
            row("timeouts", "Tools", "Timeouts", {
              field("approval_timeout:amount", "input", tostring(approval.amount), "Approval", "Automatically reject unanswered approvals after this duration. Set 0 to disable."),
              approval_unit,
              field("answer_timeout:amount", "input", tostring(answer.amount), "Answer", "Automatically answer questions with recommended defaults after this duration. Set 0 to disable."),
              answer_unit,
            }),
            row("subagent-tools", "Tools", "Subagents", {
              field("subagent-tools", "toggle", (active.subagent_tools == nil and state.subagent_tools or active.subagent_tools) == true, "Subagents", "Allow root model delegation. Manual spawning remains available."),
            }, true),
            row("lsp-tools", "Tools", "LSP", {
              field("lsp-tools", "toggle", (active.lsp_tools == nil and state.lsp_tools or active.lsp_tools) == true, "LSP", "Read-only agent queries through editor language servers."),
            }, true),
            row("session-tools", "Tools", "Session history", {
              field("session-tools", "toggle", active.session_tools ~= false and (active.session_tools ~= nil or state.session_tools ~= false), "Sessions", "Read-only Harness history tools. Configuration inspection and the Harness skill remain available when off."),
            }, true),
            row("status-thoughts", "Display", "Thoughts", {
              field("status-thoughts", "toggle", active.status_thoughts == true, "Thoughts", "Show the latest thought under live status without changing timeline messages."),
            }, true),
            row("provider", "Provider", "Connection", {
              field("provider", "action", cli or "Unknown", nil, "Choose a new chat or resume a session with another provider."),
            }),
            row("provider-configuration", "Provider", "Configuration", provider_configuration),
            row("logging", "Debug", "Logging", {
              field("logging", "toggle", status.enabled == true, "Logging", "Save the session log."),
            }, true),
          },
          footer = "↑↓ row  Tab/S-Tab field  ←→ value  Space toggle  Enter edit/open  q close",
        } },
        on_field_action = function(context, action, direction)
          if not current() or pending then return end
          local id = context.field.id
          if id == "check-editing" then
            pending = true
            client.request_for(session_id, "checks.editing", { enabled = not state.check_editing }, function(result, failure)
              if not current() then return end
              pending = false
              if failure then notifications.error(failure, "Harness checks") else state.check_editing = result.check_editing == true end
              require("forge.views.harness.controller").refresh_winbar()
              redraw()
            end)
            return
          end
          local timeout, focus = id:match("^([^:]+):([^:]+)$")
          if timeout then
            timeout_focus[timeout] = focus
            if action == "activate" and focus == "amount" then
              vim.ui.input({ prompt = (timeout == "approval_timeout" and "Approval" or "Answer") .. " timeout amount (0 disables): ",
                default = tostring(duration.value(active[timeout]).amount) }, function(text)
                if not text or not current() then return end
                local amount = tonumber(text)
                if not text:match("^%d+$") or not amount or amount > 4294967295 then notifications.error("Enter a whole number from 0 to 4294967295", "Harness timeout") return end
                local value = duration.value(active[timeout])
                value.amount = amount
                configure({ [timeout] = value })
              end)
            else toggle({ option = { id = timeout } }, direction) end
          elseif action == "activate" then activate(id)
          elseif id == "compact-threshold" then
            local policy = vim.deepcopy(active.auto_compaction or { enabled = false, threshold_tokens = 128000 })
            policy.threshold_tokens = math.max(1, policy.threshold_tokens + direction)
            configure({ auto_compaction = policy })
          elseif id:match("^main%-") then change_main(id, direction)
          else toggle({ option = { id = id } }, direction) end
        end,
      }
    end
    commit = require("forge.views.harness.background_configuration").new({
      backend = active.backend, session_id = session_id, host = host,
      capability = function() return state.capability or {} end,
      current = function() return M._revision == owner_revision and state.session ~= nil and state.session.id == session_id end,
      changed = function() if commit and picker.is_open("harness-config") then picker.update(build_spec()) end end,
      reopen = function() M.open(state, host) end,
    })
    picker.open(build_spec())
  end)
end

return M
