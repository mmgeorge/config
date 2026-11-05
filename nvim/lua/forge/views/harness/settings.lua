local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local duration = require("forge.views.harness.duration")
local threshold = require("forge.views.harness.compaction_threshold")
local notifications = require("forge.infra.notifications")
local log_buffer_by_path = {}
local log_revision_by_buffer = {}
---@type table<string, ForgeHarnessLogStatus>
local log_status_by_path = {}

--- Category ownership for the existing setting sections, in display order.
local configuration_category_list = {
  { id = "models", title = "Models" },
  { id = "workflow", title = "Workflow" },
  { id = "permissions", title = "Permissions" },
  { id = "tools", title = "Tools" },
  { id = "system", title = "System" },
}

local configuration_field_description = {
  ["main-model"] = "Used for chat, planning, and execution.",
  ["main-thinking"] = "How much reasoning the main model uses.",
  ["main-speed"] = "Service tier for main-model requests.",
  ["ask-override"] = "By default, Ask uses the main model used for planning. Enable Override to choose separate settings.",
  ["ask-model"] = "Model used for Ask requests when Override is enabled.",
  ["ask-thinking"] = "How much reasoning the Ask model uses.",
  ["ask-speed"] = "Service tier for Ask requests.",
  ["background-model"] = "Used for background requests.",
  ["background-thinking"] = "How much reasoning the background model uses.",
  ["background-speed"] = "Service tier for background-model requests.",
  ["auto-compact"] = "Summarize older session context as the conversation grows.",
  ["compact-threshold"] = "Context size in tokens that triggers automatic session compaction.",
  ["background-compact"] = "Automatically compact context for background requests.",
  ["background-threshold"] = "Context size in tokens that triggers automatic background compaction.",
  ["shell"] = "Shell used to run project checks.",
  ["check-editing"] = "Allow the assistant to edit project checks. Also controlled by /checks edit on/off.",
  ["automatic-checks"] = "Automatically run configured project checks.",
  ["check-timing"] = "Run checks between queued work items or after the queue finishes.",
  ["approval-mode"] = "Tool approval policy: Ask prompts for requests not covered by rules; Deny rejects them; Approve allows them. Explicit deny rules still apply.",
  ["read-access"] = "Directories the assistant may read.",
  ["readable-directories"] = "Edit additional readable directories, one directory per line.",
  ["write-access"] = "Directories the assistant may modify.",
  ["writable-directories"] = "Edit additional writable directories, one directory per line.",
  ["approval_timeout"] = "Time allowed for approval. Enter to edit; Up/Down adjust from 15 seconds to 60 minutes.",
  ["tool_timeout"] = "Deadline for each tool call and shell command. Expiry stops execution with an error. Enter to edit.",
  ["subagent-tools"] = "Allow the assistant to delegate work to subagents.",
  ["lsp-tools"] = "Allow the assistant to use language-server tools.",
  ["session-tools"] = "Allow the assistant to search and read session history.",
  ["status-thoughts"] = "Show model thoughts in the status display.",
  ["provider"] = "Choose the provider used by this session.",
  ["provider-setup"] = "Configure or sign in to this provider.",
  ["logging"] = "Record Harness trace events for debugging.",
}

--- Partition settings into category pages, keeping only compound values in one row.
---@param row_list table[]
---@return ForgePickerPage[]
local function configuration_pages(row_list)
  local page_list, page_by_category = {}, {}
  for _, category in ipairs(configuration_category_list) do
    local page = {
      id = category.id, title = category.title, option_list = {},
      field_navigation = true, field_description_height = 2, highlight_selected_text = false,
      wrap_selection = false, toggle_on_confirm = true,
    }
    page_list[#page_list + 1] = page
    page_by_category[category.id] = page
  end
  for _, row in ipairs(row_list) do
    local page = assert(page_by_category[row.category], "Configuration row has no category: " .. tostring(row.category))
    for _, source in ipairs(row.fields) do
      local field = vim.deepcopy(source)
      field.description = assert(field.description or configuration_field_description[field.id], "Configuration field requires a description: " .. field.id)
      if field.join_previous then
        local previous = assert(page.option_list[#page.option_list], "Compound setting requires a preceding field")
        previous.fields[#previous.fields + 1] = field
      else
        local label = field.label and field.label ~= "Enabled" and field.label or row.label
        if field.id == "provider-setup" then label = "Provider setup" end
        field.label = nil
        page.option_list[#page.option_list + 1] = {
          id = field.id, section = row.section, label = label, fields = { field },
        }
      end
    end
  end
  return page_list
end

local function cycle(values, value, direction)
  if #values == 0 then return value end
  local position = math.max(1, vim.fn.index(values, value) + 1)
  return values[(position - 1 + (direction or 1)) % #values + 1]
end

---@class ForgeAskModel
---@field model? string
---@field effort? string
---@field service_tier? string

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

---@class ForgeHarnessLogEntry
---@field record? table
---@field failure? string
---@field source? string

---Reassemble contiguous host log chunks without exposing transport envelopes as events.
---@param source string[]
---@return ForgeHarnessLogEntry[]
local function log_entries(source)
  local entries = {}
  ---@type {id: string, count: integer, next: integer, pieces: string[], damaged: boolean}?
  local pending
  local function incomplete(reason)
    if not pending then return end
    entries[#entries + 1] = { failure = "Incomplete retained log record " .. pending.id .. ": " .. reason }
    pending = nil
  end
  for _, line in ipairs(source) do
    local ok, record = pcall(vim.json.decode, line)
    if ok and type(record) == "table" and record.trace_chunk ~= nil then
      local part = record.trace_chunk
      if type(part) ~= "table" or type(part.id) ~= "string" or part.id == ""
        or type(part.sequence) ~= "number" or part.sequence < 0 or part.sequence % 1 ~= 0
        or type(part.part_count) ~= "number" or part.part_count < 1 or part.part_count % 1 ~= 0
        or part.sequence >= part.part_count or type(part.payload_hex) ~= "string"
        or #part.payload_hex % 2 ~= 0 or not part.payload_hex:match("^[%x]*$") then
        incomplete("malformed continuation")
        entries[#entries + 1] = { failure = "Incomplete retained log record: malformed chunk" }
      else
        if pending and (pending.id ~= part.id or part.sequence == 0) then incomplete("missing continuation") end
        if not pending then
          pending = { id = part.id, count = part.part_count, next = 0, pieces = {}, damaged = false }
        end
        if part.part_count ~= pending.count or part.sequence ~= pending.next then pending.damaged = true end
        pending.next = part.sequence + 1
        pending.pieces[#pending.pieces + 1] = part.payload_hex:gsub("..", function(byte)
          return string.char(tonumber(byte, 16))
        end)
        if pending.next == pending.count then
          if pending.damaged then incomplete("missing or out-of-order fragments")
          else
            local decoded, complete = pcall(vim.json.decode, table.concat(pending.pieces))
            if decoded and type(complete) == "table" then
              entries[#entries + 1] = { record = complete }
              pending = nil
            else incomplete("invalid reconstructed JSON") end
          end
        end
      end
    elseif line ~= "" then
      incomplete("missing continuation")
      if ok and type(record) == "table" then entries[#entries + 1] = { record = record }
      else entries[#entries + 1] = { failure = "Invalid JSONL record", source = line } end
    end
  end
  incomplete("missing continuation")
  return entries
end

---Read every retained segment in chronological order and track rotation in the cache revision.
---@param path string
---@return {path: string, present: boolean}[]?, string?
local function log_segments(path)
  local segments, revision = {}, {}
  for index = 10, 0, -1 do
    local segment = index > 0 and (path .. "." .. index) or path
    local metadata, failure = vim.uv.fs_stat(segment)
    if not metadata and failure and not failure:find("ENOENT", 1, true) then
      notifications.error(failure, "Harness logging")
      return nil
    end
    segments[#segments + 1] = { path = segment, present = metadata ~= nil }
    revision[#revision + 1] = metadata
      and string.format("%d:%d:%d", metadata.size, metadata.mtime.sec, metadata.mtime.nsec) or "missing"
  end
  return segments, table.concat(revision, ":")
end

---@param path string
---@param buffer integer
---@param force? boolean
local function refresh_log(path, buffer, force)
  local segments, revision = log_segments(path)
  if not segments or not force and revision == log_revision_by_buffer[buffer] then return end
  local read_ok, source = pcall(function()
    local source = {}
    for _, segment in ipairs(segments) do
      if segment.present then vim.list_extend(source, vim.fn.readfile(segment.path)) end
    end
    return source
  end)
  if not read_ok then notifications.error(tostring(source), "Harness logging") return end
  local lines = {}
  for index, entry in ipairs(log_entries(source)) do
    local record = entry.record
    if record then
      local timestamp = type(record.timestamp_ms) == "number"
          and os.date("%Y-%m-%d %H:%M:%S", math.floor(record.timestamp_ms / 1000)) or "unknown time"
      local event = type(record.event) == "string" and record.event or "unknown event"
      lines[#lines + 1] = string.format("%s  %s  #%d", timestamp, event, index)
      local payload = record.payload == nil and vim.NIL or record.payload
      for _, payload_line in ipairs(formatted_json(payload)) do lines[#lines + 1] = "  " .. payload_line end
      lines[#lines + 1] = ""
    else
      lines[#lines + 1] = entry.failure .. " #" .. index
      if entry.source then
        lines[#lines + 1] = entry.source
        notifications.error(string.format("Invalid JSONL record #%d in %s", index, path), "Harness logging")
      end
      lines[#lines + 1] = ""
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

---@class ForgeHarnessLogStatus
---@field enabled boolean
---@field path? string
---@field rejected_record_count? integer

---Describe host-reported diagnostic loss beside the log's recording state.
---@param status ForgeHarnessLogStatus
---@return string
local function log_title(status)
  local rejected = status.rejected_record_count or 0
  local loss = rejected > 0 and (" • %d record%s rejected"):format(rejected, rejected == 1 and "" or "s") or ""
  return "Harness log • " .. (status.enabled and "On" or "Off") .. loss .. " • R refresh • q close"
end

---Adopt host logging status and refresh every visible viewer of that log.
---@param status ForgeHarnessLogStatus
function M.adopt(status)
  require("forge.infra.perf").setup({ harness = { enabled = status.enabled } })
  if not status.path then return end
  log_status_by_path[status.path] = vim.deepcopy(status)
  local buffer = log_buffer_by_path[status.path]
  if not buffer or not vim.api.nvim_buf_is_valid(buffer) then return end
  for _, window in ipairs(vim.fn.win_findbuf(buffer)) do vim.wo[window].winbar = log_title(status) end
end

local function request(session_id, method, params, callback)
  client.request({ session_id = session_id, method = method, params = params }, function(status, failure)
    if failure then notifications.error(failure, "Harness logging") return end
    M.adopt(status)
    if callback then callback(status) end
  end)
end

local function open_log(status, session_id)
  local title = log_title(log_status_by_path[status.path] or status)
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
  vim.keymap.set("n", "R", function()
    request(session_id, "host.trace.status", {}, function()
      if vim.api.nvim_buf_is_valid(buffer) then refresh_log(status.path, buffer) end
    end)
  end, { buffer = buffer, desc = "Refresh Harness log" })
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
    request(session_id, "host.trace.status", {}, function(status) open_log(status, session_id) end)
  elseif action == "on" or action == "off" then
    request(session_id, "host.trace.configure", { enabled = action == "on" })
  elseif action == "clear" then
    request(session_id, "session.trace.clear", {}, function(status)
      local buffer = log_buffer_by_path[status.path]
      if buffer and vim.api.nvim_buf_is_valid(buffer) then refresh_log(status.path, buffer, true) end
    end)
  else
    notifications.error("Use /log [on|off|open|clear]", "Harness logging")
  end
end

---@param state ForgeHarnessPresentationState
---@param host table
---@param resume_state? ForgePickerResumeState State captured before a child editor opened.
function M.open(state, host, resume_state)
  if not state.session then return end
  local session_id = state.session.id
  M._revision = (M._revision or 0) + 1
  local owner_revision = M._revision
  request(session_id, "host.trace.status", {}, function(status)
    if M._revision ~= owner_revision or not state.session or state.session.id ~= session_id then return end
    local pending = false
    ---@type table<string, any>
    local active = vim.deepcopy(assert(state.session))
    active.access = state.access_preference or active.access
    active.status_thoughts = state.status_thoughts == true
    active.check_timing = state.check_timing or "after_queue"
    active.automatic_checks = state.automatic_checks == true
    local configuration_revision = 0
    local cli = active.provider_label or active.provider
    local build_spec
    local commit
    local current
    local return_state = resume_state
    --- Re-enter only after the owned child closes and the original session remains current.
    local function reopen()
      if current() and not picker.is_open() then M.open(state, host, return_state) end
    end
    ---@type ForgeAskModel
    local ask = vim.deepcopy(state.ask_model or {})
    local access = require("forge.views.harness.access")
    local function configure(params, completed)
      if not current() then if completed then completed(false) end return end
      configuration_revision = configuration_revision + 1
      local revision = configuration_revision
      active = vim.tbl_extend("force", active, params)
      local session = require("forge.session")
      local previous = session.harness
      session.activate_harness(state)
      require("forge.views.harness.controller").configure(params, function(applied)
        if not current() or revision ~= configuration_revision then
          if completed then completed(false) end
          return
        end
        active = vim.deepcopy(assert(state.session))
        if applied and params.access then state.access_preference = vim.deepcopy(params.access) end
        active.access = state.access_preference or active.access
        active.status_thoughts = state.status_thoughts == true
        active.check_timing = state.check_timing or "after_queue"
        active.automatic_checks = state.automatic_checks == true
        if applied and params.session_tools ~= nil then active.session_tools = params.session_tools end
        if completed then completed(applied) end
        if current() and picker.is_open("harness-config") then picker.update(build_spec()) end
      end)
      session.activate_harness(previous)
      if current() and picker.is_open("harness-config") then picker.update(build_spec()) end
    end
    local function apply_access(policy, completed)
      configure({ access = policy }, completed)
    end
    local function set_approval_mode(mode)
      local previous_mode = active.approval_mode or "ask"
      active.approval_mode, pending = mode, true
      local session = require("forge.session")
      local previous = session.harness
      session.activate_harness(state)
      require("forge.views.harness.controller").task_transition({
        action = "permission", change = { kind = "set_approval", mode = mode },
      }, nil, function(applied)
        if not current() then return end
        pending = false
        active.approval_mode = applied and mode or previous_mode
        if picker.is_open("harness-config") then picker.update(build_spec()) end
      end)
      session.activate_harness(previous)
      if current() and picker.is_open("harness-config") then picker.update(build_spec()) end
    end
    local function configure_ask(selection, completed)
      pending = true
      client.request({ session_id = session_id, method = "session.configure", params = { ask_model = selection } }, function(result, failure)
        if not current() then return end
        pending = false
        if failure then notifications.error(failure, "Ask model")
        else
          ask = result.ask_model or {}
          state.ask_model = vim.deepcopy(ask)
        end
        if completed then completed(failure == nil) end
        if current() and picker.is_open("harness-config") then picker.update(build_spec()) end
      end)
    end
    local function toggle(context, direction)
      if pending or not context.option then return end
      local id = context.option.id
      if id == "ask-override" then
        if ask.model then configure_ask(vim.empty_dict()) return end
        local models = commit and commit.model_list or {}
        local model = require("forge.views.harness.model_options").find(models, active.resolved_model or active.model)
        if not model then notifications.error("The main model is not available in the provider model list", "Ask model") return end
        local speeds = require("forge.views.harness.model_options").speeds(model, state.capability or {})
        configure_ask({ model = model.id,
          effort = vim.tbl_contains(model.reasoning or {}, active.effort) and active.effort or model.default_reasoning,
          service_tier = vim.tbl_contains(speeds, active.service_tier or "default") and (active.service_tier or "default") or "default" })
        return
      end
      if id == "ask-model" or id == "ask-thinking" or id == "ask-speed" then
        if not ask.model then return end
        local models = commit and commit.model_list or {}
        local model_options = require("forge.views.harness.model_options")
        local model = model_options.find(models, ask.model)
        local selection = vim.deepcopy(ask)
        if id == "ask-model" then
          local model_ids = vim.tbl_map(function(candidate) return candidate.id end, models)
          selection.model = cycle(model_ids, ask.model, direction)
          model = model_options.find(models, selection.model)
          selection.effort = model and (model.default_reasoning or (model.reasoning or {})[1]) or nil
          local speeds = model_options.speeds(model, state.capability or {})
          if not vim.tbl_contains(speeds, selection.service_tier or "default") then selection.service_tier = "default" end
        elseif id == "ask-thinking" then
          selection.effort = cycle(model and model.reasoning or {}, ask.effort, direction)
        else
          selection.service_tier = cycle(model_options.speeds(model, state.capability or {}), ask.service_tier or "default", direction)
        end
        configure_ask(selection)
        return
      end
      if commit and commit:change(id, direction or 1) then return end
      if id == "auto-compact" then
        local policy = vim.deepcopy(active.auto_compaction or { enabled = false, threshold_tokens = 128000 })
        if not (state.capability and state.capability.native_compact) then notifications.error("This provider does not support compaction", "Harness") return end
        policy.enabled = not policy.enabled
        configure({ auto_compaction = policy })
        return
      end
      if id == "approval_timeout" or id == "tool_timeout" then
        configure({ [id] = duration.change(active[id], direction or 1) })
        return
      end
      if id == "sandbox" or id == "read-access" or id == "write-access" or id == "isolation-mode" then
        local policy = access.policy(active)
        if id == "sandbox" then policy.sandbox = not policy.sandbox
        elseif id == "read-access" then policy.read_access = policy.read_access == "workspace" and "full" or "workspace"
        elseif id == "write-access" then
          local values = { "off", "workspace", "full" }
          local position = 1
          for index, value in ipairs(values) do if value == policy.write_access then position = index end end
          policy.write_access = values[(position - 1 + (direction or 1)) % #values + 1]
        else
          local values = vim.tbl_map(function(mode) return mode.id end, ((state.capability or {}).integration or {}).isolation_modes or {})
          if #values == 0 then return end
          local position = 1
          for index, value in ipairs(values) do if value == policy.isolation_mode then position = index end end
          policy.isolation_mode = values[(position - 1 + (direction or 1)) % #values + 1]
        end
        apply_access(policy)
        return
      elseif id == "readable-directories" or id == "writable-directories" then
        return_state = picker.capture_state()
        picker.close(false)
        access.directories(state, apply_access, reopen,
          id == "readable-directories" and "read" or "write")
        return
      end
      if id == "approval-mode" then
        set_approval_mode(cycle({ "ask", "deny", "allow" }, active.approval_mode or "ask", direction or 1))
        return
      end
      if id == "shell" then
        local values = { "system" }
        if vim.fn.executable("nu") == 1 then values[#values + 1] = "nushell" end
        local position = active.shell == "nushell" and #values or 1
        configure({ shell = values[(position - 1 + (direction or 1)) % #values + 1] })
      elseif id == "check-editing" then
        pending = true
        client.request({ session_id = session_id, method = "session.check.editing.set",
          params = { enabled = not state.check_editing } }, function(result, failure)
          if not current() then return end
          pending = false
          if failure then notifications.error(failure, "Harness checks")
          else
            state.check_editing = result.check_editing == true
            if require("forge.session").harness == state then
              require("forge.views.harness.controller").refresh_winbar()
            end
          end
          if picker.is_open("harness-config") then picker.update(build_spec()) end
        end)
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
        client.request({ method = "host.trace.configure", params = { enabled = not status.enabled } }, function(result, failure)
          pending = false
          if not current() then return end
          if failure then notifications.error(failure, "Harness logging") return end
          status = result
          M.adopt(status)
          if current() and picker.is_open("harness-config") then picker.update(build_spec()) end
        end)
      end
    end
    current = function()
      return M._revision == owner_revision and not state.switching_provider
        and state.session ~= nil and state.session.id == session_id
    end
    local function model_descriptor()
      return require("forge.views.harness.model_options").find(commit and commit.model_list, active.model)
    end
    local function change_main(id, direction)
      if id == "main-model" then
        local models = commit and commit.model_list or {}
        local values = vim.tbl_map(function(model) return model.id end, models)
        local selected = cycle(values, active.model, direction)
        for _, model in ipairs(models) do
          if model.id == selected then
            local speeds = require("forge.views.harness.model_options").speeds(model, state.capability or {})
            configure({ model = selected, effort = model.default_reasoning or (model.reasoning or {})[1] or "default",
              service_tier = vim.tbl_contains(speeds, active.service_tier or "default") and active.service_tier or "default" })
            return
          end
        end
      elseif id == "main-thinking" then
        local model = model_descriptor()
        configure({ effort = cycle(model and model.reasoning or {}, active.effort, direction) })
      elseif id == "main-speed" then
        local values = require("forge.views.harness.model_options").speeds(model_descriptor(), state.capability or {})
        configure({ service_tier = cycle(values, active.service_tier or "default", direction) })
      end
    end
    local function activate(id)
      return_state = picker.capture_state()
      if id == "compact-threshold" then
        local policy = active.auto_compaction or { enabled = false, threshold_tokens = 128000 }
        threshold.open({ host = host, value = policy.threshold_tokens, on_close = reopen,
          on_select = function(value)
            if not current() then return end
            local updated = vim.deepcopy(active.auto_compaction or policy)
            updated.threshold_tokens = value
            configure({ auto_compaction = updated }, reopen)
          end })
        return
      end
      if commit and id:match("^background%-") and commit:open(id) then return end
      if id == "ask-model" then
        local models = vim.deepcopy(commit and commit.model_list or {})
        for _, model in ipairs(models) do
          if model.id == ask.model then model.selected_reasoning = ask.effort end
        end
        picker.close(false)
        require("forge.views.harness.model_picker").open({ host = host, model_list = models,
          provider = active.provider, current_model = ask.model or active.model, is_current = current,
          on_close = reopen,
          on_confirm = function(value)
            picker.close(false)
            configure_ask({ model = value.model, effort = value.effort, service_tier = "default" },
              reopen)
          end })
        return
      end
      if id == "main-model" then
        local models = vim.deepcopy(commit and commit.model_list or {})
        for _, model in ipairs(models) do
          if model.id == active.model then model.selected_reasoning = active.effort end
        end
        picker.close(false)
        require("forge.views.harness.model_picker").open({ host = host, model_list = models,
          provider = active.provider, current_model = active.model, is_current = current,
          on_close = reopen,
          on_confirm = function(value)
            picker.close(false)
            local model = require("forge.views.harness.model_options").find(models, value.model)
            local speeds = require("forge.views.harness.model_options").speeds(model, state.capability or {})
            configure({ model = value.model, effort = value.effort, context_window = value.context_window,
              service_tier = vim.tbl_contains(speeds, active.service_tier or "default") and active.service_tier or "default" },
              reopen)
          end })
      elseif id == "provider-setup" then
        require("forge.views.harness.provider_setup").open({ host = host, session_id = session_id })
      elseif id == "provider" then
        require("forge.views.harness.controller").select_provider()
      elseif id:match("^main%-") then
        change_main(id, 1)
      else
        toggle({ option = { id = id } }, 1)
      end
    end
    build_spec = function()
      local policy = access.policy(active)
      local supported = access.supports_sandbox(active, state.capability)
      local background = commit and commit:fields() or {}
      local background_fields = { background.model, background.thinking }
      if background.speed then background_fields[#background_fields + 1] = background.speed end
      local capability = state.capability or {}
      ---@param id string
      ---@param kind 'choice'|'toggle'|'input'|'action'
      ---@param value string|boolean
      ---@param label string?
      ---@param disabled string?
      ---@param muted boolean?
      ---@return ForgePickerField
      local function field(id, kind, value, label, disabled, muted)
        return { id = id, kind = kind, value = value, label = label,
          disabled_reason = disabled, muted = muted,
          input = kind == "input" and { kind = "choice", values = duration.choices(), initial = duration.selection(active[id]) } or nil }
      end
      local catalog_disabled = not (commit and commit.model_list) and (commit and commit.failure or "Loading models") or nil
      local ask_disabled = not ask.model and "Enable Override to change Ask settings." or catalog_disabled
      local main = {
        field("main-model", "choice", active.model or "Default", nil,
          not capability.model_selection and "Model selection is unavailable for this provider" or catalog_disabled),
        field("main-thinking", "choice", active.effort or "Default", "Thinking",
          not capability.effort_selection and "Reasoning effort selection is unavailable for this provider" or catalog_disabled),
      }
      if #require("forge.views.harness.model_options").speeds(model_descriptor(), capability) > 1
        or active.service_tier and active.service_tier ~= "default" then
        main[#main + 1] = field("main-speed", "choice", active.service_tier or "default", "Speed")
      end
      local compaction = active.auto_compaction or { enabled = false, threshold_tokens = 128000 }
      main[#main + 1] = field("auto-compact", "toggle", compaction.enabled, "Compact",
        not capability.native_compact and "Compaction is unavailable for this provider" or nil)
      main[#main + 1] = field("compact-threshold", "choice", threshold.label(compaction.threshold_tokens), "Threshold", nil, not compaction.enabled)
      vim.list_extend(background_fields, { background.compact, background.threshold })
      local sandbox_description = access.sandbox_description(policy, supported)
      local sandbox = {
        field("sandbox", "toggle", policy.sandbox, "Enabled",
          nil, not supported),
      }
      sandbox[1].description = sandbox_description
      if vim.fn.has("win32") == 1 and #((capability.integration or {}).isolation_modes or {}) > 0 then
        local windows_description = access.isolation_mode_description(policy, supported, capability)
        sandbox[#sandbox + 1] = field("isolation-mode", "choice", policy.isolation_mode or "default", "Isolation mode",
          nil, not supported or not policy.sandbox)
        sandbox[#sandbox].description = windows_description
      end
      local provider_configuration = {}
      if capability.integration and (capability.integration.runtime_setup or capability.integration.authentication) then
        provider_configuration[#provider_configuration + 1] = field("provider-setup", "action", "Provider setup / sign in…", nil)
      end
      ---@param category string
      ---@param id string
      ---@param section string
      ---@param label string
      ---@param fields ForgePickerField[]
      ---@return table
      local function row(category, id, section, label, fields)
        return { category = category, id = id, section = section, label = label, fields = fields }
      end
      return {
        owner = "harness-config", title = "Configuration", host = host, size_ratio = 0.8, width_ratio = 1, placement = "bottom",
        page_navigation = "tabs", wrap_pages = true, hint_mode = "help", resume_state = resume_state,
        keymaps = {
          page_previous = { "<S-Tab>", "[" }, page_next = { "<Tab>", "]" },
          previous_field = "<C-Left>", next_field = "<C-Right>",
        },
        page_list = configuration_pages({
            row("models", "main-model", "Main model", "Model", main),
            row("models", "ask-override", "Ask model", "Override", {
              field("ask-override", "toggle", ask.model ~= nil, "Enabled",
                not ask.model and (not capability.model_selection and "Model selection is unavailable for this provider" or catalog_disabled) or nil),
            }),
            row("models", "ask-model", "Ask model", "Model", {
              field("ask-model", "choice", ask.model or active.resolved_model or active.model or "Default", nil,
                ask_disabled or not capability.model_selection and "Model selection is unavailable for this provider" or nil),
              field("ask-thinking", "choice", ask.model and (ask.effort or "Default") or (active.effort or "Default"), "Thinking",
                ask_disabled or not capability.effort_selection and "Reasoning effort selection is unavailable for this provider" or nil),
              field("ask-speed", "choice", ask.model and (ask.service_tier or "default") or (active.service_tier or "default"), "Speed",
                ask_disabled),
            }),
            row("models", "background-model", "Background model", "Model", background_fields),
            row("workflow", "automatic-checks", "Checks", "Automatic", {
              field("automatic-checks", "toggle", active.automatic_checks == true, "Enabled"),
              field("check-timing", "choice", active.check_timing == "between_work" and "Between queued work" or "After queue", "Timing"),
            }),
            row("workflow", "check-execution", "Checks", "Execution", {
              field("shell", "choice", active.shell == "nushell" and "Nushell" or "System", "Shell"),
            }),
            row("permissions", "read-access", "Access", "Read", {
              field("read-access", "choice", policy.read_access, nil),
              field("readable-directories", "action", tostring(#policy.readable_directory) .. "…", "Extra directories"),
            }),
            row("permissions", "write-access", "Access", "Write", {
              field("write-access", "choice", policy.write_access, nil),
              field("writable-directories", "action", tostring(#policy.writable_directory) .. "…", "Extra directories"),
            }),
            row("permissions", "tool-mode", "Tools", "Mode", {
              field("approval-mode", "choice", ({ ask = "Ask", deny = "Deny", allow = "Approve" })[active.approval_mode] or "Ask", nil),
            }),
            row("permissions", "approval-timeout", "Tools", "Approval timeout", {
              field("approval_timeout", "input", duration.render(active.approval_timeout), nil),
            }),
            row("permissions", "tool-timeout", "Tools", "Tool / shell timeout", {
              field("tool_timeout", "input", duration.render(active.tool_timeout), nil),
            }),
            row("permissions", "check-editing", "Checks", "Allow editing", {
              field("check-editing", "toggle", state.check_editing == true, nil),
            }),
            row("permissions", "sandbox", "Sandbox", "Sandbox", sandbox),
            row("tools", "subagent-tools", "Tools", "Subagents", {
              field("subagent-tools", "toggle", (active.subagent_tools == nil and state.subagent_tools or active.subagent_tools) == true, "Enabled"),
            }),
            row("tools", "lsp-tools", "Tools", "LSP", {
              field("lsp-tools", "toggle", (active.lsp_tools == nil and state.lsp_tools or active.lsp_tools) == true, "Enabled"),
            }),
            row("tools", "session-tools", "Tools", "Session history", {
              field("session-tools", "toggle", active.session_tools ~= false and (active.session_tools ~= nil or state.session_tools ~= false), "Enabled"),
            }),
            row("system", "status-thoughts", "Display", "Thoughts", {
              field("status-thoughts", "toggle", active.status_thoughts == true, "Enabled"),
            }),
            row("system", "provider", "Provider", "Connection", {
              field("provider", "action", cli or "Unknown", nil),
            }),
            row("system", "provider-configuration", "Provider", "Configuration", provider_configuration),
            row("system", "logging", "Debug", "Logging", {
              field("logging", "toggle", status.enabled == true, "Enabled"),
            }),
        }),
        on_field_submit = function(context, value, complete)
          if not current() then complete("This session is no longer active.") return end
          if pending then complete("Another setting is being saved. Try again.") return end
          local timeout = context.field.id
          if timeout ~= "approval_timeout" and timeout ~= "tool_timeout" then complete("This setting does not accept text input.") return end
          configure({ [timeout] = duration.parse(value) }, function(applied)
            complete(not applied and "Could not save the setting. Press Enter to retry." or nil)
          end)
        end,
        on_field_action = function(context, action, direction)
          if not current() or pending then return end
          local id = context.field.id
          if action == "activate" then activate(id)
          elseif id == "compact-threshold" then
            local policy = vim.deepcopy(active.auto_compaction or { enabled = false, threshold_tokens = 128000 })
            policy.threshold_tokens = threshold.change(policy.threshold_tokens, direction)
            configure({ auto_compaction = policy })
          elseif id:match("^main%-") then change_main(id, direction)
          else toggle({ option = { id = id } }, direction) end
        end,
      }
    end
    commit = require("forge.views.harness.background_configuration").new({
      provider = active.provider, session_id = session_id, host = host,
      capability = function() return state.capability or {} end,
      current = current,
      changed = function() if commit and picker.is_open("harness-config") then picker.update(build_spec()) end end,
      reopen = reopen,
    })
    picker.open(build_spec())
  end)
end

return M
