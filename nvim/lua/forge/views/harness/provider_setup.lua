local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local picker = require("forge.views.picker")
local session = require("forge.session")

local M = {}

---@class ForgeProviderAuthentication
---@field id string
---@field label string
---@field description? string
---@field interaction {kind: 'provider_managed'|'terminal', launch?: ForgeAuthenticationLaunch}

---@class ForgeAuthenticationLaunch
---@field program string
---@field arguments string[]
---@field environment table<string, string>
---@field working_directory? string

---@class ForgeProviderRuntime
---@field kind 'available'|'missing'|'incompatible'|'unavailable'

---@class ForgeVersionObservation
---@field kind 'known'|'not_reported'|'failed'
---@field version? string
---@field failure? string

---@class ForgeProviderSetup
---@field runtime ForgeProviderRuntime
---@field origin 'configured'|'managed'
---@field versions {provider: ForgeVersionObservation, adapter: ForgeVersionObservation}
---@field authentication ForgeProviderAuthentication[]

---@param method ForgeProviderAuthentication
---@param callback fun(success: boolean)
local function terminal_login(method, callback)
  local launch = method.interaction.launch
  if not launch or type(launch.program) ~= "string" then
    notifications.error("Sign-in command is unavailable", "Provider sign-in")
    callback(false)
    return
  end
  local command = { launch.program }
  vim.list_extend(command, launch.arguments or {})
  local buffer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_open_win(buffer, true, {
    relative = "editor", border = "rounded", title = " Provider sign-in ",
    width = math.max(40, math.floor(vim.o.columns * 0.8)),
    height = math.max(8, math.floor(vim.o.lines * 0.6)),
    row = math.floor(vim.o.lines * 0.15), col = math.floor(vim.o.columns * 0.1),
  })
  vim.bo[buffer].bufhidden = "wipe"
  local settled = false
  local job
  local timer = assert(vim.uv.new_timer())
  local function finish(success, failure)
    if settled then return end
    settled = true
    timer:stop()
    timer:close()
    if job and job > 0 then pcall(vim.fn.jobstop, job) end
    if failure then notifications.error(failure, "Provider sign-in") end
    if success and vim.api.nvim_win_is_valid(window) then vim.api.nvim_win_close(window, true) end
    callback(success)
  end
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = buffer, once = true,
    callback = function() finish(false) end })
  local launched, result = pcall(vim.api.nvim_buf_call, buffer, function()
    return vim.fn.jobstart(command, { term = true, env = launch.environment,
      cwd = launch.working_directory,
      on_exit = function(_, code)
        vim.schedule(function()
          finish(code == 0, code ~= 0 and ("Sign-in exited with status " .. code .. ". See the terminal output.") or nil)
        end)
      end,
    })
  end)
  if not launched or result <= 0 then
    finish(false, "Could not start sign-in: " .. tostring(result))
    return
  end
  job = result
  timer:start(120000, 0, vim.schedule_wrap(function() finish(false, "Sign-in exceeded 120 seconds. Retry when ready.") end))
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_win_is_valid(window) then vim.api.nvim_win_close(window, true) end
  end, { buffer = buffer, desc = "Cancel provider sign-in" })
  vim.cmd.startinsert()
end

---@param options {host: table, session_id: string}
function M.open(options)
  if require("forge.views.harness.activity").busy(session.harness) then
    notifications.warn("Finish active work before provider setup", "Provider setup")
    return
  end
  local owner = "harness-provider-setup"
  local setup
  local failure
  local busy = false
  local phase = "Loading provider setup..."
  local generation = 0
  local refresh
  local function current()
    return session.harness.session and session.harness.session.id == options.session_id
  end
  local function spec()
    local rows = {}
    if not busy then
      if setup then
        for _, method in ipairs(setup.authentication or {}) do
          rows[#rows + 1] = { id = method.id, label = method.label, detail = method.description, value = method }
        end
      end
      rows[#rows + 1] = { id = "refresh", label = "Refresh setup", detail = "Check runtime and sign-in methods" }
    end
    local status = phase
    if not busy and setup then
      local labels = {
        available = "Provider runtime ready",
        missing = "Provider runtime is not installed",
        incompatible = "Provider runtime requires an update",
        unavailable = "Provider runtime is unavailable",
      }
      local details = { labels[setup.runtime.kind] or labels.unavailable }
      local versions = setup.versions or {}
      for _, component in ipairs({ { key = "provider", label = "Provider version" }, { key = "adapter", label = "Adapter" } }) do
        local observation = versions[component.key]
        if observation and observation.kind == "known" then
          details[#details + 1] = component.label .. " " .. observation.version
        elseif observation and observation.kind == "failed" then
          details[#details + 1] = component.label .. " check failed (" .. observation.failure .. ")"
        end
      end
      status = table.concat(details, " · ")
    end
    return { owner = owner, host = options.host,
      page_list = { { id = "setup", title = "Provider Setup", subtitle = failure or status,
        column_headers = { "Action", "Details" }, option_list = rows,
        empty_text = phase, footer = "↑↓ select  Enter continue  q close" } },
      on_confirm = function(result)
        if busy or not current() then return false end
        local option = result.option
        if option.id == "refresh" then refresh() return false end
        local method = option.value
        if not method then return false end
        busy, failure, phase = true, nil, "Complete sign-in in the browser or terminal..."
        require("forge.views.harness.controller").render()
        local function finished(success)
          if not current() then return end
          require("forge.views.harness.controller").render()
          busy = false
          if success then
            notifications.info("Provider sign-in completed", "Provider setup")
            refresh()
          elseif picker.is_open(owner) then picker.update(spec()) end
        end
        if method.interaction.kind == "terminal" then
          picker.close()
          terminal_login(method, finished)
        else
          picker.update(spec())
          client.request({ session_id = options.session_id, method = "session.provider.authenticate", params = { method = method.id } }, function(_, error)
            failure = error
            if error then notifications.error(error, "Provider sign-in") end
            finished(not error)
          end)
        end
        return false
      end,
    }
  end
  refresh = function()
    if not current() then return end
    generation = generation + 1
    local requested = generation
    busy, failure, phase = true, nil, "Loading provider setup..."
    require("forge.views.harness.controller").render()
    if picker.is_open(owner) then picker.update(spec()) else picker.open(spec()) end
    client.request({ session_id = options.session_id, method = "session.provider.setup", params = {} }, function(result, error)
      if requested ~= generation or not current() then return end
      busy, failure = false, error
      require("forge.views.harness.controller").render()
      if error then notifications.error(error, "Provider setup") else setup = result end
      if picker.is_open(owner) then picker.update(spec()) end
    end)
  end
  refresh()
end

return M
