local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local picker = require("forge.views.picker")
local session = require("forge.session")

local M = {}

---@class ForgeProviderAuthentication
---@field id string
---@field label string
---@field description? string
---@field terminal boolean
---@field command? string[]

---@class ForgeProviderRuntime
---@field available boolean
---@field managed boolean
---@field version? string
---@field supported_version string
---@field provider_version? string
---@field installable boolean
---@field auth_methods ForgeProviderAuthentication[]

---@param command string[]
---@param callback fun(success: boolean)
local function terminal_login(command, callback)
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
  local timer = vim.uv.new_timer()
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
    return vim.fn.jobstart(command, { term = true,
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
  if session.harness.busy or session.harness.configuring then
    notifications.warn("Finish active work before provider setup", "Provider setup")
    return
  end
  local owner = "harness-provider-setup"
  local runtime
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
      if not runtime or (runtime.installable and not runtime.managed) then
        rows[#rows + 1] = { id = "install", label = "Install supported runtime",
          detail = runtime and ("Version " .. runtime.supported_version .. " · stored by Forge") or "Stored by Forge" }
      end
      if runtime and runtime.available then
        for _, method in ipairs(runtime.auth_methods or {}) do
          rows[#rows + 1] = { id = method.id, label = method.label, detail = method.description, value = method }
        end
      end
      rows[#rows + 1] = { id = "refresh", label = "Refresh setup", detail = "Check runtime and sign-in methods" }
    end
    local status = phase
    if not busy and runtime then
      status = runtime.available and ("Runtime ready · " .. (runtime.provider_version or runtime.version or "external installation"))
        or "ACP runtime is not installed"
    end
    return { owner = owner, host = options.host,
      page_list = { { id = "setup", title = "Provider Setup", subtitle = failure or status,
        column_headers = { "Action", "Details" }, option_list = rows,
        empty_text = phase, footer = "↑↓ select  Enter continue  q close" } },
      on_confirm = function(result)
        if busy or not current() then return false end
        local option = result.option
        if option.id == "refresh" then refresh(false) return false end
        if option.id == "install" then refresh(true) return false end
        local method = option.value
        if not method then return false end
        busy, failure, phase = true, nil, "Complete sign-in in the browser or terminal..."
        session.harness.configuring = true
        require("forge.views.harness.controller").render()
        local function finished(success)
          if not current() then return end
          session.harness.configuring = false
          require("forge.views.harness.controller").render()
          busy = false
          if success then
            notifications.info("Provider sign-in completed", "Provider setup")
            refresh(false)
          elseif picker.is_open(owner) then picker.update(spec()) end
        end
        if method.terminal then
          picker.close()
          terminal_login(method.command, finished)
        else
          picker.update(spec())
          client.request_for(options.session_id, "backend.authenticate", { method = method.id }, function(_, error)
            failure = error
            if error then notifications.error(error, "Provider sign-in") end
            finished(not error)
          end)
        end
        return false
      end,
    }
  end
  refresh = function(install)
    if not current() then return end
    generation = generation + 1
    local requested = generation
    busy, failure, phase = true, nil, install and "Installing supported runtime..." or "Loading provider setup..."
    session.harness.configuring = true
    require("forge.views.harness.controller").render()
    if picker.is_open(owner) then picker.update(spec()) else picker.open(spec()) end
    client.request_for(options.session_id, "backend.setup", { install = install }, function(result, error)
      if requested ~= generation or not current() then return end
      busy, failure = false, error
      session.harness.configuring = false
      require("forge.views.harness.controller").render()
      if error then notifications.error(error, "Provider setup") else runtime = result end
      if picker.is_open(owner) then picker.update(spec()) end
    end)
  end
  refresh(false)
end

return M
