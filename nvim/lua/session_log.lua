local M = {}

local path
local timer
local last_input
local last_error = ""
local reporting_failure = false
local notification_wrapper

---@return table<string, any>
local function context()
  local session = package.loaded["forge.session"]
  local harness = session and session.harness
  return {
    mode = vim.api.nvim_get_mode().mode,
    buffer = vim.api.nvim_get_current_buf(),
    filetype = vim.bo.filetype,
    session_id = harness and harness.session and harness.session.id,
    busy = harness and harness.busy,
    input = last_input,
  }
end

---@param event string
---@param fields? table<string, any>
local function write(event, fields)
  if not path then return end
  local success, failure = pcall(function()
    vim.fn.writefile({ vim.json.encode({
      time = os.date("!%Y-%m-%dT%H:%M:%SZ"),
      monotonic_ns = tostring(vim.uv.hrtime()),
      pid = vim.fn.getpid(), event = event, fields = fields or context(),
    }) }, path, "a")
  end)
  if not success and not reporting_failure then
    reporting_failure = true
    vim.schedule(function()
      vim.api.nvim_echo({ { "Session logging failed: " .. tostring(failure), "ErrorMsg" } }, true, {})
    end)
  end
end

local function observe_notifications()
  if vim.notify == notification_wrapper then return end
  local previous = vim.notify
  notification_wrapper = function(message, level, options)
    if level == vim.log.levels.ERROR or level == "error" then
      local fields = { message = tostring(message), title = options and options.title }
      if vim.in_fast_event() then
        vim.schedule(function() write("notification.error", fields) end)
      else
        write("notification.error", fields)
      end
    end
    return previous(message, level, options)
  end
  vim.notify = notification_wrapper
end

--- Installs diagnostic observers once without changing command or exit behavior.
--- Writes one JSONL file per process under stdpath("log")/sessions.
---@return string? path Diagnostic file, or nil when directory creation fails.
function M.setup()
  if path then return path end
  local directory = vim.fs.joinpath(vim.fn.stdpath("log"), "sessions")
  local success, failure = pcall(vim.fn.mkdir, directory, "p")
  if not success then
    vim.api.nvim_echo({ { "Session logging failed: " .. tostring(failure), "ErrorMsg" } }, true, {})
    return nil
  end
  path = vim.fs.joinpath(directory, os.date("!%Y%m%dT%H%M%SZ") .. "-" .. vim.fn.getpid() .. ".jsonl")
  write("start", { cwd = vim.fn.getcwd(), version = vim.version(), log_path = path })
  local group = vim.api.nvim_create_augroup("SessionDiagnostics", { clear = true })
  local namespace = vim.api.nvim_create_namespace("SessionDiagnostics")
  vim.on_key(function(_, typed)
    if typed == "" or vim.api.nvim_get_mode().mode ~= "n" then return end
    local mapping = vim.fn.maparg(typed, "n", false, true)
    last_input = { key = vim.fn.keytrans(typed), rhs = mapping.rhs, description = mapping.desc,
      callback = mapping.callback ~= nil, time = os.date("!%Y-%m-%dT%H:%M:%SZ") }
    write("input.normal")
  end, namespace)
  observe_notifications()
  vim.api.nvim_create_autocmd("User", { group = group, pattern = "VeryLazy", callback = observe_notifications })
  vim.api.nvim_create_autocmd("CmdlineLeave", { group = group, callback = function()
    if vim.fn.getcmdtype() ~= ":" then return end
    local parsed, command = pcall(vim.api.nvim_parse_cmd, vim.fn.getcmdline(), {})
    write("command", { command = parsed and command.cmd or "unparsed", bang = parsed and command.bang,
      aborted = vim.v.event.abort, context = context() })
  end })
  for _, event in ipairs({ "QuitPre", "ExitPre", "VimLeavePre", "VimLeave", "UIEnter", "UILeave" }) do
    vim.api.nvim_create_autocmd(event, { group = group, callback = function()
      local fields = context()
      fields.dying, fields.exiting = vim.v.dying, vim.v.exiting
      fields.error = vim.v.errmsg
      write(event, fields)
      if event == "VimLeavePre" and timer then
        timer:stop()
        timer:close()
        timer = nil
      end
    end })
  end
  timer = assert(vim.uv.new_timer())
  timer:start(1000, 1000, vim.schedule_wrap(function()
    if vim.v.errmsg ~= "" and vim.v.errmsg ~= last_error then
      last_error = vim.v.errmsg
      write("editor.error", { message = last_error, context = context() })
    end
    observe_notifications()
    write("heartbeat")
  end))
  return path
end

--- Returns the current process log after setup.
---@return string?
function M.path()
  return path
end

return M
