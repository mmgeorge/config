local M = {}
local client = require("forge.client")
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local log_buffer_by_path = {}

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
      vim.cmd.checktime(existing)
      return
    end
  end
  vim.cmd.tabedit(vim.fn.fnameescape(status.path))
  local buffer = vim.api.nvim_get_current_buf()
  log_buffer_by_path[status.path] = buffer
  vim.bo[buffer].readonly = true
  vim.bo[buffer].modifiable = false
  vim.bo[buffer].swapfile = false
  vim.bo[buffer].autoread = true
  vim.bo[buffer].filetype = "jsonl"
  vim.keymap.set("n", "q", "<Cmd>tabclose<CR>", { buffer = buffer, silent = true })
  vim.wo.winbar = title
  vim.keymap.set("n", "R", function() vim.cmd.checktime(buffer) end, { buffer = buffer })
  vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold" }, {
    buffer = buffer,
    callback = function() vim.cmd.checktime(buffer) end,
  })
end

---@param session_id string
---@param action? string
function M.log(session_id, action)
  action = action or "open"
  if action == "open" or action == "" then
    request(session_id, "trace.status", {}, open_log)
  elseif action == "on" or action == "off" then
    request(session_id, "trace.configure", { enabled = action == "on" })
  else
    notifications.error("Use /log [on|off|open]", "Harness logging")
  end
end

---@param state ForgeHarnessPresentationState
---@param host table
function M.open(state, host)
  local session_id = state.session.id
  request(session_id, "trace.status", {}, function(status)
    local pending = false
    local active = state.session
    local cli = active.provider_label or ({ codex = "Codex CLI", copilot = "Copilot CLI" })[active.backend] or active.backend
    local build_spec
    local function toggle()
      if pending then return end
      pending = true
      client.request_for(session_id, "trace.configure", { enabled = not status.enabled }, function(result, failure)
        pending = false
        if failure then notifications.error(failure, "Harness logging") return end
        status = result
        M.adopt(status)
        if picker.is_open("harness-config") then picker.update(build_spec()) end
      end)
    end
    build_spec = function()
      return {
        owner = "harness-config", host = host,
        page_list = { {
          id = "config", title = "Configuration", subtitle = "CLI: " .. (cli or "Unknown"),
          column_headers = { "Setting", "Value" },
          option_list = { { id = "logging", label = "Logging", columns = { "Logging", "← " .. (status.enabled and "On" or "Off") .. " →" } } },
          footer = "←→ toggle  q close",
        } },
        action_list = {
          { id = "previous-value", key = "<Left>", callback = toggle },
          { id = "next-value", key = "<Right>", callback = toggle },
        },
      }
    end
    picker.open(build_spec())
  end)
end

return M
