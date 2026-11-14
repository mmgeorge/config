local M = {}
local method = "experimental/serverStatus"
local states = setmetatable({}, { __mode = "k" })
local installed
local observed = setmetatable({}, { __mode = "k" })

---Record authoritative Rust workspace readiness without replacing user handlers.
local function record(failure, result, context)
  if failure or type(result) ~= "table" or not context then return end
  local client = vim.lsp.get_client_by_id(context.client_id)
  if client then states[client] = result end
end

---Observe editor-started clients from setup, before their first status notification.
function M.setup()
  if vim.lsp.handlers[method] == installed and installed then return end
  local previous = vim.lsp.handlers[method]
  installed = function(failure, result, context, config)
    record(failure, result, context)
    if previous then return previous(failure, result, context, config) end
  end
  vim.lsp.handlers[method] = installed
end

---Preserve configuration-specific handlers when starting or adopting a client.
function M.observe(owner)
  M.setup()
  owner.handlers = owner.handlers or {}
  local previous = owner.handlers[method]
  if not previous or previous == installed or previous == observed[owner] then return end
  local wrapped = function(failure, result, context, config)
    record(failure, result, context)
    return previous(failure, result, context, config)
  end
  observed[owner] = wrapped
  owner.handlers[method] = wrapped
end

---Distinguish unobserved/loading Rust workspaces from an explicitly ready workspace.
---@return boolean ready
---@return string? failure
---@return string? warning
function M.ready(client)
  if client.name ~= "rust_analyzer" and not (client.server_info and client.server_info.name == "rust-analyzer") then return true end
  local state = states[client]
  if not state or not state.quiescent then return false end
  if state.health == "warning" then
    return true, nil, "rust-analyzer workspace is partially functional: " .. tostring(state.message or "workspace warning")
  end
  if state.health ~= "ok" then
    return false, "unavailable: rust-analyzer workspace is not healthy: " .. tostring(state.message or state.health)
  end
  return true
end

return M
