local M = {}

---@class GithubNotificationsOpenOptions
---@field hostname? string
---@field workspace? string
---@field window? integer
---@field request? fun(params: table, callback: fun(result: table|nil, failure: string|nil))
---@field open_effect? fun(effect: table, window: integer, is_current: fun(): boolean)
---@field on_error? fun(message: string)

---@param options? GithubNotificationsOpenOptions
---@return table
function M.open(options)
  return require("forge.notifications").open(options)
end

return M
