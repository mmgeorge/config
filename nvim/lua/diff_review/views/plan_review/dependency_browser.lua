local notifications = require("diff_review.infra.notifications")

local M = {}

---@type fun(url: string): boolean?
M.opener = function(url)
  if not (vim.ui and vim.ui.open) then return false end
  vim.ui.open(url)
  return true
end

---@param package_name string
---@return boolean
function M.open(package_name)
  if package_name == "" or package_name:find("[^%w_-]") then
    notifications.error("Invalid crates.io package name", "PlanReview")
    return false
  end
  local url = "https://crates.io/crates/" .. package_name
  local ok, opened = pcall(M.opener, url)
  if not ok or opened == false then
    notifications.error("Failed to open " .. url, "PlanReview")
    return false
  end
  return true
end

return M
