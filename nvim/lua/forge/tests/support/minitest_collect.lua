local M = {}

---Collect the same top-level selection for discovery and shard execution.
---@param mini_test table
---@param path string Repository-relative test file.
---@param name? string Top-level entry, or every entry when absent.
---@return table[] cases
function M.collect(mini_test, path, name)
  return mini_test.collect({
    find_files = function() return { path } end,
    filter_cases = function(case) return name == nil or case.desc[2] == name end,
  })
end

---Describe collected entries without invoking their bodies or execution hooks.
---@param cases table[]
---@param path string Repository-relative test file.
---@return table[] items
function M.items(cases, path)
  local items, seen = {}, {}
  for _, case in ipairs(cases) do
    local name = assert(case.desc[2], "mini.test case has no top-level entry")
    if not seen[name] then
      seen[name] = true
      items[#items + 1] = { id = path .. "::" .. name, parent = path, label = name, case = true, name = name, path = path }
    end
  end
  return items
end

return M
