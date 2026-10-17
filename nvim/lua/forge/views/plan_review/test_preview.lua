local M = {}

---@class ForgePlanTestSelection
---@field file string
---@field name string

---@class ForgePlanTestRow: ForgeDraftSourceIdentity
---@field block string
---@field text string
---@field test? ForgePlanTestSelection
---@field test_change? string
---@field test_file? string
---@field test_summary? table<string, integer>
---@field hidden? boolean

--- Copy the inventory display while retaining canonical row identities for rollback and annotation capture.
---@param source ForgePlanTestRow[]
---@param selected ForgePlanTestSelection[]
---@return ForgePlanTestRow[]
function M.project(source, selected)
  local removed, retained_file, seen = {}, {}, {}
  for _, test in ipairs(selected) do removed[vim.json.encode({ test.file, test.name })] = true end
  local counts = { new = 0, modified = 0, removed = 0, reused = 0 }
  for _, row in ipairs(source) do
    if type(row.test_summary) == "table" then counts = vim.deepcopy(row.test_summary) break end
  end
  local preview = {}
  for index, row in ipairs(source) do
    local projected = vim.tbl_extend("force", {}, row)
    preview[index] = projected
    if type(row.test) == "table" then
      local identity = vim.json.encode({ row.test.file, row.test.name })
      if removed[identity] then
        projected.hidden = true
        if not seen[identity] and type(row.test_change) == "string" then
          counts[row.test_change] = counts[row.test_change] - 1
        end
      else
        retained_file[row.test.file] = true
      end
      seen[identity] = true
    end
  end
  local removed_file = {}
  for _, test in ipairs(selected) do
    if not retained_file[test.file] then removed_file[test.file] = true end
  end
  local summary = {}
  for _, change in ipairs({ "new", "modified", "removed", "reused" }) do
    if counts[change] > 0 then summary[#summary + 1] = counts[change] .. " " .. change end
  end
  for _, row in ipairs(preview) do
    if type(row.test_file) == "string" and removed_file[row.test_file] then row.hidden = true end
    if row.block == "plan:section:tests" then
      row.text = " Tests · " .. (#summary > 0 and table.concat(summary, " · ") or "None planned") .. " "
    end
  end
  return preview
end

return M
