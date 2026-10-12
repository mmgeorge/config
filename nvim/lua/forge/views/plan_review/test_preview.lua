local M = {}

---@class ForgePlanTestSelection
---@field file string
---@field name string

--- Copy the inventory display while retaining canonical row identities for rollback and annotation capture.
---@param source table[]
---@param selected ForgePlanTestSelection[]
---@return table[]
function M.project(source, selected)
  local removed, retained_file, seen = {}, {}, {}
  for _, test in ipairs(selected) do removed[vim.json.encode({ test.file, test.name })] = true end
  local counts = { new = 0, modified = 0, removed = 0, reused = 0 }
  local preview = {}
  for index, row in ipairs(source) do
    local projected = vim.tbl_extend("force", {}, row)
    preview[index] = projected
    if type(row.test) == "table" then
      local identity = vim.json.encode({ row.test.file, row.test.name })
      if removed[identity] then projected.hidden = true
      else
        retained_file[row.test.file] = true
        if not seen[identity] then
          local marker = vim.trim(row.text):sub(1, 3)
          local change = marker:sub(1, 1) == "+" and "new" or marker:sub(1, 1) == "~" and "modified"
            or marker == "−" and "removed" or "reused"
          counts[change] = counts[change] + 1
        end
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
    if row.block:match("^plan:tests:file:[^:]+$") and removed_file[vim.trim(row.text)] then row.hidden = true end
    if row.block == "plan:section:tests" then
      row.text = " Tests · " .. (#summary > 0 and table.concat(summary, " · ") or "None planned") .. " "
    end
  end
  return preview
end

return M
