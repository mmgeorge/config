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
  local file_counts = {}
  for _, row in ipairs(source) do
    if type(row.test_summary) == "table" then
      if row.block == "plan:section:tests" then counts = vim.deepcopy(row.test_summary)
      elseif row.test_file then file_counts[row.test_file] = vim.deepcopy(row.test_summary) end
    end
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
          local file = file_counts[row.test.file]
          if file then file[row.test_change] = file[row.test_change] - 1 end
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
  local capture = { new = "ForgeAddRange", modified = "ForgeModifyRange", removed = "ForgeDeleteRange", reused = "Comment" }
  for _, change in ipairs({ "new", "modified", "removed", "reused" }) do
    if counts[change] > 0 then summary[#summary + 1] = { text = counts[change] .. " " .. change, capture = capture[change] } end
  end
  for _, row in ipairs(preview) do
    if type(row.test_file) == "string" and removed_file[row.test_file] then row.hidden = true end
    local file = file_counts[row.test_file]
    if file and not row.hidden then
      row.metadata = vim.deepcopy(row.metadata)
      local start
      for _, span in ipairs(row.metadata.decoration) do
        if span.capture == "ForgeAddRange" then start = span.range.start.column break end
      end
      if start then
        row.text = row.text:sub(1, start)
        row.metadata.decoration = vim.tbl_filter(function(span) return span.range["end"].column <= start end, row.metadata.decoration)
        for _, part in ipairs({ { "+" .. file.new, "ForgeAddRange" }, { " ", "ForgeStatusPath" }, { "-" .. file.removed, "ForgeDeleteRange" } }) do
          local column = #row.text
          row.text = row.text .. part[1]
          row.metadata.decoration[#row.metadata.decoration + 1] = {
            range = { start = { row = 0, column = column }, ["end"] = { row = 0, column = #row.text } },
            capture = part[2], priority = 100,
          }
        end
        row.test_summary = vim.deepcopy(file)
      end
    end
    if row.block == "plan:section:tests" then
      row.text = ""
      row.metadata = vim.deepcopy(row.metadata or {})
      row.metadata.decoration = {}
      local function append(text, group)
        local start = #row.text
        row.text = row.text .. text
        row.metadata.decoration[#row.metadata.decoration + 1] = {
          range = { start = { row = 0, column = start }, ["end"] = { row = 0, column = #row.text } },
          capture = group, priority = 100,
        }
      end
      append(" Tests ", "RenderMarkdownH1")
      append("· ", "Comment")
      if #summary == 0 then append("None planned", "Comment") end
      for index, part in ipairs(summary) do
        if index > 1 then append(" · ", "Comment") end
        append(part.text, part.capture)
      end
      append(" ", "Normal")
      row.test_summary = vim.deepcopy(counts)
    end
  end
  return preview
end

return M
