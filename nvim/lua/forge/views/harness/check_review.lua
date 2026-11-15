--- Adapt host-authored check review sections to the shared picker and diff presentation.
local CheckReview = {}

local diff_component = require("forge.render.diff_component")

---@class ForgeCheckReviewSection
---@field id string Stable section identity within the reviewed proposal.
---@field title string Host-authored section heading.
---@field source string Exact diff text retained for rendering.
---@field expanded boolean Whether the section body is visible.
---@field content ForgePickerContent[] Rendered diff rows.

---@class ForgeCheckReview
---@field introduction string Text preceding the first section.
---@field sections ForgeCheckReviewSection[] Ordered checks, raw configuration, and helpers.

--- Decode fenced sections without interpreting content inside a fence as a heading.
---@param detail string Host-authored review text.
---@return ForgeCheckReview? review Nil for the previous plain-text review format.
function CheckReview.parse(detail)
  local introduction, sections = {}, {}
  local title, fence, source = nil, nil, {}
  for _, line in ipairs(vim.split(detail, "\n", { plain = true })) do
    if fence then
      if line == fence then
        local text = table.concat(source, "\n") .. "\n"
        sections[#sections + 1] = {
          id = "review-section:" .. (#sections + 1), title = title, source = text,
          expanded = title:match("^Check:") ~= nil, content = CheckReview.diff_content(text),
        }
        fence, source, title = nil, {}, nil
      else
        source[#source + 1] = line
      end
    elseif line:match("^### ") then
      title = line:sub(5)
    elseif title and line:match("^```+[%w%-]+$") then
      fence = line:match("^(`+)")
    elseif not title then
      introduction[#introduction + 1] = line
    end
  end
  if fence or #sections == 0 then return nil end
  return { introduction = vim.trim(table.concat(introduction, "\n")), sections = sections }
end

--- Project shared diff rows into the picker's existing text/span contract.
---@param source string Unified diff text.
---@return ForgePickerContent[] content
function CheckReview.diff_content(source)
  local result = {}
  local rows = diff_component.build_rows(source, {}, nil, nil, nil, { compact_replacements = false })
  for _, row in ipairs(rows) do
    if not row.forge_hunk_header then
      local parts, spans, offset = {}, {}, 0
      local function append(text, group)
        parts[#parts + 1] = text
        if group then spans[#spans + 1] = { first = offset, last = offset + #text, group = group } end
        offset = offset + #text
      end
      for _, chunk in ipairs(row) do
        if type(chunk[1]) == "string" then
          append(chunk[1], chunk[2])
        elseif chunk.virt_text and chunk.virt_text_pos == "inline" then
          for _, segment in ipairs(chunk.virt_text) do append(segment[1], segment[2]) end
        end
      end
      result[#result + 1] = {
        text = table.concat(parts), spans = spans, preformatted = true,
        group = row.forge_bg_hl or "ForgePickerText",
      }
    end
  end
  if #result == 0 then
    result[1] = { text = source == "\n" and "No changes." or source, preformatted = true }
  end
  return result
end

return CheckReview
