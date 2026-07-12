local M = {}

---@param command string
---@param expects_command? boolean
---@return table[]
local function command_token_list(command, expects_command)
  local token_list = {}
  local next_is_command = expects_command ~= false
  local search_start = 1
  while true do
    local token_start, token_end, token = command:find("(%S+)", search_start)
    if not token_start then break end
    local group = "DiffReviewHarnessArgument"
    if next_is_command then
      group = "DiffReviewHarnessCommand"
      next_is_command = false
    elseif token:match("^%-%-?") then
      group = "DiffReviewHarnessOption"
    end
    token_list[#token_list + 1] = {
      first = token_start,
      last = token_end,
      text = token,
      group = group,
    }
    if token == "|" or token == ";" or token == "&&" then next_is_command = true end
    search_start = token_end + 1
  end
  return token_list
end

---@param tool table
---@param width? integer
---@param indent? string
---@return table[]
function M.heading_lines(tool, width, indent)
  local leading = indent or ""
  if tool.kind ~= "command" then return { { text = leading .. M.heading(tool) } } end

  local command = M.display_command(tool.title or "command")
  if not width or width < 20 then
    local text = leading .. "• Ran " .. command
    return { { text = text, command = command, command_offset = #text - #command } }
  end
  local current = leading .. "• Ran "
  local command_fragment = ""
  local line_list = {}
  for word in command:gmatch("%S+") do
    local separator = command_fragment == "" and "" or " "
    if command_fragment ~= "" and vim.fn.strdisplaywidth(current .. separator .. word) > width then
      line_list[#line_list + 1] = {
        text = current,
        command = command_fragment,
        command_offset = #current - #command_fragment,
      }
      current = leading .. word
      command_fragment = word
    else
      current = current .. separator .. word
      command_fragment = command_fragment .. separator .. word
    end
  end
  line_list[#line_list + 1] = {
    text = current,
    command = command_fragment,
    command_offset = #current - #command_fragment,
  }
  return line_list
end

---@param title string
---@return string
function M.display_command(title)
  local command = title:match("[Pp][Oo][Ww][Ee][Rr][Ss][Hh][Ee][Ll][^%s]*.-%s%-Command%s+(.+)$")
    or title:match("[Pp][Ww][Ss][Hh][^%s]*.-%s%-Command%s+(.+)$")
  if not command then return title end
  local quote = command:sub(1, 1)
  if (quote == '"' or quote == "'") and command:sub(-1) == quote then command = command:sub(2, -2) end
  return (command:gsub('\\"', '"'))
end

---@param tool table
---@return boolean
function M.failed(tool)
  if tool.failed ~= nil then return tool.failed == true end
  local status = tostring(tool.status or ""):lower()
  return status == "failed" or status == "error" or status == "denied" or status == "rejected"
    or status == "cancelled" or status == "canceled"
end

---@param tool table
---@return string
function M.heading(tool)
  local kind = tool.kind or "tool_call"
  local verb = kind == "command" and "Ran" or (kind == "file_change" and "Edited" or "Called")
  local title = kind == "command" and M.display_command(tool.title or "command") or (tool.title or "tool")
  return ("• %s %s"):format(verb, title)
end

---@param result table
---@param line integer
---@param command string
---@param offset integer
function M.highlight_command(result, line, command, offset)
  for _, token in ipairs(command_token_list(command)) do
    result.highlights[#result.highlights + 1] = {
      line = line,
      first = offset + token.first - 1,
      last = offset + token.last,
      group = token.group,
    }
  end
end

---@param result table
---@param heading_line_list table[]
function M.highlight_command_lines(result, heading_line_list)
  local expects_command = true
  for _, heading_line in ipairs(heading_line_list) do
    local token_list = command_token_list(heading_line.command or "", expects_command)
    for _, token in ipairs(token_list) do
      result.highlights[#result.highlights + 1] = {
        line = heading_line.line,
        first = heading_line.command_offset + token.first - 1,
        last = heading_line.command_offset + token.last,
        group = token.group,
      }
    end
    if #token_list > 0 then
      local final = token_list[#token_list].text
      expects_command = final == "|" or final == ";" or final == "&&"
    end
  end
end

---@param tool table
---@param indent? string
---@param visible_text? string
---@return table[]
function M.foldtext_chunks(tool, indent, visible_text)
  local prefix = (indent or "") .. "• "
  local bullet_group = M.failed(tool) and "DiffReviewHarnessToolFailure" or "DiffReviewHarnessToolSuccess"
  local kind = tool.kind or "tool_call"
  local verb = kind == "command" and "Ran" or (kind == "file_change" and "Edited" or "Called")
  if kind ~= "command" then
    return {
      { (indent or "") .. "•", bullet_group },
      { (" %s %s"):format(verb, tool.title or "tool"), "Normal" },
    }
  end

  if visible_text and not visible_text:find("• Ran ", 1, true) then
    local command = visible_text:match("^%s*(.*)$") or visible_text
    local leading = visible_text:sub(1, #visible_text - #command)
    local chunk_list = leading ~= "" and { { leading, "Normal" } } or {}
    local previous_end = 0
    for _, token in ipairs(command_token_list(command, false)) do
      if token.first > previous_end + 1 then
        chunk_list[#chunk_list + 1] = { command:sub(previous_end + 1, token.first - 1), "Normal" }
      end
      chunk_list[#chunk_list + 1] = { token.text, token.group }
      previous_end = token.last
    end
    return chunk_list
  end

  local command = M.display_command(tool.title or "command")
  local chunk_list = {
    { prefix:sub(1, -2), bullet_group },
    { " " .. verb .. " ", "Normal" },
  }
  local previous_end = 0
  for _, token in ipairs(command_token_list(command)) do
    if token.first > previous_end + 1 then
      chunk_list[#chunk_list + 1] = { command:sub(previous_end + 1, token.first - 1), "Normal" }
    end
    chunk_list[#chunk_list + 1] = { token.text, token.group }
    previous_end = token.last
  end
  if previous_end < #command then chunk_list[#chunk_list + 1] = { command:sub(previous_end + 1), "Normal" } end
  return chunk_list
end

---@param output string?
---@return string[]
function M.output_lines(output)
  local normalized = tostring(output or ""):gsub("\r\n", "\n"):gsub("\r", "")
  normalized = vim.fn.substitute(normalized, "\\%x1b\\[[0-?]*[ -/]*[@-~]", "", "g")
  local lines = vim.split(normalized, "\n", { plain = true })
  while #lines > 0 and lines[#lines] == "" do table.remove(lines) end
  return lines
end

return M
