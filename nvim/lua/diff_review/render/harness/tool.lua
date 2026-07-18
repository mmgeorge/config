local M = {}

---@param text string
---@param width integer
---@return string[]
local function split_display_width(text, width)
  local line_list = {}
  local fragment = ""
  local char_count = vim.fn.strchars(text)
  for char_index = 0, char_count - 1 do
    local character = vim.fn.strcharpart(text, char_index, 1)
    if fragment ~= "" and vim.fn.strdisplaywidth(fragment .. character) > width then
      line_list[#line_list + 1] = fragment
      fragment = character
    else
      fragment = fragment .. character
    end
  end
  if fragment ~= "" then line_list[#line_list + 1] = fragment end
  return line_list
end

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
---@return string
local function tool_verb(tool)
  local kind = tool.kind or "tool_call"
  if kind == "command" then return "Ran" end
  if kind == "file_change" then return "Edited" end
  local status = tostring(tool.status or ""):lower()
  return (status == "inprogress" or status == "in_progress") and "Calling" or "Called"
end

---@param title string
---@return string, string?
local function mcp_title_parts(title)
  local name, arguments = title:match("^(.-)%((.*)%)$")
  return name or title, arguments
end

---@param tool table
---@param width? integer
---@param indent? string
---@return table[]
function M.heading_lines(tool, width, indent)
  local leading = indent or ""
  if tool.kind ~= "command" then
    local heading = M.heading(tool)
    if tool.kind ~= "tool_call" or not width or vim.fn.strdisplaywidth(leading .. heading) <= width then
      return { {
        text = leading .. heading,
        title_fragment = tool.kind == "tool_call" and (tool.title or "tool") or nil,
      } }
    end
    local verb = tool_verb(tool)
    local title = tool.title or "tool"
    local title_prefix = leading .. "  └ "
    local continuation_prefix = leading .. "    "
    local title_width = math.max(1, width - vim.fn.strdisplaywidth(title_prefix))
    local title_line_list = split_display_width(title, title_width)
    local line_list = { { text = leading .. "• " .. verb } }
    for line_index, line in ipairs(title_line_list) do
      local prefix = line_index == 1 and title_prefix or continuation_prefix
      line_list[#line_list + 1] = { text = prefix .. line, title_fragment = line }
    end
    return line_list
  end

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
  local verb = tool_verb(tool)
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

---@param result table
---@param tool table
---@param heading_line_list table[]
function M.highlight_tool_call_lines(result, tool, heading_line_list)
  local name, arguments = mcp_title_parts(tool.title or "tool")
  local title_offset = 0
  for _, heading_line in ipairs(heading_line_list) do
    local text = heading_line.text or ""
    local fragment = heading_line.title_fragment or ""
    local fragment_length = #fragment
    if fragment_length > 0 then
      local fragment_start = #text - fragment_length + 1
      local fragment_end = title_offset + fragment_length
      local name_end = #name
      if title_offset < name_end then
        local highlighted_end = math.min(fragment_end, name_end)
        result.highlights[#result.highlights + 1] = {
          line = heading_line.line,
          first = fragment_start - 1,
          last = fragment_start + highlighted_end - title_offset - 1,
          group = "DiffReviewHarnessMcpName",
        }
      end
      local argument_end = name_end + #(arguments or "") + 1
      if arguments and fragment_end > name_end + 1 and title_offset < argument_end then
        local argument_start = math.max(title_offset + 1, name_end + 2)
        local highlighted_end = math.min(fragment_end, argument_end)
        result.highlights[#result.highlights + 1] = {
          line = heading_line.line,
          first = fragment_start + argument_start - title_offset - 2,
          last = fragment_start + highlighted_end - title_offset - 1,
          group = "DiffReviewHarnessMcpArguments",
        }
      end
      title_offset = fragment_end
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
  local verb = tool_verb(tool)
  if kind ~= "command" then
    if kind ~= "tool_call" then
      return {
        { (indent or "") .. "•", bullet_group },
        { (" %s %s"):format(verb, tool.title or "tool"), "Normal" },
      }
    end
    local name, arguments = mcp_title_parts(tool.title or "tool")
    local chunk_list = {
      { (indent or "") .. "•", bullet_group },
      { " " .. verb .. " ", "Normal" },
      { name, "DiffReviewHarnessMcpName" },
    }
    if arguments ~= nil then
      chunk_list[#chunk_list + 1] = { "(", "Normal" }
      chunk_list[#chunk_list + 1] = { arguments, "DiffReviewHarnessMcpArguments" }
      chunk_list[#chunk_list + 1] = { ")", "Normal" }
    end
    return chunk_list
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
