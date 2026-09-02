local M = {}

--- Splits text into wrapped substrings based on display column width.
---@param text string Content text string to wrap.
---@param width integer Maximum column width.
---@return string[] lines Array of wrapped text line fragments.
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

--- Tokenizes a command string into commands, options, and arguments with highlight categories.
---@param command string Shell command string.
---@param expects_command? boolean True if first token is expected to be a command name.
---@return table[] tokens Array of `{ first: integer, last: integer, text: string, group: string }` token tables.
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

--- Returns the action verb string corresponding to tool kind and execution state.
---@param tool table Tool descriptor table.
---@return string verb Action verb string (`"Ran"`, `"Edited"`, `"Calling"`, or `"Called"`).
local function tool_verb(tool)
  local kind = tool.kind or "tool_call"
  if kind == "command" then return "Ran" end
  if kind == "file_change" then return "Edited" end
  local status = tostring(tool.status or ""):lower()
  return (status == "inprogress" or status == "in_progress") and "Calling" or "Called"
end

--- Extracts tool name and argument substrings from an MCP call signature.
---@param title string Full MCP invocation string.
---@return string name Tool function name.
---@return string? arguments Argument payload string, or nil.
local function mcp_title_parts(title)
  local name, arguments = title:match("^(.-)%((.*)%)$")
  return name or title, arguments
end

--- Generates formatted heading lines for a tool invocation with optional column wrapping.
---@param tool table Tool descriptor table.
---@param width? integer Maximum display column width.
---@param indent? string Leading indentation string.
---@return table[] lines Array of `{ text: string, command?: string, command_offset?: integer, title_fragment?: string }` line records.
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

--- Strips outer PowerShell command-line wrappers to display the underlying command.
---@param title string Raw command line string.
---@return string command Cleaned display command.
function M.display_command(title)
  local command = title:match("[Pp][Oo][Ww][Ee][Rr][Ss][Hh][Ee][Ll][^%s]*.-%s%-Command%s+(.+)$")
    or title:match("[Pp][Ww][Ss][Hh][^%s]*.-%s%-Command%s+(.+)$")
  if not command then return title end
  local quote = command:sub(1, 1)
  if (quote == '"' or quote == "'") and command:sub(-1) == quote then command = command:sub(2, -2) end
  return (command:gsub('\\"', '"'))
end

--- Checks if a tool descriptor represents a failed or cancelled execution state.
---@param tool table Tool descriptor table.
---@return boolean failed True if tool execution failed or was denied/cancelled.
function M.failed(tool)
  if tool.failed ~= nil then return tool.failed == true end
  local status = tostring(tool.status or ""):lower()
  return status == "failed" or status == "error" or status == "denied" or status == "rejected"
    or status == "cancelled" or status == "canceled"
end

--- Formats a single-line summary heading string for a tool call.
---@param tool table Tool descriptor table.
---@return string heading Formatted heading string.
function M.heading(tool)
  local kind = tool.kind or "tool_call"
  local verb = tool_verb(tool)
  local title = kind == "command" and M.display_command(tool.title or "command") or (tool.title or "tool")
  return ("• %s %s"):format(verb, title)
end

--- Applies command token highlight records for a single command line.
---@param result table Target render collection table.
---@param line integer One-based buffer line index.
---@param command string Command line string.
---@param offset integer Column offset of command text on the line.
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

--- Highlights multiple wrapped lines of a shell command invocation.
---@param result table Target render collection table.
---@param heading_line_list table[] Array of heading line records.
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

--- Highlights MCP tool name and arguments across wrapped tool call heading lines.
---@param result table Target render collection table.
---@param tool table Tool descriptor table.
---@param heading_line_list table[] Array of heading line records.
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

--- Builds syntax-highlighted chunk tuples for collapsed tool calls.
---@param tool table Tool descriptor table.
---@param indent? string Indentation prefix string.
---@param visible_text? string Visible fold text string.
---@return table[] chunks Array of `[text, hl_group]` chunk tuples.
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

--- Strips ANSI escape sequences and splits tool output text into clean lines.
---@param output string? Raw output text string.
---@return string[] lines Array of sanitized output line strings.
function M.output_lines(output)
  local normalized = tostring(output or ""):gsub("\r\n", "\n"):gsub("\r", "")
  normalized = vim.fn.substitute(normalized, "\\%x1b\\[[0-?]*[ -/]*[@-~]", "", "g")
  local lines = vim.split(normalized, "\n", { plain = true })
  while #lines > 0 and lines[#lines] == "" do table.remove(lines) end
  return lines
end

return M
