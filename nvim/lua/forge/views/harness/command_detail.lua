local CommandDetail = {}

---@param command ForgeApprovalCommand
---@return ForgePickerContent
function CommandDetail.format(command)
  local source = command.source
  local powershell, nushell = command.shell == "powershell", command.shell == "nushell"
  local output, current, byte_map = {}, {}, {}
  local output_size, indent, parentheses = 0, 0, 0
  local quote, escaped, comment = nil, false, false
  local block_stack = {}
  local closure_header = 0
  local function emit(text)
    output[#output + 1] = text
    output_size = output_size + #text
  end
  local function flush()
    local first, last = 1, #current
    while first <= last and current[first].text:match("%s") do first = first + 1 end
    while last >= first and current[last].text:match("%s") do last = last - 1 end
    if first <= last then
      if #output > 0 then emit("\n") end
      emit(string.rep("  ", indent))
      for index = first, last do
        local entry = current[index]
        byte_map[entry.source] = output_size
        emit(entry.text)
      end
    end
    current = {}
  end
  local function append(character, index) current[#current + 1] = { text = character, source = index } end
  for index = 1, #source do
    local character = source:sub(index, index)
    local following = source:sub(index + 1, index + 1)
    if comment then
      append(character, index)
      if character == "\n" then comment = false flush() end
    elseif escaped then
      append(character, index)
      escaped = false
    elseif quote then
      append(character, index)
      if character == quote then quote = nil
      elseif (powershell and quote == '"' and character == "`")
        or (not powershell and quote == '"' and character == "\\" and (following == quote or following == "\\")) then escaped = true end
    elseif closure_header > 0 then
      append(character, index)
      if character == "|" then
        closure_header = closure_header - 1
        if closure_header == 0 then flush() indent = indent + 1 end
      end
    elseif character == "'" or character == '"' or (not powershell and character == "`") then
      append(character, index)
      quote = character
    elseif (powershell and character == "`") or (not powershell and not nushell and character == "\\") then
      append(character, index)
      escaped = true
    elseif character == "#" and (#current == 0 or current[#current].text:match("%s")) then
      append(character, index)
      comment = true
    elseif character == "(" or character == "[" then
      parentheses = parentheses + 1
      append(character, index)
    elseif character == ")" or character == "]" then
      parentheses = math.max(0, parentheses - 1)
      append(character, index)
    elseif character == "{" then
      local block = #current == 0 or current[#current].text:match("[%s%)%]]") ~= nil
      block_stack[#block_stack + 1] = block
      append(character, index)
      if block and nushell and following == "|" then
        closure_header = 2
      elseif block then
        flush() indent = indent + 1
      end
    elseif character == "}" then
      local block = table.remove(block_stack)
      if block then flush() indent = math.max(0, indent - 1) end
      append(character, index)
    elseif character == "\n" or ((character == ";" or character == "|") and parentheses == 0) then
      if character ~= "\n" then append(character, index) end
      if character ~= "|" or following ~= "|" then flush() end
    else
      append(character, index)
    end
  end
  flush()
  local spans, focus_offset = {}, nil
  local function project(range, group, priority, focus)
    local first, last = range.start + 1, range["end"]
    while first <= last and byte_map[first] == nil do first = first + 1 end
    while last >= first and byte_map[last] == nil do last = last - 1 end
    if first <= last then
      spans[#spans + 1] = { first = byte_map[first], last = byte_map[last] + 1, group = group, priority = priority }
      if focus and not focus_offset then focus_offset = byte_map[first] end
    end
  end
  for _, capture in ipairs(command.highlight_list) do project(capture.range, capture.group, 200 + capture.priority, false) end
  for _, range in ipairs(command.focus_range_list) do project(range, "ForgePermissionTarget", 1000, true) end
  return { text = table.concat(output), group = "ForgePickerText", preformatted = true, spans = spans, focus_offset = focus_offset }
end

return CommandDetail
