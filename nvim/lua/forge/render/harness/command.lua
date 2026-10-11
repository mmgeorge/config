local Command = {}

local function tokens(source, shell)
  local result, current, quote, start = {}, {}, nil, nil
  local index = 1
  while index <= #source do
    local character, following = source:sub(index, index), source:sub(index + 1, index + 1)
    if not start and not character:match('%s') then start = index end
    if quote then
      if character == quote then
        if shell == 'powershell' and quote == "'" and following == "'" then
          current[#current + 1] = character
          index = index + 1
        else quote = nil end
      elseif (shell == 'powershell' and quote == '"' and character == '`')
        or (shell ~= 'powershell' and quote == '"' and character == '\\' and following:match('["\\$`]')) then
        current[#current + 1] = following
        index = index + 1
      else current[#current + 1] = character end
    elseif character == "'" or character == '"' or (shell == 'nushell' and character == '`') then
      quote = character
    elseif (shell == 'powershell' and character == '`')
      or ((shell == 'bash' or shell == 'zsh') and character == '\\') then
      current[#current + 1] = following
      index = index + 1
    elseif character:match('%s') then
      if start then result[#result + 1] = { text = table.concat(current), start = start } end
      current, start = {}, nil
    else current[#current + 1] = character end
    index = index + 1
  end
  if quote then return nil end
  if start then result[#result + 1] = { text = table.concat(current), start = start } end
  return result
end

---Projects only the script payload for command headings and terminal labels.
---@param source string
---@param shell? string
---@return string
function Command.display(source, shell)
  shell = shell or (vim.fn.has('win32') == 1 and 'powershell' or 'zsh')
  for _ = 1, 16 do
    local argument_list = tokens(source, shell)
    if not argument_list or not argument_list[1] then break end
    local executable = (argument_list[1].text:match('[^/\\]+$') or ''):lower():gsub('%.exe$', '')
    local interpreter = ({ pwsh = 'powershell', powershell = 'powershell', bash = 'bash', sh = 'bash',
      zsh = 'zsh', fish = 'bash', nu = 'nushell', nushell = 'nushell', cmd = 'unknown' })[executable]
    if not interpreter then break end
    local marker
    for index = 2, #argument_list do
      local argument = argument_list[index].text:lower()
      if (interpreter == 'powershell' and (argument == '-command' or argument == '-c' or argument == '-file' or argument == '-f'))
        or (interpreter == 'nushell' and (argument == '-c' or argument == '--commands'))
        or (executable == 'cmd' and (argument == '/c' or argument == '/k'))
        or (interpreter == 'bash' or interpreter == 'zsh') and (argument == '-c' or argument == '-lc' or argument == '-ic' or argument == '-lic') then
        marker = index
        break
      end
    end
    if not marker or not argument_list[marker + 1] then break end
    source = marker + 1 == #argument_list and argument_list[marker + 1].text
      or source:sub(argument_list[marker + 1].start):gsub('%s+$', '')
    shell = interpreter
  end
  return source
end

return Command
