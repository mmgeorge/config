local M = {}

---@param character string
---@return boolean
function M.word_character(character)
  if #character==1 then return character:match("[%w]")~=nil end
  local class=vim.fn.charclass(character)
  return class==2 or class>=256
end

---@param buffer integer
---@param row integer
---@param column integer
---@return boolean?
function M.syntax(buffer, row, column)
  local ok, captures = pcall(vim.treesitter.get_captures_at_pos, buffer, row, math.max(0, column))
  if not ok then return nil end
  local prose = false
  for _, capture in ipairs(captures) do
    if capture.capture == "nospell" or capture.capture:match("^string") then return false end
    if capture.capture == "spell" or capture.capture:match("^comment") then prose = true end
  end
  return prose
end

---@param buffer integer
---@param row integer
---@param text string
---@param owner table
---@param state table
---@param column? integer
---@return string?, integer?
function M.line(buffer, row, text, owner, state, column)
  if owner.kind == "source" then
    local offset = column or math.max(0, (text:find("%S") or 1) - 1)
    if not M.syntax(buffer, row, offset) then return nil end
    local comment_start,comment_end=0,#text
    local found,node=pcall(vim.treesitter.get_node,{bufnr=buffer,pos={row,offset}})
    if found then
      while node do
        if node:type():find("comment",1,true) then
          local first_row,first_column,last_row,last_column=node:range()
          comment_start=first_row==row and first_column or 0
          comment_end=last_row==row and last_column or #text
        end
        node=node:parent()
      end
    end
    local comment=text:sub(comment_start+1,comment_end)
    local marker = comment:match("^%s*//[/!]?%s?") or comment:match("^%s*%-%-%-?%s?")
      or comment:match("^%s*#+%s?") or comment:match("^%s*/%*+%s?") or comment:match("^%s*%*%s?") or ""
    return comment:sub(#marker + 1):gsub("%s*%*/%s*$", ""), comment_start+#marker
  end
  if owner.kind == "commit" and (state.scissors or text:match("^#")) then return nil end
  if state.fence or state.frontmatter or text:match("^%s*[`~][`~][`~]") or text:match("^    ") or text:match("^\t") then return nil end
  if owner.kind == "prompt" and text:match("^%s*/") then return nil end
  local syntax = M.syntax(buffer,row,column or 0)
  if syntax == false then
    local ok, captures = pcall(vim.treesitter.get_captures_at_pos,buffer,row,column or 0)
    if ok then for _, capture in ipairs(captures) do if capture.capture=="nospell" then return nil end end end
  end
  local marker = text:match("^%s*#+%s+") or text:match("^%s*>%s?") or text:match("^%s*[%-%*+]%s+") or text:match("^%s*%d+[%.%)]%s+") or ""
  return text:sub(#marker + 1), #marker
end

---@param context string
---@return table[]
function M.protected(context)
  local ranges = {}
  local function add(first, last) ranges[#ranges + 1] = { start = first - 1, ["end"] = last } end
  local position=1
  while position<=#context do
    local first,last=context:find("`+",position)
    if not first then break end
    local width=last-first+1
    local search=last+1
    local closing
    while search<=#context do
      local next_first,next_last=context:find("`+",search)
      if not next_first then break end
      if next_last-next_first+1==width then closing=next_last;break end
      search=next_last+1
    end
    add(first,closing or #context)
    position=(closing or #context)+1
  end
  for first, content, last in context:gmatch("()%]%((.-)%)()") do add(first,last - 1) end
  for first, word, last in context:gmatch("()(%S+)()") do
    local inner = word:gsub("[.,;:!?%)%]%}]+$", "")
    if inner:find("[/\\_@`]") or inner:find("::",1,true) or inner:match("%w%.%w") then add(first,last - 1) end
  end
  return ranges
end

return M
