local M = {}
local prose = require("forge.text.prose")
local index_by_buffer = {}

---@param buffer integer
---@return table
function M.index(buffer)
  local retained = index_by_buffer[buffer]
  if retained and retained.active then retained.references=retained.references+1; return retained end
  assert(vim.api.nvim_buf_is_loaded(buffer), "prose buffer is not loaded")
  local index = { buffer = buffer, shadow = require("forge.editable_shadow").new(vim.api.nvim_buf_get_lines(buffer,0,-1,false)), state = {}, dirty = 0, active = true, references = 1, revision = 0 }
  index_by_buffer[buffer]=index
  local attached = vim.api.nvim_buf_attach(buffer,false,{
    on_lines = function(_,_,_,first,last,new_last)
      if not index.active then return true end
      -- Deleting every line reports new_last=0, but Neovim retains one empty line.
      local replacement_end = first==0 and last==index.shadow:rows() and new_last==0 and 1 or new_last
      local replacement = vim.api.nvim_buf_get_lines(buffer,first,replacement_end,false)
      index.shadow:splice(first,last-first,replacement)
      index.dirty = math.min(index.dirty,first)
      index.revision=index.revision+1
    end,
    on_reload = function()
      if not index.active then return true end
      index.shadow=require("forge.editable_shadow").new(vim.api.nvim_buf_get_lines(buffer,0,-1,false))
      index.state,index.dirty={},0
      index.revision=index.revision+1
    end,
    on_detach = function()
      index.active = false
      if index_by_buffer[buffer]==index then index_by_buffer[buffer]=nil end
    end,
  })
  assert(attached, "could not attach prose buffer")
  return index
end

---@param index table
function M.release(index)
  index.references=index.references-1
  if index.references>0 then return end
  index.active=false
  if index_by_buffer[index.buffer]==index then index_by_buffer[index.buffer]=nil end
end

local function advance_state(previous,text,row)
  local state = { fence = previous.fence, frontmatter = previous.frontmatter, scissors = previous.scissors }
  if row == 0 and text == "---" then state.frontmatter = true
  elseif state.frontmatter and (text == "---" or text == "...") then state.frontmatter = false end
  local fence = text:match("^%s*(```+)") or text:match("^%s*(~~~+)")
  if fence then
    if state.fence and state.fence:sub(1,1)==fence:sub(1,1) and #fence>=#state.fence then state.fence=nil
    elseif not state.fence then state.fence=fence end
  end
  if text:find(">8",1,true) and text:match("^#") then state.scissors=true end
  return state
end

---@param index table
---@param owner table
---@param row integer
---@param column? integer
---@param checkpoint? fun()
---@return string?, integer?
function M.line(index,owner,row,column,checkpoint)
  if not index.active or row<0 or row>=index.shadow:rows() then return nil end
  for position=index.dirty,row do
    index.state[position+1]=advance_state(index.state[position] or {},index.shadow:row(position),position)
    index.dirty=position+1
    if checkpoint then checkpoint() end
  end
  return prose.line(index.buffer,row,index.shadow:row(row),owner,index.state[row] or {},column)
end

---@param index table
---@param owner table
---@param first {row: integer, column: integer}
---@param last {row: integer, column: integer}
---@return boolean
function M.allowed(index,owner,first,last)
  if not index.active or first.row<0 or last.row<first.row or first.column<0 or last.column<0
    or first.row==last.row and last.column<first.column then return false end
  if owner.guard and not owner.guard(first,last) then return false end
  local ending=last.row>first.row and last.column==0 and last.row-1 or last.row
  if first.row>=index.shadow:rows() or ending>=index.shadow:rows() then return false end
  for row=first.row,ending do
    local source=index.shadow:row(row)
    if not source then return false end
    local from=row==first.row and first.column or 0
    local finish=row==last.row and last.column or #source
    local text,offset=M.line(index,owner,row,row==first.row and from or nil)
    if not text or from<offset or finish>offset+#text then return false end
    local start_byte,end_byte=from-offset,finish-offset
    for _,range in ipairs(prose.protected(text)) do
      if start_byte==end_byte and range.start<=start_byte and start_byte<range['end']
        or start_byte<range['end'] and range.start<end_byte then return false end
    end
    if owner.kind=='source' and finish>from and not prose.syntax(index.buffer,row,finish-1) then return false end
  end
  return true
end

---@param index table
---@param boundary table
---@param owner table
---@param current fun(): boolean
---@param callback fun(context?: table, failure?: string)
function M.capture(index,boundary,owner,current,callback)
  local buffer = index.buffer
  local revision = index.revision
  local thread = coroutine.create(function()
    local work = 0
    local function checkpoint()
      work = work + 1
      if work % 32 == 0 then coroutine.yield() end
    end
    local function line(row,column)
      local text,prefix = M.line(index,owner,row,column,checkpoint)
      if not text or not text:find("%S") then return nil end
      return text,prefix
    end
    local row,column = boundary.row,boundary.column
    if row<0 or row>=index.shadow:rows() then return nil end
    local original_line = index.shadow:row(row)
    if not original_line then return nil end
    local before = original_line:sub(1,column)
    local finish = #before
    local function previous_character(last)
      local first=last
      while first>1 and before:byte(first)>=128 and before:byte(first)<192 do first=first-1 end
      return first,before:sub(first,last)
    end
    while finish>0 do
      local first,character=previous_character(finish)
      if prose.word_character(character) then break end
      finish=first-1
      checkpoint()
    end
    local start=finish
    while start>0 do
      local first,character=previous_character(start)
      if not prose.word_character(character) and character~="'" and character~="’" then break end
      start=first-1
      checkpoint()
    end
    local word=before:sub(start+1,finish)
    while word:sub(1,1)=="'" or word:sub(1,3)=="’" do
      local width=word:sub(1,1)=="'" and 1 or 3
      word=word:sub(width+1);start=start+width
    end
    if word=="" or word:match("^%d") then return nil end
    local target_text,prefix = line(row,start)
    if not target_text or start<prefix then return nil end
    if owner.guard and not owner.guard({row=row,column=start},{row=row,column=finish}) then return nil end
    local first,last=row,row
    while first>0 do
      local text = line(first-1)
      if not text or owner.guard and not owner.guard({row=first-1,column=0},{row=row,column=finish}) then break end
      first=first-1; checkpoint()
    end
    while last+1<index.shadow:rows() do
      local text = line(last+1)
      if not text or owner.guard and not owner.guard({row=row,column=start},{row=last+1,column=#index.shadow:row(last+1)}) then break end
      last=last+1; checkpoint()
    end
    local parts,target_start,bytes={},nil,0
    for position=first,last do
      local text,offset=line(position,position==row and start or nil)
      if position==row then target_start=bytes+start-offset end
      parts[#parts+1]=text
      bytes=bytes+#text+1
      checkpoint()
    end
    local context=table.concat(parts,"\n")
    local protected=prose.protected(context)
    for _,range in ipairs(protected) do if target_start<range["end"] and range.start<target_start+#word then return nil end end
    local before_boundary=original_line:sub(finish+1,column)
    local punctuation_space=boundary.key and boundary.key:match("^[.,;:!?]$") and before_boundary:match("[ \t]+$") or nil
    return { text=context,target={start=target_start,["end"]=target_start+#word},protected=protected,
      original=word,start={row=row,column=start},finish={row=row,column=finish},delimiter_end=boundary.after,
      before_boundary=before_boundary,punctuation_space=punctuation_space }
  end)
  local function resume()
    if not current() or not index.active or index.revision~=revision then return callback(nil) end
    local success,value=coroutine.resume(thread)
    if not success then return callback(nil,"Could not capture prose context: "..tostring(value)) end
    if coroutine.status(thread)~="dead" then vim.schedule(resume) else callback(value) end
  end
  resume()
end

return M
