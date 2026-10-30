local M = {}
local namespace = vim.api.nvim_create_namespace("ForgeTextCorrection")

local function position(buffer,mark)
  local value=vim.api.nvim_buf_get_extmark_by_id(buffer,namespace,mark,{})
  if #value==2 then return {row=value[1],column=value[2]} end
end

local function content(buffer,start,finish)
  return table.concat(vim.api.nvim_buf_get_text(buffer,start.row,start.column,finish.row,finish.column,{}),"\n")
end

---@param state table
---@param occurrence table
function M.anchor(state,occurrence)
  local buffer=state.buffer
  occurrence.start_mark=vim.api.nvim_buf_set_extmark(buffer,namespace,occurrence.context.start.row,occurrence.context.start.column,{right_gravity=false})
  occurrence.end_mark=vim.api.nvim_buf_set_extmark(buffer,namespace,occurrence.context.finish.row,occurrence.context.finish.column,{right_gravity=true})
  occurrence.delimiter_mark=vim.api.nvim_buf_set_extmark(buffer,namespace,occurrence.context.delimiter_end.row,occurrence.context.delimiter_end.column,{right_gravity=true})
  occurrence.delimiter=content(buffer,occurrence.context.finish,occurrence.context.delimiter_end)
end

---@param state table
---@param occurrence table
---@return boolean
function M.current(state,occurrence)
  if not state.active or state.occurrence~=occurrence or state.generation~=occurrence.generation
    or vim.api.nvim_get_current_buf()~=state.buffer or vim.api.nvim_get_mode().mode:sub(1,1)~="i"
    or require("forge.client").host_get_generation()~=occurrence.host_generation then return false end
  local start,finish,delimiter=position(state.buffer,occurrence.start_mark),position(state.buffer,occurrence.end_mark),position(state.buffer,occurrence.delimiter_mark)
  if not start or not finish or not delimiter then return false end
  local cursor=vim.api.nvim_win_get_cursor(0)
  if cursor[1]~=delimiter.row+1 or cursor[2]~=delimiter.column then return false end
  if content(state.buffer,start,finish)~=(occurrence.replacement or occurrence.context.original)
    or content(state.buffer,finish,delimiter)~=occurrence.delimiter then return false end
  if state.owner.guard and not state.owner.guard(start,delimiter) then return false end
  return true
end

---Replace a prose-ending slash after a typed space or newline in the current transaction.
---@param state table
---@param occurrence table
---@return boolean
function M.replace_question_slash(state,occurrence)
  if not occurrence.context.question_slash or not M.current(state,occurrence) then return false end
  local finish=position(state.buffer,occurrence.end_mark)
  local view=vim.fn.winsaveview()
  state.applying=true
  vim.api.nvim_buf_set_text(state.buffer,finish.row,finish.column,finish.row,finish.column+1,{"?"})
  -- The word-end anchor must not absorb punctuation inserted at its boundary.
  vim.api.nvim_buf_set_extmark(state.buffer,namespace,finish.row,finish.column,
    {id=occurrence.end_mark,right_gravity=true})
  occurrence.delimiter="?"..occurrence.delimiter:sub(2)
  local delimiter=position(state.buffer,occurrence.delimiter_mark)
  vim.api.nvim_win_set_cursor(0,{delimiter.row+1,delimiter.column})
  vim.fn.winrestview({topline=view.topline,leftcol=view.leftcol,topfill=view.topfill})
  state.own_tick=vim.api.nvim_buf_get_changedtick(state.buffer)
  state.applying=false
  occurrence.slash_applied=true
  state.armed=occurrence
  return true
end

---@param state table
---@param occurrence table
---@return boolean
function M.remove_punctuation_space(state,occurrence)
  local whitespace=occurrence.context.punctuation_space
  if not whitespace or not M.current(state,occurrence) then return false end
  local finish=position(state.buffer,occurrence.end_mark)
  local prefix_length=#occurrence.context.before_boundary-#whitespace
  state.applying=true
  vim.api.nvim_buf_set_text(state.buffer,finish.row,finish.column+prefix_length,
    finish.row,finish.column+#occurrence.context.before_boundary,{""})
  occurrence.delimiter=occurrence.delimiter:sub(1,prefix_length)..occurrence.delimiter:sub(#occurrence.context.before_boundary+1)
  local delimiter=position(state.buffer,occurrence.delimiter_mark)
  vim.api.nvim_win_set_cursor(0,{delimiter.row+1,delimiter.column})
  state.own_tick=vim.api.nvim_buf_get_changedtick(state.buffer)
  state.applying=false
  occurrence.spacing_applied=true
  state.armed=occurrence
  return true
end

---@param state table
---@param occurrence table
---@param result table
---@return boolean
function M.apply(state,occurrence,result)
  if result.decision~="replace" or result.occurrence~=occurrence.id or result.original~=occurrence.context.original
    or type(result.replacement)~="string" or result.replacement=="" or result.replacement:find("%s")
    or result.profile~=occurrence.profile.profile
    or result.profile_revision~=occurrence.profile.profile_revision
    or result.model_revision~=occurrence.profile.model_revision
    or result.dictionary_revision~=occurrence.profile.dictionary_revision
    or not M.current(state,occurrence) then return false end
  local start,finish=position(state.buffer,occurrence.start_mark),position(state.buffer,occurrence.end_mark)
  local view=vim.fn.winsaveview()
  state.applying=true
  vim.api.nvim_buf_set_text(state.buffer,start.row,start.column,finish.row,finish.column,{result.replacement})
  occurrence.replacement=result.replacement
  local delimiter=position(state.buffer,occurrence.delimiter_mark)
  vim.api.nvim_win_set_cursor(0,{delimiter.row+1,delimiter.column})
  vim.fn.winrestview({topline=view.topline,leftcol=view.leftcol,topfill=view.topfill})
  state.own_tick=vim.api.nvim_buf_get_changedtick(state.buffer)
  state.applying=false
  state.armed=occurrence
  return true
end

---@param state table
---@return boolean
function M.reject(state)
  local occurrence=state.armed
  if not occurrence or not M.current(state,occurrence) then return false end
  local start,finish=position(state.buffer,occurrence.start_mark),position(state.buffer,occurrence.delimiter_mark)
  state.applying=true
  local restored=occurrence.context.original..(occurrence.context.before_boundary or "")
  vim.api.nvim_buf_set_text(state.buffer,start.row,start.column,finish.row,finish.column,{restored})
  vim.api.nvim_win_set_cursor(0,{start.row+1,start.column+#restored})
  state.own_tick=vim.api.nvim_buf_get_changedtick(state.buffer)
  state.applying=false
  state.suppressed={row=start.row,column=start.column,original=occurrence.context.original,
    spelling=occurrence.replacement~=nil or occurrence.spelling_suppressed,
    punctuation=occurrence.spacing_applied==true or occurrence.slash_applied==true}
  state.armed,state.occurrence=nil,nil
  state.generation=state.generation+1
  return true
end

---@param state table
function M.clear(state)
  if vim.api.nvim_buf_is_valid(state.buffer) then vim.api.nvim_buf_clear_namespace(state.buffer,namespace,0,-1) end
  state.occurrence,state.armed=nil,nil
end

return M
