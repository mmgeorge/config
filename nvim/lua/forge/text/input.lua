local M = {}
local context = require("forge.text.context")
local transaction = require("forge.text.transaction")
local service = require("forge.text.service")
local owners = {}
local input_namespace=vim.api.nvim_create_namespace("ForgeTextInput")
local sequence=0

local function invalidate(state)
  state.generation=state.generation+1
  transaction.clear(state)
end

---@param buffer integer
function M.invalidate(buffer)
  local state=owners[buffer]
  if state then invalidate(state) state.boundary=nil end
end

local function fallback(mapping)
  if not mapping or vim.tbl_isempty(mapping) then return vim.api.nvim_replace_termcodes("<BS>",true,false,true) end
  local value
  if mapping.callback then
    if mapping.expr~=1 then
      vim.schedule(mapping.callback)
      return ""
    end
    value=mapping.callback()
  elseif mapping.expr==1 then
    value=vim.api.nvim_eval(mapping.rhs)
  else
    return vim.api.nvim_replace_termcodes(mapping.rhs or "<BS>",true,false,true)
  end
  if type(value)~="string" then return "" end
  -- Expression mappings already return encoded keys unless they opt into replacement.
  if mapping.replace_keycodes==1 then return vim.api.nvim_replace_termcodes(value,true,true,true) end
  return value
end

local function boundary(state,key)
  if not state.active or vim.fn.reg_executing()~="" or vim.fn.reg_recording()~="" or vim.o.paste or vim.fn.pumvisible()==1 then return end
  local cursor=vim.api.nvim_win_get_cursor(0)
  invalidate(state)
  state.boundary={row=cursor[1]-1,column=cursor[2],key=key,started=vim.uv.hrtime()}
end

local function changed(state)
  if state.applying or state.own_tick==vim.api.nvim_buf_get_changedtick(state.buffer) then return end
  local pending=state.boundary
  state.boundary=nil
  if not pending then invalidate(state) return end
  local cursor=vim.api.nvim_win_get_cursor(0)
  pending.after={row=cursor[1]-1,column=cursor[2]}
  local inserted=table.concat(vim.api.nvim_buf_get_text(state.buffer,pending.row,pending.column,pending.after.row,pending.after.column,{}),"\n")
  if inserted=="" then invalidate(state) return end
  for character in inserted:gmatch("[%z\1-\127\194-\244][\128-\191]*") do
    if require("forge.text.prose").word_character(character) then invalidate(state) return end
  end
  local generation=state.generation
  context.capture(state.index,pending,state.owner,function() return state.active and state.generation==generation end,function(captured,failure)
    if failure then
      require("forge.infra.notifications").error(failure,"Text correction")
      return
    end
    if not captured or state.generation~=generation then return end
    local suppressed=state.suppressed
    local same_suppression=suppressed and suppressed.row==captured.start.row and suppressed.column==captured.start.column and suppressed.original==captured.original
    if same_suppression and (not captured.punctuation_space or suppressed.punctuation) then return end
    if not same_suppression then state.suppressed=nil end
    local identity=table.concat({captured.start.row,captured.start.column,captured.original},":")
    local already_evaluated=state.last_occurrence==identity or same_suppression
    if already_evaluated and not captured.punctuation_space and not captured.question_slash then return end
    state.last_occurrence=identity
    sequence=sequence+1
    local occurrence={id=tostring(sequence),generation=generation,context=captured,started=pending.started,
      spelling_suppressed=same_suppression and suppressed.spelling,
      host_generation=require("forge.client").host_get_generation()}
    state.occurrence=occurrence
    transaction.anchor(state,occurrence)
    transaction.replace_question_slash(state,occurrence)
    transaction.remove_punctuation_space(state,occurrence)
    if already_evaluated or not require("forge.text.prose").supports_english_correction(captured.original) then return end
    service.correct(state.owner,occurrence,function() return transaction.current(state,occurrence) end,function(result,failure)
      if failure then
        require("forge.infra.notifications").error(failure,"Text correction")
        return
      end
      if type(result)~="table" or result.occurrence~=occurrence.id or (result.decision~="replace" and result.decision~="unchanged") then
        require("forge.infra.notifications").error("Invalid text correction response","Text correction")
        return
      end
      if result.decision=="replace" then
        local applied=transaction.apply(state,occurrence,result)
        require("forge.infra.perf").event("diff","text.correction",{elapsed_ms=(vim.uv.hrtime()-occurrence.started)/1e6,status=applied and "applied" or "discarded"})
        if applied and require("forge.infra.perf").enabled("diff") then
          require("forge.startup_log").watch_redraw(state.buffer,occurrence.id,occurrence.started,"text.correction.redraw",{operation=occurrence.id})
        end
      else
        require("forge.infra.perf").event("diff","text.correction",{elapsed_ms=(vim.uv.hrtime()-occurrence.started)/1e6,status="unchanged"})
      end
    end)
  end)
end

---@param buffer integer
---@param owner table
function M.attach(buffer,owner)
  if not vim.api.nvim_buf_is_loaded(buffer) then return end
  if owners[buffer] and owners[buffer].active and owners[buffer].index.active then
    if owner.explicit or not owners[buffer].owner.explicit then
      M.invalidate(buffer)
      owners[buffer].owner=owner
    end
    return
  end
  M.detach(buffer)
  local state={buffer=buffer,owner=owner,active=true,generation=0,index=context.index(buffer)}
  owners[buffer]=state
  local group=vim.api.nvim_create_augroup("ForgeText"..buffer,{clear=true})
  state.group=group
  state.previous=vim.api.nvim_buf_call(buffer,function() return vim.fn.maparg("<BS>","i",false,true) end)
  state.backspace=function()
    if state.armed and transaction.current(state,state.armed) then
      vim.schedule(function() transaction.reject(state) end)
      return ""
    end
    invalidate(state)
    state.boundary=nil
    state.last_occurrence=nil
    return fallback(state.previous)
  end
  vim.keymap.set("i","<BS>",state.backspace,{buffer=buffer,expr=true,replace_keycodes=false,remap=state.previous.noremap==0,desc="Reject last text correction or Backspace"})
  vim.api.nvim_create_autocmd("InsertCharPre",{group=group,buffer=buffer,callback=function()
    if not require("forge.text.prose").word_character(vim.v.char) and vim.v.char~="'" and vim.v.char~="’" then boundary(state,vim.v.char)
    else invalidate(state) state.boundary=nil state.last_occurrence=nil end
  end})
  vim.api.nvim_create_autocmd({"TextChangedI","TextChangedP"},{group=group,buffer=buffer,callback=function() changed(state) end})
  vim.api.nvim_create_autocmd({"InsertLeave","BufLeave"},{group=group,buffer=buffer,callback=function()
    invalidate(state) state.boundary=nil state.suppressed=nil state.last_occurrence=nil
  end})
  vim.api.nvim_create_autocmd("CursorMovedI",{group=group,buffer=buffer,callback=function()
    if state.occurrence and not state.applying and not transaction.current(state,state.occurrence) then invalidate(state) end
  end})
  vim.api.nvim_create_autocmd({"BufUnload","BufWipeout"},{group=group,buffer=buffer,once=true,callback=function() M.detach(buffer) end})
  vim.api.nvim_buf_attach(buffer,false,{on_lines=function()
    if not state.active then return true end
    if not state.applying then state.generation=state.generation+1;transaction.clear(state) end
  end,on_reload=function()
    if not state.active then return true end
    invalidate(state)
    state.boundary,state.suppressed,state.last_occurrence,state.own_tick=nil,nil,nil,nil
  end,on_detach=function() M.detach(buffer) end})
  service.initialize(owner,function(_,failure) if failure and state.active then require("forge.infra.notifications").error(failure,"Text correction") end end)
end

---@param buffer integer
function M.detach(buffer)
  local state=owners[buffer]
  if not state then return end
  state.active=false
  context.release(state.index)
  transaction.clear(state)
  if vim.api.nvim_buf_is_valid(buffer) then
    for _,mapping in ipairs(vim.api.nvim_buf_get_keymap(buffer,"i")) do
      if mapping.callback==state.backspace then
        vim.keymap.del("i","<BS>",{buffer=buffer})
        if state.previous.buffer==1 then
          local options={}
          for _,name in ipairs({"expr","noremap","nowait","silent","script","replace_keycodes"}) do
            if state.previous[name]~=nil then options[name]=state.previous[name]==1 end
          end
          options.callback,options.desc=state.previous.callback,state.previous.desc
          vim.api.nvim_buf_set_keymap(buffer,"i",state.previous.lhs,state.previous.rhs or "",options)
        end
        break
      end
    end
  end
  pcall(vim.api.nvim_del_augroup_by_id,state.group)
  owners[buffer]=nil
end

function M.invalidate_all()
  for _,state in pairs(owners) do
    invalidate(state)
    state.boundary,state.last_occurrence=nil,nil
  end
end

---@return nil
function M.detach_all()
  local buffers = {}
  for buffer in pairs(owners) do buffers[#buffers+1]=buffer end
  for _, buffer in ipairs(buffers) do M.detach(buffer) end
end

vim.on_key(function(_,typed)
  local state=owners[vim.api.nvim_get_current_buf()]
  if not state or vim.api.nvim_get_mode().mode:sub(1,1)~="i" or not typed or typed=="" then return end
  if typed=="\t" or typed=="\r" or typed=="\n" then boundary(state,typed) end
end,input_namespace)

return M
