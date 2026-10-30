local M = {}
local context=require('forge.text.context')
local state_by_buffer={}
local action_by_id={}
local action_sequence=0
local watching_install=false
local source_language={c='c',cpp='cpp',cs='csharp',clojure='clojure',cmake='cmake',dart='dart',elixir='elixir',
  go='go',groovy='groovy',haskell='haskell',html='html',java='java',javascript='javascript',
  javascriptreact='javascriptreact',lua='lua',nix='nix',php='php',powershell='powershell',python='python',
  r='r',ruby='ruby',rust='rust',scala='scala',sh='shellscript',solidity='solidity',swift='swift',
  toml='toml',typescript='typescript',typescriptreact='typescriptreact',zig='zig'}
local command_name='forge.harper.apply'

local function configuration()
  return require('forge.infra.config').options.harper
end

local function position(state,value)
  if not state.index.active or type(value)~='table' or type(value.line)~='number' or type(value.character)~='number'
    or value.line<0 or value.character<0 or value.line%1~=0 or value.character%1~=0 then return nil end
  if value.line>=state.index.shadow:rows() then return nil end
  local line=state.index.shadow:row(value.line)
  if not line then return nil end
  local success,column=pcall(vim.str_byteindex,line,'utf-16',value.character,true)
  if not success or column>#line then return nil end
  return {row=value.line,column=column}
end

local function allowed(state,range)
  if not state or not range then return false end
  local first,last=position(state,range.start),position(state,range['end'])
  return first~=nil and last~=nil and context.allowed(state.index,state.owner,first,last)
end

local function edit_allowed(state,edit)
  if not edit then return true end
  for uri,changes in pairs(edit.changes or {}) do
    if vim.uri_to_bufnr(uri)~=state.buffer then return false end
    for _,change in ipairs(changes) do if not allowed(state,change.range) then return false end end
  end
  for _,document in ipairs(edit.documentChanges or {}) do
    if not document.textDocument or vim.uri_to_bufnr(document.textDocument.uri)~=state.buffer then return false end
    for _,change in ipairs(document.edits) do if not allowed(state,change.range) then return false end end
  end
  return true
end

local function valid(state,generation,tick)
  return state.active and state.index.active and state_by_buffer[state.buffer]==state and state.generation==generation
    and vim.api.nvim_buf_is_valid(state.buffer) and vim.api.nvim_buf_get_changedtick(state.buffer)==tick
end

local function apply(command)
  local record=action_by_id[command.arguments and command.arguments[1]]
  if not record then return end
  action_by_id[command.arguments[1]]=nil
  local state=record.state
  if not valid(state,record.generation,record.tick) or not edit_allowed(state,record.action.edit) then
    require('forge.infra.notifications').error('Harper suggestion is outdated or outside editable prose','Harper')
    return
  end
  local client=vim.lsp.get_client_by_id(state.client_id)
  if not client then return end
  require('forge.text').invalidate(state.buffer)
  if record.action.edit then vim.lsp.util.apply_workspace_edit(record.action.edit,'utf-16') end
  local server_command=record.action.command
  if type(server_command)=='table' then client:exec_cmd(server_command,{bufnr=state.buffer})
  elseif type(server_command)=='string' then client:exec_cmd(record.action,{bufnr=state.buffer}) end
end

local function diagnostics(error,result,handler_context)
  if error or not result then return end
  local state=state_by_buffer[vim.uri_to_bufnr(result.uri)]
  if not state or not state.active or not state.index.active or state.client_id~=handler_context.client_id then return end
  if type(result.version)=='number' and result.version~=vim.lsp.util.buf_versions[state.buffer] then return end
  local filtered=vim.deepcopy(result)
  filtered.diagnostics={}
  for _,diagnostic in ipairs(result.diagnostics or {}) do
    if allowed(state,diagnostic.range) then filtered.diagnostics[#filtered.diagnostics+1]=diagnostic end
  end
  vim.lsp.diagnostic.on_publish_diagnostics(nil,filtered,handler_context)
end

local function transport(command,directory)
  return function(dispatchers)
    local alias_by_uri,document_uri_by_buffer,buffer_by_document_uri={},{},{}
    local uri_by_alias=setmetatable({},{__index=function(_,uri)
      local buffer=buffer_by_document_uri[uri]
      if buffer and vim.api.nvim_buf_is_valid(buffer) then return vim.uri_from_bufnr(buffer) end
    end})
    local function register_document(params,opening)
      local document=params and params.textDocument
      if not document or not document.uri then return end
      local uri=document.uri
      local buffer=vim.uri_to_bufnr(uri)
      local server_uri=document_uri_by_buffer[buffer]
      if not server_uri and opening then
        server_uri=uri:match('^file:') and uri or vim.uri_from_fname(vim.fs.joinpath(directory,'document-'..buffer..'.md'))
        document_uri_by_buffer[buffer]=server_uri
        buffer_by_document_uri[server_uri]=buffer
      end
      if server_uri then alias_by_uri[uri]=server_uri end
    end
    local function translate(value,mapping)
      if type(value)=='string' then return mapping[value] or value end
      if type(value)~='table' then return value end
      local translated=vim.deepcopy(value)
      for key,child in pairs(value) do
        local translated_key=mapping[key] or key
        if translated_key~=key then translated[key]=nil end
        translated[translated_key]=translate(child,mapping)
      end
      return translated
    end
    local adapted=vim.tbl_extend('force',dispatchers,{
      notification=function(method,params)
        return dispatchers.notification(method,translate(params,uri_by_alias))
      end,
      server_request=function(method,params)
        return dispatchers.server_request(method,translate(params,uri_by_alias))
      end,
    })
    local connection=vim.lsp.rpc.start(command,adapted,{cwd=directory})
    local notify=connection.notify
    connection.notify=function(method,params)
      register_document(params,method=='textDocument/didOpen')
      return notify(method,translate(params,alias_by_uri))
    end
    local request=connection.request
    connection.request=function(method,params,callback,notify_reply)
      register_document(params,false)
      if method~='textDocument/codeAction' then
        return request(method,translate(params,alias_by_uri),function(error,result)
          callback(error,translate(result,uri_by_alias))
        end,notify_reply)
      end
      local state=state_by_buffer[vim.uri_to_bufnr(params.textDocument.uri)]
      if not state or not allowed(state,params.range) then callback(nil,{}) return true end
      M.invalidate(state.buffer)
      local generation,tick=state.generation,vim.api.nvim_buf_get_changedtick(state.buffer)
      return request(method,translate(params,alias_by_uri),function(error,actions)
        if error then callback(error) return end
        actions=translate(actions,uri_by_alias)
        local filtered={}
        if valid(state,generation,tick) then
          for _,action in ipairs(actions or {}) do
            if edit_allowed(state,action.edit) then
              action_sequence=action_sequence+1
              action_by_id[action_sequence]={state=state,generation=generation,tick=tick,action=action}
              filtered[#filtered+1]={title=action.title,kind=action.kind,diagnostics=action.diagnostics,
                isPreferred=action.isPreferred,command={title=action.title,command=command_name,arguments={action_sequence}}}
            end
          end
        end
        callback(nil,filtered)
      end,notify_reply)
    end
    return connection
  end
end

local function executable()
  local command=vim.deepcopy(configuration().cmd)
  if vim.fn.executable(command[1])==1 then return command end
  local suffix=vim.fn.has('win32')==1 and '.exe' or ''
  local installed=vim.fs.joinpath(vim.fn.stdpath('data'),'mason','packages','harper-ls','harper-ls'..suffix)
  if vim.fn.executable(installed)==1 then command[1]=installed;return command end
end

---@param buffer integer
---@param owner table
function M.attach(buffer,owner)
  if not configuration().enabled or not vim.api.nvim_buf_is_loaded(buffer) then return end
  local state=state_by_buffer[buffer]
  if state and not state.index.active then M.detach(buffer);state=nil end
  if state and state.owner.explicit and not owner.explicit then owner=state.owner end
  local filetype=vim.bo[buffer].filetype
  local language=owner.kind=='source' and (source_language[filetype] or 'plaintext')
    or owner.kind=='prose' and 'plaintext' or 'markdown'
  if state and state.language~=language then M.detach(buffer);state=nil end
  if state then
    if (owner.explicit or not state.owner.explicit) and state.owner~=owner then
      M.invalidate(buffer)
      state.owner=owner
    end
    if state.client_id and vim.lsp.get_client_by_id(state.client_id) then return end
  else
    state={buffer=buffer,owner=owner,index=context.index(buffer),active=true,generation=0,language=language}
    state_by_buffer[buffer]=state
    vim.api.nvim_buf_attach(buffer,false,{on_lines=function()
      if not state.active then return true end
      M.invalidate(buffer)
    end,on_reload=function()
      if not state.active then return true end
      M.invalidate(buffer)
    end,on_detach=function() M.detach(buffer) end})
  end
  local command=executable()
  if not command then return end
  local directory=vim.fs.joinpath(vim.fn.stdpath('cache'),'forge','harper')
  vim.fn.mkdir(directory,'p')
  state.client_id=vim.lsp.start({name='harper_ls',cmd=transport(command,directory),root_dir=owner.root,
    get_language_id=function(target,filetype)
      local target_state=state_by_buffer[target]
      if target_state and target_state.owner.kind=='source' then return source_language[filetype] or 'plaintext' end
      return target_state and target_state.owner.kind=='prose' and 'plaintext' or 'markdown'
    end,
    capabilities={general={positionEncodings={'utf-16'}}},
    settings={['harper-ls']=configuration().settings},
    flags={debounce_text_changes=150},
    handlers={['textDocument/publishDiagnostics']=diagnostics},
    commands={[command_name]=apply},
  },{bufnr=buffer})
end

---@param buffer integer
function M.invalidate(buffer)
  local state=state_by_buffer[buffer]
  if not state then return end
  state.generation=state.generation+1
  for id,record in pairs(action_by_id) do if record.state==state then action_by_id[id]=nil end end
end

---@param buffer integer
function M.detach(buffer)
  local state=state_by_buffer[buffer]
  if not state then return end
  M.invalidate(buffer)
  state.active=false
  context.release(state.index)
  state_by_buffer[buffer]=nil
  if state.client_id and vim.api.nvim_buf_is_valid(buffer) then vim.lsp.buf_detach_client(buffer,state.client_id) end
end

function M.setup()
  if configuration().enabled then
    local ok,registry=pcall(require,'mason-registry')
    if ok and not watching_install then
      watching_install=true
      registry:on('package:install:success',function(package)
        if package.name=='harper-ls' then vim.schedule(function()
          for buffer,state in pairs(state_by_buffer) do M.attach(buffer,state.owner) end
        end) end
      end)
    end
    for _,client in ipairs(vim.lsp.get_clients({name='harper_ls'})) do
      if client.commands[command_name] then
        client.settings={['harper-ls']=vim.deepcopy(configuration().settings)}
        client:notify('workspace/didChangeConfiguration',{settings=client.settings})
      end
    end
    return
  end
  local buffers={}
  for buffer in pairs(state_by_buffer) do buffers[#buffers+1]=buffer end
  for _,buffer in ipairs(buffers) do M.detach(buffer) end
  for _,client in ipairs(vim.lsp.get_clients({name='harper_ls'})) do
    if client.commands[command_name] then client:stop() end
  end
end

return M
