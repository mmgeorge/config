local M = {}
local directory=vim.fs.dirname(debug.getinfo(1,"S").source:sub(2))
local cache={}
local pending_add={}
local publishing={}
local publication_sequence=0
local merged={}

M.default_path=vim.fs.joinpath(vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(directory))),"spell","forge.en-US.txt")
M.programming_path=vim.fs.joinpath(vim.fs.dirname(M.default_path),"programming.en-US.txt")

local function path_key(path)
  return vim.fs.normalize(vim.fn.resolve(vim.fn.fnamemodify(path,":p")))
end

--- Write all bytes asynchronously and report write and close failures.
---@param path string
---@param content string
---@param flags string
---@param callback fun(failure?: string)
local function write(path,content,flags,callback)
  vim.uv.fs_open(path,flags,438,function(open_error,descriptor)
    if open_error then vim.schedule(function() callback("Cannot write wordlist "..path..": "..open_error) end) return end
    local offset=0
    local function finish(failure)
      vim.uv.fs_close(descriptor,function(close_error)
        vim.schedule(function()
          callback(failure or (close_error and "Cannot close wordlist "..path..": "..close_error))
        end)
      end)
    end
    local function advance()
      if offset==#content then finish() return end
      vim.uv.fs_write(descriptor,content:sub(offset+1),flags=="a" and -1 or offset,function(write_error,count)
        if write_error or not count or count==0 then
          finish("Cannot write wordlist "..path..": "..(write_error or "write made no progress"))
          return
        end
        offset=offset+count
        advance()
      end)
    end
    advance()
  end)
end

---@param path string
---@return boolean
function M.invalidate(path)
  local key=path_key(path)
  local present=cache[key]~=nil
  cache[key]=nil
  return present
end

---@param path string|false
---@param callback fun(terms?: string[], failure?: string)
function M.load(path,callback)
  if path==false then callback({}) return end
  path=path_key(path)
  local previous=cache[path]
  if previous then
    if previous.terms then callback(previous.terms)
    else previous.callbacks[#previous.callbacks+1]=callback end
    return
  end
  local pending={callbacks={callback}}
  cache[path]=pending
  local function finish(terms,failure)
    vim.schedule(function()
      if cache[path]~=pending then
        for _,complete in ipairs(pending.callbacks) do M.load(path,complete) end
        return
      end
      if failure then cache[path]=nil else pending.terms=terms end
      local callbacks=pending.callbacks
      pending.callbacks={}
      for _,complete in ipairs(callbacks) do complete(terms,failure) end
    end)
  end
  local function parse(content)
    content=content:gsub("^\239\187\191","")
    local lines=(content.."\n"):gmatch("(.-)\n")
    local terms,seen,line_number={}, {},0
    local function advance()
      for _=1,256 do
        local line=lines()
        if not line then finish(terms) return end
        line_number=line_number+1
        local term=vim.trim(line)
        if term~="" and term:sub(1,1)~="#" then
          if term:find("%s") then finish(nil,path..":"..line_number..": expected one word per line") return end
          if not seen[term] then terms[#terms+1]=term;seen[term]=true end
        end
      end
      vim.schedule(advance)
    end
    advance()
  end
  vim.uv.fs_open(path,"r",438,function(open_error,descriptor)
    if open_error then finish(nil,"Cannot read text wordlist "..path..": "..open_error) return end
    vim.uv.fs_fstat(descriptor,function(stat_error,stat)
      if stat_error or not stat or stat.type~="file" then
        vim.uv.fs_close(descriptor)
        finish(nil,"Cannot read text wordlist "..path..": "..(stat_error or "not a regular file"))
        return
      end
      vim.uv.fs_read(descriptor,stat.size,0,function(read_error,content)
        vim.uv.fs_close(descriptor,function(close_error)
          if read_error or close_error then
            finish(nil,"Cannot read text wordlist "..path..": "..(read_error or close_error))
          else vim.schedule(function()parse(content)end) end
        end)
      end)
    end)
  end)
end

--- Merge vendored programming vocabulary with the configured personal dictionary.
---@param path string|false
---@param callback fun(terms?: string[], failure?: string)
function M.vocabulary(path,callback)
  M.load(M.programming_path,function(programming,failure)
    if failure then callback(nil,failure) return end
    M.load(path,function(personal,personal_failure)
      if personal_failure then callback(nil,personal_failure) return end
      local key=path==false and false or path_key(path)
      local previous=merged[key]
      if not previous or previous.programming~=programming or previous.personal~=personal then
        local terms,seen={},{}
        for _,list in ipairs({programming,personal}) do
          for _,term in ipairs(list) do
            local normalized=vim.fn.tolower(term)
            if not seen[normalized] then terms[#terms+1]=term;seen[normalized]=true end
          end
        end
        previous={programming=programming,personal=personal,terms=terms}
        merged[key]=previous
      end
      callback(previous.terms)
    end)
  end)
end

--- Append accepted words without rewriting comments or losing simultaneous editor additions.
---@param path string|false
---@param word string
---@param callback fun(failure?: string)
function M.add(path,word,callback)
  if path==false then callback("Configure text_correction.wordlist to add global words") return end
  if type(word)~="string" or word=="" or word:find("%s") or word:sub(1,1)=="#" then
    callback("Global dictionary entries must contain one word") return
  end
  path=path_key(path)
  local queue=pending_add[path]
  if queue then queue[#queue+1]={word=word,callback=callback} return end
  queue={{word=word,callback=callback}}
  pending_add[path]=queue
  local function advance()
    local item=queue[1]
    if not item then pending_add[path]=nil return end
    local function finish(failure)
      table.remove(queue,1)
      item.callback(failure)
      advance()
    end
    for _,buffer in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_loaded(buffer) and vim.bo[buffer].modified
        and path_key(vim.api.nvim_buf_get_name(buffer))==path then
        finish("Save the modified global dictionary before adding a word: "..path)
        return
      end
    end
    M.invalidate(path)
    M.load(path,function(terms,failure)
      if failure then finish(failure) return end
      for _,term in ipairs(terms) do
        if vim.fn.tolower(term)==vim.fn.tolower(item.word) then finish() return end
      end
      -- A leading newline preserves a final entry in files without a trailing newline.
      write(path,"\n"..item.word.."\n","a",function(write_failure)
        M.invalidate(path)
        if not write_failure then
          if package.loaded["forge.text.input"] then require("forge.text.input").invalidate_all() end
          vim.cmd("checktime")
        end
        finish(write_failure)
      end)
    end)
  end
  advance()
end

--- Publish a plain-word Harper dictionary derived from the same configured vocabulary.
---@param owner {root: string}
---@param callback fun(path?: string, failure?: string)
function M.harper_dictionary(owner,callback)
  local config=require("forge.infra.config").options.text_correction
  M.vocabulary(config.wordlist,function(terms,failure)
    if failure then callback(nil,failure) return end
    local combined=vim.list_extend(vim.deepcopy(terms),config.terminology)
    vim.list_extend(combined,config.project_terms[owner.root] or {})
    table.sort(combined)
    local content=table.concat(combined,"\n").."\n"
    local directory=vim.fs.joinpath(vim.fn.stdpath("cache"),"forge","harper")
    local created,create_failure=pcall(vim.fn.mkdir,directory,"p")
    if not created then callback(nil,tostring(create_failure)) return end
    local path=vim.fs.joinpath(directory,"dictionary-"..vim.fn.sha256(content)..".txt")
    if publishing[path] then publishing[path][#publishing[path]+1]=callback return end
    publishing[path]={callback}
    local function finish(failure)
      local callbacks=publishing[path]
      publishing[path]=nil
      for _,complete in ipairs(callbacks) do complete(not failure and path or nil,failure) end
    end
    vim.uv.fs_stat(path,function(stat_error,stat)
      vim.schedule(function()
        if stat then finish() return end
        if stat_error and not stat_error:find("ENOENT",1,true) then finish(stat_error) return end
        publication_sequence=publication_sequence+1
        local temporary=path.."."..vim.fn.getpid().."."..publication_sequence..".tmp"
        write(temporary,content,"w",function(write_failure)
          if write_failure then
            vim.uv.fs_unlink(temporary,function() vim.schedule(function() finish(write_failure) end) end)
            return
          end
          vim.uv.fs_rename(temporary,path,function(rename_error)
            if rename_error then
              vim.uv.fs_unlink(temporary,function()
                vim.schedule(function() finish("Cannot publish Harper dictionary: "..rename_error) end)
              end)
            else vim.schedule(function() finish() end) end
          end)
        end)
      end)
    end)
  end)
end

return M
