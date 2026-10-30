local M = {}
local directory=vim.fs.dirname(debug.getinfo(1,"S").source:sub(2))
local cache={}

M.default_path=vim.fs.joinpath(vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(directory))),"spell","forge.en-US.txt")

local function path_key(path)
  return vim.fs.normalize(vim.fn.fnamemodify(path,":p"))
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

return M
