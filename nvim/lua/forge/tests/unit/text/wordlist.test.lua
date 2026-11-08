-- Unit tests for `forge.text.wordlist` (wordlist.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["text_wordlist"] = function()
  local wordlist=require('forge.text.wordlist')
  local path=temp.file('wordlist.txt',{'# allowed words','','repo','teh','repo'})
  local completed={}
  for index=1,2 do
    wordlist.load(path,function(terms,failure)completed[index]={terms=terms,failure=failure}end)
  end
  wait_until(function()return #completed==2 end,'concurrent wordlist reads did not finish',{timeout_ms=1500})
  eq(completed[1].terms, {'repo','teh'}, { fail_reason = vim.inspect(completed) })
  assert(completed[1].terms==completed[2].terms,'concurrent reads were not coalesced')
  vim.fn.writefile({'repo','Neovim'},path)
  assert(wordlist.invalidate(path))
  local reloaded
  wordlist.load(path,function(terms,failure)assert(not failure,failure);reloaded=terms end)
  wait_until(function()return reloaded~=nil end,'wordlist reload did not finish',{timeout_ms=1500})
  eq(reloaded, {'repo','Neovim'})
  local additions={}
  for _,word in ipairs({'ok','OK','customword'}) do
    wordlist.add(path,word,function(failure) assert(not failure,failure);additions[#additions+1]=word end)
  end
  wait_until(function() return #additions==3 end,'wordlist additions did not finish',{timeout_ms=1500})
  local saved=vim.fn.readfile(path)
  assert(saved[1]=='repo' and saved[2]=='Neovim')
  local accepted
  wordlist.load(path,function(terms,failure) assert(not failure,failure);accepted=terms end)
  wait_until(function() return accepted~=nil end,'wordlist read after additions did not finish',{timeout_ms=1500})
  eq(accepted, {'repo','Neovim','ok','customword'}, { fail_reason = vim.inspect(accepted) })
  local disabled
  wordlist.add(false,'ok',function(failure) disabled=failure end)
  assert(disabled and disabled:find('Configure',1,true))
  local invalid
  wordlist.add(path,'two words',function(failure) invalid=failure end)
  assert(invalid)
  local buffer=vim.api.nvim_create_buf(true,false)
  vim.api.nvim_buf_set_name(buffer,path)
  buffer_support.set(buffer,{'unsaved vocabulary'})
  vim.bo[buffer].modified=true
  local modified
  wordlist.add(path,'anotherword',function(failure) modified=failure end)
  assert(modified and modified:find('Save the modified',1,true),vim.inspect({
    failure=modified,path=path,name=vim.api.nvim_buf_get_name(buffer),
    loaded=vim.api.nvim_buf_is_loaded(buffer),modified=vim.bo[buffer].modified,
  }))
  vim.api.nvim_buf_delete(buffer,{force=true})
  vim.fn.writefile({'# keep this comment','repo'},path,'b')
  local appended
  wordlist.add(path,'ok',function(failure) assert(not failure,failure);appended=true end)
  wait_until(function() return appended end,'wordlist append did not finish',{timeout_ms=1500})
  eq(vim.fn.readfile(path), {'# keep this comment','repo','ok'})
  vim.fn.writefile({'two words'},path)
  wordlist.invalidate(path)
  local rejected
  wordlist.load(path,function(terms,failure)assert(not terms);rejected=failure end)
  wait_until(function()return rejected~=nil end,'invalid wordlist read did not report its failure',{timeout_ms=1500})
  assert(rejected:find('expected one word per line',1,true),rejected)
  vim.fn.delete(path)
  local missing
  wordlist.load(path,function(terms,failure)assert(not terms);missing=failure end)
  wait_until(function()return missing~=nil end,'missing wordlist read did not finish',{timeout_ms=1500})
  wordlist.load(false,function(terms,failure)assert(vim.deep_equal(terms,{}) and not failure)end)
end

return T
