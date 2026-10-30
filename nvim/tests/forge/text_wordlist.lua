vim.opt.runtimepath:prepend(vim.fn.getcwd()..'/nvim')
local wordlist=require('forge.text.wordlist')
local path=vim.fn.tempname()
vim.fn.writefile({'# allowed words','','repo','teh','repo'},path)
local completed={}
for index=1,2 do
  wordlist.load(path,function(terms,failure)completed[index]={terms=terms,failure=failure}end)
end
assert(vim.wait(1500,function()return #completed==2 end,5))
assert(vim.deep_equal(completed[1].terms,{'repo','teh'}),vim.inspect(completed))
assert(completed[1].terms==completed[2].terms,'concurrent reads were not coalesced')
vim.fn.writefile({'repo','Neovim'},path)
assert(wordlist.invalidate(path))
local reloaded
wordlist.load(path,function(terms,failure)assert(not failure,failure);reloaded=terms end)
assert(vim.wait(1500,function()return reloaded~=nil end,5))
assert(vim.deep_equal(reloaded,{'repo','Neovim'}))
local additions={}
for _,word in ipairs({'ok','OK','customword'}) do
  wordlist.add(path,word,function(failure) assert(not failure,failure);additions[#additions+1]=word end)
end
assert(vim.wait(1500,function() return #additions==3 end,5))
local saved=vim.fn.readfile(path)
assert(saved[1]=='repo' and saved[2]=='Neovim')
local accepted
wordlist.load(path,function(terms,failure) assert(not failure,failure);accepted=terms end)
assert(vim.wait(1500,function() return accepted~=nil end,5))
assert(vim.deep_equal(accepted,{'repo','Neovim','ok','customword'}),vim.inspect(accepted))
local disabled
wordlist.add(false,'ok',function(failure) disabled=failure end)
assert(disabled and disabled:find('Configure',1,true))
local invalid
wordlist.add(path,'two words',function(failure) invalid=failure end)
assert(invalid)
local buffer=vim.api.nvim_create_buf(true,false)
vim.api.nvim_buf_set_name(buffer,path)
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{'unsaved vocabulary'})
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
assert(vim.wait(1500,function() return appended end,5))
assert(vim.deep_equal(vim.fn.readfile(path),{'# keep this comment','repo','ok'}))
vim.fn.writefile({'two words'},path)
wordlist.invalidate(path)
local rejected
wordlist.load(path,function(terms,failure)assert(not terms);rejected=failure end)
assert(vim.wait(1500,function()return rejected~=nil end,5))
assert(rejected:find('expected one word per line',1,true),rejected)
vim.fn.delete(path)
local missing
wordlist.load(path,function(terms,failure)assert(not terms);missing=failure end)
assert(vim.wait(1500,function()return missing~=nil end,5))
wordlist.load(false,function(terms,failure)assert(vim.deep_equal(terms,{}) and not failure)end)
print('text wordlist: asynchronous coalescing, reload, comments, duplicates, malformed and missing files passed')
vim.cmd('qa!')
