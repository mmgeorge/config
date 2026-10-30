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
