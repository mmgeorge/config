-- Unit tests for `forge.text.context` (context.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "text_empty_buffer", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. '/nvim')
local context = require('forge.text.context')
local buffer = vim.api.nvim_create_buf(false, true)
vim.api.nvim_set_current_buf(buffer)
local index = context.index(buffer)
local owner = { kind = 'prompt' }

local function assert_synchronized()
  local actual = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  assert(index.shadow:rows() == #actual, vim.inspect({ actual = actual, shadow = index.shadow:text() }))
  assert(vim.deep_equal(index.shadow:text(), actual))
end

local function capture(row, column)
  local result, failure, done
  context.capture(index, { row = row, column = column, after = { row = row, column = column } }, owner,
    function() return true end, function(value, message) result, failure, done = value, message, true end)
  assert(vim.wait(1000, function() return done end, 5))
  assert(failure == nil, failure)
  return result
end

for _, replacement in ipairs({ {}, { '' } }) do
  local lines = {}
  for row = 1, 150 do lines[row] = 'Please preserve this line.' end
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  assert_synchronized()
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, replacement)
  assert_synchronized()
  assert(index.shadow:row(0) == '')
  assert(capture(0, 0) == nil)
  assert(capture(149, 4) == nil, 'deleted row produced context')
  assert(capture(-1, 0) == nil, 'negative row produced context')
  assert(not context.allowed(index, owner, { row = 149, column = 0 }, { row = 149, column = 4 }))
  assert(context.allowed(index, owner, { row = 0, column = 0 }, { row = 0, column = 0 }))
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { 'Please recieve ' })
  assert_synchronized()
  assert(capture(0, 14).original == 'recieve', 'capture did not recover after clearing')
end

vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { 'first', 'second', 'third' })
vim.api.nvim_buf_set_lines(buffer, 1, 2, false, {})
assert_synchronized()
assert(vim.deep_equal(index.shadow:text(), { 'first', 'third' }), 'partial deletion duplicated a surviving line')
vim.api.nvim_buf_set_lines(buffer, 0, 0, false, {})
assert_synchronized()
vim.api.nvim_buf_set_lines(buffer, 0, 1, false, {})
assert_synchronized()
assert(vim.deep_equal(index.shadow:text(), { 'third' }), 'prefix deletion duplicated the next line')
vim.cmd('normal! ggVGd')
assert_synchronized()
assert(capture(0, 0) == nil)

local lines = {}
for row = 1, 96 do lines[row] = 'Continuous prose stays available.' end
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
local tick = vim.api.nvim_buf_get_changedtick(buffer)
local result, failure, done
context.capture(index, { row = 63, column = 16, after = { row = 63, column = 16 } }, owner,
  function() return vim.api.nvim_buf_get_changedtick(buffer) == tick end,
  function(value, message) result, failure, done = value, message, true end)
assert(not done, 'fixture did not reach asynchronous context capture')
vim.api.nvim_buf_set_lines(buffer, 0, -1, false, {})
assert_synchronized()
assert(vim.wait(1000, function() return done end, 5))
assert(result == nil and failure == nil, failure or 'stale capture survived deletion')
print('text empty buffer: complete deletion, partial deletion, stale rows, and capture cancellation passed')
vim.cmd('qa!')
  end },
  { name = "text_reload", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd()..'/nvim')
local context=require('forge.text.context')
local path=vim.fn.tempname()..'.md'
local success,failure=xpcall(function()
  vim.fn.writefile({'```','Hidden text.'},path)
  vim.cmd.edit(path)
  local buffer=vim.api.nvim_get_current_buf()
  local first=context.index(buffer)
  local second=context.index(buffer)
  assert(first==second,'consumers did not share their buffer index')
  assert(context.line(first,{kind='markdown'},1)==nil)
  local paragraph={}
  for row=1,100 do paragraph[row]='Please recieve this sentence.' end
  vim.api.nvim_buf_set_lines(buffer,0,-1,false,paragraph)
  local completed,captured,message
  context.capture(first,{row=99,column=14,after={row=99,column=15}},{kind='markdown'},
    function() return true end,function(value,error) completed,captured,message=true,value,error end)
  assert(not completed,'capture did not yield')
  vim.fn.writefile({'Visible text.'},path)
  vim.cmd.edit({bang=true})
  local refreshed=context.index(buffer)
  assert(refreshed.active)
  assert(vim.deep_equal(refreshed.shadow:text(),{'Visible text.'}))
  assert(context.line(refreshed,{kind='markdown'},0)=='Visible text.','reload retained cached fence state')
  assert(vim.wait(1000,function() return completed end,5))
  assert(captured==nil and message==nil,'capture crossed a buffer revision')
  context.release(first)
  context.release(second)
  local retained=context.index(buffer)
  context.release(refreshed)
  assert(retained.active,'releasing one consumer disabled another')
  vim.api.nvim_buf_set_lines(buffer,0,-1,false,{})
  assert(vim.deep_equal(retained.shadow:text(),{''}))
  vim.api.nvim_set_current_buf(vim.api.nvim_create_buf(true,false))
  vim.api.nvim_buf_delete(buffer,{unload=true,force=true})
  assert(not retained.active,'unloaded index stayed live')
  vim.fn.bufload(buffer)
  local reopened=context.index(buffer)
  assert(reopened~=retained and reopened.active)
  context.release(retained)
  assert(reopened.active,'old consumer invalidated reopened index')
  context.release(reopened)
end,debug.traceback)
vim.fn.delete(path)
assert(success,failure)
print('text reload: shared ownership, reload, capture cancellation, unload, and reattachment passed')
vim.cmd('qa!')
  end },
  { name = "context", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd()..'/nvim')
local capture=require('forge.text.context')
local buffer=vim.api.nvim_create_buf(false,true)
vim.api.nvim_set_current_buf(buffer)
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{'# first line','Please recieve a','package, with `teh` unchanged.','', '```rust','let recieve = true;','```','After code.'})
local index=capture.index(buffer)
local result,done
capture.capture(index,{row=1,column=14,after={row=1,column=15}},{kind='markdown'},function()return true end,function(value) result=value;done=true end)
assert(vim.wait(1000,function()return done end,5))
assert(result and result.text=='first line\nPlease recieve a\npackage, with `teh` unchanged.',vim.inspect(result))
assert(result.text:sub(result.target.start+1,result.target['end'])=='recieve')
assert(#result.protected>0)
result,done=nil,false
capture.capture(index,{row=5,column=11,after={row=5,column=12}},{kind='markdown'},function()return true end,function(value) result=value;done=true end)
assert(vim.wait(1000,function()return done end,5))
assert(result==nil,'fenced code was admitted')
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{'Read src/recieve.rs and `teh` please.'})
result,done=nil,false
capture.capture(index,{row=0,column=16,after={row=0,column=17}},{kind='markdown'},function()return true end,function(value)result=value;done=true end)
assert(vim.wait(1000,function()return done end,5))
assert(result==nil,'path was admitted')
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{'Écrivez “café” '})
result,done=nil,false
capture.capture(index,{row=0,column=#'Écrivez “café”',after={row=0,column=#'Écrivez “café” '}},
  {kind='markdown'},function()return true end,function(value)result=value;done=true end)
assert(vim.wait(1000,function()return done end,5))
assert(result and result.original=='café',vim.inspect(result))
assert(result.text:sub(result.target.start+1,result.target['end'])=='café')
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{"Say 'don't' "})
result,done=nil,false
capture.capture(index,{row=0,column=#"Say 'don't'",after={row=0,column=#"Say 'don't' "}},
  {kind='markdown'},function()return true end,function(value)result=value;done=true end)
assert(vim.wait(1000,function()return done end,5))
assert(result and result.original=="don't",vim.inspect(result))
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{"Say 'recieve' "})
result,done=nil,false
capture.capture(index,{row=0,column=#"Say 'recieve'",after={row=0,column=#"Say 'recieve' "}},
  {kind='markdown'},function()return true end,function(value)result=value;done=true end)
assert(vim.wait(1000,function()return done end,5))
assert(result and result.original=='recieve' and result.before_boundary=="'",vim.inspect(result))
assert(result.text:sub(result.target.start+1,result.target['end'])=='recieve','closing quote entered spelling target')
for _,text in ipairs({'Use `code with recieve ', 'Use ``code with ` and recieve`` '}) do
  local protected=require('forge.text.prose').protected(text)
  local target=assert(text:find('recieve',1,true))-1
  local covered=false
  for _,range in ipairs(protected) do covered=covered or range.start<=target and range['end']>=target+7 end
  assert(covered,'inline code was not protected: '..text)
end
local prose=require('forge.text.prose')
for _,text in ipairs({'"D:/my files/資料 Ω.txt"', '"../my files/資料 Ω.txt"', 'Ω.txt', '資料.文書'}) do
  local covered=false
  for _,range in ipairs(prose.protected(text)) do covered=covered or range.start==0 and range['end']==#text end
  assert(covered,'technical path was not fully protected: '..text)
end
local quoted_prose='Say "recieve, please."'
local target=assert(quoted_prose:find('recieve',1,true))-1
for _,range in ipairs(prose.protected(quoted_prose)) do
  assert(not (range.start<=target and range['end']>=target+7),'ordinary quoted prose was protected')
end
for _,word in ipairs({"recieve", "don't", "don’t", "i’ll", "UPPER"}) do assert(prose.supports_english_correction(word),word) end
for _,word in ipairs({'Ω','日本語','café','café','recіeve',"''"}) do
  assert(not prose.supports_english_correction(word),'unsupported English candidate: '..word)
end
local lines={}
for row=1,4096 do lines[row]='Continuous prose continues here.' end
lines[2048]='Please recieve '
vim.api.nvim_buf_set_lines(buffer,0,-1,false,lines)
result,done=nil,false
capture.capture(index,{row=2047,column=14,after={row=2047,column=15}},
  {kind='markdown'},function()return true end,function(value)result=value;done=true end)
assert(vim.wait(3000,function()return done end,5))
assert(result and result.text==table.concat(lines,'\n'),'long paragraph was truncated')
assert(result.text:sub(result.target.start+1,result.target['end'])=='recieve')
print('text context: multiline mapping, protected spans, fences, and incremental replacement passed')
vim.cmd('qa!')
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
