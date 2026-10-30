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
for _,word in ipairs({"recieve", "don't", "UPPER"}) do assert(prose.supports_english_correction(word),word) end
for _,word in ipairs({'Ω','日本語','café','café','don’t','recіeve',"''"}) do
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
