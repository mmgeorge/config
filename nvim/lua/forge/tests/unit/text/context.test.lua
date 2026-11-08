-- Unit tests for `forge.text.context` (context.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer_support = dofile("nvim/lua/forge/tests/support/buffer.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["text_empty_buffer"] = function()
  local context = require('forge.text.context')
  local buffer = buffer_support.scratch()
  vim.api.nvim_set_current_buf(buffer)
  local index = context.index(buffer)
  local owner = { kind = 'prompt' }

  local function assert_synchronized()
    local actual = buffer_support.lines(buffer)
    assert(index.shadow:rows() == #actual, vim.inspect({ actual = actual, shadow = index.shadow:text() }))
    eq(index.shadow:text(), actual)
  end

  local function capture(row, column)
    local result, failure, done
    context.capture(index, { row = row, column = column, after = { row = row, column = column } }, owner,
      function() return true end, function(value, message) result, failure, done = value, message, true end)
    wait_until(function() return done end, "context capture did not finish")
    eq(failure, nil, { fail_reason = failure })
    return result
  end

  for _, replacement in ipairs({ {}, { '' } }) do
    local lines = {}
    for row = 1, 150 do lines[row] = 'Please preserve this line.' end
    buffer_support.set(buffer, lines)
    assert_synchronized()
    buffer_support.set(buffer, replacement)
    assert_synchronized()
    eq(index.shadow:row(0), '')
    eq(capture(0, 0), nil)
    eq(capture(149, 4), nil, { fail_reason = 'deleted row produced context' })
    eq(capture(-1, 0), nil, { fail_reason = 'negative row produced context' })
    assert(not context.allowed(index, owner, { row = 149, column = 0 }, { row = 149, column = 4 }))
    assert(context.allowed(index, owner, { row = 0, column = 0 }, { row = 0, column = 0 }))
    buffer_support.set(buffer, { 'Please recieve ' })
    assert_synchronized()
    eq(capture(0, 14).original, 'recieve', { fail_reason = 'capture did not recover after clearing' })
  end

  buffer_support.set(buffer, { 'first', 'second', 'third' })
  vim.api.nvim_buf_set_lines(buffer, 1, 2, false, {})
  assert_synchronized()
  eq(index.shadow:text(), { 'first', 'third' }, { fail_reason = 'partial deletion duplicated a surviving line' })
  vim.api.nvim_buf_set_lines(buffer, 0, 0, false, {})
  assert_synchronized()
  vim.api.nvim_buf_set_lines(buffer, 0, 1, false, {})
  assert_synchronized()
  eq(index.shadow:text(), { 'third' }, { fail_reason = 'prefix deletion duplicated the next line' })
  vim.cmd('normal! ggVGd')
  assert_synchronized()
  eq(capture(0, 0), nil)

  local lines = {}
  for row = 1, 96 do lines[row] = 'Continuous prose stays available.' end
  buffer_support.set(buffer, lines)
  local tick = vim.api.nvim_buf_get_changedtick(buffer)
  local result, failure, done
  context.capture(index, { row = 63, column = 16, after = { row = 63, column = 16 } }, owner,
    function() return vim.api.nvim_buf_get_changedtick(buffer) == tick end,
    function(value, message) result, failure, done = value, message, true end)
  assert(not done, 'fixture did not reach asynchronous context capture')
  buffer_support.set(buffer, {})
  assert_synchronized()
  wait_until(function() return done end, "context capture did not settle after the buffer was cleared")
  assert(result == nil and failure == nil, failure or 'stale capture survived deletion')
end

T["text_reload"] = function()
  local context=require('forge.text.context')
  local path=temp.file('reload.md',{'```','Hidden text.'})
  vim.cmd.edit(path)
  local buffer=vim.api.nvim_get_current_buf()
  local first=context.index(buffer)
  local second=context.index(buffer)
  assert(first==second,'consumers did not share their buffer index')
  eq(context.line(first,{kind='markdown'},1), nil)
  local paragraph={}
  for row=1,100 do paragraph[row]='Please recieve this sentence.' end
  buffer_support.set(buffer, paragraph)
  local completed,captured,message
  context.capture(first,{row=99,column=14,after={row=99,column=15}},{kind='markdown'},
    function() return true end,function(value,error) completed,captured,message=true,value,error end)
  assert(not completed,'capture did not yield')
  vim.fn.writefile({'Visible text.'},path)
  vim.cmd.edit({bang=true})
  local refreshed=context.index(buffer)
  assert(refreshed.active)
  eq(refreshed.shadow:text(), {'Visible text.'})
  eq(context.line(refreshed,{kind='markdown'},0), 'Visible text.', { fail_reason = 'reload retained cached fence state' })
  wait_until(function() return completed end,'capture did not settle after the buffer reloaded')
  assert(captured==nil and message==nil,'capture crossed a buffer revision')
  context.release(first)
  context.release(second)
  local retained=context.index(buffer)
  context.release(refreshed)
  assert(retained.active,'releasing one consumer disabled another')
  buffer_support.set(buffer, {})
  eq(retained.shadow:text(), {''})
  vim.api.nvim_set_current_buf(vim.api.nvim_create_buf(true,false))
  vim.api.nvim_buf_delete(buffer,{unload=true,force=true})
  assert(not retained.active,'unloaded index stayed live')
  vim.fn.bufload(buffer)
  local reopened=context.index(buffer)
  assert(reopened~=retained and reopened.active)
  context.release(retained)
  assert(reopened.active,'old consumer invalidated reopened index')
  context.release(reopened)
end

T["context"] = function()
  local capture=require('forge.text.context')
  local buffer=buffer_support.scratch({'# first line','Please recieve a','package, with `teh` unchanged.','', '```rust','let recieve = true;','```','After code.'})
  vim.api.nvim_set_current_buf(buffer)
  local index=capture.index(buffer)
  local result,done
  capture.capture(index,{row=1,column=14,after={row=1,column=15}},{kind='markdown'},function()return true end,function(value) result=value;done=true end)
  wait_until(function()return done end,'markdown context capture did not finish')
  assert(result and result.text=='first line\nPlease recieve a\npackage, with `teh` unchanged.',vim.inspect(result))
  eq(result.text:sub(result.target.start+1,result.target['end']), 'recieve')
  assert(#result.protected>0)
  result,done=nil,false
  capture.capture(index,{row=5,column=11,after={row=5,column=12}},{kind='markdown'},function()return true end,function(value) result=value;done=true end)
  wait_until(function()return done end,'markdown context capture did not finish')
  eq(result, nil, { fail_reason = 'fenced code was admitted' })
  buffer_support.set(buffer, {'Read src/recieve.rs and `teh` please.'})
  result,done=nil,false
  capture.capture(index,{row=0,column=16,after={row=0,column=17}},{kind='markdown'},function()return true end,function(value)result=value;done=true end)
  wait_until(function()return done end,'markdown context capture did not finish')
  eq(result, nil, { fail_reason = 'path was admitted' })
  buffer_support.set(buffer, {'Écrivez “café” '})
  result,done=nil,false
  capture.capture(index,{row=0,column=#'Écrivez “café”',after={row=0,column=#'Écrivez “café” '}},
    {kind='markdown'},function()return true end,function(value)result=value;done=true end)
  wait_until(function()return done end,'markdown context capture did not finish')
  assert(result and result.original=='café',vim.inspect(result))
  eq(result.text:sub(result.target.start+1,result.target['end']), 'café')
  buffer_support.set(buffer, {"Say 'don't' "})
  result,done=nil,false
  capture.capture(index,{row=0,column=#"Say 'don't'",after={row=0,column=#"Say 'don't' "}},
    {kind='markdown'},function()return true end,function(value)result=value;done=true end)
  wait_until(function()return done end,'markdown context capture did not finish')
  assert(result and result.original=="don't",vim.inspect(result))
  buffer_support.set(buffer, {"Say 'recieve' "})
  result,done=nil,false
  capture.capture(index,{row=0,column=#"Say 'recieve'",after={row=0,column=#"Say 'recieve' "}},
    {kind='markdown'},function()return true end,function(value)result=value;done=true end)
  wait_until(function()return done end,'markdown context capture did not finish')
  assert(result and result.original=='recieve' and result.before_boundary=="'",vim.inspect(result))
  eq(result.text:sub(result.target.start+1,result.target['end']), 'recieve', { fail_reason = 'closing quote entered spelling target' })
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
  buffer_support.set(buffer, lines)
  result,done=nil,false
  capture.capture(index,{row=2047,column=14,after={row=2047,column=15}},
    {kind='markdown'},function()return true end,function(value)result=value;done=true end)
  wait_until(function()return done end,'long paragraph capture did not finish',{timeout_ms=3000})
  assert(result and result.text==table.concat(lines,'\n'),'long paragraph was truncated')
  eq(result.text:sub(result.target.start+1,result.target['end']), 'recieve')
end

return T
