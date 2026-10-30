local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').native()
local ok,failure=xpcall(function()
  fixture.exec([[
    contraction_requests={}
    local service=require('forge.text.service')
    local correct=service.correct
    service.correct=function(owner,occurrence,current,callback)
      local record={original=occurrence.context.original,context=occurrence.context.text}
      contraction_requests[#contraction_requests+1]=record
      correct(owner,occurrence,current,function(result,error)
        record.result=result
        record.error=error
        callback(result,error)
      end)
    end
  ]])
  local observations={}
  for _,case in ipairs({
    {prefix='I can',suffix="'t ",expected="I can't "},
    {prefix='I can',suffix='’t ',expected='I can’t '},
    {prefix='im',suffix=' ',expected="I'm "},
    {prefix="I can't",suffix=' ',expected="I can't "},
    {prefix='Say im',suffix=' ',expected="Say I'm "},
    {prefix="Say i'm",suffix=' ',expected="Say I'm "},
    {prefix='Say i’m',suffix=' ',expected='Say I’m '},
    {prefix='I cant',suffix=' ',expected="I can't "},
    {prefix='cant',suffix=' ',expected="Can't "},
  }) do
    fixture.type('<Esc>')
    fixture.exec([[
      vim.api.nvim_buf_set_lines(0,0,-1,false,{''})
      vim.api.nvim_win_set_cursor(0,{1,0})
      contraction_requests={}
    ]])
    fixture.type('i'..case.prefix)
    fixture.wait(case.prefix)
    local stages={}
    for character in case.suffix:gmatch('[%z\1-\127\194-\244][\128-\191]*') do
      fixture.type(character)
      vim.wait(200,function() return false end,10)
      stages[#stages+1]={typed=character,text=fixture.text()}
    end
    observations[#observations+1]={
      input=case.prefix..case.suffix,
      stages=stages,
      requests=fixture.exec('return contraction_requests'),
    }
    fixture.wait(case.expected)
    local requests=fixture.exec('return contraction_requests')
    assert(#requests==1,vim.inspect({case=case,requests=requests}))
    if case.suffix:sub(1,1)=="'" or case.suffix:sub(1,3)=='’' then
      assert(stages[1].text==case.prefix..stages[1].typed,vim.inspect(stages))
      assert(stages[2].text==case.prefix..stages[1].typed..'t',vim.inspect(stages))
    end
  end
  print(vim.inspect(observations))
  local errors=fixture.exec('return text_errors')
  assert(#errors==0,vim.inspect(errors))
end,debug.traceback)
fixture.close()
assert(ok,failure)
vim.cmd('qa!')
