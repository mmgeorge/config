local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').native()
local ok,failure=xpcall(function()
  assert(fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,'repo')]]),
    'default wordlist was not included in the profile')
  for _,term in ipairs({'coroutine','middleware','stdin','rpc','refactor','todo','opt','perf'}) do
    assert(fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,...)]],{term}),
      'programming term missing from the profile: '..term)
  end
  assert(not fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,'then')]]),
    'programming dictionary disabled contextual then/than correction')
  fixture.exec([[
    local cases={
      {'Please recieve the package.','recieve','receive'},
      {'The update should releive pressure.','releive','relieve'},
      {'Please receive the package.','receive','receive'},
      {'The update should relieve pressure.','relieve','relieve'},
      {'Read the response form the server.','form','from'},
      {'Fill out the form before submitting.','form','form'},
      {'The value is greater then zero.','then','than'},
      {'Save the file then exit.','then','then'},
      {'Do not loose your work.','loose','lose'},
      {'The cable is loose.','loose','loose'},
      {'This should definately fix the issue.','definately','definitely'},
      {'We need to seperate configuration from state.','seperate','separate'},
      {'The problem occured during startup.','occured','occurred'},
      {'Check the dependancies before building.','dependancies','dependencies'},
      {'The service recieved a request.','recieved','received'},
      {'Please recieve','recieve','receive'},
      {'The value is greater then','then','than'},
      {'Fill out the form','form','form'},
    }
    text_dictionary_gate={remaining=#cases,errors={}}
    for index,case in ipairs(cases) do
      local start=assert(case[1]:find(case[2],1,true))-1
      require('forge.client').request({method='text.correct',params={
        profile=text_ready.profile,profile_revision=text_ready.profile_revision,occurrence='dictionary-gate-'..index,
        context=case[1],target={start=start,['end']=start+#case[2]},protected={}}},function(result,failure)
        local actual=result and (result.decision=='replace' and result.replacement or case[2])
        if failure or actual~=case[3] then
          text_dictionary_gate.errors[#text_dictionary_gate.errors+1]={context=case[1],actual=actual,expected=case[3],failure=failure}
        end
        text_dictionary_gate.remaining=text_dictionary_gate.remaining-1
      end)
    end
  ]])
  assert(vim.wait(5000,function()return fixture.exec('return text_dictionary_gate.remaining')==0 end,5))
  local gate_errors=fixture.exec('return text_dictionary_gate.errors')
  assert(#gate_errors==0,vim.inspect(gate_errors))
  fixture.exec([[
    text_wordlist_path=vim.fn.tempname()
    vim.fn.writefile({'# explicitly allowed','repo','teh'},text_wordlist_path)
    require('forge').setup({text_correction={enabled=true,wordlist=text_wordlist_path},harper={enabled=false}})
    require('forge.text.service').initialize({root=vim.fn.getcwd()},function(result,failure)
      text_wordlist_ready=result;text_wordlist_failure=failure
    end)
  ]])
  assert(vim.wait(2000,function()return fixture.exec('return text_wordlist_ready~=nil or text_wordlist_failure~=nil')end,5))
  assert(fixture.exec('return text_wordlist_failure')==vim.NIL)
  fixture.type('iUse teh ')
  fixture.wait('Use teh ')
  fixture.type('<Esc>')
  fixture.exec([[
    vim.fn.writefile({'repo'},text_wordlist_path)
    vim.api.nvim_exec_autocmds('BufWritePost',{pattern=text_wordlist_path})
    vim.api.nvim_buf_set_lines(0,0,-1,false,{''})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  fixture.type('iUse teh ')
  fixture.wait('Use the ')
  assert(fixture.exec('return #text_errors')==0,vim.inspect(fixture.exec('return text_errors')))
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('text wordlist host: default repo entry, explicit allow, and save-triggered profile reload passed')
vim.cmd('qa!')
