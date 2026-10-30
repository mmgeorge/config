local executable=vim.fn.exepath('harper-ls')
if executable=='' then
  executable=vim.fs.joinpath(vim.fn.stdpath('data'),'mason','packages','harper-ls',
    'harper-ls'..(vim.fn.has('win32')==1 and '.exe' or ''))
end
assert(vim.fn.executable(executable)==1,'Harper must be installed for dictionary integration')
local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').native()
local ok,failure=xpcall(function()
  fixture.exec([[
    local executable=...
    local previous=vim.fn.stdpath
    local cache=vim.fn.tempname()
    vim.fn.mkdir(cache,'p')
    vim.fn.stdpath=function(kind) return kind=='cache' and cache or previous(kind) end
    shared_dictionary=vim.fn.tempname()
    vim.fn.writefile({'# Version-controlled vocabulary','repo'},shared_dictionary)
    vim.api.nvim_buf_set_name(0,vim.fn.tempname()..'.md')
    require('forge').setup({
      text_correction={enabled=true,wordlist=shared_dictionary},
      harper={enabled=true,cmd={executable,'--stdio'},settings={linters={SpellCheck=true}}},
    })
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'Use teh spelling.','ok that is the way.'})
  ]],{executable})
  assert(vim.wait(10000,function()
    return fixture.exec([[for _,diagnostic in ipairs(vim.diagnostic.get(0)) do
      if diagnostic.lnum==0 and diagnostic.col==4 then return true end
    end return false]])
  end,10),'Harper did not flag the unknown spelling: '..vim.inspect(fixture.exec([[
    return {diagnostics=vim.diagnostic.get(0),errors=text_errors,
      clients=vim.tbl_map(function(client) return {name=client.name,initialized=client.initialized} end,
        vim.lsp.get_clients({bufnr=0}))}
  ]])))
  fixture.exec([[
    for _,diagnostic in ipairs(vim.diagnostic.get(0)) do
      assert(diagnostic.code~='OkToOkay','OkToOkay is still enabled')
    end
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    assert(client.settings['harper-ls'].userDictPath~=shared_dictionary,'Harper must not rewrite source comments')
    client:request('textDocument/codeAction',{
      textDocument=vim.lsp.util.make_text_document_params(),
      range={start={line=0,character=4},['end']={line=0,character=7}},
      context={diagnostics={}},
    },function(error,actions) shared_actions=actions;shared_action_error=error end,0)
  ]])
  assert(vim.wait(3000,function() return fixture.exec('return shared_actions~=nil') end,10))
  fixture.exec([[
    assert(not shared_action_error,vim.inspect(shared_action_error))
    local global
    for _,action in ipairs(shared_actions) do
      assert(not action.title:find('workspace dictionary',1,true),action.title)
      assert(not action.title:find('file dictionary',1,true),action.title)
      assert(not action.title:find('user dictionary',1,true),action.title)
      if action.title:find('global dictionary',1,true) then
        assert(not global,'duplicate global dictionary actions')
        global=action
      end
    end
    assert(global,vim.inspect(shared_actions))
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    previous_dictionary=client.settings['harper-ls'].userDictPath
    client.commands[global.command.command](global.command,{client_id=client.id,bufnr=vim.api.nvim_get_current_buf()})
  ]])
  assert(vim.wait(5000,function()
    return fixture.exec([[
      local client=vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1]
      return client and client.settings['harper-ls'].userDictPath~=previous_dictionary
        and #vim.diagnostic.get(0)==0
    ]])
  end,10),'Shared dictionary did not refresh Harper against unsaved text')
  fixture.exec([[
    local saved=vim.fn.readfile(shared_dictionary)
    assert(saved[1]=='# Version-controlled vocabulary')
    assert(vim.tbl_contains(saved,'teh'),vim.inspect(saved))
    vim.api.nvim_buf_set_lines(0,0,-1,false,{''})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  fixture.type('iUse teh ')
  fixture.wait('Use teh ')
  fixture.type('<Esc>')
  fixture.exec([[
    vim.fn.writefile({'repo'},shared_dictionary)
    vim.api.nvim_exec_autocmds('BufWritePost',{pattern=shared_dictionary})
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'Use teh spelling.'})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  assert(vim.wait(5000,function() return fixture.exec('return #vim.diagnostic.get(0)>0') end,10),
    'Removing an accepted word did not refresh Harper')
  fixture.exec([[
    vim.api.nvim_buf_set_lines(0,0,-1,false,{''})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  fixture.type('iUse teh ')
  fixture.wait('Use the ')
  fixture.exec([[for _,client in ipairs(vim.lsp.get_clients({name='harper_ls'})) do client:stop() end]])
  local errors=fixture.exec('return text_errors')
  assert(#errors==0,vim.inspect(errors))
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('Harper shared dictionary: one global action, preserved comments, live grammar and autocorrect reload passed')
vim.cmd('qa!')
