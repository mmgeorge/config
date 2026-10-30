local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').open()
local ok,failure=xpcall(function()
  fixture.exec([[
    vim.api.nvim_buf_set_name(0,vim.fn.tempname()..'.md')
    require('forge').setup({text_correction={enabled=false},harper={enabled=true}})
  ]])
  fixture.type('iThis is the the sentence.')
  fixture.wait('This is the the sentence.')
  assert(vim.wait(15000,function()return fixture.exec('return #vim.diagnostic.get(0)>0')end,10),'Harper did not report repeated words')
  local diagnostics=fixture.exec('return vim.diagnostic.get(0)')
  fixture.exec([[
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    local diagnostic=vim.diagnostic.get(0)[1]
    client:request('textDocument/codeAction',{textDocument=vim.lsp.util.make_text_document_params(),
      range={start={line=diagnostic.lnum,character=diagnostic.col},['end']={line=diagnostic.end_lnum,character=diagnostic.end_col}},
      context={diagnostics={diagnostic.user_data.lsp}}},function(error,result)harper_actions=result;harper_error=error end,0)
  ]])
  assert(vim.wait(3000,function()return fixture.exec('return harper_actions~=nil')end,10),'Harper returned no actions')
  local actions=fixture.exec('return harper_actions')
  assert(#actions>0,vim.inspect(actions))
  fixture.exec([[
    local action
    for _,candidate in ipairs(harper_actions) do if candidate.title:find('“the”',1,true) then action=candidate;break end end
    assert(action,vim.inspect(harper_actions))
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    client.commands[action.command.command](action.command,{client_id=client.id,bufnr=vim.api.nvim_get_current_buf()})
  ]])
  fixture.wait('This is the sentence.')
  fixture.exec([[for _,client in ipairs(vim.lsp.get_clients({name='harper_ls'}))do client:stop()end]])
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('Harper native diagnostics and guarded code action passed')
vim.cmd('qa!')
