local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').open()
local function prepare(lines,filetype,native)
  fixture.exec([[
    local lines,filetype,native=...
    local buffer=vim.api.nvim_create_buf(true,false)
    vim.api.nvim_set_current_buf(buffer)
    vim.api.nvim_buf_set_name(buffer,(filetype=='ForgeHarnessInput' or native)
      and 'ForgeFixture://'..buffer or vim.fn.tempname()..'.'..filetype)
    vim.api.nvim_buf_set_lines(buffer,0,-1,false,lines)
    if filetype=='ForgeHarnessInput' then vim.bo.buftype='nofile' end
    vim.bo.filetype=filetype
    if filetype=='ForgeHarnessInput' then vim.api.nvim_buf_set_name(buffer,'HarnessInput://renamed-'..buffer) end
    if filetype=='lua' or filetype=='unknown-code' then vim.treesitter.start(buffer,'lua') end
    if native then
      local draft=require('forge.editable').new('harper-draft')
      require('forge.editable').register(draft,'body',0)
      require('forge.editable').attach(draft,buffer,{body={start={row=1,column=0},finish={row=1,column=#lines[2]}}},{notice=error})
    end
  ]],{lines,filetype,native or false})
end
local function expect_row(row)
  assert(vim.wait(5000,function()
    return fixture.exec([[for _,diagnostic in ipairs(vim.diagnostic.get(0))do
      if diagnostic.code=='RepeatedWords' and diagnostic.lnum==... then return true end
    end return false]],{row})
  end,10),'Harper did not report the eligible row '..row)
  local diagnostics=fixture.exec('return vim.diagnostic.get(0)')
  for _,diagnostic in ipairs(diagnostics) do assert(diagnostic.lnum==row,vim.inspect(diagnostics)) end
end
local ok,failure=xpcall(function()
  fixture.exec([[
    vim.opt.runtimepath:append(vim.fs.joinpath(vim.fn.stdpath('data'),'site'))
    require('forge').setup({text_correction={enabled=false},harper={enabled=true}})
  ]])
  prepare({'local code = "This is the the sentence."','-- Café 😀. This is the the sentence.'},'lua')
  expect_row(1)
  prepare({'local code = "This is the the sentence."','-- This is the the sentence.'},'unknown-code')
  expect_row(1)
  prepare({'/config the the','This is the the sentence.','```lua','the the','```'},'ForgeHarnessInput')
  expect_row(1)
  prepare({'This is the the sentence.','# This is the the generated text.','# ------------------------ >8 ------------------------','# the the'},'gitcommit')
  expect_row(0)
  prepare({'Read-only the the header','This is the the sentence.','Read-only the the footer'},'forge',true)
  expect_row(1)
  fixture.exec([[
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    client:request('textDocument/codeAction',{textDocument=vim.lsp.util.make_text_document_params(),
      range={start={line=0,character=0},['end']={line=0,character=23}},context={diagnostics={}}},
      function(error,result)harper_readonly_actions=result end,0)
  ]])
  assert(vim.wait(1000,function()return fixture.exec('return harper_readonly_actions~=nil')end,5))
  assert(#fixture.exec('return harper_readonly_actions')==0,'read-only content exposed a code action')
  fixture.exec([[
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    client:request('textDocument/codeAction',{textDocument=vim.lsp.util.make_text_document_params(),
      range={start={line=1,character=8},['end']={line=1,character=15}},context={diagnostics={}}},
      function(error,result)harper_stale_actions=result end,0)
  ]])
  assert(vim.wait(3000,function()return fixture.exec('return harper_stale_actions~=nil')end,5))
  fixture.exec([[
    assert(#harper_stale_actions>0)
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    local action=harper_stale_actions[1]
    vim.api.nvim_buf_set_text(0,1,0,1,0,{'Changed '})
    client.commands[action.command.command](action.command,{client_id=client.id,bufnr=vim.api.nvim_get_current_buf()})
  ]])
  fixture.wait('Read-only the the header\nChanged This is the the sentence.\nRead-only the the footer')
  local long={}
  for row=1,9000 do long[row]='Ordinary sentence.' end
  long[#long+1]='This is the the sentence.'
  prepare(long,'markdown')
  expect_row(9000)
  fixture.exec([[for _,client in ipairs(vim.lsp.get_clients({name='harper_ls'}))do client:stop()end]])
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('Harper scopes: comments, UTF-16, prompts, commits, native guards, stale fixes, and long documents passed')
vim.cmd('qa!')
