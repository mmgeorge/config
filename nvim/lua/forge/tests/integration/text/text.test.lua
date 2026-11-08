-- Integration tests for the `text` area. Each scenario row runs in its own headless Neovim:
-- `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`. Tests that launch the Forge host
-- assume `rust-build` has produced a current executable.
local scenarios = {
  { name = "harper_dictionary", run = function()
local executable=vim.fn.exepath('harper-ls')
if executable=='' then
  executable=vim.fs.joinpath(vim.fn.stdpath('data'),'mason','packages','harper-ls',
    'harper-ls'..(vim.fn.has('win32')==1 and '.exe' or ''))
end
assert(vim.fn.executable(executable)==1,'Harper must be installed for dictionary integration')
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').native()
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
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'Use recieve spelling.','ok that is the way.'})
    -- Harper also reports grammar rules and its own vocabulary gaps, so the dictionary contract
    -- tracks only the spelling diagnostic on the misspelled word.
    function spelling_flagged()
      for _,diagnostic in ipairs(vim.diagnostic.get(0)) do
        if diagnostic.lnum==0 and diagnostic.col==4 and diagnostic.code=='SpellCheck' then return true end
      end
      return false
    end
  ]],{executable})
  assert(vim.wait(10000,function()
    return fixture.exec('return spelling_flagged()')
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
      range={start={line=0,character=4},['end']={line=0,character=11}},
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
        and not spelling_flagged()
    ]])
  end,10),'Shared dictionary did not refresh Harper against unsaved text: '..vim.inspect(fixture.exec([[
    local client=vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1]
    return {previous=previous_dictionary,current=client and client.settings['harper-ls'].userDictPath,
      diagnostics=vim.diagnostic.get(0),errors=text_errors}
  ]])))
  fixture.exec([[
    local saved=vim.fn.readfile(shared_dictionary)
    assert(saved[1]=='# Version-controlled vocabulary')
    assert(vim.tbl_contains(saved,'recieve'),vim.inspect(saved))
    vim.api.nvim_buf_set_lines(0,0,-1,false,{''})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  fixture.type('iUse recieve ')
  fixture.wait('Use recieve ')
  fixture.type('<Esc>')
  fixture.exec([[
    vim.fn.writefile({'repo'},shared_dictionary)
    vim.api.nvim_exec_autocmds('BufWritePost',{pattern=shared_dictionary})
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'Use recieve spelling.'})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  assert(vim.wait(5000,function() return fixture.exec('return spelling_flagged()') end,10),
    'Removing an accepted word did not refresh Harper')
  fixture.exec([[
    vim.api.nvim_buf_set_lines(0,0,-1,false,{''})
    vim.api.nvim_win_set_cursor(0,{1,0})
  ]])
  fixture.type('iUse recieve ')
  fixture.wait('Use receive ')
  fixture.exec([[for _,client in ipairs(vim.lsp.get_clients({name='harper_ls'})) do client:stop() end]])
  local errors=fixture.exec('return text_errors')
  assert(#errors==0,vim.inspect(errors))
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('Harper shared dictionary: one global action, preserved comments, live grammar and autocorrect reload passed')
vim.cmd('qa!')
  end },
  { name = "harper_rules", run = function()
local executable = vim.fn.exepath("harper-ls")
if executable == "" then
  executable = vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", "harper-ls",
    "harper-ls" .. (vim.fn.has("win32") == 1 and ".exe" or ""))
end
assert(vim.fn.executable(executable) == 1, "Harper must be installed for rule integration")
local root = vim.fn.tempname()
vim.fn.mkdir(root .. "/first", "p")
vim.fn.mkdir(root .. "/second", "p")
local path = root .. "/rules.json"
vim.fn.writefile({ "{}" }, path)
local fixture = dofile("nvim/lua/forge/tests/support/text_fixture.lua").open()
local success, failure = xpcall(function()
  fixture.exec([[
    local executable, root, path = ...
    local previous = vim.fn.stdpath
    vim.fn.stdpath = function(kind) return kind == "cache" and root or previous(kind) end
    require("forge").setup({ text_correction = { enabled = false }, harper = {
      enabled = true, cmd = { executable, "--stdio" }, disabled_rules = path,
      settings = { linters = { SpellCheck = true } },
    } })
    rule_buffers = {}
    for _, project in ipairs({ "first", "second" }) do
      local buffer = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_name(buffer, root .. "/" .. project .. "/text.md")
      vim.b[buffer].forge_workspace = root .. "/" .. project
      vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "The the sentence.", "Use teh spelling." })
      vim.bo[buffer].filetype = "markdown"
      rule_buffers[#rule_buffers + 1] = buffer
    end
  ]], { executable, root, path })
  assert(vim.wait(10000, function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local found = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then found = true end
        end
        if not found then return false end
      end
      return true
    ]])
  end, 10), "Harper did not publish rule identifiers in both projects")
  fixture.exec([[
    local buffer = rule_buffers[1]
    local client = assert(vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1])
    local other = assert(vim.lsp.get_clients({ bufnr = rule_buffers[2], name = "harper_ls" })[1])
    assert(client.id ~= other.id, "fixture did not create independent project clients")
    client:request("textDocument/codeAction", {
      textDocument = { uri = vim.uri_from_bufnr(buffer) },
      range = { start = { line = 0, character = 4 }, ["end"] = { line = 0, character = 4 } },
      context = { diagnostics = {} },
    }, function(error, actions) rule_error = error rule_actions = actions end, buffer)
  ]])
  assert(vim.wait(3000, function() return fixture.exec("return rule_actions ~= nil") end, 10))
  fixture.exec([[
    assert(not rule_error, vim.inspect(rule_error))
    local selected
    for _, action in ipairs(rule_actions) do
      if action.title == 'Disable Harper rule "RepeatedWords" globally' then selected = action end
    end
    assert(selected, vim.inspect(rule_actions))
    local client = assert(vim.lsp.get_clients({ bufnr = rule_buffers[1], name = "harper_ls" })[1])
    client.commands[selected.command.command](selected.command)
  ]])
  assert(vim.wait(5000, function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
        if client.settings["harper-ls"].linters.RepeatedWords ~= false then return false end
        local other_rule = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then return false end
          if diagnostic.code == "The" then other_rule = true end
        end
        if not other_rule then return false end
      end
      return true
    ]])
  end, 10), "Disabling a rule did not refresh both clients while preserving other rules: " .. vim.inspect(fixture.exec([[
    local result = { errors = text_errors, buffers = {} }
    for _, buffer in ipairs(rule_buffers) do
      local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
      result.buffers[#result.buffers + 1] = {
        diagnostics = vim.diagnostic.get(buffer), linters = client and client.settings["harper-ls"].linters,
      }
    end
    return result
  ]])))
  assert(vim.json.decode(table.concat(vim.fn.readfile(path), "\n")).RepeatedWords == false)
  fixture.exec([[
    local buffer = rule_buffers[1]
    local harper = require("forge.text.harper")
    local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
    harper.detach(buffer)
    client:stop()
    harper.attach(buffer, { kind = "markdown", root = ... })
  ]], { root .. "/first" })
  assert(vim.wait(5000, function()
    return fixture.exec([[
      local client = vim.lsp.get_clients({ bufnr = rule_buffers[1], name = "harper_ls" })[1]
      return client and client.initialized and client.settings["harper-ls"].linters.RepeatedWords == false
    ]])
  end, 10), "A new Harper client did not load persisted rules")
  fixture.exec([[
    local path = ...
    vim.fn.writefile({ "{}" }, path)
    vim.api.nvim_exec_autocmds("BufWritePost", { pattern = path })
  ]], { path })
  assert(vim.wait(5000, function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local found = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then found = true end
        end
        if not found then return false end
      end
      return true
    ]])
  end, 10), "Removing a disabled rule did not restore diagnostics")
  assert(#fixture.exec("return text_errors") == 0, vim.inspect(fixture.exec("return text_errors")))
end, debug.traceback)
fixture.exec([[for _, client in ipairs(vim.lsp.get_clients({name="harper_ls"})) do client:stop(true) end]])
fixture.close()
vim.fn.delete(root, "rf")
assert(success, failure)
print("Harper rule integration: metadata action, two projects, persistence, reconnect, and re-enable passed")
vim.cmd("qa!")
  end },
  { name = "harper_scopes", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').open()
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
  end },
  { name = "async_lifecycle", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').open()
local ok,failure=xpcall(function()
  fixture.exec('text_hold=true')
  fixture.type('iPlease recieve ')
  assert(vim.wait(1000,function()return fixture.exec('return #text_requests')==1 end,10))
  fixture.type('teh ')
  assert(vim.wait(1000,function()return fixture.exec('return #text_requests')==2 end,10))
  fixture.exec([[
    local request=text_requests[2]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='teh',replacement='the',profile='fixture',profile_revision='revision'})
    request=text_requests[1]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})
  ]])
  fixture.wait('Please recieve the ')
  fixture.type('recieve ')
  assert(vim.wait(1000,function()return fixture.exec('return #text_requests')==3 end,10))
  fixture.exec([[
    local client=require('forge.client')
    local generation=client.host_get_generation()+1
    client.host_get_generation=function()return generation end
    local request=text_requests[3]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})
  ]])
  fixture.wait('Please recieve the recieve ')
  fixture.type('teh ')
  assert(vim.wait(1000,function()return fixture.exec('return #text_requests')==4 end,10))
  fixture.exec([[
    local request=text_requests[4]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='teh',replacement='the',profile='fixture',profile_revision='obsolete'})
  ]])
  fixture.wait('Please recieve the recieve teh ')
  fixture.type('<BS>')
  fixture.wait('Please recieve the recieve teh')
  fixture.type(' ')
  assert(vim.wait(1000,function()return fixture.exec('return #text_requests')==5 end,10))
  fixture.exec([[
    local request=text_requests[5]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='teh',replacement='the',profile='other-project',profile_revision='revision'})
  ]])
  fixture.wait('Please recieve the recieve teh ')
  assert(fixture.exec('return #text_errors')==0)
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('text asynchronous lifecycle: reverse completion order, host replacement, and profile mismatch passed')
vim.cmd('qa!')
  end },
  { name = "contractions", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').native()
local ok,failure=xpcall(function()
  assert(fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,'Neovim')]]),
    'Neovim was not loaded from the shared vocabulary')
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
    {prefix='I dont',suffix=' ',expected="I don't "},
    {prefix='I wont',suffix=' ',expected="I won't "},
    {prefix='It isnt',suffix=' ',expected="It isn't "},
    {prefix='They arent',suffix=' ',expected="They aren't "},
    {prefix='It doesnt',suffix=' ',expected="It doesn't "},
    {prefix='I didnt',suffix=' ',expected="I didn't "},
    {prefix='I couldnt',suffix=' ',expected="I couldn't "},
    {prefix='I shouldnt',suffix=' ',expected="I shouldn't "},
    {prefix='dont',suffix=' ',expected="Don't "},
    {prefix='I WONT',suffix=' ',expected="I WON'T "},
    {prefix='Use tokio',suffix=' ',expected='Use tokio '},
    {prefix='Use Tokio',suffix=' ',expected='Use Tokio '},
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
      local before=fixture.text()
      fixture.type(character)
      assert(vim.wait(2500,function() return fixture.text()~=before end,5),'typed character did not reach the buffer')
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
  fixture.type('<Esc>')
  fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0})]])
  fixture.type('iI dont ')
  fixture.wait("I don't ")
  fixture.type('<BS>')
  fixture.wait('I dont')
  fixture.type(' ')
  fixture.wait('I dont ')
  print(vim.inspect(observations))
  local errors=fixture.exec('return text_errors')
  assert(#errors==0,vim.inspect(errors))
end,debug.traceback)
fixture.close()
assert(ok,failure)
vim.cmd('qa!')
  end },
  { name = "eligibility", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').open()
local ok,failure=xpcall(function()
  fixture.exec([[
    vim.opt.runtimepath:append(vim.fs.joinpath(vim.fn.stdpath('data'),'site'))
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'local name = "recieve"','-- Please recieve'})
    vim.bo.filetype='lua'
    vim.treesitter.start(0,'lua')
    vim.api.nvim_win_set_cursor(0,{2,16})
  ]])
  fixture.type('A ')
  fixture.wait('local name = "recieve"\n-- Please receive ')
  local requests=fixture.exec('return #text_requests')
  fixture.type('<Esc>')
  fixture.exec([[vim.api.nvim_win_set_cursor(0,{1,20})]])
  fixture.type('A ')
  fixture.wait('local name = "recieve" \n-- Please receive ')
  assert(fixture.exec('return #text_requests')==requests,'source code was admitted')
  fixture.type('<Esc>')
  fixture.exec([[
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'local value = "untouched" -- Please recieve'})
    vim.api.nvim_win_set_cursor(0,{1,41})
  ]])
  fixture.type('A ')
  fixture.wait('local value = "untouched" -- Please receive ')
  local captured=fixture.exec('return text_requests[#text_requests].occurrence.context.text')
  assert(captured=='Please recieve ', 'unexpected inline comment context: '..vim.inspect(captured))
  fixture.type('<Esc>')
  fixture.exec([[
    vim.api.nvim_buf_set_lines(0,0,-1,false,{'Read-only recieve','Please recieve'})
    vim.bo.filetype='forge'
    require('forge.text').attach(vim.api.nvim_get_current_buf(),{kind='native',guard=function(first,last)
      return first.row==1 and last.row==1
    end})
    vim.api.nvim_win_set_cursor(0,{2,13})
  ]])
  fixture.type('A ')
  fixture.wait('Read-only recieve\nPlease receive ')
  requests=fixture.exec('return #text_requests')
  fixture.type('<Esc>')
  fixture.exec([[vim.api.nvim_win_set_cursor(0,{1,16})]])
  fixture.type('A ')
  fixture.wait('Read-only recieve \nPlease receive ')
  assert(fixture.exec('return #text_requests')==requests,'read-only native text was admitted')
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('text eligibility: real Lua comments, code exclusion, and explicit native regions passed')
vim.cmd('qa!')
  end },
  { name = "text", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').native({cold_text='Cold text '})
local ok,failure=xpcall(function()
  fixture.type('<Esc>')
  fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0})]])
  assert(fixture.exec('return require("forge.client")._client.harness_ready')~=true,'text initialized Harness')
  fixture.type('iPlease recieve ')
  fixture.wait('Please receive ')
  fixture.type('<BS>')
  fixture.wait('Please recieve')
  fixture.type(' ')
  fixture.wait('Please recieve ')
  fixture.exec([[
    local client=require('forge.client')
    local request={method='text.correct',params={profile=text_ready.profile,profile_revision=text_ready.profile_revision,
      occurrence='wire-example',context='Please recieve',target={start=7,['end']=14},protected={}}}
    client.request(request,function(result,error) text_wire={request=request,result=result,error=error} end)
  ]])
  assert(vim.wait(2000,function()return fixture.exec('return text_wire~=nil')end,10))
  local wire=fixture.exec('return text_wire')
  assert(wire.result and wire.result.replacement=='receive',vim.inspect(wire))
  fixture.exec([[
    local request={method='text.correct',params={profile=text_ready.profile,profile_revision=text_ready.profile_revision,
      occurrence='unchanged-example',context='Please receive',target={start=7,['end']=14},protected={}}}
    require('forge.client').request(request,function(result,error)text_unchanged_wire={request=request,result=result,error=error} end)
  ]])
  assert(vim.wait(2000,function()return fixture.exec('return text_unchanged_wire~=nil')end,10))
  local unchanged=fixture.exec('return text_unchanged_wire')
  assert(unchanged.result.decision=='unchanged',vim.inspect(unchanged))
  vim.fn.writefile({vim.json.encode({initialize=fixture.exec('return text_initialize_wire'),replace=wire,unchanged=unchanged})},
    'nvim/rust/forge/target/text-api-wire.json')
  fixture.exec([[
    local request={method='text.correct',session_id='forbidden',params={}}
    require('forge.client').request(request,function(_,error) text_policy_error=error end)
  ]])
  assert(type(fixture.exec('return text_policy_error'))=='string','text method accepted session context')
  fixture.exec([[require('forge.client').host_stop(nil,function()text_stopped=true end)]])
  assert(vim.wait(5000,function()return fixture.exec('return text_stopped==true')end,10),'text host did not stop')
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('text host: local model, typed correction, rejection, session independence, wire contract, and shutdown passed')
vim.cmd('qa!')
  end },
  { name = "question_slash", run = function()
local fixture = dofile("nvim/lua/forge/tests/support/text_fixture.lua").open()

---@param text string
local function begin(text)
  fixture.type("<Esc>")
  fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0})]])
  fixture.type("i" .. text)
  fixture.wait(text)
end

local ok, failure = xpcall(function()
  for _, ending in ipairs({ { " ", " " }, { "<CR>", "\n" } }) do
    begin("foo/")
    fixture.type(ending[1])
    fixture.wait("foo?" .. ending[2])
    local cursor = fixture.exec("return vim.api.nvim_win_get_cursor(0)")
    assert(cursor[1] == (ending[2] == "\n" and 2 or 1))
    assert(cursor[2] == (ending[2] == "\n" and 0 or 5))
    fixture.type("<BS>")
    fixture.wait("foo/")
    fixture.type(ending[1])
    fixture.wait("foo/" .. ending[2])
  end

  begin("Please recieve/")
  fixture.type(" ")
  fixture.wait("Please receive? ")
  fixture.type("<BS>")
  fixture.wait("Please recieve/")
  fixture.type(" ")
  fixture.wait("Please recieve/ ")
  fixture.type("teh ")
  fixture.wait("Please recieve/ the ")

  begin("café/")
  fixture.type(" ")
  fixture.wait("café? ")
  fixture.type("<BS>")
  fixture.wait("café/")

  for _, text in ipairs({
    "1/2", "1/", "foo1/", "foo/bar", "foo/bar/", "@rust/skill", "@rust/",
    "https://example.com/", "/model", "/foo/", "foo//", "foo /",
    "`foo/`", "`foo/", "[link](foo/)", "foo_bar/", "file.lua/", "C:\\foo/",
    "    foo/", "```lua\nfoo/",
  }) do
    begin(text)
    fixture.type(" ")
    fixture.wait(text .. " ")
  end
  begin("foo/")
  fixture.type("<Tab>")
  fixture.wait("foo/\t")

  begin("")
  fixture.exec([[vim.api.nvim_paste('foo/ ',true,-1)]])
  fixture.wait("foo/ ")

  begin("Please recieve/")
  fixture.exec("text_hold=true")
  fixture.type(" ")
  fixture.wait("Please recieve? ")
  fixture.type("<BS>")
  fixture.wait("Please recieve/")
  fixture.exec([[local request=text_requests[#text_requests]
    request.callback({decision='replace',occurrence=request.occurrence.id,
      original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})]])
  fixture.wait("Please recieve/")
  assert(fixture.exec("return #text_errors") == 0)
end, debug.traceback)
fixture.close()
assert(ok, failure)
print("text_question_slash: delayed punctuation, rejection, spelling, and protected tokens passed")
vim.cmd("qa!")
  end },
  { name = "stale_reply", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').open()
local ok,failure=xpcall(function()
  fixture.exec('text_hold=true')
  fixture.type('iPlease recieve ')
  assert(vim.wait(1500,function() return fixture.exec('return #text_requests')==1 end,10))
  fixture.type('more')
  fixture.wait('Please recieve more')
  fixture.exec([[local request=text_requests[1]; request.callback({decision='replace',occurrence=request.occurrence.id,
    original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})]])
  fixture.wait('Please recieve more')
  fixture.type(' recieve ')
  assert(vim.wait(1500,function() return fixture.exec('return #text_requests')==2 end,10))
  fixture.type('<BS>')
  fixture.wait('Please recieve more recieve')
  fixture.exec([[local request=text_requests[2]; request.callback({decision='replace',occurrence=request.occurrence.id,
    original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})]])
  fixture.wait('Please recieve more recieve')
  fixture.type(' teh ')
  assert(vim.wait(1500,function() return fixture.exec('return #text_requests')==3 end,10))
  fixture.exec('require("forge.text").invalidate(vim.api.nvim_get_current_buf())')
  fixture.exec([[local request=text_requests[3]; request.callback({decision='replace',occurrence=request.occurrence.id,
    original='teh',replacement='the',profile='fixture',profile_revision='revision'})]])
  fixture.wait('Please recieve more recieve teh ')
  assert(fixture.exec('return #text_errors')==0)
end,debug.traceback)
fixture.close()
assert(ok,failure)
print('text stale reply: later typing, Backspace-before-response, and submission invalidation passed')
vim.cmd('qa!')
  end },
  { name = "sustained_input", run = function()
-- Sustained typing and long contexts against the real text host. Each word is typed only after
-- the previous correction applies, so the fixture advances on host replies rather than on a
-- typing cadence measured by the real clock.
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').native()
local word_count=120
local ok,failure=xpcall(function()
  fixture.type('i')
  fixture.exec('text_measurements={}')
  local function applied_count()
    return fixture.exec([[
      local count=0
      for _,measurement in ipairs(text_measurements) do
        if measurement.event=='text.correction' and measurement.fields.status=='applied' then count=count+1 end
      end
      return count
    ]])
  end
  for word=1,word_count do
    fixture.type('recieve ')
    assert(vim.wait(2000,function()return applied_count()>=word end,1),
      ('correction %d did not apply: %s'):format(word,vim.inspect(fixture.exec('return text_measurements'))))
  end
  local redraw_count=fixture.exec([[
    local count=0
    for _,measurement in ipairs(text_measurements) do
      if measurement.event=='text.correction.redraw' then count=count+1 end
    end
    return count
  ]])
  assert(redraw_count>0,'no applied correction reached redraw')
  -- Sentence-start capitalization applies to the first word only.
  local expected='Receive '..string.rep('receive ',word_count-1)
  assert(fixture.text()==expected,'sustained input left an uncorrected word: '..fixture.text():sub(1,200))
  fixture.type('<Esc>')
  for _,words in ipairs({10,100,1000,10000}) do
    fixture.exec([[
      local words=...
      local prefix=string.rep('previous ',words)
      vim.api.nvim_buf_set_lines(0,0,-1,false,{prefix..'Please recieve a package.'})
      vim.api.nvim_win_set_cursor(0,{1,#prefix+#'Please recieve'})
      text_measurements={}
    ]],{words})
    fixture.type('i ')
    assert(vim.wait(2000,function()return fixture.exec('return #text_measurements>=2')end,1),
      ('a %d-word context did not settle'):format(words))
    local status=fixture.exec([[
      for _,entry in ipairs(text_measurements) do
        if entry.event=='text.correction' then return entry.fields.status end
      end
    ]])
    assert(status=='applied',('a %d-word context ended with %s'):format(words,vim.inspect(status)))
    fixture.type('<Esc>')
  end
  assert(#fixture.exec('return text_errors')==0,vim.inspect(fixture.exec('return text_errors')))
end,debug.traceback)
fixture.close()
assert(ok,failure)
vim.cmd('qa!')
  end },
  { name = "wordlist", run = function()
local fixture=dofile('nvim/lua/forge/tests/support/text_fixture.lua').native()
local ok,failure=xpcall(function()
  assert(fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,'repo')]]),
    'default wordlist was not included in the profile')
  for _,term in ipairs({'coroutine','middleware','stdin','rpc','refactor','todo','opt','perf','off'}) do
    assert(fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,...)]],{term}),
      'programming term missing from the profile: '..term)
  end
  assert(not fixture.exec([[return vim.tbl_contains(text_initialize_wire.request.params.terminology,'then')]]),
    'programming dictionary unexpectedly includes a common English word')
  fixture.exec([[
    local cases={
      {'Please recieve the package.','recieve','receive'},
      {'The update should releive pressure.','releive','relieve'},
      {'Please receive the package.','receive','receive'},
      {'The update should relieve pressure.','relieve','relieve'},
      {'Read the response form the server.','form','form'},
      {'Fill out the form before submitting.','form','form'},
      {'The value is greater then zero.','then','then'},
      {'Save the file then exit.','then','then'},
      {'Do not loose your work.','loose','loose'},
      {'The cable is loose.','loose','loose'},
      {'This should definately fix the issue.','definately','definitely'},
      {'We need to seperate configuration from state.','seperate','separate'},
      {'The problem occured during startup.','occured','occurred'},
      {'Check the dependancies before building.','dependancies','dependencies'},
      {'The service recieved a request.','recieved','received'},
      {'Please recieve','recieve','receive'},
      {'The value is greater then','then','then'},
      {'Fill out the form','form','form'},
      {'Turn autocorrect off.','off','off'},
      {'Turn autocorrect off','off','off'},
      {'Turn autocorrect OFF','OFF','OFF'},
      {'I answer two questions.','two','two'},
      {'I answer two','two','two'},
      {'I answer TWO','TWO','TWO'},
      {'This is too much.','too','too'},
      {'Say i’ll','i’ll','I’ll'},
      {'Say don’t','don’t','don’t'},
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
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
