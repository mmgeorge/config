local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local temp = dofile("nvim/lua/forge/tests/support/temp.lua")
local text_fixture = dofile("nvim/lua/forge/tests/support/text_fixture.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["harper_rules"] = function()
  local root = temp.directory("harper-rules")
  vim.fn.mkdir(root .. "/first", "p")
  vim.fn.mkdir(root .. "/second", "p")
  local path = root .. "/rules.json"
  vim.fn.writefile({ "{}" }, path)
  local fixture = text_fixture.open()
  MiniTest.finally(function()
    pcall(fixture.exec, [[for _, client in ipairs(vim.lsp.get_clients({name="harper_ls"})) do client:stop(true) end]])
    pcall(fixture.close)
  end)
  fixture.exec([[
    local root, path = ...
    dofile("nvim/lua/forge/tests/support/harper_server.lua").install()
    local previous = vim.fn.stdpath
    vim.fn.stdpath = function(kind) return kind == "cache" and root or previous(kind) end
    require("forge").setup({ text_correction = { enabled = false }, harper = {
      enabled = true, cmd = { vim.v.progpath }, disabled_rules = path,
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
  ]], { root, path })
  wait_until(function()
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
  end, "Harper did not publish rule identifiers in both projects", { timeout_ms = 4000 })
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
  wait_until(function() return fixture.exec("return rule_actions ~= nil") end,
    "Harper did not answer the rule code action request", { timeout_ms = 3000 })
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
  wait_until(function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
        if client.settings["harper-ls"].linters.RepeatedWords ~= false then return false end
        local other_rule = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then return false end
          if diagnostic.code == "SpellCheck" then other_rule = true end
        end
        if not other_rule then return false end
      end
      return true
    ]])
  end, "Disabling a rule did not refresh both clients while preserving other rules", { timeout_ms = 4000,
    context = function() return vim.inspect(fixture.exec([[
      local result = { errors = text_errors, buffers = {} }
      for _, buffer in ipairs(rule_buffers) do
        local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
        result.buffers[#result.buffers + 1] = {
          diagnostics = vim.diagnostic.get(buffer), linters = client and client.settings["harper-ls"].linters,
        }
      end
      return result
    ]])) end })
  assert(vim.json.decode(table.concat(vim.fn.readfile(path), "\n")).RepeatedWords == false)
  fixture.exec([[
    local buffer = rule_buffers[1]
    local harper = require("forge.text.harper")
    local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
    harper.detach(buffer)
    client:stop()
    harper.attach(buffer, { kind = "markdown", root = ... })
  ]], { root .. "/first" })
  wait_until(function()
    return fixture.exec([[
      local client = vim.lsp.get_clients({ bufnr = rule_buffers[1], name = "harper_ls" })[1]
      return client and client.initialized and client.settings["harper-ls"].linters.RepeatedWords == false
    ]])
  end, "A new Harper client did not load persisted rules", { timeout_ms = 4000 })
  fixture.exec([[
    local path = ...
    vim.fn.writefile({ "{}" }, path)
    vim.api.nvim_exec_autocmds("BufWritePost", { pattern = path })
  ]], { path })
  wait_until(function()
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
  end, "Removing a disabled rule did not restore diagnostics", { timeout_ms = 4000 })
  assert(#fixture.exec("return text_errors") == 0, vim.inspect(fixture.exec("return text_errors")))
end

T["harper_scopes"] = function()
  local fixture=text_fixture.open()
  MiniTest.finally(fixture.close)
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
    wait_until(function()
      return fixture.exec([[for _,diagnostic in ipairs(vim.diagnostic.get(0))do
        if diagnostic.code=='RepeatedWords' and diagnostic.lnum==... then return true end
      end return false]],{row})
    end,'Harper did not report the eligible row '..row,{timeout_ms=4000})
    local diagnostics=fixture.exec('return vim.diagnostic.get(0)')
    for _,diagnostic in ipairs(diagnostics) do assert(diagnostic.lnum==row,vim.inspect(diagnostics)) end
  end
  fixture.exec([[
    dofile('nvim/lua/forge/tests/support/harper_server.lua').install()
    require('forge').setup({text_correction={enabled=false},harper={enabled=true,cmd={vim.v.progpath}}})
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
  wait_until(function()return fixture.exec('return harper_readonly_actions~=nil')end,
    'Harper did not answer the read-only code action request')
  assert(#fixture.exec('return harper_readonly_actions')==0,'read-only content exposed a code action')
  fixture.exec([[
    local client=assert(vim.lsp.get_clients({bufnr=0,name='harper_ls'})[1])
    client:request('textDocument/codeAction',{textDocument=vim.lsp.util.make_text_document_params(),
      range={start={line=1,character=8},['end']={line=1,character=15}},context={diagnostics={}}},
      function(error,result)harper_stale_actions=result end,0)
  ]])
  wait_until(function()return fixture.exec('return harper_stale_actions~=nil')end,
    'Harper did not answer the stale code action request',{timeout_ms=3000})
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
end

T["async_lifecycle"] = function()
  local fixture=text_fixture.open()
  MiniTest.finally(fixture.close)
  fixture.exec('text_hold=true')
  fixture.type('iPlease recieve ')
  wait_until(function()return fixture.exec('return #text_requests')==1 end,'correction request 1 was not sent')
  fixture.type('teh ')
  wait_until(function()return fixture.exec('return #text_requests')==2 end,'correction request 2 was not sent')
  fixture.exec([[
    local request=text_requests[2]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='teh',replacement='the',profile='fixture',profile_revision='revision'})
    request=text_requests[1]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})
  ]])
  fixture.wait('Please recieve the ')
  fixture.type('recieve ')
  wait_until(function()return fixture.exec('return #text_requests')==3 end,'correction request 3 was not sent')
  fixture.exec([[
    local client=require('forge.client')
    local generation=client.host_get_generation()+1
    client.host_get_generation=function()return generation end
    local request=text_requests[3]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})
  ]])
  fixture.wait('Please recieve the recieve ')
  fixture.type('teh ')
  wait_until(function()return fixture.exec('return #text_requests')==4 end,'correction request 4 was not sent')
  fixture.exec([[
    local request=text_requests[4]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='teh',replacement='the',profile='fixture',profile_revision='obsolete'})
  ]])
  fixture.wait('Please recieve the recieve teh ')
  fixture.type('<BS>')
  fixture.wait('Please recieve the recieve teh')
  fixture.type(' ')
  wait_until(function()return fixture.exec('return #text_requests')==5 end,'correction request 5 was not sent')
  fixture.exec([[
    local request=text_requests[5]
    request.callback({decision='replace',occurrence=request.occurrence.id,original='teh',replacement='the',profile='other-project',profile_revision='revision'})
  ]])
  fixture.wait('Please recieve the recieve teh ')
  assert(fixture.exec('return #text_errors')==0)
end

T["contractions"] = function()
  local fixture=text_fixture.native()
  MiniTest.finally(fixture.close)
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
      wait_until(function() return fixture.text()~=before end,'typed character did not reach the buffer',{timeout_ms=2500})
      stages[#stages+1]={typed=character,text=fixture.text()}
    end
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
  local errors=fixture.exec('return text_errors')
  assert(#errors==0,vim.inspect(errors))
end

T["eligibility"] = function()
  local fixture=text_fixture.open()
  MiniTest.finally(fixture.close)
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
end

T["question_slash"] = function()
  local fixture = text_fixture.open()
  MiniTest.finally(fixture.close)

  ---@param text string
  local function begin(text)
    fixture.type("<Esc>")
    fixture.exec([[vim.api.nvim_buf_set_lines(0,0,-1,false,{''});vim.api.nvim_win_set_cursor(0,{1,0})]])
    fixture.type("i" .. text)
    fixture.wait(text)
  end

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
end

T["stale_reply"] = function()
  local fixture=text_fixture.open()
  MiniTest.finally(fixture.close)
  fixture.exec('text_hold=true')
  fixture.type('iPlease recieve ')
  wait_until(function() return fixture.exec('return #text_requests')==1 end,'correction request 1 was not sent',{timeout_ms=1500})
  fixture.type('more')
  fixture.wait('Please recieve more')
  fixture.exec([[local request=text_requests[1]; request.callback({decision='replace',occurrence=request.occurrence.id,
    original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})]])
  fixture.wait('Please recieve more')
  fixture.type(' recieve ')
  wait_until(function() return fixture.exec('return #text_requests')==2 end,'correction request 2 was not sent',{timeout_ms=1500})
  fixture.type('<BS>')
  fixture.wait('Please recieve more recieve')
  fixture.exec([[local request=text_requests[2]; request.callback({decision='replace',occurrence=request.occurrence.id,
    original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})]])
  fixture.wait('Please recieve more recieve')
  fixture.type(' teh ')
  wait_until(function() return fixture.exec('return #text_requests')==3 end,'correction request 3 was not sent',{timeout_ms=1500})
  fixture.exec('require("forge.text").invalidate(vim.api.nvim_get_current_buf())')
  fixture.exec([[local request=text_requests[3]; request.callback({decision='replace',occurrence=request.occurrence.id,
    original='teh',replacement='the',profile='fixture',profile_revision='revision'})]])
  fixture.wait('Please recieve more recieve teh ')
  assert(fixture.exec('return #text_errors')==0)
end

T["sustained_input"] = function()
  -- Sustained typing and long contexts against the real text host. Each word is typed only after
  -- the previous correction applies, so the fixture advances on host replies rather than on a
  -- typing cadence measured by the real clock.
  local fixture=text_fixture.native()
  MiniTest.finally(fixture.close)
  local word_count=120
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
    wait_until(function()return applied_count()>=word end,('correction %d did not apply'):format(word),
      {timeout_ms=2000,context=function() return vim.inspect(fixture.exec('return text_measurements')) end})
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
    wait_until(function()return fixture.exec('return #text_measurements>=2')end,
      ('a %d-word context did not settle'):format(words),{timeout_ms=2000})
    local status=fixture.exec([[
      for _,entry in ipairs(text_measurements) do
        if entry.event=='text.correction' then return entry.fields.status end
      end
    ]])
    assert(status=='applied',('a %d-word context ended with %s'):format(words,vim.inspect(status)))
    fixture.type('<Esc>')
  end
  assert(#fixture.exec('return text_errors')==0,vim.inspect(fixture.exec('return text_errors')))
end

T["wordlist"] = function()
  local fixture=text_fixture.native()
  MiniTest.finally(fixture.close)
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
  wait_until(function()return fixture.exec('return text_dictionary_gate.remaining')==0 end,
    'dictionary gate corrections did not all answer',{timeout_ms=4000})
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
  wait_until(function()return fixture.exec('return text_wordlist_ready~=nil or text_wordlist_failure~=nil')end,
    'text service did not reinitialize with the explicit wordlist',{timeout_ms=2000})
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
end

return T
