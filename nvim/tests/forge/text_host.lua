local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').native({cold_text='Cold text '})
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
      occurrence='wire-example',context='The value is greater then zero.',target={start=21,['end']=25},protected={}}}
    client.request(request,function(result,error) text_wire={request=request,result=result,error=error} end)
  ]])
  assert(vim.wait(2000,function()return fixture.exec('return text_wire~=nil')end,10))
  local wire=fixture.exec('return text_wire')
  assert(wire.result and wire.result.replacement=='than',vim.inspect(wire))
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
