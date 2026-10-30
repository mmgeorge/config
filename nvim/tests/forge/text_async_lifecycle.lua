local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').open()
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
