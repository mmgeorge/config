local fixture=dofile('nvim/tests/forge/support/text_fixture.lua').open()
local success,failure=xpcall(function()
  fixture.exec([[
    reload_path=vim.fn.tempname()..'.md'
    vim.fn.writefile({'Original text.'},reload_path)
    vim.cmd.edit(reload_path)
    vim.bo.filetype='markdown'
    require('forge.text').attach(vim.api.nvim_get_current_buf())
    text_hold=true
  ]])
  fixture.type('APlease recieve ')
  assert(vim.wait(2000,function() return fixture.exec('return #text_requests>0') end,5))
  fixture.exec([[
    reload_request=text_requests[#text_requests]
    vim.fn.writefile({'Replacement text.'},reload_path)
    vim.cmd.edit({bang=true})
    reload_request.callback({decision='replace',occurrence=reload_request.occurrence.id,
      original='recieve',replacement='receive',profile='fixture',profile_revision='revision'})
    text_hold=false
  ]])
  fixture.wait('Replacement text.')
  fixture.type('<Esc>APlease recieve ')
  fixture.wait('Replacement text.Please receive ')
  fixture.type('<Esc>')
  fixture.exec([[
    reload_buffer=vim.api.nvim_get_current_buf()
    vim.api.nvim_set_current_buf(vim.api.nvim_create_buf(true,false))
    vim.api.nvim_buf_delete(reload_buffer,{unload=true,force=true})
    vim.cmd.buffer(reload_buffer)
    require('forge.text').attach(reload_buffer)
  ]])
  fixture.type('APlease recieve ')
  fixture.wait('Replacement text.Please receive ')
  assert(#fixture.exec('return text_errors')==0,vim.inspect(fixture.exec('return text_errors')))
end,debug.traceback)
pcall(fixture.exec,'vim.fn.delete(reload_path)')
fixture.close()
assert(success,failure)
print('text reload host: stale reply, correction after reload, and correction after unload passed')
vim.cmd('qa!')
