vim.opt.runtimepath:prepend(vim.fn.getcwd()..'/nvim')
local published={}
local captured_config
package.loaded['forge.infra.config']={options={harper={enabled=true,cmd={vim.v.progpath},settings={}}}}
vim.lsp.start=function(config) captured_config=config; return 12345 end
vim.lsp.buf_detach_client=function() return true end
vim.lsp.diagnostic.on_publish_diagnostics=function(_,result) published[#published+1]=result end
-- The mock transport does not need a cache directory or a language server process.
vim.fn.mkdir=function() return 1 end
local buffer=vim.api.nvim_create_buf(false,true)
vim.api.nvim_set_current_buf(buffer)
vim.api.nvim_buf_set_name(buffer,'ForgeTest://diagnostics')
vim.bo.filetype='ForgeHarnessInput'
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{'First line.','The the sentence.'})
local harper=require('forge.text.harper')
harper.attach(buffer,{kind='prompt',root=vim.fn.getcwd()})
local function publish(version,first,last)
  captured_config.handlers['textDocument/publishDiagnostics'](nil,
    {uri=vim.uri_from_bufnr(buffer),version=version,diagnostics={{message='Repeated word',
      range={start=first,['end']=last}}}},{client_id=12345})
end
vim.lsp.util.buf_versions[buffer]=10
publish(10,{line=1,character=0},{line=1,character=7})
assert(#published==1 and #published[1].diagnostics==1)
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{})
vim.lsp.util.buf_versions[buffer]=11
publish(nil,{line=1,character=0},{line=1,character=7})
assert(#published==2 and #published[2].diagnostics==0,'deleted row was admitted')
vim.api.nvim_buf_set_lines(buffer,0,-1,false,{'The new sentence.'})
vim.lsp.util.buf_versions[buffer]=12
publish(10,{line=0,character=0},{line=0,character=7})
assert(#published==2,'old version was published against new text')
publish(12,{line=0,character=0},{line=0,character=99})
assert(#published==3 and #published[3].diagnostics==0,'invalid column was admitted')
publish(12,{line=0,character=0},{line=0,character=7})
assert(#published==4 and #published[4].diagnostics==1,'current diagnostic was lost')
harper.detach(buffer)
publish(12,{line=0,character=0},{line=0,character=7})
assert(#published==4,'detached consumer accepted diagnostics')
print('Harper diagnostics: deleted rows, stale versions, invalid columns, current results, and detach passed')
vim.cmd('qa!')
