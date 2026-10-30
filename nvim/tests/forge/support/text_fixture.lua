local M = {}
function M.open()
  local child = vim.fn.jobstart({vim.v.progpath,"--embed","--headless","-u","NONE","-i","NONE","-n"},{rpc=true})
  assert(child>0)
  vim.rpcrequest(child,"nvim_ui_attach",100,32,{rgb=true})
  local fixture={child=child}
  function fixture.exec(code,arguments) return vim.rpcrequest(child,"nvim_exec_lua",code,arguments or {}) end
  function fixture.type(text) vim.rpcrequest(child,"nvim_input",text) end
  function fixture.text() return fixture.exec("return table.concat(vim.api.nvim_buf_get_lines(0,0,-1,false),'\\n')") end
  function fixture.wait(expected)
    assert(vim.wait(2500,function() return fixture.text()==expected end,10),vim.inspect({expected=expected,actual=fixture.text(),debug=fixture.exec("return {mode=vim.api.nvim_get_mode().mode,requests=#text_requests,errors=text_errors}")}))
  end
  function fixture.close() vim.fn.jobstop(child) end
  fixture.exec([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd()..'/nvim')
    text_requests={}
    text_errors={}
    vim.notify=function(message) text_errors[#text_errors+1]=tostring(message) end
    package.loaded['forge.text.service']={
      initialize=function(_,callback) callback({profile='fixture',profile_revision='revision'}) end,
      correct=function(_,occurrence,_,callback)
        occurrence.profile={profile='fixture',profile_revision='revision'}
        text_requests[#text_requests+1]={occurrence=occurrence,callback=callback}
        local replacement=({recieve='receive',teh='the',i='I'})[occurrence.context.original]
        if not text_hold then vim.schedule(function()
          callback({decision=replacement and 'replace' or 'unchanged',occurrence=occurrence.id,
            original=occurrence.context.original,replacement=replacement,profile='fixture',profile_revision='revision'})
        end) end
      end,
    }
    require('forge').setup({text_correction={enabled=true},harper={enabled=false}})
    vim.bo.filetype='markdown'
  ]])
  return fixture
end

function M.native(options)
  local fixture=M.open()
  fixture.exec([[
    require('forge.text').detach(vim.api.nvim_get_current_buf())
    package.loaded['forge.text.service']=nil
    package.loaded['forge.text.input']=nil
    package.loaded['forge.text']=nil
    local data=vim.fn.tempname()
    vim.fn.mkdir(data,'p')
    local executable='forge'..(vim.fn.has('win32')==1 and '.exe' or '')
    local binary=data..'/'..executable
    assert(vim.uv.fs_copyfile(vim.fn.getcwd()..'/nvim/rust/forge/target/debug/'..executable,binary))
    package.loaded['forge.builder']={ensure=function(callback)callback({ok=true,path=binary});return function()end end}
    local previous=vim.fn.stdpath
    vim.fn.stdpath=function(kind)
      return (kind=='data' or kind=='config' or kind=='cache') and data or previous(kind)
    end
    local client=require('forge.client')
    client._host_set_launcher_for_test(vim.system)
    local request=client.request
    client.request=function(specification,callback,progress)
      if specification.method=='text.initialize' then
        return request(specification,function(result,failure,detail)
          text_initialize_wire={request=specification,result=result,error=failure}
          callback(result,failure,detail)
        end,progress)
      end
      return request(specification,callback,progress)
    end
    require('forge').setup({text_correction={enabled=true,terminology={'tokio','Neovim','serde'}},harper={enabled=false},
      diff_logging=true,diff_log_path=data..'/text-perf.jsonl'})
    text_measurements={}
    local perf=require('forge.infra.perf')
    local event=perf.event
    perf.event=function(scope,name,fields)
      if name:match('^text%.') then text_measurements[#text_measurements+1]={event=name,fields=vim.deepcopy(fields)} end
      return event(scope,name,fields)
    end
    local startup=require('forge.startup_log')
    local write=startup.write
    startup.write=function(name,fields,...)
      if name=='text.correction.redraw' then text_measurements[#text_measurements+1]={event=name,fields=vim.deepcopy(fields)} end
      return write(name,fields,...)
    end
    require('forge.text').attach(vim.api.nvim_get_current_buf())
    require('forge.text.service').initialize({root=vim.fn.getcwd()},function(result,error)text_ready=result;text_failure=error end)
  ]])
  if options and options.cold_text then
    fixture.type('i'..options.cold_text)
    fixture.wait(options.cold_text)
    assert(fixture.exec('return text_ready==nil'),'cold input fixture missed initialization')
  end
  assert(vim.wait(15000,function()return fixture.exec('return text_ready~=nil or text_failure~=nil')end,10),
    'native model did not initialize')
  local failure=fixture.exec('return text_failure')
  assert(failure==nil or failure==vim.NIL,vim.inspect(failure))
  local close=fixture.close
  fixture.close=function()
    pcall(fixture.exec,[[require('forge.client').host_stop(nil,function()text_stopped=true end)]])
    vim.wait(5000,function()local ok,stopped=pcall(fixture.exec,'return text_stopped==true');return not ok or stopped end,10)
    close()
  end
  return fixture
end
return M
