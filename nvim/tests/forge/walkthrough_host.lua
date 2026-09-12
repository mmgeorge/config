vim.loader.enable(false)
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local root, fixture, data = vim.fn.getcwd(), vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(fixture, "p") vim.fn.mkdir(data, "p")
local name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local executable = data .. "/" .. name
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback) vim.schedule(function() callback({ok=true,path=executable}) end) return function() end end }
local client = require("forge.client") client._set_launcher_for_test(vim.system)
local walkthrough = require("forge.walkthrough")
require("forge.infra.config").options.walkthrough_inventory = false
local state, notices = nil, {}
local function git(arguments)
  local command = { "git", "-C", fixture } vim.list_extend(command, arguments)
  local result = vim.system(command, {text=true,timeout=10000}):wait()
  assert(result.code == 0, result.stderr) return vim.trim(result.stdout)
end
local ok, failure = xpcall(function()
  git({"init","--quiet"}) git({"config","user.name","Forge Fixture"}) git({"config","user.email","forge@example.test"})
  vim.fn.writefile({"local captured = 1", "return captured"}, fixture .. "/source.lua")
  git({"add","source.lua"}) git({"commit","--quiet","-m","base"})
  vim.fn.writefile({"local captured = 2", "return captured"}, fixture .. "/source.lua")
  local artifact = { version=12, root="Native Walkthrough", overview="Overview", flow={{text="Source lifecycle"}}, commit=git({"rev-parse","HEAD"}), tasks={{title="Task complete heading",subtasks={{title="Subtask complete heading",changes={{action="Modify",kind="Function",target="captured",note="Native source resolution",file="source.lua",line=2,annotation={title="Immutable annotation",comment="Read-only native Markdown"}}}}}}} }
  vim.fn.writefile({vim.json.encode(artifact)}, fixture .. "/.walkthrough.json")
  state = walkthrough.open({workspace=fixture,on_error=function(message) notices[#notices+1]=message end})
  assert(vim.wait(45000,function() return #notices>0 or state.replica.status=="Applied" and not state.pending and #state.queue==0 end,10), "Walkthrough did not settle")
  assert(#notices==0,table.concat(notices,"\n"))
  assert(vim.bo[state.replica.buffer].readonly and vim.wo.foldmethod=="expr")
  local row
  for index,line in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer,0,-1,true)) do if line:find("Modify Function: captured",1,true) then row=index break end end
  assert(row,"native change target missing")
  vim.cmd("normal! zR") vim.api.nvim_win_set_cursor(0,{row,0})
  walkthrough.open_change(state)
  assert(vim.wait(10000,function() return #notices>0 or not state.opening and not state.pending and #state.queue==0
    and state.source[1] and state.source[1].annotation and state.source[1].source.replica.status=="Applied" end,10),"Walkthrough source did not settle")
  assert(#notices==0,table.concat(notices,"\n"))
  local owner = state.source[1]
  assert(owner.review and #vim.api.nvim_list_wins()==2)
  assert(vim.bo[owner.annotation.buffer].readonly)
  local review = vim.api.nvim_buf_get_lines(owner.annotation.buffer,0,-1,true)
  assert(vim.tbl_contains(review,"local captured = 1") and vim.tbl_contains(review,"local captured = 2"))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(owner.source.replica.buffer,0,-1,true),{"local captured = 2","return captured"}))
  local source_row
  for index,line in ipairs(review) do if line=="return captured" then source_row=index break end end
  assert(source_row)
  vim.api.nvim_set_current_win(owner.window)
  vim.api.nvim_win_set_cursor(0,{source_row,0})
  vim.fn.maparg("o","n",false,true).callback()
  assert(vim.wait(5000,function() return #notices>0 or vim.api.nvim_get_current_buf()==owner.source.replica.buffer end,10),"native source action did not settle")
  assert(#notices==0,table.concat(notices,"\n"))
  assert(vim.api.nvim_win_get_cursor(0)[1]==2)
  vim.fn.maparg("q","n",false,true).callback()
  assert(vim.api.nvim_get_current_buf()==owner.annotation.buffer)
  assert(git({"diff","--cached","--name-only"})=="")
  walkthrough.close(state) state=nil
end,debug.traceback)
if state then walkthrough.close(state) end
client.stop()
local collected=vim.wait(5000,function() return client._client.process==nil end,10)
vim.fn.stdpath=stdpath
vim.fn.delete(fixture,"rf") vim.fn.delete(data,"rf")
if not ok or not collected then vim.api.nvim_err_writeln(failure or "host did not collect") vim.cmd("cquit 1") end
print("walkthrough_host OK") vim.cmd("qa!")
