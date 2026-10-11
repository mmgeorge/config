vim.loader.enable(false)
local repository = vim.fn.fnamemodify(debug.getinfo(1,"S").source:sub(2),":h:h:h:h")
vim.opt.runtimepath:prepend(repository .. "/nvim")

local backend = vim.g.forge_agent_test_backend or "mock"
local function log(message)
  if not vim.g.forge_agent_test_visual then print(message) end
end
local child_backend = vim.g.forge_agent_test_child_backend or backend
local data = vim.g.forge_agent_test_data or vim.fn.tempname()
local workspace = vim.fn.tempname()
vim.fn.mkdir(data,"p")
vim.fn.mkdir(workspace,"p")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local binary = vim.g.forge_agent_test_binary or require("forge.builder").binary_path()
local private_binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(binary,private_binary))
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ok=true,path=private_binary}) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
vim.notify = function(message,level)
  if level == vim.log.levels.ERROR or level == vim.log.levels.WARN then
    errors[#errors+1] = tostring(message)
    log("AGENT_DIAGNOSTIC " .. tostring(message))
  end
end
local resolved_approval = {}
local function approvals()
  for _,approval in ipairs(state.approval or {}) do
    if not resolved_approval[approval.id] then
      resolved_approval[approval.id] = true
      log("AGENT_APPROVAL " .. approval.id)
      local answer = {}
      for _,item in ipairs(approval.item_list or {}) do
        answer[#answer+1] = {item_id=item.id,choice_id="allow_once"}
      end
      client.request("approval.resolve",{approval_id=approval.id,answer_list=answer},function(_,failure)
        if failure then errors[#errors+1] = "Approval resolution failed: " .. tostring(failure) end
      end)
    end
  end
end
local function request(method,params)
  if method ~= "state.get" then log("AGENT_REQUEST " .. backend .. " " .. method) end
  local settled,value,failure
  client.request(method,params or {},function(result,error) value,failure,settled=result,error,true end)
  assert(vim.wait(90000,function() approvals() return settled end,10),method .. " timeout: " .. vim.inspect(errors))
  assert(not failure,method .. ": " .. tostring(failure))
  return value
end
local function model_for(provider,pattern)
  if not pattern then return nil end
  local models = request("agent.models",{provider=provider})
  for _,model in ipairs(models) do
    if pattern == "default" and model.is_default then return model.id end
    if model.id:lower():find(pattern:lower(),1,true) then return model.id end
  end
  error("No advertised " .. provider .. " model matches " .. pattern .. ": " .. vim.inspect(vim.tbl_map(function(model) return model.id end,models)))
end
local function child_result(id,count)
  assert(vim.wait(90000,function()
    approvals()
    local child = state.agent and state.agent.summary and state.agent.summary[id]
    return child and child.exchange_count == count and (child.state == "complete" or child.state == "failed" or child.state == "cancelled")
  end,100),"child completion timeout: " .. id)
  local snapshot = request("state.get")
  local child = snapshot.agent.summary[id]
  assert(child.state == "complete",vim.inspect(child))
  return snapshot,child
end

local succeeded,failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  local launch = vim.g.forge_agent_test_launch or {}
  if backend == "mock" and not launch.mock then launch.mock = {command={"mock"},selectable=true} end
  require("forge").setup({harness={config_dir=data,backend=backend,backends=launch}})
  require("forge.views.harness").open()
  assert(vim.wait(30000,function() return state.session or #errors>0 end,10),"startup timeout")
  assert(state.session,table.concat(errors,"\n"))
  request("trace.configure",{enabled=true})
  request("session.configure",{access={sandbox=false,write_access="workspace",writable_directory={},windows_sandbox="elevated"}})
  errors = {}
  local root_model = model_for(backend,vim.g.forge_agent_test_model)
  if root_model then request("session.configure",{model=root_model}) end
  local initial = request("state.get")
  assert(initial.subagent_tools == false,"delegation must default off")
  if vim.g.forge_agent_test_settings then
    local picker = require("forge.views.picker")
    local function config_picker()
      require("forge.views.harness.settings").open(state,{
        window_list={state.transcript_win,state.composer_win},control_win=state.composer_win,
      })
      assert(vim.wait(2000,function() return picker.is_open("harness-config") end,10),"config picker did not open")
      local instance = picker._state_for_test()
      for _,option in ipairs(instance.spec.page_list[1].option_list) do
        if option.id == "subagent-tools" then return instance,option end
      end
      error("config picker omitted subagent tools")
    end
    local instance,option = config_picker()
    instance.spec.action_list[2].callback({option=option})
    assert(vim.wait(2000,function() return state.subagent_tools == true end,10),
      "config did not show On after enable; backend value: " .. vim.inspect(request("state.get").subagent_tools))
    assert(table.concat(vim.api.nvim_buf_get_lines(instance.buf,0,-1,false),"\n"):find("On",1,true),"picker did not render On")
    picker.close()
    instance,option = config_picker()
    assert(option.columns[2]:find("On",1,true),"reopening config lost enabled state")
    instance.spec.on_confirm({option=option})
    assert(vim.wait(2000,function() return state.subagent_tools == false end,10),"Enter did not disable subagent tools")
    assert(request("state.get").subagent_tools == false,"disabling did not persist")
    picker.close()
    log("HARNESS_AGENTS_SETTINGS_PASS " .. backend)
  end
  local marker = "CHILD_FRESH_D529"
  local child_model = model_for(child_backend,vim.g.forge_agent_test_child_model)
  if vim.g.forge_agent_test_manual ~= false then
  local admitted = request("agent.start",{task_name="fresh-child",message="Reply exactly " .. marker .. ". Do not use tools or access files.",
    provider=child_backend,model=child_model})
  local id = assert(admitted.agent.id)
  require("forge.views.harness.controller").open_agent_picker()
  local snapshot,child = child_result(id,1)
  require("forge.views.harness.controller").select_agent(id)
  assert(vim.wait(10000,function()
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf,0,-1,false),"\n"):find(marker,1,true)
  end,10),"child timeline omitted marker")
  assert(snapshot.session.backend_session_id == vim.NIL or snapshot.session.backend_session_id == nil,"manual children changed root provider history")
  require("forge.views.harness.agent_picker").refresh()
  require("forge.views.harness.agent_picker").close()
  request("agent.message",{target=id,message="Remember the token INBOX_P492 for your next task."})
  local idle = request("state.get")
  assert(idle.agent.summary[id].exchange_count == 1,"informational message started an idle child")
  request("agent.followup",{target=id,message="Reply with the remembered INBOX token only. Do not use tools."})
  snapshot,child = child_result(id,2)
  assert(vim.wait(10000,function()
    return table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf,0,-1,false),"\n"):find("INBOX_P492",1,true)
  end,10),"follow-up lost the retained inbox")
  log("HARNESS_AGENTS_MANUAL_PASS " .. backend .. " -> " .. child_backend .. " " .. id)
  end
  log("HARNESS_AGENTS_DATA " .. data)
  if vim.g.forge_agent_test_model_tools then
    require("forge.views.harness.controller").select_agent("main")
    request("session.configure",{subagent_tools=true})
    local configured = request("state.get")
    assert(configured.subagent_tools == true)
    assert(configured.session.access.sandbox == false,"delegation toggle reset sandbox policy: " .. vim.inspect(configured.session))
    require("forge.views.harness.controller").open_agent_picker()
    request("prompt.submit",{text="Use harness_spawn_agent to create a child named model-child on " .. child_backend
      .. (child_model and (" using model " .. child_model) or "")
      .. " with the task: Reply exactly MODEL_CHILD_R783. Do not use tools. Then use harness_wait_agent to collect its result."
      .. " After receiving the result, reply ROOT_SYNTHESIS_R783. Do not spawn native agents. Do not use files or shell commands."})
    local model_snapshot = request("state.get")
    local found
    for _,agent in ipairs(model_snapshot.agent.run or {}) do
      if agent.task_name == "model-child" then found=agent end
    end
    local root_transcript = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf,0,-1,false),"\n")
    assert(found,"root did not admit a managed model child: " .. vim.inspect(model_snapshot.agent.run) .. "\n" .. root_transcript)
    assert(table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf,0,-1,false),"\n"):find("ROOT_SYNTHESIS_R783",1,true),"root omitted child result synthesis")
    require("forge.views.harness.agent_picker").close()
    log("HARNESS_AGENTS_MODEL_PASS " .. backend .. " -> " .. child_backend)
  end
  if vim.g.forge_agent_test_controls then
    local target = {}
    local function spawn_control(name)
      local result = request("agent.start",{task_name=name,provider=child_backend,model=child_model,
        message="Use a shell command to wait 20 seconds, then reply CONTROL_FINISHED. This is a bounded interruption test. Do not edit files or spawn agents."})
      target[#target+1] = result.agent.id
      return result.agent.id
    end
    local function await_state(predicate)
      assert(vim.wait(45000,function()
        approvals()
        return predicate(state)
      end,50),"control settlement timeout: " .. vim.inspect(state.agent))
      return request("state.get")
    end
    local first = spawn_control("control-first")
    local sibling = spawn_control("control-sibling")
    await_state(function(snapshot) return snapshot.agent.summary[first].state == "running"
      and snapshot.agent.summary[sibling].state == "running" end)
    request("session.configure",{subagent_tools=false})
    local retained = request("state.get")
    assert(retained.subagent_tools == false)
    assert(retained.agent.summary[first].state == "running" and retained.agent.summary[sibling].state == "running",
      "tool visibility change interrupted admitted children")
    request("agent.interrupt",{target=first})
    local interrupted = await_state(function(snapshot) return snapshot.agent.summary[first].state == "cancelled" end)
    assert(interrupted.agent.summary[sibling].state == "running","individual interruption cancelled its sibling")
    local additional = spawn_control("control-additional")
    await_state(function(snapshot) return snapshot.agent.summary[sibling].state == "running"
      and snapshot.agent.summary[additional].state == "running" end)
    request("agent.interrupt_all")
    local settled = await_state(function(snapshot) return snapshot.agent.summary[sibling].state == "cancelled"
      and snapshot.agent.summary[additional].state == "cancelled" end)
    assert(settled.agent.summary[first].exchange_count == 1,"interruption restarted queued work")
    log("HARNESS_AGENTS_CONTROLS_PASS " .. backend .. " -> " .. child_backend)
    local session_id = settled.session.id
    local stopped = false
    client.stop(nil,function() stopped=true end)
    assert(vim.wait(10000,function() return stopped end,10),"recovery shutdown timeout")
    local reopened, reopen_failure
    client.start_harness(function(snapshot,failure) reopened,reopen_failure=snapshot,failure end,{session_id=session_id})
    assert(vim.wait(30000,function() return reopened or reopen_failure end,10),"recovery startup timeout")
    assert(not reopen_failure,tostring(reopen_failure))
    assert(reopened.session.id == session_id)
    assert(reopened.subagent_tools == false,"tool selection did not persist")
    for _,id in ipairs(target) do
      assert(reopened.agent.summary[id].state == "cancelled",vim.inspect(reopened.agent.summary[id]))
      assert(reopened.agent.summary[id].exchange_count == 1,"recovery replayed a child task")
    end
    log("HARNESS_AGENTS_RECOVERY_PASS " .. backend .. " -> " .. child_backend)
  end
end,debug.traceback)
if succeeded and vim.g.forge_agent_test_visual then
  vim.g.forge_agent_test_result = "pass"
  require("forge.views.harness.controller").select_agent("main")
  require("forge.views.harness.controller").open_agent_picker()
  vim.defer_fn(function() client.stop(nil,function() vim.cmd("qa!") end) end,90000)
  return
end
local stopped=false
client.stop(nil,function() stopped=true end)
vim.wait(10000,function() return stopped end,10)
vim.fn.stdpath=original_stdpath
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
vim.cmd("qa!")
