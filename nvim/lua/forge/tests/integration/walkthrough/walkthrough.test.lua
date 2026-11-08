local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
local host = dofile("nvim/lua/forge/tests/support/host.lua")
local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["walkthrough"] = function()
  local state
  -- Close the walkthrough and stop the host before the temporary directories registered below are deleted.
  MiniTest.finally(function()
    if state then pcall(require("forge.walkthrough").close, state) end
    pcall(host.stop)
  end)
  host.start()
  local walkthrough = require("forge.walkthrough")
  require("forge.infra.config").options.walkthrough_inventory = false
  local notices = {}
  local path = "source.lua"
  local before = { "local captured = 1", "return captured" }
  local after = { "local captured = 2", "return captured" }
  local repository = git_repository.create({ files = { [path] = before }, message = "base" })
  local fixture = repository.path
  vim.fn.writefile(after, fixture .. "/" .. path)
  local artifact = { version=12, root="Native Walkthrough", overview="Overview", flow={{text="Source lifecycle"}}, commit=vim.trim(repository.run({"rev-parse","HEAD"})), tasks={{title="Task complete heading",subtasks={{title="Subtask complete heading",changes={{action="Modify",kind="Function",target="captured",note="Native source resolution",file=path,line=2,annotation={title="Immutable annotation",comment="Read-only native Markdown"}}}}}}} }
  vim.fn.writefile({vim.json.encode(artifact)}, fixture .. "/.walkthrough.json")
  state = walkthrough.open({workspace=fixture,on_error=function(message) notices[#notices+1]=message end})
  wait_until(function() return state.replica.status=="Applied" and not state.pending and #state.queue==0 end,
    "Walkthrough did not settle", { timeout_ms = 4000, errors = notices })
  assert(#notices==0,table.concat(notices,"\n"))
  assert(vim.bo[state.replica.buffer].readonly and vim.wo.foldmethod=="manual")
  assert(require("forge.buffer").set_expansion(state.replica,"walkthrough:task:0:subtask:0",true))
  local row = buffer.find_row(state.replica.buffer, "Modify Function: captured")
  assert(row,"native change target missing: " .. vim.inspect(vim.api.nvim_buf_get_lines(state.replica.buffer,0,-1,true)))
  vim.api.nvim_win_set_cursor(0,{row,0})
  walkthrough.open_change(state)
  wait_until(function() return not state.opening and not state.pending and #state.queue==0
    and state.source[1] and state.source[1].annotation and state.source[1].source.replica.status=="Applied" end,
    "Walkthrough source did not settle", { timeout_ms = 4000, errors = notices })
  assert(#notices==0,table.concat(notices,"\n"))
  local owner = state.source[1]
  assert(owner.review and #vim.api.nvim_list_wins()==2)
  assert(vim.bo[owner.annotation.buffer].readonly)
  local review = vim.api.nvim_buf_get_lines(owner.annotation.buffer,0,-1,true)
  local old_row, new_row = before[1], after[1]
  assert(vim.tbl_contains(review,old_row) and vim.tbl_contains(review,new_row))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(owner.source.replica.buffer,0,-1,true),after))
  local source_row
  for index,line in ipairs(review) do if line==new_row then source_row=index break end end
  assert(source_row, "source row missing: " .. vim.inspect(review))
  vim.api.nvim_set_current_win(owner.window)
  vim.api.nvim_win_set_cursor(0,{source_row,0})
  keymap.invoke("o")
  wait_until(function() return vim.api.nvim_get_current_buf()==owner.source.replica.buffer end,
    "native source action did not settle", { timeout_ms = 4000, errors = notices })
  assert(#notices==0,table.concat(notices,"\n"))
  assert(vim.api.nvim_win_get_cursor(0)[1]==1)
  keymap.invoke("q")
  assert(vim.api.nvim_get_current_buf()==owner.annotation.buffer)
  assert(vim.trim(repository.run({"diff","--cached","--name-only"}))=="")
  walkthrough.close(state) state=nil
  assert(host.stop(), "host did not collect")
end

return T
