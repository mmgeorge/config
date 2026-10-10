vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local notices, recovered = {}, 0
local owner = buffer.open("projection-failure", {notice=function(value) notices[#notices+1]=value end,
  recover=function() recovered=recovered+1 end})
vim.api.nvim_set_current_buf(owner.buffer)
local source = { document=owner.document, revision=0, block={
  {id="body",text={"heading","body","tail"},metadata={target={},decoration={},editable_region={},fold={
    {id="section",start={row=0,column=0},["end"]={block="body",position={row=3,column=0}},closed=false}}}},
}}
assert(buffer.apply_snapshot(owner,source).kind=="Applied")
local native = vim.api.nvim_buf_set_lines
local injected = false
vim.api.nvim_buf_set_lines = function(...)
  injected=true
  error("injected projection write failure")
end
assert(not buffer.set_expansion(owner,"section",false))
vim.api.nvim_buf_set_lines=native
assert(injected and owner.status=="Desynchronized")
assert(not owner.applying and not vim.bo[owner.buffer].modifiable)
assert(#notices==1 and recovered==1, "failure must report once and admit recovery")
assert(owner.projection.choice.section==nil, "failed publication retained expansion intent")
assert(buffer.apply_snapshot(owner,source).kind=="Applied")
assert(owner.row_count==3)
assert(buffer.set_expansion(owner,"section",false) and owner.row_count==1)
assert(buffer.set_expansion(owner,"section",true) and owner.row_count==3)
buffer.close(owner)
print("projection failure recovery: passed")
