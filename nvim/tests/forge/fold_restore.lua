vim.opt.runtimepath:prepend("nvim")
local buffer=require("forge.buffer")
local nodes=require("forge.nodes")
local input=require("forge.input")
local owner=buffer.open("shared-expansion",{preserve_view=true})
local function source(revision)
 return {document=owner.document,revision=revision,block={{id="source",text={"Parent","Child","body","Sibling","other"},
  metadata={target={},decoration={},editable_region={},fold={
   {id="parent",start={row=0,column=0},["end"]={block="source",position={row=5,column=0}},closed=false},
   {id="child",start={row=1,column=0},["end"]={block="source",position={row=3,column=0}},closed=false},
   {id="sibling",start={row=3,column=0},["end"]={block="source",position={row=5,column=0}},closed=true},
 }}}}}
end
assert(buffer.apply_snapshot(owner,source(0)).kind=="Applied")
vim.api.nvim_set_current_buf(owner.buffer)
local first=vim.api.nvim_get_current_win()
local view1=input.open(owner,first)
vim.cmd("vsplit")
local second=vim.api.nvim_get_current_win()
local view2=input.open(owner,second)
vim.api.nvim_win_set_cursor(first,{3,1})
vim.api.nvim_win_set_cursor(second,{4,2})
assert(buffer.set_expansion(owner,"parent",false))
assert(owner.row_count==1 and vim.api.nvim_win_get_cursor(first)[1]==1 and vim.api.nvim_win_get_cursor(second)[1]==1)
assert(buffer.set_expansion(owner,"parent",true))
assert(owner.row_count==4 and not nodes.closed(owner,"child") and nodes.closed(owner,"sibling"))
input.close(view2)
vim.api.nvim_win_close(second,true)
vim.cmd("enew")
vim.api.nvim_set_current_buf(owner.buffer)
nodes.attach(owner,first)
assert(buffer.apply_snapshot(owner,source(1)).kind=="Applied")
assert(owner.row_count==4 and nodes.closed(owner,"sibling"))
assert(not vim.wo[first].foldenable)
input.close(view1)
buffer.close(owner)
print("shared document expansion and return: passed")
