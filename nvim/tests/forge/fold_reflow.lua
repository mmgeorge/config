vim.opt.runtimepath:prepend("nvim")
local buffer=require("forge.buffer")
local nodes=require("forge.nodes")
local owner=buffer.open("projection-reflow",{preserve_view=true})
vim.api.nvim_set_current_buf(owner.buffer)
local function snapshot(revision,prefix)
 local blocks={}
 if prefix then blocks[#blocks+1]={id="prefix",text={"new","context"},metadata={target={},decoration={},editable_region={}}} end
 blocks[#blocks+1]={id="source",text={"Outer","Inner","content","tail"},metadata={target={},decoration={},editable_region={},fold={
  {id="outer",start={row=0,column=0},["end"]={block="source",position={row=4,column=0}},closed=false},
  {id="inner",start={row=1,column=0},["end"]={block="source",position={row=3,column=0}},closed=true},
 }}}
 return {document=owner.document,revision=revision,block=blocks}
end
assert(buffer.apply_snapshot(owner,snapshot(0,false)).kind=="Applied")
vim.api.nvim_win_set_cursor(0,{2,2})
assert(buffer.apply_snapshot(owner,snapshot(1,true)).kind=="Applied")
assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0),{4,2}))
assert(nodes.closed(owner,"inner"))
assert(buffer.set_expansion(owner,"inner",true))
assert(vim.api.nvim_get_current_line()=="Inner")
vim.api.nvim_win_set_cursor(0,{5,1})
assert(buffer.set_expansion(owner,"inner",false))
assert(vim.api.nvim_win_get_cursor(0)[1]==4)
assert(buffer.apply_snapshot(owner,snapshot(2,false)).kind=="Applied")
assert(vim.api.nvim_win_get_cursor(0)[1]==2)
buffer.close(owner)
print("projection reflow preserves source identity: passed")
