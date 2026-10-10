vim.opt.runtimepath:prepend("nvim")
local buffer=require("forge.buffer")
local owner=buffer.open("nested-projection",{preserve_view=true})
vim.api.nvim_set_current_buf(owner.buffer)
local function snapshot(revision,count)
 local text={"Parent","Child"}
 for index=1,count do text[#text+1]="line "..index end
 return {document=owner.document,revision=revision,block={{id="tree",text=text,
 metadata={target={},decoration={},editable_region={},fold={
  {id="parent",start={row=0,column=0},["end"]={block="tree",position={row=#text,column=0}},closed=false},
  {id="child",start={row=1,column=0},["end"]={block="tree",position={row=#text,column=0}},closed=true},
 }}}}}
end
assert(buffer.apply_snapshot(owner,snapshot(0,3)).kind=="Applied")
for index=1,20 do
 assert(buffer.apply_snapshot(owner,snapshot(index,index)).kind=="Applied")
 assert(owner.row_count==2)
 assert(buffer.set_expansion(owner,"child",true))
 assert(owner.row_count==index+2)
 assert(buffer.set_expansion(owner,"parent",false))
 assert(owner.row_count==1)
 assert(buffer.set_expansion(owner,"parent",true))
 assert(owner.row_count==index+2)
 assert(buffer.set_expansion(owner,"child",false))
end
buffer.close(owner)
print("nested subtree growth and collapse: passed")
