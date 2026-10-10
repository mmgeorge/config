vim.opt.runtimepath:prepend("nvim")
local buffer=require("forge.buffer")
local owner=buffer.open("collapsed-highlighting")
vim.api.nvim_set_current_buf(owner.buffer)
local metadata={target={},editable_region={},
 decoration={{range={start={row=0,column=0},["end"]={row=3,column=0}},capture="DiffAdd",priority=50}},
 gutter={{position={row=0,column=0},priority=100,chunk={{text="1 + ",capture="LineNr"}}}},
 fold={{id="declaration",start={row=0,column=0},["end"]={block="source",position={row=3,column=0}},
  closed=true,collapsed_suffix="...}"}}}
assert(buffer.apply_snapshot(owner,{document=owner.document,revision=0,block={{id="source",
 text={"pub enum Error {","  Invalid,","}"},metadata=metadata}}}).kind=="Applied")
assert(vim.api.nvim_get_current_line()=="pub enum Error {...}")
local entry=owner.block.source
assert(entry.row_count==1 and entry.metadata.decoration[1].range["end"].row==1)
assert(entry.metadata.gutter[1].chunk[1].text=="1 + ")
assert(buffer.set_expansion(owner,"declaration",true))
assert(vim.api.nvim_get_current_line()=="pub enum Error {")
assert(owner.row_count==3 and owner.projection.source.block.source.text[1]=="pub enum Error {")
buffer.close(owner)
print("collapsed declaration source and decoration: passed")
