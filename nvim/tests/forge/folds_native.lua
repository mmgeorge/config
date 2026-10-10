vim.opt.runtimepath:prepend("nvim")
local buffer=require("forge.buffer")
local owner=buffer.open("adjacent-nodes",{preserve_view=true})
vim.api.nvim_set_current_buf(owner.buffer)
local function metadata(id,rows)
 return {target={},decoration={},editable_region={},fold={{id=id,start={row=0,column=0},
  ["end"]={block=id,position={row=rows,column=0}},closed=true}}}
end
local source={document=owner.document,revision=0,block={
 {id="first",text={"first","hidden first"},metadata=metadata("first",2)},
 {id="second",text={"second","hidden second"},metadata=metadata("second",2)}}}
assert(buffer.apply_snapshot(owner,source).kind=="Applied")
assert(owner.row_count==2)
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(owner.buffer,0,-1,true),{"first","second"}))
assert(buffer.set_expansion(owner,"second",true))
assert(owner.row_count==3)
local result=buffer.apply_patch(owner,{document=owner.document,base=0,next=1,base_rows=4,next_rows=5,base_blocks=2,next_blocks=2,
 text_edit={{start_row=2,removed_rows=2,text={"second","new content","more content"}}},block_edit={},removed_block={},
 metadata_edit={{block="second",row_count=3,metadata=metadata("second",3)}}})
assert(result.kind=="Applied",result.diagnostic)
assert(owner.row_count==4 and owner.projection.source.row_count==5)
assert(buffer.set_expansion(owner,"second",false))
assert(owner.row_count==2)
assert(buffer.set_expansion(owner,"second",true))
assert(vim.api.nvim_buf_get_lines(owner.buffer,3,4,true)[1]=="more content")
buffer.close(owner)
print("adjacent node projection and source patching: passed")
