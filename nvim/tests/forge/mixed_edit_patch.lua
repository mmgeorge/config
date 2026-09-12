vim.loader.enable(false)
local buffer = require("forge.buffer")
local editable = require("forge.editable")
assert(vim.fn.filereadable("nvim/rust/forge/target/review-interleave-fixture.json") == 1, "Run forge-review service test immutable_file_continuation_publishes_only_new_rows_after_an_edit first")
local fixture = vim.json.decode(table.concat(vim.fn.readfile("nvim/rust/forge/target/review-interleave-fixture.json"),"\n"))
local session
local function open()
  session=buffer.open(fixture.snapshot.document,{editable={delay=1000,max_delay=1000,send=function() return true end}})
  assert(buffer.apply_snapshot(session,fixture.snapshot).kind=="Applied")
  vim.bo[session.buffer].modifiable=true
  vim.api.nvim_buf_set_text(session.buffer,1,0,1,13,vim.split(fixture.edit.text,"\n",{plain=true}))
  local request=editable.take_pending(session.editable,"title")
  assert(request.sequence==fixture.edit.sequence)
end
local ok,failure=xpcall(function()
  for _,ack_first in ipairs({true,false}) do
    open()
    local attachment=session.editable.native
    local original=vim.api.nvim_buf_get_lines(session.buffer,0,-1,true)
    local result
    if ack_first then
      assert(buffer.acknowledge_edit(session,fixture.acknowledgement,fixture.acknowledgement.patch).kind=="Deferred")
      result=buffer.apply_patch(session,fixture.file_patch)
    else
      assert(buffer.apply_patch(session,fixture.file_patch).kind=="Deferred")
      result=buffer.acknowledge_edit(session,fixture.acknowledgement,fixture.acknowledgement.patch)
    end
    assert(result.kind=="Applied",vim.inspect(result))
    assert(session.editable.native==attachment,"native edit attachment changed")
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,1,3,true),{original[2],original[3]}),"local title changed")
    for _,patch in ipairs(fixture.continuation) do assert(buffer.apply_patch(session,patch).kind=="Applied") end
    local expected={}
    for _,block in ipairs(fixture.expected_snapshot.block) do vim.list_extend(expected,block.text) end
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,0,-1,true),expected),"mixed result differs from native snapshot")
    assert(session.revision==fixture.expected_snapshot.revision)
    local _,body_row=session.sequence:position("region:body")
    vim.bo[session.buffer].modifiable=true
    vim.api.nvim_buf_set_text(session.buffer,body_row,0,body_row,12,{"Edited after projection"})
    local body_edit=editable.take_pending(session.editable,"body")
    assert(body_edit and body_edit.base==0,"generated projection lost another native region anchor")
    buffer.close(session)
  end
  local original_fixture=fixture
  fixture=vim.deepcopy(fixture)
  for index=1,10000 do
    fixture.snapshot.block[#fixture.snapshot.block+1]={id="unrelated:"..index,text={"untouched"},metadata={target={},decoration={},editable_region={}}}
    fixture.expected_snapshot.block[#fixture.expected_snapshot.block+1]=fixture.snapshot.block[#fixture.snapshot.block]
  end
  local updates={fixture.file_patch,fixture.acknowledgement.patch}
  vim.list_extend(updates,fixture.continuation)
  for _,patch in ipairs(updates) do patch.base_rows=patch.base_rows+10000 patch.next_rows=patch.next_rows+10000 patch.base_blocks=patch.base_blocks+10000 patch.next_blocks=patch.next_blocks+10000 end
  open()
  local untouched=session.block["unrelated:5000"]
  local attachment=session.editable.native
  assert(buffer.acknowledge_edit(session,fixture.acknowledgement,fixture.acknowledgement.patch).kind=="Deferred")
  local get_lines=vim.api.nvim_buf_get_lines
  local broad_read=false
  vim.api.nvim_buf_get_lines=function(buf,start,finish,strict)
    if buf==session.buffer and (finish==-1 or finish-start>1024) then broad_read=true end
    return get_lines(buf,start,finish,strict)
  end
  local applied=buffer.apply_patch(session,fixture.file_patch)
  vim.api.nvim_buf_get_lines=get_lines
  assert(applied.kind=="Applied",vim.inspect(applied))
  assert(not broad_read,"mixed projection copied unrelated native text")
  assert(session.block["unrelated:5000"]==untouched and session.editable.native==attachment)
  buffer.close(session)
  fixture=original_fixture
  open()
  local attachment=session.editable.native
  vim.api.nvim_buf_set_text(session.buffer,2,20,2,20,{"","Latest typing"})
  local latest=vim.api.nvim_buf_get_lines(session.buffer,1,4,true)
  assert(buffer.acknowledge_edit(session,fixture.acknowledgement,fixture.acknowledgement.patch).kind=="Deferred")
  assert(buffer.apply_patch(session,fixture.file_patch).kind=="Deferred")
  for _,patch in ipairs(fixture.continuation) do assert(buffer.apply_patch(session,patch).kind=="Deferred") end
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,1,4,true),latest))
  local next_request=editable.take_pending(session.editable,"title")
  local final=fixture.expected_snapshot
  local row_count=0
  for _,block in ipairs(final.block) do row_count=row_count+#block.text end
  local metadata=vim.deepcopy(fixture.acknowledgement.patch.metadata_edit[1])
  metadata.row_count=3 metadata.metadata.editable_region[1].revision=2
  metadata.metadata.editable_region[1].range["end"]={row=2,column=#latest[3]}
  local patch={document=final.document,base=final.revision,next=final.revision+1,base_rows=row_count,next_rows=row_count+1,
    base_blocks=#final.block,next_blocks=#final.block,block_edit={},removed_block={},metadata_edit={metadata},
    text_edit={{start_row=1,removed_rows=2,text=latest}}}
  local result=buffer.acknowledge_edit(session,{document=final.document,region="title",sequence=next_request.sequence,revision=2},patch)
  assert(result.kind=="Applied",vim.inspect(result))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,1,4,true),latest),"newer typing changed")
  assert(session.editable.native==attachment,"newer typing recreated attachment")
  buffer.close(session)
  open()
  local text=vim.api.nvim_buf_get_lines(session.buffer,0,-1,true)
  local malformed=vim.deepcopy(fixture.file_patch)
  malformed.metadata_edit[1].row_count=999999
  assert(buffer.acknowledge_edit(session,fixture.acknowledgement,fixture.acknowledgement.patch).kind=="Deferred")
  result=buffer.apply_patch(session,malformed)
  assert(result.kind~="Applied")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,0,-1,true),text),"malformed patch changed physical text")
  buffer.close(session)
  for _,conflict in ipairs({"acknowledged_text","editable_overlap"}) do
    open()
    local physical=vim.api.nvim_buf_get_lines(session.buffer,0,-1,true)
    local acknowledgement=vim.deepcopy(fixture.acknowledgement.patch)
    local generated=vim.deepcopy(fixture.file_patch)
    if conflict=="acknowledged_text" then acknowledgement.text_edit[1].text[1]="Forged native text"
    else generated.text_edit[#generated.text_edit+1]={start_row=1,removed_rows=1,text={"Forged local replacement"}} end
    assert(buffer.acknowledge_edit(session,fixture.acknowledgement,acknowledgement).kind=="Deferred")
    local rejected=buffer.apply_patch(session,generated)
    assert(rejected.kind=="Desynchronized",vim.inspect(rejected))
    assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,0,-1,true),physical),"conflicting native patch changed local physical text")
    buffer.close(session)
  end
  buffer.close(session)
  open()
  local before=vim.api.nvim_buf_get_lines(session.buffer,0,-1,true)
  for revision=10,265 do
    local queued={document=session.document,base=revision,next=revision+1,base_rows=12,next_rows=12,base_blocks=11,next_blocks=11,
      block_edit={},text_edit={},metadata_edit={},removed_block={}}
    assert(buffer.apply_patch(session,queued).kind=="Deferred")
  end
  result=buffer.apply_patch(session,{document=session.document,base=266,next=267,base_rows=12,next_rows=12,base_blocks=11,next_blocks=11,block_edit={},text_edit={},metadata_edit={},removed_block={}})
  assert(result.kind=="Desynchronized" and result.diagnostic:find("admission exhausted",1,true))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,0,-1,true),before))
  buffer.close(session)
  open()
  before=vim.api.nvim_buf_get_lines(session.buffer,0,-1,true)
  result=buffer.apply_patch(session,{document=session.document,base=10,next=11,block_edit={},removed_block={},metadata_edit={},text_edit={{start_row=0,removed_rows=0,text={string.rep("x",16*1024*1024)}}}})
  assert(result.kind=="Desynchronized" and result.diagnostic:find("admission exhausted",1,true))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer,0,-1,true),before))
end,debug.traceback)
if session then buffer.close(session) end
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("mixed_edit_patch OK") vim.cmd("qa!")
