local M = {}
local editable = require("forge.editable")
local spans = require("forge.patch_spans")
local MAX_PATCHES, MAX_BYTES = 256, 16 * 1024 * 1024

local function local_owner(session, patch, acknowledgement)
  assert(#patch.block_edit == 0 and #patch.removed_block == 0, "local patch changes block identity")
  local owner = assert(session.region_owner[acknowledgement.region], "local region has no owner")
  assert(#patch.metadata_edit == 1 and patch.metadata_edit[1].block == owner, "local patch changes another block")
  local matched = false
  for _, region in ipairs(patch.metadata_edit[1].metadata.editable_region) do
    if region.id == acknowledgement.region then assert(region.revision == acknowledgement.revision, "local patch region revision differs") matched = true end
  end
  assert(matched, "local patch omitted acknowledged region")
  return owner
end

local function physical_text(session, id)
  local anchor = session.editable.native.anchor[id]
  if anchor.finish.row == vim.api.nvim_buf_line_count(session.buffer) then
    assert(anchor.finish.column == 0, "invalid local end anchor")
    local rows = vim.api.nvim_buf_get_lines(session.buffer, anchor.start.row, anchor.finish.row, true)
    if #rows > 0 then rows[1] = rows[1]:sub(anchor.start.column + 1) end
    rows[#rows + 1] = ""
    return rows
  end
  return vim.api.nvim_buf_get_text(session.buffer, anchor.start.row, anchor.start.column, anchor.finish.row, anchor.finish.column, {})
end

local function physical_map(session, pending)
  local owner = {}
  for _, patch in pairs(pending.patch) do
    local acknowledgement = pending.acknowledgement[patch.base]
    if acknowledgement then
      local id = local_owner(session, patch, acknowledgement)
      if not owner[id] or owner[id].base < patch.base then owner[id] = { base=patch.base, entry=patch.metadata_edit[1] } end
    end
  end
  local mapped = {}
  for id, captured in pairs(owner) do
    local entry = captured.entry
    local region = entry.metadata.editable_region[1]
    local anchor = assert(session.editable.native.anchor[region.id], "local physical anchor missing")
    local _, start = session.sequence:position(id)
    local physical_start = anchor.start.row - region.range.start.row
    mapped[#mapped + 1] = { id=id, start=start, count=session.block[id].row_count, physical_start=physical_start, next_count=entry.row_count, entry=entry }
    for _, declared in ipairs(entry.metadata.editable_region) do
      local accepted = assert(session.editable.region[declared.id], "local region state missing").accepted_text
      if accepted then assert(vim.deep_equal(physical_text(session, declared.id), accepted), "physical local text differs from acknowledgement") end
    end
  end
  table.sort(mapped, function(left,right) return left.start < right.start end)
  local delta = 0
  for _, entry in ipairs(mapped) do
    assert(entry.physical_start == entry.start + delta, "local owner moved outside native anchors")
    delta = delta + entry.next_count - entry.count
  end
  assert(vim.api.nvim_buf_line_count(session.buffer) == math.max(1,session.row_count + delta), "local physical row count differs")
  local function position(row)
    local offset = 0
    for _, entry in ipairs(mapped) do
      if row < entry.start then break end
      if row < entry.start + entry.count then return entry.physical_start + math.min(row-entry.start,entry.next_count-1) end
      offset = offset + entry.next_count-entry.count
    end
    return row + offset
  end
  return mapped, position
end

local function validate_generated_patch(shadow, patch)
  local inserted = {}
  for _, edit in ipairs(patch.block_edit) do
    for _, id in ipairs(edit.inserted) do inserted[id] = true end
    for index=edit.start_block,edit.start_block+edit.removed_blocks-1 do
      assert(#shadow.sequence:at(index).entry.metadata.editable_region == 0, "generated patch removes an editable block")
    end
  end
  for _, entry in ipairs(patch.metadata_edit) do
    local existing = shadow.sequence.node[entry.block] and shadow.block[entry.block]
    assert(not existing or #existing.metadata.editable_region == 0, "generated patch changes editable region")
    if #entry.metadata.editable_region > 0 then
      assert(not existing and inserted[entry.block], "generated patch changes editable region")
      for _, region in ipairs(entry.metadata.editable_region) do
        assert(not shadow.region_owner[region.id], "generated patch reuses editable region")
      end
    end
  end
  for _, edit in ipairs(patch.text_edit) do
    if edit.removed_rows > 0 then
      local first = assert(shadow.sequence:locate(edit.start_row))
      local last = assert(shadow.sequence:locate(edit.start_row+edit.removed_rows-1))
      for index=shadow.sequence:position(first.id),shadow.sequence:position(last.id) do
        assert(#shadow.sequence:at(index).entry.metadata.editable_region == 0, "generated text crosses editable block")
      end
    elseif edit.start_row < shadow.row_count then
      local block = shadow.sequence:locate(edit.start_row)
      local _, start = shadow.sequence:position(block.id)
      assert(edit.start_row == start or #block.entry.metadata.editable_region == 0, "generated insertion crosses editable block")
    end
  end
end

local function compose(session,pending)
  local native_buffer = require("forge.buffer")
  local local_block, physical_position = physical_map(session,pending)
  local text, block = spans.new(session.row_count), spans.new(session.sequence:count())
  local shadow = setmetatable({block=setmetatable({},{__index=session.block}), region_owner=setmetatable({},{__index=session.region_owner}), revision=session.revision,row_count=session.row_count}, {__index=session})
  local changed = {}
  local function original(row) return assert(vim.api.nvim_buf_get_lines(session.buffer,physical_position(row),physical_position(row)+1,true)[1], "original physical row missing") end
  session.sequence:begin()
  local ok, result = pcall(function()
    for _=1,pending.processed do
      local patch = pending.patch[shadow.revision]
      local acknowledgement = pending.acknowledgement[shadow.revision]
      if not acknowledgement then validate_generated_patch(shadow,patch)
      else
        local owner=local_owner(shadow,patch,acknowledgement)
        local _,start=shadow.sequence:position(owner)
        local finish=start+shadow.block[owner].row_count
        for _,edit in ipairs(patch.text_edit) do
          assert(edit.start_row>=start and edit.start_row+edit.removed_rows<=finish,"local text patch crosses its owner")
        end
      end
      for _, edit in ipairs(patch.text_edit) do text=spans.splice(text,edit.start_row,edit.removed_rows,edit.text) end
      for _, edit in ipairs(patch.block_edit) do block=spans.splice(block,edit.start_block,edit.removed_blocks,edit.inserted) end
      local prepared=native_buffer.preflight(shadow,patch,1,{keep_sequence=true,read_row=function(row) return spans.at(text,row,original) end})
      if acknowledgement then
        local owner=local_owner(shadow,patch,acknowledgement)
        local _,start=shadow.sequence:position(owner)
        local region
        for _,candidate in ipairs(patch.metadata_edit[1].metadata.editable_region) do if candidate.id==acknowledgement.region then region=candidate break end end
        local captured={}
        for row=region.range.start.row,region.range["end"].row do
          local value=(start+row==patch.next_rows and region.range["end"].column==0) and "" or spans.at(text,start+row,original)
          if row==region.range["end"].row then value=value:sub(1,region.range["end"].column) end
          if row==region.range.start.row then value=value:sub(region.range.start.column+1) end
          captured[#captured+1]=value
        end
        assert(vim.deep_equal(captured,pending.accepted[patch.base]),"native acknowledgement text differs from submitted edit")
      end
      for id in pairs(prepared.retired) do shadow.block[id]=false changed[id]=true end
      for id,entry in pairs(prepared.block) do shadow.block[id]=entry changed[id]=true end
      for id in pairs(prepared.released_region) do shadow.region_owner[id]=false end
      for id,owner in pairs(prepared.region_owner) do shadow.region_owner[id]=owner end
      shadow.revision,shadow.row_count=patch.next,patch.next_rows
    end
    local physical = {}
    for _, piece in ipairs(text) do
      if not piece.start then physical[#physical+1]=piece
      else
        local cursor,finish=piece.start,piece.start+piece.count
        for _, entry in ipairs(local_block) do
          for _, boundary in ipairs({entry.start,entry.start+entry.count}) do
            if boundary>cursor and boundary<finish then physical[#physical+1]={start=physical_position(cursor),count=boundary-cursor} cursor=boundary end
          end
        end
        if cursor<finish then physical[#physical+1]={start=physical_position(cursor),count=finish-cursor} end
      end
    end
    local final_local={}
    for _, entry in ipairs(local_block) do
      local _,start=shadow.sequence:position(entry.id)
      assert(start,"acknowledged local owner was removed")
      final_local[#final_local+1]={start=start,entry=entry}
    end
    table.sort(final_local,function(left,right) return left.start>right.start end)
    for _, local_entry in ipairs(final_local) do
      local entry=local_entry.entry
      physical=spans.splice(physical,local_entry.start,entry.next_count,{},{{start=entry.physical_start,count=entry.next_count}})
    end
    local combined={document=session.document,base=session.revision,next=shadow.revision,base_rows=session.row_count,next_rows=shadow.row_count,
      base_blocks=pending.patch[session.revision].base_blocks,next_blocks=shadow.sequence:count(),text_edit={},block_edit={},metadata_edit={},removed_block={}}
    for _, edit in ipairs(spans.edits(text,session.row_count)) do combined.text_edit[#combined.text_edit+1]={start_row=edit.start,removed_rows=edit.removed,text=edit.value} end
    for _, edit in ipairs(spans.edits(block,combined.base_blocks)) do combined.block_edit[#combined.block_edit+1]={start_block=edit.start,removed_blocks=edit.removed,inserted=edit.value} end
    for id in pairs(changed) do
      if shadow.sequence.node[id] then local entry=shadow.block[id] combined.metadata_edit[#combined.metadata_edit+1]={block=id,row_count=entry.row_count,metadata=entry.metadata}
      elseif session.block[id] then combined.removed_block[#combined.removed_block+1]=id end
    end
    local physical_edit={}
    for _, edit in ipairs(spans.edits(physical,vim.api.nvim_buf_line_count(session.buffer))) do
      for _, entry in ipairs(local_block) do
        assert(edit.start+edit.removed<=entry.physical_start or edit.start>=entry.physical_start+entry.next_count,"generated physical patch crosses local text")
      end
      physical_edit[#physical_edit+1]={start_row=edit.start,removed_rows=edit.removed,text=edit.value}
    end
    return {patch=combined,revisions=pending.processed,following={},projection={text_edit=physical_edit,read_row=function(row)
      return spans.at(physical,row,function(source) return assert(vim.api.nvim_buf_get_lines(session.buffer,source,source+1,true)[1],"physical source row missing") end)
    end}}
  end)
  session.sequence:rollback()
  if not ok then error(result) end
  return result
end

function M.enqueue(session,patch,acknowledgement)
  assert(session.status=="Applied" and type(patch)=="table" and patch.document==session.document,"mixed patch document differs")
  assert(type(patch.base)=="number" and patch.base>=session.revision and patch.base<9007199254740991 and patch.base==math.floor(patch.base) and patch.next==patch.base+1,"mixed patch revision is invalid")
  if acknowledgement then local_owner(session,patch,acknowledgement) end
  local pending=session.local_patch or {mixed=true,patch={},acknowledgement={},accepted={},count=0,processed=0,bytes=0}
  assert(pending.mixed,"mixed patch conflicts with composer queue")
  assert(not pending.patch[patch.base],"duplicate mixed patch revision")
  local accepted = acknowledgement and { document=acknowledgement.document,region=acknowledgement.region,sequence=acknowledgement.sequence,revision=acknowledgement.revision } or false
  local bytes=#vim.json.encode({patch=patch,acknowledgement=accepted})
  assert(pending.count<MAX_PATCHES and bytes<=MAX_BYTES-pending.bytes,"mixed patch admission exhausted")
  pending.patch[patch.base]=vim.deepcopy(patch)
  pending.acknowledgement[patch.base]=accepted
  pending.count,pending.bytes=pending.count+1,pending.bytes+bytes
  session.local_patch=pending
  while pending.patch[session.revision+pending.processed] do
    local accepted=pending.acknowledgement[session.revision+pending.processed]
    if accepted then
      local entry=assert(session.editable.region[accepted.region],"acknowledgement region missing")
      local retained=0
      for _,row in ipairs(entry.sent and entry.sent.text or {}) do retained=retained+#row+1 end
      assert(retained<=MAX_BYTES-pending.bytes,"mixed accepted text admission exhausted")
      pending.bytes=pending.bytes+retained
      pending.accepted[session.revision+pending.processed]=entry.sent and entry.sent.text
      local ok,failure=editable.acknowledge(session.editable,accepted) assert(ok,failure)
    end
    pending.processed=pending.processed+1
  end
  if pending.processed~=pending.count or not editable.ready_to_reconcile(session.editable) then return {kind="Deferred",edit_sequence=session.editable.sequence} end
  return compose(session,pending)
end
return M
