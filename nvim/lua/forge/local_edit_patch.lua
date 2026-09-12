local M = {}
local editable = require("forge.editable")
local MAX_PATCHES = 256
local MAX_BYTES = 16 * 1024 * 1024

local function region_text(buffer, start_row, range)
  local finish = start_row + range["end"].row
  if finish == vim.api.nvim_buf_line_count(buffer) then
    assert(range["end"].column == 0, "invalid adopted region end")
    local text = vim.api.nvim_buf_get_lines(buffer, start_row + range.start.row, finish, true)
    if #text > 0 then text[1] = text[1]:sub(range.start.column + 1) end
    text[#text + 1] = ""
    return text
  end
  return vim.api.nvim_buf_get_text(buffer, start_row + range.start.row, range.start.column,
    finish, range["end"].column, {})
end

function M.validate_snapshot(state, block, text)
  for _, entry in pairs(block) do
    for _, region in ipairs(entry.metadata.editable_region) do
      local accepted = state.region[region.id] and state.region[region.id].accepted_text
      if accepted then
        local first = entry.start_row + region.range.start.row
        local finish = entry.start_row + region.range["end"].row
        local captured = {}
        for row = first, finish do
          local value = text[row + 1] or ""
          if row == finish then value = value:sub(1, region.range["end"].column) end
          if row == first then value = value:sub(region.range.start.column + 1) end
          captured[#captured + 1] = value
        end
        assert(vim.deep_equal(captured, accepted), "snapshot source differs from acknowledged local text")
      end
    end
  end
end

local function prepare(session, acknowledgement, patch)
  assert(session.status == "Applied", "local acknowledgement requires an applied document")
  assert(type(patch) == "table" and patch.document == session.document, "local patch document differs")
  assert(type(patch.base) == "number" and patch.base >= session.revision and patch.base == math.floor(patch.base),
    "local patch base is stale or invalid")
  assert(patch.next == patch.base + 1, "local patch must advance one revision")
  assert(#patch.block_edit == 0 and #patch.removed_block == 0, "local patch changes block identity")
  local region_id = acknowledgement and acknowledgement.region or patch.metadata_edit[1].metadata.editable_region[1].id
  local owner = assert(session.region_owner[region_id], "local region has no owner")
  assert(#patch.metadata_edit == 1 and patch.metadata_edit[1].block == owner, "local patch changes another block")
  local matched = false
  for _, region in ipairs(patch.metadata_edit[1].metadata.editable_region) do
    if region.id == region_id then
      assert(not acknowledgement or region.revision == acknowledgement.revision, "local patch region revision differs")
      matched = true
    end
  end
  assert(matched, "local patch omitted acknowledged region")
  local pending = session.local_patch or { patch = {}, acknowledgement = {}, count = 0, processed = 0, bytes = 0 }
  assert(not pending.patch[patch.base], "duplicate local patch revision")
  local bytes = #vim.json.encode(patch)
  assert(pending.count < MAX_PATCHES and bytes <= MAX_BYTES - pending.bytes, "local patch admission exhausted")
  pending.patch[patch.base] = vim.deepcopy(patch)
  pending.acknowledgement[patch.base] = acknowledgement and vim.deepcopy(acknowledgement) or false
  pending.count, pending.bytes = pending.count + 1, pending.bytes + bytes
  session.local_patch = pending
  while pending.patch[session.revision + pending.processed] do
    local base = session.revision + pending.processed
    local accepted = pending.acknowledgement[base]
    if accepted then
      local success, failure = editable.acknowledge(session.editable, accepted)
      assert(success, failure)
    else
      if editable.ready_to_reconcile(session.editable) then break end
      local region = pending.patch[base].metadata_edit[1].metadata.editable_region[1]
      local entry = assert(session.editable.region[region.id], "generated region disappeared")
      assert(region.revision == entry.revision + 1, "generated region revision is not contiguous")
      assert(entry.pending, "generated region has no newer local source")
      if entry.sent then
        assert(entry.sent.base == entry.revision, "generated edit cannot rebase this local request")
        entry.sent.base = region.revision
      end
      entry.revision, entry.accepted_text = region.revision, nil
    end
    pending.processed = pending.processed + 1
  end
  if not editable.ready_to_reconcile(session.editable) then
    return { kind = "Deferred", edit_sequence = session.editable.sequence }
  end
  local revision, row_count, changed = session.revision, session.row_count, {}
  for _ = 1, pending.processed do
    local next_patch = pending.patch[revision]
    if not next_patch then return { kind = "Deferred", missing_revision = revision } end
    assert(next_patch.base_rows == row_count and next_patch.base_blocks == session.sequence:count()
      and next_patch.next_blocks == next_patch.base_blocks, "local patch base counts differ")
    local entry = next_patch.metadata_edit[1]
    local old = changed[entry.block] or session.block[entry.block]
    local _, start_row = session.sequence:position(entry.block)
    local position = session.sequence:position(entry.block)
    for id, prior in pairs(changed) do
      if session.sequence:position(id) < position then
        start_row = start_row + prior.row_count - session.block[id].row_count
      end
    end
    local delta, previous = 0, nil
    for _, edit in ipairs(next_patch.text_edit) do
      assert(edit.start_row >= start_row and edit.start_row + edit.removed_rows <= start_row + old.row_count,
        "local text patch crosses its owner")
      assert(not previous or edit.start_row + edit.removed_rows <= previous, "local text patches overlap")
      previous = edit.start_row
      delta = delta + #edit.text - edit.removed_rows
    end
    assert(entry.row_count == old.row_count + delta and next_patch.next_rows == row_count + delta,
      "local patch row delta differs")
    changed[entry.block] = entry
    revision, row_count = next_patch.next, next_patch.next_rows
  end
  assert(vim.api.nvim_buf_line_count(session.buffer) == math.max(1, row_count), "adopted native row count differs")
  local ordered = {}
  for id in pairs(changed) do ordered[#ordered + 1] = id end
  table.sort(ordered, function(left, right) return session.sequence:position(left) < session.sequence:position(right) end)
  local combined = { document = session.document, base = session.revision, next = revision,
    base_rows = session.row_count, next_rows = row_count, base_blocks = session.sequence:count(),
    next_blocks = session.sequence:count(), text_edit = {}, metadata_edit = {}, block_edit = {}, removed_block = {} }
  local offset = 0
  for _, id in ipairs(ordered) do
    local entry = changed[id]
    local _, original_start = session.sequence:position(id)
    local final_start = original_start + offset
    for _, region in ipairs(entry.metadata.editable_region) do
      local state = assert(session.editable.region[region.id], "adopted region identity differs")
      assert(region.revision == state.revision, "adopted region revision differs")
      if state.accepted_text then
        assert(vim.deep_equal(region_text(session.buffer, final_start, region.range), state.accepted_text),
          "native text differs from acknowledged edit")
      end
    end
    table.insert(combined.text_edit, 1, { start_row = original_start, removed_rows = session.block[id].row_count,
      text = vim.api.nvim_buf_get_lines(session.buffer, final_start, final_start + entry.row_count, true) })
    combined.metadata_edit[#combined.metadata_edit + 1] = entry
    offset = offset + entry.row_count - session.block[id].row_count
  end
  local following = {}
  for base = revision, session.revision + pending.count - 1 do
    assert(pending.patch[base] and pending.acknowledgement[base] == false, "unresolved local patch ordering")
    following[#following + 1] = pending.patch[base]
  end
  return { patch = combined, revisions = pending.processed, following = following }
end

M.prepare = function(session, acknowledgement, patch)
  if session.editable.native and (session.sequence:count() ~= 1 or session.local_patch and session.local_patch.mixed) then
    return require("forge.mixed_edit_patch").enqueue(session, patch, acknowledgement)
  end
  return prepare(session, acknowledgement, patch)
end

function M.generated(session, patch)
  if session.editable.native and (session.sequence:count() ~= 1 or session.local_patch and session.local_patch.mixed) then
    return require("forge.mixed_edit_patch").enqueue(session, patch)
  end
  if session.sequence:count() ~= 1 or vim.tbl_count(session.editable.region) ~= 1
    or #patch.metadata_edit ~= 1 or #patch.metadata_edit[1].metadata.editable_region ~= 1
  then
    return { kind = "Deferred", edit_sequence = session.editable.sequence }
  end
  return prepare(session, nil, patch)
end

return M
