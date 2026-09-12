local M = {}
local editable = require("forge.editable")
local snapshot_transfer = require("forge.snapshot")
local BlockSequence = require("forge.block_sequence")
local folds = require("forge.folds")
local decorations = require("forge.decorations")
local MAX_COUNTER = 9007199254740991

local function counter(value)
  assert(type(value) == "number" and value >= 0 and value <= MAX_COUNTER and value == math.floor(value), "invalid counter")
  return value
end

local function identity(value)
  assert(type(value) == "string" and #value > 0 and #value <= 256 and not value:find("%c"), "invalid identity")
  return value
end

local function array(value)
  assert(type(value) == "table" and vim.tbl_count(value) == #value, "expected dense array")
  return value
end

local function rows(value)
  for _, row in ipairs(array(value)) do
    assert(type(row) == "string" and not row:find("[\n%z]"), "invalid text row")
  end
  return value
end

local function before_or_equal(left, right)
  return left.row < right.row or (left.row == right.row and left.column <= right.column)
end

local function validate_metadata(entry, read_row, region_seen)
  local function validate_range(range)
    assert(before_or_equal(range.start, range["end"]), "reversed metadata range")
    for _, position in ipairs({ range.start, range["end"] }) do
      counter(position.row)
      counter(position.column)
      if position.row == entry.row_count then
        assert(position.column == 0, "invalid metadata end column")
      else
        assert(position.row < entry.row_count, "metadata outside block")
        local row = read_row(entry.start_row + position.row)
        local byte = row:byte(position.column + 1)
        assert(position.column <= #row and (not byte or byte < 128 or byte >= 192), "metadata splits UTF-8")
      end
    end
  end
  for _, target in ipairs(array(entry.metadata.target)) do
    identity(target.id)
    validate_range(target.range)
  end
  for _, list in ipairs({ entry.metadata.decoration, entry.metadata.visible_decoration or {}, entry.metadata.source_highlight or {} }) do
    for _, decoration in ipairs(array(list)) do
      identity(decoration.capture)
      assert(counter(decoration.priority) <= 65535, "invalid decoration priority")
      validate_range(decoration.range)
    end
  end
  for _, gutter in ipairs(array(entry.metadata.gutter or {})) do
    validate_range({ start = gutter.position, ["end"] = gutter.position })
    assert(gutter.position.row < entry.row_count, "gutter requires a physical row")
    assert(counter(gutter.priority) <= 65535, "invalid gutter priority")
    for _, chunk in ipairs(array(gutter.chunk)) do
      assert(type(chunk.text) == "string" and not chunk.text:find("[\n%z]"), "invalid gutter text")
      identity(chunk.capture)
    end
  end
  for _, conceal in ipairs(array(entry.metadata.conceal or {})) do
    validate_range(conceal.range)
    assert(type(conceal.replacement) == "string" and not conceal.replacement:find("[%c]")
      and vim.fn.strchars(conceal.replacement) <= 1, "invalid conceal replacement")
    assert(type(conceal.line) == "boolean", "invalid conceal line flag")
    assert(counter(conceal.priority) <= 65535, "invalid conceal priority")
  end
  for _, overlay in ipairs(array(entry.metadata.source_overlay or {})) do
    validate_range(overlay.range)
    assert(overlay.range.start.row == overlay.range["end"].row
      and overlay.range.start.row < entry.row_count, "invalid source overlay range")
    assert(type(overlay.text) == "string" and #overlay.text > 0 and #overlay.text <= 8192
      and not overlay.text:find("%c"), "invalid source overlay text")
    identity(overlay.capture)
    assert(counter(overlay.priority) <= 65535, "invalid source overlay priority")
  end
  local previous = {}
  for _, region in ipairs(array(entry.metadata.editable_region)) do
    identity(region.id)
    assert(not region_seen[region.id], "duplicate editable region")
    region_seen[region.id] = region.revision
    counter(region.revision)
    counter(region.sequence or 0)
    validate_range(region.range)
    for _, earlier in ipairs(previous) do
      assert(before_or_equal(region.range["end"], earlier.start) or before_or_equal(earlier["end"], region.range.start), "overlapping editable regions")
    end
    previous[#previous + 1] = region.range
  end
end

local function validate_order(order, block, row_count, read_row)
  local seen, region_seen, start = {}, {}, 0
  for _, id in ipairs(order) do
    identity(id)
    assert(not seen[id], "duplicate block identity")
    seen[id] = true
    local entry = assert(block[id], "missing block metadata")
    counter(entry.row_count)
    entry.start_row = start
    validate_metadata(entry, read_row, region_seen)
    start = start + entry.row_count
  end
  assert(start == row_count, "block row coverage differs")
  return region_seen
end

function M.open(document, options)
  options = options or {}
  identity(document)
  local physical = options.physical == true
  local generated = options.generated == true
  assert(not (physical and generated), "buffer ownership modes are exclusive")
  assert(not options.buffer or physical or generated, "existing buffers require explicit ownership")
  assert(not physical or options.buffer, "physical ownership requires an existing file buffer")
  assert(not generated or options.buffer, "generated adoption requires an existing buffer")
  if generated then counter(options.expected_changedtick) end
  assert(not physical or not options.editable, "physical editing retains file callback ownership")
  local buffer = options.buffer or vim.api.nvim_create_buf(false, true)
  assert(vim.api.nvim_buf_is_valid(buffer), "physical buffer is invalid")
  local previous_native = vim.b[buffer].forge_native_document
  vim.b[buffer].forge_native_document = true
  if not physical and not generated then
    vim.bo[buffer].buftype = "nofile"
    vim.bo[buffer].bufhidden = "hide"
    vim.bo[buffer].swapfile = false
    vim.bo[buffer].filetype = options.filetype or "forge"
    vim.bo[buffer].modifiable = false
  end
  local session = {
    physical = physical, generated = generated, previous_native = previous_native,
    expected_changedtick = generated and options.expected_changedtick or nil,
    generated_filetype = options.filetype or "forge",
    document = document, buffer = buffer, namespace = vim.api.nvim_create_namespace(""),
    revision = nil, row_count = 0, sequence = BlockSequence.new(), block = {}, marks = {}, region_owner = {},
    changedtick = vim.api.nvim_buf_get_changedtick(buffer), status = "Desynchronized",
    editable = editable.new(document), recover = options.recover, notice = options.notice,
    edit_options = options.editable,
    transfer = snapshot_transfer.new(document),
  }
  session.lifecycle_autocmd = vim.api.nvim_create_autocmd("BufWipeout", { buffer = buffer, once = true, callback = function()
    session.lifecycle_autocmd = nil
    M.invalidate(session)
  end })
  return session
end

function M.fail_apply(session, diagnostic)
  session.status = "Desynchronized"
  session.diagnostic = tostring(diagnostic)
  if vim.api.nvim_buf_is_valid(session.buffer) then
    pcall(vim.api.nvim_buf_clear_namespace, session.buffer, session.namespace, 0, -1)
    if not session.physical then pcall(function() vim.bo[session.buffer].modifiable = false end) end
  end
  if session.notice then
    pcall(session.notice, session.diagnostic)
  end
  if session.recover then
    pcall(session.recover, session.document, session.diagnostic)
  end
  return { kind = "Desynchronized", diagnostic = session.diagnostic }
end

local function change_sequence(sequence, patch, entries)
  for _, edit in ipairs(patch.block_edit) do
    sequence:splice(edit.start_block, edit.removed_blocks, {})
  end
  local delta = 0
  for index = #patch.block_edit, 1, -1 do
    local edit = patch.block_edit[index]
    local inserted = {}
    for _, id in ipairs(edit.inserted) do
      inserted[#inserted + 1] = { id = id, entry = assert(entries[id], "missing inserted metadata") }
    end
    sequence:splice(edit.start_block + delta, 0, inserted)
    delta = delta + #inserted - edit.removed_blocks
  end
  for id, entry in pairs(entries) do
    assert(sequence.node[id], "metadata identity is not retained")
    sequence:update(id, entry)
  end
end

function M.preflight(session, patch, adoption, projection)
  assert(session.status == "Applied", "document requires snapshot recovery")
  if not session.fragment then
  assert(vim.api.nvim_buf_is_valid(session.buffer), "native buffer is invalid")
  assert(adoption or vim.api.nvim_buf_get_changedtick(session.buffer) == session.changedtick, "native changedtick differs")
  assert(session.sentinel and #vim.api.nvim_buf_get_extmark_by_id(session.buffer, session.namespace, session.sentinel, {}) == 2,
    "native document anchor is missing")
  end
  assert(patch.document == session.document and patch.base == session.revision, "patch base differs")
  assert(not session.physical or #array(patch.text_edit) == 0, "physical buffer only accepts metadata patches")
  assert(counter(patch.next) == counter(patch.base) + (adoption or 1), "patch revision advance differs")
  assert(patch.base_rows == session.row_count and patch.base_blocks == session.sequence:count(), "patch base counts differ")
  counter(patch.next_rows)
  counter(patch.next_blocks)
  local previous, count = nil, patch.base_rows
  local touched = {}
  for _, edit in ipairs(array(patch.text_edit)) do
    local finish = counter(edit.start_row) + counter(edit.removed_rows)
    assert(finish <= patch.base_rows and (not previous or (finish <= previous and edit.start_row < previous)), "text edits overlap or are unordered")
    count = count + #rows(edit.text) - edit.removed_rows
    local first = session.sequence:locate(math.min(edit.start_row, patch.base_rows - 1))
    local last = session.sequence:locate(math.min(math.max(edit.start_row, finish - 1), patch.base_rows - 1))
    if first and last then
      local first_index, last_index = session.sequence:position(first.id), session.sequence:position(last.id)
      for index = first_index, last_index do touched[session.sequence:at(index).id] = true end
    end
    previous = edit.start_row
  end
  assert(count == patch.next_rows, "patch row count differs")
  local entries, inserted, removed = {}, {}, {}
  previous = nil
  local next_count = patch.base_blocks
  for _, edit in ipairs(array(patch.block_edit)) do
    local finish = counter(edit.start_block) + counter(edit.removed_blocks)
    assert(finish <= patch.base_blocks and (not previous or (finish <= previous and edit.start_block < previous)), "block edits overlap or are unordered")
    for _, id in ipairs(array(edit.inserted)) do
      identity(id)
      assert(not inserted[id], "duplicate inserted identity")
      inserted[id] = true
    end
    for index = edit.start_block, finish - 1 do removed[session.sequence:at(index).id] = true end
    next_count = next_count + #edit.inserted - edit.removed_blocks
    previous = edit.start_block
  end
  assert(next_count == patch.next_blocks, "patch block count differs")
  local changed, retired = {}, {}
  for _, edit in ipairs(array(patch.metadata_edit)) do
    identity(edit.block)
    assert(not changed[edit.block], "duplicate metadata identity")
    changed[edit.block] = true
    entries[edit.block] = { row_count = counter(edit.row_count), metadata = vim.deepcopy(edit.metadata) }
  end
  for _, id in ipairs(array(patch.removed_block)) do
    assert(removed[id] and not inserted[id] and not retired[id], "invalid retired identity")
    retired[id] = true
  end
  for id in pairs(removed) do
    assert(inserted[id] or retired[id], "missing retired identity")
  end
  for id in pairs(inserted) do
    assert(not session.block[id] or removed[id], "duplicate retained identity")
    assert(entries[id] or session.block[id], "missing inserted metadata")
    changed[id] = true
    if not entries[id] then
      local old = session.block[id]
      entries[id] = { row_count = old.row_count, metadata = old.metadata }
    end
  end
  for id in pairs(touched) do
    if not retired[id] then
      changed[id] = true
      if not entries[id] then
        local old = session.block[id]
        entries[id] = { row_count = old.row_count, metadata = old.metadata }
      end
    end
  end
  local function read_row(row)
    if projection and projection.read_row then return projection.read_row(row) end
    if adoption then return assert(vim.api.nvim_buf_get_lines(session.buffer, row, row + 1, true)[1], "missing adopted row") end
    local delta = 0
    for index = #patch.text_edit, 1, -1 do
      local edit = patch.text_edit[index]
      local start = edit.start_row + delta
      if row < start then break end
      if row < start + #edit.text then return edit.text[row - start + 1] end
      delta = delta + #edit.text - edit.removed_rows
    end
    return assert(vim.api.nvim_buf_get_lines(session.buffer, row - delta, row - delta + 1, true)[1], "missing resulting row")
  end
  local released_region, region_owner, region, position = {}, {}, {}, {}
  for _, set in ipairs({ changed, retired }) do
    for id in pairs(set) do
      for _, handle in ipairs(session.marks[id] or {}) do
        assert(#vim.api.nvim_buf_get_extmark_by_id(session.buffer, session.namespace, handle, {}) == 2,
          "native decoration handle is missing")
      end
      local old = session.block[id]
      if old then
        for _, editable_region in ipairs(old.metadata.editable_region) do released_region[editable_region.id] = true end
      end
    end
  end
  if not (projection and projection.keep_sequence) then session.sequence:begin() end
  local ok, failure = pcall(function()
    change_sequence(session.sequence, patch, entries)
    assert(session.sequence:rows() == patch.next_rows, "block row coverage differs")
    for id in pairs(changed) do
      local entry = assert(entries[id])
      local _, start_row = session.sequence:position(id)
      position[id] = start_row
      validate_metadata({ row_count = entry.row_count, metadata = entry.metadata, start_row = start_row }, read_row, region)
      decorations.prepare(entry)
      for _, editable_region in ipairs(entry.metadata.editable_region) do
        assert(not session.region_owner[editable_region.id] or released_region[editable_region.id], "duplicate editable region")
        region_owner[editable_region.id] = id
      end
    end
    folds.validate(session.sequence, changed, retired, session.fold, read_row)
  end)
  if not (projection and projection.keep_sequence) then session.sequence:rollback() end
  if not ok then error(failure) end
  return { block = entries, position = position, region = region, changed = changed, retired = retired,
    released_region = released_region, region_owner = region_owner }
end

local function install_metadata(session, prepared, replace_all)
  if replace_all then
    vim.api.nvim_buf_clear_namespace(session.buffer, session.namespace, 0, -1)
    session.marks = {}
    session.sentinel = vim.api.nvim_buf_set_extmark(session.buffer, session.namespace, 0, 0, {})
  end
  for _, set in ipairs({ prepared.changed, prepared.retired }) do
    for id in pairs(set) do
      local mark = session.marks[id] or {}
      for _, handle in ipairs(mark) do
        vim.api.nvim_buf_del_extmark(session.buffer, session.namespace, handle)
      end
      session.marks[id] = nil
    end
  end
  for id in pairs(prepared.changed) do
      local entry, mark = prepared.block[id], {}
      local start_row = prepared.position[id]
      for _, decoration in ipairs(entry.metadata.decoration) do
        local range = decoration.range
        mark[#mark + 1] = vim.api.nvim_buf_set_extmark(session.buffer, session.namespace,
          start_row + range.start.row, range.start.column, {
            end_row = start_row + range["end"].row, end_col = range["end"].column,
            hl_group = decoration.capture, priority = decoration.priority, strict = true,
          })
      end
      for _, gutter in ipairs(entry.metadata.gutter or {}) do
        local text = {}
        for _, chunk in ipairs(gutter.chunk) do text[#text + 1] = { chunk.text, chunk.capture } end
        mark[#mark + 1] = vim.api.nvim_buf_set_extmark(session.buffer, session.namespace,
          start_row + gutter.position.row, gutter.position.column, {
            virt_text = text, virt_text_pos = "inline", hl_mode = "combine", priority = gutter.priority,
            right_gravity = false, strict = true,
          })
      end
      for _, conceal in ipairs(entry.metadata.conceal or {}) do
        local range = conceal.range
        mark[#mark + 1] = vim.api.nvim_buf_set_extmark(session.buffer, session.namespace,
          start_row + range.start.row, range.start.column, {
            end_row = start_row + range["end"].row, end_col = range["end"].column,
            conceal = conceal.replacement, conceal_lines = conceal.line and "" or nil,
            priority = conceal.priority, strict = true,
          })
      end
      session.marks[id] = mark
  end
end

local function attach_regions(session, prepared)
  if not session.edit_options then return end
  local anchor, region_state = {}, {}
  for id in pairs(prepared.changed) do
    local block = prepared.block[id]
    local start_row = prepared.position[id]
    for _, region in ipairs(block.metadata.editable_region) do
      session.editable.sequence = math.max(session.editable.sequence, region.sequence or 0)
      region_state[region.id] = { revision = region.revision }
      anchor[region.id] = {
        start = { row = start_row + region.range.start.row, column = region.range.start.column },
        finish = { row = start_row + region.range["end"].row, column = region.range["end"].column },
      }
    end
  end
  if session.editable.native then
    editable.update_regions(session.editable, anchor, region_state, prepared.released_region, function(id)
      local owner = assert(session.region_owner[id], "editable region has no block owner")
      local _, start_row = session.sequence:position(owner)
      for _, region in ipairs(session.block[owner].metadata.editable_region) do
        if region.id == id then
          return {
            start = { row = start_row + region.range.start.row, column = region.range.start.column },
            finish = { row = start_row + region.range["end"].row, column = region.range["end"].column },
          }
        end
      end
      error("editable region metadata is missing")
    end)
  else
    session.editable.region = region_state
    editable.attach(session.editable, session.buffer, anchor, session.edit_options)
  end
end

local function commit_patch(session, patch, adoption, projection)
  if not adoption and editable.suspend_generated_text(session.editable) then
    return { kind = "Deferred", edit_sequence = session.editable.sequence }
  end
  local ok, prepared = pcall(M.preflight, session, patch, adoption, projection)
  if not ok then return M.fail_apply(session, prepared) end
  if adoption then
    local revision = {}
    for id, entry in pairs(session.editable.region) do revision[id] = entry.revision end
    for id, value in pairs(prepared.region) do revision[id] = value end
    local reconciled, failure = editable.reconciled(session.editable, revision)
    if not reconciled then return M.fail_apply(session, failure) end
  end
  local readonly = vim.bo[session.buffer].readonly
  ok, prepared.failure = pcall(function()
    assert(not vim.in_fast_event(), "buffer mutation requires the main loop")
    session.applying = true
    change_sequence(session.sequence, patch, prepared.block)
    for id in pairs(prepared.retired) do session.block[id] = nil end
    for id, entry in pairs(prepared.block) do session.block[id] = entry end
    folds.update(session, prepared, false)
    editable.applying(session.editable, true)
    if not session.physical then
      vim.bo[session.buffer].readonly = false
      vim.bo[session.buffer].modifiable = true
    end
    session.fold_pending = projection and projection.text_edit or adoption and {} or patch.text_edit
    for index, edit in ipairs(session.fold_pending) do
      session.fold_pending_index = index + 1
      local finish = edit.start_row + edit.removed_rows
      if session.row_count == 0 then finish = 1 end
      vim.api.nvim_buf_set_lines(session.buffer, edit.start_row, finish, true, edit.text)
    end
    install_metadata(session, prepared, false)
    assert(vim.api.nvim_buf_line_count(session.buffer) == math.max(1, patch.next_rows), "native result row count differs")
    if not session.physical then vim.bo[session.buffer].modifiable = false end
  end)
  session.applying, session.fold_pending, session.fold_pending_index = nil, nil, nil
  pcall(function() vim.bo[session.buffer].readonly = readonly end)
  editable.applying(session.editable, false)
  if not ok then return M.fail_apply(session, prepared.failure) end
  for id in pairs(prepared.released_region) do session.region_owner[id] = nil end
  for id, owner in pairs(prepared.region_owner) do session.region_owner[id] = owner end
  ok, prepared.failure = pcall(function()
    attach_regions(session, prepared)
  end)
  if not ok then return M.fail_apply(session, prepared.failure) end
  session.row_count, session.revision = patch.next_rows, patch.next
  session.changedtick = vim.api.nvim_buf_get_changedtick(session.buffer)
  decorations.attach(session)
  folds.refresh(session)
  return { kind = "Applied", revision = session.revision }
end

local function adopt_local_result(session, result)
  if result.kind then return result end
  local applied = commit_patch(session, result.patch, result.revisions, result.projection)
  if applied.kind == "Applied" then
    session.local_patch = nil
    for _, following in ipairs(result.following) do
      applied = M.apply_patch(session, following)
      if applied.kind ~= "Applied" then break end
    end
  end
  return applied
end

function M.apply_patch(session, patch)
  if session.status == "Closed" then return { kind = "Closed" } end
  if session.local_patch or editable.suspend_generated_text(session.editable) then
    local ok, result = pcall(require("forge.local_edit_patch").generated, session, patch)
    if not ok then return M.fail_apply(session, result) end
    return adopt_local_result(session, result)
  end
  return commit_patch(session, patch)
end

function M.acknowledge_edit(session, acknowledgement, patch)
  if session.status == "Closed" then return { kind = "Closed" } end
  local ok, result = pcall(require("forge.local_edit_patch").prepare, session, acknowledgement, patch)
  if not ok then return M.fail_apply(session, result) end
  return adopt_local_result(session, result)
end

function M.apply_snapshot(session, snapshot)
  if session.status == "Closed" then return { kind = "Closed" } end
  if session.expected_changedtick ~= nil and vim.api.nvim_buf_is_valid(session.buffer)
    and vim.api.nvim_buf_get_changedtick(session.buffer) ~= session.expected_changedtick
  then
    return { kind = "SourceChanged", changedtick = vim.api.nvim_buf_get_changedtick(session.buffer) }
  end
  local reconciling = editable.suspend_generated_text(session.editable)
  if reconciling and not editable.ready_to_reconcile(session.editable) then
    return { kind = "Deferred", edit_sequence = session.editable.sequence }
  end
  local ok, prepared = pcall(function()
    assert(vim.api.nvim_buf_is_valid(session.buffer), "native buffer is invalid")
    assert(session.status ~= "Closed", "document is closed")
    assert(snapshot.document == session.document, "snapshot document differs")
    counter(snapshot.revision)
    assert(not session.revision or snapshot.revision >= session.revision, "snapshot revision is stale")
    local order, block, text = {}, {}, {}
    for _, entry in ipairs(array(snapshot.block)) do
      identity(entry.id)
      assert(not block[entry.id], "duplicate snapshot block")
      order[#order + 1] = entry.id
      block[entry.id] = { row_count = #rows(entry.text), metadata = vim.deepcopy(entry.metadata) }
      vim.list_extend(text, entry.text)
    end
    local region = validate_order(order, block, #text, function(row) return text[row + 1] end)
    if session.physical then
      local native = vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true)
      assert(vim.deep_equal(native, #text == 0 and { "" } or text), "physical buffer differs from projected source")
      assert(next(region) == nil, "physical buffer edits retain file ownership")
    end
    local sequence, changed, position, region_owner = BlockSequence.new(), {}, {}, {}
    for _, id in ipairs(order) do
      local entry = block[id]
      sequence:splice(sequence:count(), 0, { { id = id, entry = entry } })
      changed[id], position[id] = true, entry.start_row
      decorations.prepare(entry)
      for _, editable_region in ipairs(entry.metadata.editable_region) do region_owner[editable_region.id] = id end
    end
    folds.validate(sequence, changed, {}, nil, function(row) return text[row + 1] end)
    if reconciling then
      for id, entry in pairs(session.editable.region) do
        assert(region[id] == entry.revision, "snapshot does not contain acknowledged region revision")
      end
      require("forge.local_edit_patch").validate_snapshot(session.editable, block, text)
    end
    return { sequence = sequence, block = block, text = text, region = region, changed = changed,
      position = position, region_owner = region_owner, retired = {} }
  end)
  if not ok then return M.fail_apply(session, prepared) end
  local readonly = vim.bo[session.buffer].readonly
  ok, prepared.failure = pcall(function()
    assert(not vim.in_fast_event(), "buffer mutation requires the main loop")
    editable.detach(session.editable)
    session.applying = true
    if session.generated and not session.generated_owned then
      vim.bo[session.buffer].buftype = "nofile"
      vim.bo[session.buffer].bufhidden = "hide"
      vim.bo[session.buffer].swapfile = false
      vim.bo[session.buffer].filetype = session.generated_filetype
      session.generated_owned = true
    end
    session.sequence, session.block, session.region_owner = prepared.sequence, prepared.block, prepared.region_owner
    folds.update(session, prepared, true)
    if not session.physical then
      vim.bo[session.buffer].readonly = false
      vim.bo[session.buffer].modifiable = true
      vim.api.nvim_buf_set_lines(session.buffer, 0, -1, true, prepared.text)
    end
    install_metadata(session, prepared, true)
    if not session.physical then vim.bo[session.buffer].modifiable = false end
    if reconciling then
      assert(editable.reconciled(session.editable, prepared.region))
    end
    attach_regions(session, prepared)
  end)
  session.applying = nil
  pcall(function() vim.bo[session.buffer].readonly = readonly end)
  if not ok then return M.fail_apply(session, prepared.failure) end
  session.row_count, session.revision = #prepared.text, snapshot.revision
  session.changedtick = vim.api.nvim_buf_get_changedtick(session.buffer)
  session.status, session.diagnostic = "Applied", nil
  session.expected_changedtick = nil
  session.local_patch = nil
  decorations.attach(session)
  folds.refresh(session)
  return { kind = "Applied", revision = session.revision }
end

local function release(session, preserve_buffer)
  if session.status == "Closed" then return end
  session.status = "Closed"
  if session.lifecycle_autocmd then
    pcall(vim.api.nvim_del_autocmd, session.lifecycle_autocmd)
    session.lifecycle_autocmd = nil
  end
  editable.detach(session.editable)
  folds.detach(session)
  decorations.detach(session)
  if vim.api.nvim_buf_is_valid(session.buffer) then
    if preserve_buffer or session.physical or (session.generated and not session.generated_owned) then
      vim.api.nvim_buf_clear_namespace(session.buffer, session.namespace, 0, -1)
      vim.b[session.buffer].forge_native_document = session.previous_native
    else
      vim.api.nvim_buf_delete(session.buffer, { force = true })
    end
  end
  session.transfer.active = nil
  session.local_patch, session.gutter_selection, session.edit_options = nil, nil, nil
  session.editable.region, session.editable.suspended = {}, false
  session.marks, session.block, session.region_owner = {}, {}, {}
  session.sequence = BlockSequence.new()
end

--- Releases a replica after local edit acknowledgement, optionally retaining its physical buffer.
---@param session table
---@param options? {preserve_buffer?: boolean}
function M.close(session, options)
  if session.status == "Closed" then return end
  if editable.suspend_generated_text(session.editable) then
    return { kind = "Deferred", edit_sequence = session.editable.sequence }
  end
  release(session, options and options.preserve_buffer)
end

--- Revokes a replica after its host generation has been collected.
--- Retained physical text and changedtick govern admission into the replacement generation.
---@param session table
function M.invalidate(session)
  release(session, true)
end

function M.apply_snapshot_part(session, part)
  if session.status == "Closed" then return { kind = "Closed" } end
  local snapshot, failure, status = snapshot_transfer.accept(session.transfer, part)
  if failure then return M.fail_apply(session, failure) end
  if not snapshot then return { kind = status } end
  return M.apply_snapshot(session, snapshot)
end

---@param session table
---@param row integer
---@param column integer
---@return {block: string, position: {row: integer, column: integer}, target: string?}?
function M.locate(session, row, column)
  if session.locate then return session.locate(row, column) end
  local node = session.sequence:locate(row)
  if not node then return nil end
  local _, start_row = session.sequence:position(node.id)
  local position = { row = row - start_row, column = column }
  local target_id
  for _, target in ipairs(node.entry.metadata.target) do
    if before_or_equal(target.range.start, position) and not before_or_equal(target.range["end"], position) then
      target_id = target.id
      break
    end
  end
  return { block = node.id, position = position, target = target_id }
end

---@class ForgeBufferFragment
---@field document string
---@field revision integer
---@field row_count integer
---@field sequence ForgeBlockSequence
---@field block table<string, table>
---@field fold table
---@field fragment true
---@field status string

---@param snapshot table
---@return ForgeBufferFragment
function M.fragment(snapshot)
  identity(snapshot.document)
  counter(snapshot.revision)
  local fragment = { document = snapshot.document, revision = snapshot.revision, fragment = true,
    status = "Applied", row_count = 0, block = {}, marks = {}, region_owner = {} }
  local order, entries, text, changed, position = {}, {}, {}, {}, {}
  for _, block in ipairs(array(snapshot.block)) do
    identity(block.id)
    assert(not fragment.block[block.id], "duplicate body block")
    local entry = { row_count = #rows(block.text), text = block.text, metadata = block.metadata }
    fragment.block[block.id] = entry
    order[#order + 1], entries[#entries + 1] = block.id, { id = block.id, entry = entry }
    changed[block.id], position[block.id] = true, #text
    vim.list_extend(text, block.text)
    decorations.prepare(entry)
  end
  local region = validate_order(order, fragment.block, #text, function(row) return text[row + 1] end)
  assert(next(region) == nil, "status bodies cannot contain editable regions")
  fragment.sequence, fragment.row_count = BlockSequence.from(entries), #text
  folds.validate(fragment.sequence, changed, {}, nil, function(row) return text[row + 1] end)
  folds.update(fragment, { block = fragment.block, changed = changed, retired = {} }, true)
  return fragment
end

---@param fragment ForgeBufferFragment
---@param patch table
---@param read_original fun(row: integer): string
---@return table
function M.patch_fragment(fragment, patch, read_original)
  local function read_row(row)
    local delta = 0
    for index = #patch.text_edit, 1, -1 do
      local edit = patch.text_edit[index]
      local start = edit.start_row + delta
      if row < start then break end
      if row < start + #edit.text then return edit.text[row - start + 1] end
      delta = delta + #edit.text - edit.removed_rows
    end
    return read_original(row - delta)
  end
  local prepared = M.preflight(fragment, patch, nil, { read_row = read_row })
  assert(next(prepared.region) == nil, "status bodies cannot contain editable regions")
  for id, entry in pairs(prepared.block) do
    entry.text = {}
    for row = 0, entry.row_count - 1 do entry.text[row + 1] = read_row(prepared.position[id] + row) end
  end
  change_sequence(fragment.sequence, patch, prepared.block)
  for id in pairs(prepared.retired) do fragment.block[id] = nil end
  for id, entry in pairs(prepared.block) do fragment.block[id] = entry end
  folds.update(fragment, prepared, false)
  fragment.revision, fragment.row_count = patch.next, patch.next_rows
  return prepared
end

---@param session table
---@param prepared table Body metadata with positions resolved into the owning buffer.
function M.install_fragment_metadata(session, prepared)
  install_metadata(session, prepared, false)
end

return M
