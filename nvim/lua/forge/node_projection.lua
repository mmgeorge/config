local M = {}
local cooperative = require("forge.cooperative")

-- Source blocks remain outside Neovim. Only this projection contributes buffer rows.
local function clipped(values, mapping)
  local result = {}
  for _, value in ipairs(values or {}) do
    local range = value.range
    if range then
      for _, span in ipairs(mapping) do
        local first = math.max(range.start.row, span.first)
        local last = math.min(range["end"].row, span.last)
        local last_column = range["end"].row <= span.last and range["end"].column or 0
        if first < last or first == last and last_column > 0 and first < span.last then
          local copy = vim.tbl_extend("force", {}, value)
          copy.range = { start = { row = span.offset + first - span.first,
            column = first == range.start.row and range.start.column or 0 },
            ["end"] = { row = span.offset + last - span.first, column = last_column } }
          result[#result + 1] = copy
        end
      end
    end
  end
  return result
end

local function mapped_row(mapping, row)
  for _, span in ipairs(mapping) do
    if row >= span.first and row < span.last then return span.offset + row - span.first end
  end
end

function M.source_position(session, block, position)
  local mapping = session.projection and session.projection.mapping[block]
  if not mapping then return position end
  for _, span in ipairs(mapping) do
    if position.row >= span.offset and position.row < span.offset + span.last - span.first then
      return { row = span.first + position.row - span.offset, column = position.column }
    end
  end
  return position
end

function M.contains(owner, block, row)
  local mapping = owner.mapping[block]
  return mapping ~= nil and mapped_row(mapping, row) ~= nil
end

function M.visible_position(mapping, block, row)
  local previous
  for _, span in ipairs(mapping[block] or {}) do
    if row >= span.first and row < span.last then return span.offset + row - span.first end
    if row < span.first then return previous or span.offset end
    previous = span.offset + span.last - span.first - 1
  end
  return previous
end

local function project(owner)
  local source = owner.source
  local hidden, ranges = {}, {}
  for id, record in pairs(source.fold.record) do
    local _, start = source.sequence:position(record.owner)
    local _, finish = source.sequence:position(record.fold["end"].block)
    local first = start + record.fold.start.row
    local last = finish + record.fold["end"].position.row
      + (record.fold["end"].position.column > 0 and 1 or 0)
    local state = source.block[record.owner].metadata.node
    local closed = state and state ~= vim.NIL and state.id == id and state.display == "heading"
    if not state or state == vim.NIL or state.id ~= id then
      closed = owner.choice[id]
      if closed == nil then closed = record.fold.closed end
    end
    ranges[id] = { first = first, last = last, closed = closed, record = record }
    if closed and first + 1 < last then hidden[#hidden + 1] = { first = first + 1, last = last } end
    cooperative.checkpoint()
  end
  table.sort(hidden, function(left, right) return left.first < right.first end)
  local merged = {}
  for _, range in ipairs(hidden) do
    local previous = merged[#merged]
    if previous and range.first <= previous.last then previous.last = math.max(previous.last, range.last)
    else merged[#merged + 1] = range end
  end
  local visible, mapping, hidden_index, index = {}, {}, 1, 0
  while index < source.sequence:count() do
    local node = source.sequence:at(index)
    local entry, spans, offset = node.entry, {}, 0
    local _, first = source.sequence:position(node.id)
    local last, cursor = first + entry.row_count, first
    while merged[hidden_index] and merged[hidden_index].last <= cursor do hidden_index = hidden_index + 1 end
    local current = hidden_index
    while cursor < last do
      local hidden_range = merged[current]
      if not hidden_range or hidden_range.first >= last then
        spans[#spans + 1] = { first = cursor - first, last = last - first, offset = offset }
        break
      end
      if cursor < hidden_range.first then
        spans[#spans + 1] = { first = cursor - first, last = hidden_range.first - first, offset = offset }
        offset = offset + hidden_range.first - cursor
      end
      cursor = math.max(cursor, hidden_range.last)
      current = current + 1
    end
    if #spans > 0 then
      mapping[node.id] = spans
      visible[#visible + 1] = { id = node.id, entry = entry, spans = spans, first = first, fold = {} }
    end
    local covering = merged[hidden_index]
    if covering and first >= covering.first and last < covering.last then
      local next_node = source.sequence:locate(covering.last)
      index = next_node and select(1, source.sequence:position(next_node.id)) or source.sequence:count()
    else index = index + 1 end
    cooperative.checkpoint()
  end
  local by_id = {}
  for _, item in ipairs(visible) do by_id[item.id] = item end
  local function anchor(source_row)
    local low, high, found = 1, #visible, nil
    while low <= high do
      local middle = math.floor((low + high) / 2)
      if visible[middle].first <= source_row then found, low = visible[middle], middle + 1
      else high = middle - 1 end
    end
    if not found then return nil end
    local mapped = mapped_row(found.spans, source_row - found.first)
    local last = found.spans[#found.spans]
    return { block = found.id, position = { row = mapped or last.offset + last.last - last.first, column = 0 } }
  end
  for _, range in pairs(ranges) do
    local record = range.record
    local item = by_id[record.owner]
    local start = item and mapped_row(item.spans, record.fold.start.row)
    if start then
      local fold = vim.tbl_extend("force", {}, record.fold)
      fold.start = { row = start, column = record.fold.start.column }
      fold["end"] = range.closed and { block = record.owner, position = { row = start + 1, column = 0 } }
        or anchor(range.last)
      fold.closed = range.closed
      if fold.heading_start then
        local _, heading = source.sequence:position(fold.heading_start.block)
        fold.heading_start = anchor(heading + fold.heading_start.position.row)
      end
      item.fold[#item.fold + 1] = fold
    end
  end
  local blocks, next_cache = {}, {}
  for _, item in ipairs(visible) do
    table.sort(item.fold, function(left, right) return left.id < right.id end)
    local entry, spans, cached = item.entry, item.spans, owner.cache[item.id]
    local projected
    if cached and cached.entry == entry and vim.deep_equal(cached.spans, spans) and vim.deep_equal(cached.fold, item.fold) then
      projected = cached.block
    else
      local text
      if #spans == 1 and spans[1].first == 0 and spans[1].last == entry.row_count then text = entry.text
      else
        text = {}
        for _, span in ipairs(spans) do
          for source_row = span.first, span.last - 1 do text[#text + 1] = entry.text[source_row + 1] end
        end
      end
      local metadata = vim.tbl_extend("force", {}, entry.metadata)
      for _, name in ipairs({ "target", "decoration", "visible_decoration", "source_highlight", "conceal", "source_overlay", "editable_region" }) do
        metadata[name] = clipped(entry.metadata[name], spans)
      end
      metadata.gutter, metadata.fold = {}, item.fold
      for _, gutter in ipairs(entry.metadata.gutter or {}) do
        local target = mapped_row(spans, gutter.position.row)
        if target then
          local copy = vim.tbl_extend("force", {}, gutter)
          copy.position = { row = target, column = gutter.position.column }
          metadata.gutter[#metadata.gutter + 1] = copy
        end
      end
      for _, fold in ipairs(item.fold) do
        local marker = metadata.node_marker
        if marker then
          local marker_row = mapped_row(spans, marker.row)
          if marker_row then
            metadata.gutter[#metadata.gutter + 1] = {
              position = { row = marker_row, column = 0 }, placement = "sign", priority = 200,
              chunk = { { text = fold.closed and "▸" or "▾", capture = marker.capture } },
            }
          end
        end
        if fold.closed and fold.collapsed_suffix then
          if text == entry.text then text = vim.list_slice(text) end
          text[fold.start.row + 1] = text[fold.start.row + 1] .. fold.collapsed_suffix
        end
      end
      projected = { id = item.id, text = text, metadata = metadata }
    end
    blocks[#blocks + 1] = projected
    next_cache[item.id] = { entry = entry, spans = spans, fold = item.fold, block = projected }
    cooperative.checkpoint()
  end
  owner.cache = next_cache
  return { document = source.document, revision = source.revision, block = blocks }, mapping
end

function M.new(source)
  return { source = source, choice = {}, mapping = {}, blocks = {}, cache = {} }
end

function M.render(owner)
  return project(owner)
end

-- The patch retains unchanged blocks and text. Expansion replaces only the affected visible span.
function M.patch(session, snapshot)
  local blocks, before = snapshot.block, session.projection.blocks
  local prefix = 0
  while prefix < #before and prefix < #blocks and vim.deep_equal(before[prefix + 1], blocks[prefix + 1]) do prefix = prefix + 1 end
  local suffix = 0
  while suffix < #before - prefix and suffix < #blocks - prefix
    and vim.deep_equal(before[#before - suffix], blocks[#blocks - suffix]) do suffix = suffix + 1 end
  local start, removed, inserted, metadata, retired, text, old_count, new_count = 0, 0, {}, {}, {}, {}, 0, 0
  for index, block in ipairs(before) do
    old_count = old_count + #block.text
    if index <= prefix then start = start + #block.text end
    if index > prefix and index <= #before - suffix then removed = removed + #block.text end
  end
  local present = {}
  for _, block in ipairs(blocks) do present[block.id] = true new_count = new_count + #block.text end
  for index = prefix + 1, #before - suffix do
    if not present[before[index].id] then retired[#retired + 1] = before[index].id end
  end
  for index = prefix + 1, #blocks - suffix do
    local block = blocks[index]
    inserted[#inserted + 1] = block.id
    metadata[#metadata + 1] = { block = block.id, row_count = #block.text, metadata = block.metadata }
    vim.list_extend(text, block.text)
  end
  return { document = snapshot.document, base = session.revision, next = snapshot.revision,
    base_rows = old_count, next_rows = new_count, base_blocks = #before, next_blocks = #blocks,
    text_edit = (#metadata > 0 or removed > 0) and { { start_row = start, removed_rows = removed, text = text } } or {},
    block_edit = { { start_block = prefix, removed_blocks = #before - prefix - suffix, inserted = inserted } },
    metadata_edit = metadata, removed_block = retired }
end

return M
