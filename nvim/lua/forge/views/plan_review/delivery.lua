local M = {}
local buffer = require("forge.buffer")

--- Retains validated source blocks between plan deliveries.
---@param state {fragment?: ForgeBufferFragment, anchor?: table<string, table>}
---@param delivery ForgePlanReviewUpdate
---@return table snapshot
---@return table[] source
---@return ForgeBufferFragment fragment
function M.apply(state, delivery)
  assert(type(delivery.source_edit) == "table", "Plan review source anchors are missing")
  local fragment = state.fragment
  if delivery.snapshot then
    assert(not fragment, "Plan review updates must use patches")
    fragment = buffer.fragment(delivery.snapshot, true)
  else
    assert(fragment, "Plan review initial snapshot is missing")
    local patch = delivery.patch
    if patch and patch ~= vim.NIL then
      local prepared = buffer.prepare_fragment_patch(fragment, patch)
      buffer.apply_fragment_patch(fragment, patch, prepared)
    end
  end
  local anchor = state.anchor or {}
  for _, id in ipairs(delivery.source_edit.remove) do anchor[id] = nil end
  for _, row in ipairs(delivery.source_edit.upsert) do anchor[row.id] = row end
  local snapshot = { document = fragment.document, revision = fragment.revision, block = {} }
  local source = {}
  fragment.sequence:visit_range(0, fragment.row_count, function(node)
    snapshot.block[#snapshot.block + 1] = { id = node.id, text = node.entry.text, metadata = node.entry.metadata }
    for position, text in ipairs(node.entry.text) do
      local id = node.id .. ":" .. (position - 1)
      local retained = assert(anchor[id], "Plan review source anchor is missing: " .. id)
      local target
      for _, candidate in ipairs(node.entry.metadata.target) do
        if candidate.range.start.row <= position - 1 and position - 1 <= candidate.range["end"].row
          and (position - 1 < candidate.range["end"].row or candidate.range["end"].column > 0)
        then target = candidate.id break end
      end
      source[#source + 1] = { id = id, text = text, block = node.id, position = { row = position - 1, column = 0 },
        metadata = node.entry.metadata, target = target, source_line = retained.source_line,
        test = retained.test, test_change = retained.test_change, test_file = retained.test_file,
        test_summary = retained.test_summary, check = retained.check }
    end
  end)
  state.fragment, state.anchor = fragment, anchor
  return snapshot, source, fragment
end

return M
