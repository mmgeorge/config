local M = {}
local json_transfer = require("forge.json_transfer")
local MAX_COUNTER = 9007199254740991

local function counter(value, minimum, maximum)
  return type(value) == "number" and value >= minimum and value <= maximum and value == math.floor(value)
end

function M.new(document)
  return { document = document, latest = 0 }
end

function M.accept(state, part)
  if type(part) ~= "table" or part.document ~= state.document
    or not counter(part.transfer, 1, MAX_COUNTER)
    or not counter(part.revision, 0, MAX_COUNTER)
    or not json_transfer.valid(part)
  then
    state.active = nil
    return nil, "invalid snapshot part"
  end
  if part.transfer < state.latest or (part.transfer == state.latest and not state.active) then
    return nil, nil, "Obsolete"
  end
  if part.transfer > state.latest then
    state.latest = part.transfer
    state.active = nil
    if part.sequence ~= 0 then return nil, "snapshot transfer must begin at sequence zero" end
    state.active = {
      revision = part.revision, assembly = json_transfer.new(),
    }
  end
  local active = state.active
  if not active or part.revision ~= active.revision then
    state.active = nil
    return nil, "snapshot transfer order or totals differ"
  end
  local failure = json_transfer.accept(active.assembly, part)
  if failure then state.active = nil; return nil, failure end
  if active.assembly.sequence < active.assembly.part_count then return nil, nil, "Pending" end
  state.active = nil
  local encoded, incomplete = json_transfer.finish(active.assembly)
  if not encoded then return nil, incomplete end
  local ok, snapshot = pcall(vim.json.decode, encoded)
  if not ok or type(snapshot) ~= "table" or snapshot.document ~= state.document or snapshot.revision ~= active.revision then
    return nil, "snapshot payload identity or JSON differs"
  end
  return snapshot, nil, "Complete"
end

return M
