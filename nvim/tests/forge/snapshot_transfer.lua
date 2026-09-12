vim.loader.enable(false)
local replica = require("forge.buffer")
local transfer = require("forge.snapshot")
local session = replica.open("document")

local function snapshot(revision, text)
  return { document = "document", revision = revision, block = {
    { id = "body", text = { text }, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }
end

local function parts(value, identity)
  local encoded = vim.json.encode(value)
  local midpoint = math.floor(#encoded / 2)
  local result = {}
  for sequence, payload in ipairs({ encoded:sub(1, midpoint), encoded:sub(midpoint + 1) }) do
    result[sequence] = {
      document = "document", revision = value.revision, transfer = identity, sequence = sequence - 1,
      part_count = 2, total_bytes = #encoded, payload = payload,
    }
  end
  return result
end

local ok, failure = xpcall(function()
  assert(replica.apply_snapshot(session, snapshot(0, "original")).kind == "Applied")
  local first = parts(snapshot(1, "complete"), 1)
  local changedtick = session.changedtick
  assert(replica.apply_snapshot_part(session, first[1]).kind == "Pending")
  assert(session.revision == 0 and session.changedtick == changedtick)
  assert(vim.api.nvim_buf_get_lines(session.buffer, 0, 1, true)[1] == "original")
  assert(replica.apply_snapshot_part(session, first[2]).kind == "Applied")
  assert(session.revision == 1 and session.transfer.active == nil)
  assert(vim.api.nvim_buf_get_lines(session.buffer, 0, 1, true)[1] == "complete")
  assert(replica.apply_snapshot_part(session, first[2]).kind == "Obsolete")
  local obsolete = parts(snapshot(2, "obsolete"), 2)
  local newer = parts(snapshot(3, "newer"), 3)
  assert(replica.apply_snapshot_part(session, obsolete[1]).kind == "Pending")
  assert(replica.apply_snapshot_part(session, newer[1]).kind == "Pending")
  assert(replica.apply_snapshot_part(session, obsolete[2]).kind == "Obsolete")
  assert(replica.apply_snapshot_part(session, newer[2]).kind == "Applied")
  assert(session.revision == 3)
  local invalid = parts(snapshot(4, "bad"), 4)
  assert(replica.apply_snapshot_part(session, invalid[2]).kind == "Desynchronized")
  assert(session.revision == 3 and session.transfer.active == nil)

  for mode = 1, 5 do
    local state = transfer.new("document")
    local candidate = parts(snapshot(1, "test"), 1)
    assert(select(3, transfer.accept(state, candidate[1])) == "Pending")
    if mode == 1 then candidate[2].sequence = 0
    elseif mode == 2 then candidate[2].total_bytes = candidate[2].total_bytes + 1
    elseif mode == 3 then candidate[2].revision = 2
    elseif mode == 4 then candidate[2].payload = candidate[2].payload .. "extra"
    else candidate[2].payload = string.rep("x", 256 * 1024 + 1) end
    local complete, error = transfer.accept(state, candidate[2])
    assert(not complete and error and state.active == nil)
  end
end, debug.traceback)

replica.close(session)
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
