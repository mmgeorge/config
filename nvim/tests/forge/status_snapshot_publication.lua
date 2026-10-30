vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local render = require("forge.status_render")
local fixture = dofile("nvim/tests/forge/support/status_fixture.lua")
local notices = {}

local function snapshot(replica, revision, section)
  local value = fixture.snapshot(replica.document, { revision = revision })
  value.section = { { kind = section or "unstaged", file = {} } }
  value.file = {}
  for ordinal = 1, 75 do
    local record = fixture.file(("source_%02d.lua"):format(ordinal), ordinal)
    record.section = section or "unstaged"
    value.file[ordinal], value.section[1].file[ordinal] = record, ordinal
  end
  return value
end

local function open(name)
  local replica = render.open(name, { notice = function(message) notices[#notices + 1] = message end })
  vim.api.nvim_set_current_buf(replica.buffer)
  assert(render.apply_snapshot(replica, snapshot(replica, 0)).kind == "Applied")
  return replica
end

local function paused_snapshot(replica, value)
  local original_clock, original_schedule = vim.uv.hrtime, vim.schedule
  local instant, continuation = original_clock(), nil
  local pending = { completed = 0 }
  vim.uv.hrtime = function() instant = instant + 5000000 return instant end
  vim.schedule = function(callback)
    assert(not continuation, "snapshot queued multiple continuations before yielding")
    continuation = callback
  end
  local success, failure = pcall(render.apply_snapshot, replica, value, function(result)
    pending.completed = pending.completed + 1
    pending.result = result
  end)
  vim.uv.hrtime, vim.schedule = original_clock, original_schedule
  assert(success, failure)
  assert(continuation and not pending.result, "snapshot did not pause during preparation")
  pending.resume = continuation
  return pending
end

local function stage_delta(replica)
  local staged = snapshot(replica, 1, "staged")
  return { document = replica.document, base = 0, next = 1, file = staged.file,
    removed = {}, pending = {}, section = {
      { kind = "unstaged", file = {} }, staged.section[1],
    } }
end

local function assert_preserved(replica, pending)
  local text = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local viewport = vim.fn.winsaveview()
  local changedtick, publication = replica.changedtick, replica.publication_revision
  pending.resume()
  assert(pending.completed == 1 and pending.result.kind == "Superseded", "outdated snapshot did not finish once")
  assert(replica.changedtick == changedtick and replica.publication_revision == publication,
    "outdated snapshot published another frame")
  assert(vim.deep_equal(text, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)), "outdated snapshot replaced text")
  assert(vim.deep_equal(cursor, vim.api.nvim_win_get_cursor(0)), "outdated snapshot moved the cursor")
  assert(viewport.topline == vim.fn.winsaveview().topline, "outdated snapshot moved the viewport")
end

for _, publication in ipairs({ "patch", "mutation" }) do
  local replica = open("snapshot-" .. publication)
  local pending = paused_snapshot(replica, snapshot(replica, 0))
  local delta = stage_delta(replica)
  local result = publication == "patch" and render.apply_patch(replica, delta)
    or render.apply_update(replica, { document = replica.document, delta = delta, body = {}, phase = "accepted" })
  assert(result.kind == "Applied")
  assert_preserved(replica, pending)
  assert(replica.inventory.revision == 1 and #replica.inventory.file == 75)
  assert(replica.inventory.section[1].kind == "staged", "staged section disappeared")
  render.close(replica)
end

for _, ahead in ipairs({ false, true }) do
  local replica = open(ahead and "snapshot-ahead" or "snapshot-body")
  assert(render.apply_body(replica, fixture.body(replica.document, { "original body" })).kind == "Applied")
  local pending = paused_snapshot(replica, snapshot(replica, ahead and 2 or 0))
  local body = fixture.body(replica.document, { "newer body" })
  body.snapshot.revision = 2
  assert(render.apply_body(replica, body).kind == "Applied")
  if ahead then
    pending.resume()
    assert(vim.wait(1000, function() return pending.result ~= nil end, 5), "needed snapshot was not prepared again")
    assert(pending.completed == 1 and pending.result.kind == "Applied")
    assert(replica.inventory.revision == 2, "needed inventory revision was discarded")
  else
    assert_preserved(replica, pending)
  end
  assert(replica.file[1].body.revision == 2, "snapshot replaced a newer diff body")
  render.close(replica)
end

local replica = open("snapshot-replaced")
local pending = paused_snapshot(replica, snapshot(replica, 1))
assert(render.apply_snapshot(replica, snapshot(replica, 2, "staged")).kind == "Applied")
assert_preserved(replica, pending)
assert(replica.inventory.revision == 2)
assert(render.apply_snapshot(replica, snapshot(replica, 0)).kind == "Superseded", "late snapshot was admitted")
render.close(replica)

replica = open("snapshot-closed")
pending = paused_snapshot(replica, snapshot(replica, 1))
render.close(replica)
pending.resume()
assert(pending.completed == 1 and pending.result.kind == "Closed", "closing lost the pending completion callback")
assert(#notices == 0, table.concat(notices, "\n"))
print("status snapshot publication: staging, patches, body updates, retry, replacement, and close passed")
vim.cmd("qa!")
