vim.loader.enable(false)
local fixture = dofile("nvim/tests/forge/support/status_fixture.lua")
local render = require("forge.status_render")
local nodes = require("forge.nodes")
local notices = {}
local replica = render.open("status-body-ordering", {
  notice = function(message) notices[#notices + 1] = message end,
})
vim.api.nvim_set_current_buf(replica.buffer)
local recovery_count = 0
replica.recover_body = function(file, generation)
  assert(file == 1 and generation == 2)
  recovery_count = recovery_count + 1
end
local function text()
  return vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)
end
---@param revision integer
---@param content string
---@return table
local function delivery(revision, content)
  local value = fixture.body(replica.document, { content }, { more = true })
  value.generation, value.snapshot.document, value.snapshot.revision = 2, "body:1:2", revision
  return value
end
---@param base integer
---@return table
local function patch_delivery(base)
  return { document = replica.document, file = 1, generation = 2, more = true,
    patch = { document = "body:1:2", base = base, next = base + 1 } }
end
---@param base integer
---@param body table
---@return table
local function update(base, body)
  local record = fixture.file()
  record.generation = 2
  return { document = replica.document, phase = "context", operation_id = 0,
    delta = { document = replica.document, base = base, next = base + 1,
      removed = {}, section = {}, file = { record }, pending = {} },
    body = { body } }
end

assert(render.apply_snapshot(replica, fixture.snapshot(replica.document)).kind == "Applied")
assert(nodes.set_open(replica, 0, "file:1", true))
assert(render.apply_body(replica, fixture.body(replica.document, { "initial source" }, { more = true })).kind == "Applied")
assert(render.apply_body(replica, patch_delivery(4)).kind == "Discarded",
  "future generation arrived before its inventory")
assert(render.apply_update(replica, update(0, delivery(4, "syntax source"))).kind == "Applied")
local before, changedtick = text(), replica.changedtick
assert(render.apply_body(replica, patch_delivery(5)).kind == "Recovering", "revision gap did not request recovery")
assert(replica.status == "Applied" and replica.file[1].body.revision == 4)
assert(recovery_count == 1 and #notices == 0)
assert(replica.changedtick == changedtick and vim.deep_equal(before, text()), "recovery changed the usable view")
assert(not nodes.closed(replica, "file:1"), "recovery collapsed the file")

assert(render.apply_snapshot(replica, replica.inventory).kind == "Applied")
assert(render.apply_body(replica, patch_delivery(6)).kind == "Recovering")
assert(recovery_count == 1, "inventory replacement lost in-flight body recovery")
assert(render.apply_body(replica, delivery(7, "recovered source")).kind == "Applied")
replica.file[1].recovering = nil
before, changedtick = text(), replica.changedtick
assert(render.apply_body(replica, patch_delivery(5)).kind == "Applied")
assert(render.apply_body(replica, delivery(6, "stale source")).kind == "Applied")
assert(replica.file[1].body.revision == 7 and replica.changedtick == changedtick)
assert(vim.deep_equal(before, text()), "late delivery replaced newer source")
assert(render.apply_update(replica, update(1, delivery(6, "stale event source"))).kind == "Applied")
assert(replica.file[1].body.revision == 7 and vim.deep_equal(before, text()),
  "late context event replaced a newer body snapshot")

local next_delivery = patch_delivery(7)
local patch = next_delivery.patch
patch.base_rows, patch.next_rows, patch.base_blocks, patch.next_blocks = 1, 1, 1, 1
patch.text_edit = { { start_row = 0, removed_rows = 1, text = { "continued source" } } }
patch.block_edit, patch.removed_block = {}, {}
patch.metadata_edit = { { block = "body:1", row_count = 1, metadata = fixture.metadata("hunk:1", 1) } }
assert(render.apply_body(replica, next_delivery).kind == "Applied")
assert(replica.file[1].body.revision == 8 and table.concat(text(), "\n"):find("continued source", 1, true))

local invalid = vim.deepcopy(next_delivery)
invalid.patch.base, invalid.patch.next, invalid.patch.base_rows = 8, 9, 2
before = text()
assert(render.apply_body(replica, invalid).kind == "Desynchronized")
assert(#notices == 1 and notices[1]:find("patch base counts differ", 1, true))
assert(recovery_count == 2 and vim.deep_equal(before, text()), "invalid patch replaced the prior view")
assert(render.apply_body(replica, delivery(8, "continued source")).kind == "Applied",
  "desynchronized body rejected its recovery snapshot")
assert(replica.status == "Applied" and not nodes.closed(replica, "file:1"))
render.close(replica)
print("status body ordering: revision gaps, late deliveries, and desynchronized snapshot recovery passed")
vim.cmd("qa!")
