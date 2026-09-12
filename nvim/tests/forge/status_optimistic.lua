vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local render = require("forge.status_render")
local input = require("forge.input")
local notices = {}
local replica = render.open("optimistic-render", { notice = function(message) notices[#notices + 1] = message end })
vim.api.nvim_win_set_buf(0, replica.buffer)

---@param id integer
---@param generation integer
---@param section string
---@return table
local function record(id, generation, section)
  return { id = id, generation = generation, section = section, change = "modified", path = id == 2 and "unrelated.lua" or "sample.lua", untracked = false, stats = { state = "exact", added = 1, deleted = 1 } }
end

---@param target string
---@return table
local function metadata(target)
  return { target = { { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } }, decoration = {}, editable_region = {} }
end

---@param id integer
---@param generation integer
---@param group string[]
---@return table
local function delivery(id, generation, group)
  local block = {}
  for _, name in ipairs(group) do
    local header = metadata("group:" .. name)
    header.fold = { { id = "header:" .. name, start = { row = 0, column = 0 }, ["end"] = { block = "body:" .. name, position = { row = 1, column = 0 } }, closed = false } }
    block[#block + 1] = { id = "header:" .. name, text = { "@@ " .. name }, metadata = header }
    block[#block + 1] = { id = "body:" .. name, text = { "changed " .. name }, metadata = metadata("hunk:" .. name) }
  end
  return { document = replica.document, file = id, generation = generation, more = false, state = { state = "ready" }, snapshot = { document = ("body:%d:%d"):format(id, generation), revision = 1, block = block } }
end

local snapshot = { document = replica.document, revision = 0, view = { kind = "status" }, head = { state = "attached", reference = "main", object = "abc" },
  section = { { kind = "unstaged", file = { 1, 2 } } }, file = { record(1, 1, "unstaged"), record(2, 1, "unstaged") }, pending = {} }
assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
local view = input.open(replica, vim.api.nvim_get_current_win(), { margin = 0 })
vim.cmd("4foldopen")
assert(render.apply_body(replica, delivery(1, 1, { "A", "B" })).kind == "Applied")
vim.api.nvim_win_set_cursor(0, { 7, 0 })
vim.cmd("7foldclose")
assert(vim.fn.foldclosed(7) == 7)
local writes = 0
vim.api.nvim_buf_attach(replica.buffer, false, { on_lines = function() writes = writes + 1 end })
local untouched = replica.file[2]
local accepted = { document = replica.document, operation_id = 0, phase = "accepted", diagnostic = {}, delta = { document = replica.document, base = 0, next = 1,
  removed = {}, file = { record(1, 2, "unstaged"), record(3, 1, "staged") }, section = { { kind = "staged", file = { 3 } } }, pending = { 0 } },
  body = { delivery(1, 2, { "B" }), delivery(3, 1, { "staged-A" }) } }
assert(render.apply_update(replica, accepted).kind == "Applied", table.concat(notices, "\n"))
assert(replica.file[2].record == untouched.record and replica.file[2].body == untouched.body)
assert(replica.inventory.pending[1] == 0)
local _, header_row = replica.sequence:position("header:B")
assert(vim.fn.foldclosed(header_row + 1) == header_row + 1, "surviving hunk lost its closed fold")
assert(vim.api.nvim_win_get_cursor(0)[1] == header_row + 1, "cursor left its surviving hunk")
local changedtick = vim.api.nvim_buf_get_changedtick(replica.buffer)
local previous_writes = writes
local settled = { document = replica.document, operation_id = 0, phase = "settled", diagnostic = {}, body = {}, delta = { document = replica.document,
  base = 1, next = 2, removed = {}, file = {}, section = {}, pending = {} } }
assert(render.apply_update(replica, settled).kind == "Applied")
assert(writes == previous_writes and vim.api.nvim_buf_get_changedtick(replica.buffer) == changedtick, "matching settlement rewrote the buffer")
assert(render.apply_update(replica, accepted).kind == "Discarded", "duplicate acceptance was applied twice")
local before = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
local invalid = vim.deepcopy(accepted)
invalid.delta.base, invalid.delta.next = 2, 3
invalid.body[1].generation = 99
assert(render.apply_update(replica, invalid).kind == "Desynchronized")
assert(vim.deep_equal(before, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)), "invalid atomic update changed text")
assert(replica.revision == 2 and #notices == 1)
input.close(view)
render.close(replica)
print("optimistic status rendering: passed")
