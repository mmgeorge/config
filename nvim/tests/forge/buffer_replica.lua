vim.loader.enable(false)
local replica = require("forge.buffer")
local editable = require("forge.editable")
local recovery = 0
local session = replica.open("document", { recover = function() recovery = recovery + 1 end })

local function block(id, text)
  return { id = id, text = text, metadata = { target = {}, decoration = {}, editable_region = {} } }
end

local snapshot = { document = "document", revision = 0, block = { block("first", { "one" }), block("last", { "last" }) } }
snapshot.block[2].metadata.decoration = {
  { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 4 } }, capture = "String", priority = 10 },
}
local patch = {
  document = "document", base = 0, next = 1, base_rows = 2, next_rows = 3, base_blocks = 2, next_blocks = 2,
  text_edit = { { start_row = 0, removed_rows = 1, text = { "one", "new" } } },
  block_edit = {}, removed_block = {}, metadata_edit = {
    { block = "first", row_count = 2, metadata = block("first", {}).metadata },
  },
}

local ok, failure = xpcall(function()
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  local handle = session.marks.last[1]
  assert(replica.apply_patch(session, patch).kind == "Applied")
  assert(session.revision == 1 and not vim.bo[session.buffer].modifiable)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "one", "new", "last" }))
  assert(session.marks.last[1] == handle, "unchanged decoration was recreated")
  assert(vim.api.nvim_buf_get_extmark_by_id(session.buffer, session.namespace, handle, {})[1] == 2)
  assert(replica.apply_patch(session, patch).kind == "Desynchronized")
  assert(session.revision == 1 and recovery == 1)
  snapshot.revision = 2
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")

  local two_edits = vim.deepcopy(patch)
  two_edits.base, two_edits.next = 2, 3
  two_edits.next_rows = 2
  two_edits.metadata_edit = {}
  two_edits.text_edit = {
    { start_row = 1, removed_rows = 1, text = { "tail" } },
    { start_row = 0, removed_rows = 1, text = { "head" } },
  }
  local native_set_lines = vim.api.nvim_buf_set_lines
  local calls = 0
  vim.api.nvim_buf_set_lines = function(...)
    calls = calls + 1
    if calls == 2 then error("injected second-edit failure") end
    return native_set_lines(...)
  end
  local result = replica.apply_patch(session, two_edits)
  vim.api.nvim_buf_set_lines = native_set_lines
  assert(result.kind == "Desynchronized" and result.diagnostic:find("injected second-edit failure", 1, true))
  assert(session.revision == 2, "partial application acknowledged target revision")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "one", "tail" }))
  assert(#vim.api.nvim_buf_get_extmarks(session.buffer, session.namespace, 0, -1, {}) == 0)
  snapshot.revision = 4
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  editable.register(session.editable, "body", 0)
  editable.record(session.editable, "body", { "unsaved" })
  assert(replica.apply_snapshot(session, snapshot).kind == "Deferred")
  assert(replica.apply_patch(session, two_edits).kind == "Deferred")
  assert(replica.close(session).kind == "Deferred")
  assert(vim.api.nvim_buf_is_valid(session.buffer))
  session.editable = editable.new("document")

  snapshot.revision, snapshot.block = 5, {}
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
  local insertion = {
    document = "document", base = 5, next = 6, base_rows = 0, next_rows = 1, base_blocks = 0, next_blocks = 1,
    text_edit = { { start_row = 0, removed_rows = 0, text = { "first" } } },
    block_edit = { { start_block = 0, removed_blocks = 0, inserted = { "first" } } }, removed_block = {},
    metadata_edit = { { block = "first", row_count = 1, metadata = block("first", {}).metadata } },
  }
  assert(replica.apply_patch(session, insertion).kind == "Applied")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), { "first" }))
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_lines(session.buffer, 0, -1, true, { "external change" })
  insertion.base, insertion.next = 6, 7
  assert(replica.apply_patch(session, insertion).kind == "Desynchronized")
  assert(session.diagnostic:find("changedtick", 1, true))
  assert(session.revision == 6)

  replica.close(session)
  local sent = {}
  session = replica.open("editable", { editable = { send = function(request)
    sent[#sent + 1] = request
    return true
  end } })
  local source = block("body", { "original" })
  source.metadata.editable_region = {
    { id = "body", revision = 0, range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 8 } } },
  }
  local initial = { document = "editable", revision = 0, block = { source } }
  assert(replica.apply_snapshot(session, initial).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 0, 0, 0, 8, { "typed" })
  assert(editable.suspend_generated_text(session.editable))
  editable.flush(session.editable)
  assert(#sent == 1 and sent[1].text[1] == "typed")
  assert(editable.acknowledge(session.editable, {
    document = "editable", region = "body", sequence = sent[1].sequence, revision = 1,
  }))
  source.text = { "typed" }
  source.metadata.editable_region[1].revision = 1
  source.metadata.editable_region[1].range["end"].column = 5
  initial.revision = 1
  assert(replica.apply_snapshot(session, initial).kind == "Applied")
  assert(not editable.suspend_generated_text(session.editable))
  assert(session.editable.sequence == sent[1].sequence, "generated snapshot was recaptured as local typing")
  assert(not session.editable.fault)
  assert(session.editable.native and session.editable.native.active)
  assert(vim.api.nvim_buf_get_lines(session.buffer, 0, 1, true)[1] == "typed")
end, debug.traceback)

editable.detach(session.editable)
session.editable = editable.new(session.document)
replica.close(session)
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
