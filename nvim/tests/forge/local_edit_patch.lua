vim.loader.enable(false)
local replica = require("forge.buffer")
local editable = require("forge.editable")
local sent = {}
local session = replica.open("edit-adoption", { editable = {
  delay = 1000, max_delay = 1000, send = function(request) sent[#sent + 1] = request return true end,
} })
local function metadata(region, revision, text)
  return { target = {}, decoration = {}, editable_region = region and {
    { id = region, revision = revision, range = { start = { row = 0, column = 0 },
      ["end"] = { row = #text - 1, column = #text[#text] } } },
  } or {} }
end
local function patch(base, region, region_revision, start, old_rows, text, total)
  return { document = session.document, base = base, next = base + 1,
    base_rows = total, next_rows = total + #text - old_rows, base_blocks = 4, next_blocks = 4,
    block_edit = {}, removed_block = {},
    text_edit = { { start_row = start, removed_rows = old_rows, text = text } },
    metadata_edit = { { block = region, row_count = #text, metadata = metadata(region, region_revision, text) } } }
end
local function acknowledge(request, revision, update)
  return replica.acknowledge_edit(session, { document = session.document, region = request.region,
    sequence = request.sequence, revision = revision }, update)
end
local ok, failure = xpcall(function()
  assert(replica.apply_snapshot(session, { document = session.document, revision = 0, block = {
    { id = "header", text = { "read only" }, metadata = metadata() },
    { id = "body", text = { "body" }, metadata = metadata("body", 0, { "body" }) },
    { id = "other", text = { "other" }, metadata = metadata("other", 0, { "other" }) },
    { id = "footer", text = { "tail" }, metadata = metadata() },
  } }).kind == "Applied")
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 1, 0, 1, 4, { "first", "line" })
  editable.flush(session.editable)
  local first = sent[#sent]
  vim.api.nvim_buf_set_text(session.buffer, 2, 4, 2, 4, { " newer" })
  local native_tick = vim.api.nvim_buf_get_changedtick(session.buffer)
  assert(acknowledge(first, 1, patch(0, "body", 1, 1, 1, { "first", "line" }, 4)).kind == "Deferred")
  assert(vim.api.nvim_buf_get_changedtick(session.buffer) == native_tick)
  assert(vim.api.nvim_buf_get_lines(session.buffer, 2, 3, true)[1] == "line newer")
  editable.flush(session.editable)
  local second = sent[#sent]
  vim.api.nvim_buf_set_text(session.buffer, 3, 0, 3, 5, { "other final" })
  editable.flush(session.editable)
  local third = sent[#sent]
  assert(third.region == "other")
  native_tick = vim.api.nvim_buf_get_changedtick(session.buffer)
  assert(acknowledge(third, 1, patch(2, "other", 1, 3, 1, { "other final" }, 5)).kind == "Deferred")
  local result = acknowledge(second, 2, patch(1, "body", 2, 1, 2, { "first", "line newer" }, 5))
  assert(result.kind == "Applied", vim.inspect(result))
  assert(session.revision == 3 and session.row_count == 5)
  assert(vim.api.nvim_buf_get_changedtick(session.buffer) == native_tick, "adoption rewrote native text")
  assert(not session.editable.suspended and not session.local_patch)
  assert(session.sequence:locate(4).id == "footer")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true),
    { "read only", "first", "line newer", "other final", "tail" }))
  vim.bo[session.buffer].modifiable = true
  vim.api.nvim_buf_set_text(session.buffer, 2, 10, 2, 10, { "!" })
  assert(not session.editable.fault, session.editable.fault)
  editable.flush(session.editable)
  local last = sent[#sent]
  assert(last.base == 2 and last.region == "body", "adoption lost editable attachment")
  result = acknowledge(last, 3, patch(3, "body", 3, 1, 2, { "first", "wrong" }, 5))
  assert(result.kind == "Desynchronized", "mismatched accepted source was adopted")
  assert(vim.api.nvim_buf_get_lines(session.buffer, 2, 3, true)[1] == "line newer!")
  local recovery = { document = session.document, revision = 4, block = {
    { id = "header", text = { "read only" }, metadata = metadata() },
    { id = "body", text = { "first", "stale value" }, metadata = metadata("body", 3, { "first", "stale value" }) },
    { id = "other", text = { "other final" }, metadata = metadata("other", 1, { "other final" }) },
    { id = "footer", text = { "tail" }, metadata = metadata() },
  } }
  assert(replica.apply_snapshot(session, recovery).kind == "Desynchronized", "snapshot erased acknowledged source")
  assert(vim.api.nvim_buf_get_lines(session.buffer, 2, 3, true)[1] == "line newer!")
  recovery.block[2].text = { "first", "line newer!" }
  recovery.block[2].metadata = metadata("body", 3, recovery.block[2].text)
  assert(replica.apply_snapshot(session, recovery).kind == "Applied", "exact source recovery failed")
end, debug.traceback)
replica.close(session)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("local_edit_patch OK")
vim.cmd("qa!")
