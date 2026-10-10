vim.loader.enable(false)
local replica = require("forge.buffer")
local session = replica.open("node-contract")
local function node(id, parent, last)
  return { id = id, text = { id }, metadata = { target = {}, decoration = {}, editable_region = {},
    node = { id = id, parent = parent, kind = "group", order = 0, lifecycle = "settled", more = false,
      default_display = "full", display = "full", generation = 1, content_revision = 1, loaded_rows = 1, loaded_bytes = 1 },
    fold = { { id = id, start = { row = 0, column = 0 },
      ["end"] = { block = last, position = { row = 1, column = 0 } }, closed = false } },
  } }
end
local snapshot = { document = "node-contract", revision = 0, block = {
  node("exchange", nil, "output"), node("tools", "exchange", "output"),
  { id = "output", text = { "output" }, metadata = { target = {}, decoration = {}, editable_region = {} } },
} }
local function recover()
  snapshot.revision = snapshot.revision + 1
  assert(replica.apply_snapshot(session, snapshot).kind == "Applied")
end
local function reject(metadata, expected)
  local text = vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true)
  local revision = session.revision
  local patch = { document = session.document, base = revision, next = revision + 1,
    base_rows = 3, next_rows = 3, base_blocks = 3, next_blocks = 3,
    text_edit = {}, block_edit = {}, removed_block = {}, metadata_edit = metadata }
  local result = replica.apply_patch(session, patch)
  assert(result.kind == "Desynchronized" and result.diagnostic:find(expected, 1, true), result.diagnostic)
  assert(session.revision == revision and vim.deep_equal(text, vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true)))
  recover()
end
local function edit(index, change)
  local entry = snapshot.block[index]
  local metadata = vim.deepcopy(entry.metadata)
  change(metadata)
  return { block = entry.id, row_count = 1, metadata = metadata }
end
local ok, failure = xpcall(function()
  recover()
  reject({ edit(1, function(metadata) metadata.fold[1]["end"].block = "tools" end) }, "beyond parent")
  reject({ edit(2, function(metadata) metadata.node.parent = "missing" end) }, "parent is absent")
  reject({ edit(1, function(metadata) metadata.node.parent = "tools" end) }, "document node parent")
  reject({ edit(2, function(metadata) metadata.node.id = "exchange" end) }, "duplicate document node")
  assert(session.node_child.exchange.tools and session.node_owner.tools == "tools")
end, debug.traceback)
replica.close(session)
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
