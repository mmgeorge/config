vim.loader.enable(false)
local client = require("forge.client")
local expanded, loaded, revision = false, 0, 0
local requests, notices, pending = {}, {}, {}
local total, hold = 300, false
local owner
local transcript = vim.api.nvim_create_buf(false, true)
local composer = vim.api.nvim_create_buf(false, true)
local window = vim.api.nvim_get_current_win()
vim.o.lines, vim.o.columns = 24, 100
vim.api.nvim_win_set_buf(window, transcript)

local function snapshot(document)
  local body = {}
  for row = 1, loaded do body[row] = ("source row %03d"):format(row) end
  local last = expanded and "file:deferred-body" or "empty"
  local blocks = {
    { id = "file", text = { "Modified source.rs" }, metadata = {
      node = { id = "file", kind = "file", lifecycle = "settled", generation = 1,
        content_revision = revision, loaded_rows = loaded, loaded_bytes = 0,
        display = expanded and "full" or "heading", default_display = "heading", expansion = expanded },
      section = { { id = "file", revision = revision, open = expanded, more = false } },
      fold = { { id = "file", start = { row = 0, column = 0 },
        ["end"] = { block = last, position = { row = 1, column = 0 } },
        closed = true, expand_children = true } },
    } },
  }
  if expanded then
    blocks[#blocks + 1] = { id = "hunk", text = { "@@ -1 +1 @@" }, metadata = {
      section = { { id = "hunk", revision = revision, open = true, more = false } },
      fold = { { id = "hunk", start = { row = 0, column = 0 },
        ["end"] = { block = "rows", position = { row = #body, column = 0 } }, closed = false } },
    } }
    blocks[#blocks + 1] = { id = "rows", text = body, metadata = {} }
    blocks[#blocks + 1] = { id = "file:deferred-body", text = { loaded < total and "More content available" or "End" }, metadata = {
      section = loaded < total and { { id = "file", revision = revision, open = true, more = true } } or {},
    } }
  else
    blocks[#blocks + 1] = { id = "empty", text = { "" }, metadata = {} }
  end
  for _, block in ipairs(blocks) do
    block.metadata.target = {}
    block.metadata.decoration = {}
    block.metadata.editable_region = {}
  end
  return { document = document, revision = revision, block = blocks }
end

local function deliver(params, callback)
  if params.operation == "node" then
    assert(params.node == "file", "hunk body caused a separate request")
    assert(params.rows == 2 * vim.api.nvim_win_get_height(window))
    assert(params.width.columns > 0)
    expanded = params.expanded
    if expanded then loaded = math.min(total, (params.action == "load_more") and loaded + params.rows or math.max(loaded, params.rows)) end
    revision = revision + 1
    callback({})
  elseif params.operation == "sync" then callback({ snapshot = snapshot(params.document) })
  else callback({}) end
end

client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  if params.operation == "background_terminals" then callback({ supported = false })
  elseif params.operation == "open" then
    vim.schedule(function() callback({ transcript = snapshot(params.document) }) end)
  else
    requests[#requests + 1] = params
    if hold and params.operation == "node" and (params.action == "load_more") then
      pending[#pending + 1] = { params = params, callback = callback }
    else deliver(params, callback) end
  end
end

local function settled()
  return not owner.syncing and not owner.applying and not owner.pending and not owner.transcript.update_pending
end
local function page_count()
  local count = 0
  for _, request in ipairs(requests) do
    if request.operation == "node" and (request.action == "load_more") then count = count + 1 end
  end
  return count
end
local ok, failure = xpcall(function()
  owner = require("forge.views.harness.presentation").open({
    session_id = "file-paging", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) notices[#notices + 1] = message end,
  }, function() end)
  assert(vim.wait(1000, function() return owner.ready end, 1), table.concat(notices, "\n"))
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return expanded and settled() and vim.fn.foldclosed(1) == -1 end, 1))
  local initial = loaded
  assert(initial == 2 * vim.api.nvim_win_get_height(window))
  for _ = 1, 5 do owner.observe_sections() end
  assert(page_count() == 0, "visible heading fetched another page")
  assert(vim.fn.foldclosed(2) == -1, "initial hunk was not opened with its file")

  hold = true
  vim.api.nvim_win_set_cursor(window, { math.floor(initial / 2) + 4, 0 })
  vim.cmd("normal! zt")
  local picker = vim.api.nvim_open_win(composer, true, {
    relative = "editor", row = 2, col = 2, width = 30, height = 4, style = "minimal",
  })
  owner.observe_sections()
  assert(#pending == 0, "approval picker triggered background paging")
  vim.api.nvim_win_close(picker, true)
  vim.api.nvim_set_current_win(window)
  owner.observe_sections()
  assert(#pending == 1, "one-screen lead did not request the next page")
  for _ = 1, 8 do owner.observe_sections() end
  assert(#pending == 1, "duplicate page requests while delivery was pending")
  local cursor = vim.api.nvim_win_get_cursor(window)
  local top = vim.fn.line("w0")
  local delivery = table.remove(pending, 1)
  deliver(delivery.params, delivery.callback)
  assert(vim.wait(1000, settled, 1))
  assert(loaded == initial + 2 * vim.api.nvim_win_get_height(window))
  assert(vim.deep_equal(cursor, vim.api.nvim_win_get_cursor(window)), "page delivery moved cursor")
  assert(top == vim.fn.line("w0"), "page delivery moved viewport")
  assert(page_count() == 1, "publication fetched another page away from the boundary")

  vim.api.nvim_win_set_cursor(window, { loaded - math.floor(initial / 2) + 4, 0 })
  vim.cmd("normal! zt")
  owner.observe_sections()
  assert(#pending == 1)
  vim.api.nvim_win_set_cursor(window, { 1, 0 })
  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, settled, 1))
  delivery = table.remove(pending, 1)
  -- A stale page response cannot reopen a fold the user has closed.
  delivery.callback({})
  assert(vim.wait(1000, settled, 1))
  assert(vim.fn.foldclosed(1) == 1)
  assert(#notices == 0, table.concat(notices, "\n"))

  assert(owner.toggle_heading(window))
  assert(vim.wait(1000, function() return settled() and vim.fn.foldclosed(1) == -1 end, 1))
  vim.api.nvim_win_set_cursor(window, { loaded - math.floor(initial / 2) + 4, 0 })
  vim.cmd("normal! zt")
  owner.observe_sections()
  assert(#pending == 1)
  delivery = table.remove(pending, 1)
  delivery.callback(nil, "section exceeds the 16 MiB loaded-body limit")
  local failed_pages = page_count()
  for _ = 1, 20 do owner.observe_sections() end
  assert(page_count() == failed_pages and #pending == 0, "failed page cascaded into automatic retries")
  assert(#notices == 1, table.concat(notices, "\n"))
  owner.set_section("file", true, false)
  assert(vim.wait(1000, settled, 1), "explicit retry was blocked")
end, debug.traceback)
if owner then owner.close() end
assert(ok, failure)
print("harness_file_paging: passed")
vim.cmd("qa!")
