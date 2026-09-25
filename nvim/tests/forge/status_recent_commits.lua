vim.loader.enable(false)
local fixture = dofile("nvim/tests/forge/support/status_fixture.lua")
local status = require("forge.status")
local render = require("forge.status_render")
local history = require("forge.status_history")
package.loaded["forge.views.status.status_context"] = { attach = function()
  return { refresh = function() end, close = function() end, producer_handlers = function() return {} end }
end, producer_handlers = function() return {} end }
local notices, requests, closed, demand = {}, {}, {}, {}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local first, second, empty, failing = string.rep("a", 40), string.rep("b", 40), string.rep("c", 40), string.rep("d", 40)
local context = { workspace = vim.fn.getcwd(), branch = "main", issues = {}, recent = {
  { oid = first, reference = "main", subject = "Latest commit" },
  { oid = second, reference = "", subject = "Earlier commit" },
  { oid = empty, reference = "", subject = "Empty commit" },
  { oid = failing, reference = "", subject = "Failed comparison" },
} }
local pending
local state
status._set_runner_for_test(function(method, params, callback)
  requests[#requests + 1] = params.operation or method
  if params.operation == "open" then callback(fixture.snapshot(params.document, { context = context }))
  elseif params.operation == "comparison" then
    assert(params.worktree == false and params.reference:match("^%x+$"))
    local snapshot = fixture.snapshot(params.document, { view = { kind = "comparison", title = params.reference, worktree = false } })
    if params.reference == empty then snapshot.file, snapshot.section = {}, {} end
    if params.reference == failing then callback(nil, "comparison fixture failure")
    else pending = function() callback(snapshot) end end
  elseif params.operation == "demand" then
    demand[#demand + 1] = params.input
    assert(params.input.location.id == 1, "UI identity leaked to native comparison")
    callback(fixture.body(params.input.document, { "@@ -1 +1 @@", "old", "new" }, { block = "body:2", target = "hunk:3" }))
  elseif params.operation == "open_target" then
    assert(params.input.document == state.replica.commit[first].document)
    assert(params.input.location.block == "body:2" and params.input.location.target == "hunk:3")
    callback({ id = "effect", document = params.input.document, revision = params.input.revision,
      view = params.input.view, sequence = params.input.sequence, kind = "notify", message = "Historical source selected", level = 2 })
  elseif params.operation == "refresh" or params.operation == "close_view" then callback(nil)
  elseif params.operation == "close" then closed[params.document] = true callback({ closed = true })
  else error("unexpected request " .. vim.inspect(params)) end
end)
local function settle()
  assert(vim.wait(2000, function() return state.ready and not state.request_active and not state.scheduled end, 5), table.concat(notices, "\n"))
end
local function row(id) return select(2, state.replica.sequence:position(id)) + 1 end
local function toggle(id)
  vim.api.nvim_win_set_cursor(0, { row(id) + (id == "status:context:recent-title" and 1 or 0), 0 })
  vim.fn.maparg("<Tab>", "n", false, true).callback()
end
local function open_commit(oid)
  toggle("status:context:recent:" .. oid)
  assert(vim.wait(1000, function() return pending ~= nil end, 5), table.concat(notices, "\n"))
  local complete = pending
  pending = nil
  if oid == first then toggle("status:context:recent:" .. oid) end
  complete()
  settle()
  if oid == first then
    assert(vim.fn.foldclosed(row("status:context:recent:" .. oid)) == row("status:context:recent:" .. oid), "late files reopened a collapsed commit")
    toggle("status:context:recent:" .. oid)
    settle()
  end
end
state = status.open({ workspace = vim.fn.getcwd() })
settle()
assert(#requests == 1 and #demand == 0, "collapsed history loaded eagerly")
toggle("status:context:recent-title")
settle()
assert(#requests == 1, "opening history loaded all commits")
open_commit(first)
local first_key = history.file_key(state.replica.commit[first], 1)
assert(vim.fn.foldclosed(row("file:" .. first_key)) == row("file:" .. first_key), "commit file did not start closed")
assert(#demand == 0, "commit expansion eagerly loaded file source")
toggle("file:" .. first_key)
settle()
assert(#demand == 1 and state.replica.file[first_key].body)
local first_body = state.replica.file[first_key].body
local first_owner = state.replica.commit[first]
local patch = { document = "body:1:1", base = 1, next = 2, base_rows = 3, next_rows = 4, base_blocks = 1, next_blocks = 2,
  text_edit = { { start_row = 3, removed_rows = 0, text = { "additional context" } } },
  block_edit = { { start_block = 1, removed_blocks = 0, inserted = { "body:4" } } },
  metadata_edit = { { block = "body:4", row_count = 1, metadata = fixture.metadata("hunk:3", 1) } }, removed_block = {} }
assert(render.apply_body(state.replica, { document = first_owner.document, file = 1, generation = 1,
  patch = patch, more = false }).kind == "Applied", table.concat(notices, "\n"))
assert(state.replica.block[first_owner.prefix .. "body:4"].text[1] == "additional context")
assert(first_body.revision == 2, "incremental commit body did not advance")
open_commit(second)
local second_key = history.file_key(state.replica.commit[second], 1)
toggle("file:" .. second_key)
settle()
assert(#demand == 2 and state.replica.file[first_key].body == first_body, "another commit replaced the first diff")
assert(state.replica.file[1].body == nil, "historical body replaced working-tree body")
local first_block = state.replica.commit[first].prefix .. "body:2"
vim.api.nvim_win_set_cursor(0, { row(first_block), 0 })
status.action(state, "stage")
assert(notices[#notices] == "Historical commit diffs are read-only")
notices = {}
status.action(state, "open")
settle()
assert(notices[1] == "Historical source selected", "historical effect was discarded")
notices = {}
local selection_ok = pcall(render.selection, state.replica, row("file:1") - 1, row(first_block))
assert(not selection_ok, "mixed selection admitted historic mutation")
assert(render.apply_snapshot(state.replica, state.replica.inventory).kind == "Applied")
assert(state.replica.file[first_key].body == first_body)
assert(vim.fn.foldclosed(row("file:" .. first_key)) == -1, "refresh closed expanded diff")
local updated_record = vim.deepcopy(first_owner.snapshot.file[1])
updated_record.generation = 2
local updated_body = fixture.body(first_owner.document, { "syntax-enriched historical diff" })
updated_body.generation, updated_body.snapshot.document = 2, "body:1:2"
local revision = state.replica.revision
status.apply_update(state, { document = first_owner.document, operation_id = 0, phase = "context",
  delta = { document = first_owner.document, base = 0, next = 1, removed = {}, section = {}, file = { updated_record }, pending = {} },
  body = { updated_body } })
settle()
assert(first_owner.snapshot.revision == 1 and state.replica.revision == revision, "child context update changed root revision")
assert(state.replica.file[first_key].record.generation == 2 and state.replica.file[first_key].body)
assert(vim.fn.foldclosed(row("file:" .. first_key)) == -1, "context update collapsed historical file")
vim.api.nvim_win_set_cursor(0, { row("file:" .. first_key), 0 })
local captured = assert(render.capture(state.replica, state.view[vim.api.nvim_get_current_win()], "open"))
assert(captured.document == first_owner.document and captured.revision == 1 and captured.location.id == 1)
open_commit(empty)
assert(state.replica.block["status:context:recent:" .. empty .. ":placeholder"].text[1] == "No changed files")
toggle("status:context:recent:" .. failing)
settle()
assert(#notices == 1 and notices[1] == "comparison fixture failure")
assert(state.replica.block["status:context:recent:" .. failing .. ":placeholder"].text[1]:find("Unable to load commit", 1, true))
local request_count = #requests
status.demand(state)
settle()
assert(#requests == request_count, "failed comparison retried without user action")
toggle("status:context:recent:" .. failing)
toggle("status:context:recent:" .. failing)
settle()
assert(#notices == 2, "reopening failed comparison did not retry")
notices = {}
local old_document = state.replica.commit[second].document
local snapshot = vim.deepcopy(state.replica.inventory)
table.remove(snapshot.context.recent, 2)
assert(render.apply_snapshot(state.replica, snapshot).kind == "Applied")
status.demand(state)
settle()
assert(closed[old_document] and not state.replica.commit[second], "removed recent commit retained native document")
assert(render.apply_body(state.replica, fixture.body(old_document, { "stale" })).kind == "Discarded")
local remaining = state.replica.commit[first].document
status.close(state)
assert(vim.wait(2000, function() return not state.request_active end, 5))
assert(closed[remaining] and closed[state.document], "status close leaked comparison")
assert(#notices == 0, table.concat(notices, "\n"))
state = status.open({ workspace = vim.fn.getcwd() })
settle()
toggle("status:context:recent-title")
toggle("status:context:recent:" .. first)
assert(vim.wait(1000, function() return pending ~= nil end, 5))
local pending_document = state.replica.commit[first].document
status.close(state)
pending()
assert(vim.wait(2000, function() return not state.request_active end, 5))
assert(closed[pending_document], "closing during comparison open leaked native document")
assert(#notices == 0, table.concat(notices, "\n"))
status._set_runner_for_test(nil)
print("status recent commits: passed")
