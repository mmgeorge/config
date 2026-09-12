local render = require("forge.status_render")
local context = require("forge.views.status.status_context")
local writer = require("forge.git.write")
local notifications = require("forge.infra.notifications")
local original_execute, original_error = writer.execute, notifications.error
local errors, pending, refreshes = {}, {}, 0
notifications.error = function(message) errors[#errors + 1] = message end
writer.execute = function(workspace, action, callback, progress)
  assert(workspace == "fixture")
  pending[#pending + 1] = { action = action, callback = callback, progress = progress }
end
local replica = render.open("remote-progress", { notice = function(message) error(message) end })
vim.api.nvim_set_current_buf(replica.buffer)
local info = { workspace = "fixture", branch = "main", head = { oid = string.rep("a", 40), reference = "main", subject = "initial" },
  recent = {}, issues = {} }
local snapshot = { document = replica.document, revision = 0, view = { kind = "status" },
  head = { state = "attached", reference = "refs/heads/main", object = "abc123" }, context = info,
  section = { { kind = "unstaged", file = { 1 } } },
  file = { { id = 1, generation = 1, section = "unstaged", change = "modified", path = "sample.lua",
    untracked = false, stats = { state = "exact", added = 2, deleted = 1 } } } }
assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
local view = require("forge.input").open(replica, vim.api.nvim_get_current_win())
local file_row = select(2, replica.sequence:position("file:1")) + 1
vim.cmd(file_row .. "foldopen")
assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1, more = false,
  state = { state = "ready" }, snapshot = { document = "body:1:1", revision = 1, block = {
    { id = "body:line", text = { "expanded source" }, metadata = { target = {}, decoration = {}, editable_region = {}, fold = {} } },
  } } }).kind == "Applied")
local body = replica.file[1].body
local owner = context.attach({ document_id = replica.document, workspace = "fixture",
  is_alive = function() return true end, capture_input = function() end, is_input_current = function() return true end,
  present = function(presentation) render.present_context(replica, presentation) end })
local handlers = context.producer_handlers({ workspace = "fixture", context = function() return owner end,
  is_alive = function() return true end, refresh_status = function() refreshes = refreshes + 1 end })
local function text() return table.concat(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false), "\n") end
local function expect(value)
  assert(vim.api.nvim_get_current_buf() == replica.buffer, "remote operation switched the Status buffer")
  assert(replica.status == "Applied", "remote progress desynchronized Status")
  assert(replica.file[1].body == body, "header progress discarded the expanded diff")
  assert(vim.fn.foldclosed(select(2, replica.sequence:position("file:1")) + 1) == -1, "header progress closed the expanded file")
  assert(text():find(value, 1, true), "missing progress: " .. value .. "\n" .. text())
end
local buffer_count = #vim.api.nvim_list_bufs()
local success, failure = xpcall(function()
  local before = text()
  handlers.push()
  expect("Push:   Pushing...")
  assert(#vim.api.nvim_list_bufs() == buffer_count, "push created a scratch console")
  handlers.push()
  handlers.pull()
  assert(#pending == 1, "concurrent remote action admitted")
  pending[1].progress("Enumerating objects: 200, done.\rWriting objects: 42% (84/200), 1.2 MiB | 2.0 MiB/s\r")
  expect("Writing objects: 42% (84/200)")
  local root = replica.root
  pending[1].progress("Writing objects: 80% (160/200)\n")
  expect("Writing objects: 80% (160/200)")
  pending[1].progress("Writing objects: 100% (200/200), done.\nTotal 200 (delta 20), reused 0 (delta 0)\n")
  expect("Writing objects: 100% (200/200)")
  pending[1].progress("Total 200 (delta 20), reused 0 (delta 0)\n")
  expect("Writing objects: 100% (200/200)")
  assert(replica.root == root, "progress rebuilt the inventory after row admission")
  pending[1].callback({ ok = true })
  assert(text() == before and refreshes == 1, "success failed to clear temporary progress")
  pending[1].progress("late progress")
  assert(text() == before, "late progress reappeared after completion")
  info.upstream = { oid = string.rep("b", 40), reference = "origin/main", subject = "upstream" }
  info.push = info.upstream
  assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
  before = text()
  root = replica.root
  handlers.pull()
  expect("Merge:  Pulling...")
  assert(replica.root == root, "existing remote row rebuilt the inventory")
  pending[2].progress("Receiving objects: 75% (150/200)\rResolving deltas: 20% (4/20)\n")
  expect("Resolving deltas: 20% (4/20)")
  pending[2].callback({ ok = false, output = "remote rejected the operation" })
  assert(text() == before and refreshes == 2, "failure failed to restore remote reference")
  assert(#errors == 1 and errors[1]:find("remote rejected", 1, true), "failure omitted notification")
  handlers.push()
  expect("Pushing...")
  pending[2].progress("stale pull")
  expect("Pushing...")
  owner.close()
  local closed_text = text()
  pending[3].progress("Writing objects: 99%")
  pending[3].callback({ ok = true })
  assert(text() == closed_text, "closed context adopted late progress")
  assert(#vim.api.nvim_list_bufs() == buffer_count, "remote operations created a scratch console")
end, debug.traceback)
writer.execute, notifications.error = original_execute, original_error
require("forge.input").close(view)
render.close(replica)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("status_remote_progress OK")
vim.cmd("qa!")
