vim.o.columns = 160
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
package.path = root .. "/nvim/lua/?.lua;" .. root .. "/nvim/lua/?/init.lua;" .. package.path
local status = require("forge.status")
local render = require("forge.status_render")
local nodes = require("forge.nodes")
local artifact = root .. "/nvim/rust/forge/target/status-semantic-fixture.json"
local fixture = vim.json.decode(table.concat(vim.fn.readfile(artifact), "\n"))
local original_document = fixture.initial.document
local notices, controls, sections = {}, {}, {}
local full_delivered = {}
local delayed
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local function replace_document(value, document)
  if type(value) == "string" then return value:gsub(vim.pesc(original_document), document) end
  if type(value) ~= "table" then return value end
  local result = {}
  for key, entry in pairs(value) do result[key] = replace_document(entry, document) end
  return result
end
local function wait_for(predicate)
  assert(vim.wait(3000, predicate, 5), table.concat(notices, "\n"))
end
status._set_runner_for_test(function(method, params, callback)
  assert(method == "repository.status.document", method)
  if params.operation == "open" then
    fixture = replace_document(fixture, params.document)
    callback(fixture.initial)
  elseif params.operation == "demand" then
    for _, delivery in ipairs(fixture.full) do
      if delivery.document == params.input.document and delivery.file == params.input.location.id then
        if not full_delivered[delivery.document] then
          full_delivered[delivery.document] = true
          callback(delivery)
        else callback({ document = delivery.document, file = delivery.file, generation = delivery.generation, more = false, state = "ready" }) end
        return
      end
    end
    error("unexpected source demand: " .. vim.inspect(params.input))
  elseif params.operation == "diff_view" then
    controls[#controls + 1] = params
    assert(#params.related == 1 and params.related[1] == fixture.child_initial.document)
    local group = ({ fixture.semantic, fixture.public, fixture.public_full, fixture.all_full })[#controls]
    assert(group, "unexpected diff control request")
    if #controls == 1 then delayed = function() callback(group) end else callback(group) end
  elseif params.operation == "source_section" then
    sections[#sections + 1] = params.input
    assert(params.input.action == "source_expand")
    callback(fixture.expanded)
  elseif params.operation == "refresh" or params.operation == "close_view" then callback(nil)
  elseif params.operation == "close" then callback({ closed = true })
  else error("unexpected status request: " .. params.operation) end
end)
local state = status.open({ workspace = root })
wait_for(function() return state.ready and not state.request_active end)
local commit = fixture.child_initial.document:match(":commit:(.+)$")
local owner = require("forge.status_history").new(state.document, commit)
owner.snapshot = fixture.child_initial
state.replica.commit[commit] = owner
assert(render.apply_snapshot(state.replica, state.replica.inventory).kind == "Applied")
local parent_file = fixture.full[1].file
for _, delivery in ipairs(fixture.full) do
  assert(render.apply_body(state.replica, delivery).kind == "Applied", table.concat(notices, "\n"))
  full_delivered[delivery.document] = true
end
local child_file = require("forge.status_history").file_key(owner, fixture.full[2].file)
assert(nodes.set_open(state.replica, 0, "status:context:recent:" .. commit, true))
assert(nodes.set_open(state.replica, 0, "file:" .. parent_file, true))
assert(nodes.set_open(state.replica, 0, "file:" .. child_file, true))
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
local first_window = vim.fn.win_findbuf(state.replica.buffer)[1]
status.demand(state)
assert(vim.wait(3000, function() return state.view[first_window] and state.view[second_window] end, 5), vim.inspect({ view = state.view, windows = vim.fn.win_findbuf(state.replica.buffer), first = first_window, second = second_window, ready = state.ready, active = state.active, pending = state.pending, request = state.request_active, status = state.replica.status, notices = notices }))
assert(vim.fn.maparg("ov", "n", false, true).callback)
assert(vim.fn.maparg("ox", "n", false, true).callback)
vim.fn.maparg("ox", "n", false, true).callback()
wait_for(function() return delayed ~= nil end)
status.toggle_diff(state, "toggle_visibility")
status.toggle_diff(state, "toggle_visibility")
assert(state.replica.inventory.diff_view.presentation == "full", "uncommitted control changed the visible settings")
delayed()
wait_for(function() return not state.request_active and not state.diff_active end)
assert(#controls == 1, "cancelled rapid visibility toggles issued work")
assert(state.replica.inventory.diff_view.presentation == "semantic")
assert(owner.snapshot.diff_view.presentation == "semantic")
for _, window in ipairs(vim.fn.win_findbuf(state.replica.buffer)) do
  assert(vim.wo[window].winbar:find("Semantic", 1, true), vim.wo[window].winbar)
end
local parent_heading
for id, entry in pairs(state.replica.source_entry) do
  if state.replica.body_owner[id] == parent_file and entry.metadata.section and #entry.metadata.section > 0
    and entry.text[1]:find("pub fn visible()", 1, true) then parent_heading = id break end
end
assert(parent_heading, "semantic parent heading missing")
assert(nodes.closed(state.replica, parent_heading), "semantic body started open")
local _, row = state.replica.sequence:position(parent_heading)
vim.api.nvim_win_set_cursor(second_window, { row + 1, 0 })
vim.fn.maparg("<Tab>", "n", false, true).callback()
wait_for(function() return #sections == 1 and not state.request_active end)
assert(not nodes.closed(state.replica, parent_heading))
local text = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), "\n")
assert(text:find("before()", 1, true) and text:find("after()", 1, true), text)
local source_block
for id, entry in pairs(state.replica.source_entry) do
  if state.replica.body_owner[id] == parent_file and entry.text[1]:find("after()", 1, true) then source_block = id break end
end
assert(source_block)
local _, source_row = state.replica.sequence:position(source_block)
vim.api.nvim_win_set_cursor(second_window, { source_row + 1, 0 })
vim.fn.maparg("ov", "n", false, true).callback()
wait_for(function() return #controls == 2 and not state.request_active end)
assert(state.replica.inventory.diff_view.visibility == "public")
assert(owner.snapshot.diff_view.visibility == "public")
text = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), "\n")
assert(not text:find("hidden", 1, true) and not text:find("private_", 1, true), text)
local cursor = vim.api.nvim_win_get_cursor(second_window)
assert(render.locate(state.replica, cursor[1] - 1, cursor[2]).block == source_block, "visibility toggle moved the surviving source cursor")
vim.fn.maparg("ox", "n", false, true).callback()
wait_for(function() return #controls == 3 and not state.request_active end)
assert(state.replica.inventory.diff_view.presentation == "full" and state.replica.inventory.diff_view.visibility == "public")
assert(owner.snapshot.diff_view.presentation == "full")
vim.fn.maparg("ov", "n", false, true).callback()
wait_for(function() return #controls == 4 and not state.request_active end)
text = table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), "\n")
assert(text:find("private_after", 1, true) and text:find("private_before", 1, true), text)
assert(#notices == 0, table.concat(notices, "\n"))
status.close(state)
wait_for(function() return not state.request_active end)
status._set_runner_for_test(nil)
local replica = render.open(fixture.initial.document, { notice = function(message) notices[#notices + 1] = message end })
vim.api.nvim_win_set_buf(0, replica.buffer)
local child = require("forge.status_history").new(replica.document, commit)
child.snapshot = fixture.child_initial
replica.commit[commit] = child
assert(render.apply_snapshot(replica, fixture.initial).kind == "Applied")
for _, delivery in ipairs(fixture.full) do assert(render.apply_body(replica, delivery).kind == "Applied") end
local retained_parent = replica.file[parent_file].body
local retained_child = replica.file[child_file].body
local parent_revision, child_revision = retained_parent.revision, retained_child.revision
local before = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true)
local invalid = vim.deepcopy(fixture.semantic)
invalid[2].body[1].generation = invalid[2].body[1].generation + 1
assert(render.apply_updates(replica, invalid).kind == "Desynchronized")
assert(replica.file[parent_file].body == retained_parent and retained_parent.revision == parent_revision)
assert(replica.file[child_file].body == retained_child and retained_child.revision == child_revision)
assert(child.snapshot == fixture.child_initial, "failed transaction published a child snapshot")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, true), before), "failed transaction changed the usable buffer")
assert(#notices == 1)
render.close(replica)
print("shared status diff controls: passed")
