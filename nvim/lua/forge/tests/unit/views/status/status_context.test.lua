-- Unit tests for `forge.views.status.status_context` (status_context.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "status_context", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local ai = require("forge.integrations.ai_commit")
local gh = require("forge.integrations.gh")
local config = require("forge.infra.config")
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
local original_notify = vim.notify
local errors = {}
local notices = {}
notifications.error = function(message) errors[#errors + 1] = message end
vim.notify = function(message) notices[#notices + 1] = message end
local original_auto = config.options.about_auto_generate
local original_lookup, original_lookup_delay = config.options.pr_lookup_mode, config.options.pr_mock_delay_ms
assert(config.options.about_auto_generate ~= false, "automatic About must be enabled by default")
local original_request, original_ai, original_clear, original_pr = client.request, ai.ensure, ai.clear, gh.current_pr_async
local ok, failure = xpcall(function()
  local updates, generation, pull_request, presentation = {}, {}, {}, {}
  local about_source = { head = "old-head", file = { { id = 1, generation = 1, section = "unstaged" } } }
  local cleared = 0
  client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

    assert(method == "repository.status.context")
    updates[#updates + 1] = { params = params, callback = callback }
  end
  ai.ensure = function(_, options, callback)
    assert(options.ignored_paths[1] == "excluded.txt", "About omitted Status Ignore selection")
    generation[#generation + 1] = callback
  end
  ai.clear = function() cleared = cleared + 1 end
  gh.current_pr_async = function(_, callback) pull_request[#pull_request + 1] = callback end
  local applied, opened, current = 0, nil, true
  local owner = require("forge.views.status.status_context").attach({ document_id = "fixture", workspace = "fixture", window = vim.api.nvim_get_current_win(),
    capture_input = function(target) return { document = "fixture", target = target, revision = 1, sequence = 1, view = "view" } end,
    is_input_current = function() return current end,
    is_alive = function() return true end,
    get_info = function() return { workspace = "fixture" } end,
    get_about_source = function() return vim.deepcopy(about_source) end,
    ignored_paths = function() return { "excluded.txt" } end,
    present = function(value) applied = applied + 1; presentation[#presentation + 1] = value end,
    open_commit = function(action) opened = action.oid end })
  owner.refresh({ info_loaded = true })
  assert(#generation == 1, "default Status delayed generation behind a timer or PR response")
  owner.refresh({ info_loaded = true })
  assert(#updates == 0 and applied == 2, "presentation made a host round trip")
  assert(#generation == 1, "status refresh restarted the draft")
  pull_request[1]({ ok = true, pr = { title = "stale PR" } })
  assert(owner.pr == nil, "stale PR callback was adopted")
  generation[1]({ state = "ready", message = "current message" })
  pull_request[2]({ ok = true, pr = { title = "current PR" } })
  assert(#updates == 0 and applied == 4)
  assert(presentation[#presentation].about.text == "current message")
  assert(presentation[#presentation].pr.text == "current PR")
  owner.refocus()
  assert(#generation == 1, "unchanged refocus restarted About")
  about_source.file[1].generation = 2
  owner.refocus()
  owner.refocus()
  assert(#generation == 2, "changed refocus did not generate exactly once")
  generation[2]({ state = "ready", message = "updated draft" })
  about_source.head = "new-head"
  owner.refocus()
  assert(#generation == 3, "HEAD change did not regenerate About")
  about_source.file = {}
  owner.refocus()
  assert(cleared == 1 and presentation[#presentation].about.state == "none", "clean refocus retained About")
  generation[3]({ state = "ready", message = "obsolete draft" })
  assert(presentation[#presentation].about.text == "", "late generation restored a cleared About")
  owner.activate("status:context:head")
  updates[1].callback({ kind = "commit", oid = string.rep("a", 40) })
  assert(opened == string.rep("a", 40))
  owner.activate("status:context:head")
  current = false
  updates[2].callback({ kind = "commit", oid = string.rep("b", 40) })
  assert(opened == string.rep("a", 40), "stale action response opened another commit")
  config.options.about_auto_generate = false
  local generation_count = #generation
  owner.refresh({ info_loaded = true })
  assert(#generation == generation_count, "disabled automatic About started generation")
  owner.generate_about(true)
  assert(#generation == generation_count + 1, "explicit About did not generate when automatic mode was disabled")
  about_source.file = { { id = 2, generation = 1, section = "staged" } }
  owner.refocus()
  assert(#generation == generation_count + 1, "disabled automatic About regenerated on refocus")
  config.options.pr_lookup_mode, config.options.pr_mock_delay_ms = "mock-delay", 1000
  local lookup_count = #pull_request
  owner.refresh({ info_loaded = true })
  assert(#pull_request == lookup_count and owner.pr_timer, "mock PR lookup contacted GitHub")
  owner.generate_about(true)
  assert(owner.pr_timer, "About generation cancelled the independent mock PR timer")
  local pending_about = { state = "generating", model = "luna · thinking low · fast" }
  generation[#generation](pending_about)
  assert(presentation[#presentation].about.model == pending_about.model, "pending About omitted the model")
  local progress_text = presentation[#presentation].about.text
  assert(vim.wait(1000, function() return presentation[#presentation].about.text ~= progress_text end, 10),
    "pending About did not publish an animated frame")
  pending_about.model = "resolved-luna · thinking low · fast"
  assert(vim.wait(1000, function() return presentation[#presentation].about.model == pending_about.model end, 10),
    "pending About did not publish the resolved model")
  local pending_generation_count = #generation
  current = true
  owner.activate("status:context:about")
  table.remove(updates).callback({ kind = "about" })
  assert(#notices == 0 and #generation == pending_generation_count,
    "opening pending About notified or restarted generation")
  generation[#generation]({ state = "error", error = "provider authentication failed" })
  assert(owner.about.state == "error" and owner.about.error:find("authentication failed", 1, true),
    "current generation failure was lost")
  owner.activate("status:context:about")
  table.remove(updates).callback({ kind = "about" })
  assert(#errors == 1 and errors[1]:find("authentication failed", 1, true), "opening failed About hid its error")
  local closed_about = owner.about
  generation[#generation]({ state = "generating", model = "luna" })
  closed_about = owner.about
  owner.close()
  local closed_presentations = #presentation
  vim.wait(150, function() return false end)
  assert(#presentation == closed_presentations, "closed Status kept animating")
  assert(not owner.pr_timer, "closed Status retained its mock PR timer")
  generation[generation_count + 1]({ state = "ready", message = "late message" })
  assert(owner.about == closed_about, "closed context adopted generation")
end, debug.traceback)
client.request, ai.ensure, ai.clear, gh.current_pr_async = original_request, original_ai, original_clear, original_pr
notifications.error = original_error
vim.notify = original_notify
config.options.about_auto_generate = original_auto
config.options.pr_lookup_mode, config.options.pr_mock_delay_ms = original_lookup, original_lookup_delay
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
  end },
  { name = "status_pr_cache", run = function()
vim.opt.runtimepath:append("nvim")
local context = require("forge.views.status.status_context")
local gh = require("forge.integrations.gh")
require("forge.infra.config").options.about_auto_generate = false
local request = {}
gh.prs_for_branch_async = function(_, branch, _, callback)
  request[#request + 1] = { branch = branch, callback = callback }
end
gh.pr_async = function(_, number, _, callback)
  callback({ ok = true, pr = { number = number, title = "Cached title", repo = "owner/repo", snapshot = { title = "Cached title" } } })
end
local branch, shown, opened = "feature", nil, nil
require("forge.views.commands").open_pr = function(pr) opened = pr end
local function attach()
  return context.attach({ document_id = "cache-test", workspace = vim.fn.getcwd(),
    window = vim.api.nvim_get_current_win(), is_alive = function() return true end,
    present = function(value) shown = value.pr end,
    get_info = function() return { branch = branch } end,
    get_about_source = function() return { file = {} } end,
    capture_input = function() return {} end, is_input_current = function() return true end })
end
local first = attach()
first.refresh()
assert(shown.state == "fetching")
request[1].callback({ ok = true, prs = { { number = 7, state = "OPEN" } } })
assert(shown.text == "Cached title")
first.close()
local second = attach()
second.refresh()
assert(shown.text == "Cached title" and shown.state == "ready", "refresh hid cached title")
second.open_pull_request(vim.api.nvim_get_current_win(), false)
assert(opened and opened.snapshot.title == "Cached title", "open waited for refresh or discarded the snapshot")
request[2].callback({ ok = false })
assert(shown.text == "Cached title", "failed refresh discarded cached title")
branch = "another-branch"
second.refresh()
assert(shown.state == "fetching" and shown.text == "", "branch change reused a foreign PR")
request[3].callback({ ok = true, prs = {} })
assert(shown.state == "none")
branch = "feature"
second.refresh()
assert(shown.text == "Cached title")
request[4].callback({ ok = true, prs = {} })
second.refresh()
assert(shown.state == "none" and shown.text == "", "confirmed absence retained the old PR")
second.close()
print("status_pr_cache: cached opening, background refresh, failure retention, branch isolation and invalidation passed")
  end },
  { name = "remote_progress", run = function()
local render = require("forge.status_render")
local context = require("forge.views.status.status_context")
local writer = require("forge.git.write")
local notifications = require("forge.infra.notifications")
local original_execute, original_error = writer.execute, notifications.error
local errors, pending, refreshes = {}, {}, 0
local alive = true
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
    header = dofile("nvim/lua/forge/tests/support/status_fixture.lua").header("sample.lua", 2, 1),
    untracked = false, stats = { state = "exact", added = 2, deleted = 1 } } } }
assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
local view = require("forge.input").open(replica, vim.api.nvim_get_current_win())
local file_row = select(2, replica.sequence:position("file:1")) + 1
require("forge.buffer").set_expansion(replica, "file:1", true)
assert(render.apply_body(replica, { document = replica.document, file = 1, generation = 1, more = false,
  state = { state = "ready" }, snapshot = { document = "body:1:1", revision = 1, block = {
    { id = "body:line", text = { "expanded source" }, metadata = { target = {}, decoration = {}, editable_region = {}, fold = {} } },
  } } }).kind == "Applied")
local body = replica.file[1].body
local owner = context.attach({ document_id = replica.document, workspace = "fixture",
  is_alive = function() return true end, capture_input = function() end, is_input_current = function() return true end,
  get_about_source = function() return { file = {} } end,
  present = function(presentation) render.present_context(replica, presentation) end })
local handlers = context.producer_handlers({ workspace = "fixture", context = function() return owner end,
  is_alive = function() return alive end, refresh_status = function() refreshes = refreshes + 1 end })
local function text() return table.concat(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false), "\n") end
local function expect(value)
  assert(vim.api.nvim_get_current_buf() == replica.buffer, "remote operation switched the Status buffer")
  assert(replica.status == "Applied", "remote progress desynchronized Status")
  assert(replica.file[1].body == body, "header progress discarded the expanded diff")
  assert(not require("forge.nodes").closed(replica, "file:1"), "header progress closed the expanded file")
  assert(text():find(value, 1, true), "missing progress: " .. value .. "\n" .. text())
end
local buffer_count = #vim.api.nvim_list_bufs()
local success, failure = xpcall(function()
  local before = text()
  handlers.push()
  expect("Pushing…")
  local initial = text()
  assert(vim.wait(1000, function() return text() ~= initial end, 10), "push spinner did not animate")
  assert(#vim.api.nvim_list_bufs() == buffer_count, "push created a scratch console")
  handlers.push()
  handlers.pull()
  assert(#pending == 1, "concurrent remote action admitted")
  pending[1].progress("Enumerating objects: 200, done.\rWriting objects: 42% (84/200), 1.2 MiB | 2.0 MiB/s\r")
  expect("Writing objects: 42% (84/200)")
  local root = replica.root
  pending[1].progress("Writing objects: 80% (160/200)\n")
  expect("Writing objects: 80% (160/200)")
  local progressing = text()
  assert(vim.wait(1000, function() return text() ~= progressing end, 10), "Git output stopped the spinner")
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
  vim.wait(150, function() return false end)
  assert(text() == before, "completed push kept animating")
  info.upstream = { oid = string.rep("b", 40), reference = "origin/main", subject = "upstream" }
  info.push = info.upstream
  assert(render.apply_snapshot(replica, snapshot).kind == "Applied")
  before = text()
  root = replica.root
  handlers.pull()
  expect("Pulling…")
  assert(replica.root == root, "existing remote row rebuilt the inventory")
  pending[2].progress("Receiving objects: 75% (150/200)\rResolving deltas: 20% (4/20)\n")
  expect("Resolving deltas: 20% (4/20)")
  pending[2].callback({ ok = false, output = "remote rejected the operation" })
  assert(text() == before and refreshes == 2, "failure failed to restore remote reference")
  assert(#errors == 1 and errors[1]:find("remote rejected", 1, true), "failure omitted notification")
  handlers.push()
  expect("Pushing…")
  pending[2].progress("stale pull")
  expect("Pushing…")
  owner.close()
  alive = false
  local closed_text = text()
  pending[3].progress("Writing objects: 99%")
  pending[3].callback({ ok = true })
  vim.wait(150, function() return false end)
  assert(text() == closed_text, "closed context adopted late progress")
  assert(#vim.api.nvim_list_bufs() == buffer_count, "remote operations created a scratch console")
end, debug.traceback)
writer.execute, notifications.error = original_execute, original_error
require("forge.input").close(view)
render.close(replica)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("status_remote_progress OK")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
