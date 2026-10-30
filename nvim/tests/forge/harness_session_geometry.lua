vim.opt.runtimepath:prepend("nvim")
vim.loader.enable(false)
require("forge.infra.config").setup({})
require("forge.infra.highlights").setup()

local client = require("forge.client")
local session = require("forge.session")
local navigation = require("forge.views.harness.session_navigation")
local generation = 1
local queue_by_session = {}
local failures = {}
client.host_get_generation = function() return generation end
client.host_is_accepting = function() return true end
client.session_events_subscribe = function() return function() end end
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end

---Build a native document response for the production presentation adapter.
local function document_snapshot(document, text)
  return { document = document, revision = 0, block = { { id = "body", text = { text },
    metadata = { target = {}, decoration = {}, editable_region = {} } } } }
end

client.request = function(request, callback)
  local params = request.params or {}
  local method = request.method
  local result
  if method == "session.input.get" then
    result = { revision = 0, messages = queue_by_session[request.session_id] or {}, suspended = true, execution_busy = false }
  elseif method == "session.health.get" then result = { responsive = true }
  elseif method == "session.provider.model.list" then result = {}
  elseif method == "session.document" then
    if params.operation == "open" then
      result = { transcript = document_snapshot(params.document, request.session_id .. " transcript"),
        status = document_snapshot(params.document .. ":status", "Waiting") }
    elseif params.operation == "background_terminals" then result = { supported = false }
    elseif params.operation == "staging" then result = { updated = true }
    elseif params.operation == "close" then result = { closed = true }
    else result = { patch = {} } end
  else error("Unexpected native request: " .. method) end
  vim.schedule(function() callback(result) end)
end

---Wait for native presentation work without replacing the real layout owner.
local function settled(state)
  assert(vim.wait(2000, function() return state.presentation and state.presentation.ready end, 1), "presentation did not become ready")
end

---Construct a session with real presentation, status view, and distinct input state.
local function create_session(id)
  local pending = navigation.begin_new(id)
  local state = pending.state
  state.ready = true
  queue_by_session[id] = { { id = id .. "-queued", revision = 1, text = id .. " queued input", attachments = {},
    purpose = { kind = "user" }, intent = "queue", delivery = "queued" } }
  navigation.complete_pending(pending, { session = { id = id, backend = "mock", model = "mock", approval_mode = "ask" } })
  settled(state)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { id .. " draft", "λ" })
  return state
end

---Transfer the current panes and verify both previous geometry owners were collected.
local function transfer(target, source)
  local previous_target, previous_source = target.presentation, source.presentation
  local window, tab = source.composer_win, source.timeline_tab
  generation = generation + 1
  navigation.activate({ session = target.session }, { state = source, open_mode = "current" })
  settled(target)
  assert(session.harness == target and target.composer_win == window and target.timeline_tab == tab)
  assert(source.composer_win == nil and source.timeline_tab == nil)
  assert(not previous_target or previous_target.closed, "recipient retained its old presentation")
  assert(not previous_source or previous_source.closed, "donor retained the reused window's geometry owner")
  assert(target.presentation.status_view.options.composer_window == window)
  assert(not pcall(require("forge.views.harness.layout").observe_composer, window, function() end),
    "live status view did not retain exclusive geometry ownership")
  assert(vim.api.nvim_win_get_buf(window) == target.composer_buf)
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(target.composer_buf, 0, -1, false), { target.session.id .. " draft", "λ" }))
  assert(#target.queue == 1 and target.queue[1].id == target.session.id .. "-queued", "transfer lost queued input identity")
  local height = vim.api.nvim_win_get_height(window)
  vim.api.nvim_buf_set_lines(target.composer_buf, 0, -1, false, { "one", "two", "three", "four", "five", "six" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = target.composer_buf })
  assert(vim.api.nvim_win_get_height(window) > height, "composer callback still targets the old window")
  vim.api.nvim_buf_set_lines(target.composer_buf, 0, -1, false, { target.session.id .. " draft", "λ" })
  vim.api.nvim_exec_autocmds("TextChangedI", { buffer = target.composer_buf })
end

local ok, failure = xpcall(function()
  local first = create_session("geometry-a")
  local second = create_session("geometry-b")
  transfer(first, second)
  transfer(second, first)
  transfer(first, second)
  assert(#failures == 0, table.concat(failures, "\n"))

  local owner = first.presentation
  local close = owner.close
  local window, tab, buffer = first.composer_win, first.timeline_tab, first.composer_buf
  owner.close = function() return false end
  navigation.activate({ session = second.session }, { state = first, open_mode = "current" })
  assert(session.harness == first and first.composer_win == window and first.timeline_tab == tab)
  assert(vim.api.nvim_win_get_buf(window) == buffer and second.composer_win == nil, "failed cleanup mutated pane ownership")
  assert(first.presentation == owner and not owner.closed)
  assert(#failures == 1 and failures[1]:find("cleanup did not complete", 1, true))
  owner.close = function() error("cleanup fault") end
  navigation.activate({ session = second.session }, { state = first, open_mode = "current" })
  assert(session.harness == first and first.composer_win == window and first.timeline_tab == tab)
  assert(vim.api.nvim_win_get_buf(window) == buffer and second.composer_win == nil)
  assert(#failures == 2 and failures[2]:find("cleanup fault", 1, true))
  owner.close = close
  transfer(second, first)
  assert(#failures == 2)
  local third = create_session("geometry-c")
  local previous_third, third_tab = third.presentation, third.timeline_tab
  navigation.activate({ session = second.session }, { open_mode = "current" })
  assert(previous_third.closed and third.presentation == nil, "closing source tab retained presentation resources")
  assert(not vim.api.nvim_tabpage_is_valid(third_tab))
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(third.composer_buf, 0, -1, false), { "geometry-c draft", "λ" }))
  local fourth = create_session("geometry-d")
  local previous_fourth, fourth_tab = fourth.presentation, fourth.timeline_tab
  navigation.activate({ session = second.session }, { open_mode = "tab" })
  assert(fourth.presentation == previous_fourth and not previous_fourth.closed, "tab activation closed an independent view")
  assert(vim.api.nvim_tabpage_is_valid(fourth_tab))
  require("forge.views.harness.workspace").release(fourth)
  assert(fourth.presentation.close())
  require("forge.views.harness.workspace").release(second)
  assert(second.presentation.close())
end, debug.traceback)
if not ok then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("harness_session_geometry: passed")
vim.cmd("qa!")
