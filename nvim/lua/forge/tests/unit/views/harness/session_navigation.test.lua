-- Unit tests for `forge.views.harness.session_navigation` (session_navigation.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

---@class NativeHostFixture
---@field queue_by_session table<string, table[]> Queued input `session.input.get` reports per session.
---@field revision_by_session table<string, integer> Snapshot revision the host reports per session.
---@field runtime_epoch string
---@field failures NotificationCapture
---@field generation fun(next?: integer): integer Read or replace the host generation.
---@field before_reply? fun() Runs before each host reply, at the points where the editor can redraw.
---@field settled fun(state: table) Wait until the view's presentation is ready.
---@field create_session fun(id: string): table Open a session in its own tab with a real presentation.

---Answer the host requests of real presentations, status views, and input state with fixed
---documents, each reply delivered on a later event-loop turn as the host's would be.
---@return NativeHostFixture
local function native_host_fixture()
  require("forge.infra.config").setup({})
  require("forge.infra.highlights").setup()
  local client = require("forge.client")
  local navigation = require("forge.views.harness.session_navigation")
  local host_generation = 1
  ---@type NativeHostFixture
  local fixture = {
    queue_by_session = {},
    -- The host owns snapshot revisions within one runtime epoch, so state reads report its current revision.
    revision_by_session = {},
    runtime_epoch = "geometry-runtime",
    failures = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true }),
  }
  function fixture.generation(next)
    if next then host_generation = next end
    return host_generation
  end
  client.host_get_generation = function() return host_generation end
  client.host_is_accepting = function() return true end
  client.session_events_subscribe = function() return function() end end

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
      result = { revision = 0, messages = fixture.queue_by_session[request.session_id] or {}, suspended = true,
        execution_busy = false }
    elseif method == "session.state.get" or method == "session.resume" then
      local session_id = params.session_id or request.session_id
      result = { session = { id = session_id, provider = "mock", model = "mock", approval_mode = "ask" },
        runtime_epoch = fixture.runtime_epoch, snapshot_revision = fixture.revision_by_session[session_id] or 0 }
    elseif method == "session.health.get" then result = { responsive = true }
    elseif method == "session.provider.model.list" then result = {}
    elseif method == "session.document" then
      if params.operation == "open" then
        result = { transcript = document_snapshot(params.document, request.session_id .. " transcript"),
          status = document_snapshot(params.document .. ":status", "Waiting") }
      elseif params.operation == "background_terminals" then result = { supported = false }
      elseif params.operation == "sync" then
        result = { patch = {}, status = document_snapshot(params.document .. ":status", "Waiting") }
      elseif params.operation == "staging" then result = { updated = true }
      elseif params.operation == "close" then result = { closed = true }
      else result = { patch = {} } end
    else error("Unexpected native request: " .. method) end
    vim.schedule(function()
      if fixture.before_reply then fixture.before_reply() end
      callback(result)
    end)
  end

  ---Wait for native presentation work without replacing the real layout owner.
  function fixture.settled(state)
    wait_until(function() return state.presentation and state.presentation.ready end, "presentation did not become ready",
      { timeout_ms = 2000 })
  end

  ---Construct a session with real presentation, status view, and distinct input state.
  function fixture.create_session(id)
    local pending = navigation.begin_new(id)
    local state = pending.state
    state.ready = true
    fixture.queue_by_session[id] = { { id = id .. "-queued", revision = 1, text = id .. " queued input", attachments = {},
      purpose = { kind = "user" }, intent = "queue", delivery = "queued" } }
    navigation.complete_pending(pending, { session = { id = id, provider = "mock", model = "mock", approval_mode = "ask" } })
    fixture.settled(state)
    buffer.set(state.composer_buf, { id .. " draft", "λ" })
    return state
  end
  return fixture
end

T["session_geometry"] = function()
  local client = require("forge.client")
  local session = require("forge.session")
  local navigation = require("forge.views.harness.session_navigation")
  local fixture = native_host_fixture()
  local failures, settled, create_session = fixture.failures, fixture.settled, fixture.create_session
  local queue_by_session, host_revision_by_session = fixture.queue_by_session, fixture.revision_by_session
  local host_runtime_epoch = fixture.runtime_epoch

  ---Transfer the current panes and verify both previous geometry owners were collected.
  local function transfer(target, source, resume)
    local previous_target, previous_source = target.presentation, source.presentation
    local window, tab = source.composer_win, source.timeline_tab
    fixture.generation(fixture.generation() + 1)
    if resume then
      session.select_harness(source)
      vim.api.nvim_set_current_win(source.composer_win)
      navigation.resume(target.session.id, { open_mode = "current" })
      wait_until(function() return target.timeline_tab == tab end,
        "resuming an already open session should use the requesting tab")
    else
      navigation.activate({ session = target.session }, { state = source, open_mode = "current" })
    end
    settled(target)
    assert(session.harness == target and target.composer_win == window and target.timeline_tab == tab)
    assert(source.composer_win == nil and source.timeline_tab == nil)
    assert(not previous_target or previous_target.closed, "recipient retained its old presentation")
    assert(not previous_source or previous_source.closed, "donor retained the reused window's geometry owner")
    eq(target.presentation.status_view.options.composer_window, window)
    assert(not pcall(require("forge.views.harness.layout").observe_composer, window, function() end),
      "live status view did not retain exclusive geometry ownership")
    eq(vim.api.nvim_win_get_buf(window), target.composer_buf)
    eq(buffer.lines(target.composer_buf), { target.session.id .. " draft", "λ" })
    assert(#target.queue == 1 and target.queue[1].id == target.session.id .. "-queued", "transfer lost queued input identity")
    local height = vim.api.nvim_win_get_height(window)
    buffer.set(target.composer_buf, { "one", "two", "three", "four", "five", "six" })
    vim.api.nvim_exec_autocmds("TextChangedI", { buffer = target.composer_buf })
    assert(vim.api.nvim_win_get_height(window) > height, "composer callback still targets the old window")
    buffer.set(target.composer_buf, { target.session.id .. " draft", "λ" })
    vim.api.nvim_exec_autocmds("TextChangedI", { buffer = target.composer_buf })
  end

  local first = create_session("geometry-a")
  local second = create_session("geometry-b")
  local pending = navigation.begin_new("delayed acknowledgement")
  navigation.activate({ session = first.session }, { open_mode = "tab" })
  local selected_tab, selected_window = vim.api.nvim_get_current_tabpage(), vim.api.nvim_get_current_win()
  local selected_generation = session.harness_selection()
  navigation.complete_pending(pending, { session = { id = "geometry-late", provider = "mock", model = "mock" } })
  assert(session.harness == first, "background acknowledgement selected its pending session")
  assert(vim.api.nvim_get_current_tabpage() == selected_tab and vim.api.nvim_get_current_win() == selected_window,
    "background acknowledgement moved the user's window")
  assert(session.harness_selection() == selected_generation, "background acknowledgement changed intentional selection")
  assert(pending.state.session.id == "geometry-late", "background acknowledgement lost its session result")
  settled(pending.state)
  assert(session.harness == first and vim.api.nvim_get_current_win() == selected_window,
    "background presentation completion stole focus")
  require("forge.views.harness.workspace").release(pending.state)
  assert(pending.state.presentation.close())
  transfer(first, second, true)
  transfer(second, first)
  transfer(first, second)
  failures:assert_none()

  local owner = first.presentation
  local close = owner.close
  local window, tab, composer_buffer = first.composer_win, first.timeline_tab, first.composer_buf
  owner.close = function() return false end
  navigation.activate({ session = second.session }, { state = first, open_mode = "current" })
  assert(session.harness == first and first.composer_win == window and first.timeline_tab == tab)
  assert(vim.api.nvim_win_get_buf(window) == composer_buffer and second.composer_win == nil, "failed cleanup mutated pane ownership")
  assert(first.presentation == owner and not owner.closed)
  assert(#failures == 1 and failures[1].message:find("cleanup did not complete", 1, true))
  owner.close = function() error("cleanup fault") end
  navigation.activate({ session = second.session }, { state = first, open_mode = "current" })
  assert(session.harness == first and first.composer_win == window and first.timeline_tab == tab)
  assert(vim.api.nvim_win_get_buf(window) == composer_buffer and second.composer_win == nil)
  assert(#failures == 2 and failures[2].message:find("cleanup fault", 1, true))
  owner.close = close
  transfer(second, first)
  eq(#failures, 2)
  local third = create_session("geometry-c")
  local previous_third, third_tab, second_tab = third.presentation, third.timeline_tab, second.timeline_tab
  navigation.activate({ session = second.session }, { open_mode = "current" })
  -- The session shown in another tab moves into the current one, whose session stays loaded.
  wait_until(function() return second.timeline_tab == third_tab end, "the displayed session did not move tabs")
  settled(second)
  assert(previous_third.closed and third.presentation == nil, "the replaced session retained presentation resources")
  assert(not vim.api.nvim_tabpage_is_valid(second_tab), "the moved session left its former tab open")
  eq(buffer.lines(third.composer_buf), { "geometry-c draft", "λ" })
  local fourth = create_session("geometry-d")
  local previous_fourth, fourth_tab = fourth.presentation, fourth.timeline_tab
  navigation.activate({ session = second.session }, { open_mode = "tab" })
  assert(fourth.presentation == previous_fourth and not previous_fourth.closed, "tab activation closed an independent view")
  assert(vim.api.nvim_tabpage_is_valid(fourth_tab))
  local draft_buffer = second.composer_buf
  buffer.set(draft_buffer, { "Retain draft across public close" })
  assert(require("forge.shared.view_command_set").dispatch(
    require("forge.views.harness.controller").command_set(), "close", second))
  assert(vim.api.nvim_buf_is_valid(draft_buffer), "public close destroyed the unsent draft")
  host_revision_by_session[second.session.id] = 7
  second.runtime_epoch, second.snapshot_revision = host_runtime_epoch, 7
  client.session_initialize = function(callback, options)
    assert(options and options.session_id == second.session.id, "reopen did not refresh the retained session")
    vim.schedule(function()
      callback({ session = second.session, runtime_epoch = host_runtime_epoch, snapshot_revision = 6 })
    end)
  end
  session.activate_harness(second)
  require("forge.views.harness").open()
  settled(second)
  eq(second.composer_buf, draft_buffer)
  eq(buffer.line(draft_buffer, 1), "Retain draft across public close")
  require("forge.views.harness.workspace").release(fourth)
  assert(fourth.presentation.close())
  local current_tab, current_window = second.timeline_tab, second.composer_win
  local tab_count = #vim.api.nvim_list_tabpages()
  local create_reply
  client.session_create = function(options, callback)
    eq(options.source_session_id, second.session.id)
    create_reply = callback
  end
  require("forge.views.harness").new_session(nil, { open_mode = "current" })
  eq(#vim.api.nvim_list_tabpages(), tab_count)
  create_reply({ session = { id = "geometry-fresh", provider = "mock", model = "mock", approval_mode = "ask" } })
  settled(second)
  eq(second.session.id, "geometry-fresh")
  eq(second.timeline_tab, current_tab)
  eq(second.composer_win, current_window)
  eq(#vim.api.nvim_list_tabpages(), tab_count)
  eq(second.queue, {})
  assert(table.concat(buffer.lines(second.transcript_buf), "\n"):find("geometry-fresh transcript", 1, true),
    "new session did not replace the current transcript")
  eq(#failures, 2)
  require("forge.views.harness.workspace").release(second)
  assert(second.presentation.close())
end

-- Switching sessions in one tab drew every step between the two views: the session being left
-- without its decorations, a blank or raw transcript, and the target before it was positioned.
-- At every host reply, the point where the editor can redraw, the tab must show either the source
-- with its presentation intact or the target ready, and a hidden window must never be drawn.
T["session_switch_shows_only_complete_views"] = function()
  local session = require("forge.session")
  local navigation = require("forge.views.harness.session_navigation")
  local fixture = native_host_fixture()
  local source = fixture.create_session("switch-source")
  local tab, composer_window = source.timeline_tab, source.composer_win
  local _, state_by_buffer = navigation._registry_for_test()
  local frame_list = {}
  local function record_frame()
    if vim.api.nvim_get_current_tabpage() ~= tab then return end
    local frame = {}
    for _, window in ipairs(vim.api.nvim_tabpage_list_wins(tab)) do
      local displayed = state_by_buffer[vim.api.nvim_win_get_buf(window)]
      if displayed and not vim.api.nvim_win_get_config(window).hide
        and vim.api.nvim_win_get_buf(window) == displayed.transcript_buf then
        local presentation = displayed.presentation
        frame[#frame + 1] = { session = displayed.session and displayed.session.id,
          complete = presentation ~= nil and presentation.ready == true and not presentation.closed
            and displayed.transcript_win == window }
      end
    end
    frame_list[#frame_list + 1] = frame
  end
  fixture.before_reply = record_frame
  session.select_harness(source)
  vim.api.nvim_set_current_win(composer_window)
  navigation.resume("switch-target", { open_mode = "current" })
  local target
  wait_until(function()
    target = session.harness_by_id["switch-target"]
    return target and target.timeline_tab == tab and target.presentation and target.presentation.ready
  end, "the target session was not shown in the requesting tab")
  fixture.before_reply = nil
  record_frame()
  assert(#frame_list > 2, "the switch completed without host replies to observe")
  for index, frame in ipairs(frame_list) do
    assert(#frame == 1 and frame[1].complete, "frame " .. index .. " showed an incomplete view: " .. vim.inspect(frame_list))
  end
  eq(frame_list[1][1].session, "switch-source")
  eq(frame_list[#frame_list][1].session, "switch-target")
  eq({ vim.api.nvim_get_current_win(), vim.api.nvim_win_get_buf(composer_window) }, { composer_window, target.composer_buf })
  -- The target's composer buffer was created for the hidden build and still draws the prompt marker.
  assert(vim.wo[composer_window].statuscolumn:find("❯", 1, true), "the switched composer lost its prompt gutter")
  eq(buffer.lines(source.composer_buf), { "switch-source draft", "λ" })
  fixture.failures:assert_none()
end

-- A target that another tab shows is prepared in a hidden window of the requesting tab. Its
-- former tab, which still showed its transcript until the switch completed, was then claimed by
-- no view and labelled with the transcript buffer's name, the raw session id.
T["session_switch_never_labels_a_tab_with_a_session_id"] = function()
  local session = require("forge.session")
  local navigation = require("forge.views.harness.session_navigation")
  local tabline = require("forge.views.harness.tabline")
  local fixture = native_host_fixture()
  local target = fixture.create_session("labelled-target")
  local source = fixture.create_session("labelled-source")
  local target_tab, source_tab = target.timeline_tab, source.timeline_tab
  local label_list = {}
  fixture.before_reply = function() label_list[#label_list + 1] = tabline.render() end
  session.select_harness(source)
  vim.api.nvim_set_current_win(source.composer_win)
  navigation.resume("labelled-target", { open_mode = "current" })
  wait_until(function()
    return target.timeline_tab == source_tab and not vim.api.nvim_tabpage_is_valid(target_tab)
  end, "the target did not move into the requesting tab")
  fixture.before_reply = nil
  label_list[#label_list + 1] = tabline.render()
  assert(#label_list > 2, "the switch completed without host replies to observe")
  for index, label in ipairs(label_list) do
    assert(not label:find("labelled-", 1, true), "frame " .. index .. " labelled a tab with a session id: " .. label)
  end
  fixture.failures:assert_none()
end

T["resume_targets_the_requesting_tab"] = function()
  local session = require("forge.session")
  local navigation = require("forge.views.harness.session_navigation")
  local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
  local requests = mock_client.record()
  local activations = {}
  navigation.activate = function(result, options) activations[#activations + 1] = { result = result, options = options } end
  local requesting, other = session.new_harness_state(), session.new_harness_state()
  session.select_harness(requesting)
  navigation.resume("chosen", { open_mode = "current" })
  -- The user selects another tab before the host answers.
  session.select_harness(other)
  requests[#requests].callback({ session = { id = "chosen" } })
  local options = activations[1].options
  assert(options.source_state == requesting and options.focus == false and options.open_mode == "current",
    "a late resume acted on the selected tab or took its focus")
  navigation.resume("chosen-again", { open_mode = "tab" })
  requests[#requests].callback({ session = { id = "chosen-again" } })
  options = activations[2].options
  assert(options.source_state == other and options.focus == nil, "a resume for the selected tab lost its focus")
end

T["resume_reopens_only_an_unavailable_current_session"] = function()
  local session = require("forge.session")
  local navigation = require("forge.views.harness.session_navigation")
  local mock_client = dofile("nvim/lua/forge/tests/support/mock_client.lua")
  local requests = mock_client.record()
  local activations = {}
  navigation.activate = function(result, options) activations[#activations + 1] = { result = result, options = options } end
  local current = session.new_harness_state()
  current.transcript_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(0, current.transcript_buf)
  current.transcript_win, current.timeline_tab = vim.api.nvim_get_current_win(), vim.api.nvim_get_current_tabpage()
  session.register_harness("current", current)
  session.select_harness(current)

  navigation.resume("current", { open_mode = "current" })
  eq(#requests, 0)
  eq(session.harness, current)

  current.activity = { revision = 3, phase = "unavailable",
    unavailable_reason = "The provider stopped and could not be restarted (fixture)." }
  navigation.resume("current", { open_mode = "current" })
  eq(#requests, 1)
  eq(requests[1].spec.method, "session.resume")
  eq(requests[1].spec.params, { session_id = "current" })
  requests[1].callback({ session = { id = "current" } })
  assert(activations[1].options.state == current, "the reopened session left its own view")
end


-- A fork the host refuses creates no session. A tab opened while the request was in flight was
-- drawn for a frame before the refusal closed it, and its focus change left Insert mode and its
-- submission emptied the composer. The editor now asks first: no tab or buffer appears before the
-- host answers or after it refuses, and the command stays in the composer in the mode it had.
T["refused_fork_draws_no_tab_and_keeps_the_draft"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    require("forge.infra.config").setup({})
    local session = require("forge.session")
    local layout = require("forge.views.harness.layout")
    local controller = require("forge.views.harness.controller")
    require("forge.views.harness.prompt_history").record = function() end
    _G.requests = {}
    require("forge.client").request = function(spec, callback)
      _G.requests[#_G.requests + 1] = { spec = spec, callback = callback }
    end
    _G.messages = {}
    vim.notify = function(message) _G.messages[#_G.messages + 1] = message end
    local source = session.harness
    source.transcript_buf, source.transcript_win, source.composer_buf, source.composer_win, source.timeline_tab =
      layout.open("fork-source", "tab")
    source.session = { id = "fork-source", name = "source" }
    source.capability = { native_fork = true }
    session.register_harness("fork-source", source)
    session.select_harness(source)
    vim.api.nvim_set_current_win(source.composer_win)
    vim.keymap.set("i", "<F2>", controller.submit, { buffer = source.composer_buf })
    _G.source = source
    _G.window_state = function()
      return { tab_count = #vim.api.nvim_list_tabpages(), buffer_list = vim.api.nvim_list_bufs(),
        window = vim.api.nvim_get_current_win() == _G.source.composer_win }
    end
    _G.before = _G.window_state()
  ]])
  child.type_keys("i", "/fork", "<F2>")
  eq(child.lua_get([[{ #_G.requests, _G.requests[1].spec.method }]]), { 1, "session.fork" })
  eq(child.lua_get([[vim.deep_equal(_G.window_state(), _G.before)]]), true)
  child.lua([[_G.requests[1].callback(nil, "cannot fork a forked session during its first turn")]])
  eq(child.lua_get([[vim.deep_equal(_G.window_state(), _G.before)]]), true)
  eq(child.lua_get([[vim.api.nvim_buf_get_lines(_G.source.composer_buf, 0, -1, false)]]), { "/fork" })
  eq(child.api.nvim_get_mode().mode, "i")
  eq(child.lua_get([[_G.messages]]), { "cannot fork a forked session during its first turn" })
end

-- A fork the host accepts opens its tab, named for the child, and focuses it when the user is
-- still where `/fork` was typed. No buffer named for a placeholder remains, listed or not.
T["accepted_fork_opens_its_tab_without_placeholders"] = function()
  require("forge.infra.config").setup({})
  local session = require("forge.session")
  local layout = require("forge.views.harness.layout")
  local controller = require("forge.views.harness.controller")
  for _, name in ipairs({ "activate_snapshot", "attach", "attach_transcript", "refresh_winbar" }) do
    controller[name] = function() end
  end
  local requests = mock_client.record()
  local source = session.harness
  source.transcript_buf, source.transcript_win, source.composer_buf, source.composer_win, source.timeline_tab =
    layout.open("fork-origin", "tab")
  source.session = { id = "fork-origin", name = "origin" }
  source.capability = { native_fork = true }
  session.register_harness("fork-origin", source)
  session.select_harness(source)
  vim.api.nvim_set_current_win(source.composer_win)
  local tab_count = #vim.api.nvim_list_tabpages()

  controller.fork_session()
  eq(#vim.api.nvim_list_tabpages(), tab_count)
  requests[1].callback({ session = { id = "fork-child", provider = "mock", model = "mock" } })
  eq(#vim.api.nvim_list_tabpages(), tab_count + 1)
  local child = session.harness_by_id["fork-child"]
  eq(vim.api.nvim_get_current_tabpage(), child.timeline_tab)
  eq(vim.api.nvim_get_current_win(), child.composer_win)
  local name_list = vim.tbl_map(vim.api.nvim_buf_get_name, vim.api.nvim_list_bufs())
  eq(vim.tbl_filter(function(name) return name:find("pending", 1, true) ~= nil end, name_list), {})
  eq(vim.tbl_contains(name_list, "ForgeHarness://fork-child"), true)
end

-- Renaming a buffer keeps its former name on a new unlisted buffer, the alternate file, so each
-- view opened under a placeholder name and then bound to its session left one such buffer per
-- pane behind in `:ls!`.
T["new_session_leaves_no_placeholder_buffer"] = function()
  require("forge.infra.config").setup({})
  local navigation = require("forge.views.harness.session_navigation")
  local controller = require("forge.views.harness.controller")
  for _, name in ipairs({ "activate_snapshot", "attach", "attach_transcript", "refresh_winbar" }) do
    controller[name] = function() end
  end
  local pending = navigation.begin_new("named")
  navigation.complete_pending(pending, { session = { id = "created", provider = "mock", model = "mock" } })
  local name_list = vim.tbl_map(vim.api.nvim_buf_get_name, vim.api.nvim_list_bufs())
  eq(vim.tbl_filter(function(name) return name:find("pending", 1, true) ~= nil end, name_list), {})
  eq(vim.tbl_contains(name_list, "ForgeHarness://created"), true)
  eq(vim.tbl_contains(name_list, "HarnessInput://created"), true)
end

-- `/new` focuses the composer of a new tab, whose entry leaves Insert mode. `/clear` replaces the
-- session in the composer that already has focus, so no entry happened and the composer stayed in
-- Insert mode; both commands now leave the composer in Normal mode.
T["clear_leaves_the_composer_in_normal_mode"] = function()
  local child = MiniTest.new_child_neovim()
  child.start({ "-u", "NONE" })
  MiniTest.finally(function() pcall(child.stop) end)
  child.lua([[
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    require("forge.infra.config").setup({})
    local session = require("forge.session")
    local layout = require("forge.views.harness.layout")
    local controller = require("forge.views.harness.controller")
    for _, name in ipairs({ "activate_snapshot", "attach", "attach_transcript", "refresh_winbar" }) do
      controller[name] = function() end
    end
    -- The host answers after the composer has returned to waiting for keys, as it does in use.
    require("forge.client").session_create = function(_, callback) _G.reply = callback end
    local source = session.harness
    source.transcript_buf, source.transcript_win, source.composer_buf, source.composer_win, source.timeline_tab =
      layout.open("clear-source", "tab")
    source.session = { id = "clear-source" }
    session.register_harness("clear-source", source)
    session.select_harness(source)
    vim.api.nvim_set_current_win(source.composer_win)
    vim.keymap.set("i", "<F2>", function()
      require("forge.views.harness").new_session(nil, { open_mode = "current" })
    end, { buffer = source.composer_buf })
    _G.source = source
  ]])
  child.type_keys("i", "<F2>")
  eq(child.api.nvim_get_mode().mode, "i")
  child.lua([[_G.reply({ session = { id = "cleared", provider = "mock", model = "mock" } })]])
  eq(child.lua_get([[{ require("forge.session").harness_by_id.cleared == _G.source,
    vim.api.nvim_get_current_win() == _G.source.composer_win }]]), { true, true })
  eq(child.api.nvim_get_mode().mode, "n")
end

-- Choosing a session from Insert mode returns its composer in Insert mode. The chosen composer's
-- entry guard ran `stopinsert` when focus reached it, and a `startinsert` issued while Insert mode
-- is still active, as in a reply that arrives after the picker restored it, is ignored, so the
-- deliberate focus must not trigger the guard at all.
T["insert_focus_does_not_trigger_the_composer_entry_guard"] = function()
  local session = require("forge.session")
  local layout = require("forge.views.harness.layout")
  local navigation = require("forge.views.harness.session_navigation")
  local function view(name, open_mode)
    local state = session.new_harness_state()
    state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
      layout.open(name, open_mode)
    session.register_harness(name, state)
    return state
  end
  local chosen = view("chosen", "tab")
  local current = view("current", "tab")
  session.select_harness(current)
  vim.api.nvim_set_current_win(current.composer_win)
  local stop_count = 0
  local original_cmd = vim.cmd
  vim.cmd = setmetatable({}, { __index = original_cmd, __call = function(_, command)
    if command == "stopinsert" then stop_count = stop_count + 1 end
    return original_cmd(command)
  end })
  MiniTest.finally(function() vim.cmd = original_cmd end)
  vim.keymap.set("i", "<F2>", function()
    navigation.resume("chosen", { open_mode = "tab", insert = true })
  end, { buffer = current.composer_buf })
  vim.api.nvim_feedkeys(vim.keycode("i<F2>"), "xt", false)
  vim.cmd = original_cmd
  eq({ vim.api.nvim_get_current_win(), stop_count }, { chosen.composer_win, 0 })
end

return T
