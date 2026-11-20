-- Unit tests for `forge.views.harness` (init.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["open_uses_new_tab_on_first_launch"] = function()
  local session = require("forge.session")
  local client = require("forge.client")
  local controller = require("forge.views.harness.controller")
  local state = session.harness
  local tab = vim.api.nvim_get_current_tabpage()
  local tab_count = #vim.api.nvim_list_tabpages()
  controller.attach = function() end
  controller.render = function() end
  client.session_initialize = function() end
  require("forge").open_harness()
  eq(#vim.api.nvim_list_tabpages(), tab_count + 1)
  eq(state.timeline_tab ~= tab, true)
  assert(vim.api.nvim_win_is_valid(state.transcript_win))
  assert(vim.api.nvim_win_is_valid(state.composer_win))
  local composer = state.composer_win
  require("forge").open_harness()
  eq(#vim.api.nvim_list_tabpages(), tab_count + 1)
  eq(state.composer_win, composer)
  state.startup_trace.finish("test")
end

T["provider_switch"] = function()
  local client = require('forge.client')
  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
  local config = require('forge.infra.config')
  local state = require('forge.session').harness
  local picker = require('forge.views.picker')
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  local pending_stop, pending_start, starts = nil, nil, 0
  local activated, saved, initialize_options, lease_picker, activation_options
  require('forge.infra.notifications').error = function() end
  require('forge.infra.notifications').warn = function() end
  require('forge.harness.provider_preference').save = function(provider, callback) saved = provider callback(true) end
  require('forge.views.harness.session_navigation').activate = function(result, options)
    activated, activation_options = result, options
    state.session = result.session
  end
  picker.open = function(spec) lease_picker = spec end
  client.host_stop = function(_, callback) pending_stop = callback end
  client.session_initialize = function(callback, options)
    starts = starts + 1
    pending_start, initialize_options = callback, options
  end
  activity.set_busy(state, false)
  state.session = { id = 'source', provider = 'fixture_alpha' }
  config.options.harness.provider = 'fixture_alpha'
  local harness = require('forge.views.harness')
  local destination = { kind = 'resume', session_id = 'fixture_beta-chosen' }
  activity.set_busy(state, true)
  harness.switch_provider('fixture_beta', destination)
  assert(starts == 0 and pending_stop == nil)
  activity.set_busy(state, false)
  state.queue = { { prompt = 'pending' } }
  harness.switch_provider('fixture_beta', destination)
  assert(#state.queue == 1 and pending_stop == nil)
  state.queue = {}
  harness.switch_provider('fixture_beta', destination)
  assert(starts == 0 and config.options.harness.provider == 'fixture_alpha', 'switch started during host drain')
  pending_stop()
  assert(starts == 1 and config.options.harness.provider == 'fixture_beta')
  assert(initialize_options.session_id == 'fixture_beta-chosen' and initialize_options.new_session_name == nil)
  state.initial_snapshot_pending = true
  pending_start(nil, 'simulated startup failure')
  assert(not state.initial_snapshot_pending, 'startup failure retained the pending presentation')
  assert(starts == 1 and config.options.harness.provider == 'fixture_alpha', 'fallback started during host drain')
  pending_stop()
  assert(starts == 2 and initialize_options.session_id == 'source', 'restored the latest rather than the source')
  pending_start({ session = { id = 'source', provider = 'fixture_alpha' } })
  assert(activated.session.id == 'source' and saved == nil and not state.switching_provider)
  harness.switch_provider('fixture_beta', { kind = 'new' })
  pending_stop()
  assert(initialize_options.new_session_name == '' and initialize_options.session_id == nil)
  pending_start(nil, 'leased', { code = 'session_lease_conflict', data = { session_id = 'leased-chat' } })
  assert(state.switching_provider and saved == nil)
  state.initial_snapshot_pending = true
  lease_picker.on_close()
  assert(not state.initial_snapshot_pending, 'dismissed session selection retained the pending presentation')
  pending_stop()
  eq(initialize_options.session_id, 'source')
  pending_start({ session = { id = 'source', provider = 'fixture_alpha' } })
  assert(not state.switching_provider and saved == nil)
  harness.switch_provider('fixture_beta', { kind = 'new' })
  pending_stop()
  pending_start({ session = { id = 'new-fixture_beta', provider = 'fixture_beta' } })
  assert(activated.session.id == 'new-fixture_beta' and saved == 'fixture_beta')
  -- The new chat is built behind the switching view, which stays the activation's source.
  assert(activation_options.state == nil and activation_options.source_state == state
    and activation_options.open_mode == 'current')
end

T["provider_switch_reports_its_start_and_can_keep_the_default"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
  local client = require("forge.client")
  local state = require("forge.session").harness
  local harness = require("forge.views.harness")
  notify.capture({ notifications = true })
  local saved_provider, pending_stop, pending_start = nil, nil, nil
  stub(require("forge.harness.provider_preference"), "save", function(provider, callback)
    saved_provider = provider
    callback(true)
  end)
  stub(require("forge.views.harness.session_navigation"), "activate", function(result) state.session = result.session end)
  stub(client, "host_stop", function(_, callback) pending_stop = callback end)
  stub(client, "session_initialize", function(callback) pending_start = callback end)
  state.session = { id = "source", provider = "fixture_alpha" }
  require("forge.infra.config").options.harness.provider = "fixture_alpha"

  -- A caller such as the session picker dismisses its own view only once the switch has started.
  activity.set_busy(state, true)
  eq(harness.switch_provider("fixture_beta", { kind = "resume", session_id = "chosen" }), false)
  eq(pending_stop, nil)
  activity.set_busy(state, false)
  eq(harness.switch_provider("unregistered", { kind = "resume", session_id = "chosen" }), false)
  eq(harness.switch_provider("fixture_beta", { kind = "resume" }), false)
  eq(pending_stop, nil)

  -- Resuming a session is not a choice of default provider, so its switch leaves the saved default.
  eq(harness.switch_provider("fixture_beta", { kind = "resume", session_id = "chosen", persist_default = false }), true)
  pending_stop()
  pending_start({ session = { id = "chosen", provider = "fixture_beta" } })
  eq({ state.session.id, state.switching_provider, saved_provider }, { "chosen", false, nil })

  -- An explicit provider choice keeps saving the default.
  eq(harness.switch_provider("fixture_alpha", { kind = "resume", session_id = "source" }), true)
  pending_stop()
  pending_start({ session = { id = "source", provider = "fixture_alpha" } })
  eq({ state.session.id, saved_provider }, { "source", "fixture_alpha" })
end

T["provider_switch_checks_every_tab"] = function()
  local session = require("forge.session")
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  local harness = require("forge.views.harness")
  local state = session.harness
  state.session = { id = "switching", provider = "fixture_alpha" }
  activity.set_busy(state, false)
  local other = session.new_harness_state()
  other.session = { id = "other-tab", provider = "fixture_alpha" }
  session.register_harness("switching", state)
  session.register_harness("other-tab", other)
  activity.set_busy(other, true)
  -- The host serves every tab, so a turn running in another tab blocks the switch.
  eq(harness.provider_switch_available(), false)
  activity.set_busy(other, false)
  other.queue = { { text = "queued elsewhere" } }
  eq(harness.provider_switch_available(), false)
  other.queue, other.status = {}, { kind = "finalizing" }
  eq(harness.provider_switch_available(), false)
  other.status = { kind = "idle" }
  eq(harness.provider_switch_available(), true)
  -- A tab whose host already stopped has nothing left to interrupt.
  activity.set_busy(other, true)
  other.host_error = "Forge host stopped"
  eq(harness.provider_switch_available(), true)
end

T["provider_switch_tells_other_tabs_and_their_submit_switches_back"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
  local session = require("forge.session")
  local client = require("forge.client")
  local config = require("forge.infra.config")
  local controller = require("forge.views.harness.controller")
  local diagnostic = require("forge.views.harness.diagnostic")
  local health = require("forge.views.harness.health")
  local harness = require("forge.views.harness")
  notify.capture({ notifications = true })
  stub(controller, "render", function() end)
  stub(controller, "configure", function() end)
  local saved_provider
  stub(require("forge.harness.provider_preference"), "save", function(provider) saved_provider = provider end)
  local activation_options
  stub(require("forge.views.harness.session_navigation"), "activate", function(result, options)
    activation_options = options
    local target = session.harness_by_id[result.session.id] or session.new_harness_state()
    target.session = result.session
    target.host_error = nil
    session.register_harness(result.session.id, target)
    if options.on_settled then options.on_settled() end
  end)
  local pending_stop, pending_start, initialize_options
  stub(client, "host_stop", function(_, callback) pending_stop = callback end)
  stub(client, "session_initialize", function(callback, options) pending_start, initialize_options = callback, options end)
  -- The host's stop notice reaches every subscribed tab before the stop completes.
  local function stop_host()
    for _, state in pairs(session.harness_by_id) do
      if not state.switching_provider then
        diagnostic.set(state, "host_error", "Forge host stopped", { category = "host", recovery = "reconnect" })
      end
    end
    pending_stop()
  end
  config.options.harness.provider = "fixture_alpha"
  local source = session.harness
  source.session = { id = "source", provider = "fixture_alpha" }
  local other = session.new_harness_state()
  other.session = { id = "other-tab", provider = "fixture_alpha" }
  session.register_harness("source", source)
  session.register_harness("other-tab", other)
  activity.set_busy(source, false)
  activity.set_busy(other, false)

  eq(harness.switch_provider("fixture_beta", { kind = "resume", session_id = "chosen", persist_default = false }), true)
  -- The switching view says that a switch is running until the chosen session replaces it.
  local running = health.notice(source)
  eq({ running and running.text, running and running.animated }, { "Switching Harness to Fixture 2", true })
  stop_host()
  pending_start({ session = { id = "chosen", provider = "fixture_beta" } })
  -- The chosen session is built hidden behind the source view, as a same-provider switch is.
  eq({ activation_options.state, activation_options.source_state, activation_options.open_mode },
    { nil, source, "current" })
  eq(source.switching_provider, false)
  -- Every other tab keeps a notice in its own view that names the provider the host now serves.
  local detached = health.notice(other)
  eq(detached and detached.text,
    "Harness now serves Fixture 2. Submit or run :ForgeHarness to return to Fixture 1.")

  -- A submit from that tab switches the host back to its provider and resumes its session there,
  -- leaving the saved default alone and keeping the draft unsent.
  other.composer_buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(other.composer_buf, 0, -1, false, { "kept draft" })
  controller.with_state(other, controller.submit)
  eq(initialize_options, { session_id = "chosen" })
  stop_host()
  eq(initialize_options, { session_id = "other-tab" })
  eq(health.notice(other).text, "Switching Harness to Fixture 1")
  pending_start({ session = { id = "other-tab", provider = "fixture_alpha" } })
  eq({ other.host_error, other.switching_provider, config.options.harness.provider, saved_provider },
    { nil, false, "fixture_alpha", nil })
  eq(vim.api.nvim_buf_get_lines(other.composer_buf, 0, -1, false), { "kept draft" })
  -- The tab that switched away first is now the one that says how to come back.
  eq(health.notice(session.harness_by_id["chosen"]).text,
    "Harness now serves Fixture 1. Submit or run :ForgeHarness to return to Fixture 2.")
end

T["provider_switch_back_is_refused_while_another_tab_has_work"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
  local session = require("forge.session")
  local client = require("forge.client")
  local diagnostic = require("forge.views.harness.diagnostic")
  local harness = require("forge.views.harness")
  local notices = notify.capture({ notifications = true })
  local stopped = false
  stub(client, "host_stop", function() stopped = true end)
  require("forge.infra.config").options.harness.provider = "fixture_beta"
  local live = session.harness
  live.session = { id = "live", provider = "fixture_beta" }
  local detached = session.new_harness_state()
  detached.session = { id = "detached", provider = "fixture_alpha" }
  diagnostic.set(detached, "host_error", "Harness now serves Fixture 2.", { category = "host", recovery = "reconnect" })
  session.register_harness("live", live)
  session.register_harness("detached", detached)
  activity.set_busy(live, true)
  -- The refusal is the reported outcome of the reconnect, so the caller keeps the draft and
  -- reports nothing else.
  require("forge.views.harness.controller").with_state(detached, function()
    eq(harness.reconnect(detached), true)
  end)
  eq(stopped, false)
  eq(detached.host_error ~= nil, true)
  eq(notices:messages(), { "Cannot switch providers yet: another Harness session has a running turn. Finish or cancel it first." })
end

T["provider_switch_refusal_names_the_tab_and_its_work"] = function()
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  local session = require("forge.session")
  local harness = require("forge.views.harness")
  local state = session.harness
  state.session = { id = "current", provider = "fixture_alpha", name = "" }
  activity.set_busy(state, false)
  local other = session.new_harness_state()
  other.session = { id = "other-tab", provider = "fixture_alpha", name = "Fix tax rate" }
  vim.cmd("tabnew")
  other.timeline_tab = vim.api.nvim_get_current_tabpage()
  session.register_harness("current", state)
  session.register_harness("other-tab", other)
  local other_tab = vim.api.nvim_tabpage_get_number(other.timeline_tab)
  activity.set_busy(other, true)
  eq(harness.provider_switch_refusal(), ("Cannot switch providers yet: tab %d (Fix tax rate) has a running turn. "
    .. "Finish or cancel it first."):format(other_tab))
  -- A turn stopped at an approval request needs the user's answer, not patience.
  other.status = { kind = "awaiting_approval" }
  eq(harness.provider_switch_refusal(), ("Cannot switch providers yet: tab %d (Fix tax rate) has a turn waiting "
    .. "for an approval. Finish or cancel it first."):format(other_tab))
  other.status = nil
  activity.set_busy(other, false)
  other.queue = { { text = "one" }, { text = "two" } }
  eq(harness.provider_switch_refusal(), ("Cannot switch providers yet: tab %d (Fix tax rate) has 2 queued prompts. "
    .. "Finish or cancel it first."):format(other_tab))
  other.queue = {}
  state.pending_submit = { { text = "sending" } }
  eq(harness.provider_switch_refusal(), "Cannot switch providers yet: this tab has a prompt that is still being "
    .. "submitted. Finish or cancel it first.")
  state.pending_submit = {}
  eq(harness.provider_switch_refusal(), nil)
end

T["provider_switch_waits_for_a_detached_tab_that_is_switching_back"] = function()
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  local diagnostic = require("forge.views.harness.diagnostic")
  local session = require("forge.session")
  local harness = require("forge.views.harness")
  local state = session.harness
  state.session = { id = "current", provider = "fixture_alpha" }
  activity.set_busy(state, false)
  -- The other tab's host stopped, and that tab is now opening its session on its own provider.
  local returning = session.new_harness_state()
  returning.session = { id = "returning", provider = "fixture_beta", name = "Returning" }
  diagnostic.set(returning, "host_error", "Harness now serves Fixture 1.", { category = "host", recovery = "reconnect" })
  returning.switching_provider = true
  session.register_harness("current", state)
  session.register_harness("returning", returning)
  -- Stopping the host now would discard that tab's session open.
  eq(harness.provider_switch_available(), false)
  eq(harness.provider_switch_refusal(),
    'Cannot switch providers yet: session "Returning" is switching the Harness provider. Wait for it to finish.')
  returning.switching_provider = false
  eq(harness.provider_switch_available(), true)
end

T["failed_provider_switch_leaves_its_cause_in_the_view"] = function()
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
  local activity = dofile("nvim/lua/forge/tests/support/activity.lua")
  dofile("nvim/lua/forge/tests/support/provider_catalog.lua").install()
  local session = require("forge.session")
  local client = require("forge.client")
  local controller = require("forge.views.harness.controller")
  local health = require("forge.views.harness.health")
  local harness = require("forge.views.harness")
  notify.capture({ notifications = true })
  stub(controller, "render", function() end)
  stub(require("forge.views.harness.session_navigation"), "activate", function(result, options)
    session.harness.session = result.session
    if options.on_settled then options.on_settled() end
  end)
  local pending_stop, pending_start
  stub(client, "host_stop", function(_, callback) pending_stop = callback end)
  stub(client, "session_initialize", function(callback) pending_start = callback end)
  require("forge.infra.config").options.harness.provider = "fixture_alpha"
  local state = session.harness
  state.session = { id = "source", provider = "fixture_alpha" }
  session.register_harness("source", state)
  activity.set_busy(state, false)
  eq(harness.switch_provider("fixture_beta", { kind = "new" }), true)
  pending_stop()
  pending_start(nil, "Forge host stopped: unresolved request 952 (session.initialize)")
  pending_stop()
  pending_start({ session = { id = "source", provider = "fixture_alpha" } })
  -- The view is back on its own session, and it still says why the switch did not happen.
  local notice = health.notice(state)
  eq({ state.session.id, state.switching_provider }, { "source", false })
  eq(notice and { notice.text, notice.failed }, { "Switching to Fixture 2 failed: the Harness host stopped before "
    .. "the session opened (Forge host stopped: unresolved request 952 (session.initialize)). Harness still serves "
    .. "Fixture 1.", true })
  -- The next switch replaces the explanation with its own progress.
  eq(harness.switch_provider("fixture_beta", { kind = "new" }), true)
  eq(health.notice(state).text, "Switching Harness to Fixture 2")
end

T["replace_session_preserves_requesting_tab_and_handles_failure"] = function()
  local session = require("forge.session")
  local client = require("forge.client")
  local navigation = require("forge.views.harness.session_navigation")
  local harness = require("forge.views.harness")
  local source = session.harness
  source.session = { id = "source" }
  source.timeline_tab = vim.api.nvim_get_current_tabpage()
  source.transcript_win = vim.api.nvim_get_current_win()
  source.composer_win = vim.api.nvim_get_current_win()
  source.transcript_buf = vim.api.nvim_get_current_buf()
  local reply, activation, failure
  client.session_create = function(options, callback)
    eq(options.source_session_id, "source")
    reply = callback
  end
  navigation.begin_new = function() error("replacement opened a pending tab") end
  navigation.activate = function(result, options) activation = { result = result, options = options } end
  require("forge.infra.notifications").error = function(message) failure = message end
  harness.new_session(nil, { open_mode = "current" })
  reply(nil, "creation failed")
  eq(failure, "creation failed")
  eq(source.session.id, "source")
  eq(activation, nil)
  harness.new_session(nil, { open_mode = "current" })
  local other = session.new_harness_state()
  session.select_harness(other)
  reply({ session = { id = "fresh" } })
  eq(activation.result.session.id, "fresh")
  eq(activation.options.state, source)
  eq(activation.options.open_mode, "current")
  eq(activation.options.focus, false)
end

T["new_session_traces_response_and_watches_the_created_view"] = function()
  local session = require("forge.session")
  local navigation = require("forge.views.harness.session_navigation")
  local startup_log = require("forge.harness_startup_log")
  local events, watched, reply = {}, nil, nil
  local state = session.new_harness_state()
  state.transcript_buf = vim.api.nvim_get_current_buf()
  state.timeline_tab = vim.api.nvim_get_current_tabpage()
  startup_log.begin = function(fields)
    eq(fields.operation, "new")
    return {
      mark = function(phase) events[#events + 1] = phase end,
      finish = function(phase) events[#events + 1] = phase end,
      watch = function(buffer) watched = buffer end,
    }
  end
  navigation.begin_new = function() return { state = state } end
  navigation.complete_pending = function() events[#events + 1] = "activated" end
  navigation.fail_pending = function() end
  require("forge.infra.notifications").error = function() end
  require("forge.client").session_create = function(_, callback) reply = callback end
  local harness = require("forge.views.harness")
  harness.new_session()
  eq(events, { "layout.ready", "session.new.dispatch" })
  reply({ session = { id = "created" } })
  eq(events, { "layout.ready", "session.new.dispatch", "session.new.received",
    "snapshot.apply.begin", "activated", "snapshot.apply.end" })
  eq(watched, state.transcript_buf)
  events, watched = {}, nil
  harness.new_session()
  reply(nil, "creation failed")
  eq(events, { "layout.ready", "session.new.dispatch", "session.new.received", "failed" })
  eq(watched, nil)
end

return T
