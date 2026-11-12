-- Unit tests for `forge.views.harness` (init.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["open_uses_current_tab_on_first_launch"] = function()
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
  eq(#vim.api.nvim_list_tabpages(), tab_count)
  eq(state.timeline_tab, tab)
  assert(vim.api.nvim_win_is_valid(state.transcript_win))
  assert(vim.api.nvim_win_is_valid(state.composer_win))
  local composer = state.composer_win
  require("forge").open_harness()
  eq(#vim.api.nvim_list_tabpages(), tab_count)
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
  require('forge.harness.provider_preference').save = function(provider) saved = provider return true end
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
  assert(activation_options.state == state and activation_options.open_mode == 'current')
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

return T
