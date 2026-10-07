vim.opt.runtimepath:prepend('nvim')
local client = require('forge.client')
local config = require('forge.infra.config')
local state = require('forge.session').harness
local picker = require('forge.views.picker')
local pending_stop, pending_start, starts = nil, nil, 0
local activated, saved, initialize_options, lease_picker, activation_options
require('forge.infra.notifications').error = function() end
require('forge.infra.notifications').warn = function() end
require('forge.harness.backend_preference').save = function(backend) saved = backend return true end
require('forge.views.harness.session_navigation').activate = function(result, options)
  activated, activation_options = result, options
  state.session = result.session
end
picker.open = function(spec) lease_picker = spec end
client.stop = function(_, callback) pending_stop = callback end
client.start_harness = function(callback, options)
  starts = starts + 1
  pending_start, initialize_options = callback, options
end
state.busy = false
state.session = { id = 'source', backend = 'codex' }
config.options.harness.backend = 'codex'
local harness = require('forge.views.harness')
local destination = { kind = 'resume', session_id = 'copilot-chosen' }
state.busy = true
harness.switch_backend('copilot', destination)
assert(starts == 0 and pending_stop == nil)
state.busy = false
state.queue = { { prompt = 'pending' } }
harness.switch_backend('copilot', destination)
assert(#state.queue == 1 and pending_stop == nil)
state.queue = {}
harness.switch_backend('copilot', destination)
assert(starts == 0 and config.options.harness.backend == 'codex', 'switch started during host drain')
pending_stop()
assert(starts == 1 and config.options.harness.backend == 'copilot')
assert(initialize_options.session_id == 'copilot-chosen' and initialize_options.new_session_name == nil)
pending_start(nil, 'simulated startup failure')
assert(starts == 1 and config.options.harness.backend == 'codex', 'fallback started during host drain')
pending_stop()
assert(starts == 2 and initialize_options.session_id == 'source', 'restored the latest rather than the source')
pending_start({ session = { id = 'source', backend = 'codex' } })
assert(activated.session.id == 'source' and saved == nil and not state.switching_backend)
harness.switch_backend('copilot', { kind = 'new' })
pending_stop()
assert(initialize_options.new_session_name == '' and initialize_options.session_id == nil)
pending_start(nil, 'leased', { code = 'session_lease_conflict', data = { session_id = 'leased-chat' } })
assert(state.switching_backend and saved == nil)
lease_picker.on_close()
pending_stop()
assert(initialize_options.session_id == 'source')
pending_start({ session = { id = 'source', backend = 'codex' } })
assert(not state.switching_backend and saved == nil)
harness.switch_backend('copilot', { kind = 'new' })
pending_stop()
pending_start({ session = { id = 'new-copilot', backend = 'copilot' } })
assert(activated.session.id == 'new-copilot' and saved == 'copilot')
assert(activation_options.state == state and activation_options.open_mode == 'current')
print('harness_backend_switch: passed')
vim.cmd('qa!')
