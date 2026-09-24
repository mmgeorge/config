vim.opt.runtimepath:prepend('nvim')
local client = require('forge.client')
local config = require('forge.infra.config')
local state = require('forge.session').harness
local pending_stop, pending_start, starts = nil, nil, 0
local activated, saved
require('forge.infra.notifications').error = function() end
require('forge.harness.backend_preference').save = function(backend) saved = backend return true end
require('forge.views.harness.session_navigation').activate = function(result) activated = result end
client.stop = function(_, callback) pending_stop = callback end
client.start_harness = function(callback) starts = starts + 1 pending_start = callback end
state.busy = false
config.options.harness.backend = 'codex'
local harness = require('forge.views.harness')
harness.switch_backend('copilot')
assert(starts == 0 and config.options.harness.backend == 'codex', 'switch started during host drain')
pending_stop()
assert(starts == 1 and config.options.harness.backend == 'copilot')
pending_start(nil, 'simulated startup failure')
assert(starts == 1 and config.options.harness.backend == 'codex', 'fallback started during host drain')
pending_stop()
assert(starts == 2)
pending_start({ restored = true })
assert(activated.restored and saved == nil, 'failed backend was persisted')
harness.switch_backend('copilot')
pending_stop()
pending_start({ switched = true })
assert(activated.switched and saved == 'copilot')
print('harness_backend_switch: passed')
vim.cmd('qa!')
