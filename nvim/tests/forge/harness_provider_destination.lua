vim.opt.runtimepath:prepend('nvim')
local session = require('forge.session')
local client = require('forge.client')
local picker = require('forge.views.picker')
local harness = require('forge.views.harness')
local controller = require('forge.views.harness.controller')
local session_picker = require('forge.views.harness.session_picker')
local preview = require('forge.views.harness.session_preview')
local state = session.harness
state.session = { id = 'source', backend = 'codex', workspace = 'workspace' }
state.busy = false
state.queue, state.pending_steer = {}, {}
local spec, selected, list_failure, empty_list, previewed, notifications
local preview_request_list = {}
local deferred_preview_list = {}
local defer_preview = false
require('forge.infra.notifications').error = function(message) notifications = message end
require('forge.infra.notifications').warn = function(message) notifications = message end
preview.open = function() end
preview.close = function() end
preview.render = function(result) previewed = result end
picker.open = function(next_spec) spec = next_spec end
picker.update = function(next_spec) spec = next_spec end
picker.is_open = function() return false end
picker.close = function(notify)
  if notify ~= false and spec and spec.on_close then spec.on_close() end
end
harness.switch_backend = function(backend, destination) selected = { backend = backend, destination = destination } end
client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

  if method == 'session.preview' then
    assert(request_specification.session_id == 'source',
      'preview must use the captured calling provider session, not the candidate')
    preview_request_list[#preview_request_list + 1] = vim.deepcopy(request_specification)
    if defer_preview then
      deferred_preview_list[#deferred_preview_list + 1] = function() callback({ preview = params.session_id }) end
    else
      callback({ preview = params.session_id })
    end
    return
  end
  assert(method == 'session.list' and params.scope == 'repo')
  if list_failure then callback(nil, 'list failed') return end
  callback(empty_list and {} or {
    { id = 'chosen', backend = 'copilot', workspace = 'workspace', name = 'Chosen', created_at_ms = 1 },
    { id = 'codex', backend = 'codex', workspace = 'workspace', name = 'Codex', created_at_ms = 2 },
    { id = 'other-workspace', backend = 'copilot', workspace = 'elsewhere', name = 'Other', created_at_ms = 3 },
  })
end
local function choose_provider()
  controller.select_backend()
  for _, option in ipairs(spec.page_list[1].option_list) do
    if option.value == 'copilot' then spec.on_confirm({ option = option }) return end
  end
  error('missing Copilot provider')
end
local function choose_destination(kind)
  for _, option in ipairs(spec.page_list[1].option_list) do
    if option.value == kind then spec.on_confirm({ option = option }) return end
  end
  error('missing destination')
end
controller.select_backend()
for _, option in ipairs(spec.page_list[1].option_list) do
  if option.value == 'codex' then spec.on_confirm({ option = option }) break end
end
assert(spec.page_list[1].title == 'Select Harness' and selected == nil)
choose_provider()
assert(spec.page_list[1].title == 'Select Chat' and selected == nil)
choose_destination('resume')
assert(#spec.page_list[1].option_list == 1 and #spec.action_list == 0)
local option = spec.page_list[1].option_list[1]
assert(option.id == 'chosen')
local source_session = state.session
state.session = { id = 'later-view', backend = 'codex', workspace = 'workspace' }
spec.on_change({ option = option })
state.session = source_session
assert(previewed.preview == 'chosen')
assert(preview_request_list[#preview_request_list].params.session_id == 'chosen',
  'the cross-provider candidate belongs in preview params')
assert(selected == nil and state.session.id == 'source', 'preview must not switch sessions')
defer_preview = true
spec.on_change({ option = option })
spec.on_change({ option = nil })
previewed = nil
deferred_preview_list[#deferred_preview_list]()
assert(previewed == nil, 'clearing selection must invalidate an outstanding preview')
spec.on_change({ option = option })
local closed_preview = deferred_preview_list[#deferred_preview_list]
defer_preview = false
spec.on_confirm({ option = option })
closed_preview()
assert(previewed == nil, 'a closed destination picker must reject late preview callbacks')
assert(selected.backend == 'copilot' and selected.destination.session_id == 'chosen')
assert(state.session.id == 'source', 'selection changed the source before startup')
selected = nil
choose_provider()
empty_list = true
choose_destination('resume')
assert(#spec.page_list[1].option_list == 0)
spec.on_close()
vim.wait(100, function() return spec.page_list[1].title == 'Select Chat' end)
assert(selected == nil and spec.page_list[1].title == 'Select Chat')
choose_destination('new')
assert(selected.destination.kind == 'new')
selected = nil
choose_provider()
list_failure = true
choose_destination('resume')
vim.wait(100, function() return spec.page_list[1].title == 'Select Chat' end)
assert(notifications == 'list failed' and selected == nil)
assert(session_picker._state_for_test() == nil)
list_failure, empty_list = false, false
state.session = nil
session_picker.open({})
local preview_request_count = #preview_request_list
previewed, notifications = nil, nil
spec.on_change({ option = spec.page_list[1].option_list[1] })
assert(#preview_request_list == preview_request_count,
  'an uninitialized caller must not route preview through the candidate')
assert(previewed == nil and notifications ==
  'Wait for the calling session to initialize before previewing saved sessions.')
session_picker.close()
state.session = source_session
state.pending_steer = { { prompt = 'pending' } }
assert(not harness.backend_switch_available())
print('harness_provider_destination: passed')
vim.cmd('qa!')
