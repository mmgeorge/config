vim.loader.enable(false)
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, 'p') == 1 and vim.fn.mkdir(data, 'p') == 1)
local executable = require('forge.builder').binary_path()
assert(vim.fn.executable(executable) == 1, 'build the release Forge host first')
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == 'data' or kind == 'config') and data or original_stdpath(kind)
end
package.loaded['forge.builder'] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
local client = require('forge.client')
client._host_set_launcher_for_test(vim.system)
local harness = require('forge.views.harness')
local controller = require('forge.views.harness.controller')
local session = require('forge.session')
local picker = require('forge.views.picker')
local errors = {}
require('forge.infra.notifications').error = function(message) errors[#errors + 1] = message .. '\n' .. debug.traceback() end
controller.resolve_runtime_model = function() end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require('forge').setup({})
  harness.open()
  assert(vim.wait(10000, function() return (session.harness.ready and session.harness.session) or #errors > 0 end, 10))
  assert(#errors == 0, table.concat(errors, '\n'))
  local source_id = session.harness.session.id
  local config = require('forge.infra.config')
  local provider_ids = {}
  for provider, descriptor in pairs(config.options.harness.providers) do
    if descriptor.selectable ~= false then provider_ids[#provider_ids + 1] = provider end
  end
  table.sort(provider_ids)
  local initial_provider = session.harness.session.provider
  local alternate_provider
  for _, provider in ipairs(provider_ids) do
    if provider ~= initial_provider then alternate_provider = alternate_provider or provider end
  end
  assert(alternate_provider, 'registry must expose multiple selectable providers')
  local source_tab = vim.api.nvim_get_current_tabpage()
  local tab_count = #vim.api.nvim_list_tabpages()
  local function request(method, params)
    local result, failure, completed
    client.request({ method = method, params = params, session_id = require("forge.event_contract").method_policy(method).session_context ~= "none" and assert(require("forge.session").harness.session).id or nil }, function(value, request_error)
      result, failure, completed = value, request_error, true
    end)
    assert(vim.wait(10000, function() return completed end, 10), method .. ' did not complete')
    assert(not failure, failure)
    return result
  end
  local preference = request('settings.get', {})
  local operation = request('settings.update', {
    operation_id = 'provider-transfer', expected_revision = preference.revision, scope = 'global',
    desired = { shell = 'system', status_thoughts = false },
  })
  assert(vim.wait(10000, function()
    local observed = request('session.command.operation.get', { operation_id = operation.id })
    return observed.state == 'completed' or observed.state == 'failed'
  end, 10), 'portable settings did not settle')
  assert(request('session.command.operation.get', { operation_id = operation.id }).state == 'completed')
  assert(vim.wait(3000, function() return harness.provider_switch_available() end, 10))
  local function switch_provider(provider, destination)
    assert(vim.wait(10000, harness.provider_switch_available, 10), 'provider controls did not become available: '
      .. vim.inspect({ busy = session.harness.busy, configuring = session.harness.configuring,
        switching = session.harness.switching_provider, syncing = session.harness.state_sync_pending,
        operation = session.harness.task_operation, status = session.harness.status,
        queue = session.harness.queue, steering = session.harness.pending_steer,
        submission = session.harness.pending_submit }))
    harness.switch_provider(provider, destination)
  end
  local function settle(provider)
    assert(vim.wait(10000, function()
      local state = session.harness
      return #errors > 0 or (state.ready and not state.switching_provider and state.session and state.session.provider == provider)
    end, 10), 'provider switch did not settle')
    assert(#errors == 0, table.concat(errors, '\n'))
    assert(vim.api.nvim_get_current_tabpage() == source_tab and #vim.api.nvim_list_tabpages() == tab_count)
    assert(session.harness_by_id[session.harness.session.id] == session.harness)
    local transferred = request('settings.get', {})
    assert(transferred.desired.shell == 'system')
    assert(transferred.desired.status_thoughts == false)
    assert(session.harness.status_thoughts == false)
  end
  require('forge.views.harness.settings').open(session.harness, {
    window_list = { session.harness.transcript_win, session.harness.composer_win },
    control_win = session.harness.composer_win,
  })
  assert(vim.wait(3000, function() return picker.is_open('harness-config') end, 10))
  local config_picker = picker._state_for_test()
  local provider_option
  for _, option in ipairs(config_picker.spec.page_list[1].option_list) do
    if option.id == 'provider' then provider_option = option break end
  end
  assert(provider_option, 'configuration omitted provider selection')
  config_picker.spec.on_field_action({ field = { id = 'provider' }, option = provider_option }, 'activate', 1)
  local provider_picker = picker._state_for_test()
  for _, option in ipairs(provider_picker.spec.page_list[1].option_list) do
    if option.value == alternate_provider then provider_picker.spec.on_confirm({ option = option }) break end
  end
  local destination_picker = picker._state_for_test()
  destination_picker.spec.on_confirm({ option = destination_picker.spec.page_list[1].option_list[1] })
  picker.close(false)
  settle(alternate_provider)
  local chosen_id = session.harness.session.id
  assert(chosen_id ~= source_id)
  local created
  client.request({ method = 'session.new', params = { name = 'newer chat' }, session_id = assert(require("forge.session").harness.session).id }, function(result, request_error)
    assert(not request_error, request_error)
    created = result
  end)
  assert(vim.wait(3000, function() return created ~= nil end, 10))
  controller.activate_snapshot(created)
  assert(created.session.id ~= chosen_id)
  switch_provider(initial_provider, { kind = 'resume', session_id = source_id })
  settle(initial_provider)
  assert(session.harness.session.id == source_id)
  switch_provider(alternate_provider, { kind = 'resume', session_id = chosen_id })
  settle(alternate_provider)
  assert(session.harness.session.id == chosen_id, 'resumed latest instead of the selected session')
  for _, provider in ipairs(provider_ids) do
    if provider ~= initial_provider and provider ~= alternate_provider then
      switch_provider(provider, { kind = 'new' })
      settle(provider)
      assert(session.harness.session.model == 'default', 'provider switch copied a foreign model')
    end
  end
  switch_provider(alternate_provider, { kind = 'resume', session_id = chosen_id })
  settle(alternate_provider)
  local source_provider = session.harness.session.provider
  config.options.harness.providers[initial_provider].command = {}
  switch_provider(initial_provider, { kind = 'new' })
  assert(vim.wait(10000, function() return #errors > 0 and session.harness.ready and not session.harness.switching_provider end, 10))
  assert(session.harness.session.provider == source_provider and config.options.harness.provider == source_provider)
end, debug.traceback)
picker.close(false)
client.host_stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or 'host was not collected'), '\n')
  vim.cmd('cquit 1')
end
print('harness_provider_switch_host: passed (native host and storage, provider model discovery disabled)')
vim.cmd('qa!')
