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
client._set_launcher_for_test(vim.system)
local harness = require('forge.views.harness')
local controller = require('forge.views.harness.controller')
local session = require('forge.session')
local picker = require('forge.views.picker')
local errors = {}
require('forge.infra.notifications').error = function(message) errors[#errors + 1] = message .. '\n' .. debug.traceback() end
controller.resolve_runtime_model = function() end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require('forge').setup({ harness = { backend = 'codex' } })
  harness.open()
  assert(vim.wait(10000, function() return (session.harness.ready and session.harness.session) or #errors > 0 end, 10))
  assert(#errors == 0, table.concat(errors, '\n'))
  local source_id = session.harness.session.id
  local source_tab = vim.api.nvim_get_current_tabpage()
  local tab_count = #vim.api.nvim_list_tabpages()
  local function request(method, params)
    local result, failure, completed
    client.request(method, params, function(value, request_error)
      result, failure, completed = value, request_error, true
    end)
    assert(vim.wait(10000, function() return completed end, 10), method .. ' did not complete')
    assert(not failure, failure)
    return result
  end
  local preference = request('settings.get', {})
  local operation = request('settings.update', {
    operation_id = 'provider-transfer', expected_revision = preference.revision, scope = 'global',
    desired = { shell = 'system', default_write_permission = 'yolo', plan_auto_approve_revisions = false },
  })
  assert(vim.wait(10000, function()
    local observed = request('task.operation', { operation_id = operation.id })
    return observed.state == 'completed' or observed.state == 'failed'
  end, 10), 'portable settings did not settle')
  assert(request('task.operation', { operation_id = operation.id }).state == 'completed')
  assert(vim.wait(3000, function() return harness.backend_switch_available() end, 10))
  local function settle(backend)
    assert(vim.wait(10000, function()
      local state = session.harness
      return #errors > 0 or (state.ready and not state.switching_backend and state.session and state.session.backend == backend)
    end, 10), 'provider switch did not settle')
    assert(#errors == 0, table.concat(errors, '\n'))
    assert(vim.api.nvim_get_current_tabpage() == source_tab and #vim.api.nvim_list_tabpages() == tab_count)
    assert(session.harness_by_id[session.harness.session.id] == session.harness)
    local transferred = request('settings.get', {})
    assert(transferred.desired.shell == 'system' and transferred.desired.default_write_permission == 'yolo')
    assert(transferred.desired.plan_auto_approve_revisions == false)
    assert(session.harness.session.default_write_permission == 'yolo')
    assert(session.harness.session.plan_auto_approve_revisions == false)
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
  config_picker.spec.on_confirm({ option = provider_option })
  local provider_picker = picker._state_for_test()
  for _, option in ipairs(provider_picker.spec.page_list[1].option_list) do
    if option.value == 'copilot' then provider_picker.spec.on_confirm({ option = option }) break end
  end
  local destination_picker = picker._state_for_test()
  destination_picker.spec.on_confirm({ option = destination_picker.spec.page_list[1].option_list[1] })
  picker.close(false)
  settle('copilot')
  local chosen_id = session.harness.session.id
  assert(chosen_id ~= source_id)
  local created
  client.request('session.new', { name = 'newer Copilot chat' }, function(result, request_error)
    assert(not request_error, request_error)
    created = result
  end)
  assert(vim.wait(3000, function() return created ~= nil end, 10))
  controller.activate_snapshot(created)
  assert(created.session.id ~= chosen_id)
  harness.switch_backend('codex', { kind = 'resume', session_id = source_id })
  settle('codex')
  assert(session.harness.session.id == source_id)
  harness.switch_backend('copilot', { kind = 'resume', session_id = chosen_id })
  settle('copilot')
  assert(session.harness.session.id == chosen_id, 'resumed latest instead of the selected session')
  for _, backend in ipairs({ 'claude', 'antigravity' }) do
    harness.switch_backend(backend, { kind = 'new' })
    settle(backend)
    assert(session.harness.session.model == 'default', 'provider switch copied a foreign model')
  end
  harness.switch_backend('copilot', { kind = 'resume', session_id = chosen_id })
  settle('copilot')
  local config = require('forge.infra.config')
  config.options.harness.backends.codex.command = {}
  harness.switch_backend('codex', { kind = 'new' })
  assert(vim.wait(10000, function() return #errors > 0 and session.harness.ready and not session.harness.switching_backend end, 10))
  assert(session.harness.session.id == chosen_id and config.options.harness.backend == 'copilot')
  assert(errors[1]:find('Codex backend requires', 1, true), errors[1])
end, debug.traceback)
picker.close(false)
client.stop()
local collected = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
if not success or not collected then
  io.stderr:write(tostring(failure or 'host was not collected'), '\n')
  vim.cmd('cquit 1')
end
print('harness_provider_switch_host: passed (native host and storage, provider model discovery disabled)')
vim.cmd('qa!')
