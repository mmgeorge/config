local script_path = debug.getinfo(1, 'S').source:sub(2)
local tests_dir = script_path:match('^(.*)[/\\][^/\\]+$')
local wezterm_dir = tests_dir:match('^(.*)[/\\]tests$')
package.path = wezterm_dir .. '/?.lua;' .. package.path

local branch_source = require 'worktree_branch'

local function assert_equal(actual, expected, message)
  if actual ~= expected then
    error(message .. ': expected ' .. tostring(expected) .. ', got ' .. tostring(actual))
  end
end

local choices = branch_source.source_choices()
assert_equal(choices[1].id, 'local', 'local source must remain first')
assert_equal(choices[2].id, 'remote', 'remote source must remain second')

local fetch_args = branch_source.remote_fetch_args('origin')
assert_equal(table.concat(fetch_args, ' '),
  'fetch --prune --no-tags --no-recurse-submodules origin',
  'remote refresh must stay scoped to one remote')

assert_equal(branch_source.ref_namespace('remote', 'upstream'), 'refs/remotes/upstream',
  'remote namespace must stay scoped to the selected remote')
assert_equal(branch_source.ref_namespace('local'), 'refs/heads',
  'local namespace must avoid remote refs')
assert_equal(branch_source.logical_name('origin/feature/demo', 'remote'), 'feature/demo',
  'remote logical names must preserve branch slashes')
assert_equal(branch_source.logical_name('local-only', 'local'), 'local-only',
  'local logical names must remain unchanged')

assert_equal(branch_source.existing_remote_action({ local_exists = false }), 'create_tracking',
  'missing local branch must create a tracking branch')
assert_equal(branch_source.existing_remote_action({ local_exists = true, checked_out = true }),
  'blocked_checked_out', 'checked-out local branch must not move')
assert_equal(branch_source.existing_remote_action({ local_exists = true, same_tip = true }),
  'use_local', 'matching local branch must be reused')
assert_equal(branch_source.existing_remote_action({ local_exists = true, local_is_ancestor = true }),
  'fast_forward', 'behind local branch must fast-forward')
assert_equal(branch_source.existing_remote_action({ local_exists = true }), 'blocked_diverged',
  'ahead or divergent local branch must not move')

local current_action
local command_history = {}
local fake_action = {}
local notifications = {}
local active_workspace = 'default'
local active_tab_title
local fake_repository = {
  common_dir = 'D:/repo/.git',
  pane_path = 'D:/repo',
  worktrees = 'worktree D:/repo\nHEAD abc123\nbranch refs/heads/main\n\n',
}

function fake_action.InputSelector(spec)
  return { kind = 'selector', spec = spec }
end

function fake_action.PromptInputLine(spec)
  return { kind = 'prompt', spec = spec }
end

function fake_action.SwitchToWorkspace(spec)
  return { kind = 'workspace', spec = spec }
end

local function command_contains(command, expected)
  return table.concat(command, ' '):find(expected, 1, true) ~= nil
end

package.preload.wezterm = function()
  return {
    GLOBAL = {},
    action = fake_action,
    action_callback = function(callback)
      return callback
    end,
    config_dir = wezterm_dir,
    executable_dir = wezterm_dir,
    home_dir = wezterm_dir,
    mux = {
      get_workspace_names = function()
        return { 'default' }
      end,
    },
    run_child_process = function(command)
      table.insert(command_history, command)
      if command_contains(command, 'rev-parse --path-format=absolute --git-common-dir') then
        return true, fake_repository.common_dir .. '\n', ''
      end
      if command_contains(command, 'worktree list --porcelain') then
        return true, fake_repository.worktrees, ''
      end
      if command[1] == 'cmd.exe' then
        return true, 'yes\n', ''
      end
      if command_contains(command, ' remote') then
        return true, 'origin\n', ''
      end
      if command_contains(command, 'for-each-ref') and command_contains(command, 'refs/remotes/origin') then
        return true, 'origin\t100\trefs/remotes/origin/master\tEND\norigin/main\t100\t\tEND\norigin/coworker/demo\t90\t\tEND\norigin/behind\t80\t\tEND\n', ''
      end
      if command_contains(command, 'for-each-ref') and command_contains(command, 'refs/heads') then
        return true, 'main\t100\t\tEND\nlocal-only\t90\t\tEND\n', ''
      end
      if command_contains(command, 'show-ref --verify --quiet refs/heads/coworker/demo') then
        return false, '', ''
      end
      if command_contains(command, 'show-ref --verify --quiet refs/heads/mmgeorge/progress')
        or command_contains(command, 'show-ref --verify --quiet refs/heads/mmgeorge/failed')
      then
        return false, '', ''
      end
      if command_contains(command, 'rev-parse --verify origin/behind') then
        return true, 'remote-tip\n', ''
      end
      if command_contains(command, 'rev-parse --verify behind') then
        return true, 'local-tip\n', ''
      end
      return true, '', ''
    end,
    target_triple = 'x86_64-pc-windows-msvc',
    time = {
      call_after = function(_, callback)
        callback()
      end,
    },
  }
end

local worktree = require 'worktree'
worktree.settings.debug = false
assert_equal(type(worktree.menu), 'function', 'worktree module must load with the branch-source model')
assert_equal(worktree.settings.branch_prefix, 'mmgeorge/', 'default branch prefix must use mmgeorge')
assert_equal(worktree.settings.layouts.by_repo.maps.branch_prefix, 'matt9222/',
  'maps repository must retain the matt9222 prefix')

local fake_window = {}
function fake_window:active_workspace()
  return active_workspace
end
function fake_window:active_tab()
  return {
    set_title = function(_, title)
      active_tab_title = title
    end,
  }
end
function fake_window:mux_window()
  return {}
end
function fake_window:perform_action(action)
  current_action = action
end
function fake_window:toast_notification(_, message)
  table.insert(notifications, message)
end

local fake_pane = {}
function fake_pane:get_current_working_dir()
  return { file_path = fake_repository.pane_path }
end

local function choose_id(id)
  assert_equal(current_action.kind, 'selector', 'expected a selector before choosing ' .. id)
  current_action.spec.action(fake_window, fake_pane, id)
end

local function command_history_contains(expected)
  for _, command in ipairs(command_history) do
    if command_contains(command, expected) then
      return true
    end
  end

  return false
end

local function current_spawn_contains(expected)
  local spawn = current_action.spec and current_action.spec.spawn
  local args = spawn and spawn.args or {}
  return table.concat(args, ' '):find(expected, 1, true) ~= nil
end

local function choice_by_id(id)
  for _, choice in ipairs(current_action.spec.choices) do
    if choice.id == id then
      return choice
    end
  end

  return nil
end

local function notification_contains(expected)
  for _, message in ipairs(notifications) do
    if message:find(expected, 1, true) then
      return true
    end
  end

  return false
end

local function open_branch_source(mode)
  command_history = {}
  notifications = {}
  worktree.menu()(fake_window, fake_pane)
  choose_id('create-current')
  choose_id(mode)
end

open_branch_source('new')
choose_id('remote')
assert_equal(command_history_contains('fetch --prune --no-tags --no-recurse-submodules origin'), true,
  'new branch remote source must refresh only the selected remote')
assert_equal(current_action.spec.title, 'Start point from origin for repo',
  'new branch remote source must show refreshed remote refs')
assert_equal(choice_by_id('origin'), nil,
  'remote source must omit the symbolic remote HEAD')
choose_id('origin/main')
assert_equal(current_action.kind, 'prompt', 'new branch start point must prompt for a branch name')
current_action.spec.action(fake_window, fake_pane, 'temp')
assert_equal(command_history_contains('worktree add -b mmgeorge/temp'), false,
  'existing branch collision must stop before git worktree add')
assert_equal(notification_contains('Branch mmgeorge/temp already exists'), true,
  'existing branch collision must explain how to use the local branch flow')

open_branch_source('new')
choose_id('local')
assert_equal(command_history_contains(' fetch '), false,
  'new branch local source must not fetch')
assert_equal(current_action.spec.title, 'Start point from local for repo',
  'new branch local source must show local refs')

choose_id('main')
current_action.spec.action(fake_window, fake_pane, 'progress')
assert_equal(current_action.kind, 'workspace', 'new local branch must switch to a progress workspace immediately')
assert_equal(current_action.spec.name, 'repo:mmgeorge/progress',
  'progress workspace must use the future worktree branch name')
assert_equal(current_action.spec.spawn.cwd, 'D:/repo',
  'progress runner must start from the existing repository path')
assert_equal(current_spawn_contains(
  'create-worktree.ps1 -RepositoryPath D:/repo -WorktreePath D:/mmgeorge/progress -StartPoint main -NewBranch mmgeorge/progress'), true,
  'progress workspace must receive the complete worktree creation request')
assert_equal(command_history_contains('worktree add -b mmgeorge/progress'), false,
  'Lua must not block on git worktree add before switching workspaces')

fake_repository.worktrees = table.concat({
  'worktree D:/repo',
  'HEAD abc123',
  'branch refs/heads/main',
  '',
  'worktree D:/mmgeorge/progress',
  'HEAD def456',
  'branch refs/heads/mmgeorge/progress',
  '',
}, '\n')
active_workspace = 'repo:mmgeorge/progress'
worktree.handle_user_var_changed(fake_window, fake_pane, 'WORKTREE_CREATE_READY', active_workspace)
worktree.handle_update_status(fake_window, fake_pane)
assert_equal(active_tab_title, 'main',
  'successful progress runner completion must apply the repository layout')
active_workspace = 'default'

open_branch_source('new')
choose_id('local')
choose_id('main')
current_action.spec.action(fake_window, fake_pane, 'failed')
active_workspace = 'repo:mmgeorge/failed'
worktree.handle_user_var_changed(fake_window, fake_pane, 'WORKTREE_CREATE_FAILED', active_workspace)
assert_equal(notification_contains('Worktree creation failed'), true,
  'failed progress runner completion must remain visible to the user')
active_workspace = 'default'

open_branch_source('existing')
choose_id('remote')
assert_equal(command_history_contains('fetch --prune --no-tags --no-recurse-submodules origin'), true,
  'existing remote source must refresh only the selected remote')
assert_equal(current_action.spec.title, 'Existing branch from origin for repo',
  'existing remote source must show refreshed remote refs')
choose_id('origin/coworker/demo')
local recorded_commands = {}
for _, command in ipairs(command_history) do
  table.insert(recorded_commands, table.concat(command, ' '))
end
assert_equal(current_action.kind, 'workspace',
  'newly discovered remote branch must open a progress workspace')
assert_equal(current_spawn_contains(
  '-StartPoint origin/coworker/demo -NewBranch coworker/demo'), true,
  'newly discovered remote branch must pass its tracking branch request to the progress runner\n'
    .. table.concat(recorded_commands, '\n') .. '\nspawn: ' .. table.concat(current_action.spec.spawn.args, ' '))

open_branch_source('existing')
choose_id('remote')
local existing_remote_choice_ids = {}
for _, choice in ipairs(current_action.spec.choices) do
  table.insert(existing_remote_choice_ids, choice.id)
end
assert_equal(choice_by_id('origin/behind') ~= nil, true,
  'existing remote source must include origin/behind: ' .. table.concat(existing_remote_choice_ids, ', '))
choose_id('origin/behind')
recorded_commands = {}
for _, command in ipairs(command_history) do
  table.insert(recorded_commands, table.concat(command, ' '))
end
assert_equal(command_history_contains('branch --force behind origin/behind'), true,
  'remote source must fast-forward a matching local branch that is behind\n' .. table.concat(recorded_commands, '\n'))

open_branch_source('existing')
choose_id('local')
assert_equal(command_history_contains(' fetch '), false,
  'existing local source must not fetch')
assert_equal(current_action.spec.title, 'Existing branch from local for repo',
  'existing local source must show local refs')
choose_id('local-only')
assert_equal(current_action.kind, 'workspace',
  'existing local branch must open a progress workspace')
assert_equal(current_spawn_contains('-StartPoint local-only -WorkspaceName repo:local-only'), true,
  'existing local branch must pass its local tip to the progress runner')
assert_equal(current_spawn_contains('-NewBranch'), false,
  'existing local branch must not ask the progress runner to create a branch')

fake_repository = {
  common_dir = 'D:/code/test-wt/.bare',
  pane_path = '/D:/code/test-wt',
  worktrees = table.concat({
    'worktree D:/code/test-wt/.bare',
    'bare',
    '',
    'worktree D:/code/test-wt/master',
    'HEAD abc123',
    'branch refs/heads/master',
    '',
  }, '\n'),
}
worktree.menu()(fake_window, fake_pane)
assert_equal(choice_by_id('create-current') ~= nil, true,
  'bare repository root must offer new worktree creation for the current repository')
assert_equal(command_history_contains('git -C D:/code/test-wt rev-parse'), true,
  'Windows file URL paths must lose their leading slash before Git runs')
local main_worktree_choice = choice_by_id('D:/code/test-wt/master')
assert_equal(main_worktree_choice ~= nil, true,
  'bare repository root must list its main worktree')
assert_equal(main_worktree_choice.label:sub(1, 2), '* ',
  'bare repository root must resolve its main worktree as current')
assert_equal(choice_by_id('D:/code/test-wt/.bare'), nil,
  'bare Git storage must not appear as a worktree')

fake_repository.pane_path = 'D:/code/test-wt/.bare'
worktree.menu()(fake_window, fake_pane)
assert_equal(choice_by_id('remove-current'), nil,
  'bare Git storage must not resolve as a removable current worktree')
