local wezterm = require 'wezterm'
local act = wezterm.action

local M = {}

local is_windows = wezterm.target_triple:find('windows') ~= nil

M.settings = {
  roots = {
    wezterm.home_dir .. '/Developer',
    wezterm.home_dir .. '/code',
    wezterm.home_dir .. '/dev',
    -- wezterm.home_dir .. '/src',
    -- wezterm.home_dir .. '/worktrees',
    -- wezterm.home_dir .. '/projects',
  },
  auto_setup_new_workspaces = true,
  branch_prefix = 'matt9222/',
  layouts = {
    default = {
      tabs = {
        { title = 'main' },
      },
    },
    -- Add repo-specific layouts here, keyed by the repo directory name.
    by_repo = {
      ['maps'] = {
        env = {
          FOO = 1
        },
        tabs = {
          { title = 'main',
            focus = true,
            first_run_commands = {
              commands = {
                'nvim',
              },
            },
            panes = {
              {
                direction = 'Bottom',
                size = 0.25,
                first_run_commands = {
                  commands = {
                    'git submodule update --init --recursive; pnpm install; pnpm run codegen',
                  },
                },
              },
            },
          },
          { title = 'copilot',
            first_run_commands = {
              commands = {
                'load-env { "COPILOT_CUSTOM_INSTRUCTIONS_DIRS": "/Users/matt9222/Developer/maps/prompts" }; co',
              },
            },
          },
        },
        branch_prefix = 'matt9222/',
      },
    },
  },
}

local function trim(text)
  if not text then
    return ''
  end

  return text:gsub('^%s+', ''):gsub('%s+$', '')
end

local function normalize_path(path)
  if not path or path == '' then
    return nil
  end

  path = path:gsub('\\', '/')

  if path ~= '/' and not path:match('^%a:/$') then
    path = path:gsub('/+$', '')
  end

  return path
end

local function is_absolute(path)
  return path and (path:sub(1, 1) == '/' or path:match('^%a:/'))
end

local function join_path(base, child)
  base = normalize_path(base)
  child = normalize_path(child)

  if not base or base == '' then
    return child
  end

  if not child or child == '' then
    return base
  end

  if is_absolute(child) then
    return child
  end

  if base == '/' or base:match('^%a:/$') then
    return normalize_path(base .. child)
  end

  return normalize_path(base .. '/' .. child)
end

local function dirname(path)
  path = normalize_path(path)
  if not path then
    return nil
  end

  if path == '/' or path:match('^%a:/$') then
    return path
  end

  local parent = path:match('^(.*)/[^/]+$')
  if not parent or parent == '' then
    return '.'
  end

  if parent:match('^%a:$') then
    return parent .. '/'
  end

  return parent
end

local function basename(path)
  path = normalize_path(path)
  if not path then
    return nil
  end

  return path:match('([^/]+)$') or path
end

local function split_lines(text)
  local result = {}
  if not text then
    return result
  end

  text = text:gsub('\r', '')
  for line in text:gmatch('[^\n]+') do
    table.insert(result, line)
  end

  return result
end

local function notify(window, message)
  window:toast_notification('Worktree', message, nil, 4000)
end

local function run_command(command)
  local success, stdout, stderr = wezterm.run_child_process(command)
  if not success then
    return nil, trim(stderr ~= '' and stderr or stdout)
  end

  return stdout or '', stderr or ''
end

local function git_output(cwd, args)
  local command = { 'git' }

  if cwd and cwd ~= '' then
    table.insert(command, '-C')
    table.insert(command, cwd)
  end

  for _, arg in ipairs(args) do
    table.insert(command, arg)
  end

  return run_command(command)
end

local function git_capture(cwd, args)
  local stdout, stderr = git_output(cwd, args)
  if not stdout then
    return nil, stderr
  end

  return trim(stdout), stderr
end

local function wezterm_executable()
  return join_path(wezterm.executable_dir, is_windows and 'wezterm.exe' or 'wezterm')
end

local function list_child_directories(root)
  root = normalize_path(root)
  if not root then
    return {}
  end

  local stdout
  if is_windows then
    stdout = run_command({
      'cmd.exe',
      '/S',
      '/C',
      'dir /B /A:D "' .. root .. '"',
    })
  else
    stdout = run_command({
      'find',
      root,
      '-mindepth',
      '1',
      '-maxdepth',
      '1',
      '-type',
      'd',
    })
  end

  if not stdout then
    return {}
  end

  local results = {}
  for _, line in ipairs(split_lines(stdout)) do
    if is_windows then
      table.insert(results, join_path(root, line))
    else
      table.insert(results, normalize_path(line))
    end
  end

  return results
end

local function path_exists(path)
  path = normalize_path(path)
  if not path then
    return false
  end

  if is_windows then
    local stdout = run_command({
      'cmd.exe',
      '/S',
      '/C',
      'if exist "' .. path .. '" (echo yes) else (echo no)',
    })
    return stdout ~= nil and trim(stdout) == 'yes'
  end

  return run_command({ 'test', '-e', path }) ~= nil
end

local function directory_exists(path)
  path = normalize_path(path)
  if not path then
    return false
  end

  if is_windows then
    local stdout = run_command({
      'cmd.exe',
      '/S',
      '/C',
      'if exist "' .. path .. '\\NUL" (echo yes) else (echo no)',
    })
    return stdout ~= nil and trim(stdout) == 'yes'
  end

  return run_command({ 'test', '-d', path }) ~= nil
end

local function remove_directory_tree(path)
  path = normalize_path(path)
  if not path or not path_exists(path) then
    return true
  end

  local ok, err
  if is_windows then
    ok, err = run_command({
      'cmd.exe',
      '/S',
      '/C',
      'rmdir /S /Q "' .. path .. '"',
    })
  else
    ok, err = run_command({ 'rm', '-rf', path })
  end

  if ok == nil and path_exists(path) then
    return false, err
  end

  if path_exists(path) then
    return false, 'Could not remove directory ' .. path
  end

  return true
end

local function current_pane_path(pane)
  local cwd = pane:get_current_working_dir()
  if not cwd or not cwd.file_path then
    return nil
  end

  return normalize_path(cwd.file_path)
end

local function git_common_dir(path)
  local common_dir = git_capture(path, {
    'rev-parse',
    '--path-format=absolute',
    '--git-common-dir',
  })

  if not common_dir then
    common_dir = git_capture(path, { 'rev-parse', '--git-common-dir' })
  end

  if not common_dir or common_dir == '' then
    return nil
  end

  common_dir = normalize_path(common_dir)
  if not is_absolute(common_dir) then
    common_dir = join_path(path, common_dir)
  end

  return common_dir
end

local function parse_worktrees(path)
  local stdout = git_output(path, { 'worktree', 'list', '--porcelain' })
  if not stdout then
    return nil
  end

  local worktrees = {}
  local current = nil

  local function flush()
    if not current or not current.path then
      return
    end

    current.path = normalize_path(current.path)
    if current.branch then
      current.branch_name = current.branch:gsub('^refs/heads/', '')
    end

    table.insert(worktrees, current)
    current = nil
  end

  for line in (stdout .. '\n'):gmatch('(.-)\n') do
    line = line:gsub('\r$', '')

    if line == '' then
      flush()
    elseif line == 'bare' or line == 'detached' or line == 'locked' or line == 'prunable' then
      current = current or {}
      current[line] = true
    else
      local key, value = line:match('^(%S+)%s+(.*)$')
      if key == 'worktree' then
        flush()
        current = { path = value }
      elseif key then
        current = current or {}
        current[key] = value
      end
    end
  end

  return worktrees
end

local function worktree_workspace_name(repo_name, worktree)
  local suffix = worktree.branch_name or basename(worktree.path) or 'worktree'
  return repo_name .. ':' .. suffix
end

local function preferred_worktree_parent(worktrees, fallback)
  local counts = {}
  local best_path = fallback
  local best_count = 0

  for _, worktree in ipairs(worktrees) do
    if not worktree.is_main then
      local parent = dirname(worktree.path)
      counts[parent] = (counts[parent] or 0) + 1
      if counts[parent] > best_count then
        best_count = counts[parent]
        best_path = parent
      end
    end
  end

  return best_path
end

local function main_worktree_path(worktrees)
  for _, worktree in ipairs(worktrees) do
    if not worktree.bare and directory_exists(join_path(worktree.path, '.git')) then
      return worktree.path
    end
  end

  for _, worktree in ipairs(worktrees) do
    if not worktree.bare then
      return worktree.path
    end
  end

  return worktrees[1] and worktrees[1].path or nil
end

local function should_display_worktree(worktree)
  return worktree ~= nil and not worktree.bare
end

local function build_repo(path, common_dir)
  path = normalize_path(path)
  if not path then
    return nil
  end

  common_dir = common_dir or git_common_dir(path)
  if not common_dir then
    return nil
  end

  local worktrees = parse_worktrees(path)
  if not worktrees or #worktrees == 0 then
    return nil
  end

  local repo = {
    id = common_dir,
    name = basename(dirname(common_dir)),
    main_path = main_worktree_path(worktrees),
    worktrees = worktrees,
  }

  for _, worktree in ipairs(repo.worktrees) do
    worktree.is_main = should_display_worktree(worktree) and worktree.path == repo.main_path
    worktree.repo_id = repo.id
    worktree.repo_name = repo.name
    worktree.repo_main_path = repo.main_path
    worktree.workspace_name = worktree_workspace_name(repo.name, worktree)
  end

  repo.worktree_parent = preferred_worktree_parent(
    repo.worktrees,
    dirname(repo.main_path)
  )

  return repo
end

local function sort_repos(repos)
  table.sort(repos, function(left, right)
    return left.name < right.name
  end)
end

local function sort_worktrees(worktrees)
  table.sort(worktrees, function(left, right)
    if left.repo_name == right.repo_name then
      local left_key = left.branch_name or left.path
      local right_key = right.branch_name or right.path
      return left_key < right_key
    end

    return left.repo_name < right.repo_name
  end)
end

local function discover_repositories(extra_paths)
  local candidates = {}
  local seen_candidates = {}

  local function add_candidate(path)
    path = normalize_path(path)
    if not path or seen_candidates[path] then
      return
    end

    seen_candidates[path] = true
    table.insert(candidates, path)
  end

  for _, root in ipairs(M.settings.roots) do
    add_candidate(root)
    for _, child in ipairs(list_child_directories(root)) do
      add_candidate(child)
    end
  end

  for _, path in ipairs(extra_paths or {}) do
    add_candidate(path)
  end

  local repo_candidates = {}
  for _, candidate in ipairs(candidates) do
    local common_dir = git_common_dir(candidate)
    if common_dir and not repo_candidates[common_dir] then
      repo_candidates[common_dir] = candidate
    end
  end

  local repos = {}
  for common_dir, candidate in pairs(repo_candidates) do
    local repo = build_repo(candidate, common_dir)
    if repo then
      table.insert(repos, repo)
    end
  end

  sort_repos(repos)
  return repos
end

local function repo_for_pane(pane)
  local path = current_pane_path(pane)
  if not path then
    return nil
  end

  return build_repo(path)
end

local function flatten_worktrees(repos)
  local worktrees = {}
  for _, repo in ipairs(repos) do
    for _, worktree in ipairs(repo.worktrees) do
      if should_display_worktree(worktree) then
        table.insert(worktrees, worktree)
      end
    end
  end

  sort_worktrees(worktrees)
  return worktrees
end

local function find_worktree_by_path(repos, path)
  path = normalize_path(path)
  for _, repo in ipairs(repos) do
    for _, worktree in ipairs(repo.worktrees) do
      if worktree.path == path then
        return worktree, repo
      end
    end
  end

  return nil, nil
end

local function workspace_exists(name)
  for _, workspace in ipairs(wezterm.mux.get_workspace_names()) do
    if workspace == name then
      return true
    end
  end

  return false
end

local function pending_layouts()
  wezterm.GLOBAL.worktree_pending_layouts = wezterm.GLOBAL.worktree_pending_layouts or {}
  return wezterm.GLOBAL.worktree_pending_layouts
end

local function pending_removals()
  wezterm.GLOBAL.worktree_pending_removals = wezterm.GLOBAL.worktree_pending_removals or {}
  return wezterm.GLOBAL.worktree_pending_removals
end

local function pending_session_removals()
  wezterm.GLOBAL.worktree_pending_session_removals = wezterm.GLOBAL.worktree_pending_session_removals or {}
  return wezterm.GLOBAL.worktree_pending_session_removals
end

local function repo_settings(repo_name)
  local layouts = M.settings.layouts or {}
  local by_repo = layouts.by_repo or {}
  return by_repo[repo_name] or {}
end

local function normalize_first_run(first_run)
  if not first_run then
    return nil
  end

  if type(first_run) ~= 'table' then
    return {
      commands = { tostring(first_run) },
      target = nil,
    }
  end

  if first_run.commands ~= nil or first_run.target ~= nil then
    local commands = first_run.commands or {}
    if type(commands) ~= 'table' then
      commands = { tostring(commands) }
    end

    return {
      commands = commands,
      target = first_run.target,
    }
  end

  return {
    commands = first_run,
    target = nil,
  }
end

local function first_run_commands_for_spec(spec)
  if not spec then
    return nil
  end

  local normalized = normalize_first_run(spec.first_run_commands or spec.first_run)
  if not normalized or not normalized.commands or #normalized.commands == 0 then
    return nil
  end

  return normalized.commands
end

local function resolve_layout(worktree)
  local layouts = M.settings.layouts or {}
  local default_layout = layouts.default or {}
  local repo_layout = repo_settings(worktree.repo_name)
  local first_run = repo_layout.first_run_commands
  if first_run == nil then
    first_run = repo_layout.first_run
  end
  if first_run == nil then
    first_run = default_layout.first_run_commands
  end
  if first_run == nil then
    first_run = default_layout.first_run
  end

  return {
    tabs = repo_layout.tabs or default_layout.tabs,
    first_run = normalize_first_run(first_run),
  }
end

local function resolve_layout_cwd(worktree_path, cwd)
  cwd = cwd or '.'
  if cwd == '.' then
    return worktree_path
  end

  if is_absolute(cwd) then
    return normalize_path(cwd)
  end

  return join_path(worktree_path, cwd)
end

local function startup_command_for_spec(worktree_path, spec)
  if not spec then
    return nil
  end

  local commands = {}
  if spec.cwd and normalize_path(resolve_layout_cwd(worktree_path, spec.cwd)) ~= normalize_path(worktree_path) then
    table.insert(commands, wezterm.shell_join_args({ 'cd', resolve_layout_cwd(worktree_path, spec.cwd) }))
  end

  for _, command in ipairs(first_run_commands_for_spec(spec) or {}) do
    table.insert(commands, command)
  end

  if spec.args and #spec.args > 0 then
    table.insert(commands, 'exec ' .. wezterm.shell_join_args(spec.args))
  end

  if #commands == 0 then
    return nil
  end

  return table.concat(commands, ' && ')
end

local function apply_tab_layout(tab, pane, worktree_path, spec)
  local tab_state = {
    panes = {
      {
        pane = pane,
        spec = spec,
        startup_command = startup_command_for_spec(worktree_path, spec),
      },
    },
    focus_pane = spec.focus and pane or nil,
    root_pane = pane,
    root_spec = spec,
    title = spec.title,
  }

  if spec.title and tab then
    tab:set_title(spec.title)
  end

  for _, pane_spec in ipairs(spec.panes or {}) do
    local new_pane = pane:split({
      cwd = resolve_layout_cwd(worktree_path, pane_spec.cwd),
      direction = pane_spec.direction or 'Right',
      size = pane_spec.size or 0.5,
      top_level = pane_spec.top_level,
    })
    table.insert(tab_state.panes, {
      pane = new_pane,
      spec = pane_spec,
      startup_command = startup_command_for_spec(worktree_path, pane_spec),
    })

    if pane_spec.focus then
      tab_state.focus_pane = new_pane
    end
  end

  return tab_state
end

local function first_run_target_pane(tab_state, first_run)
  if not first_run or not first_run.commands or #first_run.commands == 0 then
    return nil
  end

  local root_starts_program = tab_state.root_spec and tab_state.root_spec.args and #tab_state.root_spec.args > 0
  if root_starts_program then
    for index = 2, #tab_state.panes do
      local pane_info = tab_state.panes[index]
      if not pane_info.spec.args or #pane_info.spec.args == 0 then
        return pane_info.pane
      end
    end
  end

  return tab_state.root_pane
end

local function find_tab_state(tab_states, target)
  if target == nil then
    return tab_states[1]
  end

  if type(target) == 'number' then
    return tab_states[target]
  end

  if type(target) == 'string' then
    for _, tab_state in ipairs(tab_states) do
      if tab_state.title == target then
        return tab_state
      end
    end
  end

  return nil
end

local function resolve_first_run_pane(tab_states, first_run)
  if not first_run or not first_run.commands or #first_run.commands == 0 then
    return nil, nil
  end

  local target = first_run.target
  if not target then
    return first_run_target_pane(tab_states[1], first_run), nil
  end

  if type(target) ~= 'table' then
    target = { tab = target }
  end

  local tab_state = find_tab_state(tab_states, target.tab)
  if not tab_state then
    return first_run_target_pane(tab_states[1], first_run), 'Could not resolve first_run_commands target tab'
  end

  local pane_index = target.pane or 1
  local pane_info = tab_state.panes[pane_index]
  if not pane_info then
    return first_run_target_pane(tab_state, first_run), 'Could not resolve first_run_commands target pane'
  end

  return pane_info.pane, nil
end

local function add_startup_command(startup_commands, pane, command, opts)
  opts = opts or {}
  if not pane or not command or command == '' then
    return
  end

  for _, startup in ipairs(startup_commands) do
    if startup.pane == pane then
      if opts.prepend then
        startup.command = command .. ' && ' .. startup.command
      else
        startup.command = startup.command .. ' && ' .. command
      end
      return
    end
  end

  table.insert(startup_commands, {
    pane = pane,
    command = command,
  })
end

local function apply_layout(window, pane, worktree)
  local layout = resolve_layout(worktree)
  if not layout then
    return {
      first_run_warning = nil,
      focus_pane = nil,
      startup_commands = {},
    }
  end

  local tabs = layout.tabs or {}
  if #tabs == 0 then
    return {
      first_run_warning = nil,
      focus_pane = nil,
      startup_commands = {},
    }
  end

  local first_tab_state = apply_tab_layout(window:active_tab(), pane, worktree.path, tabs[1])
  local tab_states = { first_tab_state }

  local mux_window = window:mux_window()
  for index = 2, #tabs do
    local tab_spec = tabs[index]
    local tab, tab_pane = mux_window:spawn_tab({
      cwd = resolve_layout_cwd(worktree.path, tab_spec.cwd),
    })
    table.insert(tab_states, apply_tab_layout(tab, tab_pane, worktree.path, tab_spec))
  end

  local focus_pane = first_tab_state.focus_pane

  if #tabs > 1 and not focus_pane then
    window:perform_action(act.ActivateTab(0), pane)
  end

  local startup_commands = {}
  for _, tab_state in ipairs(tab_states) do
    if not focus_pane and tab_state.focus_pane then
      focus_pane = tab_state.focus_pane
    end

    for _, pane_info in ipairs(tab_state.panes) do
      add_startup_command(startup_commands, pane_info.pane, pane_info.startup_command)
    end
  end

  local first_run_pane, first_run_warning = resolve_first_run_pane(tab_states, worktree.first_run)
  if first_run_pane and worktree.first_run and worktree.first_run.commands and #worktree.first_run.commands > 0 then
    add_startup_command(
      startup_commands,
      first_run_pane,
      table.concat(worktree.first_run.commands, ' && '),
      { prepend = true }
    )
  end

  return {
    first_run_warning = first_run_warning,
    focus_pane = focus_pane,
    startup_commands = startup_commands,
  }
end

local function run_shell_command_in_pane(pane, command)
  if not pane or not command or command == '' then
    return
  end

  pane:send_text(command .. '\n')
end

local function queue_layout(worktree, opts)
  opts = opts or {}
  pending_layouts()[worktree.workspace_name] = {
    first_run = opts.first_run,
    path = worktree.path,
    repo_name = worktree.repo_name,
    workspace_name = worktree.workspace_name,
  }
end

local function switch_to_worktree(window, pane, worktree, opts)
  opts = opts or {}
  local exists = workspace_exists(worktree.workspace_name)
  if not exists and M.settings.auto_setup_new_workspaces then
    queue_layout(worktree, opts)
  end

  window:perform_action(
    act.SwitchToWorkspace({
      name = worktree.workspace_name,
      spawn = {
        cwd = worktree.path,
        label = worktree.workspace_name,
      },
    }),
    pane
  )
end

local function prompt(window, pane, description, callback, opts)
  opts = opts or {}
  window:perform_action(
    act.PromptInputLine({
      description = description,
      action = wezterm.action_callback(callback),
    }),
    pane
  )
end

local function after_overlay(callback)
  wezterm.time.call_after(0.05, callback)
end

local function choose(window, pane, opts, callback)
  if #opts.choices == 0 then
    return
  end

  window:perform_action(
    act.InputSelector({
      action = wezterm.action_callback(callback),
      choices = opts.choices,
      fuzzy = opts.fuzzy ~= false,
      title = opts.title,
    }),
    pane
  )
end

local function sanitize_name(name)
  local function sanitize_component(value)
    value = value:gsub('[^%w._-]+', '-')
    value = value:gsub('%-+', '-')
    value = value:gsub('^%-+', '')
    value = value:gsub('%-+$', '')
    return value
  end

  name = trim(name)
  if name == '' then
    return ''
  end

  name = name:gsub('^refs/remotes/', '')
  name = name:gsub('^refs/heads/', '')
  name = name:gsub('^[^/]+/', function(prefix)
    if prefix == 'origin/' then
      return ''
    end

    return prefix
  end)

  local first, rest = name:match('^([^/]+)/(.+)$')
  if not first then
    return sanitize_component(name)
  end

  first = sanitize_component(first)
  rest = sanitize_component(rest:gsub('/', '-'))

  if first == '' then
    return rest
  end

  if rest == '' then
    return first
  end

  return first .. '/' .. rest
end

local function resolve_branch_prefix(repo)
  if repo then
    local settings = repo_settings(repo.name)
    if settings.branch_prefix ~= nil then
      return settings.branch_prefix
    end

    if settings.new_branch_prefix ~= nil then
      return settings.new_branch_prefix
    end
  end

  if M.settings.branch_prefix ~= nil then
    return M.settings.branch_prefix
  end

  if M.settings.new_branch_prefix ~= nil then
    return M.settings.new_branch_prefix
  end

  return ''
end

local function qualify_branch_name(repo, name)
  name = trim(name)
  local prefix = resolve_branch_prefix(repo)
  if prefix == '' then
    return name
  end

  if name:find('/', 1, true) then
    return name
  end

  return prefix .. name
end

local function resolve_new_worktree_path(repo, input, suggestion)
  local value = trim(input)
  if value == '' then
    value = suggestion
  end

  value = normalize_path(value)
  if not value or value == '' then
    return nil
  end

  if is_absolute(value) then
    return value
  end

  return join_path(repo.worktree_parent, value)
end

local function default_worktree_path(repo, suggestion)
  local default_name = suggestion ~= '' and suggestion or (repo.name .. '-worktree')
  return resolve_new_worktree_path(repo, '', default_name)
end

local function find_worktree(repo, path)
  path = normalize_path(path)
  for _, worktree in ipairs(repo.worktrees) do
    if worktree.path == path then
      return worktree
    end
  end

  return nil
end

local function path_is_within(path, root)
  path = normalize_path(path)
  root = normalize_path(root)
  if not path or not root then
    return false
  end

  return path == root or path:sub(1, #root + 1) == (root .. '/')
end

local function current_worktree_for_path(repo, path)
  if not repo then
    return nil
  end

  local best_match = nil
  local best_length = -1

  for _, worktree in ipairs(repo.worktrees) do
    if path_is_within(path, worktree.path) and #worktree.path > best_length then
      best_match = worktree
      best_length = #worktree.path
    end
  end

  return best_match
end

local function create_worktree(window, pane, repo, opts)
  local args = { 'worktree', 'add' }

  if find_worktree(repo, opts.path) then
    notify(window, 'A worktree already exists at ' .. opts.path)
    return
  end

  if opts.new_branch then
    table.insert(args, '-b')
    table.insert(args, opts.new_branch)
  end

  table.insert(args, opts.path)
  table.insert(args, opts.start_point)

  local stdout, stderr = git_output(repo.main_path, args)
  if not stdout then
    notify(window, stderr)
    return
  end

  local updated_repo = build_repo(opts.path) or build_repo(repo.main_path)
  if not updated_repo then
    notify(window, 'Created the worktree, but could not refresh repository metadata')
    return
  end

  local worktree = find_worktree(updated_repo, opts.path)
  if not worktree then
    notify(window, 'Created the worktree, but could not locate it in git worktree list')
    return
  end

  switch_to_worktree(window, pane, worktree, {
    first_run = resolve_layout(worktree).first_run,
  })
end

local function existing_refs(repo)
  local checked_out = {}
  for _, worktree in ipairs(repo.worktrees) do
    if worktree.branch_name then
      checked_out[worktree.branch_name] = true
    end
  end

  local local_branch_names = {}
  local ref_records = {}

  local function logical_ref_name(branch, source)
    if source == 'remote' then
      return branch:match('^[^/]+/(.+)$') or branch
    end

    return branch
  end

  local function add_refs(ref, source)
    local output = git_capture(repo.main_path, {
      'for-each-ref',
      '--format=%(refname:short)%09%(committerdate:unix)',
      ref,
    }) or ''

    for _, line in ipairs(split_lines(output)) do
      local branch, timestamp = line:match('^(.-)%s+([0-9]+)$')
      branch = branch or line
      if branch ~= '' and not branch:match('/HEAD$') then
        local logical_name = logical_ref_name(branch, source)
        if source == 'local' then
          local_branch_names[logical_name] = true
        end

        if source ~= 'local' or not checked_out[logical_name] then
          table.insert(ref_records, {
            committed_at = tonumber(timestamp) or 0,
            id = branch,
            label = source .. ': ' .. branch,
            logical_name = logical_name,
            source = source,
          })
        end
      end
    end
  end

  add_refs('refs/heads', 'local')
  add_refs('refs/remotes', 'remote')

  table.sort(ref_records, function(left, right)
    local left_source = left.source == 'local' and 0 or 1
    local right_source = right.source == 'local' and 0 or 1
    if left_source ~= right_source then
      return left_source < right_source
    end

    local left_is_matt = left.logical_name:match('^matt9222/') ~= nil
    local right_is_matt = right.logical_name:match('^matt9222/') ~= nil
    if left_is_matt ~= right_is_matt then
      return left_is_matt
    end

    if left_is_matt and left.committed_at ~= right.committed_at then
      return left.committed_at > right.committed_at
    end

    if left.logical_name ~= right.logical_name then
      return left.logical_name < right.logical_name
    end

    return left.id < right.id
  end)

  return ref_records, checked_out, local_branch_names
end

local function start_points(repo)
  local point_records = {}
  local seen = {}
  local source_branch_names = {
    ['local'] = {},
    ['remote'] = {},
  }

  local function logical_branch_name(branch, source)
    if source == 'remote' then
      return branch:match('^[^/]+/(.+)$') or branch
    end

    return branch
  end

  local function parse_release_version(branch)
    local version = branch:match('^([%d%.]+)%-release$')
    if not version then
      return nil
    end

    local parts = {}
    for part in version:gmatch('[^.]+') do
      if not part:match('^%d+$') then
        return nil
      end

      table.insert(parts, tonumber(part))
    end

    if #parts == 0 then
      return nil
    end

    return parts
  end

  local function compare_release_versions(left, right)
    local limit = math.max(#left, #right)
    for index = 1, limit do
      local left_part = left[index] or 0
      local right_part = right[index] or 0
      if left_part ~= right_part then
        return left_part > right_part
      end
    end

    return false
  end

  local function add_ref_points(ref, source)
    local output = git_capture(repo.main_path, {
      'for-each-ref',
      '--format=%(refname:short)%09%(committerdate:unix)',
      ref,
    }) or ''

    for _, line in ipairs(split_lines(output)) do
      local branch, timestamp = line:match('^(.-)%s+([0-9]+)$')
      branch = branch or line
      if branch ~= '' and not branch:match('/HEAD$') and not seen[branch] then
        seen[branch] = true
        local logical_name = logical_branch_name(branch, source)
        source_branch_names[source][logical_name] = true
        table.insert(point_records, {
          id = branch,
          label = source .. ': ' .. branch,
          committed_at = tonumber(timestamp) or 0,
          logical_name = logical_name,
          release_version = parse_release_version(logical_name),
          source = source,
        })
      end
    end
  end

  local function source_rank(point)
    if point.source == 'local' then
      return 0
    end

    return 1
  end

  local function branch_group(point)
    local names = source_branch_names[point.source]
    if point.logical_name == 'main' then
      return 0
    end

    if not names.main and point.logical_name == 'master' then
      return 0
    end

    if point.release_version then
      return 1
    end

    if point.logical_name:match('^matt9222/') then
      return 2
    end

    return 3
  end

  add_ref_points('refs/heads', 'local')
  add_ref_points('refs/remotes', 'remote')

  table.sort(point_records, function(left, right)
    local left_source = source_rank(left)
    local right_source = source_rank(right)
    if left_source ~= right_source then
      return left_source < right_source
    end

    local left_group = branch_group(left)
    local right_group = branch_group(right)
    if left_group ~= right_group then
      return left_group < right_group
    end

    if left_group == 1 then
      if compare_release_versions(left.release_version, right.release_version) then
        return true
      end

      if compare_release_versions(right.release_version, left.release_version) then
        return false
      end
    end

    if left.logical_name ~= right.logical_name then
      return left.logical_name < right.logical_name
    end

    return left.id < right.id
  end)

  local points = {}
  for _, point in ipairs(point_records) do
    table.insert(points, {
      id = point.id,
      label = point.label,
    })
  end

  return points
end

local function choose_existing_branch(window, pane, repo)
  local refs, checked_out, local_branch_names = existing_refs(repo)
  if #refs == 0 then
    notify(window, 'No local or remote refs are available for a new worktree')
    return
  end

  local choices = {}
  local refs_by_id = {}
  for _, ref in ipairs(refs) do
    local choice_id = ref.source .. '\t' .. ref.id
    refs_by_id[choice_id] = ref
    table.insert(choices, {
      id = choice_id,
      label = ref.label,
    })
  end

  choose(window, pane, {
    choices = choices,
    title = 'Existing branch or remote for ' .. repo.name,
  }, function(inner_window, inner_pane, id)
    if not id then
      return
    end

    local ref = refs_by_id[id]
    if not ref then
      notify(inner_window, 'Could not resolve the selected ref')
      return
    end

    local target_branch = ref.logical_name
    local create_opts = {
      start_point = ref.id,
    }

    if ref.source == 'remote' then
      if local_branch_names[target_branch] then
        if checked_out[target_branch] then
          notify(inner_window, 'Local branch ' .. target_branch .. ' is already checked out; use the new-branch path instead')
          return
        end

        create_opts.start_point = target_branch
      else
        create_opts.new_branch = target_branch
      end
    end

    after_overlay(function()
      create_opts.path = default_worktree_path(repo, sanitize_name(target_branch))
      create_worktree(inner_window, pane, repo, create_opts)
    end)
  end)
end

local function choose_new_branch(window, pane, repo)
  local points = start_points(repo)
  if #points == 0 then
    notify(window, 'No local or remote branches were found for ' .. repo.name)
    return
  end

  choose(window, pane, {
    choices = points,
    title = 'Start point for new branch in ' .. repo.name,
  }, function(inner_window, inner_pane, start_point)
    if not start_point then
      return
    end

    after_overlay(function()
      prompt(
        inner_window,
        pane,
        'New branch name from ' .. start_point,
        function(branch_window, branch_pane, line)
          if line == nil then
            return
          end

          local new_branch = qualify_branch_name(repo, line)
          if new_branch == '' then
            notify(branch_window, 'A new branch name is required')
            return
          end

          if new_branch:sub(-1) == '/' then
            notify(branch_window, 'A branch name is required after ' .. (resolve_branch_prefix(repo) or 'the prefix'))
            return
          end

          after_overlay(function()
            create_worktree(branch_window, pane, repo, {
              new_branch = new_branch,
              path = default_worktree_path(repo, sanitize_name(new_branch)),
              start_point = start_point,
            })
          end)
        end
      )
    end)
  end)
end

local function start_create_flow(window, pane, repo)
  choose(window, pane, {
    choices = {
      {
        id = 'new',
        label = 'Create new branch',
      },
      {
        id = 'existing',
        label = 'Use existing branch or remote ref',
      },
    },
    fuzzy = false,
    title = 'Create worktree for ' .. repo.name,
  }, function(inner_window, inner_pane, id)
    if id == 'existing' then
      after_overlay(function()
        choose_existing_branch(inner_window, pane, repo)
      end)
    elseif id == 'new' then
      after_overlay(function()
        choose_new_branch(inner_window, pane, repo)
      end)
    end
  end)
end

local function choose_repo_for_creation(window, pane, current_repo, repos)
  if #repos == 0 then
    prompt(
      window,
      pane,
      'Repository path to use for the new worktree',
      function(inner_window, inner_pane, line)
        if line == nil then
          return
        end

        local repo = build_repo(line)
        if not repo then
          notify(inner_window, 'That path is not inside a git repository')
          return
        end

        after_overlay(function()
          start_create_flow(inner_window, pane, repo)
        end)
      end
    )
    return
  end

  local choices = {}
  for _, repo in ipairs(repos) do
    local label = repo.name .. ' (' .. repo.worktree_parent .. ')'
    if current_repo and repo.id == current_repo.id then
      label = label .. ' [current]'
    end

    table.insert(choices, {
      id = repo.id,
      label = label,
    })
  end

  table.insert(choices, {
    id = '__manual__',
    label = 'Enter a repository path manually',
  })

  choose(window, pane, {
    choices = choices,
    title = 'Choose a repository',
  }, function(inner_window, inner_pane, id)
    if not id then
      return
    end

    if id == '__manual__' then
      after_overlay(function()
        prompt(
          inner_window,
          pane,
          'Repository path to use for the new worktree',
          function(path_window, path_pane, line)
            if line == nil then
              return
            end

            local repo = build_repo(line)
            if not repo then
              notify(path_window, 'That path is not inside a git repository')
              return
            end

            after_overlay(function()
              start_create_flow(path_window, pane, repo)
            end)
          end
        )
      end)
      return
    end

    for _, repo in ipairs(repos) do
      if repo.id == id then
        after_overlay(function()
          start_create_flow(inner_window, pane, repo)
        end)
        return
      end
    end
  end)
end

local function removable_worktrees(repos, current_workspace)
  local removable = {}
  for _, repo in ipairs(repos) do
    for _, worktree in ipairs(repo.worktrees) do
      if should_display_worktree(worktree)
        and not worktree.is_main
        and worktree.workspace_name ~= current_workspace
      then
        table.insert(removable, worktree)
      end
    end
  end

  sort_worktrees(removable)
  return removable
end

local function workspace_panes(workspace_name)
  local panes = {}
  for _, mux_window in ipairs(wezterm.mux.all_windows()) do
    if mux_window:get_workspace() == workspace_name then
      for _, tab in ipairs(mux_window:tabs()) do
        for _, pane in ipairs(tab:panes()) do
          table.insert(panes, pane)
        end
      end
    end
  end

  return panes
end

local function close_workspace_panes(workspace_name)
  local panes = workspace_panes(workspace_name)

  for _, pane in ipairs(panes) do
    run_command({
      wezterm_executable(),
      'cli',
      'kill-pane',
      '--pane-id',
      tostring(pane:pane_id()),
    })
  end

  return #panes
end

local finish_remove_worktree

local function worktree_is_registered(worktree)
  local worktrees = parse_worktrees(worktree.repo_main_path)
  if not worktrees then
    return nil
  end

  for _, candidate in ipairs(worktrees) do
    if candidate.path == normalize_path(worktree.path) then
      return true
    end
  end

  return false
end

local function wait_for_workspace_to_close(window, workspace_name, attempts_remaining, on_closed, failure_message)
  if #workspace_panes(workspace_name) == 0 then
    return on_closed()
  end

  if attempts_remaining <= 0 then
    notify(window, failure_message or ('Could not close workspace ' .. workspace_name))
    return false
  end

  close_workspace_panes(workspace_name)
  wezterm.time.call_after(0.1, function()
    wait_for_workspace_to_close(window, workspace_name, attempts_remaining - 1, on_closed, failure_message)
  end)

  return true
end

finish_remove_worktree = function(window, worktree, attempts_remaining)
  attempts_remaining = attempts_remaining or 20

  local registered = worktree_is_registered(worktree)
  if registered == nil then
    notify(window, 'Could not inspect worktree state for ' .. worktree.path)
    return false
  end

  if registered then
    local stdout, stderr = git_output(worktree.repo_main_path, {
      'worktree',
      'remove',
      '--force',
      worktree.path,
    })

    if not stdout then
      if attempts_remaining > 0 then
        wezterm.time.call_after(0.2, function()
          finish_remove_worktree(window, worktree, attempts_remaining - 1)
        end)
        return true
      end

      notify(window, stderr)
      return false
    end
  end

  git_output(worktree.repo_main_path, { 'worktree', 'prune' })

  if path_exists(worktree.path) then
    local ok, err = remove_directory_tree(worktree.path)
    if not ok then
      if attempts_remaining > 0 then
        wezterm.time.call_after(0.2, function()
          finish_remove_worktree(window, worktree, attempts_remaining - 1)
        end)
        return true
      end

      notify(window, err or ('Could not remove directory ' .. worktree.path))
      return false
    end
  end

  notify(window, 'Removed ' .. worktree.path)
  return true
end

local function remove_worktree(window, worktree)
  local workspace_name = worktree.worktree_workspace_name or worktree.workspace_name
  if workspace_name and #workspace_panes(workspace_name) > 0 then
    close_workspace_panes(workspace_name)
    wezterm.time.call_after(0.1, function()
      wait_for_workspace_to_close(
        window,
        workspace_name,
        20,
        function()
          return finish_remove_worktree(window, worktree)
        end,
        'Could not close workspace ' .. workspace_name .. ' before removal'
      )
    end)
    return true
  end

  return finish_remove_worktree(window, worktree)
end

local choose_worktree_to_remove

local function main_worktree(repo)
  for _, worktree in ipairs(repo.worktrees) do
    if worktree.is_main then
      return worktree
    end
  end

  return nil
end

local function choose_workspace_destination(current_workspace, fallback)
  local previous = wezterm.GLOBAL.previous_workspace
  if previous and previous ~= current_workspace and workspace_exists(previous) then
    return { name = previous }
  end

  for _, workspace in ipairs(wezterm.mux.get_workspace_names()) do
    if workspace ~= current_workspace then
      return { name = workspace }
    end
  end

  if fallback and fallback.name and fallback.name ~= current_workspace then
    return fallback
  end

  if current_workspace ~= 'default' then
    return {
      cwd = wezterm.home_dir,
      name = 'default',
    }
  end

  return {
    cwd = wezterm.home_dir,
    name = 'home',
  }
end

local function clear_removed_workspace_references(workspace_name)
  if wezterm.GLOBAL.previous_workspace == workspace_name then
    wezterm.GLOBAL.previous_workspace = nil
  end

  if wezterm.GLOBAL.current_workspace == workspace_name then
    wezterm.GLOBAL.current_workspace = nil
  end
end

local function finish_remove_session(window, workspace_name)
  clear_removed_workspace_references(workspace_name)
  notify(window, 'Removed session ' .. workspace_name)
  return true
end

local function confirm_and_remove_current(window, pane, worktree, repo)
  after_overlay(function()
    choose(window, pane, {
      choices = {
        {
          id = 'remove-current',
          label = 'Remove ' .. worktree.path,
        },
        {
          id = 'back',
          label = 'Back',
        },
      },
      fuzzy = false,
      title = 'Remove current worktree',
    }, function(inner_window, _inner_pane, id)
      if id == 'back' then
        after_overlay(function()
          inner_window:perform_action(M.menu(), pane)
        end)
        return
      end

      if id ~= 'remove-current' then
        return
      end

      local current_workspace = inner_window:active_workspace()
      local main = main_worktree(repo)
      local fallback = nil
      if main and main.workspace_name ~= current_workspace then
        fallback = {
          cwd = main.path,
          name = main.workspace_name,
        }
      end
      local target = choose_workspace_destination(current_workspace, fallback)
      pending_removals()[target.name] = {
        path = worktree.path,
        repo_main_path = worktree.repo_main_path,
        target_workspace_name = target.name,
        worktree_workspace_name = worktree.workspace_name,
      }

      inner_window:perform_action(
        act.SwitchToWorkspace({
          name = target.name,
          spawn = target.cwd and {
            cwd = target.cwd,
            label = target.name,
          } or nil,
        }),
        pane
      )
    end)
  end)
end

function M.remove_current_session()
  return wezterm.action_callback(function(window, pane)
    local current_workspace = window:active_workspace()
    local target = choose_workspace_destination(current_workspace)
    if not target or target.name == current_workspace then
      notify(window, 'Could not determine a session to switch to before removal')
      return
    end

    pending_session_removals()[target.name] = {
      source_workspace_name = current_workspace,
      target_workspace_name = target.name,
    }

    window:perform_action(
      act.SwitchToWorkspace({
        name = target.name,
        spawn = target.cwd and {
          cwd = target.cwd,
          label = target.name,
        } or nil,
      }),
      pane
    )
  end)
end

choose_worktree_to_remove = function(window, pane, repos, current_workspace)
  local removable = removable_worktrees(repos, current_workspace)
  if #removable == 0 then
    notify(window, 'No linked worktrees are available to remove')
    return
  end

  local choices = {}
  for _, worktree in ipairs(removable) do
    local state = workspace_exists(worktree.workspace_name) and 'open' or 'closed'
    table.insert(choices, {
      id = worktree.path,
      label = worktree.repo_name .. ' [' .. (worktree.branch_name or 'detached') .. '] - ' .. basename(worktree.path) .. ' (' .. state .. ')',
    })
  end

  choose(window, pane, {
    choices = choices,
    title = 'Remove worktree',
  }, function(inner_window, inner_pane, id)
    if not id then
      return
    end

    local worktree = find_worktree_by_path(repos, id)
    if not worktree then
      notify(inner_window, 'Could not resolve the selected worktree')
      return
    end

    after_overlay(function()
      remove_worktree(inner_window, worktree)
    end)
  end)
end

local function worktree_label(worktree, current_workspace)
  local branch = worktree.branch_name or 'detached'
  local target = worktree.is_main and 'main worktree' or basename(worktree.path)
  local prefix = worktree.workspace_name == current_workspace and '* ' or '  '
  return prefix .. worktree.repo_name .. ' [' .. branch .. '] - ' .. target
end

function M.handle_update_status(window, pane)
  local pending = pending_layouts()[window:active_workspace()]
  if not pending then
    local pending_removal = pending_removals()[window:active_workspace()]
    if pending_removal then
      pending_removals()[pending_removal.target_workspace_name] = nil
      remove_worktree(window, pending_removal)
    end

    local pending_session_removal = pending_session_removals()[window:active_workspace()]
    if pending_session_removal then
      pending_session_removals()[pending_session_removal.target_workspace_name] = nil
      wait_for_workspace_to_close(
        window,
        pending_session_removal.source_workspace_name,
        20,
        function()
          return finish_remove_session(window, pending_session_removal.source_workspace_name)
        end,
        'Could not close session ' .. pending_session_removal.source_workspace_name
      )
    end
    return
  end

  pending_layouts()[pending.workspace_name] = nil
  local layout_state = apply_layout(window, pane, pending)
  if layout_state.focus_pane or #layout_state.startup_commands > 0 or layout_state.first_run_warning then
    after_overlay(function()
      if layout_state.first_run_warning then
        notify(window, layout_state.first_run_warning)
      end

      if layout_state.focus_pane then
        layout_state.focus_pane:activate()
      end

      for _, startup in ipairs(layout_state.startup_commands) do
        run_shell_command_in_pane(startup.pane, startup.command)
      end
    end)
  end

  local pending_removal = pending_removals()[window:active_workspace()]
  if pending_removal then
    pending_removals()[pending_removal.target_workspace_name] = nil
    remove_worktree(window, pending_removal)
  end

  local pending_session_removal = pending_session_removals()[window:active_workspace()]
  if pending_session_removal then
    pending_session_removals()[pending_session_removal.target_workspace_name] = nil
    wait_for_workspace_to_close(
      window,
      pending_session_removal.source_workspace_name,
      20,
      function()
        return finish_remove_session(window, pending_session_removal.source_workspace_name)
      end,
      'Could not close session ' .. pending_session_removal.source_workspace_name
    )
  end
end

function M.menu()
  return wezterm.action_callback(function(window, pane)
    local current_path = current_pane_path(pane)
    local current_repo = repo_for_pane(pane)
    local current_worktree = current_worktree_for_path(current_repo, current_path)
    local extra_paths = {}
    if current_repo then
      table.insert(extra_paths, current_repo.main_path)
    else
      if current_path then
        table.insert(extra_paths, current_path)
      end
    end

    local repos = current_repo and { current_repo } or discover_repositories(extra_paths)
    local choices = {}

    if current_repo then
      table.insert(choices, {
        id = 'create-current',
        label = '+ New worktree from current repo (' .. current_repo.name .. ')',
      })

      if current_worktree and not current_worktree.is_main then
        table.insert(choices, {
          id = 'remove-current',
          label = '- Remove current worktree (' .. basename(current_worktree.path) .. ')',
        })
      end
    end

    if current_repo then
      table.insert(choices, {
        id = 'remove',
        label = '- Remove worktree...',
      })
    else
      table.insert(choices, {
        id = 'create-repo',
        label = '+ New worktree from repo...',
      })
      table.insert(choices, {
        id = 'remove',
        label = '- Remove worktree...',
      })
    end

    for _, worktree in ipairs(flatten_worktrees(repos)) do
      table.insert(choices, {
        id = worktree.path,
        label = worktree_label(worktree, window:active_workspace()),
      })
    end

    choose(window, pane, {
      choices = choices,
      title = 'Worktrees',
    }, function(inner_window, inner_pane, id)
      if not id then
        return
      end

      if id == 'create-current' then
        if current_repo then
          after_overlay(function()
            start_create_flow(inner_window, pane, current_repo)
          end)
        else
          notify(inner_window, 'Current pane is not inside a git repository')
        end
        return
      end

      if id == 'remove-current' then
        if current_repo and current_worktree and not current_worktree.is_main then
          after_overlay(function()
            confirm_and_remove_current(inner_window, pane, current_worktree, current_repo)
          end)
        else
          notify(inner_window, 'Current pane is not in a removable linked worktree')
        end
        return
      end

      if id == 'create-repo' then
        after_overlay(function()
          choose_repo_for_creation(inner_window, pane, current_repo, repos)
        end)
        return
      end

      if id == 'remove' then
        after_overlay(function()
          choose_worktree_to_remove(inner_window, pane, repos, window:active_workspace())
        end)
        return
      end

      local worktree = find_worktree_by_path(repos, id)
      if not worktree then
        notify(inner_window, 'Could not resolve the selected worktree')
        return
      end

      switch_to_worktree(inner_window, inner_pane, worktree)
    end)
  end)
end

return M
