local wezterm = require 'wezterm'
local act = wezterm.action
local branch_source = require 'worktree_branch'

local M = {}

local is_windows = wezterm.target_triple:find('windows') ~= nil

M.settings = {
  creation_runner_path = wezterm.config_dir .. '/../scripts/create-worktree.nu',
  debug = false,
  debug_log_path = wezterm.home_dir .. '/wezterm-worktree-debug.log',
  roots = {
    wezterm.home_dir .. '/Developer',
    wezterm.home_dir .. '/code',
    wezterm.home_dir .. '/dev',
    -- wezterm.home_dir .. '/src',
    -- wezterm.home_dir .. '/worktrees',
    -- wezterm.home_dir .. '/projects',
  },
  auto_setup_new_workspaces = true,
  branch_prefix = 'mmgeorge/',
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
        new_worktree_setup = {
          symlinks = {
            {
              path = '.screenshots',
              target = 'screenshots',
              target_base = 'worktree_parent',
            },
          },
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
                'load-env { "COPILOT_CUSTOM_INSTRUCTIONS_DIRS": "/Users/matt9222/Developer/maps/prompts", "COPILOT_SKILLS_DIR": "/Users/matt9222/Developer/maps/skills" }; co',
              },
            },
          },
        },
        branch_prefix = 'matt9222/',
      },
    },
  },
}

local function debug_log(event, fields)
  if not M.settings.debug then
    return
  end

  local payload = ''
  if fields then
    local encoded, value = pcall(wezterm.json_encode, fields)
    payload = encoded and (' ' .. value) or (' ' .. tostring(fields))
  end

  local line = os.date('!%Y-%m-%dT%H:%M:%SZ') .. ' [worktree] ' .. event .. payload
  if wezterm.log_info then
    wezterm.log_info(line)
  end

  local file, err = io.open(M.settings.debug_log_path, 'a')
  if not file then
    if wezterm.log_error then
      wezterm.log_error('[worktree] Could not open debug log: ' .. tostring(err))
    end
    return
  end

  file:write(line, '\n')
  file:close()
end

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
  if is_windows and path:match('^/%a:/') then
    path = path:sub(2)
  end

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
  debug_log('command.start', { command = command })
  local success, stdout, stderr = wezterm.run_child_process(command)
  if not success then
    debug_log('command.finish', {
      command = command,
      stderr = trim(stderr ~= '' and stderr or stdout),
      success = false,
    })
    return nil, trim(stderr ~= '' and stderr or stdout)
  end

  debug_log('command.finish', {
    command = command,
    stderr = trim(stderr or ''),
    stdout = trim(stdout or ''),
    success = true,
  })
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

local function ensure_directory(path)
  path = normalize_path(path)
  if not path then
    return false, 'Directory path is required'
  end

  if directory_exists(path) then
    return true
  end

  local stdout, stderr
  if is_windows then
    stdout, stderr = run_command({
      'powershell.exe',
      '-NoProfile',
      '-NonInteractive',
      '-Command',
      'New-Item -ItemType Directory -Force -LiteralPath $args[0] | Out-Null',
      path,
    })
  else
    stdout, stderr = run_command({ 'mkdir', '-p', path })
  end

  if not stdout then
    return false, stderr
  end

  if not directory_exists(path) then
    return false, 'Could not create directory ' .. path
  end

  return true
end

local function read_symlink_target(path)
  path = normalize_path(path)
  if not path then
    return nil, 'Symlink path is required'
  end

  local stdout, stderr
  if is_windows then
    stdout, stderr = run_command({
      'powershell.exe',
      '-NoProfile',
      '-NonInteractive',
      '-Command',
      '$item = Get-Item -LiteralPath $args[0] -Force -ErrorAction Stop; if ($item.LinkType) { [Console]::Out.Write($item.Target) }',
      path,
    })
  else
    stdout, stderr = run_command({ 'readlink', path })
  end

  if not stdout then
    return nil, stderr
  end

  local target = trim(stdout)
  if target == '' then
    return nil, nil
  end

  return normalize_path(target), nil
end

local function symlink_points_to(path, target)
  local existing_target = read_symlink_target(path)
  if not existing_target then
    return false
  end

  if not is_absolute(existing_target) then
    existing_target = join_path(dirname(path), existing_target)
  end

  return normalize_path(existing_target) == normalize_path(target)
end

local function symlink_or_path_exists(path)
  if path_exists(path) then
    return true
  end

  return read_symlink_target(path) ~= nil
end

local function create_symbolic_link(path, target)
  local stdout, stderr
  if is_windows then
    stdout, stderr = run_command({
      'powershell.exe',
      '-NoProfile',
      '-NonInteractive',
      '-Command',
      'New-Item -ItemType SymbolicLink -LiteralPath $args[0] -Target $args[1] | Out-Null',
      path,
      target,
    })
  else
    stdout, stderr = run_command({ 'ln', '-s', target, path })
  end

  if not stdout then
    return false, stderr
  end

  return true
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
    debug_log('pane.cwd.missing', {
      cwd = cwd and tostring(cwd) or nil,
    })
    return nil
  end

  local foreground = nil
  local foreground_ok, foreground_info = pcall(function()
    return pane:get_foreground_process_info()
  end)
  if foreground_ok and foreground_info then
    foreground = {
      cwd = foreground_info.cwd,
      executable = foreground_info.executable,
      name = foreground_info.name,
    }
  end

  local path = normalize_path(cwd.file_path)
  debug_log('pane.cwd', {
    file_path = cwd.file_path,
    foreground = foreground,
    normalized_path = path,
    uri = tostring(cwd),
  })
  return path
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
    if not worktree.bare and not worktree.is_main then
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

local function default_worktree_parent(common_dir, main_path)
  common_dir = normalize_path(common_dir)
  local common_name = basename(common_dir)
  if common_name == '.bare' then
    return dirname(common_dir)
  end

  return dirname(main_path)
end

local function main_worktree_path(worktrees)
  for _, worktree in ipairs(worktrees) do
    if not worktree.bare and directory_exists(join_path(worktree.path, '.git')) then
      return worktree.path
    end
  end

  for _, worktree in ipairs(worktrees) do
    if not worktree.bare and worktree.branch_name == 'main' then
      return worktree.path
    end
  end

  for _, worktree in ipairs(worktrees) do
    if not worktree.bare and worktree.branch_name == 'master' then
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
    debug_log('repo.build.rejected', { reason = 'missing path' })
    return nil
  end

  debug_log('repo.build.start', {
    common_dir = common_dir,
    path = path,
  })
  common_dir = common_dir or git_common_dir(path)
  if not common_dir then
    debug_log('repo.build.rejected', {
      path = path,
      reason = 'git common directory unavailable',
    })
    return nil
  end

  local worktrees = parse_worktrees(path)
  if not worktrees or #worktrees == 0 then
    debug_log('repo.build.rejected', {
      common_dir = common_dir,
      path = path,
      reason = 'worktree inventory unavailable',
    })
    return nil
  end

  local is_bare_layout = basename(common_dir) == '.bare'
  local repo = {
    id = common_dir,
    name = basename(dirname(common_dir)),
    main_path = main_worktree_path(worktrees),
    root_path = is_bare_layout and dirname(common_dir) or nil,
    worktrees = worktrees,
  }

  for _, worktree in ipairs(repo.worktrees) do
    worktree.is_main = should_display_worktree(worktree) and worktree.path == repo.main_path
    worktree.repo_id = repo.id
    worktree.repo_name = repo.name
    worktree.repo_main_path = repo.main_path
    worktree.workspace_name = worktree_workspace_name(repo.name, worktree)
  end

  local default_parent = default_worktree_parent(common_dir, repo.main_path)
  if is_bare_layout then
    repo.worktree_parent = default_parent
  else
    repo.worktree_parent = preferred_worktree_parent(repo.worktrees, default_parent)
  end

  local worktree_records = {}
  for _, worktree in ipairs(repo.worktrees) do
    table.insert(worktree_records, {
      bare = worktree.bare == true,
      branch = worktree.branch_name,
      is_main = worktree.is_main == true,
      path = worktree.path,
      workspace = worktree.workspace_name,
    })
  end
  debug_log('repo.build.resolved', {
    common_dir = repo.id,
    main_path = repo.main_path,
    name = repo.name,
    root_path = repo.root_path,
    source_path = path,
    worktree_parent = repo.worktree_parent,
    worktrees = worktree_records,
  })
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
    debug_log('pane.repo.missing', { reason = 'pane path unavailable' })
    return nil
  end

  local repo = build_repo(path)
  debug_log('pane.repo', {
    path = path,
    repo = repo and {
      id = repo.id,
      main_path = repo.main_path,
      name = repo.name,
      root_path = repo.root_path,
    } or nil,
  })
  return repo
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

local function find_worktree_by_workspace(repos, workspace_name)
  if not workspace_name then
    return nil, nil
  end

  for _, repo in ipairs(repos) do
    for _, worktree in ipairs(repo.worktrees) do
      if worktree.workspace_name == workspace_name then
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

local function pending_creations()
  wezterm.GLOBAL.worktree_pending_creations = wezterm.GLOBAL.worktree_pending_creations or {}
  return wezterm.GLOBAL.worktree_pending_creations
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
  debug_log('worktree.switch', {
    exists = exists,
    path = worktree.path,
    workspace = worktree.workspace_name,
  })
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

  path = normalize_path(path)
  if repo.root_path and path == repo.root_path then
    for _, worktree in ipairs(repo.worktrees) do
      if worktree.is_main then
        return worktree
      end
    end
  end

  local best_match = nil
  local best_length = -1

  for _, worktree in ipairs(repo.worktrees) do
    if should_display_worktree(worktree)
      and path_is_within(path, worktree.path)
      and #worktree.path > best_length
    then
      best_match = worktree
      best_length = #worktree.path
    end
  end

  return best_match
end

local function append_setup_specs(target_specs, source_specs)
  if not source_specs then
    return
  end

  if type(source_specs) ~= 'table' then
    return
  end

  if source_specs.path or source_specs.target then
    table.insert(target_specs, source_specs)
    return
  end

  for _, spec in ipairs(source_specs) do
    table.insert(target_specs, spec)
  end
end

local function resolve_new_worktree_setup(repo)
  local setup = {
    symlinks = {},
  }

  local default_setup = M.settings.new_worktree_setup or {}
  append_setup_specs(setup.symlinks, default_setup.symlinks)

  local settings = repo_settings(repo.name)
  local repo_setup = settings.new_worktree_setup or {}
  append_setup_specs(setup.symlinks, repo_setup.symlinks)

  return setup
end

local function resolve_setup_base(repo, worktree, base)
  base = base or 'worktree'

  if base == 'worktree' then
    return worktree.path, worktree.path and nil or 'Worktree path is unavailable'
  end

  if base == 'main_worktree' or base == 'repo_main' then
    return repo.main_path, repo.main_path and nil or 'Main worktree path is unavailable'
  end

  if base == 'worktree_parent' then
    return repo.worktree_parent, repo.worktree_parent and nil or 'Worktree parent is unavailable'
  end

  if is_absolute(base) then
    return normalize_path(base)
  end

  return nil, 'Unknown setup path base: ' .. tostring(base)
end

local function resolve_setup_path(repo, worktree, path, base)
  path = normalize_path(path)
  if not path then
    return nil, 'Setup path is required'
  end

  if is_absolute(path) then
    return path
  end

  local base_path, base_err = resolve_setup_base(repo, worktree, base)
  if not base_path then
    return nil, base_err
  end

  return join_path(base_path, path)
end

local function create_worktree_symlink(repo, worktree, spec)
  if type(spec) ~= 'table' then
    return false, 'Symlink setup must be a table'
  end

  local link_path, link_err = resolve_setup_path(
    repo,
    worktree,
    spec.path or spec.link_path or spec.link,
    spec.path_base or 'worktree'
  )
  if not link_path then
    return false, link_err
  end

  local target_path, target_err = resolve_setup_path(
    repo,
    worktree,
    spec.target or spec.to,
    spec.target_base or 'main_worktree'
  )
  if not target_path then
    return false, target_err
  end

  if not path_exists(target_path) then
    if not spec.create_target then
      return false, 'Symlink target does not exist: ' .. target_path
    end

    local target_ok, target_mkdir_err = ensure_directory(target_path)
    if not target_ok then
      return false, target_mkdir_err
    end
  end

  if symlink_or_path_exists(link_path) then
    if symlink_points_to(link_path, target_path) then
      return true
    end

    return false, 'Cannot create symlink at existing path: ' .. link_path
  end

  local parent_ok, parent_err = ensure_directory(dirname(link_path))
  if not parent_ok then
    return false, parent_err
  end

  local link_ok, link_err = create_symbolic_link(link_path, target_path)
  if not link_ok then
    return false, link_err
  end

  if not symlink_points_to(link_path, target_path) then
    return false, 'Could not verify symlink ' .. link_path .. ' -> ' .. target_path
  end

  return true
end

local function run_new_worktree_setup(window, repo, worktree)
  local setup = resolve_new_worktree_setup(repo)
  local failures = {}

  for _, spec in ipairs(setup.symlinks) do
    local setup_ok, setup_err = create_worktree_symlink(repo, worktree, spec)
    if not setup_ok then
      table.insert(failures, setup_err or 'unknown setup failure')
    end
  end

  if #failures == 0 then
    return true
  end

  local message = failures[1]
  if #failures > 1 then
    message = tostring(#failures) .. ' setup steps failed; first: ' .. message
  end

  notify(window, 'Created the worktree, but setup failed: ' .. message)
  return false
end

local function list_remotes(repo)
  local output, err = git_capture(repo.main_path, { 'remote' })
  if output == nil then
    return nil, err
  end

  return split_lines(output)
end

local function refresh_remote(window, repo, remote)
  local stdout, stderr = git_output(repo.main_path, branch_source.remote_fetch_args(remote))
  if not stdout then
    notify(window, 'Could not refresh ' .. remote .. ' for ' .. repo.name .. ': ' .. stderr)
    return false
  end

  return true
end

local function branch_upstream(repo, branch)
  local upstream = git_capture(repo.main_path, {
    'rev-parse',
    '--abbrev-ref',
    branch .. '@{upstream}',
  })

  if upstream and upstream ~= '' then
    return upstream
  end

  return nil
end

local function ensure_branch_upstream(repo, branch, upstream)
  if not upstream or upstream == '' then
    return true
  end

  if branch_upstream(repo, branch) == upstream then
    return true
  end

  local stdout, stderr = git_output(repo.main_path, {
    'branch',
    '--set-upstream-to=' .. upstream,
    branch,
  })
  if not stdout then
    return false, stderr
  end

  return true
end

local function checked_out_worktree(repo, branch)
  for _, worktree in ipairs(repo.worktrees) do
    if worktree.branch_name == branch then
      return worktree
    end
  end

  return nil
end

local function local_branch_exists(repo, branch)
  local stdout = git_capture(repo.main_path, {
    'show-ref',
    '--verify',
    '--quiet',
    'refs/heads/' .. branch,
  })

  return stdout ~= nil
end

local function ref_oid(repo, ref)
  return git_capture(repo.main_path, { 'rev-parse', '--verify', ref })
end

local function local_branch_is_ancestor(repo, branch, remote_ref)
  local stdout, stderr = git_output(repo.main_path, {
    'merge-base',
    '--is-ancestor',
    branch,
    remote_ref,
  })

  if stdout ~= nil then
    return true, nil
  end

  if stderr and stderr ~= '' then
    return nil, stderr
  end

  return false, nil
end

local function created_branch(opts)
  return opts.branch or opts.new_branch or opts.start_point
end

local function finish_created_worktree(window, repo, opts)
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

  if opts.upstream then
    local branch = created_branch(opts)
    local upstream_ok, upstream_err = ensure_branch_upstream(updated_repo, branch, opts.upstream)
    if not upstream_ok then
      local message = upstream_err and upstream_err ~= '' and upstream_err or ('could not set upstream to ' .. opts.upstream)
      notify(window, 'Created the worktree, but could not set upstream for ' .. branch .. ': ' .. message)
    end
  end

  run_new_worktree_setup(window, updated_repo, worktree)

  local layout_options = {
    first_run = resolve_layout(worktree).first_run,
  }
  queue_layout(worktree, layout_options)
end

local function worktree_creation_command(repo, opts, workspace_name)
  local command = {
    'nu',
    '--no-config-file',
    M.settings.creation_runner_path,
    '--repository-path',
    repo.main_path,
    '--worktree-path',
    opts.path,
    '--start-point',
    opts.start_point,
  }
  if opts.new_branch then
    table.insert(command, '--new-branch')
    table.insert(command, opts.new_branch)
  end
  if opts.use_cached_remote_base then
    table.insert(command, '--use-cached-remote-base')
  end
  table.insert(command, '--workspace-name')
  table.insert(command, workspace_name)
  return command
end

local function start_visible_worktree_creation(window, pane, repo, opts)
  local branch = created_branch(opts)
  local worktree = {
    branch_name = branch,
    path = opts.path,
    repo_main_path = repo.main_path,
    repo_name = repo.name,
  }
  worktree.workspace_name = worktree_workspace_name(repo.name, worktree)

  if workspace_exists(worktree.workspace_name) then
    notify(window, 'Workspace ' .. worktree.workspace_name .. ' already exists')
    return
  end

  pending_creations()[worktree.workspace_name] = {
    branch = opts.branch,
    new_branch = opts.new_branch,
    path = opts.path,
    repo_main_path = repo.main_path,
    start_point = opts.start_point,
    upstream = opts.upstream,
    workspace_name = worktree.workspace_name,
  }

  window:perform_action(
    act.SwitchToWorkspace({
      name = worktree.workspace_name,
      spawn = {
        args = worktree_creation_command(repo, opts, worktree.workspace_name),
        cwd = repo.main_path,
        label = worktree.workspace_name,
      },
    }),
    pane
  )
end

local function create_worktree(window, pane, repo, opts)
  if find_worktree(repo, opts.path) then
    notify(window, 'A worktree already exists at ' .. opts.path)
    return
  end

  if opts.new_branch then
    if local_branch_exists(repo, opts.new_branch) then
      notify(
        window,
        'Branch ' .. opts.new_branch .. ' already exists. Use Check out existing branch, then choose Local branch'
      )
      return
    end
  end

  start_visible_worktree_creation(window, pane, repo, opts)
end

local function branch_records(repo, source, remote, exclude_checked_out)
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

  local ref_records = {}
  local branch_names = {}
  local output = git_capture(repo.main_path, {
    'for-each-ref',
    '--format=%(refname:short)%09%(committerdate:unix)%09%(symref)%09END',
    branch_source.ref_namespace(source, remote),
  }) or ''

  for _, line in ipairs(split_lines(output)) do
    local ref, timestamp, symref = line:match('^(.-)\t([0-9]+)\t(.-)\tEND$')
    if ref and ref ~= '' and symref == '' then
      local logical_name = branch_source.logical_name(ref, source)
      if not (exclude_checked_out and checked_out_worktree(repo, logical_name)) then
        branch_names[logical_name] = true
        table.insert(ref_records, {
          committed_at = tonumber(timestamp) or 0,
          id = ref,
          logical_name = logical_name,
          release_version = parse_release_version(logical_name),
          source = source,
        })
      end
    end
  end

  local function branch_group(record)
    if record.logical_name == 'main' then
      return 0
    end

    if not branch_names.main and record.logical_name == 'master' then
      return 0
    end

    if record.release_version then
      return 1
    end

    if record.logical_name:match('^matt9222/') then
      return 2
    end

    return 3
  end

  table.sort(ref_records, function(left, right)
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

  return ref_records
end

local function choose_remote(window, pane, repo, callback)
  local remotes, err = list_remotes(repo)
  if not remotes then
    notify(window, 'Could not list remotes for ' .. repo.name .. ': ' .. err)
    return
  end

  if #remotes == 0 then
    notify(window, 'No remotes are configured for ' .. repo.name)
    return
  end

  if #remotes == 1 then
    callback(window, pane, remotes[1])
    return
  end

  local choices = {}
  for _, remote in ipairs(remotes) do
    table.insert(choices, {
      id = remote,
      label = remote,
    })
  end

  choose(window, pane, {
    choices = choices,
    fuzzy = false,
    title = 'Remote for ' .. repo.name,
  }, function(inner_window, inner_pane, remote)
    if not remote then
      return
    end

    after_overlay(function()
      callback(inner_window, pane, remote)
    end)
  end)
end

local function choose_branch_source(window, pane, repo, title, callback)
  choose(window, pane, {
    choices = branch_source.source_choices(),
    fuzzy = false,
    title = title .. ' for ' .. repo.name,
  }, function(inner_window, inner_pane, source)
    if not source then
      return
    end

    after_overlay(function()
      if source == 'remote' then
        choose_remote(inner_window, pane, repo, function(remote_window, remote_pane, remote)
          callback(remote_window, remote_pane, source, remote)
        end)
      else
        callback(inner_window, pane, source, nil)
      end
    end)
  end)
end

local function choose_branch(window, pane, repo, source, remote, title, exclude_checked_out, callback)
  local records = branch_records(repo, source, remote, exclude_checked_out)
  if #records == 0 then
    notify(window, 'No ' .. source .. ' branches are available for ' .. repo.name)
    return
  end

  local records_by_id = {}
  local choices = {}
  for _, record in ipairs(records) do
    records_by_id[record.id] = record
    table.insert(choices, {
      id = record.id,
      label = record.logical_name,
    })
  end

  choose(window, pane, {
    choices = choices,
    title = title .. ' from ' .. (remote or source) .. ' for ' .. repo.name,
  }, function(inner_window, inner_pane, id)
    local record = records_by_id[id]
    if not record then
      return
    end

    after_overlay(function()
      callback(inner_window, pane, record)
    end)
  end)
end

local function prepare_remote_existing_branch(window, repo, record)
  local branch = record.logical_name
  local local_exists = local_branch_exists(repo, branch)
  local checked_out = local_exists and checked_out_worktree(repo, branch) or nil
  local local_oid, local_oid_err
  if local_exists then
    local_oid, local_oid_err = ref_oid(repo, branch)
    if not local_oid then
      notify(window, 'Could not resolve local branch ' .. branch .. ': ' .. local_oid_err)
      return nil
    end
  end

  local remote_oid, remote_oid_err = ref_oid(repo, record.id)
  if not remote_oid then
    notify(window, 'Could not resolve remote branch ' .. record.id .. ': ' .. remote_oid_err)
    return nil
  end

  local same_tip = local_oid ~= nil and remote_oid ~= nil and local_oid == remote_oid
  local local_is_ancestor = false
  if local_exists and not checked_out and not same_tip then
    local ancestor, ancestor_err = local_branch_is_ancestor(repo, branch, record.id)
    if ancestor == nil then
      notify(window, 'Could not compare ' .. branch .. ' with ' .. record.id .. ': ' .. ancestor_err)
      return nil
    end
    local_is_ancestor = ancestor
  end
  local action = branch_source.existing_remote_action({
    checked_out = checked_out ~= nil,
    local_exists = local_exists,
    local_is_ancestor = local_is_ancestor,
    same_tip = same_tip,
  })

  if action == 'blocked_checked_out' then
    notify(window, 'Local branch ' .. branch .. ' is already checked out at ' .. checked_out.path)
    return nil
  end

  if action == 'blocked_diverged' then
    notify(
      window,
      'Local branch ' .. branch .. ' is ahead of or diverged from ' .. record.id .. '. Choose Local branch or resolve it first'
    )
    return nil
  end

  if action == 'fast_forward' then
    local stdout, stderr = git_output(repo.main_path, {
      'branch',
      '--force',
      branch,
      record.id,
    })
    if not stdout then
      notify(window, 'Could not fast-forward ' .. branch .. ' to ' .. record.id .. ': ' .. stderr)
      return nil
    end
  end

  return {
    branch = branch,
    new_branch = action == 'create_tracking' and branch or nil,
    start_point = action == 'create_tracking' and record.id or branch,
    upstream = record.id,
  }
end

local function choose_existing_branch(window, pane, repo)
  choose_branch_source(window, pane, repo, 'Existing branch source', function(source_window, source_pane, source, remote)
    if source == 'remote' and not refresh_remote(source_window, repo, remote) then
      return
    end

    choose_branch(
      source_window,
      source_pane,
      repo,
      source,
      remote,
      'Existing branch',
      source == 'local',
      function(branch_window, branch_pane, record)
        local create_opts
        if source == 'remote' then
          create_opts = prepare_remote_existing_branch(branch_window, repo, record)
        else
          create_opts = {
            branch = record.logical_name,
            start_point = record.id,
          }
        end

        if not create_opts then
          return
        end

        create_opts.path = default_worktree_path(repo, sanitize_name(record.logical_name))
        create_worktree(branch_window, branch_pane, repo, create_opts)
      end
    )
  end)
end

local function prompt_new_branch(window, pane, repo, record, use_cached_remote_base)
  prompt(
    window,
    pane,
    'New branch name from ' .. record.id,
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
          start_point = record.id,
          use_cached_remote_base = use_cached_remote_base,
        })
      end)
    end
  )
end

local function choose_new_branch(window, pane, repo)
  choose_branch_source(window, pane, repo, 'New branch source', function(source_window, source_pane, source, remote)
    if source == 'remote' and not refresh_remote(source_window, repo, remote) then
      return
    end

    choose_branch(
      source_window,
      source_pane,
      repo,
      source,
      remote,
      'Start point',
      false,
      function(branch_window, branch_pane, record)
        prompt_new_branch(branch_window, branch_pane, repo, record, source == 'local')
      end
    )
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
        label = 'Check out existing branch',
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

local function worktree_label(worktree, current_workspace, current_worktree)
  local branch = worktree.branch_name or 'detached'
  local target = worktree.is_main and 'main worktree' or basename(worktree.path)
  local is_current = worktree.workspace_name == current_workspace
    or (current_worktree and worktree.path == current_worktree.path)
  local prefix = is_current and '* ' or '  '
  return prefix .. worktree.repo_name .. ' [' .. branch .. '] - ' .. target
end

function M.handle_user_var_changed(window, pane, name, value)
  if name ~= 'WORKTREE_CREATE_READY' and name ~= 'WORKTREE_CREATE_FAILED' then
    return
  end

  local pending = pending_creations()[value]
  if not pending then
    return
  end

  pending_creations()[value] = nil
  if name == 'WORKTREE_CREATE_FAILED' then
    notify(window, 'Worktree creation failed. The Git error remains visible in the workspace')
    return
  end

  finish_created_worktree(window, {
    main_path = pending.repo_main_path,
  }, pending)
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
    local current_workspace = window:active_workspace()
    local current_repo = repo_for_pane(pane)
    local extra_paths = {}
    if current_repo then
      table.insert(extra_paths, current_repo.main_path)
    else
      if current_path then
        table.insert(extra_paths, current_path)
      end
    end

    local repos = current_repo and { current_repo } or discover_repositories(extra_paths)
    local workspace_worktree, workspace_repo = find_worktree_by_workspace(repos, current_workspace)
    local current_worktree = workspace_worktree or current_worktree_for_path(current_repo, current_path)
    if workspace_worktree then
      current_repo = workspace_repo
    end

    debug_log('menu.context', {
      current_path = current_path,
      current_repo = current_repo and {
        id = current_repo.id,
        main_path = current_repo.main_path,
        name = current_repo.name,
        root_path = current_repo.root_path,
      } or nil,
      current_worktree = current_worktree and {
        branch = current_worktree.branch_name,
        is_main = current_worktree.is_main == true,
        path = current_worktree.path,
        workspace = current_worktree.workspace_name,
      } or nil,
      current_workspace = current_workspace,
      repository_count = #repos,
    })

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
        label = worktree_label(worktree, current_workspace, current_worktree),
      })
    end

    debug_log('menu.choices', { choices = choices })

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
