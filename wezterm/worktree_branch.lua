local M = {}

---@alias WorktreeBranchSource 'remote'|'local'

---@class WorktreeBranchChoice
---@field id WorktreeBranchSource
---@field label string

---@class WorktreeRemoteState
---@field local_exists boolean
---@field checked_out? boolean
---@field same_tip? boolean
---@field local_is_ancestor? boolean

---@return WorktreeBranchChoice[]
function M.source_choices()
  return {
    {
      id = 'remote',
      label = 'Remote branch (refresh first)',
    },
    {
      id = 'local',
      label = 'Local branch (use current tip)',
    },
  }
end

---@param remote string
---@return string[]
function M.remote_fetch_args(remote)
  return {
    'fetch',
    '--prune',
    '--no-tags',
    '--no-recurse-submodules',
    remote,
  }
end

---@param source WorktreeBranchSource
---@param remote? string
---@return string
function M.ref_namespace(source, remote)
  if source == 'local' then
    return 'refs/heads'
  end

  return 'refs/remotes/' .. assert(remote, 'Remote branch source requires a remote')
end

---@param ref string
---@param source WorktreeBranchSource
---@return string
function M.logical_name(ref, source)
  if source == 'remote' then
    return ref:match('^[^/]+/(.+)$') or ref
  end

  return ref
end

---@param state WorktreeRemoteState
---@return 'create_tracking'|'blocked_checked_out'|'use_local'|'fast_forward'|'blocked_diverged'
function M.existing_remote_action(state)
  if not state.local_exists then
    return 'create_tracking'
  end

  if state.checked_out then
    return 'blocked_checked_out'
  end

  if state.same_tip then
    return 'use_local'
  end

  if state.local_is_ancestor then
    return 'fast_forward'
  end

  return 'blocked_diverged'
end

return M
