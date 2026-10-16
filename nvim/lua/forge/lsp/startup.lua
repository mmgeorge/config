local source = require("forge.lsp.source")
local M = {}

---@class ForgeLspStartupJob
---@field path string
---@field filetype string
---@field name string
---@field started integer
---@field client? integer
---@field buffer? integer
---@field done? boolean
---@field failure? string
---@field root? string
---@field rejected? boolean
---@field on_settled? fun(job: ForgeLspStartupJob)

---@class ForgeLspStartup
---@field workspace string
---@field discovering boolean
---@field jobs table<string, ForgeLspStartupJob>
---@field configs vim.lsp.Config[]
---@field timer uv.uv_timer_t
---@field closed? boolean
---@field failure? string

---@type table<string, ForgeLspStartup>
local workspaces = {}
local excluded = { [".git"] = true, node_modules = true, target = true, build = true, dist = true, [".cache"] = true }

---@param job ForgeLspStartupJob
---@param failure? string
local function settle(job, failure)
  if job.done then return end
  job.done, job.failure = true, failure
  if job.buffer then source.release(job.buffer) job.buffer = nil end
  if failure then require("forge.infra.notifications").error(failure, "Harness LSP startup") end
  if job.on_settled then job.on_settled(job) end
end

---@param state ForgeLspStartup
---@param config vim.lsp.Config
---@param path string
---@param filetype string
---@param accept_root? fun(root: string?): boolean
---@param on_settled? fun(job: ForgeLspStartupJob)
local function launch(state, config, path, filetype, accept_root, on_settled)
  local key = config.name .. ":" .. path
  local previous = state.jobs[key]
  if previous then
    local client = previous.client and vim.lsp.get_client_by_id(previous.client)
    if not previous.done or (client and not client:is_stopped()) then
      if on_settled then
        if previous.done then vim.schedule(function() on_settled(previous) end)
        else
          local subscribed = previous.on_settled
          previous.on_settled = function(job)
            if subscribed then subscribed(job) end
            on_settled(job)
          end
        end
      end
      return
    end
  end
  local job = { path = path, filetype = filetype, name = config.name, started = vim.uv.now(), on_settled = on_settled }
  state.jobs[key] = job
  state.timer:again()
  source.read(path, nil, function(contents, failure)
    if state.closed then return end
    if failure then settle(job, "startup_failed: " .. config.name .. " — " .. failure) return end
    local buffer, owned, open_error = source.open(path, contents, filetype)
    if not buffer then settle(job, open_error) return end
    job.buffer = buffer
    source.retain(buffer, owned)
    local resolved = vim.deepcopy(config)
    local start = vim.schedule_wrap(function(root)
      if state.closed or job.done then return end
      job.root = root
      if accept_root and not accept_root(root) then
        job.rejected = true
        settle(job)
        return
      end
      resolved.root_dir = root
      if not root and resolved.workspace_required then settle(job) return end
      local ok, identity = pcall(vim.lsp.start, resolved, { bufnr = buffer, reuse_client = resolved.reuse_client })
      if not ok or not identity then
        settle(job, "startup_failed: " .. config.name .. " — " .. tostring(identity or "Server launch failed."))
      else job.client = identity end
    end)
    local ok, root_error = pcall(function()
      if type(resolved.root_dir) == "function" then resolved.root_dir(buffer, start)
      else start(resolved.root_dir or (resolved.root_markers and vim.fs.root(buffer, resolved.root_markers))) end
    end)
    if not ok then settle(job, "startup_failed: " .. config.name .. " — " .. tostring(root_error)) end
  end)
end

---@param state ForgeLspStartup
local function poll(state)
  local pending = state.discovering
  for _, job in pairs(state.jobs) do
    if not job.done then
      local client = job.client and vim.lsp.get_client_by_id(job.client)
      if client and client.initialized and not client:is_stopped() then settle(job)
      elseif job.client and (not client or client:is_stopped()) then settle(job, "startup_failed: " .. job.name .. " stopped during initialization.")
      elseif vim.uv.now() - job.started >= 60000 then settle(job, "timeout: " .. job.name .. " initialization exceeded 60 seconds.") end
      pending = pending or not job.done
    end
  end
  if not pending then state.timer:stop() end
end

---@param state ForgeLspStartup
---@param path string
local function prepare_path(state, path)
  local filetype = vim.filetype.match({ filename = path })
  if not filetype then return end
  for _, config in ipairs(state.configs) do
    if vim.tbl_contains(config.filetypes or {}, filetype) then launch(state, config, path, filetype) end
  end
end

---@param state ForgeLspStartup
---@param candidates table<string, {path: string, filetype: string}[]>
local function select_sources(state, candidates)
  local remaining = vim.tbl_count(candidates)
  if remaining == 0 then state.discovering = false return end
  local function selected()
    remaining = remaining - 1
    if remaining == 0 then state.discovering = false end
  end
  for _, config in ipairs(state.configs) do
    local sources = candidates[config.name]
    if sources then
      local index, nested_roots, fallback, fallback_depth = 1, {}, nil, math.huge
      local next_source
      next_source = function()
        if state.closed then return end
        local candidate = sources[index]
        while candidate and vim.iter(nested_roots):any(function(root) return source.inside(candidate.path, root) end) do
          index = index + 1
          candidate = sources[index]
        end
        if not candidate then
          if fallback then launch(state, config, fallback.path, fallback.filetype, nil, selected)
          else selected() end
          return
        end
        index = index + 1
        launch(state, config, candidate.path, candidate.filetype, function(root)
          return root ~= nil and source.inside(state.workspace, root)
        end, function(job)
          if job.failure or (job.root and source.inside(state.workspace, job.root)) then selected() return end
          local depth = job.root and #source.canonical(job.root) or math.huge
          if not fallback or depth < fallback_depth then fallback, fallback_depth = candidate, depth end
          if job.root and source.inside(job.root, state.workspace) then nested_roots[#nested_roots + 1] = job.root end
          vim.schedule(next_source)
        end)
      end
      next_source()
    end
  end
end

---@param state ForgeLspStartup
local function discover(state)
  local queue, cursor, entries, visited = { { path = state.workspace, depth = 0 } }, 1, 0, 0
  local candidates, candidate_directory = {}, {}
  local function next_directory()
    if state.closed then return end
    if #state.configs == 0 or cursor > #queue or entries >= 4096 or visited >= 128 then
      select_sources(state, candidates)
      return
    end
    local directory = queue[cursor]
    cursor, visited = cursor + 1, visited + 1
    vim.uv.fs_scandir(directory.path, function(failure, scan)
      vim.schedule(function()
        if state.closed then return end
        if failure then
          state.failure = "Could not discover project language sources: " .. failure
          state.discovering = false
          require("forge.infra.notifications").error(state.failure, "Harness LSP startup")
          return
        end
        while entries < 4096 do
          local name, kind = vim.uv.fs_scandir_next(scan)
          if not name then break end
          entries = entries + 1
          local path = directory.path .. "/" .. name
          if kind == "directory" and directory.depth < 8 and not excluded[name] then
            queue[#queue + 1] = { path = path, depth = directory.depth + 1 }
          elseif kind == "file" then
            local filetype = vim.filetype.match({ filename = path })
            for _, config in ipairs(state.configs) do
              local key = config.name .. ":" .. directory.path
              if filetype and not candidate_directory[key] and vim.tbl_contains(config.filetypes or {}, filetype) then
                candidate_directory[key] = true
                candidates[config.name] = candidates[config.name] or {}
                table.insert(candidates[config.name], { path = path, filetype = filetype })
              end
            end
          end
        end
        next_directory()
      end)
    end)
  end
  next_directory()
end

---Warm the editor's enabled configurations using existing project sources.
---@param workspace string
---@return ForgeLspStartup
function M.start(workspace)
  workspace = source.canonical(workspace)
  if workspaces[workspace] then return workspaces[workspace] end
  local state = { workspace = workspace, discovering = true, jobs = {}, configs = {}, timer = assert(vim.uv.new_timer()) }
  workspaces[workspace] = state
  local ok, configs = pcall(vim.lsp.get_configs, { enabled = true })
  if not ok then
    state.failure = "Could not read enabled LSP configurations: " .. tostring(configs)
    state.discovering = false
    require("forge.infra.notifications").error(state.failure, "Harness LSP startup")
    return state
  end
  state.configs = configs
  state.timer:start(50, 50, vim.schedule_wrap(function() poll(state) end))
  discover(state)
  return state
end

---Wait for project discovery and matching server initialization without blocking the editor.
---@param workspace string
---@param path? string
---@param receive fun(failure?: string)
---@return fun() cancel
function M.wait(workspace, path, receive)
  if path and not source.loaded(path) then
    local stat, failure = vim.uv.fs_stat(path)
    if not stat then
      local cancelled = false
      vim.schedule(function() if not cancelled then receive(source.read_failure(path, failure or "Source is unavailable.")) end end)
      return function() cancelled = true end
    end
  end
  local state = M.start(workspace)
  if path and source.inside(path, workspace) then prepare_path(state, source.canonical(path)) end
  local timer, started, finished = assert(vim.uv.new_timer()), vim.uv.now(), false
  local function finish(failure, cancelled)
    if finished then return end
    finished = true
    timer:stop() timer:close()
    if not cancelled then receive(failure) end
  end
  timer:start(0, 50, vim.schedule_wrap(function()
    if state.closed then finish("cancelled: editor LSP startup closed") return end
    if vim.uv.now() - started >= 60000 then finish("timeout: project LSP startup exceeded 60 seconds") return end
    if state.discovering then return end
    if state.failure then finish(state.failure) return end
    poll(state)
    local filetype = path and vim.filetype.match({ filename = path })
    local failure, ready = nil, false
    for _, client in ipairs(vim.lsp.get_clients({ _uninitialized = true })) do
      local config = client.config --[[@as vim.lsp.Config]]
      if source.belongs(client, workspace) and not client:is_stopped()
        and (not filetype or vim.tbl_contains(config.filetypes or {}, filetype)) then
        if not client.initialized or client.progress and next(client.progress.pending or {}) ~= nil then return end
        ready = true
      end
    end
    for _, job in pairs(state.jobs) do
      if not filetype or job.filetype == filetype then
        if not job.done then return end
        if not ready or path and source.canonical(job.path) == source.canonical(path) then failure = failure or job.failure end
      end
    end
    finish(failure)
  end))
  return function() finish(nil, true) end
end

---Release startup bookkeeping and hidden buffers, retaining editor-owned servers.
function M.close()
  for _, state in pairs(workspaces) do
    state.closed = true
    state.timer:stop() state.timer:close()
    for _, job in pairs(state.jobs) do
      if job.buffer then source.release(job.buffer) job.buffer = nil end
    end
  end
  workspaces = {}
end

return M
