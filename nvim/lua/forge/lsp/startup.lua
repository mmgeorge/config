local source = require("forge.lsp.source")
local M = {}

---@class ForgeLspStartupJob
---@field path string
---@field filetype string
---@field filetypes string[]
---@field name string
---@field started integer
---@field client? integer
---@field buffer? integer
---@field done? boolean
---@field failure? string
---@field root? string
---@field rejected? boolean
---@field on_resolved? fun(job: ForgeLspStartupJob)

---@class ForgeLspStartup
---@field workspace string
---@field discovering boolean
---@field discovered boolean
---@field jobs table<string, ForgeLspStartupJob>
---@field configs vim.lsp.Config[]
---@field timer uv.uv_timer_t
---@field closed? boolean
---@field failure? string

---@type table<string, ForgeLspStartup>
local workspaces = {}
local excluded = { [".git"] = true, node_modules = true, target = true, build = true, dist = true, [".cache"] = true }
local package_filetypes = {
  javascript = true, javascriptreact = true, typescript = true, typescriptreact = true,
  vue = true, svelte = true, astro = true, htmlangular = true,
}

---@param config vim.lsp.Config
---@return string?
local function project_manifest(config)
  for _, filetype in ipairs(config.filetypes or {}) do
    if filetype == "rust" then return "Cargo.toml" end
    if package_filetypes[filetype] then return "package.json" end
  end
end

---@param job ForgeLspStartupJob
local function complete_resolution(job)
  local receive = job.on_resolved
  job.on_resolved = nil
  if receive then receive(job) end
end

---@param job ForgeLspStartupJob
---@param failure? string
local function settle(job, failure)
  if job.done then return end
  job.done, job.failure = true, failure
  if job.buffer then source.release(job.buffer) job.buffer = nil end
  if failure then require("forge.infra.notifications").error(failure, "Harness LSP startup") end
  complete_resolution(job)
end

---@param state ForgeLspStartup
---@param config vim.lsp.Config
---@param path string
---@param filetype string
---@param accept_root? fun(root: string?): boolean
---@param on_resolved? fun(job: ForgeLspStartupJob)
local function launch(state, config, path, filetype, accept_root, on_resolved)
  local key = config.name .. ":" .. path
  local previous = state.jobs[key]
  if previous then
    local client = previous.client and vim.lsp.get_client_by_id(previous.client)
    if not previous.done or (client and not client:is_stopped()) then
      if client and client.initialized then previous.failure = nil end
      if on_resolved then
        if previous.done or previous.client then vim.schedule(function() on_resolved(previous) end)
        else
          local subscribed = previous.on_resolved
          previous.on_resolved = function(job)
            if subscribed then subscribed(job) end
            on_resolved(job)
          end
        end
      end
      return
    end
  end
  local job = { path = path, filetype = filetype, filetypes = config.filetypes or {}, name = config.name,
    started = vim.uv.now(), on_resolved = on_resolved }
  state.jobs[key] = job
  state.timer:again()
  source.read(path, function(contents, failure)
    if state.closed or job.done then return end
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
      local options = { bufnr = buffer, reuse_client = resolved.reuse_client }
      if project_manifest(config) == vim.fs.basename(path) then options.attach = false end
      for _, client in ipairs(vim.lsp.get_clients({ _uninitialized = true })) do
        local client_root = client.root_dir or client.config.root_dir
        local same_root = type(root) == "string" and type(client_root) == "string"
          and source.canonical(root) == source.canonical(client_root)
        for _, folder in ipairs(client.workspace_folders or {}) do
          same_root = same_root or root ~= nil and source.canonical(vim.uri_to_fname(folder.uri)) == source.canonical(root)
        end
        if client.name == config.name and not client:is_stopped()
          and (same_root or vim.lsp.buf_is_attached(buffer, client.id)) then
          job.client = client.id
          complete_resolution(job)
          return
        end
      end
      local ok, identity = pcall(vim.lsp.start, resolved, options)
      if not ok or not identity then
        settle(job, "startup_failed: " .. config.name .. " — " .. tostring(identity or "Server launch failed."))
      else job.client = identity complete_resolution(job) end
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
      elseif vim.uv.now() - job.started >= 60000 then
        if job.client then settle(job, "timeout: " .. job.name .. " initialization exceeded 60 seconds.")
        else settle(job) end
      end
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
  if remaining == 0 then state.discovering, state.discovered = false, true return end
  local function selected()
    remaining = remaining - 1
    if remaining == 0 then state.discovering, state.discovered = false, true end
  end
  for _, config in ipairs(state.configs) do
    local sources = candidates[config.name]
    if sources then
      local index, roots = 1, {}
      local next_source
      next_source = function()
        if state.closed then return end
        local candidate = sources[index]
        if not candidate then selected() return end
        index = index + 1
        launch(state, config, candidate.path, candidate.filetype, function(root)
          if not root then return false end
          root = source.canonical(root)
          if roots[root] or not (source.inside(root, state.workspace) or source.inside(state.workspace, root)) then return false end
          roots[root] = true
          return true
        end, function()
          vim.schedule(next_source)
        end)
      end
      next_source()
    end
  end
end

---@param state ForgeLspStartup
local function discover(state)
  local queue, cursor = { state.workspace }, 1
  local candidates, candidate_directory = {}, {}
  local function next_directory()
    if state.closed then return end
    if #state.configs == 0 or cursor > #queue then
      select_sources(state, candidates)
      return
    end
    local directory = queue[cursor]
    cursor = cursor + 1
    vim.uv.fs_scandir(directory, function(failure, scan)
      vim.schedule(function()
        if state.closed then return end
        if failure then
          state.failure = "Could not discover project language sources: " .. failure
          state.discovering = false
          require("forge.infra.notifications").error(state.failure, "Harness LSP startup")
          return
        end
        local function read_batch()
          if state.closed then return end
          for _ = 1, 256 do
            local name, kind = vim.uv.fs_scandir_next(scan)
            if not name then next_directory() return end
            local path = directory .. "/" .. name
            if kind == "directory" and not excluded[name] then
              queue[#queue + 1] = path
            elseif kind == "file" then
              local filetype
              for _, config in ipairs(state.configs) do
                local manifest = project_manifest(config)
                if not manifest or name == manifest then
                  filetype = filetype or vim.filetype.match({ filename = path })
                end
                local matches = manifest and name == manifest or not manifest
                  and filetype and vim.tbl_contains(config.filetypes or {}, filetype)
                local key = config.name .. ":" .. directory
                if matches and not candidate_directory[key] then
                  candidate_directory[key] = true
                  candidates[config.name] = candidates[config.name] or {}
                  table.insert(candidates[config.name], { path = path, filetype = filetype })
                end
              end
            end
          end
          vim.schedule(read_batch)
        end
        read_batch()
      end)
    end)
  end
  next_directory()
end

---@param workspace string
---@return ForgeLspStartup
local function workspace_state(workspace)
  workspace = source.canonical(workspace)
  if workspaces[workspace] then return workspaces[workspace] end
  local state = { workspace = workspace, discovering = false, discovered = false, jobs = {}, configs = {}, timer = assert(vim.uv.new_timer()) }
  workspaces[workspace] = state
  local ok, configs = pcall(vim.lsp.get_configs, { enabled = true })
  if ok then state.configs = configs
  else state.failure = "Could not read enabled LSP configurations: " .. tostring(configs) end
  state.timer:start(50, 50, vim.schedule_wrap(function() poll(state) end))
  return state
end

---Discover project servers only for an explicit workspace request.
---@param workspace string
---@return ForgeLspStartup
function M.start(workspace)
  local state = workspace_state(workspace)
  if state.discovered then
    for _, job in pairs(state.jobs) do
      local client = job.client and vim.lsp.get_client_by_id(job.client)
      if job.failure or job.client and (not client or client:is_stopped()) then state.discovered = false break end
    end
  end
  if not state.failure and not state.discovering and not state.discovered then
    state.discovering = true
    state.timer:again()
    discover(state)
  end
  return state
end

---Wait for project discovery and matching server initialization without blocking the editor.
---@param workspace string
---@param path? string
---@param receive fun(failure?: string, issues?: table[])
---@return fun() cancel
function M.wait(workspace, path, receive)
  local file_request = path ~= nil
  if path and not source.loaded(path) then
    local stat, failure = vim.uv.fs_stat(path)
    if not stat then
      local cancelled = false
      vim.schedule(function() if not cancelled then receive(source.read_failure(path, failure or "Source is unavailable.")) end end)
      return function() cancelled = true end
    end
    if stat.type == "directory" then workspace, path, file_request = path, nil, false end
  end
  local state = file_request and workspace_state(workspace) or M.start(workspace)
  if path and source.inside(path, workspace) then prepare_path(state, source.canonical(path)) end
  local timer, started, finished = assert(vim.uv.new_timer()), vim.uv.now(), false
  local function finish(failure, cancelled, issues)
    if finished then return end
    finished = true
    timer:stop() timer:close()
    if not cancelled then receive(failure, issues) end
  end
  timer:start(0, 50, vim.schedule_wrap(function()
    if state.closed then finish("cancelled: editor LSP startup closed") return end
    local expired = vim.uv.now() - started >= 60000
    if not file_request and state.discovering and not expired then return end
    if state.failure then finish(state.failure) return end
    poll(state)
    local filetype = path and vim.filetype.match({ filename = path })
    local failure, ready, issues = nil, false, {}
    local pending = not file_request and state.discovering
    local failed_clients = {}
    local function issue(name, root, message)
      root = type(root) == "string" and root or state.workspace
      local key = name .. ":" .. source.canonical(root or state.workspace)
      if failed_clients[key] then return end
      failed_clients[key] = true
      issues[#issues + 1] = { name = name, root = root or state.workspace, state = "failed", message = message }
      if file_request then failure = failure or message end
    end
    local buffer = path and source.loaded(path)
    for _, client in ipairs(vim.lsp.get_clients({ _uninitialized = true })) do
      local config = client.config --[[@as vim.lsp.Config]]
      local attached = buffer and vim.lsp.buf_is_attached(buffer, client.id)
      if (source.belongs(client, workspace) or attached) and not client:is_stopped()
        and (not filetype or vim.tbl_contains(config.filetypes or {}, filetype))
        and (not path or source.covers(client, path) or attached) then
        if not client.initialized or client.progress and next(client.progress.pending or {}) ~= nil then
          pending = true
          if expired then issue(client.name, client.root_dir or config.root_dir, "timeout: " .. client.name .. " initialization or indexing exceeded 60 seconds.") end
        else ready = true end
      end
    end
    for _, job in pairs(state.jobs) do
      if not file_request or path and source.canonical(job.path) == source.canonical(path) then
        if not job.done then
          pending = true
          if expired then issue(job.name, job.root, "timeout: " .. job.name .. " initialization exceeded 60 seconds.") end
        end
        if job.failure then
          issue(job.name, job.root, job.failure)
        end
      end
    end
    if pending and not expired then return end
    if expired and state.discovering and not file_request then
      issue("workspace discovery", state.workspace, "timeout: workspace project discovery exceeded 60 seconds.")
    end
    if file_request and not ready and #issues > 0 then failure = failure or issues[1].message end
    finish(failure, false, issues)
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
  source.close()
end

return M
