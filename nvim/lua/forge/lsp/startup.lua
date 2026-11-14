local source = require("forge.lsp.source")
local M = {}

---@class ForgeLspReadiness
---@field method? string
---@field ready? table<integer, vim.lsp.Client>
---@field accepts? fun(client: vim.lsp.Client): boolean

---@class ForgeLspStartupJob
---@field path string
---@field filetype string
---@field filetypes string[]
---@field name string
---@field started integer
---@field resolving_since? integer
---@field client? integer
---@field buffer? integer
---@field done? boolean
---@field failure? string
---@field root? string
---@field rejected? boolean
---@field on_resolved? fun(job: ForgeLspStartupJob)

---@class ForgeLspStartup
---@field workspace string
---@field jobs table<string, ForgeLspStartupJob>
---@field timer uv.uv_timer_t
---@field closed? boolean
---@field failure? string

---@type table<string, ForgeLspStartup>
local workspaces = {}
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
    resolved.capabilities = vim.tbl_deep_extend("force", resolved.capabilities or {}, require("forge.lsp.rename").capabilities())
    local start = vim.schedule_wrap(function(root)
      if state.closed or job.done then return end
      if not job.resolving_since then return end
      if vim.uv.now() - job.resolving_since >= 2000 then
        job.rejected = true
        settle(job)
        return
      end
      job.resolving_since = nil
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
          job.started = vim.uv.now()
          complete_resolution(job)
          return
        end
      end
      local ok, identity = pcall(vim.lsp.start, resolved, options)
      if not ok or not identity then
        settle(job, "startup_failed: " .. config.name .. " — " .. tostring(identity or "Server launch failed."))
      else job.client = identity job.started = vim.uv.now() complete_resolution(job) end
    end)
    job.resolving_since = vim.uv.now()
    local ok, root_error = pcall(function()
      if type(resolved.root_dir) == "function" then resolved.root_dir(buffer, start)
      else start(resolved.root_dir or (resolved.root_markers and vim.fs.root(buffer, resolved.root_markers))) end
    end)
    if not ok then settle(job, "startup_failed: " .. config.name .. " — " .. tostring(root_error)) end
  end)
end

---@param state ForgeLspStartup
local function poll(state)
  local pending = false
  for _, job in pairs(state.jobs) do
    if not job.done then
      local client = job.client and vim.lsp.get_client_by_id(job.client)
      if job.resolving_since and vim.uv.now() - job.resolving_since >= 2000 then
        job.rejected = true
        settle(job)
      elseif client and client.initialized and not client:is_stopped() and not (client.progress and next(client.progress.pending or {})) then settle(job)
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

---@param workspace string
---@return ForgeLspStartup
local function workspace_state(workspace)
  workspace = source.canonical(workspace)
  if workspaces[workspace] then return workspaces[workspace] end
  local state = { workspace = workspace, jobs = {}, timer = assert(vim.uv.new_timer()) }
  workspaces[workspace] = state
  state.timer:start(50, 50, vim.schedule_wrap(function() poll(state) end))
  return state
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

---Start one configured server for the explicit manifest, sharing existing jobs.
function M.selected(manifest, config, receive)
  local matching = {}
  for _, client in ipairs(vim.lsp.get_clients({ name = config.name, _uninitialized = true })) do
    if client.name == config.name and source.covers(client, manifest) and not client:is_stopped() then matching[#matching + 1] = client end
  end
  if #matching > 1 then receive(nil, "ambiguous_server: multiple matching clients") return function() end end
  local state = workspace_state(vim.fs.dirname(manifest))
  local cancelled = false
  launch(state, config, manifest, config.filetypes and config.filetypes[1] or "")
  local function inspect()
    if cancelled then return end
    local job = state.jobs[config.name .. ":" .. manifest]
    local client = job and job.client and vim.lsp.get_client_by_id(job.client)
    if state.closed then receive(nil, "cancelled: LSP startup closed")
    elseif job and job.failure then receive(nil, job.failure)
    elseif client and client.initialized and not (client.progress and next(client.progress.pending or {})) then receive(client)
    elseif job and job.done then receive(nil, "unavailable: selected configuration did not start a server")
    else vim.defer_fn(inspect, 20) end
  end
  inspect()
  return function() cancelled = true end
end

return M
