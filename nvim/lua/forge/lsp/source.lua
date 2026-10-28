local M = {}

---@param path string
---@param failure string
---@return string
function M.read_failure(path, failure)
  if failure:find("ENOENT", 1, true) then return "path_not_found: " .. path end
  if failure:find("EACCES", 1, true) or failure:find("EPERM", 1, true) then return "access_denied: " .. path end
  return "read_failed: " .. path .. " — " .. failure
end
---@type table<integer, { count: integer, owned: boolean }>
local leases = {}

---@param buffer integer
---@param owned boolean
function M.retain(buffer, owned)
  local lease = leases[buffer] or { count = 0, owned = owned, generation = 0 }
  if lease.timer and not lease.timer:is_closing() then lease.timer:stop() lease.timer:close() end
  lease.timer = nil
  lease.generation = lease.generation + 1
  lease.count = lease.count + 1
  leases[buffer] = lease
end

---@param buffer integer
function M.release(buffer)
  local lease = leases[buffer]
  if not lease then return end
  lease.count = lease.count - 1
  if lease.count > 0 then return end
  if not lease.owned then leases[buffer] = nil return end
  local generation = lease.generation
  lease.timer = vim.defer_fn(function()
    if leases[buffer] ~= lease or lease.count ~= 0 or lease.generation ~= generation then return end
    leases[buffer] = nil
    if vim.api.nvim_buf_is_valid(buffer) and not vim.bo[buffer].modified
      and #vim.fn.win_findbuf(buffer) == 0 then vim.api.nvim_buf_delete(buffer, { force = true }) end
  end, 60000)
end

---Refresh retained hidden source from disk without replacing editor-owned changes.
---@param buffer integer
---@param receive fun(failure: string?)
function M.refresh(buffer, receive)
  local lease = leases[buffer]
  if not lease or not lease.owned or vim.bo[buffer].modified or #vim.fn.win_findbuf(buffer) > 0 then receive() return end
  M.retain(buffer, true)
  local tick = vim.api.nvim_buf_get_changedtick(buffer)
  M.read(vim.api.nvim_buf_get_name(buffer), function(contents, failure)
    if not failure and (not vim.api.nvim_buf_is_valid(buffer) or vim.api.nvim_buf_get_changedtick(buffer) ~= tick) then
      failure = "source_changed: retained source changed while refreshing"
    end
    if not failure and not vim.bo[buffer].modified and #vim.fn.win_findbuf(buffer) == 0 then
      local rows = M.rows(contents)
      if not vim.deep_equal(rows, vim.api.nvim_buf_get_lines(buffer, 0, -1, false)) then
        vim.api.nvim_buf_set_lines(buffer, 0, -1, false, rows)
        vim.bo[buffer].modified = false
      end
    end
    M.release(buffer)
    receive(failure)
  end)
end

---Release retained source buffers when the Harness language service closes.
function M.close()
  for buffer, lease in pairs(leases) do
    if lease.count == 0 then
      if lease.timer and not lease.timer:is_closing() then lease.timer:stop() lease.timer:close() end
      leases[buffer] = nil
      if lease.owned and vim.api.nvim_buf_is_valid(buffer) and not vim.bo[buffer].modified
        and #vim.fn.win_findbuf(buffer) == 0 then vim.api.nvim_buf_delete(buffer, { force = true }) end
    end
  end
end

---@param path string
---@return string
function M.canonical(path)
  local normalized = vim.fs.normalize(vim.uv.fs_realpath(path) or path)
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

---@param path string
---@param root string
---@return boolean
function M.inside(path, root)
  root = M.canonical(root):gsub("/$", "")
  path = M.canonical(path)
  return path == root or path:sub(1, #root + 1) == root .. "/"
end

---@param path string
---@param receive fun(contents: string?, failure: string?)
function M.read(path, receive)
  vim.uv.fs_open(path, "r", 438, function(open_error, descriptor)
    if open_error then vim.schedule(function() receive(nil, M.read_failure(path, open_error)) end) return end
    vim.uv.fs_fstat(descriptor, function(stat_error, stat)
      if stat_error or not stat or stat.type ~= "file" then
        vim.uv.fs_close(descriptor)
        local failure = stat_error and M.read_failure(path, stat_error)
          or (stat and stat.type ~= "file" and "invalid_path: " .. path .. " is not a regular file.")
          or ("read_failed: No file metadata for " .. path)
        vim.schedule(function() receive(nil, failure) end)
        return
      end
      ---@type string[]
      local chunks = {}
      local offset = 0
      ---@param contents string?
      ---@param failure string?
      local function finish(contents, failure)
        vim.uv.fs_close(descriptor, function(close_error)
          vim.schedule(function()
            failure = failure or (close_error and M.read_failure(path, close_error))
            receive(not failure and contents or nil, failure)
          end)
        end)
      end
      ---@type fun()
      local read_next
      read_next = function()
        vim.uv.fs_read(descriptor, 64 * 1024, offset, function(read_error, contents)
          if read_error then finish(nil, M.read_failure(path, read_error)) return end
          if not contents then finish(nil, "read_failed: No file contents for " .. path) return end
          if contents == "" then finish(table.concat(chunks), nil) return end
          chunks[#chunks + 1] = contents
          offset = offset + #contents
          read_next()
        end)
      end
      read_next()
    end)
  end)
end

---@param contents string
---@return string[]
function M.rows(contents)
  return vim.split(contents:gsub("\r\n", "\n"):gsub("\n$", ""), "\n", { plain = true })
end

---@param path string
---@return integer?
function M.named(path)
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buffer) ~= "" and M.canonical(vim.api.nvim_buf_get_name(buffer)) == M.canonical(path) then return buffer end
  end
end

---@param path string
---@return integer?
function M.loaded(path)
  local buffer = M.named(path)
  return buffer and vim.api.nvim_buf_is_loaded(buffer) and buffer or nil
end

---@param client vim.lsp.Client
---@param workspace string
---@return boolean
function M.belongs(client, workspace)
  local root = client.root_dir or client.config.root_dir
  if type(root) == "string" and (M.inside(workspace, root) or M.inside(root, workspace)) then return true end
  for _, folder in ipairs(client.workspace_folders or {}) do
    root = vim.uri_to_fname(folder.uri)
    if M.inside(workspace, root) or M.inside(root, workspace) then return true end
  end
  return false
end

---@param client vim.lsp.Client
---@param path string
---@return boolean
function M.covers(client, path)
  local root = client.root_dir or client.config.root_dir
  if type(root) == "string" and M.inside(path, root) then return true end
  for _, folder in ipairs(client.workspace_folders or {}) do
    if M.inside(path, vim.uri_to_fname(folder.uri)) then return true end
  end
  return false
end

---@param path string
---@param filetype string
---@param buffer integer?
---@param method? string
---@param workspace? string
---@return vim.lsp.Client?, string?
function M.select_client(path, filetype, buffer, method, workspace)
  local candidates, unsupported = {}, {}
  for _, client in ipairs(vim.lsp.get_clients()) do
    local attached = buffer and vim.lsp.buf_is_attached(buffer, client.id)
    local config = client.config --[[@as vim.lsp.Config]]
    local language = attached or vim.tbl_contains(config.filetypes or {}, filetype)
    if language and not client:is_stopped()
      and (attached or not workspace or M.belongs(client, workspace)) then
      local specificity = -1
      for _, folder in ipairs(client.workspace_folders or {}) do
        local root = vim.uri_to_fname(folder.uri)
        if M.inside(path, root) then specificity = math.max(specificity, #M.canonical(root)) end
      end
      local root = client.root_dir or client.config.root_dir
      if type(root) == "string" and M.inside(path, root) then specificity = math.max(specificity, #M.canonical(root)) end
      if specificity >= 0 or attached then
        if not method or client:supports_method(method, buffer) then
          candidates[#candidates + 1] = { client = client, attached = attached == true, specificity = specificity }
        else unsupported[#unsupported + 1] = client.name end
      end
    end
  end
  table.sort(candidates, function(left, right)
    if left.attached ~= right.attached then return left.attached end
    return left.specificity > right.specificity
  end)
  if #candidates == 0 then
    if #unsupported > 0 then
      table.sort(unsupported)
      local label = ({ ["textDocument/prepareCallHierarchy"] = "call hierarchy", ["textDocument/typeDefinition"] = "type definition",
        ["textDocument/implementation"] = "implementation lookup", ["workspace/symbol"] = "workspace symbols" })[method]
        or (method or "this operation"):match("[^/]+$")
      return nil, "unsupported: " .. table.concat(unsupported, ", ") .. " does not support " .. label .. "."
    end
    return nil, "unavailable: No running " .. filetype .. " language server for this project."
  end
  if candidates[2] and candidates[1].attached == candidates[2].attached and candidates[1].specificity == candidates[2].specificity then
    local names = {}
    for _, candidate in ipairs(candidates) do
      if candidate.attached == candidates[1].attached and candidate.specificity == candidates[1].specificity then names[#names + 1] = candidate.client.name end
    end
    table.sort(names)
    return nil, "ambiguous_server: " .. table.concat(names, ", ")
  end
  return candidates[1].client
end

---@param path string
---@param contents string
---@param filetype string
---@return integer?, boolean, string?
function M.open(path, contents, filetype)
  local buffer = M.named(path)
  if buffer and vim.api.nvim_buf_is_loaded(buffer) then return buffer, false end
  local owned = buffer == nil
  buffer = buffer or vim.api.nvim_create_buf(false, false)
  local eventignore = vim.o.eventignore
  vim.o.eventignore = "all"
  local success, failure = pcall(function()
    vim.api.nvim_buf_set_name(buffer, path)
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, M.rows(contents))
    vim.bo[buffer].fileformat = contents:find("\r\n", 1, true) and "dos" or "unix"
    vim.bo[buffer].endofline = contents:sub(-1) == "\n"
    vim.bo[buffer].filetype = filetype
    vim.bo[buffer].bufhidden = "hide"
    vim.bo[buffer].swapfile = false
    vim.bo[buffer].modified = false
  end)
  vim.o.eventignore = eventignore
  if not success then
    if owned and vim.api.nvim_buf_is_valid(buffer) then vim.api.nvim_buf_delete(buffer, { force = true }) end
    return nil, false, tostring(failure)
  end
  return buffer, owned
end

return M
