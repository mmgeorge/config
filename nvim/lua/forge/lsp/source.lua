local M = {}
---@type table<integer, { count: integer, owned: boolean }>
local leases = {}

---@param buffer integer
---@param owned boolean
function M.retain(buffer, owned)
  local lease = leases[buffer] or { count = 0, owned = owned }
  lease.count = lease.count + 1
  leases[buffer] = lease
end

---@param buffer integer
function M.release(buffer)
  local lease = leases[buffer]
  if not lease then return end
  lease.count = lease.count - 1
  if lease.count > 0 then return end
  leases[buffer] = nil
  if lease.owned and vim.api.nvim_buf_is_valid(buffer) and not vim.bo[buffer].modified
    and #vim.fn.win_findbuf(buffer) == 0 then vim.api.nvim_buf_delete(buffer, { force = true }) end
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
---@param limit integer
---@param receive fun(contents: string?, failure: string?)
function M.read(path, limit, receive)
  vim.uv.fs_open(path, "r", 438, function(open_error, descriptor)
    if open_error then vim.schedule(function() receive(nil, open_error) end) return end
    vim.uv.fs_fstat(descriptor, function(stat_error, stat)
      if stat_error or not stat or stat.type ~= "file" or stat.size > limit then
        vim.uv.fs_close(descriptor)
        vim.schedule(function() receive(nil, stat_error or "Source is not a regular file within the size limit") end)
        return
      end
      vim.uv.fs_read(descriptor, stat.size, 0, function(read_error, contents)
        vim.uv.fs_close(descriptor)
        vim.schedule(function() receive(contents, read_error) end)
      end)
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

---@param path string
---@param filetype string
---@param buffer integer?
---@param method? string
---@param workspace? string
---@return vim.lsp.Client?, string?
function M.select_client(path, filetype, buffer, method, workspace)
  local candidates = {}
  for _, client in ipairs(vim.lsp.get_clients()) do
    local attached = buffer and vim.lsp.buf_is_attached(buffer, client.id)
    local config = client.config --[[@as vim.lsp.Config]]
    local language = attached or vim.tbl_contains(config.filetypes or {}, filetype)
    if language and not client:is_stopped() and client:supports_method(method or "textDocument/definition", buffer)
      and (not workspace or M.belongs(client, workspace)) then
      local specificity = -1
      for _, folder in ipairs(client.workspace_folders or {}) do
        local root = vim.uri_to_fname(folder.uri)
        if M.inside(path, root) then specificity = math.max(specificity, #M.canonical(root)) end
      end
      local root = client.root_dir or client.config.root_dir
      if type(root) == "string" and M.inside(path, root) then specificity = math.max(specificity, #M.canonical(root)) end
      if specificity >= 0 or workspace then candidates[#candidates + 1] = { client = client, attached = attached == true, specificity = specificity } end
    end
  end
  table.sort(candidates, function(left, right)
    if left.attached ~= right.attached then return left.attached end
    return left.specificity > right.specificity
  end)
  if #candidates == 0 then return nil, "No running project language server supports this operation for the source" end
  if candidates[2] and candidates[1].attached == candidates[2].attached and candidates[1].specificity == candidates[2].specificity then
    return nil, "Multiple running language servers match this source. Attach the intended server to the source buffer."
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
