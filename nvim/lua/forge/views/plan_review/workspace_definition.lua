local M = {}

---@class ForgeWorkspaceDefinition
---@field path string
---@field digest string
---@field line integer
---@field column integer
---@field filetype string

---@class ForgeDefinitionLocation
---@field path string
---@field line integer
---@field column integer
---@field modified? boolean

local function canonical(path)
  local normalized = vim.fs.normalize(vim.uv.fs_realpath(path) or path)
  return vim.fn.has("win32") == 1 and normalized:lower() or normalized
end

local function inside(path, root)
  root = canonical(root):gsub("/$", "")
  return path == root or path:sub(1, #root + 1) == root .. "/"
end

local function read(path, limit, receive)
  vim.uv.fs_open(path, "r", 438, function(open_error, descriptor)
    if open_error then vim.schedule(function() receive(nil, open_error) end) return end
    vim.uv.fs_fstat(descriptor, function(stat_error, stat)
      if stat_error or not stat or stat.type ~= "file" or stat.size > limit then
        vim.uv.fs_close(descriptor)
        vim.schedule(function() receive(nil, stat_error or "Definition file is not a regular file within the size limit") end)
        return
      end
      vim.uv.fs_read(descriptor, stat.size, 0, function(read_error, contents)
        vim.uv.fs_close(descriptor)
        vim.schedule(function() receive(contents, read_error) end)
      end)
    end)
  end)
end

local function rows(contents)
  return vim.split(contents:gsub("\r\n", "\n"):gsub("\n$", ""), "\n", { plain = true })
end

local function loaded(path)
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buffer) and canonical(vim.api.nvim_buf_get_name(buffer)) == path then return buffer end
  end
end

local function named(path)
  for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_get_name(buffer) ~= "" and canonical(vim.api.nvim_buf_get_name(buffer)) == path then return buffer end
  end
end

local function select_client(path, filetype, buffer)
  local candidates = {}
  for _, client in ipairs(vim.lsp.get_clients()) do
    local attached = buffer and vim.lsp.buf_is_attached(buffer, client.id)
    local language = attached or vim.tbl_contains(client.config.filetypes or {}, filetype)
    if language and not client:is_stopped() and client:supports_method("textDocument/definition", buffer) then
      local specificity = -1
      for _, folder in ipairs(client.workspace_folders or {}) do
        local root = vim.uri_to_fname(folder.uri)
        if inside(path, root) then specificity = math.max(specificity, #canonical(root)) end
      end
      local root = client.root_dir or client.config.root_dir
      if type(root) == "string" and inside(path, root) then specificity = math.max(specificity, #canonical(root)) end
      if specificity >= 0 then candidates[#candidates + 1] = { client = client, attached = attached == true, specificity = specificity } end
    end
  end
  table.sort(candidates, function(left, right)
    if left.attached ~= right.attached then return left.attached end
    return left.specificity > right.specificity
  end)
  if #candidates == 0 then return nil, "No running project language server supports definition lookup for this source" end
  if candidates[2] and candidates[1].attached == candidates[2].attached
    and candidates[1].specificity == candidates[2].specificity then
    return nil, "Multiple running language servers match this source. Attach the intended server to the source buffer."
  end
  return candidates[1].client
end

---@param source ForgeWorkspaceDefinition
---@param current fun(): boolean
---@param receive fun(target: ForgeDefinitionLocation?, failure: string?, message: string?)
---@return fun() cancel
function M.request(source, current, receive)
  local finished, owned, buffer, client, request_id, changedtick = false, false, nil, nil, nil, nil
  local deadline, guard
  local function finish(target, failure, message)
    if finished then return end
    finished = true
    if deadline then deadline:stop() deadline:close() end
    if guard then guard:stop() guard:close() end
    if client and request_id then client:cancel_request(request_id) end
    if owned and buffer and vim.api.nvim_buf_is_valid(buffer) and not vim.bo[buffer].modified
      and #vim.fn.win_findbuf(buffer) == 0 then vim.api.nvim_buf_delete(buffer, { force = true }) end
    receive(target, failure, message)
  end
  local function valid()
    return not finished and current() and (not buffer or vim.api.nvim_buf_is_valid(buffer)
      and vim.api.nvim_buf_get_changedtick(buffer) == changedtick)
      and (not client or not client:is_stopped())
  end
  deadline = vim.uv.new_timer()
  deadline:start(5000, 0, vim.schedule_wrap(function() finish(nil, "Language-server definition lookup timed out after five seconds") end))
  guard = vim.uv.new_timer()
  guard:start(50, 50, vim.schedule_wrap(function()
    if not valid() then finish(nil, nil, "cancelled") end
  end))
  read(source.path, 1024 * 1024, function(contents, failure)
    if finished then return end
    if not valid() then finish(nil, nil, "cancelled") return end
    if failure then finish(nil, failure) return end
    if vim.fn.sha256(contents) ~= source.digest then finish(nil, nil, "Source changed since this plan was captured") return end
    local path = canonical(source.path)
    buffer = named(path)
    client, failure = select_client(path, source.filetype, buffer)
    if not client then finish(nil, nil, failure) return end
    local source_rows = rows(contents)
    if buffer and vim.api.nvim_buf_is_loaded(buffer) then
      if vim.bo[buffer].modified or not vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), source_rows) then
        finish(nil, nil, "Source buffer differs from the captured plan") return
      end
    else
      owned = buffer == nil
      buffer = buffer or vim.api.nvim_create_buf(false, false)
      -- Hidden source setup must not trigger configured LSP autostart.
      local eventignore = vim.o.eventignore
      vim.o.eventignore = "all"
      local success, setup_error = pcall(function()
        vim.api.nvim_buf_set_name(buffer, source.path)
        vim.api.nvim_buf_set_lines(buffer, 0, -1, false, source_rows)
        vim.bo[buffer].fileformat = contents:find("\r\n", 1, true) and "dos" or "unix"
        vim.bo[buffer].endofline = contents:sub(-1) == "\n"
        vim.bo[buffer].filetype = source.filetype
        vim.bo[buffer].bufhidden = "hide"
        vim.bo[buffer].swapfile = false
        vim.bo[buffer].modified = false
      end)
      vim.o.eventignore = eventignore
      if not success then finish(nil, tostring(setup_error)) return end
    end
    changedtick = vim.api.nvim_buf_get_changedtick(buffer)
    if not vim.lsp.buf_is_attached(buffer, client.id) and not vim.lsp.buf_attach_client(buffer, client.id) then
      finish(nil, "Could not attach the running language server to the source buffer") return
    end
    local line = source_rows[source.line]
    if not line or source.column > #line then finish(nil, "Captured definition position is outside the source") return end
    local encoding = client.offset_encoding or "utf-16"
    local converted, character = pcall(vim.str_utfindex, line, encoding, source.column, false)
    if not converted then finish(nil, tostring(character)) return end
    local sent
    sent, request_id = client:request("textDocument/definition", {
      textDocument = { uri = vim.uri_from_fname(source.path) },
      position = { line = source.line - 1, character = character },
    }, function(request_error, result)
      if finished then return end
      request_id = nil
      if not valid() then finish(nil, nil, "cancelled") return end
      if request_error then
        if request_error.code == -32800 or request_error.code == -32801 or request_error.code == -32802 then
          finish(nil, nil, "Language server cancelled the lookup while updating its source. Retry the jump.")
        else finish(nil, request_error.message or vim.inspect(request_error)) end
        return
      end
      if not result or result == vim.NIL then finish(nil, nil, "No definition returned by the project language server") return end
      local locations = (result.uri or result.targetUri) and { result } or result
      local choices, seen = {}, {}
      for _, location in ipairs(locations) do
        local uri = location.targetUri or location.uri
        local range = location.targetSelectionRange or location.range or location.targetRange
        if type(uri) ~= "string" or uri:sub(1, 7) ~= "file://" or not range or not range.start then
          finish(nil, "Language server returned an unsupported definition location") return
        end
        local target_path = vim.fs.normalize(vim.uri_to_fname(uri))
        local key = canonical(target_path) .. ":" .. range.start.line .. ":" .. range.start.character
        if not seen[key] then
          seen[key] = true
          choices[#choices + 1] = { path = target_path, position = range.start }
        end
      end
      local function selected(choice)
        if not valid() then finish(nil, nil, "cancelled") return end
        if not choice then finish(nil, nil, "cancelled") return end
        local function target_rows(target_contents, target_error)
          if not valid() then finish(nil, nil, "cancelled") return end
          if target_error then finish(nil, target_error) return end
          local target_line = rows(target_contents)[choice.position.line + 1]
          if not target_line then finish(nil, "Definition position is outside the target file") return end
          local converted, byte_column = pcall(vim.str_byteindex, target_line, encoding, choice.position.character, false)
          if not converted then finish(nil, tostring(byte_column)) return end
          read(source.path, 1024 * 1024, function(latest, source_error)
            if not valid() then finish(nil, nil, "cancelled") return end
            if source_error then finish(nil, source_error) return end
            if vim.fn.sha256(latest) ~= source.digest then finish(nil, nil, "Source changed during definition lookup") return end
            local destination = loaded(canonical(choice.path))
            finish({ path = choice.path, line = choice.position.line + 1, column = byte_column,
              modified = destination and vim.bo[destination].modified or false })
          end)
        end
        local target_buffer = loaded(canonical(choice.path))
        if target_buffer then target_rows(table.concat(vim.api.nvim_buf_get_lines(target_buffer, 0, -1, false), "\n"))
        else read(choice.path, 16 * 1024 * 1024, target_rows) end
      end
      if #choices == 0 then finish(nil, nil, "No definition returned by the project language server")
      elseif #choices == 1 then selected(choices[1])
      else
        deadline:stop()
        vim.ui.select(choices, { prompt = "Definition", format_item = function(choice)
        return choice.path .. ":" .. (choice.position.line + 1)
        end }, function(choice)
          if finished then return end
          deadline:start(5000, 0, vim.schedule_wrap(function() finish(nil, "Definition target loading timed out after five seconds") end))
          selected(choice)
        end)
      end
    end, buffer)
    if not sent then finish(nil, "Language server rejected the definition request") end
  end)
  return function() finish(nil, nil, "cancelled") end
end

return M
