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
---@field capture? string

local source_buffer = require("forge.lsp.source")
local canonical, read, rows = source_buffer.canonical, source_buffer.read, source_buffer.rows
local loaded, named, select_client = source_buffer.loaded, source_buffer.named, source_buffer.select_client

---@param source ForgeWorkspaceDefinition
---@param current fun(): boolean
---@param receive fun(target: ForgeDefinitionLocation?, failure: string?, message: string?)
---@return fun() cancel
function M.request(source, current, receive)
  local finished, owned, buffer, client, request_id, changedtick = false, false, nil, nil, nil, nil
  local deadline, guard, retained
  local function finish(target, failure, message)
    if finished then return end
    finished = true
    if deadline then deadline:stop() deadline:close() end
    if guard then guard:stop() guard:close() end
    if client and request_id then client:cancel_request(request_id) end
    if retained and buffer then source_buffer.release(buffer) end
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
  read(source.path, function(contents, failure)
    if finished then return end
    if not valid() then finish(nil, nil, "cancelled") return end
    if failure then finish(nil, failure) return end
    if vim.fn.sha256(contents) ~= source.digest then finish(nil, nil, "Source changed since this plan was captured") return end
    local path = canonical(source.path)
    buffer = named(path)
    client, failure = select_client(path, source.filetype, buffer, "textDocument/definition")
    if not client then finish(nil, nil, failure) return end
    local source_rows = rows(contents)
    if buffer and vim.api.nvim_buf_is_loaded(buffer) then
      if vim.bo[buffer].modified or not vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), source_rows) then
        finish(nil, nil, "Source buffer differs from the captured plan") return
      end
    else
      local open_error
      buffer, owned, open_error = source_buffer.open(source.path, contents, source.filetype)
      if not buffer then finish(nil, open_error) return end
    end
    source_buffer.retain(buffer, owned)
    retained = true
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
          read(source.path, function(latest, source_error)
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
        else read(choice.path, target_rows) end
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
