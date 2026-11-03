local source = require("forge.lsp.source")
local snapshots = require("forge.lsp.snapshot")
local semantic = require("forge.lsp.semantic")
local M = {}

---@class ForgeRenameHold
---@field buffers table<integer, {tick: integer, modifiable: boolean, path: string, clients: integer[]}>
---@field operations table[]
---@field notifications table[]
---@field leases integer[]

---@type table<string, ForgeRenameHold>
local holds = {}

---Advertise only the workspace-edit features implemented by the transaction owner.
---@return lsp.ClientCapabilities
function M.capabilities()
  return { workspace = { workspaceEdit = { documentChanges = true, resourceOperations = { "create", "rename", "delete" }, failureHandling = "undo" },
    fileOperations = { dynamicRegistration = true, willRename = true, didRename = true, willCreate = true, didCreate = true, willDelete = true, didDelete = true } },
    textDocument = { rename = { dynamicRegistration = true, prepareSupport = true, prepareSupportDefaultBehavior = 1, honorsChangeAnnotations = false } } }
end

local function path(uri)
  if type(uri) ~= "string" or not uri:match("^file:") then return nil, "unsupported: Rename requires file URIs." end
  return vim.fs.normalize(vim.uri_to_fname(uri))
end

local function matches(client, method, filename, directory)
  local field = method:match("/(.+)Files$")
  local capability = client.server_capabilities.workspace and client.server_capabilities.workspace.fileOperations
  local registrations = {}
  if capability and capability[field] then registrations[#registrations + 1] = capability[field] end
  if client.dynamic_capabilities then
    for _, registration in ipairs(client.dynamic_capabilities:get(method) or {}) do
      registrations[#registrations + 1] = registration.registerOptions or {}
    end
  end
  for _, registration in ipairs(registrations) do
    for _, filter in ipairs(registration.filters or {}) do
      local pattern = filter.pattern
      if (not filter.scheme or filter.scheme == "file") and pattern
        and (not pattern.matches or pattern.matches == (directory and "folder" or "file")) then
        local glob, candidate = pattern.glob, filename:gsub("\\", "/")
        if pattern.options and pattern.options.ignoreCase then glob, candidate = glob:lower(), candidate:lower() end
        local ok, matched = pcall(function() return vim.glob.to_lpeg(glob):match(candidate) end)
        if ok and matched then return true end
      end
    end
  end
  return false
end

---Release all held buffers, synchronizing only after a committed host transaction.
---@param id string
---@param committed boolean
function M.release(id, committed)
  local hold = holds[id]
  if not hold then return end
  holds[id] = nil
  local destinations, views = {}, {}
  if committed then
    for _, window in ipairs(vim.api.nvim_list_wins()) do
      if hold.buffers[vim.api.nvim_win_get_buf(window)] then
        views[window] = vim.api.nvim_win_call(window, vim.fn.winsaveview)
      end
    end
    for buffer, state in pairs(hold.buffers) do
      local filename, deleted = state.path, false
      for _, operation in ipairs(hold.operations) do
        if operation.kind == "rename" then
          if source.inside(filename, operation.old_path) then
            filename = operation.new_path .. filename:sub(#operation.old_path + 1)
          elseif source.inside(filename, operation.new_path) then deleted = true end
        elseif operation.kind == "delete" and source.inside(filename, operation.path) then deleted = true end
      end
      destinations[buffer] = { path = filename, deleted = deleted, clients = state.clients }
      if vim.api.nvim_buf_is_valid(buffer) and (filename ~= state.path or deleted) then
        for _, client_id in ipairs(state.clients) do
          if vim.lsp.buf_is_attached(buffer, client_id) then vim.lsp.buf_detach_client(buffer, client_id) end
        end
        vim.api.nvim_buf_set_name(buffer, "")
      end
    end
  end
  for buffer, state in pairs(hold.buffers) do
    if vim.api.nvim_buf_is_valid(buffer) then
      vim.bo[buffer].modifiable = state.modifiable
      if committed then
        local destination = destinations[buffer]
        local ok, failure = pcall(function()
          if destination.deleted then
            vim.bo[buffer].buftype = "nofile"
            vim.bo[buffer].modified = false
          elseif vim.api.nvim_buf_get_changedtick(buffer) == state.tick and not vim.bo[buffer].modified then
            if destination.path ~= state.path then vim.api.nvim_buf_set_name(buffer, destination.path) end
            vim.bo[buffer].modifiable = true
            vim.api.nvim_buf_call(buffer, function() vim.cmd("silent noautocmd edit!") end)
            -- Detach cleanup is scheduled by Neovim; reattach after it settles.
            vim.schedule(function()
              if not vim.api.nvim_buf_is_valid(buffer) or source.canonical(vim.api.nvim_buf_get_name(buffer)) ~= source.canonical(destination.path) then return end
              vim.fn.bufload(buffer)
              for _, client_id in ipairs(destination.clients) do
                local client = vim.lsp.get_client_by_id(client_id)
                if client and not client:is_stopped() then vim.lsp.buf_attach_client(buffer, client_id) end
              end
            end)
          else
            error("buffer changed during commit: " .. destination.path)
          end
        end)
        vim.bo[buffer].modifiable = state.modifiable
        if not ok then require("forge.infra.notifications").error("Rename committed on disk: " .. tostring(failure), "Harness LSP") end
      end
    end
  end
  for window, view in pairs(views) do
    if vim.api.nvim_win_is_valid(window) then vim.api.nvim_win_call(window, function() vim.fn.winrestview(view) end) end
  end
  for _, buffer in ipairs(hold.leases) do source.release(buffer) end
  if committed then
    for _, notice in ipairs(hold.notifications) do
      if not notice.client:is_stopped() then notice.client:notify(notice.method, notice.params) end
    end
  end
end

---Release preparations when the editor connection is replaced.
function M.close()
  for id in pairs(holds) do M.release(id, false) end
end

---Compute complete server edits without modifying source buffers or filesystem state.
---@param id string
---@param workspace string
---@param request table
---@param authorize function
---@param analyze function
---@param receive fun(result: table?, failure: string?)
---@return fun()
function M.prepare(id, workspace, request, authorize, analyze, receive)
  local finished, active_cancel = false, nil
  local readiness = {}
  local timer = assert(vim.uv.new_timer())
  local hold = { buffers = {}, operations = {}, notifications = {}, leases = {} }
  local captured = snapshots.new(authorize, analyze)
  local virtual, absent, redirects = {}, {}, {}
  local merging, merged = false, {}
  local function finish(failure)
    if finished then return end
    finished = true
    timer:stop() timer:close()
    if active_cancel then active_cancel() active_cancel = nil end
    if failure then
      holds[id] = hold
      M.release(id, false)
      receive(nil, failure)
    else
      local current, stale = captured.current()
      for buffer, state in pairs(hold.buffers) do
        if not vim.api.nvim_buf_is_valid(buffer) or vim.bo[buffer].modified or vim.api.nvim_buf_get_changedtick(buffer) ~= state.tick then
          current, stale = false, "source_changed: " .. state.path
        end
      end
      holds[id] = hold
      if not current then M.release(id, false) receive(nil, stale)
      else receive({ operations = hold.operations }) end
    end
  end
  local function guard(callback)
    return function(...)
      if finished then return end
      local ok, failure = pcall(callback, ...)
      if not ok then finish("invalid_edit: " .. tostring(failure)) end
    end
  end
  local function touch(filename)
    for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
      local name = vim.api.nvim_buf_get_name(buffer)
      if name ~= "" and source.inside(name, filename) then
        if vim.bo[buffer].modified then return "unsaved_changes: " .. name end
        if not hold.buffers[buffer] then
          local attached = {}
          for _, client in ipairs(vim.lsp.get_clients({ bufnr = buffer })) do
            if vim.lsp.buf_is_attached(buffer, client.id) then attached[#attached + 1] = client.id end
          end
          hold.buffers[buffer] = { tick = vim.api.nvim_buf_get_changedtick(buffer), modifiable = vim.bo[buffer].modifiable, path = name, clients = attached }
          vim.bo[buffer].modifiable = false
        end
      end
    end
  end
  local function read(filename, callback)
    local failure = touch(filename)
    if failure then finish(failure) return end
    local canonical = source.canonical(filename)
    if absent[canonical] then finish("path_not_found: " .. filename) return end
    if virtual[canonical] ~= nil then callback(virtual[canonical]) return end
    authorize({ filename }, guard(function(allowed, denied)
      if denied or not allowed[filename] then finish(denied or ("access_denied: " .. filename)) return end
      local disk_path = filename
      for index = #redirects, 1, -1 do
        local move = redirects[index]
        if source.inside(disk_path, move.new_path) then disk_path = move.old_path .. disk_path:sub(#move.new_path + 1) end
      end
      source.read(disk_path, guard(function(contents, read_error)
        if read_error then finish(read_error) return end
        local buffer = source.loaded(disk_path)
        if buffer then
          local separator = vim.bo[buffer].fileformat == "dos" and "\r\n" or "\n"
          local loaded = table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), separator)
            .. (vim.bo[buffer].endofline and separator or "")
          if loaded ~= contents then finish("source_changed: " .. filename .. " differs from its editor buffer.") return end
        end
        virtual[canonical] = contents
        callback(contents)
      end))
    end))
  end
  local function send(client, method, params, buffer, callback)
    local accepted, request_id = client:request(method, params, guard(function(error, result)
      active_cancel = nil
      if error then finish(method .. ": " .. (error.message or tostring(error))) return end
      if client:is_stopped() then finish("disconnected: " .. client.name) return end
      callback(result == vim.NIL and nil or result)
    end), buffer)
    if not accepted then finish("disconnected: " .. client.name) return end
    if not finished then active_cancel = function() client:cancel_request(request_id) end end
  end
  local function offset(contents, position, encoding)
    assert(type(position) == "table" and position.line >= 0 and position.character >= 0, "invalid edit position")
    local start = 1
    for _ = 1, position.line do start = assert(contents:find("\n", start, true), "line outside source") + 1 end
    local ending = contents:find("\n", start, true) or (#contents + 1)
    local line = contents:sub(start, ending - 1):gsub("\r$", "")
    local byte = vim.str_byteindex(line, encoding, position.character, true)
    return start - 1 + byte
  end
  local function edit_text(filename, edits, encoding, version, callback)
    local buffer = source.loaded(filename)
    if version ~= nil and version ~= vim.NIL and (not buffer or vim.lsp.util.buf_versions[buffer] ~= version) then
      finish("source_changed: " .. filename) return
    end
    read(filename, function(before)
      local group = merging and merged[source.canonical(filename)] or nil
      if group then before = group.before end
      local ranges = group and vim.deepcopy(group.ranges) or {}
      local prior_count = #ranges
      for _, edit in ipairs(edits) do
        assert(type(edit.newText) == "string", "edit text is missing")
        local range = { first = offset(before, edit.range.start, encoding), last = offset(before, edit.range["end"], encoding), text = edit.newText, ordinal = #ranges + 1 }
        local duplicate = false
        for index = 1, prior_count do
          local existing = ranges[index]
          if existing.first == range.first and existing.last == range.last and existing.text == range.text then duplicate = true break end
        end
        if not duplicate then ranges[#ranges + 1] = range end
      end
      table.sort(ranges, function(a, b) return a.first < b.first or a.first == b.first and a.ordinal < b.ordinal end)
      local ending = 0
      for _, range in ipairs(ranges) do
        assert(range.first >= ending and range.last >= range.first, "overlapping or inverted edits")
        ending = range.last
      end
      local after = before
      for index = #ranges, 1, -1 do
        local range = ranges[index]
        local replacement = range.text
        if before:find("\r\n", 1, true) then replacement = replacement:gsub("\r\n", "\n"):gsub("\n", "\r\n") end
        after = after:sub(1, range.first) .. replacement .. after:sub(range.last + 1)
      end
      virtual[source.canonical(filename)] = after
      if group then group.operation.after, group.operation.edits, group.ranges = after, #ranges, ranges
      else
        local operation = { kind = "text", path = filename, before = before, after = after, edits = #ranges }
        hold.operations[#hold.operations + 1] = operation
        if merging then merged[source.canonical(filename)] = { before = before, ranges = ranges, operation = operation } end
      end
      callback()
    end)
  end
  local apply_edit
  local function resource_clients(operation, phase)
    local suffix = ({ create = "Create", rename = "Rename", delete = "Delete" })[operation.kind]
    local method = "workspace/" .. phase .. suffix .. "Files"
    local filename = operation.old_path or operation.path
    local stat = vim.uv.fs_stat(filename)
    local directory = stat and stat.type == "directory" or false
    local selected = {}
    for _, client in ipairs(vim.lsp.get_clients()) do
      if not client:is_stopped() and source.belongs(client, filename) and matches(client, method, filename, directory) then
        selected[#selected + 1] = client
      end
    end
    table.sort(selected, function(a, b) return a.id < b.id end)
    local file = operation.kind == "rename"
      and { oldUri = vim.uri_from_fname(operation.old_path), newUri = vim.uri_from_fname(operation.new_path) }
      or { uri = vim.uri_from_fname(operation.path) }
    return selected, method, { files = { file } }
  end
  local function notify_for(operation)
    local selected, method, params = resource_clients(operation, "did")
    for _, client in ipairs(selected) do
      hold.notifications[#hold.notifications + 1] = { client = client, method = method, params = params }
    end
  end
  local function resource_hooks(operation, callback)
    local selected, method, params = resource_clients(operation, "will")
    local index = 0
    local function next_client()
      index = index + 1
      local client = selected[index]
      if not client then notify_for(operation) callback() return end
      send(client, method, params, nil, function(edit) apply_edit(edit, client, next_client, true) end)
    end
    next_client()
  end
  apply_edit = function(edit, client, callback, hook)
    if not edit or edit == vim.NIL then callback() return end
    local changes = edit.documentChanges
    if not changes then
      changes = {}
      for uri, edits in pairs(edit.changes or {}) do changes[#changes + 1] = { textDocument = { uri = uri }, edits = edits } end
      table.sort(changes, function(a, b) return a.textDocument.uri < b.textDocument.uri end)
    end
    local index = 0
    local function next_change()
      if finished then return end
      index = index + 1
      local change = changes[index]
      if not change then callback() return end
      if change.textDocument then
        local filename, failure = path(change.textDocument.uri)
        if not filename then finish(failure) return end
        edit_text(filename, change.edits, client.offset_encoding or "utf-16", change.textDocument.version, next_change)
      else
        if hook then finish("unsupported: File-operation preparation returned nested resource operations.") return end
        local operation = { kind = change.kind }
        local update_virtual
        local options = change.options or {}
        if change.kind == "rename" then
          local old, failure = path(change.oldUri)
          local new, other = path(change.newUri)
          if not old or not new then finish(failure or other) return end
          operation.old_path, operation.new_path = old, new
          operation.overwrite, operation.ignore_if_exists = options.overwrite == true, options.ignoreIfExists == true
          failure = touch(old) or touch(new)
          if failure then finish(failure) return end
          update_virtual = function()
            if operation.ignore_if_exists and not operation.overwrite and vim.uv.fs_stat(new) then return end
            redirects[#redirects + 1] = operation
            for filename, contents in pairs(vim.deepcopy(virtual)) do
              if source.inside(filename, old) then
                virtual[source.canonical(new) .. filename:sub(#source.canonical(old) + 1)] = contents
                virtual[filename], absent[filename] = nil, true
              end
            end
          end
        elseif change.kind == "create" or change.kind == "delete" then
          local filename, failure = path(change.uri)
          if not filename then finish(failure) return end
          operation.path = filename
          failure = touch(filename)
          if failure then finish(failure) return end
          if change.kind == "create" then
            operation.overwrite, operation.ignore_if_exists = options.overwrite == true, options.ignoreIfExists == true
            update_virtual = function()
              if options.overwrite or not options.ignoreIfExists or not vim.uv.fs_stat(filename) then
                virtual[source.canonical(filename)], absent[source.canonical(filename)] = "", nil
              end
            end
          else
            operation.recursive, operation.ignore_if_not_exists = options.recursive == true, options.ignoreIfNotExists == true
            update_virtual = function() virtual[source.canonical(filename)], absent[source.canonical(filename)] = nil, true end
          end
        else finish("unsupported: Unknown resource operation.") return end
        local authorization = operation.kind == "rename" and { operation.old_path, operation.new_path } or { operation.path }
        authorize(authorization, guard(function(allowed, denied)
          if denied then finish(denied) return end
          for _, filename in ipairs(authorization) do
            if not allowed[filename] then finish("access_denied: " .. filename) return end
          end
          resource_hooks(operation, function()
            update_virtual()
            hold.operations[#hold.operations + 1] = operation
            next_change()
          end)
        end))
      end
    end
    next_change()
  end
  local function get(filename, callback)
    captured.get(filename, guard(function(snapshot, failure)
      if failure then finish(failure) else callback(snapshot) end
    end))
  end
  local function query(arguments, callback)
    active_cancel = semantic.request(workspace, arguments, authorize, guard(function(reply, failure)
      active_cancel = nil
      if failure then finish(failure) else callback(reply) end
    end), nil, readiness)
  end
  local function symbol()
    require("forge.lsp.target").resolve(workspace, request.target, get, query, guard(function(target, failure)
      if not target then finish(failure) return end
      local filetype = vim.filetype.match({ filename = target.path }) or ""
      local buffer = source.loaded(target.path)
      local client, error = source.select_client(target.path, filetype, buffer, "textDocument/rename", workspace)
      if not client then finish(error) return end
      read(target.path, function(contents)
        if not buffer then
          local owned
          buffer, owned, error = source.open(target.path, contents, filetype)
          if not buffer then finish(error) return end
          source.retain(buffer, owned)
          hold.leases[#hold.leases + 1] = buffer
          local failed = touch(target.path)
          if failed then finish(failed) return end
        end
        local line = vim.api.nvim_buf_get_lines(buffer, target.line - 1, target.line, false)[1]
        local byte = vim.str_byteindex(line, "utf-32", target.column - 1, true)
        local params = { textDocument = { uri = vim.uri_from_fname(target.path) }, position = {
          line = target.line - 1, character = vim.str_utfindex(line, client.offset_encoding, byte, true) } }
        local function rename()
          params.newName = request.new_name
          send(client, "textDocument/rename", params, buffer, function(edit) apply_edit(edit, client, function() finish() end) end)
        end
        if client:supports_method("textDocument/prepareRename", buffer) then
          send(client, "textDocument/prepareRename", params, buffer, function(prepared)
            if not prepared or prepared.defaultBehavior == false then finish("not_renameable: " .. target.path) else rename() end
          end)
        else rename() end
      end)
    end))
  end
  local function file()
    local old, new = request.old_path, request.new_path
    local stat = vim.uv.fs_stat(old)
    if not stat then finish("path_not_found: " .. old) return end
    local failure = touch(old) or touch(new)
    if failure then finish(failure) return end
    local selected = {}
    for _, client in ipairs(vim.lsp.get_clients()) do
      if not client:is_stopped() and source.belongs(client, old) and matches(client, "workspace/willRenameFiles", old, stat.type == "directory") then
        selected[#selected + 1] = client
      end
    end
    table.sort(selected, function(a, b) return a.id < b.id end)
    if #selected == 0 then finish("unsupported: No matching language server supports file rename.") return end
    local params = { files = { { oldUri = vim.uri_from_fname(old), newUri = vim.uri_from_fname(new) } } }
    merging = true
    local index = 0
    local function next_client()
      index = index + 1
      local client = selected[index]
      if not client then
        local operation = { kind = "rename", old_path = old, new_path = new }
        hold.operations[#hold.operations + 1] = operation
        notify_for(operation)
        finish() return
      end
      send(client, "workspace/willRenameFiles", params, nil, function(edit)
        apply_edit(edit, client, next_client, true)
      end)
    end
    next_client()
  end
  local rename_path = request.kind == "symbol" and request.target.path or request.old_path
  local cancel = require("forge.lsp.startup").wait(workspace, rename_path, guard(function(failure)
    active_cancel = nil
    if failure then finish(failure) return end
    timer:start(10000, 0, vim.schedule_wrap(function() finish("timeout: Rename preparation exceeded 10 seconds.") end))
    if request.kind == "symbol" then symbol() else file() end
  end), { ready = readiness, method = request.kind == "symbol" and "textDocument/rename" or nil,
    accepts = request.kind == "file" and function(client)
      local stat = vim.uv.fs_stat(request.old_path)
      return stat ~= nil and matches(client, "workspace/willRenameFiles", request.old_path, stat.type == "directory")
    end or nil })
  if not finished then active_cancel = active_cancel or cancel end
  return function() finish("cancelled: Rename request cancelled.") end
end

return M
