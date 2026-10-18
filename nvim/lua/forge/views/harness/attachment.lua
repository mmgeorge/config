local M = {}

local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local namespace = vim.api.nvim_create_namespace("ForgeHarnessAttachmentImport")
---@type table<integer, ForgeAttachmentOwner>
local owner_by_buffer = {}
local paste_installed = false
---@type ForgeAttachmentOwner?
local paste_owner
local register_attachment = {}

---@class ForgeAttachmentSelection
---@field id string
---@field label string
---@field name string
---@field media_type string
---@field start integer
---@field end integer

---@class ForgePromptDraft
---@field text string
---@field attachments ForgeAttachmentSelection[]

---@class ForgeAttachmentImport
---@field first integer
---@field last integer
---@field source string
---@field path string
---@field number integer

---@class ForgeAttachmentOwner
---@field buffer integer
---@field session_id? string
---@field queue ForgeAttachmentImport[]
---@field head integer
---@field number integer
---@field active? ForgeAttachmentImport
---@field objects table[]
---@field undo table<integer, ForgeAttachmentSelection[]>
---@field undo_sequence? integer

---@param buffer integer
---@param mark integer
---@return integer[]?
local function position(buffer, mark)
  local point = vim.api.nvim_buf_get_extmark_by_id(buffer, namespace, mark, {})
  return #point == 2 and point or nil
end

---@param owner ForgeAttachmentOwner
---@param item ForgeAttachmentImport
---@return integer[]?, integer[]?
local function range(owner, item)
  if not vim.api.nvim_buf_is_valid(owner.buffer) then return end
  local first, last = position(owner.buffer, item.first), position(owner.buffer, item.last)
  if not first or not last then return end
  local line_count = vim.api.nvim_buf_line_count(owner.buffer)
  if first[1] >= line_count or last[1] >= line_count or first[1] > last[1]
    or (first[1] == last[1] and first[2] > last[2]) then return end
  local first_line = vim.api.nvim_buf_get_lines(owner.buffer, first[1], first[1] + 1, false)[1]
  local last_line = vim.api.nvim_buf_get_lines(owner.buffer, last[1], last[1] + 1, false)[1]
  if first[2] > #first_line or last[2] > #last_line then return end
  local source = table.concat(vim.api.nvim_buf_get_text(owner.buffer, first[1], first[2], last[1], last[2], {}), "\n")
  if source == item.source then return first, last end
end

---@param owner ForgeAttachmentOwner
---@param item ForgeAttachmentImport
local function release(owner, item)
  if vim.api.nvim_buf_is_valid(owner.buffer) then
    vim.api.nvim_buf_del_extmark(owner.buffer, namespace, item.first)
    vim.api.nvim_buf_del_extmark(owner.buffer, namespace, item.last)
  end
end

local function bind(owner, selection, row, column)
  local object = { selection = vim.deepcopy(selection),
    first = vim.api.nvim_buf_set_extmark(owner.buffer, namespace, row, column, { right_gravity = true,
      end_row = row, end_col = column + #selection.label, end_right_gravity = false, hl_group = "ForgeHarnessAttachmentLabel" }),
    last = vim.api.nvim_buf_set_extmark(owner.buffer, namespace, row, column + #selection.label, { right_gravity = false }) }
  owner.objects[#owner.objects + 1] = object
end

---@param buffer integer?
---@param metadata_only? boolean
---@return ForgePromptDraft
function M.capture(buffer, metadata_only)
  local draft = { text = "", attachments = {} }
  if not buffer or not vim.api.nvim_buf_is_valid(buffer) then return draft end
  if not metadata_only then draft.text = table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n") end
  local owner = owner_by_buffer[buffer]
  if not owner then return draft end
  local sequence = vim.api.nvim_buf_call(buffer, function() return vim.fn.undotree().seq_cur end)
  if owner.undo_sequence ~= sequence and owner.undo[sequence] then
    M.restore(buffer, owner.undo[sequence])
  end
  for _, object in ipairs(owner.objects) do
    local first, last = position(buffer, object.first), position(buffer, object.last)
    local line = first and vim.api.nvim_buf_get_lines(buffer, first[1], first[1] + 1, false)[1]
    if first and last and first[1] == last[1] and line
      and line:sub(first[2] + 1, last[2]) == object.selection.label then
      local selected = vim.deepcopy(object.selection)
      local offset = vim.api.nvim_buf_get_offset(buffer, first[1])
      selected.start, selected["end"] = offset + first[2], offset + last[2]
      draft.attachments[#draft.attachments + 1] = selected
    end
  end
  table.sort(draft.attachments, function(left, right) return left.start < right.start end)
  owner.undo_sequence, owner.undo[sequence] = sequence, vim.deepcopy(draft.attachments)
  return draft
end

---@param buffer integer
---@param attachments ForgeAttachmentSelection[]?
function M.restore(buffer, attachments)
  local owner = owner_by_buffer[buffer]
  if not owner then return end
  for _, object in ipairs(owner.objects) do release(owner, object) end
  owner.objects = {}
  local sequence = vim.api.nvim_buf_call(buffer, function() return vim.fn.undotree().seq_cur end)
  owner.undo_sequence, owner.undo[sequence] = sequence, vim.deepcopy(attachments or {})
  local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
  for _, selected in ipairs(attachments or {}) do
    local offset = selected.start
    for row, line in ipairs(lines) do
      if offset <= #line then
        if line:sub(offset + 1, offset + #selected.label) == selected.label then bind(owner, selected, row - 1, offset) end
        break
      end
      offset = offset - #line - 1
    end
  end
end

---@param owner ForgeAttachmentOwner
local function pump(owner)
  if owner.active or owner_by_buffer[owner.buffer] ~= owner then return end
  local item = owner.queue[owner.head]
  while item and not range(owner, item) do
    release(owner, item)
    owner.head = owner.head + 1
    item = owner.queue[owner.head]
  end
  if not item then owner.queue, owner.head = {}, 1 return end
  owner.active = item
  local function complete(result, failure, text_only)
    owner.active = nil
    owner.head = owner.head + 1
    local first, last = range(owner, item)
    if not text_only and owner_by_buffer[owner.buffer] == owner and first and last then
      if failure or not result or type(result.label) ~= "string" or type(result.id) ~= "string" then
        notifications.error(failure or "Attachment import returned no object", "Harness attachment")
      else
        vim.api.nvim_buf_set_text(owner.buffer, first[1], first[2], last[1], last[2], { result.label })
        bind(owner, result, first[1], first[2])
        M.capture(owner.buffer, true)
      end
    end
    release(owner, item)
    vim.schedule(function() pump(owner) end)
  end
  vim.uv.fs_stat(item.path, vim.schedule_wrap(function(failure, metadata)
    if owner_by_buffer[owner.buffer] ~= owner or not range(owner, item) then complete(nil, nil, true) return end
    if not metadata or metadata.type ~= "file" then
      if failure and not (failure:match("^ENOENT") or failure:match("^ENOTDIR")
        or failure:match("^ENAMETOOLONG") or failure:match("^EINVAL")) then
        complete(nil, failure)
      else complete(nil, nil, true) end
      return
    end
    owner.number = owner.number + 1
    item.number = owner.number
    client.request_for(owner.session_id, "attachment.import", { path = item.path, number = item.number }, complete)
  end))
end

---@param source string
---@return string?
local function absolute_path(source)
  local path = source
  if path:match("^file://") then
    local ok, decoded = pcall(vim.uri_to_fname, path)
    if not ok then return end
    path = decoded
  elseif path:sub(1, 1) == "/" then
    path = path:gsub("\\ ", " "):gsub("\\([()])", "%1")
  end
  if path:match("^[A-Za-z]:[/\\]") or path:match("^\\\\[^\\]") then return path end
  if path:sub(1, 1) == "/" then return path end
end

---@param line string
---@return { first: integer, last: integer, path: string }[]?
local function path_list(line)
  local first, last = line:find("%S"), line:match(".*%S()")
  if not first then return {} end
  local initial = line:sub(first, first)
  if initial ~= '"' and initial ~= "'" and not absolute_path(line:sub(first, last - 1)) then return end
  local result, offset = {}, first
  while offset < last do
    local quote = line:sub(offset, offset)
    local closing, path
    if quote == '"' or quote == "'" then
      closing = line:find(quote, offset + 1, true)
      if not closing then return end
      path = absolute_path(line:sub(offset + 1, closing - 1))
    else
      closing = last - 1
      local search = offset
      while true do
        local space_first, space_last = line:find("%s+", search)
        if not space_first then break end
        local following = line:sub(space_last + 1, space_last + 9)
        if following:match("^[A-Za-z]:[/\\]") or following:match("^file://")
          or following:match("^[/\"']") or following:match("^\\\\") then
          closing = space_first - 1
          break
        end
        search = space_last + 1
      end
      path = absolute_path(line:sub(offset, closing))
    end
    if not path then return end
    result[#result + 1] = { first = offset - 1, last = closing, path = path }
    offset = closing + 1
    local next_nonspace = line:find("%S", offset)
    if not next_nonspace then break end
    if next_nonspace == offset then return end
    offset = next_nonspace
  end
  return result
end

---@param owner ForgeAttachmentOwner
local function import_paste(owner)
  local first = vim.api.nvim_buf_get_mark(owner.buffer, "[")
  local last = vim.api.nvim_buf_get_mark(owner.buffer, "]")
  if first[1] == 0 or last[1] < first[1] then return end
  local final_line = vim.api.nvim_buf_get_lines(owner.buffer, last[1] - 1, last[1], false)[1] or ""
  local final_column = math.min(#final_line, last[2] + #vim.fn.strcharpart(final_line:sub(last[2] + 1), 0, 1))
  local lines = vim.api.nvim_buf_get_text(owner.buffer, first[1] - 1, first[2], last[1] - 1, final_column, {})
  local candidates = {}
  for index, line in ipairs(lines) do
    local paths = path_list(line)
    if not paths then return end
    for _, path in ipairs(paths) do
      local column = index == 1 and first[2] or 0
      candidates[#candidates + 1] = { row = first[1] + index - 2,
        first = path.first + column, last = path.last + column, path = path.path }
    end
  end
  if #candidates == 0 then return end
  for _, selected in ipairs(M.capture(owner.buffer).attachments) do
    owner.number = math.max(owner.number, tonumber(selected.label:match("#(%d+)%]$")) or 0)
  end
  for _, candidate in ipairs(candidates) do
    local text = vim.api.nvim_buf_get_text(owner.buffer, candidate.row, candidate.first, candidate.row, candidate.last, {})[1]
    local item = {
      first = vim.api.nvim_buf_set_extmark(owner.buffer, namespace, candidate.row, candidate.first, { right_gravity = true }),
      last = vim.api.nvim_buf_set_extmark(owner.buffer, namespace, candidate.row, candidate.last, { right_gravity = false }),
      source = text,
      path = candidate.path,
      number = 0,
    }
    owner.queue[#owner.queue + 1] = item
  end
  pump(owner)
end

local function after_register_paste(owner, draft)
  if draft then
    local first = vim.api.nvim_buf_get_mark(owner.buffer, "[")
    local lines = vim.api.nvim_buf_get_lines(owner.buffer, 0, -1, false)
    local base = first[2]
    for row = 1, first[1] - 1 do base = base + #lines[row] + 1 end
    local captured = M.capture(owner.buffer)
    for _, selected in ipairs(draft.attachments) do
      local copy = vim.deepcopy(selected)
      copy.start, copy["end"] = base + selected.start, base + selected["end"]
      captured.attachments[#captured.attachments + 1] = copy
    end
    M.restore(owner.buffer, captured.attachments)
  else
    import_paste(owner)
  end
end

local function install_paste()
  if paste_installed then return end
  paste_installed = true
  vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
      if owner_by_buffer[vim.api.nvim_get_current_buf()] then return end
      local register = vim.v.event.regname ~= "" and vim.v.event.regname or '"'
      register_attachment[register], register_attachment['"'] = nil, nil
    end,
  })
  local pending
  local function finish_register_paste()
    local captured = pending
    pending = nil
    if not captured then return end
    local owner = captured.owner
    if owner_by_buffer[owner.buffer] ~= owner or not vim.api.nvim_buf_is_valid(owner.buffer)
      or vim.api.nvim_get_current_buf() ~= owner.buffer
      or vim.api.nvim_buf_get_changedtick(owner.buffer) == captured.tick then return end
    local ok, failure = pcall(after_register_paste, owner, captured.draft)
    if not ok then notifications.error(tostring(failure), "Harness attachment") end
  end
  vim.on_key(function(key)
    finish_register_paste()
    local owner = owner_by_buffer[vim.api.nvim_get_current_buf()]
    if not owner or (key ~= "p" and key ~= "P") then return end
    local mode = vim.fn.mode(1)
    if mode ~= "n" and mode ~= "v" and mode ~= "V" and mode ~= "\22" then return end
    local register = vim.v.register
    local draft = register_attachment[register]
    if draft and vim.fn.getreg(register) ~= draft.text then draft = nil end
    pending = { owner = owner, draft = draft, tick = vim.api.nvim_buf_get_changedtick(owner.buffer) }
    vim.schedule(finish_register_paste)
  end, namespace)
  local previous = vim.paste
  vim.paste = function(lines, phase)
    if phase == -1 or phase == 1 then
      paste_owner = owner_by_buffer[vim.api.nvim_get_current_buf()]
      if vim.fn.getcmdtype() ~= "" then paste_owner = nil end
    end
    local accepted = previous(lines, phase)
    if phase == -1 or phase == 3 then
      local owner = paste_owner
      paste_owner = nil
      if accepted and owner and vim.api.nvim_get_current_buf() == owner.buffer then
        local ok, failure = pcall(import_paste, owner)
        if not ok then notifications.error(tostring(failure), "Harness attachment") end
      end
    end
    return accepted
  end
end

---@param buffer integer?
---@return boolean
function M.pending(buffer)
  local owner = owner_by_buffer[buffer]
  return owner ~= nil and (owner.active ~= nil or owner.queue[owner.head] ~= nil)
end

---@param buffer integer
function M.decorate(buffer)
  vim.api.nvim_set_hl(0, "ForgeHarnessAttachmentLabel", { link = "Special", default = true })
end

---@param buffer integer
---@param session_id? string
function M.attach(buffer, session_id)
  local owner = owner_by_buffer[buffer]
  if owner then owner.session_id = session_id return end
  owner = { buffer = buffer, session_id = session_id, queue = {}, head = 1, number = 0, objects = {}, undo = {} }
  owner_by_buffer[buffer] = owner
  install_paste()
  M.decorate(buffer)
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    buffer = buffer, callback = function() M.capture(buffer, true) end,
  })
  vim.api.nvim_create_autocmd("TextYankPost", {
    buffer = buffer,
    callback = function()
      local copied = table.concat(vim.v.event.regcontents, "\n")
      if vim.v.event.regtype == "V" then copied = copied .. "\n" end
      local selected
      do
        local source = M.capture(buffer)
        local first = vim.api.nvim_buf_get_mark(buffer, "[")
        local lines = vim.split(source.text, "\n", { plain = true })
        local offset = vim.v.event.regtype == "V" and 0 or first[2]
        for row = 1, first[1] - 1 do offset = offset + #(lines[row] or "") + 1 end
        selected = { text = copied, attachments = {} }
        for _, object in ipairs(source.attachments) do
          if object.start >= offset and object["end"] <= offset + #copied then
            local copy = vim.deepcopy(object)
            copy.start, copy["end"] = copy.start - offset, copy["end"] - offset
            selected.attachments[#selected.attachments + 1] = copy
          end
        end
      end
      local register = vim.v.event.regname ~= "" and vim.v.event.regname or '"'
      local saved = #selected.attachments > 0 and selected or nil
      register_attachment[register], register_attachment['"'] = saved, saved
    end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    group = vim.api.nvim_create_augroup("ForgeAttachmentOwner" .. buffer, { clear = true }),
    buffer = buffer,
    callback = function() owner_by_buffer[buffer] = nil end,
  })
end

return M
