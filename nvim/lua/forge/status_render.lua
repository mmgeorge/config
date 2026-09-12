local M = {}
local buffer = require("forge.buffer")
local Sequence = require("forge.block_sequence")
local folds = require("forge.folds")
local decorations = require("forge.decorations")
local empty_metadata = { target = {}, decoration = {}, editable_region = {} }
local empty_map = {}
local maximum = 9007199254740991
local section_order = { "unstaged", "staged", "conflicted", "ignored" }
local section_label = { unstaged = "Unstaged changes", staged = "Staged changes", conflicted = "Conflicted files", ignored = "Ignored changes" }
local change_label = { added = "New", deleted = "Deleted", renamed = "Renamed", copied = "Copied", conflicted = "Conflict", modified = "Modified" }
local change_capture = { added = "ForgeStatusFileNew", deleted = "ForgeStatusFileDeleted", renamed = "ForgeStatusFileRenamed",
  copied = "ForgeStatusFileNew", conflicted = "ForgeStatusFileDeleted", modified = "ForgeStatusFileModified" }

---@class ForgeStatusFileRecord
---@field id integer
---@field generation integer
---@field section string
---@field change string
---@field path string
---@field origin? string
---@field untracked boolean
---@field stats {state: string, added?: integer, deleted?: integer}

---@class ForgeStatusFileModel
---@field record ForgeStatusFileRecord
---@field header table
---@field body? ForgeBufferFragment
---@field recovering? boolean

---@class ForgeStatusInput
---@field document string
---@field revision integer
---@field view string
---@field sequence integer
---@field action string
---@field location table Semantic header identity or validated body-relative position.

---@class ForgeStatusReplica
---@field document string
---@field buffer integer
---@field revision? integer
---@field status string
---@field file table<integer, ForgeStatusFileModel>
---@field root ForgeBlockSequence
---@field sequence table
---@field block table<string, table>
---@field body_owner table<string, integer>
---@field inventory table
---@field presentation table
---@field width integer
---@field context_width? integer
---@field row_count integer
---@field fold table
---@field changedtick integer
---@field generation integer
---@field namespace integer
---@field marks table<string, integer[]>
---@field sentinel? integer
---@field notice fun(message: string)
---@field recover_body? fun(file: integer, generation: integer)
---@field applying? boolean
---@field fold_pending? table[]
---@field fold_pending_index? integer
---@field locate fun(row: integer, column: integer): table?
---@field capture fun(view: ForgeInputView, action: string): ForgeStatusInput?, string?
---@field header_text fun(row: integer): table?
---@field draw_header fun(row: integer, namespace: integer): boolean
---@field issues_editor? ForgeStatusIssuesEditor

local function counter(value)
  assert(type(value) == "number" and value >= 0 and value <= maximum and value == math.floor(value), "invalid status counter")
  return value
end

local function optional(value) return value ~= vim.NIL and value or nil end
local function file_key(id) return string.format("file:%.0f", id) end

local function elapsed(started) return math.floor((vim.uv.hrtime() - started) / 1000) end

local function trace(session, event, fields)
  fields.document = session.document
  require("forge.startup_log").write(event, fields)
end

local function entry(text, chunk, location, kind)
  return { text = text, chunk = chunk, location = location, kind = kind, row_count = #text,
    metadata = empty_metadata, gutter_row = empty_map, source_overlay_row = empty_map }
end

local function file_header(record, body)
  counter(record.id)
  counter(record.generation)
  assert(type(record.path) == "string" and not record.path:find("[\n\r%z]"), "invalid status display path")
  local label = assert(change_label[record.change], "unknown status change")
  assert(section_label[record.section] or record.section == "untracked", "unknown status section")
  local chunk = { { label, change_capture[record.change] }, { string.rep(" ", 9 - #label) .. record.path, "ForgeStatusPath" } }
  local stats = record.stats
  assert(type(stats) == "table", "missing status statistics")
  if stats.state == "exact" then
    counter(stats.added) counter(stats.deleted)
    chunk[#chunk + 1] = { " ", "ForgeStatusPath" }
    chunk[#chunk + 1] = { "+" .. stats.added, "ForgeAddRange" }
    chunk[#chunk + 1] = { " ", "ForgeStatusPath" }
    chunk[#chunk + 1] = { "-" .. stats.deleted, "ForgeDeleteRange" }
  else
    assert(stats.state == "unknown" or stats.state == "exceeds_limit", "unknown statistics state")
    if record.untracked then chunk[#chunk + 1] = { " new", "ForgeStatusPath" } end
  end
  local parts = {}
  for index = 1, #chunk do parts[index] = chunk[index][1] end
  local header = entry({ table.concat(parts) }, { chunk }, { kind = "file", id = record.id }, "file")
  header.file = record.id
  header.row_count = body and body.row_count > 0 and 1 + body.row_count or 2
  return header
end

local function wrap(text, width, indent)
  local result, source = { "" }, { { start = 0, prefix = 0 } }
  local indentation = string.rep(" ", math.min(indent, width - 1))
  local offset = 0
  local function continuation()
    assert(#result < 65536, "status context row budget exceeded")
    result[#result + 1] = indentation
    source[#source + 1] = { start = offset, prefix = #indentation }
  end
  for physical_index, physical in ipairs(vim.split(text, "\n", { plain = true })) do
    if physical_index > 1 then offset = offset + 1 continuation() end
    for part in physical:gmatch("[^%s]*%s?") do
      if #part > 0 then
        local prefix = #result == 1 and 0 or #indentation
        if #result[#result] > prefix and vim.fn.strdisplaywidth(result[#result] .. part) > width then continuation() end
        if vim.fn.strdisplaywidth(result[#result] .. part) <= width then
          result[#result] = result[#result] .. part
          offset = offset + #part
        else
          local character_index = 0
          while true do
            local character = vim.fn.strcharpart(part, character_index, 1, true)
            if character == "" then break end
            prefix = #result == 1 and 0 or #indentation
            if #result[#result] > prefix and vim.fn.strdisplaywidth(result[#result] .. character) > width then continuation() end
            result[#result] = result[#result] .. character
            offset = offset + #character
            character_index = character_index + 1
          end
        end
      end
    end
  end
  return result, source
end

local function context_entry(role, label, values, width)
  local prefix = label ~= "" and string.format("%-8s", label .. ":") or ""
  local chunk = { { prefix, "ForgeStatusLabel" } }
  vim.list_extend(chunk, values)
  local parts, spans, offset = {}, {}, 0
  for _, value in ipairs(chunk) do
    parts[#parts + 1] = value[1]
    spans[#spans + 1] = { start = offset, finish = offset + #value[1], capture = value[2] }
    offset = offset + #value[1]
  end
  local text = table.concat(parts)
  assert(#text <= 65536 and not text:find("%z"), "status context text exceeds capacity")
  local rows, source = wrap(text, role == "issues" and math.max(width, vim.fn.strdisplaywidth(text)) or width, #prefix)
  local chunks = {}
  for index, row in ipairs(rows) do
    local mapping = source[index]
    local line = {}
    if mapping.prefix > 0 then line[#line + 1] = { row:sub(1, mapping.prefix), "ForgeStatusPR" } end
    local finish = mapping.start + #row - mapping.prefix
    for _, span in ipairs(spans) do
      local first, last = math.max(span.start, mapping.start), math.min(span.finish, finish)
      if first < last then line[#line + 1] = { text:sub(first + 1, last), span.capture } end
    end
    chunks[index] = line
  end
  return entry(rows, chunks, { kind = "context", role = role }, "context")
end

local function commit_chunks(commit, role)
  return { { commit.oid:sub(1, 7), "ForgeStatusObjectId" }, { " ", "ForgeStatusPR" },
    { commit.reference, role == "head" and "ForgeStatusBranch" or "ForgeStatusRemote" }, { " " .. commit.subject, "ForgeStatusPR" } }
end

local function summary_chunks(summary)
  summary = summary or {}
  local text = summary.text and summary.text:match("[^\n]*") or ""
  local capture = "ForgeStatusPR"
  if text == "" then
    text = ({ fetching = "...fetching...", generating = "...generating...", error = "error", unavailable = "unavailable" })[summary.state] or "none"
    capture = summary.state == "error" and "ErrorMsg" or (summary.state == "fetching" or summary.state == "generating") and "ForgeStatusFetching" or "ForgeStatusObjectId"
  end
  if summary.state == "closed" then text = text .. " [closed]" end
  return { { text, capture } }
end

local function context_entries(session, context)
  local leading, recent = {}, {}
  local function append(collection, role, label, chunks)
    collection[#collection + 1] = { id = "status:context:" .. role,
      entry = context_entry(role, label, chunks, session.width) }
  end
  local head = optional(context.head)
  append(leading, "head", "Head", head and commit_chunks(head, "head")
    or { { (optional(context.branch) or "(detached)") .. " (no commits)", "ForgeStatusPR" } })
  for _, role in ipairs({ "upstream", "push" }) do
    local commit = optional(context[role])
    local remote = session.presentation.remote_action
    if remote and role == (remote.action == "push" and "push" or "upstream") then
      append(leading, role, role == "upstream" and "Merge" or "Push", { { remote.status, "ForgeStatusFetching" } })
    elseif commit then append(leading, role, role == "upstream" and "Merge" or "Push", commit_chunks(commit, role)) end
  end
  append(leading, "pr", "PR", summary_chunks(session.presentation.pr))
  append(leading, "about", "About", summary_chunks(session.presentation.about))
  local issues = {}
  for _, number in ipairs(context.issues) do issues[#issues + 1] = "#" .. number end
  append(leading, "issues", "Issues", { { #issues > 0 and table.concat(issues, " ") or "none",
    #issues > 0 and "ForgeStatusPR" or "ForgeStatusObjectId" } })
  if session.issues_editor and session.issues_editor.text then
    local text = session.issues_editor.text
    leading[#leading].entry = entry({ text }, { { { text, "ForgeStatusPR" } } }, { kind = "context", role = "issues" }, "context")
  end
  for _, commit in ipairs(context.recent) do
    append(recent, "recent:" .. commit.oid, "", { { commit.oid:sub(1, 7), "ForgeStatusObjectId" },
      { "  " .. commit.reference .. " " .. commit.subject, "ForgeStatusPR" } })
  end
  return leading, recent
end

local function facade(session)
  local sequence = {}
  sequence.node = setmetatable({}, { __index = function(_, id)
    local owner = session.body_owner[id]
    return owner and session.file[owner].body.sequence.node[id] or session.root.node[id]
  end })
  function sequence:locate(row)
    local node = session.root:locate(row)
    if not node or node.entry.kind ~= "file" then return node end
    local _, start = session.root:position(node.id)
    local body = session.file[node.entry.file].body
    if body and row > start and body.row_count > 0 then return body.sequence:locate(row - start - 1) end
    return node
  end
  function sequence:position(id)
    local owner = session.body_owner[id]
    if not owner then return session.root:position(id) end
    local index, start = session.root:position(file_key(owner))
    local _, relative = session.file[owner].body.sequence:position(id)
    return index, start + 1 + relative
  end
  function sequence:fold_level(row)
    local level, starts = session.root:fold_level(row)
    local node = session.root:locate(row)
    if node and node.entry.kind == "file" then
      local _, start = session.root:position(node.id)
      local body = session.file[node.entry.file].body
      if body and row > start then
        local body_level, body_start = body.sequence:fold_level(row - start - 1)
        return level + body_level, starts or body_start
      end
    end
    return level, starts
  end
  function sequence:rows() return session.root:rows() end
  return sequence
end

local function root_text(value, file)
  if value.kind ~= "file" then return value.text end
  local text = { value.text[1] }
  local body = file[value.file].body
  if not body or body.row_count == 0 then text[2] = "" return text end
  for index = 0, body.sequence:count() - 1 do vim.list_extend(text, body.sequence:at(index).entry.text) end
  return text
end

local function prepare(session, snapshot, checkpoint)
  assert(snapshot.document == session.document, "status document differs")
  counter(snapshot.revision)
  assert(not session.revision or snapshot.revision >= session.revision, "stale status snapshot")
  local entries, file, block, body_owner, record, changed = {}, {}, {}, {}, {}, {}
  local function push(id, value)
    assert(not block[id], "duplicate status node")
    block[id] = value
    entries[#entries + 1] = { id = id, entry = value, boundary = {} }
    return entries[#entries]
  end
  local function fold(id, owner, start, endpoint, finish, closed)
    record[id] = { owner = owner.id, fold = { id = id, start = { row = start, column = 0 },
      ["end"] = { block = endpoint.id, position = { row = finish, column = 0 } }, closed = closed } }
    owner.boundary["start:" .. id] = { row = start, delta = 1 }
    endpoint.boundary["end:" .. id] = { row = finish, delta = -1 }
    if not session.fold or not session.fold.record[id] then changed[id] = true end
  end
  local context = optional(snapshot.context)
  local leading, recent = {}, {}
  if context then
    leading, recent = context_entries(session, context)
    for _, value in ipairs(leading) do push(value.id, value.entry) end  else
    local view, head = snapshot.view, snapshot.head
    local text
    if view.kind == "comparison" then text = view.worktree and ("Diff:   " .. view.title .. " -> working tree") or ("Commit: " .. view.title)
    elseif head.state == "unborn" then text = head.reference .. " (unborn)"
    elseif head.state == "attached" then text = head.reference .. " " .. head.object
    else text = "Detached " .. head.object end
    push("repository", entry({ text }, { { { text, "Title" } } }, nil, "label"))
    if view.kind == "comparison" and optional(view.path) then
      push("comparison:path", entry({ "File:   " .. view.path }, { { { "File:   ", "Title" }, { view.path, "ForgeStatusBranch" } } }, nil, "label"))
    end
  end
  for index, value in ipairs(snapshot.file) do
    assert(not file[value.id], "duplicate status file")
    local previous = session.file[value.id]
    local body = previous and previous.record.generation == value.generation and previous.body or nil
    local header = previous and previous.record == value and previous.header or file_header(value, body)
    file[value.id] = { record = value, header = header, body = body }
    if checkpoint and index % 64 == 0 then checkpoint() end
  end
  local seen = {}
  for _, section in ipairs(snapshot.section) do
    assert(section_label[section.kind] and not seen[section.kind] and #section.file > 0, "invalid status section")
    seen[section.kind] = true
    local text = section_label[section.kind] .. " (" .. #section.file .. "):"
    if snapshot.view.kind == "comparison" then
      text = (snapshot.view.worktree and "Changes vs " or "Commit ") .. snapshot.view.title .. " (" .. #section.file .. "):"
    end
    local section_node = push("section:" .. section.kind, entry({ "", text }, { {}, { { text, "ForgeStatusHeader" } } },
      { kind = "section", section = section.kind }, "section"))
    local endpoint
    for _, id in ipairs(section.file) do
      local model = assert(file[id], "section refers to missing file")
      assert(model.record.section == section.kind or (section.kind == "unstaged" and model.record.section == "untracked"), "file belongs to another section")
      endpoint = push(file_key(id), model.header)
      if checkpoint and #entries % 64 == 0 then checkpoint() end
      fold(file_key(id), endpoint, 0, endpoint, model.header.row_count, true)
      if model.body then
        for body_id, value in pairs(model.body.block) do block[body_id], body_owner[body_id] = value, id end
        for fold_id, value in pairs(model.body.fold.record) do record[fold_id] = value end
      end
    end
    fold(section_node.id, section_node, 1, endpoint, endpoint.entry.row_count, false)
  end
  for id in pairs(file) do assert(block[file_key(id)], "status file has no section") end
  if snapshot.view.kind == "comparison" and #snapshot.file == 0 then
    push("comparison:empty", entry({ "", "No changes" }, { {}, { { "No changes", "ForgeStatusHeader" } } }, nil, "label"))
  end
  if context and #context.recent > 0 then
    local label = "Recent Commits (" .. #context.recent .. "):"
    local header = push("status:context:recent-title", entry({ "", label }, { {}, { { label, "ForgeStatusHeader" } } }, nil, "label"))
    local endpoint = header
    for _, value in ipairs(recent) do endpoint = push(value.id, value.entry) end
    fold("status:context:recent", header, 1, endpoint, endpoint.entry.row_count, true)
  end
  local built = vim.uv.hrtime()
  local root = Sequence.from(entries, checkpoint)
  return { root = root, entries = entries, file = file, block = block, body_owner = body_owner,
    fold = { record = record, changed = changed }, inventory = snapshot, index_us = elapsed(built) }
end

local function edits_between(session, prepared)
  if not session.revision or session.status == "Desynchronized" then
    local text = {}
    for _, value in ipairs(prepared.entries) do vim.list_extend(text, root_text(value.entry, prepared.file)) end
    return { { start_row = 0, removed_rows = vim.api.nvim_buf_line_count(session.buffer), text = text } }
  end
  local previous, current = {}, {}
  for index = 0, session.root:count() - 1 do previous[#previous + 1] = session.root:at(index).id end
  for _, value in ipairs(prepared.entries) do current[#current + 1] = value.id end
  local changes = vim.diff(table.concat(previous, "\n") .. "\n", table.concat(current, "\n") .. "\n", { result_type = "indices", algorithm = "histogram" })
  local edits, replaced = {}, {}
  for _, change in ipairs(changes) do
    local first = change[2] == 0 and change[1] or change[1] - 1
    local next_first = change[4] == 0 and change[3] or change[3] - 1
    local start = first < #previous and select(2, session.root:position(previous[first + 1])) or session.row_count
    local finish = first + change[2] < #previous and select(2, session.root:position(previous[first + change[2] + 1])) or session.row_count
    local text = {}
    for index = next_first + 1, next_first + change[4] do
      local id = current[index]
      replaced[id] = true
      vim.list_extend(text, root_text(prepared.block[id], prepared.file))
    end
    edits[#edits + 1] = { start_row = start, removed_rows = finish - start, text = text }
  end
  for _, value in ipairs(prepared.entries) do
    local id, next_entry = value.id, value.entry
    local previous_entry = session.block[id]
    if previous_entry and not replaced[id] then
      local _, start = session.root:position(id)
      local same_body = next_entry.kind == "file" and session.file[next_entry.file].body == prepared.file[next_entry.file].body
      local old_text = same_body and previous_entry.text or root_text(previous_entry, session.file)
      local new_text = same_body and next_entry.text or root_text(next_entry, prepared.file)
      if not vim.deep_equal(old_text, new_text) then
        for _, change in ipairs(vim.diff(table.concat(old_text, "\n") .. "\n", table.concat(new_text, "\n") .. "\n", { result_type = "indices", algorithm = "histogram" })) do
          local offset = change[2] == 0 and change[1] or change[1] - 1
          local next_offset = change[4] == 0 and change[3] or change[3] - 1
          local text = {}
          for index = next_offset + 1, next_offset + change[4] do text[#text + 1] = new_text[index] end
          edits[#edits + 1] = { start_row = start + offset, removed_rows = change[2], text = text }
        end
      end
    end
  end
  table.sort(edits, function(left, right) return left.start_row > right.start_row end)
  return edits
end

local function write(session, edits)
  local started = vim.uv.hrtime()
  session.applying, session.fold_pending = true, edits
  vim.bo[session.buffer].modifiable = true
  local ok, failure = pcall(function()
    for index, edit in ipairs(edits) do
      session.fold_pending_index = index + 1
      vim.api.nvim_buf_set_lines(session.buffer, edit.start_row, edit.start_row + edit.removed_rows, true, edit.text)
    end
  end)
  vim.bo[session.buffer].modifiable = false
  session.applying, session.fold_pending, session.fold_pending_index = nil, nil, nil
  session.changedtick = vim.api.nvim_buf_get_changedtick(session.buffer)
  assert(ok, failure)
  if session.issues_editor then session.issues_editor.sync() end
  return elapsed(started)
end

local function adopt(session, prepared)
  local recovering = session.status == "Desynchronized"
  assert(session.status == "Desynchronized" or vim.api.nvim_buf_get_changedtick(session.buffer) == session.changedtick, "status buffer was changed externally")
  local edits = edits_between(session, prepared)
  local fold_state = #edits > 0 and folds.capture(session) or {}
  local metadata = { changed = {}, retired = {}, block = {}, position = {} }
  for id in pairs(prepared.body_owner) do
    local previous = session.block[id]
    local reinstall = recovering or previous ~= prepared.block[id]
    if previous and not reinstall then
      local _, start = session.sequence:position(id)
      local finish = start + #previous.text
      for _, edit in ipairs(edits) do
        if edit.start_row <= finish and edit.start_row + edit.removed_rows >= start then
          reinstall = true
          break
        end
      end
    end
    if reinstall then
      metadata.changed[id], metadata.block[id] = true, prepared.block[id]
    end
  end
  if recovering then
    vim.api.nvim_buf_clear_namespace(session.buffer, session.namespace, 0, -1)
    session.sentinel, session.marks = nil, {}
  end
  for id, mark in pairs(session.marks) do
    if not prepared.block[id] then
      for _, handle in ipairs(mark) do vim.api.nvim_buf_del_extmark(session.buffer, session.namespace, handle) end
      session.marks[id] = nil
    end
  end
  session.root, session.file, session.block, session.body_owner = prepared.root, prepared.file, prepared.block, prepared.body_owner
  session.fold, session.inventory = prepared.fold, prepared.inventory
  session.context_width = session.width
  session.revision, session.row_count = prepared.inventory.revision, prepared.root:rows()
  local buffer_us = write(session, edits)
  session.status = "Applied"
  for id in pairs(metadata.changed) do
    metadata.position[id] = select(2, session.sequence:position(id))
  end
  buffer.install_fragment_metadata(session, metadata)
  if not session.sentinel then session.sentinel = vim.api.nvim_buf_set_extmark(session.buffer, session.namespace, 0, 0, {}) end
  decorations.attach(session)
  folds.register(session)
  folds.refresh(session)
  folds.restore(session, fold_state)
  return buffer_us, edits
end

---@param document string
---@param options table
---@return ForgeStatusReplica
function M.open(document, options)
  local session = buffer.open(document, options)
  session.file, session.body_owner, session.root = {}, {}, Sequence.new()
  session.presentation, session.width, session.generation = { pr = { state = "fetching" }, about = { state = "none" } }, 80, 0
  session.sequence = facade(session)
  session.locate = function(row, column) return M.locate(session, row, column) end
  session.capture = function(view, action) return M.capture(session, view, action) end
  session.header_text = function(row)
    local node = session.root:locate(row)
    if not node then return nil end
    local _, start = session.root:position(node.id)
    return session.block[node.id].chunk[row - start + 1]
  end
  session.draw_header = function(row, namespace)
    local chunks = session.header_text(row)
    if not chunks then return false end
    local column = 0
    for _, chunk in ipairs(chunks) do
      if #chunk[1] > 0 then
        vim.api.nvim_buf_set_extmark(session.buffer, namespace, row, column, { end_col = column + #chunk[1],
          hl_group = chunk[2], priority = 110, ephemeral = true })
        column = column + #chunk[1]
      end
    end
    return true
  end
  return session
end

---@param session ForgeStatusReplica
---@param snapshot table
---@param callback? fun(result: table)
---@return table?
function M.apply_snapshot(session, snapshot, callback)
  session.generation = session.generation + 1
  local generation, started = session.generation, vim.uv.hrtime()
  local budget = started
  local timing = { files = #snapshot.file, callback_count = 0, longest_callback_us = 0, work_us = 0 }
  local thread = coroutine.create(function()
    local prepared = prepare(session, snapshot, callback and function()
      if elapsed(budget) >= 4000 then coroutine.yield() budget = vim.uv.hrtime() end
    end)
    if session.status == "Closed" or session.generation ~= generation then return { kind = "Closed" } end
    local formatting_us = elapsed(started) - prepared.index_us
    local buffer_us = adopt(session, prepared)
    timing.rows, timing.formatting_us = session.row_count, formatting_us
    timing.index_us, timing.buffer_us = prepared.index_us, buffer_us
    return { kind = "Applied", revision = session.revision }
  end)
  local function resume()
    if session.status == "Closed" or session.generation ~= generation then return { kind = "Closed" } end
    local callback_started = vim.uv.hrtime()
    local ok, result = coroutine.resume(thread)
    local callback_us = elapsed(callback_started)
    timing.callback_count = timing.callback_count + 1
    timing.longest_callback_us = math.max(timing.longest_callback_us, callback_us)
    timing.work_us = timing.work_us + callback_us
    if not ok then result = buffer.fail_apply(session, result) end
    if ok and coroutine.status(thread) ~= "dead" then vim.schedule(resume) return end
    if ok and result.kind == "Applied" then
      timing.elapsed_us = elapsed(started)
      trace(session, "status.semantic.applied", timing)
    end
    if callback then callback(result) end
    return result
  end
  return resume()
end

---@param session ForgeStatusReplica
---@param delta table
---@return table
local function delta_snapshot(session, delta)
    assert(delta.document == session.document and delta.base == session.revision and delta.next == delta.base + 1, "status delta revision differs")
    local snapshot = vim.tbl_extend("force", session.inventory, { revision = counter(delta.next) })
    local file = {}
    for id, value in pairs(session.file) do file[id] = value.record end
    for _, id in ipairs(delta.removed) do file[id] = nil end
    for _, value in ipairs(delta.file) do file[value.id] = value end
    local section = {}
    for _, value in ipairs(snapshot.section) do section[value.kind] = value end
    for _, value in ipairs(delta.section) do section[value.kind] = value end
    snapshot.file, snapshot.section = {}, {}
    for _, kind in ipairs(section_order) do
      local value = section[kind]
      if value and #value.file > 0 then
        snapshot.section[#snapshot.section + 1] = value
        for _, id in ipairs(value.file) do snapshot.file[#snapshot.file + 1] = assert(file[id], "delta section has missing file") end
      end
    end
    snapshot.context = optional(delta.context) or snapshot.context
    snapshot.head = optional(delta.head) or snapshot.head
    snapshot.pending = delta.pending or {}
    return snapshot
end

---@param session ForgeStatusReplica
---@param delta table
---@return table
function M.apply_patch(session, delta)
  local ok, result = pcall(function()
    local snapshot = delta_snapshot(session, delta)
    adopt(session, prepare(session, snapshot))
    return { kind = "Applied", revision = session.revision }
  end)
  if not ok then return buffer.fail_apply(session, result) end
  return result
end

---@param session ForgeStatusReplica
---@param update table
---@return table
function M.apply_update(session, update)
  if session.status == "Closed" or update.document ~= session.document then return { kind = "Discarded" } end
  if update.delta.next <= session.revision then return { kind = "Discarded" } end
  local previous_file = session.file
  local candidate_file
  local ok, result = pcall(function()
    local snapshot = delta_snapshot(session, update.delta)
    local file = vim.tbl_extend("force", {}, previous_file)
    candidate_file = file
    local record = {}
    for _, value in ipairs(snapshot.file) do record[value.id] = value end
    for _, delivery in ipairs(update.body) do
      local value = assert(record[delivery.file], "status update body has no file")
      assert(delivery.generation == value.generation, "status update body generation differs")
      assert(delivery.snapshot and delivery.snapshot.document == string.format("body:%.0f:%.0f", delivery.file, delivery.generation), "status update body identity differs")
      file[delivery.file] = { record = value, body = buffer.fragment(delivery.snapshot) }
    end
    session.file = file
    local prepared = prepare(session, snapshot)
    session.file = previous_file
    local buffer_us, edits = adopt(session, prepared)
    trace(session, "status.optimistic.applied", { operation = update.operation_id, phase = update.phase, edits = #edits, buffer_us = buffer_us,
      native_us = update.elapsed_us, files = #update.delta.file, bodies = #update.body, pending = #update.delta.pending })
    return { kind = "Applied", revision = session.revision }
  end)
  if not ok then
    if session.file == candidate_file then session.file = previous_file end
    return buffer.fail_apply(session, result)
  end
  return result
end

---@param session ForgeStatusReplica
---@param delivery table
---@return table
function M.apply_body(session, delivery)
  if delivery.document ~= session.document or session.status ~= "Applied" then return { kind = "Discarded" } end
  local model = session.file[delivery.file]
  if not model or model.record.generation ~= delivery.generation then return { kind = "Discarded" } end
  if vim.api.nvim_buf_get_changedtick(session.buffer) ~= session.changedtick then
    return buffer.fail_apply(session, "status buffer was changed externally")
  end
  local started, previous = vim.uv.hrtime(), model.body
  local fold_state = folds.capture(session)
  local previous_fold = vim.tbl_extend("force", {}, previous and previous.fold.record or {})
  local ok, result = pcall(function()
    assert(vim.api.nvim_buf_get_changedtick(session.buffer) == session.changedtick, "status buffer was changed externally")
    local key = file_key(delivery.file)
    local _, start = session.root:position(key)
    local prepared, body, edits
    if optional(delivery.snapshot) then
      assert(delivery.snapshot.document == string.format("body:%.0f:%.0f", delivery.file, delivery.generation), "status body identity differs")
      assert(not previous or delivery.snapshot.revision >= previous.revision, "stale status body snapshot")
      body = buffer.fragment(delivery.snapshot)
      prepared = { block = body.block, changed = {}, retired = {}, position = {} }
      for id in pairs(body.block) do prepared.changed[id] = true end
      for id in pairs(previous and previous.block or {}) do if not body.block[id] then prepared.retired[id] = true end end
      local text = {}
      for index = 0, body.sequence:count() - 1 do vim.list_extend(text, body.block[body.sequence:at(index).id].text) end
      if #text == 0 then text[1] = "" end
      edits = { { start_row = start + 1, removed_rows = previous and math.max(1, previous.row_count) or 1, text = text } }
    elseif optional(delivery.patch) then
      body = assert(previous, "status body requires a snapshot")
      prepared = buffer.patch_fragment(body, delivery.patch, function(row)
        return assert(vim.api.nvim_buf_get_lines(session.buffer, start + 1 + row, start + 2 + row, true)[1], "body source row disappeared")
      end)
      edits = {}
      for _, edit in ipairs(delivery.patch.text_edit) do
        edits[#edits + 1] = { start_row = start + 1 + edit.start_row, removed_rows = edit.removed_rows, text = edit.text }
      end
    else return { kind = "Applied" } end
    model.body = body
    model.header = file_header(model.record, body)
    session.block[key] = model.header
    session.root:update(key, model.header)
    session.root:fold_boundary(key, "end:" .. key, model.header.row_count, -1)
    session.fold.record[key].fold["end"].position.row = model.header.row_count
    local section = model.record.section == "untracked" and "unstaged" or model.record.section
    local section_key = "section:" .. section
    local section_fold = session.fold.record[section_key]
    if section_fold.fold["end"].block == key then
      section_fold.fold["end"].position.row = model.header.row_count
      session.root:fold_boundary(key, "end:" .. section_key, model.header.row_count, -1)
    end
    session.fold.changed = { [key] = true, [section_key] = true }
    for id in pairs(prepared.retired) do session.block[id], session.body_owner[id] = nil, nil end
    for id, value in pairs(prepared.block) do
      session.block[id], session.body_owner[id] = value, delivery.file
      prepared.position[id] = start + 1 + select(2, body.sequence:position(id))
    end
    for id in pairs(previous_fold) do
      if not body.fold.record[id] then session.fold.record[id] = nil end
    end
    for id, value in pairs(body.fold.record) do
      if not session.fold.record[id] or (body.fold.changed and body.fold.changed[id]) then session.fold.changed[id] = true end
      session.fold.record[id] = value
    end
    session.row_count = session.root:rows()
    local buffer_us = write(session, edits)
    buffer.install_fragment_metadata(session, prepared)
    folds.refresh(session)
    folds.restore(session, fold_state)
    trace(session, "status.body.applied", { file = delivery.file, generation = delivery.generation,
      rows = body.row_count, edits = #edits, buffer_us = buffer_us, elapsed_us = elapsed(started) })
    return { kind = "Applied" }
  end)
  if not ok then
    session.notice(tostring(result))
    if session.recover_body and not model.recovering then
      model.recovering = true
      session.recover_body(delivery.file, delivery.generation)
    end
    return { kind = "Desynchronized" }
  end
  return result
end

---@param session ForgeStatusReplica
---@param row integer
---@param column integer
---@return table?
function M.locate(session, row, column)
  local node = session.sequence:locate(row)
  if not node then return nil end
  local _, start = session.sequence:position(node.id)
  local relative = row - start
  local semantic = node.entry.location
  local owner = session.body_owner[node.id]
  local target
  if owner then
    for _, value in ipairs(node.entry.metadata.target) do
      local first, last = value.range.start, value.range["end"]
      if (relative > first.row or relative == first.row and column >= first.column)
        and (relative < last.row or relative == last.row and column < last.column) then target = value.id break end
    end
    local model = session.file[owner]
    semantic = { kind = "body", file = owner, generation = model.record.generation, revision = model.body.revision,
      block = node.id, position = { row = relative, column = column }, target = target }
  elseif node.entry.kind == "file" and relative > 0 or node.entry.kind == "section" and relative == 0 then
    semantic = nil
  elseif semantic then
    target = semantic.kind == "file" and file_key(semantic.id) or semantic.kind == "context" and ("status:context:" .. semantic.role)
      or "section:" .. semantic.section
  end
  return { block = node.id, position = { row = relative, column = column }, target = target, location = semantic }
end

---@param session ForgeStatusReplica
---@param view ForgeInputView
---@param action string
---@param selection? {target: table[]}
---@return ForgeStatusInput?
---@return string?
function M.capture(session, view, action, selection)
  if session.status ~= "Applied" or session.applying or not view.active or view.document ~= session.document
    or not vim.api.nvim_win_is_valid(view.window) or vim.api.nvim_win_get_buf(view.window) ~= session.buffer then return nil, "input view is no longer current" end
  if vim.api.nvim_buf_get_changedtick(session.buffer) ~= session.changedtick then return nil, "status buffer was changed externally" end
  local cursor = vim.api.nvim_win_get_cursor(view.window)
  local located = M.locate(session, cursor[1] - 1, cursor[2])
  local location = selection and selection.target[1] or located and located.location
  if not location and located and action == "navigate" then
    location = session.block[located.block].location
      or { kind = "boundary", after_files = located.block == "status:context:recent-title" }
  end
  if not location then return nil, "status row has no target" end
  assert(view.sequence < maximum, "input sequence exhausted")
  view.sequence, view.cursor, view.effect = view.sequence + 1, cursor, {}
  return { document = session.document, revision = session.revision, view = view.id, sequence = view.sequence,
    action = action, location = location }
end

---@param session ForgeStatusReplica
---@param first integer
---@param last integer
---@return table
function M.selection(session, first, last)
  local candidates, hunk_file, raw_file = {}, {}, {}
  for row = first, last do
    local located = M.locate(session, row, 0)
    if located and located.target and located.location then
      local location = located.location
      local endpoint = row == first or row == last
      candidates[#candidates + 1] = { location = location, target = located.target, endpoint = endpoint }
      if location.kind == "body" then
        hunk_file[location.file] = true
        if located.target:match("^hunk:") then raw_file[location.file] = true end
      end
    end
  end
  local selected, seen = {}, {}
  for _, candidate in ipairs(candidates) do
    local location = candidate.location
    local skip = not candidate.endpoint and (location.kind == "section"
      or location.kind == "file" and hunk_file[location.id]
      or location.kind == "body" and candidate.target:match("^group:") and raw_file[location.file])
    if not skip and location.kind ~= "context" and not seen[candidate.target] then
      selected[#selected + 1], seen[candidate.target] = location, true
    end
  end
  assert(#selected > 0 and #selected <= 65536, "invalid status selection size")
  return { target = selected }
end

---@param session ForgeStatusReplica
---@param presentation table
function M.present_context(session, presentation)
  local previous_presentation = session.presentation
  session.presentation = presentation
  local context = session.inventory and optional(session.inventory.context)
  if not context or session.status ~= "Applied" then return end
  local started = vim.uv.hrtime()
  local ok, failure = pcall(function()
    assert(vim.api.nvim_buf_get_changedtick(session.buffer) == session.changedtick, "status buffer was changed externally")
    local remote_changed = not vim.deep_equal(presentation.remote_action, previous_presentation.remote_action)
    if remote_changed then
      for _, role in ipairs({ "upstream", "push" }) do
        local remote = presentation.remote_action
        local visible = optional(context[role]) ~= nil
          or (remote ~= nil and role == (remote.action == "push" and "push" or "upstream"))
        if (session.block["status:context:" .. role] ~= nil) ~= visible then
          adopt(session, prepare(session, session.inventory))
          return
        end
      end
    end
    local replacement, edits = {}, {}
    local leading, recent = {}, {}
    local resized = session.context_width ~= session.width
    if resized or remote_changed then
      leading, recent = context_entries(session, context)
      vim.list_extend(leading, recent)
    else
      for _, role in ipairs({ "pr", "about" }) do
        local chunks = summary_chunks(presentation[role])
        if not vim.deep_equal(chunks, summary_chunks(previous_presentation[role])) then
          leading[#leading + 1] = { id = "status:context:" .. role,
            entry = context_entry(role, role == "pr" and "PR" or "About", chunks, session.width) }
        end
      end
    end
    if #leading == 0 then return end
    for _, candidate in ipairs(leading) do
      local key, value = candidate.id, candidate.entry
      local previous = assert(session.block[key], "missing status context row")
      replacement[key] = value
      if not vim.deep_equal(previous.text, value.text) then
        local _, start = session.root:position(key)
        edits[#edits + 1] = { start_row = start, removed_rows = previous.row_count, text = value.text }
      end
    end
    for key, value in pairs(replacement) do
      session.block[key] = value
      session.root:update(key, value)
    end
    session.fold.changed = {}
    local recent = session.fold.record["status:context:recent"]
    if recent and resized then
      local endpoint = recent.fold["end"]
      endpoint.position.row = session.block[endpoint.block].row_count
      session.root:fold_boundary(endpoint.block, "end:status:context:recent", endpoint.position.row, -1)
      session.fold.changed["status:context:recent"] = true
    end
    session.row_count = session.root:rows()
    session.context_width = session.width
    table.sort(edits, function(left, right) return left.start_row > right.start_row end)
    local buffer_us = write(session, edits)
    folds.refresh(session)
    trace(session, "status.context.applied", { edits = #edits, buffer_us = buffer_us, elapsed_us = elapsed(started) })
  end)
  if not ok then buffer.fail_apply(session, failure) end
end

---@param session ForgeStatusReplica
---@param window integer
function M.resize(session, window)
  local width = math.max(1, math.min(4096, vim.api.nvim_win_get_width(window)))
  if session.width == width then return end
  session.width = width
  if session.inventory then M.present_context(session, session.presentation) end
end

---@param session ForgeStatusReplica
---@param view ForgeInputView
---@param effect table
function M.apply_effect(session, view, effect)
  local location = effect.location
  if location and effect.kind == "cursor" then
    if location.kind == "body" then
      local model = session.file[location.file]
      if not model or model.record.generation ~= location.generation or not model.body or model.body.revision ~= location.revision then return "Discarded" end
      effect = vim.tbl_extend("force", effect, { block = location.block, position = location.position })
    elseif location.kind == "file" then
      effect = vim.tbl_extend("force", effect, { block = file_key(location.id), position = { row = 0, column = 0 } })
    else return "Discarded" end
  end
  return require("forge.effects").apply(session, view, effect)
end

M.close = buffer.close
return M
