--- Computes walkthrough inventory exclusively from Sem's tracked diff and untracked entities.
--- Owns Sem JSON normalization while callers inject the async process boundary.

local M = {}

---@class DiffReviewInventoryCounts
---@field added integer
---@field removed integer
---@field modified integer

---@class DiffReviewInventoryChange
---@field name string
---@field relpath string
---@field line? integer

---@class DiffReviewInventoryChangeSet
---@field added DiffReviewInventoryChange[]
---@field removed DiffReviewInventoryChange[]
---@field modified DiffReviewInventoryChange[]

---@class DiffReviewInventoryRow
---@field label string
---@field added integer
---@field removed integer
---@field modified integer

---@class DiffReviewInventoryResult
---@field rows DiffReviewInventoryRow[]
---@field details table<string, DiffReviewInventoryChangeSet>?
---@field error? string

---@class DiffReviewSemResult
---@field code integer
---@field stdout string
---@field stderr string
---@field output string

---@class DiffReviewInventoryComputeOpts
---@field cwd string
---@field sections table[]?
---@field run_text_async fun(command: string[], input: string?, cb: fun(result: DiffReviewSemResult))
---@field sem_available? fun(): boolean
---@field repo_relative? fun(filename: string): string?

local row_order = {
  "function",
  "struct",
  "class",
  "interface",
  "enum",
  "trait",
  "type",
  "module",
  "docs",
  "plans",
  "files",
}

local row_rank = {}
for index, label in ipairs(row_order) do
  row_rank[label] = index
end

local counted_entity_types = {
  ["function"] = true,
  struct = true,
  class = true,
  interface = true,
  enum = true,
  trait = true,
  type = true,
  module = true,
}

local action_by_change_type = {
  added = "added",
  deleted = "removed",
  modified = "modified",
  moved = "modified",
  renamed = "modified",
  reordered = "modified",
}

---@param path string?
---@return string
local function normalize_relpath(path)
  return (tostring(path or ""):gsub("\\", "/"):gsub("^%./", ""))
end

---@param relpath string
---@return string?
local function artifact_bucket(relpath)
  for segment in normalize_relpath(relpath):lower():gmatch("[^/]+") do
    if segment == "docs" then return "docs" end
    if segment == "plans" then return "plans" end
  end
  return nil
end

---@param counts table<string, DiffReviewInventoryCounts>
---@param label string
---@param action "added"|"removed"|"modified"
local function increment(counts, label, action)
  local count = counts[label]
  if not count then
    count = { added = 0, removed = 0, modified = 0 }
    counts[label] = count
  end
  count[action] = count[action] + 1
end

---@param details table<string, DiffReviewInventoryChangeSet>
---@param label string
---@param action "added"|"removed"|"modified"
---@param change DiffReviewInventoryChange
local function add_detail(details, label, action, change)
  local change_set = details[label]
  if not change_set then
    change_set = { added = {}, removed = {}, modified = {} }
    details[label] = change_set
  end
  change_set[action][#change_set[action] + 1] = change
end

---@param status string?
---@param untracked boolean?
---@return "added"|"removed"|"modified"
local function artifact_action(status, untracked)
  status = tostring(status or "")
  if untracked or status == "??" or status:find("A", 1, true) then return "added" end
  if status:find("D", 1, true) then return "removed" end
  return "modified"
end

---@param opts DiffReviewInventoryComputeOpts
---@param filename string?
---@param fallback string?
---@return string
local function repo_relpath(opts, filename, fallback)
  if filename and opts.repo_relative then
    local relpath = opts.repo_relative(filename)
    if relpath and relpath ~= "" then return normalize_relpath(relpath) end
  end
  return normalize_relpath(fallback or filename)
end

---@param opts DiffReviewInventoryComputeOpts
---@return table<string, { action: "added"|"removed"|"modified", filename: string }>
---@return string[] untracked_files
local function status_file_index(opts)
  local files = {}
  local untracked_files = {}
  local seen_untracked = {}
  for _, section in ipairs(opts.sections or {}) do
    for _, file in ipairs(section.files or {}) do
      local relpath = repo_relpath(opts, file.filename, file.relpath)
      if relpath ~= "" then
        local untracked = file.untracked == true or file.git_status == "??"
        files[relpath] = {
          action = artifact_action(file.git_status, untracked),
          filename = tostring(file.filename or relpath),
        }
        if untracked and not seen_untracked[relpath] then
          seen_untracked[relpath] = true
          untracked_files[#untracked_files + 1] = tostring(file.filename or relpath)
        end
      end
    end
  end
  table.sort(untracked_files)
  return files, untracked_files
end

---@param text string
---@param label string
---@return any? decoded
---@return string? err
local function decode_json(text, label)
  local ok, decoded = pcall(vim.json.decode, text, { luanil = { object = true, array = true } })
  if not ok then return nil, ("Sem %s returned invalid JSON: %s"):format(label, tostring(decoded)) end
  return decoded, nil
end

---@param result DiffReviewSemResult
---@param label string
---@return any? decoded
---@return string? err
local function decode_result(result, label)
  if result.code ~= 0 then
    local message = vim.trim(result.output or "")
    return nil, message ~= "" and ("Sem %s failed: %s"):format(label, message)
      or ("Sem %s exited with code %d"):format(label, result.code)
  end
  return decode_json(result.stdout or "", label)
end

---@param counts table<string, DiffReviewInventoryCounts>
---@param details table<string, DiffReviewInventoryChangeSet>
---@param touched_files table<string, boolean>
---@param seen_entities table<string, boolean>
---@param opts DiffReviewInventoryComputeOpts
---@param entity table
---@param default_action? "added"|"removed"|"modified"
local function add_sem_entity(counts, details, touched_files, seen_entities, opts, entity, default_action)
  local filename = entity.filePath or entity.file
  local relpath = repo_relpath(opts, filename, filename)
  if relpath == "" then return end
  touched_files[relpath] = true

  local entity_type = tostring(entity.entityType or entity.type or ""):lower()
  if not counted_entity_types[entity_type] then return end
  local action = default_action or action_by_change_type[tostring(entity.changeType or ""):lower()]
  if not action then return end
  local entity_name = tostring(entity.entityName or entity.name or "")
  if entity_name == "" then return end
  local identity = table.concat({ action, entity_type, relpath, tostring(entity.entityId or entity.parent_id or ""), entity_name }, "\t")
  if seen_entities[identity] then return end
  seen_entities[identity] = true

  increment(counts, entity_type, action)
  add_detail(details, entity_type, action, {
    name = entity_name,
    relpath = relpath,
    line = tonumber(entity.startLine or entity.start_line or entity.oldStartLine or entity.old_start_line),
  })
end

---@param counts table<string, DiffReviewInventoryCounts>
---@param details table<string, DiffReviewInventoryChangeSet>
---@param touched_files table<string, boolean>
---@param status_files table<string, { action: "added"|"removed"|"modified", filename: string }>
local function add_artifact_counts(counts, details, touched_files, status_files)
  for relpath in pairs(touched_files) do
    local action = status_files[relpath] and status_files[relpath].action or "modified"
    increment(counts, "files", action)
    add_detail(details, "files", action, { name = relpath, relpath = relpath })
    local bucket = artifact_bucket(relpath)
    if bucket then
      increment(counts, bucket, action)
      add_detail(details, bucket, action, { name = relpath, relpath = relpath })
    end
  end
end

---@param counts table<string, DiffReviewInventoryCounts>
---@return DiffReviewInventoryRow[]
local function rows_from_counts(counts)
  local rows = {}
  for label, count in pairs(counts) do
    rows[#rows + 1] = {
      label = label,
      added = count.added,
      removed = count.removed,
      modified = count.modified,
    }
  end
  table.sort(rows, function(left, right)
    local left_rank = row_rank[left.label] or 999
    local right_rank = row_rank[right.label] or 999
    if left_rank ~= right_rank then return left_rank < right_rank end
    return left.label < right.label
  end)
  return rows
end

---@param opts DiffReviewInventoryComputeOpts
---@param cb fun(result: DiffReviewInventoryResult)
function M.compute_async(opts, cb)
  if opts.sem_available and not opts.sem_available() then
    cb({ rows = {}, details = {}, error = "Sem executable not found; walkthrough inventory requires Sem" })
    return
  end

  local status_files, untracked_files = status_file_index(opts)
  local tracked_result
  local untracked_result = #untracked_files == 0 and {} or nil
  local errors = {}

  local function finish()
    if tracked_result == nil or untracked_result == nil then return end
    if #errors > 0 then
      cb({ rows = {}, details = {}, error = table.concat(errors, "\n") })
      return
    end

    local counts = {}
    local details = {}
    local touched_files = {}
    local seen_entities = {}
    for _, entity in ipairs(tracked_result.changes or {}) do
      add_sem_entity(counts, details, touched_files, seen_entities, opts, entity)
    end
    for _, entity in ipairs(untracked_result or {}) do
      add_sem_entity(counts, details, touched_files, seen_entities, opts, entity, "added")
    end
    add_artifact_counts(counts, details, touched_files, status_files)
    cb({ rows = rows_from_counts(counts), details = details })
  end

  opts.run_text_async({
    "sem", "diff", "HEAD", "--format", "json", "--no-cosmetics", "-C", opts.cwd,
  }, nil, function(result)
    local decoded, err = decode_result(result, "diff")
    if err then errors[#errors + 1] = err end
    tracked_result = decoded or {}
    finish()
  end)

  if #untracked_files > 0 then
    local command = { "sem", "entities", "--format", "json" }
    vim.list_extend(command, untracked_files)
    opts.run_text_async(command, nil, function(result)
      local decoded, err = decode_result(result, "entities")
      if err then errors[#errors + 1] = err end
      untracked_result = decoded or {}
      finish()
    end)
  end
end

return M
