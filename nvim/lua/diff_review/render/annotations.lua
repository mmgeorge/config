--- Owns the diff-annotation (review comment) model: the by-id and by-anchor index, the
--- per-comment sync state machine, and a serial sync queue that drains dirty annotations
--- to the remote one at a time.
---
--- Rejects stale completions by operation id, so an out-of-order async result never
--- overwrites a newer local edit.

local source = require("diff_review.render.source")

---@alias DiffReviewAnnotationSide "LEFT"|"RIGHT"|"BOTH"|"old"|"new"|"both"|"file"
---@alias DiffReviewAnnotationState "clean"|"new"|"dirty"|"syncing"|"failed"|"error"|"deleted"

---@class DiffReviewDiffAnnotation
---@field id string
---@field kind string?
---@field source_id string?
---@field file_key string?
---@field path string
---@field side DiffReviewAnnotationSide
---@field line integer
---@field end_line integer?
---@field body string
---@field base_body string?
---@field remote_body string?
---@field author string?
---@field editable boolean
---@field state DiffReviewAnnotationState
---@field remote_id string?
---@field remote_url string?
---@field sync_operation_id integer?
---@field sync_error string?
---@field metadata table?

---@class DiffReviewAnnotationIndex
---@field by_id table<string, DiffReviewDiffAnnotation>
---@field by_anchor table<string, DiffReviewDiffAnnotation[]>
---@field dirty_by_id table<string, boolean>

---@class DiffReviewAnnotationSyncQueue
---@field pending DiffReviewDiffAnnotation[]
---@field queued_by_id table<string, boolean>
---@field running boolean
---@field next_operation_id integer
---@field handler fun(annotation: DiffReviewDiffAnnotation, done: fun(ok: boolean, remote_payload?: table, err?: string))

---@class DiffReviewAnnotationModule
local M = {}

--- Generates a composite anchor lookup key from a file path, diff side, and line number.
---@param path string Relative or absolute file path.
---@param side DiffReviewAnnotationSide Side of the diff anchor (`"LEFT"`, `"RIGHT"`, `"BOTH"`).
---@param line integer One-based line number inside the file or diff.
---@return string key Composite null-delimited anchor index key.
function M.anchor_key(path, side, line)
  return source.normalize_path(path) .. "\0" .. tostring(M.normalize_side(side)) .. "\0" .. tostring(tonumber(line) or 0)
end

--- Constructs an empty diff annotation index table.
---@return DiffReviewAnnotationIndex index Empty annotation index structure.
function M.new_index()
  return {
    by_id = {},
    by_anchor = {},
    dirty_by_id = {},
  }
end

---@param bucket DiffReviewDiffAnnotation[]
---@param id string
local function remove_from_bucket(bucket, id)
  for index = #bucket, 1, -1 do
    if bucket[index].id == id then table.remove(bucket, index) end
  end
end

--- Normalizes a diff side descriptor into canonical `"LEFT"`, `"RIGHT"`, or `"BOTH"`.
---@param side any Raw input side string (e.g. `"old"`, `"new"`, `"both"`, `"file"`).
---@return DiffReviewAnnotationSide side Canonical annotation side.
function M.normalize_side(side)
  local normalized = tostring(side or "RIGHT")
  if normalized == "new" then return "RIGHT" end
  if normalized == "old" then return "LEFT" end
  if normalized == "both" or normalized == "file" then return "BOTH" end
  if normalized == "LEFT" or normalized == "RIGHT" or normalized == "BOTH" then return normalized end
  return "RIGHT"
end

--- Normalizes a synchronization state identifier into a canonical status string.
---@param state any Raw state descriptor.
---@return DiffReviewAnnotationState state Normalized status string (`"clean"`, `"new"`, `"dirty"`, `"syncing"`, `"failed"`, `"error"`, `"deleted"`).
function M.normalize_state(state)
  local normalized = tostring(state or "clean")
  if normalized == "new"
    or normalized == "dirty"
    or normalized == "syncing"
    or normalized == "failed"
    or normalized == "error"
    or normalized == "deleted" then
    return normalized
  end
  return "clean"
end

--- Determines whether an annotation's status requires remote synchronization.
---@param state DiffReviewAnnotationState Annotation sync status.
---@return boolean requires_sync True if annotation needs remote sync.
function M.state_requires_sync(state)
  return state == "new" or state == "dirty" or state == "failed" or state == "error" or state == "deleted"
end

--- Inserts or updates an annotation record in the index, indexing it by ID and anchor location.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param annotation DiffReviewDiffAnnotation Annotation record to insert or update.
---@return DiffReviewDiffAnnotation annotation Normalized inserted annotation record.
function M.upsert(index, annotation)
  local previous = index.by_id[annotation.id]
  if previous then
    local previous_key = M.anchor_key(previous.path, previous.side, previous.line)
    local previous_bucket = index.by_anchor[previous_key]
    if previous_bucket then remove_from_bucket(previous_bucket, annotation.id) end
  end

  annotation.path = source.normalize_path(annotation.path)
  annotation.side = M.normalize_side(annotation.side)
  annotation.line = math.max(0, math.floor(tonumber(annotation.line) or 0))
  annotation.end_line = annotation.end_line and math.max(0, math.floor(tonumber(annotation.end_line) or 0)) or nil
  annotation.body = M.body_from_buffer_lines({ annotation.body or "" })
  annotation.base_body = annotation.base_body and M.body_from_buffer_lines({ annotation.base_body }) or nil
  annotation.remote_body = annotation.remote_body and M.body_from_buffer_lines({ annotation.remote_body }) or nil
  annotation.state = M.normalize_state(annotation.state)
  annotation.editable = annotation.editable == true
  annotation.metadata = annotation.metadata or {}

  index.by_id[annotation.id] = annotation
  local key = M.anchor_key(annotation.path, annotation.side, annotation.line)
  index.by_anchor[key] = index.by_anchor[key] or {}
  index.by_anchor[key][#index.by_anchor[key] + 1] = annotation
  if M.state_requires_sync(annotation.state) then
    index.dirty_by_id[annotation.id] = true
  else
    index.dirty_by_id[annotation.id] = nil
  end
  return annotation
end

--- Removes an annotation from the index by its unique identifier.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param id string Unique annotation identifier.
function M.remove(index, id)
  local annotation = index.by_id[id]
  if not annotation then return end
  local key = M.anchor_key(annotation.path, annotation.side, annotation.line)
  local bucket = index.by_anchor[key]
  if bucket then remove_from_bucket(bucket, id) end
  index.by_id[id] = nil
  index.dirty_by_id[id] = nil
end

--- Retrieves all annotations matching a given file anchor position.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param path string File path.
---@param side DiffReviewAnnotationSide Anchor diff side.
---@param line integer One-based line number.
---@return DiffReviewDiffAnnotation[] annotations Array of matching annotations.
function M.by_anchor(index, path, side, line)
  return index.by_anchor[M.anchor_key(path, side, line)] or {}
end

--- Marks an existing annotation as dirty with an optional updated comment body.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param id string Unique annotation identifier.
---@param body string? Optional new comment body text.
function M.mark_dirty(index, id, body)
  local annotation = index.by_id[id]
  if not annotation then return end
  if body ~= nil then annotation.body = M.body_from_buffer_lines({ body }) end
  annotation.state = "dirty"
  index.dirty_by_id[id] = true
end

--- Updates the text content of an annotation and marks it dirty when changes occur.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param id string Unique annotation identifier.
---@param body string|string[] Replacement text or lines array.
function M.replace_body(index, id, body)
  local annotation = index.by_id[id]
  if not annotation then return end
  local line_list = type(body) == "table" and body or { tostring(body or "") }
  local next_body = M.body_from_buffer_lines(line_list)
  if next_body == annotation.body then return end
  annotation.body = next_body
  if annotation.state ~= "new" then annotation.state = "dirty" end
  index.dirty_by_id[id] = true
end

--- Updates the sync state and error details for an annotation.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param id string Unique annotation identifier.
---@param state DiffReviewAnnotationState Next synchronization state.
---@param err? string Optional error message description.
function M.set_sync_state(index, id, state, err)
  local annotation = index.by_id[id]
  if not annotation then return end
  annotation.state = M.normalize_state(state)
  annotation.sync_error = err and tostring(err) or nil
  if M.state_requires_sync(annotation.state) then
    index.dirty_by_id[id] = true
  else
    annotation.base_body = annotation.body
    annotation.remote_body = annotation.body
    index.dirty_by_id[id] = nil
  end
end

--- Marks an annotation as clean, removing it from the dirty set.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@param id string Unique annotation identifier.
function M.mark_clean(index, id)
  local annotation = index.by_id[id]
  if not annotation then return end
  annotation.state = "clean"
  index.dirty_by_id[id] = nil
end

--- Returns all dirty annotations sorted alphabetically by ID.
---@param index DiffReviewAnnotationIndex Target annotation index.
---@return DiffReviewDiffAnnotation[] annotations Array of dirty annotation records.
function M.dirty(index)
  local annotation = {}
  for id in pairs(index.dirty_by_id or {}) do
    if index.by_id[id] then annotation[#annotation + 1] = index.by_id[id] end
  end
  table.sort(annotation, function(left, right)
    return tostring(left.id) < tostring(right.id)
  end)
  return annotation
end

--- Converts buffer lines into a single newline-separated body string.
---@param line string[] Array of buffer lines.
---@return string body Normalized multi-line string.
function M.body_from_buffer_lines(line)
  local text = table.concat(line or {}, "\n")
  text = text:gsub("\r\n", "\n"):gsub("\r", "\n")
  return text
end

--- Constructs a serial synchronization queue with operation-ID race protection.
---@param handler fun(annotation: table, done: fun(ok: boolean, remote_payload?: table, err?: string)) Handler invoked for each item.
---@param opts? { on_success?: fun(annotation: table, remote_payload?: table), on_failure?: fun(annotation: table, err?: string), on_idle?: fun(), stop_on_failure?: boolean } Queue configuration options.
---@return DiffReviewAnnotationSyncQueue queue Initialized synchronization queue.
function M.new_sync_queue(handler, opts)
  opts = opts or {}
  return {
    pending = {},
    queued_by_id = {},
    running = false,
    next_operation_id = 0,
    handler = handler,
    on_success = opts.on_success or M.on_sync_success,
    on_failure = opts.on_failure or M.on_sync_failure,
    on_idle = opts.on_idle,
    stop_on_failure = opts.stop_on_failure == true,
  }
end

--- Updates an annotation record following successful remote synchronization.
---@param annotation DiffReviewDiffAnnotation Target annotation record.
---@param remote_payload table? Response payload containing remote identifiers.
function M.on_sync_success(annotation, remote_payload)
  remote_payload = remote_payload or {}
  annotation.base_body = annotation.body
  annotation.remote_body = annotation.body
  annotation.state = "clean"
  annotation.sync_error = nil
  annotation.sync_operation_id = nil
  if remote_payload.id ~= nil then annotation.remote_id = remote_payload.id end
  if remote_payload.url ~= nil then annotation.remote_url = remote_payload.url end
end

--- Updates an annotation record following failed remote synchronization.
---@param annotation DiffReviewDiffAnnotation Target annotation record.
---@param err string? Error message describing the sync failure.
function M.on_sync_failure(annotation, err)
  annotation.state = "failed"
  annotation.sync_error = err and tostring(err) or "annotation sync failed"
  annotation.sync_operation_id = nil
end

--- Drains pending items from the sync queue serially.
---@param queue DiffReviewAnnotationSyncQueue Target sync queue.
function M.drain_sync_queue(queue)
  if queue.running then return end
  local annotation = table.remove(queue.pending, 1)
  if not annotation then
    if queue.on_idle then queue.on_idle() end
    return
  end
  if annotation.id ~= nil then queue.queued_by_id[annotation.id] = nil end
  queue.running = true
  queue.next_operation_id = (queue.next_operation_id or 0) + 1
  local operation_id = queue.next_operation_id
  annotation.sync_operation_id = operation_id
  annotation.state = "syncing"
  queue.handler(annotation, function(ok, remote_payload, err)
    -- Ignore a completion that a newer sync of the same annotation superseded.
    if annotation.sync_operation_id == operation_id then
      if ok then
        (queue.on_success or M.on_sync_success)(annotation, remote_payload)
      else
        (queue.on_failure or M.on_sync_failure)(annotation, err)
      end
    end
    queue.running = false
    -- Stop-on-failure queues halt the drain after a failed item and let callers requeue.
    if not (ok == false and queue.stop_on_failure) then
      M.drain_sync_queue(queue)
    elseif queue.on_idle then
      queue.on_idle()
    end
  end)
end

--- Enqueues an annotation for synchronization and starts queue draining if idle.
---@param queue DiffReviewAnnotationSyncQueue Target sync queue.
---@param annotation DiffReviewDiffAnnotation Target annotation to sync.
function M.enqueue_sync(queue, annotation)
  if queue.queued_by_id[annotation.id] then return end
  queue.queued_by_id[annotation.id] = true
  queue.pending[#queue.pending + 1] = annotation
  M.drain_sync_queue(queue)
end

return M
