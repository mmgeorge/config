--- Owns the diff-source data model: the source registry, per-source state, and the
--- per-file hunk, annotation, syntax-context, and text-snapshot records the render
--- pipeline reads from for git, commit, PR, review, and walkthrough diffs.
---
--- Loads file text lazily through per-side loaders and invalidates or reloads paths in
--- place, so a status refresh re-diffs only the files that actually changed.

local hunk_index = require("diff_review.render.hunk_index")
local syntax_context = require("diff_review.render.syntax_context")
local text_snapshot = require("diff_review.render.text_snapshot")

---@alias DiffReviewDiffSourceKind "unstaged"|"staged"|"commit"|"pr"|"review"|"walkthrough"|"branch"|"working_tree"|"index"|"pull_request"
---@alias DiffReviewDiffFileStageState "unstaged"|"staged"|"unviewed"|"viewed"|"readonly"

---@class DiffReviewDiffSourceHandle
---@field id string
---@field kind DiffReviewDiffSourceKind
---@field label string?
---@field cwd string?
---@field commit_oid string?
---@field lazy boolean?
---@field reload_paths? fun(source: DiffReviewDiffSourceState, paths: string[], done: fun(ok: boolean, err?: string))
---@field load? fun(source: DiffReviewDiffSourceState, done: fun(ok: boolean, err?: string))
---@field metadata table?

---@class DiffReviewRawHunk
---@field id string
---@field diff string
---@field patch_text string
---@field old_start integer
---@field old_count integer
---@field new_start integer
---@field new_count integer
---@field added integer
---@field removed integer
---@field staged boolean?
---@field metadata table?

---@class DiffReviewDisplayHunk
---@field id string
---@field source_id string?
---@field file_key string?
---@field diff string
---@field raw_hunks DiffReviewRawHunk[]
---@field old_start integer
---@field old_count integer
---@field new_start integer
---@field new_count integer
---@field added integer
---@field removed integer
---@field metadata table?

---@class DiffReviewDiffFileState
---@field key string
---@field source_id string
---@field path string
---@field original_path string?
---@field status string?
---@field stage_state DiffReviewDiffFileStageState?
---@field added integer
---@field removed integer
---@field expanded boolean
---@field old_text DiffReviewTextSnapshot?
---@field new_text DiffReviewTextSnapshot?
---@field old_revision string?
---@field new_revision string?
---@field text_loader table<string, fun(done: fun(ok: boolean, text?: string, revision?: string, err?: string))>
---@field text_loading table<string, boolean>
---@field text_pending_callback table<string, function[]>
---@field hunks DiffReviewRawHunk[]
---@field annotations table[]
---@field annotation_index table?
---@field syntax table
---@field syntax_context table?
---@field hunk_index_by_id table<string, DiffReviewHunkIndex>
---@field body_layout table?
---@field layout table?
---@field layout_dirty boolean
---@field layout_revision integer
---@field pending boolean
---@field stale boolean
---@field error string?
---@field metadata table

---@class DiffReviewDiffSourceState
---@field handle DiffReviewDiffSourceHandle
---@field loaded boolean
---@field loading boolean
---@field error string?
---@field revision integer
---@field invalidated_path table<string, boolean>
---@field file_by_key table<string, DiffReviewDiffFileState>
---@field file_order string[]
---@field pending_callback function[]
---@field metadata table

---@class DiffReviewDiffSourceRegistry
---@field handle_by_id table<string, DiffReviewDiffSourceHandle>
---@field source_by_id table<string, DiffReviewDiffSourceState>
---@field source_order string[]
---@field invalidation_by_source table<string, table<string, boolean>>
---@field policy_by_kind table<string, table>
---@field policy_by_source table<string, table>

---@class DiffReviewSourceModule
local M = {}

--- Normalizes backslashes in a file path to forward slashes.
---@param path string? Raw path string.
---@return string path Normalized path string with forward slashes.
function M.normalize_path(path)
  return (tostring(path or ""):gsub("\\", "/"))
end

--- Generates a composite unique lookup key for a file within a source.
---@param source_id string Unique source identifier.
---@param path string Relative file path.
---@return string key Composite null-delimited file key.
function M.file_key(source_id, path)
  return tostring(source_id) .. "\0" .. M.normalize_path(path)
end

--- Constructs an empty diff source registry table.
---@return DiffReviewDiffSourceRegistry registry Initialized source registry structure.
function M.new_registry()
  return {
    handle_by_id = {},
    source_by_id = {},
    source_order = {},
    invalidation_by_source = {},
    policy_by_kind = {},
    policy_by_source = {},
  }
end

--- Inserts or merges a source handle descriptor into the registry.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param handle DiffReviewDiffSourceHandle Source handle configuration.
---@return DiffReviewDiffSourceHandle handle Registered source handle descriptor.
function M.ensure_handle(registry, handle)
  local existing = registry.handle_by_id[handle.id]
  if existing then
    for key, value in pairs(handle) do
      existing[key] = value
    end
    return existing
  end
  local copy = vim.deepcopy(handle)
  registry.handle_by_id[handle.id] = copy
  return copy
end

--- Instantiates a new diff source state table from a handle descriptor.
---@param handle DiffReviewDiffSourceHandle Source handle configuration.
---@return DiffReviewDiffSourceState source Initialized diff source state structure.
function M.new_source(handle)
  return {
    handle = vim.deepcopy(handle),
    loaded = handle.lazy ~= true,
    loading = false,
    error = nil,
    revision = 0,
    invalidated_path = {},
    file_by_key = {},
    file_order = {},
    pending_callback = {},
    metadata = vim.deepcopy(handle.metadata or {}),
  }
end

--- Retrieves or instantiates a source state record in the registry.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param handle DiffReviewDiffSourceHandle Source handle configuration.
---@return DiffReviewDiffSourceState source Existing or newly created source state record.
function M.ensure_source(registry, handle)
  handle = M.ensure_handle(registry, handle)
  local source = registry.source_by_id[handle.id]
  if source then
    source.handle = handle
    source.metadata = vim.tbl_extend("force", source.metadata or {}, handle.metadata or {})
    return source
  end
  source = M.new_source(handle)
  registry.source_by_id[handle.id] = source
  registry.source_order[#registry.source_order + 1] = handle.id
  return source
end

--- Retrieves or instantiates a file diff state record under a source.
---@param source DiffReviewDiffSourceState Target diff source state.
---@param path string Relative file path.
---@param opts? table Optional initial file options.
---@return DiffReviewDiffFileState file File state structure.
function M.ensure_file(source, path, opts)
  opts = opts or {}
  path = M.normalize_path(path)
  local key = M.file_key(source.handle.id, path)
  local file = source.file_by_key[key]
  if file then return file end
  file = {
    key = key,
    source_id = source.handle.id,
    path = path,
    original_path = opts.original_path and M.normalize_path(opts.original_path) or nil,
    status = opts.status,
    stage_state = opts.stage_state,
    added = tonumber(opts.added) or 0,
    removed = tonumber(opts.removed) or 0,
    expanded = opts.expanded == true,
    old_text = nil,
    new_text = nil,
    old_revision = opts.old_revision,
    new_revision = opts.new_revision,
    text_loader = {},
    text_loading = {},
    text_pending_callback = {},
    hunks = {},
    annotations = {},
    annotation_index = opts.annotation_index,
    syntax = {},
    syntax_context = nil,
    hunk_index_by_id = {},
    body_layout = nil,
    layout = nil,
    layout_dirty = true,
    layout_revision = 0,
    pending = opts.pending == true,
    stale = opts.stale == true,
    error = opts.error,
    metadata = opts.metadata or {},
  }
  source.file_by_key[key] = file
  source.file_order[#source.file_order + 1] = key
  return file
end

--- Updates the text snapshot and revision for either the old or new side of a file diff.
---@param file DiffReviewDiffFileState Target file diff state.
---@param side "old"|"new" Diff side descriptor.
---@param text string Full buffer content string.
---@param revision string? Optional revision hash or timestamp.
function M.set_text(file, side, text, revision)
  local snapshot = text_snapshot.from_text(text or "")
  file.syntax_context = syntax_context.ensure_context(file.syntax_context, file.key)
  syntax_context.set_snapshot(file.syntax_context, side, snapshot, revision)
  if side == "old" then
    file.old_text = snapshot
    file.old_revision = revision
  else
    file.new_text = snapshot
    file.new_revision = revision
  end
end

--- Registers an asynchronous content loader for a specific side of a file diff.
---@param file DiffReviewDiffFileState Target file diff state.
---@param side "old"|"new" Diff side descriptor.
---@param loader fun(done: fun(ok: boolean, text?: string, revision?: string, err?: string)) Async content loading function.
function M.set_text_loader(file, side, loader)
  file.text_loader = file.text_loader or {}
  file.text_loader[side] = loader
end

---@param file DiffReviewDiffFileState
---@param side "old"|"new"
---@return DiffReviewTextSnapshot?
local function cached_text(file, side)
  if side == "old" then return file.old_text end
  return file.new_text
end

--- Ensures the text snapshot for a file diff side is loaded asynchronously.
--- Invokes `done` with the snapshot or error details.
---@param file DiffReviewDiffFileState Target file diff state.
---@param side "old"|"new" Diff side descriptor.
---@param done fun(ok: boolean, snapshot?: DiffReviewTextSnapshot, err?: string) Completion callback.
function M.ensure_text(file, side, done)
  local snapshot = cached_text(file, side)
  if snapshot then
    done(true, snapshot)
    return
  end
  local loader = file.text_loader and file.text_loader[side] or nil
  if type(loader) ~= "function" then
    done(false, nil, "No text loader for " .. tostring(side) .. " side")
    return
  end
  file.text_pending_callback = file.text_pending_callback or {}
  file.text_pending_callback[side] = file.text_pending_callback[side] or {}
  if file.text_loading and file.text_loading[side] then
    file.text_pending_callback[side][#file.text_pending_callback[side] + 1] = done
    return
  end
  file.text_loading = file.text_loading or {}
  file.text_loading[side] = true
  file.text_pending_callback[side][#file.text_pending_callback[side] + 1] = done
  loader(function(ok, text, revision, err)
    file.text_loading[side] = false
    local callback = file.text_pending_callback[side] or {}
    file.text_pending_callback[side] = {}
    local loaded_snapshot = nil
    local load_error = nil
    if ok then
      M.set_text(file, side, text or "", revision)
      loaded_snapshot = cached_text(file, side)
    else
      load_error = tostring(err or "Unable to load text snapshot")
    end
    for _, pending in ipairs(callback) do
      pending(ok == true, loaded_snapshot, load_error)
    end
  end)
end

--- Appends a raw diff hunk descriptor to a file state record and creates its hunk index.
---@param file DiffReviewDiffFileState Target file diff state.
---@param hunk DiffReviewRawHunk Raw hunk descriptor table.
function M.add_raw_hunk(file, hunk)
  local copy = vim.deepcopy(hunk)
  copy.source_id = copy.source_id or file.source_id
  copy.file_key = copy.file_key or file.key
  copy.diff_review_hunk_index = copy.diff_review_hunk_index or hunk_index.from_hunk(copy)
  file.hunk_index_by_id = file.hunk_index_by_id or {}
  file.hunk_index_by_id[copy.id or tostring(#file.hunks + 1)] = copy.diff_review_hunk_index
  file.hunks[#file.hunks + 1] = copy
  file.layout_dirty = true
end

--- Sets the expanded/collapsed view state of a file diff.
---@param file DiffReviewDiffFileState Target file diff state.
---@param expanded boolean True to expand diff hunks, false to collapse.
function M.set_expanded(file, expanded)
  file.expanded = expanded == true
end

--- Attaches an annotation metadata record to a file diff state.
---@param file DiffReviewDiffFileState Target file diff state.
---@param annotation table Annotation descriptor table.
function M.add_annotation(file, annotation)
  file.annotations[#file.annotations + 1] = annotation
  file.layout_dirty = true
end

--- Removes a file diff record from a source by relative path.
---@param source DiffReviewDiffSourceState Target source state.
---@param path string Relative file path to remove.
function M.remove_file(source, path)
  local key = M.file_key(source.handle.id, path)
  if not source.file_by_key[key] then return end
  source.file_by_key[key] = nil
  local next_order = {}
  for _, file_key in ipairs(source.file_order or {}) do
    if file_key ~= key then next_order[#next_order + 1] = file_key end
  end
  source.file_order = next_order
  source.revision = (source.revision or 0) + 1
end

--- Clears all file diff state records under a source.
---@param source DiffReviewDiffSourceState Target source state.
function M.clear_files(source)
  source.file_by_key = {}
  source.file_order = {}
  source.revision = (source.revision or 0) + 1
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_id string
---@param path string
local function invalidate_one_path(registry, source_id, path)
  path = M.normalize_path(path)
  registry.invalidation_by_source[source_id] = registry.invalidation_by_source[source_id] or {}
  registry.invalidation_by_source[source_id][path] = true
  local source = registry.source_by_id[source_id]
  if not source then return end
  source.invalidated_path[path] = true
  for _, file in pairs(source.file_by_key or {}) do
    if file.path == path or file.original_path == path then
      file.stale = true
      file.pending = true
      file.layout_dirty = true
    end
  end
end

--- Marks specific file paths as invalidated and stale across selected sources.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_ids string|string[] One or more source IDs to invalidate paths in.
---@param paths string|string[] One or more relative file paths to invalidate.
function M.invalidate_paths(registry, source_ids, paths)
  if type(source_ids) == "string" then source_ids = { source_ids } end
  if type(paths) == "string" then paths = { paths } end
  for _, source_id in ipairs(source_ids or {}) do
    for _, path in ipairs(paths or {}) do
      invalidate_one_path(registry, source_id, path)
    end
  end
end

--- Returns a sorted list of invalidated paths registered under a source ID.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_id string Target source identifier.
---@return string[] paths Sorted array of invalidated file path strings.
function M.invalidated_paths(registry, source_id)
  local invalidated = registry.invalidation_by_source[source_id] or {}
  local path_list = {}
  for path in pairs(invalidated) do
    path_list[#path_list + 1] = path
  end
  table.sort(path_list)
  return path_list
end

--- Clears all invalidated path tracking records for a given source ID.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_id string Target source identifier.
function M.clear_invalidated_paths(registry, source_id)
  registry.invalidation_by_source[source_id] = nil
  local source = registry.source_by_id[source_id]
  if source then source.invalidated_path = {} end
end

--- Clears specific resolved paths from the invalidation sets while preserving outstanding paths.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_ids string|string[] Target source identifier or identifiers.
---@param paths string|string[] Resolved file paths to clear from the invalidation set.
function M.clear_invalidated_path_list(registry, source_ids, paths)
  if type(source_ids) == "string" then source_ids = { source_ids } end
  if type(paths) == "string" then paths = { paths } end
  for _, source_id in ipairs(source_ids or {}) do
    local registry_path_set = registry.invalidation_by_source[source_id]
    local source_state = registry.source_by_id[source_id]
    for _, path in ipairs(paths or {}) do
      local normalized_path = M.normalize_path(path)
      if registry_path_set then registry_path_set[normalized_path] = nil end
      if source_state then source_state.invalidated_path[normalized_path] = nil end
    end
    if registry_path_set and next(registry_path_set) == nil then
      registry.invalidation_by_source[source_id] = nil
    end
  end
end

--- Associates a layout/render policy table with a diff source kind.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param kind DiffReviewDiffSourceKind|string Target source kind.
---@param policy table Policy options table.
function M.set_kind_policy(registry, kind, policy)
  registry.policy_by_kind[kind] = policy
end

--- Associates a layout/render policy table with a specific source ID.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_id string Target source identifier.
---@param policy table Policy options table.
function M.set_source_policy(registry, source_id, policy)
  registry.policy_by_source[source_id] = policy
end

--- Resolves the effective layout/render policy for a source ID from source or kind overrides.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_id string Target source identifier.
---@return table? policy Resolved policy table, or nil if no policy matched.
function M.policy(registry, source_id)
  local source_policy = registry.policy_by_source[source_id]
  if source_policy then return source_policy end
  local source = registry.source_by_id[source_id]
  local handle = source and source.handle or registry.handle_by_id[source_id]
  return handle and registry.policy_by_kind[handle.kind] or nil
end

--- Reloads diff state asynchronously for invalidated paths across target sources.
--- Invokes `done` once all source reload handlers settle.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_ids string|string[] One or more source IDs to reload.
---@param paths string|string[] Relative file paths that were modified.
---@param done fun(ok: boolean, err?: string)? Optional completion callback.
function M.reload_paths(registry, source_ids, paths, done)
  if type(source_ids) == "string" then source_ids = { source_ids } end
  if type(paths) == "string" then paths = { paths } end
  source_ids = source_ids or {}
  local normalized_paths = {}
  for _, path in ipairs(paths or {}) do
    normalized_paths[#normalized_paths + 1] = M.normalize_path(path)
  end
  paths = normalized_paths
  M.invalidate_paths(registry, source_ids, paths)
  local remaining = #source_ids
  local failed = nil
  if remaining == 0 then
    if done then done(true) end
    return
  end
  local function finish_one(ok, err)
    if not ok and not failed then failed = tostring(err or "Unable to reload diff source") end
    remaining = remaining - 1
    if remaining > 0 then return end
    if done then done(failed == nil, failed) end
  end
  for _, source_id in ipairs(source_ids) do
    local source = registry.source_by_id[source_id]
    if not source then
      finish_one(true)
    elseif type(source.handle.reload_paths) ~= "function" then
      source.revision = (source.revision or 0) + 1
      M.clear_invalidated_paths(registry, source_id)
      for _, file in pairs(source.file_by_key or {}) do
        file.pending = false
      end
      finish_one(true)
    else
      local revision = (source.revision or 0) + 1
      source.revision = revision
      source.loading = true
      source.handle.reload_paths(source, paths, function(ok, err)
        if source.revision ~= revision then
          -- A newer reload superseded this one and will settle the source; this
          -- callback is a harmless no-op, not a failure — don't poison the batch.
          finish_one(true)
          return
        end
        source.loading = false
        source.error = ok and nil or tostring(err or "Unable to reload diff source")
        if ok then
          M.clear_invalidated_paths(registry, source_id)
          for _, file in pairs(source.file_by_key or {}) do
            file.pending = false
            file.stale = false
          end
        end
        finish_one(ok, source.error)
      end)
    end
  end
end

--- Ensures a lazy diff source has completed its asynchronous load handler.
---@param source DiffReviewDiffSourceState Target diff source state.
---@param done fun(ok: boolean, err?: string) Completion callback.
function M.ensure_loaded(source, done)
  if source.loaded then
    done(true)
    return
  end
  if source.loading then
    source.pending_callback[#source.pending_callback + 1] = done
    return
  end
  local loader = source.handle.load
  if type(loader) ~= "function" then
    source.loaded = true
    done(true)
    return
  end
  source.loading = true
  source.pending_callback[#source.pending_callback + 1] = done
  loader(source, function(ok, err)
    source.loading = false
    source.loaded = ok == true
    source.error = ok and nil or tostring(err or "Unable to load diff source")
    local callback = source.pending_callback
    source.pending_callback = {}
    for _, pending in ipairs(callback) do
      pending(source.loaded, source.error)
    end
  end)
end

--- Parses unified diff hunk header line components (`@@ -old,count +new,count @@ context`).
---@param header string? Raw hunk header string.
---@return integer old_start Starting line in old revision.
---@return integer old_count Line count in old revision.
---@return integer new_start Starting line in new revision.
---@return integer new_count Line count in new revision.
---@return string? context Trailing function or scope context string, if present.
function M.parse_hunk_header(header)
  local old_start, old_count, new_start, new_count, context = tostring(header or ""):match(
    "^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@%s?(.*)$"
  )
  old_start = tonumber(old_start) or 0
  new_start = tonumber(new_start) or 0
  old_count = tonumber(old_count ~= "" and old_count or "1") or 1
  new_count = tonumber(new_count ~= "" and new_count or "1") or 1
  if context == "" then context = nil end
  return old_start, old_count, new_start, new_count, context
end

--- Splits a unified diff text string into file header lines and per-hunk section arrays.
---@param diff_text string? Full diff text string.
---@return string[] header_lines Array of leading file header lines preceding the first hunk.
---@return string[][] hunk_sections Array of hunk section line arrays.
function M.hunk_diff_parts(diff_text)
  local header_lines = {}
  local hunk_sections = {}
  local current_section = nil
  for _, line in ipairs(vim.split(tostring(diff_text or ""), "\n", { plain = true })) do
    if line:match("^@@ ") then
      current_section = { line }
      hunk_sections[#hunk_sections + 1] = current_section
    elseif current_section then
      current_section[#current_section + 1] = line
    else
      header_lines[#header_lines + 1] = line
    end
  end
  return header_lines, hunk_sections
end

--- Tests whether a line string is a diff body line starting with `" "`, `"+"`, `"-"`, or `"\"`.
---@param diff_line string? Line string to inspect.
---@return boolean is_body True if line starts with a diff line prefix.
function M.diff_body_line(diff_line)
  local prefix = tostring(diff_line or ""):sub(1, 1)
  return prefix == " " or prefix == "+" or prefix == "-" or prefix == "\\"
end

--- Counts added (`+`) and removed (`-`) lines in a list of diff lines.
---@param lines string[] Array of diff line strings.
---@return integer added Count of lines with `+` prefix.
---@return integer removed Count of lines with `-` prefix.
function M.diff_stats(lines)
  local added = 0
  local removed = 0
  for _, line in ipairs(lines or {}) do
    local prefix = tostring(line or ""):sub(1, 1)
    if prefix == "+" then
      added = added + 1
    elseif prefix == "-" then
      removed = removed + 1
    end
  end
  return added, removed
end

--- Computes old and new revision line counts from a list of diff lines.
---@param lines string[] Array of diff line strings.
---@return integer old_count Total context and deleted lines.
---@return integer new_count Total context and added lines.
function M.diff_line_counts(lines)
  local old_count = 0
  local new_count = 0
  for _, line in ipairs(lines or {}) do
    local prefix = tostring(line or ""):sub(1, 1)
    if prefix == " " then
      old_count = old_count + 1
      new_count = new_count + 1
    elseif prefix == "-" then
      old_count = old_count + 1
    elseif prefix == "+" then
      new_count = new_count + 1
    end
  end
  return old_count, new_count
end

--- Constructs a sub-hunk chunk from header and body lines, calculating stats and render keys.
---@param hunk table Parent raw hunk table.
---@param header_lines string[] Leading file header lines.
---@param body_lines string[] Chunk body lines.
---@param old_start integer Starting line in old revision.
---@param new_start integer Starting line in new revision.
---@param context string? Optional function/scope context text.
---@param hunk_key string Parent hunk unique key.
---@param section_index integer One-based hunk section index.
---@param chunk_index integer One-based chunk index within section.
---@return table chunk Formatted sub-hunk chunk descriptor.
---@return string render_key Unique render key for the chunk.
function M.chunk_hunk(hunk, header_lines, body_lines, old_start, new_start, context, hunk_key, section_index, chunk_index)
  local added, removed = M.diff_stats(body_lines)
  local old_count, new_count = M.diff_line_counts(body_lines)
  local suffix = context and context ~= "" and (" " .. context) or ""
  local chunk_lines = vim.deepcopy(header_lines or {})
  chunk_lines[#chunk_lines + 1] = ("@@ -%d,%d +%d,%d @@%s"):format(old_start, old_count, new_start, new_count, suffix)
  vim.list_extend(chunk_lines, body_lines)
  local chunk = vim.deepcopy(hunk)
  chunk.diff = table.concat(chunk_lines, "\n")
  chunk.pos = new_start
  chunk.added = added
  chunk.removed = removed
  chunk.lazy_estimate = nil
  chunk.raw_hunks = hunk.raw_hunks or { hunk }
  local render_key = ("%s:lazy:%d:%d"):format(hunk_key, section_index, chunk_index)
  return chunk, render_key
end

--- Advances old and new line numbers based on the leading character prefix of a diff line.
---@param diff_line string? Diff line string to parse.
---@param old_line integer Current old line number.
---@param new_line integer Current new line number.
---@return integer next_old_line Updated old line number.
---@return integer next_new_line Updated new line number.
function M.advance_diff_line(diff_line, old_line, new_line)
  local prefix = tostring(diff_line or ""):sub(1, 1)
  if prefix == " " then
    return old_line + 1, new_line + 1
  elseif prefix == "-" then
    return old_line + 1, new_line
  elseif prefix == "+" then
    return old_line, new_line + 1
  end
  return old_line, new_line
end

--- Extracts a window of body lines from a hunk for lazy rendering.
---@param hunk table Target hunk structure.
---@param hunk_key string Unique hunk key string.
---@param body_start_line integer One-based starting body line within the hunk.
---@param body_count integer Number of body lines to extract.
---@param chunk_index integer One-based chunk index.
---@return table? chunk Rendered hunk chunk descriptor, or nil.
---@return string? render_key Unique render key for the chunk, or nil.
---@return table? syntax_offset Syntax highlight line offsets for old and new sides, or nil.
function M.deferred_hunk_chunk(hunk, hunk_key, body_start_line, body_count, chunk_index)
  local indexed_chunk, indexed_render_key, indexed_syntax_offset = hunk_index.chunk(
    hunk_index.ensure(hunk),
    hunk_key,
    body_start_line,
    body_count,
    chunk_index
  )
  if indexed_chunk then return indexed_chunk, indexed_render_key, indexed_syntax_offset end

  body_start_line = math.max(1, tonumber(body_start_line) or 1)
  body_count = math.max(1, tonumber(body_count) or 1)
  chunk_index = math.max(1, tonumber(chunk_index) or 1)

  local header_lines = {}
  local section_index = 0
  local old_line = 0
  local new_line = 0
  local context = nil
  local chunk_old_start = nil
  local chunk_new_start = nil
  local selected_body = {}
  local body_line = 0
  local in_hunk = false
  local remaining = body_count
  local old_syntax_row_offset = 0
  local new_syntax_row_offset = 0

  for line in tostring(hunk and hunk.diff or ""):gmatch("[^\n]+") do
    if line:match("^@@ ") then
      section_index = section_index + 1
      old_line, _, new_line, _, context = M.parse_hunk_header(line)
      in_hunk = true
    elseif in_hunk then
      if M.diff_body_line(line) then
        body_line = body_line + 1
        if body_line < body_start_line then
          local prefix = line:sub(1, 1)
          if prefix == " " then
            old_syntax_row_offset = old_syntax_row_offset + 1
            new_syntax_row_offset = new_syntax_row_offset + 1
          elseif prefix == "-" then
            old_syntax_row_offset = old_syntax_row_offset + 1
          elseif prefix == "+" then
            new_syntax_row_offset = new_syntax_row_offset + 1
          end
          old_line, new_line = M.advance_diff_line(line, old_line, new_line)
        elseif remaining > 0 then
          chunk_old_start = chunk_old_start or old_line
          chunk_new_start = chunk_new_start or new_line
          selected_body[#selected_body + 1] = line
          remaining = remaining - 1
          old_line, new_line = M.advance_diff_line(line, old_line, new_line)
          if remaining <= 0 then break end
        end
      end
    else
      header_lines[#header_lines + 1] = line
    end
  end

  if #selected_body == 0 then return nil, nil end
  local chunk, render_key = M.chunk_hunk(
    hunk,
    header_lines,
    selected_body,
    chunk_old_start or old_line,
    chunk_new_start or new_line,
    context,
    hunk_key,
    math.max(1, section_index),
    chunk_index
  )
  return chunk, render_key, {
    old = old_syntax_row_offset,
    new = new_syntax_row_offset,
  }
end

--- Divides large diff hunks into smaller chunk blocks bounded by `chunk_size`.
--- Preserves replacement pairs across chunk boundaries.
---@param hunk table Target hunk structure.
---@param hunk_key string Unique hunk key string.
---@param chunk_size integer Maximum body lines allowed per chunk.
---@return table[] blocks Array of chunk block descriptors.
function M.hunk_chunks(hunk, hunk_key, chunk_size)
  local header_lines, hunk_sections = M.hunk_diff_parts(hunk and hunk.diff or "")
  local block_list = {}
  chunk_size = math.max(20, tonumber(chunk_size) or 80)
  for section_index, section in ipairs(hunk_sections or {}) do
    local old_start, _, new_start, _, context = M.parse_hunk_header(section[1] or "")
    local old_line = old_start
    local new_line = new_start
    local body = {}
    for line_index = 2, #section do
      body[#body + 1] = section[line_index]
    end
    local chunk_body = {}
    local chunk_old_start = old_line
    local chunk_new_start = new_line
    local chunk_index = 0
    local function flush_chunk()
      if #chunk_body == 0 then return end
      chunk_index = chunk_index + 1
      local chunk_hunk, render_key = M.chunk_hunk(
        hunk,
        header_lines,
        chunk_body,
        chunk_old_start,
        chunk_new_start,
        context,
        hunk_key,
        section_index,
        chunk_index
      )
      block_list[#block_list + 1] = {
        hunk_key = hunk_key,
        render_key = render_key,
        hunk = chunk_hunk,
        original_hunk = hunk,
      }
      chunk_body = {}
    end
    for _, line in ipairs(body) do
      local prefix = line:sub(1, 1)
      local previous_prefix = #chunk_body > 0 and chunk_body[#chunk_body]:sub(1, 1) or ""
      local keep_replacement_pair = (previous_prefix == "-" and prefix == "+") or (previous_prefix == "+" and prefix == "-")
      if #chunk_body >= chunk_size and prefix ~= "\\" and not keep_replacement_pair then
        flush_chunk()
        chunk_old_start = old_line
        chunk_new_start = new_line
      end
      if #chunk_body == 0 then
        chunk_old_start = old_line
        chunk_new_start = new_line
      end
      chunk_body[#chunk_body + 1] = line
      if prefix == " " then
        old_line = old_line + 1
        new_line = new_line + 1
      elseif prefix == "-" then
        old_line = old_line + 1
      elseif prefix == "+" then
        new_line = new_line + 1
      end
    end
    flush_chunk()
  end
  if #block_list == 0 then
    block_list[1] = {
      hunk_key = hunk_key,
      render_key = hunk_key .. ":lazy:1:1",
      hunk = vim.deepcopy(hunk),
      original_hunk = hunk,
    }
  end
  for index, block in ipairs(block_list) do
    block.hide_header = index > 1
    block.previous_hunk = block_list[index - 1] and block_list[index - 1].hunk or nil
    block.next_hunk = block_list[index + 1] and block_list[index + 1].hunk or nil
  end
  return block_list
end

--- Parses unified diff text into an array of raw diff hunk descriptors.
---@param diff_text string Raw unified diff text.
---@param opts? { id_prefix?: string, source_id?: string, file_key?: string, staged?: boolean, metadata?: table } Raw hunk options.
---@return DiffReviewRawHunk[] hunks Array of raw hunk descriptors.
function M.raw_hunks_from_diff(diff_text, opts)
  opts = opts or {}
  local header_lines, sections = M.hunk_diff_parts(diff_text)
  local hunks = {}
  for section_index, section in ipairs(sections or {}) do
    local old_start, old_count, new_start, new_count = M.parse_hunk_header(section[1])
    local section_lines = vim.deepcopy(header_lines)
    vim.list_extend(section_lines, section)
    local added, removed = M.diff_stats(section)
    local patch_text = table.concat(section_lines, "\n")
    hunks[#hunks + 1] = {
      id = ("%s:%d"):format(opts.id_prefix or "raw", section_index),
      diff = patch_text,
      patch_text = patch_text,
      old_start = old_start,
      old_count = old_count,
      new_start = new_start,
      new_count = new_count,
      added = added,
      removed = removed,
      staged = opts.staged,
      metadata = vim.deepcopy(opts.metadata or {}),
    }
  end
  return hunks
end

---@class DiffReviewWalkthroughDiffSource : DiffReviewDiffSourceState
---@field base_source_ids string[]
---@field file_order string[]
---@field step_annotations table[]
---@field navigation_index table<string, integer>

--- Constructs a walkthrough diff source layering step annotations over base Git sources.
---@param handle DiffReviewDiffSourceHandle? Optional source handle descriptor.
---@param opts? { base_source_ids?: string[], file_order?: string[], step_annotations?: table[] } Walkthrough source options.
---@return DiffReviewWalkthroughDiffSource source Initialized walkthrough diff source structure.
function M.new_walkthrough_source(handle, opts)
  opts = opts or {}
  handle = vim.deepcopy(handle or { id = "walkthrough", kind = "walkthrough" })
  handle.kind = "walkthrough"
  local source = M.new_source(handle) --[[@as DiffReviewWalkthroughDiffSource]]
  source.base_source_ids = opts.base_source_ids or {}
  source.file_order = opts.file_order or {}
  source.step_annotations = opts.step_annotations or {}
  source.navigation_index = {}
  for index, step in ipairs(source.step_annotations) do
    if step.id then source.navigation_index[step.id] = index end
  end
  return source
end

--- Appends a walkthrough step annotation record and updates its navigation index entry.
---@param source DiffReviewWalkthroughDiffSource Target walkthrough source state.
---@param step { id: string } Step annotation table containing a unique ID.
function M.add_walkthrough_step(source, step)
  source.step_annotations[#source.step_annotations + 1] = step
  if step.id then source.navigation_index[step.id] = #source.step_annotations end
end

return M
