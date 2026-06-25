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

---@class DiffReviewDiffSourceRegistry
---@field handle_by_id table<string, DiffReviewDiffSourceHandle>
---@field source_by_id table<string, DiffReviewDiffSourceState>
---@field source_order string[]
---@field invalidation_by_source table<string, table<string, boolean>>
---@field policy_by_kind table<string, table>
---@field policy_by_source table<string, table>

---@class DiffReviewSourceModule
local M = {}

---@param path string?
---@return string
function M.normalize_path(path)
  return tostring(path or ""):gsub("\\", "/")
end

---@param source_id string
---@param path string
---@return string
function M.file_key(source_id, path)
  return tostring(source_id) .. "\0" .. M.normalize_path(path)
end

---@return DiffReviewDiffSourceRegistry
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

---@param registry DiffReviewDiffSourceRegistry
---@param handle DiffReviewDiffSourceHandle
---@return DiffReviewDiffSourceHandle
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

---@param handle DiffReviewDiffSourceHandle
---@return DiffReviewDiffSourceState
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

---@param registry DiffReviewDiffSourceRegistry
---@param handle DiffReviewDiffSourceHandle
---@return DiffReviewDiffSourceState
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

---@param source DiffReviewDiffSourceState
---@param path string
---@param opts? table
---@return DiffReviewDiffFileState
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

---@param file DiffReviewDiffFileState
---@param side "old"|"new"
---@param text string
---@param revision string?
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

---@param file DiffReviewDiffFileState
---@param side "old"|"new"
---@param loader fun(done: fun(ok: boolean, text?: string, revision?: string, err?: string))
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

---@param file DiffReviewDiffFileState
---@param side "old"|"new"
---@param done fun(ok: boolean, snapshot?: DiffReviewTextSnapshot, err?: string)
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

---@param file DiffReviewDiffFileState
---@param hunk DiffReviewRawHunk
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

---@param file DiffReviewDiffFileState
---@param expanded boolean
function M.set_expanded(file, expanded)
  file.expanded = expanded == true
end

---@param file DiffReviewDiffFileState
---@param annotation table
function M.add_annotation(file, annotation)
  file.annotations[#file.annotations + 1] = annotation
  file.layout_dirty = true
end

---@param source DiffReviewDiffSourceState
---@param path string
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

---@param source DiffReviewDiffSourceState
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

---@param registry DiffReviewDiffSourceRegistry
---@param source_ids string|string[]
---@param paths string|string[]
function M.invalidate_paths(registry, source_ids, paths)
  if type(source_ids) == "string" then source_ids = { source_ids } end
  if type(paths) == "string" then paths = { paths } end
  for _, source_id in ipairs(source_ids or {}) do
    for _, path in ipairs(paths or {}) do
      invalidate_one_path(registry, source_id, path)
    end
  end
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_id string
---@return string[]
function M.invalidated_paths(registry, source_id)
  local invalidated = registry.invalidation_by_source[source_id] or {}
  local path_list = {}
  for path in pairs(invalidated) do
    path_list[#path_list + 1] = path
  end
  table.sort(path_list)
  return path_list
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_id string
function M.clear_invalidated_paths(registry, source_id)
  registry.invalidation_by_source[source_id] = nil
  local source = registry.source_by_id[source_id]
  if source then source.invalidated_path = {} end
end

---@param registry DiffReviewDiffSourceRegistry
---@param kind DiffReviewDiffSourceKind|string
---@param policy table
function M.set_kind_policy(registry, kind, policy)
  registry.policy_by_kind[kind] = policy
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_id string
---@param policy table
function M.set_source_policy(registry, source_id, policy)
  registry.policy_by_source[source_id] = policy
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_id string
---@return table?
function M.policy(registry, source_id)
  local source_policy = registry.policy_by_source[source_id]
  if source_policy then return source_policy end
  local source = registry.source_by_id[source_id]
  local handle = source and source.handle or registry.handle_by_id[source_id]
  return handle and registry.policy_by_kind[handle.kind] or nil
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_ids string|string[]
---@param paths string|string[]
---@param done fun(ok: boolean, err?: string)?
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
          finish_one(false, "Stale diff source reload ignored")
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

---@param source DiffReviewDiffSourceState
---@param done fun(ok: boolean, err?: string)
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

---@param header string?
---@return integer old_start
---@return integer old_count
---@return integer new_start
---@return integer new_count
---@return string? context
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

---@param diff_text string?
---@return string[] header_lines
---@return string[][] hunk_sections
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

---@param diff_line string?
---@return boolean
function M.diff_body_line(diff_line)
  local prefix = tostring(diff_line or ""):sub(1, 1)
  return prefix == " " or prefix == "+" or prefix == "-" or prefix == "\\"
end

---@param lines string[]
---@return integer added
---@return integer removed
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

---@param lines string[]
---@return integer old_count
---@return integer new_count
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

---@param hunk table
---@param header_lines string[]
---@param body_lines string[]
---@param old_start integer
---@param new_start integer
---@param context string?
---@param hunk_key string
---@param section_index integer
---@param chunk_index integer
---@return table
---@return string
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

---@param diff_line string?
---@param old_line integer
---@param new_line integer
---@return integer
---@return integer
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

---@param hunk table
---@param hunk_key string
---@param body_start_line integer
---@param body_count integer
---@param chunk_index integer
---@return table?
---@return string?
---@return table?
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

---@param hunk table
---@param hunk_key string
---@param chunk_size integer
---@return table[]
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

---@param diff_text string
---@param opts? { id_prefix?: string, source_id?: string, file_key?: string, staged?: boolean, metadata?: table }
---@return DiffReviewRawHunk[]
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

--- Build a walkthrough source that layers step annotations and ordering over base Git sources.
--- Reference base source ids instead of duplicating their file text.
---@param handle DiffReviewDiffSourceHandle?
---@param opts? { base_source_ids?: string[], file_order?: string[], step_annotations?: table[] }
---@return DiffReviewWalkthroughDiffSource
function M.new_walkthrough_source(handle, opts)
  opts = opts or {}
  handle = vim.deepcopy(handle or { id = "walkthrough", kind = "walkthrough" })
  handle.kind = "walkthrough"
  local source = M.new_source(handle)
  source.base_source_ids = opts.base_source_ids or {}
  source.file_order = opts.file_order or {}
  source.step_annotations = opts.step_annotations or {}
  source.navigation_index = {}
  for index, step in ipairs(source.step_annotations) do
    if step.id then source.navigation_index[step.id] = index end
  end
  return source
end

--- Append a walkthrough step annotation and index it by id for navigation.
---@param source DiffReviewWalkthroughDiffSource
---@param step { id: string }
function M.add_walkthrough_step(source, step)
  source.step_annotations[#source.step_annotations + 1] = step
  if step.id then source.navigation_index[step.id] = #source.step_annotations end
end

return M
