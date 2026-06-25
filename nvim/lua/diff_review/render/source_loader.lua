--- Resolves a registry, source, and file in one call and replaces a file's hunks from
--- fresh diff text, so callers mutate the source model without threading the three
--- handles through every callsite.

local source = require("diff_review.render.source")

---@class DiffReviewSourceLoader
---@field registry DiffReviewDiffSourceRegistry
---@field source DiffReviewDiffSourceState

---@class DiffReviewSourceLoaderModule
local M = {}

---@param registry DiffReviewDiffSourceRegistry
---@param handle DiffReviewDiffSourceHandle
---@return DiffReviewSourceLoader
function M.ensure(registry, handle)
  local source_state = source.ensure_source(registry, handle)
  return {
    registry = registry,
    source = source_state,
  }
end

---@param registry DiffReviewDiffSourceRegistry
---@param handle DiffReviewDiffSourceHandle
---@param path string
---@param opts? table
---@return DiffReviewDiffFileState
function M.ensure_file(registry, handle, path, opts)
  local loader = M.ensure(registry, handle)
  return source.ensure_file(loader.source, path, opts or {})
end

---@param file DiffReviewDiffFileState
---@param diff_text string
---@param opts? table
function M.replace_file_hunks(file, diff_text, opts)
  opts = opts or {}
  file.hunks = {}
  file.hunk_index_by_id = {}
  file.layout_dirty = true
  for _, hunk in ipairs(source.raw_hunks_from_diff(diff_text or "", {
    id_prefix = opts.id_prefix or file.key,
    source_id = file.source_id,
    file_key = file.key,
    staged = opts.staged,
    metadata = opts.metadata or {},
  })) do
    source.add_raw_hunk(file, hunk)
  end
end

---@param registry DiffReviewDiffSourceRegistry
---@param source_ids string[]
---@param paths string[]
function M.invalidate(registry, source_ids, paths)
  source.invalidate_paths(registry, source_ids or {}, paths or {})
end

return M
