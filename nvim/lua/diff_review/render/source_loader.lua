--- Resolves a registry, source, and file in one call and replaces a file's hunks from
--- fresh diff text, so callers mutate the source model without threading the three
--- handles through every callsite.

local source = require("diff_review.render.source")

---@class DiffReviewSourceLoader
---@field registry DiffReviewDiffSourceRegistry
---@field source DiffReviewDiffSourceState

---@class DiffReviewSourceLoaderModule
local M = {}

--- Ensures source handle registration and returns a bundled loader structure.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param handle DiffReviewDiffSourceHandle Source handle configuration.
---@return DiffReviewSourceLoader loader Bundled registry and source state structure.
function M.ensure(registry, handle)
  local source_state = source.ensure_source(registry, handle)
  return {
    registry = registry,
    source = source_state,
  }
end

--- Ensures both the source and file diff state records are created in the registry.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param handle DiffReviewDiffSourceHandle Source handle configuration.
---@param path string Relative file path.
---@param opts? table Optional initial file options.
---@return DiffReviewDiffFileState file File diff state record.
function M.ensure_file(registry, handle, path, opts)
  local loader = M.ensure(registry, handle)
  return source.ensure_file(loader.source, path, opts or {})
end

--- Parses fresh diff text and replaces all hunk descriptors on a file state record.
---@param file DiffReviewDiffFileState Target file diff state.
---@param diff_text string Unified diff text.
---@param opts? table Optional hunk parsing options.
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

--- Invalidates specific file paths across target sources.
---@param registry DiffReviewDiffSourceRegistry Target source registry.
---@param source_ids string[] Array of source identifiers to invalidate.
---@param paths string[] Array of relative file paths to invalidate.
function M.invalidate(registry, source_ids, paths)
  source.invalidate_paths(registry, source_ids or {}, paths or {})
end

return M
