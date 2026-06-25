---@class DiffReviewRowSpans
---@field highlights table[]? list of { col_start, col_end, hl_group }
---@field gutter table[]? virt_text chunks for the sign/line-number column
---@field intraline table[]? list of { col_start, col_end, hl_group }

---@class DiffReviewDecorationCache
---@field spans_by_line table<string, table<integer, table<integer, DiffReviewRowSpans>>>
---@field hits integer
---@field misses integer

---@class DiffReviewRowDecorationRequest
---@field file_key string
---@field revision integer
---@field line integer source line within the file/side
---@field side "old"|"new"
---@field kind string row node kind

---@class DiffReviewDecorationProvider
---@field buf integer
---@field namespace integer
---@field cache DiffReviewDecorationCache
---@field visible_top integer
---@field visible_bottom integer

---@class DiffReviewDecorationHooks
---@field resolve fun(row: integer): DiffReviewRowDecorationRequest? map a buffer row to a decoration request
---@field compute fun(request: DiffReviewRowDecorationRequest): DiffReviewRowSpans? compute spans for a cache miss; return nil to defer
---@field emit fun(buf: integer, namespace: integer, row: integer, spans: DiffReviewRowSpans) place ephemeral marks

---@class DiffReviewDecorationModule
local M = {}

---@return DiffReviewDecorationCache
function M.new_cache()
  return { spans_by_line = {}, hits = 0, misses = 0 }
end

--- Read cached spans for a source line, counting the lookup as a hit or miss.
---@param cache DiffReviewDecorationCache
---@param file_key string
---@param revision integer
---@param line integer
---@return DiffReviewRowSpans?
function M.cache_get(cache, file_key, revision, line)
  local by_revision = cache.spans_by_line[file_key]
  local by_line = by_revision and by_revision[revision] or nil
  local spans = by_line and by_line[line] or nil
  if spans then
    cache.hits = cache.hits + 1
  else
    cache.misses = cache.misses + 1
  end
  return spans
end

--- Memoize computed spans for a source line.
--- Drop other revisions so a file never caches stale highlight state.
---@param cache DiffReviewDecorationCache
---@param file_key string
---@param revision integer
---@param line integer
---@param spans DiffReviewRowSpans
function M.cache_put(cache, file_key, revision, line, spans)
  local by_revision = cache.spans_by_line[file_key]
  if not by_revision then
    by_revision = {}
    cache.spans_by_line[file_key] = by_revision
  end
  for existing_revision in pairs(by_revision) do
    if existing_revision ~= revision then by_revision[existing_revision] = nil end
  end
  local by_line = by_revision[revision]
  if not by_line then
    by_line = {}
    by_revision[revision] = by_line
  end
  by_line[line] = spans
end

--- Drop a file's cached spans after a reload, mutation, or annotation change.
---@param cache DiffReviewDecorationCache
---@param file_key string
function M.cache_invalidate(cache, file_key)
  cache.spans_by_line[file_key] = nil
end

---@param cache DiffReviewDecorationCache
---@return { hits: integer, misses: integer }
function M.cache_stats(cache)
  return { hits = cache.hits, misses = cache.misses }
end

---@param buf integer
---@param namespace integer
---@param cache DiffReviewDecorationCache?
---@return DiffReviewDecorationProvider
function M.new_provider(buf, namespace, cache)
  return {
    buf = buf,
    namespace = namespace,
    cache = cache or M.new_cache(),
    visible_top = 0,
    visible_bottom = -1,
  }
end

--- Record the visible row window reported by a redraw's on_win callback.
---@param provider DiffReviewDecorationProvider
---@param top integer
---@param bottom integer
function M.set_visible_window(provider, top, bottom)
  provider.visible_top = math.max(0, math.floor(tonumber(top) or 0))
  provider.visible_bottom = math.max(provider.visible_top, math.floor(tonumber(bottom) or provider.visible_top))
end

--- Resolve the decoration spans for one visible row, from cache or a fresh compute.
--- Return nil when the row owns no diff content or the compute defers (parse pending).
---@param provider DiffReviewDecorationProvider
---@param row integer 0-based buffer row
---@param resolve fun(row: integer): DiffReviewRowDecorationRequest?
---@param compute fun(request: DiffReviewRowDecorationRequest): DiffReviewRowSpans?
---@return DiffReviewRowSpans?
function M.decorate_row(provider, row, resolve, compute)
  local request = resolve(row)
  if not request then return nil end
  local spans = M.cache_get(provider.cache, request.file_key, request.revision, request.line)
  if spans then return spans end
  local computed = compute(request)
  if computed then
    M.cache_put(provider.cache, request.file_key, request.revision, request.line, computed)
  end
  return computed
end

--- Register the buffer's decoration provider so per-row work runs only for visible rows.
--- Keep on_line cheap; it reads the cache or defers, never parses synchronously.
---@param provider DiffReviewDecorationProvider
---@param hooks DiffReviewDecorationHooks
function M.register(provider, hooks)
  vim.api.nvim_set_decoration_provider(provider.namespace, {
    on_win = function(_, _, buf, toprow, botrow)
      if buf ~= provider.buf then return false end
      M.set_visible_window(provider, toprow, botrow)
      return true
    end,
    on_line = function(_, _, buf, row)
      if buf ~= provider.buf then return end
      local spans = M.decorate_row(provider, row, hooks.resolve, hooks.compute)
      if spans then hooks.emit(buf, provider.namespace, row, spans) end
    end,
  })
end

return M
