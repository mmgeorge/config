---@class ForgeRowSpans
---@field highlights table[]? list of { col_start, col_end, hl_group }
---@field gutter table[]? virt_text chunks for the sign/line-number column
---@field intraline table[]? list of { col_start, col_end, hl_group }

---@class ForgeDecorationCache
---@field spans_by_line table<string, table<integer, table<integer, ForgeRowSpans>>>
---@field hits integer
---@field misses integer

---@class ForgeRowDecorationRequest
---@field file_key string
---@field revision integer
---@field line integer source line within the file/side
---@field side "old"|"new"
---@field kind string row node kind

---@class ForgeDecorationProvider
---@field buf integer
---@field namespace integer
---@field cache ForgeDecorationCache
---@field visible_top integer
---@field visible_bottom integer

---@class ForgeDecorationHooks
---@field resolve fun(row: integer): ForgeRowDecorationRequest? map a buffer row to a decoration request
---@field compute fun(request: ForgeRowDecorationRequest): ForgeRowSpans? compute spans for a cache miss; return nil to defer
---@field emit fun(buf: integer, namespace: integer, row: integer, spans: ForgeRowSpans) place ephemeral marks

---@class ForgeDecorationModule
local M = {}

--- Constructs an empty row decoration spans cache table with performance counters.
---@return ForgeDecorationCache cache Newly initialized decoration cache.
function M.new_cache()
  return { spans_by_line = {}, hits = 0, misses = 0 }
end

--- Retrieves cached decoration spans for a source line, tracking cache hits and misses.
---@param cache ForgeDecorationCache Target decoration cache.
---@param file_key string Unique file identifier key.
---@param revision integer Content render revision number.
---@param line integer One-based source line number.
---@return ForgeRowSpans? spans Cached decoration spans, or nil if miss.
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

--- Memoizes computed decoration spans for a source line and evicts outdated revisions.
---@param cache ForgeDecorationCache Target decoration cache.
---@param file_key string Unique file identifier key.
---@param revision integer Content render revision number.
---@param line integer One-based source line number.
---@param spans ForgeRowSpans Computed decoration spans to store.
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

--- Evicts all cached spans for a file following mutations or reloads.
---@param cache ForgeDecorationCache Target decoration cache.
---@param file_key string Unique file identifier key.
function M.cache_invalidate(cache, file_key)
  cache.spans_by_line[file_key] = nil
end

--- Returns lookup statistics for cache hits and misses.
---@param cache ForgeDecorationCache Target decoration cache.
---@return { hits: integer, misses: integer } stats Cache hit and miss counts.
function M.cache_stats(cache)
  return { hits = cache.hits, misses = cache.misses }
end

--- Constructs a decoration provider instance bound to a target buffer and namespace.
---@param buf integer Target buffer handle.
---@param namespace integer Neovim highlight namespace ID.
---@param cache ForgeDecorationCache? Optional shared decoration cache.
---@return ForgeDecorationProvider provider Initialized decoration provider.
function M.new_provider(buf, namespace, cache)
  return {
    buf = buf,
    namespace = namespace,
    cache = cache or M.new_cache(),
    visible_top = 0,
    visible_bottom = -1,
  }
end

--- Records the visible row range boundaries reported during a window redraw callback.
---@param provider ForgeDecorationProvider Target decoration provider.
---@param top integer Zero-based visible top row.
---@param bottom integer Zero-based visible bottom row.
function M.set_visible_window(provider, top, bottom)
  provider.visible_top = math.max(0, math.floor(tonumber(top) or 0))
  provider.visible_bottom = math.max(provider.visible_top, math.floor(tonumber(bottom) or provider.visible_top))
end

--- Resolves decoration spans for a visible row from cache or fresh calculation.
--- Returns nil when the row has no diff content or calculation is deferred.
---@param provider ForgeDecorationProvider Target decoration provider.
---@param row integer Zero-based buffer row index.
---@param resolve fun(row: integer): ForgeRowDecorationRequest? Mapper resolving row to descriptor.
---@param compute fun(request: ForgeRowDecorationRequest): ForgeRowSpans? Calculator computing spans.
---@return ForgeRowSpans? spans Resolved decoration spans, or nil.
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

--- Registers the decoration provider callbacks with Neovim's decoration subsystem.
--- Scopes work to visible rows only.
---@param provider ForgeDecorationProvider Target decoration provider.
---@param hooks ForgeDecorationHooks Provider callbacks for resolution, calculation, and emission.
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
