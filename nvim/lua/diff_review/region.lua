---@class DiffReviewRenderedRegion
---@field buf integer
---@field namespace integer
---@field start_anchor integer extmark id at the region's first row
---@field end_anchor integer extmark id at the region's end row
---@field region_kind string
---@field owner_id string?
---@field editable boolean
---@field end_exclusive boolean whether the end anchor sits one row past the last content row
---@field base_text string?

---@class DiffReviewRegionModule
local M = {}

--- Track a buffer line range by extmarks so edits and splices move the region with its text.
--- Inclusive mode (default) anchors the last content row with right gravity, so appending to
--- it stays inside the region. Exclusive mode anchors the row one past the content with left
--- gravity, so inserting at the boundary stays outside — matching a "beyond-the-last-line"
--- end marker.
---@param buf integer
---@param namespace integer
---@param first_row integer 0-based inclusive
---@param end_row integer 0-based; last content row (inclusive mode) or one-past row (exclusive mode)
---@param opts? { region_kind?: string, owner_id?: string, editable?: boolean, end_exclusive?: boolean, end_right_gravity?: boolean }
---@return DiffReviewRenderedRegion
function M.new(buf, namespace, first_row, end_row, opts)
  opts = opts or {}
  local end_exclusive = opts.end_exclusive == true
  local end_right_gravity = opts.end_right_gravity
  if end_right_gravity == nil then end_right_gravity = not end_exclusive end
  first_row = math.max(0, math.floor(tonumber(first_row) or 0))
  end_row = math.max(first_row, math.floor(tonumber(end_row) or first_row))
  local start_anchor = vim.api.nvim_buf_set_extmark(buf, namespace, first_row, 0, { right_gravity = false })
  local end_anchor = vim.api.nvim_buf_set_extmark(buf, namespace, end_row, 0, { right_gravity = end_right_gravity })
  local region = {
    buf = buf,
    namespace = namespace,
    start_anchor = start_anchor,
    end_anchor = end_anchor,
    region_kind = opts.region_kind or "markdown",
    owner_id = opts.owner_id,
    editable = opts.editable == true,
    end_exclusive = end_exclusive,
    base_text = nil,
  }
  region.base_text = M.read_text(region)
  return region
end

--- Resolve the region's raw 0-based anchor rows: the first row and the end-anchor row
--- (the last content row in inclusive mode, or the one-past row in exclusive mode).
---@param region DiffReviewRenderedRegion
---@return integer? first_row
---@return integer? end_row
function M.bounds(region)
  local start_pos = vim.api.nvim_buf_get_extmark_by_id(region.buf, region.namespace, region.start_anchor, {})
  local end_pos = vim.api.nvim_buf_get_extmark_by_id(region.buf, region.namespace, region.end_anchor, {})
  if not (start_pos[1] and end_pos[1]) then return nil, nil end
  local first_row = start_pos[1]
  return first_row, math.max(end_pos[1], first_row)
end

--- Resolve the region's current 0-based inclusive content row range from its anchors.
---@param region DiffReviewRenderedRegion
---@return integer? first_row
---@return integer? last_row inclusive last content row, or nil when the region is empty
function M.range(region)
  local first_row, end_row = M.bounds(region)
  if not first_row then return nil, nil end
  if region.end_exclusive then
    if end_row <= first_row then return first_row, nil end
    return first_row, end_row - 1
  end
  return first_row, end_row
end

--- Read the region's current buffer lines as a single newline-joined string.
---@param region DiffReviewRenderedRegion
---@return string
function M.read_text(region)
  local first_row, end_row = M.bounds(region)
  if not first_row then return "" end
  local after_row = region.end_exclusive and end_row or (end_row + 1)
  if after_row <= first_row then return "" end
  local lines = vim.api.nvim_buf_get_lines(region.buf, first_row, after_row, false)
  return table.concat(lines, "\n")
end

--- Report whether the region's current text differs from its last saved baseline.
---@param region DiffReviewRenderedRegion
---@return boolean
function M.is_dirty(region)
  return M.read_text(region) ~= (region.base_text or "")
end

--- Adopt the region's current text as the saved baseline after a successful save.
---@param region DiffReviewRenderedRegion
function M.mark_saved(region)
  region.base_text = M.read_text(region)
end

--- Overwrite the saved baseline with an explicit value (e.g. the server-side body) so dirty
--- tracking compares against the source of truth rather than the freshly rendered buffer.
---@param region DiffReviewRenderedRegion
---@param text string
function M.set_baseline(region, text)
  region.base_text = text
end

--- Remove the region's anchor extmarks.
---@param region DiffReviewRenderedRegion
function M.clear(region)
  pcall(vim.api.nvim_buf_del_extmark, region.buf, region.namespace, region.start_anchor)
  pcall(vim.api.nvim_buf_del_extmark, region.buf, region.namespace, region.end_anchor)
end

return M
