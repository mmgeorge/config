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

--- Creates an extmark-anchored buffer region that tracks row shifts across buffer edits.
---@param buf integer Target buffer handle.
---@param namespace integer Highlight namespace ID for extmark placement.
---@param first_row integer Zero-based inclusive start row.
---@param end_row integer Zero-based anchor end row (last content row or boundary).
---@param opts? { region_kind?: string, owner_id?: string, editable?: boolean, end_exclusive?: boolean, end_right_gravity?: boolean } Region options.
---@return DiffReviewRenderedRegion region Initialized region tracker table.
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

--- Resolves the region's raw zero-based anchor row positions.
---@param region DiffReviewRenderedRegion Target region record.
---@return integer? first_row Zero-based start row index, or nil.
---@return integer? end_row Zero-based anchor end row index, or nil.
function M.bounds(region)
  local start_pos = vim.api.nvim_buf_get_extmark_by_id(region.buf, region.namespace, region.start_anchor, {})
  local end_pos = vim.api.nvim_buf_get_extmark_by_id(region.buf, region.namespace, region.end_anchor, {})
  if not (start_pos[1] and end_pos[1]) then return nil, nil end
  local first_row = start_pos[1]
  return first_row, math.max(end_pos[1], first_row)
end

--- Resolves the region's zero-based inclusive content row range.
--- Returns nil for `last_row` if an exclusive region has collapsed to empty.
---@param region DiffReviewRenderedRegion Target region record.
---@return integer? first_row Zero-based starting content row, or nil.
---@return integer? last_row Zero-based inclusive ending content row, or nil.
function M.range(region)
  local first_row, end_row = M.bounds(region)
  if not first_row then return nil, nil end
  if region.end_exclusive then
    if end_row <= first_row then return first_row, nil end
    return first_row, end_row - 1
  end
  return first_row, end_row
end

--- Reads the buffer lines bounded by the region and joins them into a single string.
---@param region DiffReviewRenderedRegion Target region record.
---@return string text Newline-joined buffer text content.
function M.read_text(region)
  local first_row, end_row = M.bounds(region)
  if not first_row then return "" end
  local after_row = region.end_exclusive and end_row or (end_row + 1)
  if after_row <= first_row then return "" end
  local lines = vim.api.nvim_buf_get_lines(region.buf, first_row, after_row, false)
  return table.concat(lines, "\n")
end

--- Reports whether current region text differs from the saved baseline string.
---@param region DiffReviewRenderedRegion Target region record.
---@return boolean dirty True if modified.
function M.is_dirty(region)
  return M.read_text(region) ~= (region.base_text or "")
end

--- Adopts current buffer content as the saved baseline after successful persistence.
---@param region DiffReviewRenderedRegion Target region record.
function M.mark_saved(region)
  region.base_text = M.read_text(region)
end

--- Explicitly overrides the saved baseline comparison text.
---@param region DiffReviewRenderedRegion Target region record.
---@param text string New baseline string to compare against.
function M.set_baseline(region, text)
  region.base_text = text
end

--- Deletes the start and end extmarks tracking the region.
---@param region DiffReviewRenderedRegion Target region record.
function M.clear(region)
  pcall(vim.api.nvim_buf_del_extmark, region.buf, region.namespace, region.start_anchor)
  pcall(vim.api.nvim_buf_del_extmark, region.buf, region.namespace, region.end_anchor)
end

return M
