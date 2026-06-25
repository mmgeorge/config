---@class DiffReviewRenderedRegion
---@field buf integer
---@field namespace integer
---@field start_anchor integer extmark id at the region's first row
---@field end_anchor integer extmark id at the region's last row
---@field region_kind string
---@field owner_id string?
---@field editable boolean
---@field base_text string?

---@class DiffReviewRegionModule
local M = {}

--- Track a buffer line range by extmarks so edits and splices move the region with its text.
--- Use right gravity on the end anchor so appending to the last line stays inside the region.
---@param buf integer
---@param namespace integer
---@param first_row integer 0-based inclusive
---@param last_row integer 0-based inclusive
---@param opts? { region_kind?: string, owner_id?: string, editable?: boolean }
---@return DiffReviewRenderedRegion
function M.new(buf, namespace, first_row, last_row, opts)
  opts = opts or {}
  first_row = math.max(0, math.floor(tonumber(first_row) or 0))
  last_row = math.max(first_row, math.floor(tonumber(last_row) or first_row))
  local start_anchor = vim.api.nvim_buf_set_extmark(buf, namespace, first_row, 0, { right_gravity = false })
  local end_anchor = vim.api.nvim_buf_set_extmark(buf, namespace, last_row, 0, { right_gravity = true })
  local region = {
    buf = buf,
    namespace = namespace,
    start_anchor = start_anchor,
    end_anchor = end_anchor,
    region_kind = opts.region_kind or "markdown",
    owner_id = opts.owner_id,
    editable = opts.editable == true,
    base_text = nil,
  }
  region.base_text = M.read_text(region)
  return region
end

--- Resolve the region's current 0-based inclusive row range from its anchors.
---@param region DiffReviewRenderedRegion
---@return integer? first_row
---@return integer? last_row
function M.range(region)
  local start_pos = vim.api.nvim_buf_get_extmark_by_id(region.buf, region.namespace, region.start_anchor, {})
  local end_pos = vim.api.nvim_buf_get_extmark_by_id(region.buf, region.namespace, region.end_anchor, {})
  if not (start_pos[1] and end_pos[1]) then return nil, nil end
  local first_row = start_pos[1]
  local last_row = math.max(end_pos[1], first_row)
  return first_row, last_row
end

--- Read the region's current buffer lines as a single newline-joined string.
---@param region DiffReviewRenderedRegion
---@return string
function M.read_text(region)
  local first_row, last_row = M.range(region)
  if not first_row then return "" end
  local lines = vim.api.nvim_buf_get_lines(region.buf, first_row, last_row + 1, false)
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

--- Remove the region's anchor extmarks.
---@param region DiffReviewRenderedRegion
function M.clear(region)
  pcall(vim.api.nvim_buf_del_extmark, region.buf, region.namespace, region.start_anchor)
  pcall(vim.api.nvim_buf_del_extmark, region.buf, region.namespace, region.end_anchor)
end

return M
