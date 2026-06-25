local text_snapshot = require("diff_review.render.text_snapshot")

---@alias DiffReviewSyntaxSide "old"|"new"

---@class DiffReviewFileSyntaxSide
---@field snapshot DiffReviewTextSnapshot?
---@field revision string?
---@field parser any?
---@field tree any?
---@field query vim.treesitter.Query?
---@field source integer|string?
---@field pending boolean
---@field error string?

---@class DiffReviewFileSyntaxContext
---@field file_key string
---@field side_by_name table<DiffReviewSyntaxSide, DiffReviewFileSyntaxSide>
---@field revision integer

---@class DiffReviewSyntaxContextModule
local M = {}

---@param file_key string
---@return DiffReviewFileSyntaxContext
function M.new(file_key)
  return {
    file_key = tostring(file_key or ""),
    side_by_name = {
      old = { pending = false },
      new = { pending = false },
    },
    revision = 0,
  }
end

---@param context DiffReviewFileSyntaxContext?
---@param file_key string
---@return DiffReviewFileSyntaxContext
function M.ensure_context(context, file_key)
  if context then return context end
  return M.new(file_key)
end

---@param context DiffReviewFileSyntaxContext
---@param side DiffReviewSyntaxSide
---@param snapshot DiffReviewTextSnapshot?
---@param revision string?
function M.set_snapshot(context, side, snapshot, revision)
  local state = context.side_by_name[side]
  if not state then return end
  state.snapshot = snapshot
  state.revision = revision
  state.parser = nil
  state.tree = nil
  state.pending = false
  state.error = nil
  context.revision = (context.revision or 0) + 1
end

---@param context DiffReviewFileSyntaxContext
---@param side DiffReviewSyntaxSide
---@return boolean
function M.has_tree(context, side)
  local state = context and context.side_by_name and context.side_by_name[side] or nil
  return state and state.tree ~= nil or false
end

---@param context DiffReviewFileSyntaxContext
---@param side DiffReviewSyntaxSide
---@return any?
function M.tree(context, side)
  local state = context and context.side_by_name and context.side_by_name[side] or nil
  return state and state.tree or nil
end

---@param context DiffReviewFileSyntaxContext
---@param side DiffReviewSyntaxSide
---@param parser any
---@param done? fun(ok: boolean, tree?: any, err?: string)
function M.parse_async(context, side, parser, done)
  local state = context.side_by_name[side]
  if not state then
    if done then done(false, nil, "unknown syntax side") end
    return
  end
  if state.tree then
    if done then done(true, state.tree) end
    return
  end
  if state.pending then
    if done then done(false, nil, "syntax parse already pending") end
    return
  end
  if not parser then
    state.error = "missing parser"
    if done then done(false, nil, state.error) end
    return
  end
  state.parser = parser
  state.pending = true
  local ok = pcall(function()
    parser:parse(nil, function(first, second)
      local tree_list = type(first) == "table" and first or second
      vim.schedule(function()
        state.pending = false
        state.tree = tree_list and tree_list[1] or nil
        state.error = state.tree and nil or "parser returned no tree"
        if done then done(state.tree ~= nil, state.tree, state.error) end
      end)
    end)
  end)
  if not ok then
    state.pending = false
    state.error = "syntax parse failed"
    if done then done(false, nil, state.error) end
  end
end

--- Attach a parsed tree plus the highlight query and the source it was parsed from.
--- The decoration provider needs the source (bufnr or text) to resolve captures.
---@param context DiffReviewFileSyntaxContext
---@param side DiffReviewSyntaxSide
---@param tree any
---@param query vim.treesitter.Query?
---@param source integer|string
function M.set_tree(context, side, tree, query, source)
  local state = context and context.side_by_name and context.side_by_name[side] or nil
  if not state then return end
  state.tree = tree
  state.query = query
  state.source = source
  state.pending = false
  state.error = nil
end

--- Collect highlight column ranges for one source row from the parsed tree.
--- Clamp ranges to the line so a multi-row capture only covers its slice of this row.
---@param query vim.treesitter.Query
---@param root TSNode
---@param source integer|string
---@param row integer 0-based
---@param line_len integer
---@return table[] list of { col_start, col_end, hl_group }
local function row_highlight_ranges(query, root, source, row, line_len)
  local ranges = {}
  pcall(function()
    for id, node in query:iter_captures(root, source, row, row + 1) do
      local capture = query.captures[id]
      if capture and capture ~= "" then
        local start_row, start_col, end_row, end_col = node:range()
        if start_row <= row and end_row >= row then
          local range_start = math.max(start_row == row and start_col or 0, 0)
          local range_end = math.min(end_row == row and end_col or line_len, line_len)
          if range_end > range_start then
            ranges[#ranges + 1] = { range_start, range_end, "@" .. capture }
          end
        end
      end
    end
  end)
  return ranges
end

--- Resolve highlight spans for a 0-based inclusive source row range, for the decoration provider.
--- Return nil when the side's tree, query, or source is not ready so the caller emits a fallback and retries.
---@param context DiffReviewFileSyntaxContext
---@param side DiffReviewSyntaxSide
---@param first_row integer 0-based inclusive
---@param last_row integer 0-based inclusive
---@return table<integer, { highlights: table[] }>?
function M.highlights(context, side, first_row, last_row)
  local state = context and context.side_by_name and context.side_by_name[side] or nil
  if not (state and state.tree and state.query and state.source ~= nil) then return nil end
  local root = state.tree:root()
  local snapshot = state.snapshot
  first_row = math.max(0, math.floor(tonumber(first_row) or 0))
  last_row = math.max(first_row, math.floor(tonumber(last_row) or first_row))
  local result = {}
  for row = first_row, last_row do
    local line_text = snapshot and text_snapshot.line_text(snapshot, row + 1) or ""
    result[row] = { highlights = row_highlight_ranges(state.query, root, state.source, row, #line_text) }
  end
  return result
end

return M
