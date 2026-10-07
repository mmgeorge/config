local M = {}
---@type boolean
M.show_caller = false
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")
local preview_namespace = vim.api.nvim_create_namespace("forge.plan.references.preview")

---@class ForgePlanReferenceHighlight
---@field range table<string, { row: integer, column: integer }>
---@field capture string
---@field priority integer

---@class ForgePlanReference
---@field id string
---@field path string
---@field owner string
---@field owner_capture (string|userdata)[]
---@field name string
---@field kind string
---@field line integer
---@field text string
---@field text_highlight ForgePlanReferenceHighlight[]

--- Open snapshot-bound plan usages through the shared searchable picker.
---@param review table
---@param references ForgePlanReference[]
---@param captured table
function M.open(review, references, captured)
  local show_caller = M.show_caller
  local text_column = show_caller and 3 or 2
  local options = {}
  for _, reference in ipairs(references) do
    local caller_segments = {}
    local caller_parts = {}
    local caller = reference.owner ~= "" and reference.owner or reference.name
    for index, part in ipairs(show_caller and vim.split(caller, "::", { plain = true }) or {}) do
      if not part:match("^@impl:%d+$") then
        if #caller_parts > 0 then caller_segments[#caller_segments + 1] = { "::", "@punctuation.delimiter" } end
        caller_parts[#caller_parts + 1] = part
        local capture = reference.owner_capture and reference.owner_capture[index]
        caller_segments[#caller_segments + 1] = { part, type(capture) == "string" and capture or nil }
      end
    end
    caller = table.concat(caller_parts, "::")
    local filename = reference.path:match("[^/\\]+$") or reference.path
    local directory = reference.path:sub(1, #reference.path - #filename)
    local location = ("%s:%d"):format(reference.path, reference.line)
    local text = reference.text:gsub("^%s+", "")
    local leading_bytes = #reference.text - #text
    local text_spans = {}
    for _, highlight in ipairs(reference.text_highlight) do
      local first = math.max(0, highlight.range.start.column - leading_bytes)
      local last = highlight.range["end"].column - leading_bytes
      if first < last then
        text_spans[#text_spans + 1] = {
          first = first, last = last, group = highlight.capture, priority = highlight.priority,
        }
      end
    end
    options[#options + 1] = {
      id = reference.id,
      value = reference,
      columns = show_caller and { location, caller, text } or { location, text },
      column_segments = {
        { { directory, "ForgeDirName" }, { filename, "ForgeFileName" }, { ":" .. reference.line, "ForgePickerHint" } },
        show_caller and caller_segments or nil,
      },
      column_spans = { [text_column] = text_spans },
      label = show_caller and ("%s  %s  %s"):format(location, caller, text)
        or ("%s  %s"):format(location, text),
    }
  end
  local origin = vim.api.nvim_win_call(review.win, vim.fn.winsaveview)
  local origin_block, origin_position = captured.block, vim.deepcopy(captured.position)
  local closed, busy, confirming = false, false, false
  local preview_height = vim.api.nvim_win_get_height(review.win)
  ---@type ForgePlanReference?
  local pending
  ---@type string?
  local displayed
  local function clear()
    if vim.api.nvim_buf_is_valid(review.buf) then
      vim.api.nvim_buf_clear_namespace(review.buf, preview_namespace, 0, -1)
    end
  end
  local function center()
    vim.api.nvim_win_call(review.win, function()
      local scrolloff = vim.wo[review.win].scrolloff
      vim.wo[review.win].scrolloff = 0
      vim.cmd("normal! zz")
      local offset = vim.fn.winline() - math.max(1, math.floor(preview_height / 2))
      if offset > 0 then vim.cmd("normal! " .. offset .. "\5") end
      vim.wo[review.win].scrolloff = scrolloff
    end)
    review.owner.current_view(review.win).cursor = vim.api.nvim_win_get_cursor(review.win)
  end
  local function restore()
    if not review.owner.is_current(captured, false) or not vim.api.nvim_win_is_valid(review.win) then return end
    local _, row = review.owner.replica.sequence:position(origin_block)
    if not row then return end
    origin.lnum = require("forge.buffer").physical_row(review.owner.replica, row) + origin_position.row + 1
    origin.col = origin_position.column
    vim.api.nvim_win_call(review.win, function() vim.fn.winrestview(origin) end)
    review.owner.current_view(review.win).cursor = vim.api.nvim_win_get_cursor(review.win)
  end
  local function finish()
    local destination = vim.api.nvim_win_get_cursor(review.win)
    restore()
    vim.api.nvim_win_call(review.win, function() vim.cmd("normal! m'") end)
    vim.api.nvim_win_set_cursor(review.win, destination)
    review.owner.current_view(review.win).cursor = destination
    closed = true
    clear()
    picker.close(false)
    vim.api.nvim_set_current_win(review.win)
    vim.api.nvim_win_call(review.win, function() vim.cmd("normal! zz") end)
  end
  ---@type fun(reference: ForgePlanReference)
  local preview
  ---@param reference ForgePlanReference
  preview = function(reference)
    if closed then return end
    pending = reference
    if busy then clear() return end
    if not review.owner.is_current(captured, false) then
      notifications.warn("Plan references changed while the picker was open", "ForgePlanReview")
      picker.close(true)
      return
    end
    if displayed == reference.id then
      if confirming then finish() else center() end
      return
    end
    clear()
    busy = true
    local started = review.owner.action("reveal_reference:" .. reference.id, function(result, failure, input)
      busy = false
      if failure then
        notifications.error(failure, "ForgePlanReview")
        return
      end
      captured = input
      if closed then restore() return end
      if not review.owner.is_current(captured, false) then
        notifications.warn("Plan references changed while the picker was open", "ForgePlanReview")
        picker.close(true)
        return
      end
      if not pending or pending.id ~= reference.id then
        if pending then preview(pending) end
        return
      end
      local view = review.owner.current_view(review.win)
      local applied = require("forge.effects").apply(review.owner.replica, view, {
        id = "plan:reference:" .. captured.sequence, kind = "cursor", jump = false,
        document = captured.document, revision = captured.revision, view = captured.view,
        sequence = captured.sequence, block = result.jump.block, position = result.jump.position,
      })
      if applied ~= "Applied" then return end
      displayed = reference.id
      local cursor = vim.api.nvim_win_get_cursor(review.win)
      center()
      vim.api.nvim_buf_set_extmark(review.buf, preview_namespace, cursor[1] - 1, 0, {
        line_hl_group = "Visual", priority = 250,
      })
      if confirming then finish() end
    end, review.win)
    if not started then busy = false confirming = false end
  end
  picker.open({
    id = "plan_references",
    title = "Plan references",
    height_ratio = 0.3,
    host = { control_win = review.win, window_list = { review.win } },
    page_list = { { id = "references", title = "References", option_list = options,
      column_headers = show_caller and { "Location", "Caller", "Text" } or { "Location", "Text" },
      flexible_column = text_column,
      highlight_selected_line = true, highlight_selected_text = false,
      selection_mode = "single", show_item_counter = true, search = { start_in_normal = true } } },
    on_change = function(context)
      preview_height = context.preview_height
      if context.option then preview(context.option.value) else pending, displayed = nil, nil clear() end
    end,
    on_close = function()
      closed = true
      clear()
      if not busy then restore() end
    end,
    on_confirm = function(result)
      confirming = true
      preview(result.option.value)
      return false
    end,
  })
end

return M
