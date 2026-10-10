local M = {}
local comments = require("forge.draft_comments")
local editable = require("forge.editable")
local source_mapping = require("forge.draft_source")
local namespace = vim.api.nvim_create_namespace("ForgeReviewDraftField")

---@class ForgeReviewCommentAnchor
---@field revision string
---@field path string
---@field side 'left'|'right'
---@field first_line integer
---@field last_line integer

---@class ForgeReviewLocalComment
---@field region string
---@field comment? integer
---@field anchor? ForgeReviewCommentAnchor
---@field reply_to? integer
---@field text? string
---@field viewer_did_author boolean
---@field deleted? boolean
---@field local_draft? boolean

---@class ForgeReviewCommentOwner
---@field replica table
---@field window integer
---@field comment_by_region table<string, ForgeReviewLocalComment>
---@field inline_anchor table<string, ForgeReviewCommentAnchor>
---@field local_comment_view? ForgeDraftCommentState
---@field local_comment_sequence? integer
---@field comment_focus? ForgeReviewLocalComment

---@class ForgeReviewCommentPresentation
---@field snapshot table
---@field comment ForgeReviewLocalComment[]
---@field inline_anchor table<string, ForgeReviewCommentAnchor>

---@param state ForgeReviewCommentOwner
---@param delivery ForgeReviewCommentPresentation
function M.capture(state)
  if not state.local_comment_view then return nil end
  local source = state.local_comment_view.source_provider()
  local annotation = {}
  for _, captured in ipairs(comments.capture(state.replica.buffer)) do
    local comment = state.comment_by_region[captured.id]
    local field = state.replica.editable.region[captured.id]
    if comment.local_draft or field and field.pending then
      local identity
      for _, row in ipairs(source) do
        if row.source_line == captured.source.start_line then identity = row.id break end
      end
      assert(identity, "review comment source identity is missing")
      annotation[#annotation + 1] = { id = captured.id, source_id = identity,
        body = captured.source.body, comment = vim.deepcopy(comment) }
    end
  end
  return { annotation = annotation, sequence = state.replica.editable.sequence }
end

function M.attach(state, delivery, recovery)
  local replica = state.replica
  local source, annotation, inline_region = {}, {}, {}
  local snapshot_by_region = {}
  for _, comment in ipairs(delivery.comment or {}) do
    snapshot_by_region[comment.region] = comment
    if comment.anchor and comment.anchor ~= vim.NIL and not comment.deleted then
      for _, anchor in pairs(delivery.inline_anchor or {}) do
        if anchor.revision == comment.anchor.revision and anchor.path == comment.anchor.path
          and anchor.side == comment.anchor.side and anchor.last_line == comment.anchor.last_line then
          inline_region[comment.region] = comment
          break
        end
      end
    end
  end
  state.comment_by_region = snapshot_by_region
  state.inline_anchor = delivery.inline_anchor or {}
  local canonical = 0
  for _, block in ipairs(delivery.snapshot.block) do
    local region = block.id:match("^region:(.+)$") or block.id:match("^label:(.+)$")
    if not inline_region[region] then
      for offset, text in ipairs(block.text) do
        local target
        for _, candidate in ipairs(block.metadata.target or {}) do
          local span = candidate.range
          if span.start.row <= offset - 1 and (span["end"].row > offset - 1
            or span["end"].row == offset - 1 and span["end"].column > 0) then
            target = target or candidate.id
            if state.inline_anchor[candidate.id] then target = candidate.id break end
          end
        end
        source[#source + 1] = { id = block.id .. ":" .. offset, text = text,
          source_line = canonical + offset, canonical_row = canonical + offset - 1,
          block = block.id, position = { row = offset - 1, column = 0 }, target = target }
      end
    end
    canonical = canonical + #block.text
  end
  local projection = require("forge.node_projection")
  local retained_source = require("forge.buffer").fragment(delivery.snapshot, true)
  local node_owner = replica.projection or projection.new(retained_source)
  node_owner.source = retained_source
  replica.projection = node_owner
  local function project_source()
    local _, mapping = projection.render(node_owner)
    node_owner.mapping = mapping
    for _, row in ipairs(source) do row.hidden = not projection.contains(node_owner, row.block, row.position.row) end
    if state.local_comment_view then comments.update(replica.buffer, {}) end
    return true
  end
  state.local_comment_view = nil
  replica.project_source = project_source
  project_source()
  for region, comment in pairs(inline_region) do
    for _, row in ipairs(source) do
      local anchor = row.target and state.inline_anchor[row.target]
      if anchor and anchor.revision == comment.anchor.revision and anchor.path == comment.anchor.path
        and anchor.side == comment.anchor.side and anchor.last_line == comment.anchor.last_line then
        annotation[#annotation + 1] = { id = region, source_line = row.source_line,
          end_source_line = row.source_line, body = comment.text, readonly = not comment.viewer_did_author }
        break
      end
    end
  end
  table.sort(annotation, function(left, right) return left.id < right.id end)
  local baseline
  if recovery then
    baseline = {}
    for _, persisted in ipairs(annotation) do
      baseline[#baseline + 1] = { id = persisted.id, source = {
        start_line = persisted.source_line, end_line = persisted.end_source_line, body = persisted.body } }
    end
    for _, restored in ipairs(recovery.annotation) do
      local row
      for _, candidate in ipairs(source) do if candidate.id == restored.source_id then row = candidate break end end
      assert(row, "replacement review comment source is missing")
      local existing
      for _, candidate in ipairs(annotation) do if candidate.id == restored.id then existing = candidate break end end
      if existing then existing.body = restored.body
      else annotation[#annotation + 1] = { id = restored.id, source_line = row.source_line,
        end_source_line = row.source_line, body = restored.body,
        readonly = restored.comment.viewer_did_author == false } end
      state.comment_by_region[restored.id] = vim.deepcopy(restored.comment)
      if restored.comment.viewer_did_author ~= false then
        if not replica.editable.region[restored.id] then editable.register(replica.editable, restored.id, 0) end
        replica.editable.sequence = math.max(replica.editable.sequence, recovery.sequence or 0)
        editable.record(replica.editable, restored.id, vim.split(restored.body, "\n", { plain = true }))
      end
    end
  end
  local field_mark = {}
  local local_state
  local function before_render()
    editable.applying(replica.editable, true)
    vim.api.nvim_buf_clear_namespace(replica.buffer, namespace, 0, -1)
    field_mark = {}
    for region, bounds in pairs(replica.editable.native.anchor) do
      local comment = state.comment_by_region[region]
      local managed = false
      if local_state then
        for _, annotation in ipairs(local_state.annotation_list) do
          if annotation.id == region then managed = true break end
        end
      else managed = inline_region[region] ~= nil end
      if not managed and not (comment and comment.local_draft) then
        field_mark[region] = {
          vim.api.nvim_buf_set_extmark(replica.buffer, namespace, bounds.start.row, bounds.start.column, { right_gravity = false }),
          vim.api.nvim_buf_set_extmark(replica.buffer, namespace, bounds.finish.row, bounds.finish.column, { right_gravity = true }),
        }
      end
    end
    if local_state then
      local excluded, original = {}, {}
      for _, range in ipairs(local_state.range_list) do
        local first = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, local_state.namespace, range.header_mark, {})
        local last = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, local_state.namespace, range.footer_mark, {})
        for row = first[1], last[1] do excluded[row] = true end
      end
      for _, mark in ipairs(local_state.source_mark) do
        local position = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, local_state.namespace, mark.mark, {})
        original[position[1]] = source[mark.source_index]
      end
      local replacement, previous = {}, nil
      for index, text in ipairs(vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)) do
        if not excluded[index - 1] then
          local row = vim.deepcopy(original[index - 1] or previous)
          assert(row, "review source identity is missing")
          row.text = text
          replacement[#replacement + 1] = row
          previous = row
        end
      end
      local by_id, retained = {}, {}
      for _, row in ipairs(replacement) do
        by_id[row.id] = by_id[row.id] or {}
        by_id[row.id][#by_id[row.id] + 1] = row
      end
      for _, row in ipairs(source) do
        if row.hidden then retained[#retained + 1] = row
        elseif by_id[row.id] then
          vim.list_extend(retained, by_id[row.id])
          by_id[row.id] = nil
        end
      end
      for index = #source, 1, -1 do source[index] = nil end
      vim.list_extend(source, retained)
    end
  end
  local function after_render(_, _, projection)
    local anchor = {}
    local present = {}
    for _, range in ipairs(projection.range_list) do present[range.annotation.id] = true end
    for region, comment in pairs(state.comment_by_region) do
      if comment.local_draft and not present[region] then
        state.comment_by_region[region], replica.editable.region[region] = nil, nil
        if state.comment_focus == comment then state.comment_focus = nil end
      end
    end
    for region, mark in pairs(field_mark) do
      local first = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, mark[1], {})
      local last = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, namespace, mark[2], {})
      anchor[region] = { start = { row = first[1], column = first[2] }, finish = { row = last[1], column = last[2] } }
    end
    for _, range in ipairs(projection.range_list) do
      if not range.compact and not range.annotation.readonly then
        local region = range.annotation.id
        if not replica.editable.region[region] then
          editable.register(replica.editable, region, 0)
          editable.record(replica.editable, region, vim.split(range.annotation.body, "\n", { plain = true }))
        end
        local last = range.last_row - 1
        anchor[region] = { start = { row = range.first_row + 1, column = 0 },
          finish = { row = last, column = #projection.line_list[last + 1] } }
      end
    end
    editable.reanchor(replica.editable, anchor)
    replica.editable.suspended = false
    for _, field in pairs(replica.editable.region) do
      if field.pending then replica.editable.suspended = true end
    end
    editable.applying(replica.editable, false)
    if replica.editable.suspended then vim.bo[replica.buffer].modified = true end
  end
  local lines = {}
  for _, row in ipairs(source) do lines[#lines + 1] = row.text end
  local_state = comments.attach(replica.buffer, state.window, lines, annotation, {
    baseline = baseline,
    heading = "Comment", source_provider = function() return source end,
    source_label = function(annotation)
      local anchor = state.comment_by_region[annotation.id].anchor
      if not anchor or anchor == vim.NIL then return "Conversation" end
      if anchor.first_line == anchor.last_line then return "line " .. anchor.last_line end
      return ("lines %d-%d"):format(anchor.first_line, anchor.last_line)
    end,
    before_render = before_render, after_render = after_render,
    editable_source = function(row)
      for _, bounds in pairs(replica.editable.native.anchor) do
        if row >= bounds.start.row and row <= bounds.finish.row then return true end
      end
      return false
    end,
  })
  state.local_comment_view = local_state
  replica.local_projection = true
  source_mapping.attach(replica, local_state, source)
  if recovery and editable.suspend_generated_text(replica.editable) then vim.bo[replica.buffer].modified = true end
end

---@param state ForgeReviewCommentOwner
---@return boolean
function M.delete(state)
  local comment = state.comment_focus
  if not state.local_comment_view or not comment or not comment.local_draft then return false end
  comments.delete_at_cursor(state.replica.buffer)
  return state.comment_by_region[comment.region] == nil
end

---@param state ForgeReviewCommentOwner
---@return ForgeReviewLocalComment?
function M.selected(state)
  local local_state = state.local_comment_view
  if not local_state or not vim.api.nvim_win_is_valid(state.window) then return nil end
  local row = vim.api.nvim_win_get_cursor(state.window)[1] - 1
  for _, range in ipairs(local_state.range_list) do
    local first = vim.api.nvim_buf_get_extmark_by_id(state.replica.buffer, local_state.namespace, range.header_mark, {})
    local last = vim.api.nvim_buf_get_extmark_by_id(state.replica.buffer, local_state.namespace, range.footer_mark, {})
    if #first > 0 and #last > 0 and row >= first[1] and row <= last[1] then
      return state.comment_by_region[range.annotation.id]
    end
  end
  return nil
end

---@param state ForgeReviewCommentOwner
function M.detach(state)
  if not state.local_comment_view then return end
  editable.applying(state.replica.editable, true)
  comments.detach(state.replica.buffer, false)
  editable.applying(state.replica.editable, false)
  for _, name in ipairs({ "locate", "physical_row", "prepare_source", "decoration_location", "fold_location", "project_source" }) do
    state.replica[name] = nil
  end
  state.local_comment_view = nil
  state.replica.local_projection = nil
end

---@param state ForgeReviewCommentOwner
---@param parent? ForgeReviewLocalComment
---@return boolean
function M.add(state, parent)
  if not state.local_comment_view then return false end
  local cursor = vim.api.nvim_win_get_cursor(state.window)
  local location = state.replica.locate(cursor[1] - 1, cursor[2])
  local anchor = parent and parent.anchor or location and location.target and state.inline_anchor[location.target]
  if parent and (parent.local_draft or not parent.comment or not anchor or anchor == vim.NIL) then return false end
  if parent then
    for region, draft in pairs(state.comment_by_region) do
      if draft.local_draft and draft.reply_to == parent.comment then
        state.comment_focus = draft
        return comments.focus(state.replica.buffer, region)
      end
    end
  end
  state.local_comment_sequence = (state.local_comment_sequence or 0) + 1
  local region = ("draft-comment/%s/%d/body"):format(tostring(vim.uv.hrtime()), state.local_comment_sequence)
  local line = comments.source_line_at_cursor(state.replica.buffer)
  if parent then
    for _, annotation in ipairs(state.local_comment_view.annotation_list) do
      if annotation.id == parent.region then line = annotation.end_source_line break end
    end
  end
  local comment = { region = region, local_draft = true, anchor = anchor and vim.deepcopy(anchor) or nil,
    reply_to = parent and parent.comment or nil, viewer_did_author = true }
  state.comment_by_region[region], state.comment_focus = comment, comment
  comments.add(state.replica.buffer, { id = region, source_line = line, end_source_line = line, body = "" })
  return true
end

return M
