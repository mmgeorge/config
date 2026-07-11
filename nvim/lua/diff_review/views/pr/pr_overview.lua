--- Renders the GitHub PR overview: header metadata, checks, reviews, and inline review
--- comments, assembling sections through section_builder and the status render core.
---@class DiffReviewPrOverviewModule
local M = {}

--- Number of context lines kept around a change when rendering inline review comments.
M.review_context_radius = 2

local status_buffer = require("diff_review.views.status.status_buffer")
local notifications = require("diff_review.infra.notifications")
local gh = require("diff_review.integrations.gh")

-- status_render edge kept lazy to avoid a load-time cycle.
local function status_render() return require("diff_review.views.status.status_render") end
-- render_orchestrator edge kept lazy to avoid a load-time cycle.
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
local comment_rows = require("github.comment_rows")
-- review edge kept lazy to avoid a load-time cycle.
local function review_mod() return require("diff_review.views.pr.review") end
local pr_edit = require("diff_review.views.pr.pr_edit")
-- section_builder edge kept lazy to avoid a load-time cycle.
local function section_builder() return require("diff_review.views.status.section_builder") end
local status_keys = require("diff_review.views.status.status_keys")
local status_head = require("diff_review.views.status.status_head")
local entry_nav = require("diff_review.views.status.entry_nav")
-- fold_state edge kept lazy to avoid a load-time cycle.
local function fold_state() return require("diff_review.views.status.fold_state") end
local datetime = require("diff_review.integrations.datetime")
local ui = require("diff_review.infra.ui")
local session = require("diff_review.session")

-- Render-core shims: thread the active status into the state-passing buffer core so the
-- view's accumulator calls render into the live buffer unchanged.
local function status_add_line(text, entry, hl)
  return status_buffer.add_line(session.status, text, entry, hl)
end
local function status_add_highlight(line, start_col, end_col, hl_group, priority)
  return status_buffer.add_highlight(session.status, line, start_col, end_col, hl_group, priority)
end
local function status_add_segment_line(segments, entry)
  return status_buffer.add_segment_line(session.status, segments, entry)
end
local function status_folded(key, default, state)
  return status_buffer.folded(state or session.status or {}, key, default)
end
local function set_status_folded(key, folded, state)
  if not state then
    session.status = session.status or {}
    state = session.status
  end
  return status_buffer.set_folded(state, key, folded)
end
local function notify_error(message, title)
  return notifications.error(message, title)
end

-- Seams to init-owned helpers the overview shares (diff parsing, provider keys, cursor
-- lookup, render orchestrators).
local diff_parse = require("diff_review.render.diff_parse")
local parse_hunk_header = diff_parse.parse_hunk_header
local function status_short_oid(...) return status_head._status_short_oid(...) end
local function status_provider_file_key(...) return status_keys.provider_file_key(...) end
local function status_provider_hunk_key(...) return status_keys.provider_hunk_key(...) end
local function status_entry_under_cursor(...) return entry_nav._status_entry_under_cursor(...) end
local function status_render_file(...) return status_render().status_render_file(...) end
local function render_pr_status(...) return render_orchestrator().render_pr_status(...) end

---@param value any
---@return string
function M.comment_datetime(value)
  return datetime.relative(value)
end

---@param body string
---@return string
function M.comment_preview(body)
  return comment_rows.preview_text(body)
end

---@param comment table
---@return string
function M.comment_author(comment)
  return comment_rows.author(comment)
end

---@param text any
---@param width integer?
---@return string
function M.pad_display_right(text, width)
  return comment_rows.pad_right(text, width)
end

---@return GithubCommentRowsOpts
function M.issue_comment_row_options()
  return {
    comment_icon = ui.comment_icon,
    relative_date = M.comment_datetime,
    entry_id = M.issue_comment_entry_id,
    preview_width = function(prefix)
      return math.max(0, review_mod().comment_rule_width(nil, nil) - vim.fn.strdisplaywidth(prefix))
    end,
    truncate_preview = review_mod().truncate_preview_text,
    body_lines = review_mod().comment_body_lines,
    kind = "pr_comment",
    line_hl_group = "DiffReviewReviewComment",
    body_hl_group = "DiffReviewReviewComment",
    date_hl_group = "DiffReviewStatusDate",
  }
end

---@param comment table
---@return string
function M.issue_comment_date(comment)
  return comment_rows.date(comment, M.issue_comment_row_options())
end

---@param comments table[]?
---@return { author_width: integer, date_width: integer }
function M.issue_comment_alignment(comments)
  return comment_rows.alignment(comments, M.issue_comment_row_options())
end

---@param comment table
---@param alignment? { author_width?: integer, date_width?: integer }
---@param has_preview? boolean
---@return string
function M.issue_comment_prefix(comment, alignment, has_preview)
  return comment_rows.prefix(comment, alignment, has_preview, M.issue_comment_row_options())
end

---@param comment table
---@param expanded? boolean
---@param alignment? { author_width?: integer, date_width?: integer }
---@return string
function M.issue_comment_line(comment, expanded, alignment)
  return comment_rows.line(comment, expanded, alignment, M.issue_comment_row_options())
end

---@param comment table?
---@param index integer
---@return string
function M.issue_comment_entry_id(comment, index)
  local key = M.comment_identity_key(comment)
  if key then return "pr-issue-comment:" .. key end
  local local_id = comment and comment.local_id or nil
  if local_id and local_id ~= "" then return "pr-issue-comment:local:" .. tostring(local_id) end
  return "pr-issue-comment:" .. tostring(index)
end

---@param pr DiffReviewGhPR
---@param status table?
---@return string
function M.activity_text(pr, status)
  local latest_epoch = nil
  local latest_value = nil

  local function consider(value)
    local epoch = datetime.parse(value)
    if epoch and (not latest_epoch or epoch > latest_epoch) then
      latest_epoch = epoch
      latest_value = value
    end
  end

  local function consider_comment(comment)
    if type(comment) ~= "table" then return end
    consider(comment.updated_at or comment.updatedAt or comment.created_at or comment.createdAt)
    for _, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
      if type(reply) == "table" then consider(reply.updated_at or reply.updatedAt or reply.created_at or reply.createdAt) end
    end
  end

  for _, commit in ipairs(type(pr.commits) == "table" and pr.commits or {}) do
    if type(commit) == "table" then
      consider(commit.committedDate or commit.committed_date or commit.committed_at or commit.authoredDate or commit.authored_date or commit.authored_at)
    end
  end

  local comments = status and status.pr_comments or nil
  for _, review in ipairs(type(comments and comments.reviews) == "table" and comments.reviews or {}) do
    if type(review) == "table" then
      consider(review.submitted_at or review.submittedAt or review.updated_at or review.updatedAt or review.created_at or review.createdAt)
      for _, comment in ipairs(type(review.comments) == "table" and review.comments or {}) do
        consider_comment(comment)
      end
    end
  end
  for _, comment in ipairs(type(comments and comments.code_comments) == "table" and comments.code_comments or {}) do
    consider_comment(comment)
  end
  for _, comment in ipairs(type(comments and comments.issue_comments) == "table" and comments.issue_comments or {}) do
    consider_comment(comment)
  end
  for _, comment in ipairs(status and status.pr_standalone_comments or {}) do
    consider_comment(comment)
  end
  for _, comment in ipairs(status and status.pr_regular_comments or {}) do
    consider_comment(comment)
  end

  return latest_value and datetime.relative(latest_value) or ""
end

---@param username string
---@return string
function M.reviewer_token(username)
  return "@" .. tostring(username or ""):gsub("^@", "")
end

---@param reviewer any
---@return string
function M.reviewer_login(reviewer)
  if type(reviewer) == "table" then
    return (tostring(reviewer.login or reviewer.slug or reviewer.name or ""):gsub("^@", ""))
  end
  return (tostring(reviewer or ""):gsub("^@", ""))
end

---@param reviewer any
---@return boolean
function M.reviewer_is_code_owner(reviewer)
  return type(reviewer) == "table" and reviewer.is_code_owner == true
end

---@param login string
---@param is_code_owner boolean?
---@return DiffReviewGhRequestedReviewer
function M.reviewer_entry(login, is_code_owner)
  return {
    login = tostring(login or ""):gsub("^@", ""),
    is_code_owner = is_code_owner or nil,
  }
end

---@param reviewers any[]?
---@param out DiffReviewGhRequestedReviewer[]
---@param seen table<string, boolean>
function M.add_reviewers(reviewers, out, seen)
  for _, reviewer in ipairs(reviewers or {}) do
    local username = M.reviewer_login(reviewer)
    local key = username:lower()
    if username ~= "" and not seen[key] then
      seen[key] = true
      out[#out + 1] = M.reviewer_entry(username, M.reviewer_is_code_owner(reviewer))
    elseif username ~= "" and M.reviewer_is_code_owner(reviewer) then
      for _, existing in ipairs(out) do
        if M.reviewer_login(existing):lower() == key then
          existing.is_code_owner = true
          break
        end
      end
    end
  end
end

---@param pr DiffReviewGhPR
---@param status table?
---@return DiffReviewGhRequestedReviewer[]
function M.pending_reviewers(pr, status)
  local reviewers = {}
  local seen = {}
  M.add_reviewers(pr and pr.requestedReviewers or nil, reviewers, seen)
  local edit_state = status and status.pr_edit or nil
  M.add_reviewers(edit_state and edit_state.pending_reviewers or nil, reviewers, seen)
  return reviewers
end

---@param pr DiffReviewGhPR
---@param status table?
---@return string
function M.pending_review_text(pr, status)
  local reviewers = M.pending_reviewers(pr, status)
  if #reviewers == 0 then return "" end
  local tokens = { ui.pending_review_icon }
  for _, reviewer in ipairs(reviewers) do
    local token = M.reviewer_token(M.reviewer_login(reviewer))
    if pr and pr.isDraft and M.reviewer_is_code_owner(reviewer) then
      token = ui.codeowner_review_icon .. token
    end
    tokens[#tokens + 1] = token
  end
  return table.concat(tokens, " ")
end

---@param pr DiffReviewGhPR?
---@return string
function M.milestone_title(pr)
  local milestone = pr and pr.milestone or nil
  if type(milestone) == "table" then return vim.trim(tostring(milestone.title or "")) end
  if type(milestone) == "string" then return vim.trim(milestone) end
  return ""
end

---@param pr DiffReviewGhPR?
---@return string
function M.milestone_text(pr)
  local title = M.milestone_title(pr)
  if title == "" then return "" end
  return ("%s %s"):format(ui.milestone_icon, title)
end

---@param pr DiffReviewGhPR?
---@return string
function M.status_text(pr)
  local state = tostring(pr and pr.state or "OPEN"):upper()
  if state == "CLOSED" then return "CLOSED" end
  if state == "MERGED" then return "MERGED" end
  return pr and pr.isDraft and "DRAFT" or "OPEN"
end

---@param pr DiffReviewGhPR?
---@return string
function M.status_highlight(pr)
  local status = M.status_text(pr)
  if status == "CLOSED" or status == "MERGED" then return "DiffReviewStatusClosed" end
  if status == "OPEN" then return "DiffReviewStatusOpen" end
  return "DiffReviewStatusFetching"
end

---@param check DiffReviewGhPRCheck
---@return string icon
---@return string hl_group
function M.check_icon(check)
  local state = tostring(check and check.state or ""):lower()
  if state == "success" then return "✓", "DiffReviewAddRange" end
  if state == "failure" or state == "error" or state == "timed_out" or state == "startup_failure" or state == "action_required" then
    return "✗", "DiffReviewDeleteRange"
  end
  if state == "cancelled" or state == "skipped" or state == "neutral" then return "!", "DiffReviewStatusFetching" end
  return "◷", "DiffReviewStatusFetching"
end

---@param check DiffReviewGhPRCheck
---@return string
function M.check_workflow_suffix(check)
  local workflow_name = vim.trim(tostring(check and check.workflow_name or ""))
  local name = vim.trim(tostring(check and check.name or ""))
  if workflow_name == "" or workflow_name == name then return "" end
  return workflow_name
end

---@param pr DiffReviewGhPR
---@param status table?
---@return DiffReviewStatusHeadLine[]
function M.check_head_lines(pr, status)
  local section_id = "pr-head-section:checks"
  local lines = {}
  lines[#lines + 1] = { segments = { { "" } } }
  lines[#lines + 1] = {
    segments = { { "Checks:", "DiffReviewStatusHeader" } },
    entry = { id = section_id, kind = "pr_head_section", default_folded = false },
  }
  if status_folded(section_id, false, status) then return lines end
  if not (pr.repo and pr.repo ~= "") then
    lines[#lines + 1] = {
      segments = { { "No checks", "Comment" } },
      parent_id = section_id,
      entry = { id = section_id .. ":message", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  if status and status.pr_checks_error then
    lines[#lines + 1] = {
      segments = { { tostring(status.pr_checks_error), "ErrorMsg" } },
      parent_id = section_id,
      entry = { id = section_id .. ":error", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  if status and status.pr_checks_loading then
    lines[#lines + 1] = {
      segments = { { "...loading checks...", "DiffReviewStatusFetching" } },
      parent_id = section_id,
      entry = { id = section_id .. ":loading", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  local checks = status and status.pr_checks or nil
  if type(checks) ~= "table" then
    lines[#lines + 1] = {
      segments = { { "...loading checks...", "DiffReviewStatusFetching" } },
      parent_id = section_id,
      entry = { id = section_id .. ":loading", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  if #checks == 0 then
    lines[#lines + 1] = {
      segments = { { "No checks", "Comment" } },
      parent_id = section_id,
      entry = { id = section_id .. ":message", kind = "pr_head_line", fold_target_id = section_id },
    }
    return lines
  end
  for check_index, check in ipairs(checks) do
    local icon, icon_hl = M.check_icon(check)
    local workflow_suffix = M.check_workflow_suffix(check)
    local segments = {
      { icon .. " ", icon_hl },
      { tostring(check.name or "Check"), "DiffReviewStatusPath" },
    }
    if workflow_suffix ~= "" then
      segments[#segments + 1] = { " | ", "Comment" }
      segments[#segments + 1] = { workflow_suffix, "DiffReviewStatusRemote" }
    end
    lines[#lines + 1] = {
      segments = segments,
      parent_id = section_id,
      entry = {
        id = "pr:check:" .. tostring(check_index) .. ":" .. tostring(check.name or "check"),
        kind = "pr_check",
        pr_check = check,
        fold_target_id = section_id,
      },
    }
  end
  return lines
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M.review_key(review)
  local id = tostring(review.node_id or "")
  if id == "" then id = tostring(review.id or "") end
  if id == "" then id = vim.fn.sha256(tostring(review.user or "") .. tostring(review.submitted_at or "") .. tostring(review.body or "")) end
  return "pr-review:" .. id
end

---@param state string?
---@return string icon
---@return string status_hl
function M.review_state_style(state)
  state = tostring(state or "")
  if state == "APPROVED" then return "✓", "DiffReviewAddRange" end
  if state == "CHANGES_REQUESTED" then return "✗", "DiffReviewDeleteRange" end
  return ui.comment_icon, "DiffReviewReviewComment"
end

---@param state string?
---@return string
function M.review_action(state)
  state = tostring(state or "")
  if state == "APPROVED" then return "approved" end
  if state == "CHANGES_REQUESTED" then return "requested changes" end
  if state == "DISMISSED" then return "dismissed" end
  if state == "COMMENTED" then return "commented" end
  return "reviewed"
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M.review_author(review)
  return tostring(review.user or "unknown")
end

---@param review DiffReviewGhSubmittedReview
---@return string
function M.review_date(review)
  return M.comment_datetime(review.submitted_at or review.updated_at or review.created_at)
end

---@param reviews DiffReviewGhSubmittedReview[]?
---@return { author_width: integer, date_width: integer }
function M.review_summary_alignment(reviews)
  local alignment = { author_width = 0, date_width = 0 }
  for _, review in ipairs(reviews or {}) do
    alignment.author_width = math.max(alignment.author_width, vim.fn.strdisplaywidth(M.review_author(review)))
    alignment.date_width = math.max(alignment.date_width, vim.fn.strdisplaywidth(M.review_date(review)))
  end
  return alignment
end

---@param review DiffReviewGhSubmittedReview
---@return string[] tail_parts
function M.review_summary_tail_parts(review)
  local tail_parts = {}
  local preview = M.comment_preview(review.body or "")
  if preview ~= "" then tail_parts[#tail_parts + 1] = preview end
  return tail_parts
end

---@param review DiffReviewGhSubmittedReview
---@param alignment? { author_width?: integer, date_width?: integer }
---@return string
function M.review_summary_line(review, alignment)
  local icon = M.review_state_style(review.state)
  alignment = alignment or {}
  local tail_parts = M.review_summary_tail_parts(review)
  local line = ("%s %s"):format(
    icon,
    M.pad_display_right(M.review_author(review), alignment.author_width)
  )
  local date_text = M.review_date(review)
  if date_text ~= "" then
    line = line .. " " .. M.pad_display_right(date_text, #tail_parts > 0 and alignment.date_width or 0)
  end
  if #tail_parts > 0 then line = line .. "  " .. table.concat(tail_parts, "  ") end
  return (line:gsub("%s+$", ""))
end

---@param review DiffReviewGhSubmittedReview
---@param alignment? { author_width?: integer, date_width?: integer }
---@return table[]
function M.review_summary_segments(review, alignment)
  local icon, status_hl = M.review_state_style(review.state)
  alignment = alignment or {}
  local tail_parts = M.review_summary_tail_parts(review)
  local segments = {
    { icon .. " ", status_hl },
    {
      M.pad_display_right(M.review_author(review), alignment.author_width),
      "DiffReviewReviewComment",
    },
  }
  local date_text = M.review_date(review)
  if date_text ~= "" then
    segments[#segments + 1] = { " ", "DiffReviewReviewComment" }
    segments[#segments + 1] = {
      M.pad_display_right(date_text, #tail_parts > 0 and alignment.date_width or 0),
      "DiffReviewStatusDate",
    }
  end
  for _, tail in ipairs(tail_parts) do
    segments[#segments + 1] = { "  ", "DiffReviewReviewComment" }
    segments[#segments + 1] = { tail, "DiffReviewReviewComment" }
  end
  return segments
end

---@param comments? DiffReviewGhPRCommentsResult
---@return DiffReviewStatusSection?
function M.reviews_section(comments)
  local reviews = comments and comments.reviews or nil
  if type(reviews) ~= "table" or #reviews == 0 then return nil end
  local visible_reviews = {}
  for _, review in ipairs(reviews) do
    local is_empty_single_comment_shell = tostring(review.state or "") == "COMMENTED"
      and vim.trim(tostring(review.body or "")) == ""
      and #(review.comments or {}) <= 1
    if not is_empty_single_comment_shell then visible_reviews[#visible_reviews + 1] = review end
  end
  if #visible_reviews == 0 then return nil end
  return {
    name = "pr:reviews",
    title = "Reviews",
    default_folded = false,
    files = {},
    files_by_name = {},
    reviews = visible_reviews,
  }
end

---@param comments? DiffReviewGhPRCommentsResult
---@param local_comments? table[]
---@return DiffReviewStatusSection?
function M.issue_comments_section(comments, local_comments)
  local remote_comments = comments and comments.issue_comments or nil
  if type(remote_comments) ~= "table" then remote_comments = {} end
  local local_keys = {}
  for _, comment in ipairs(local_comments or {}) do
    local key = M.comment_identity_key(comment)
    if key then local_keys[key] = true end
  end

  local issue_comments = {}
  for _, comment in ipairs(remote_comments) do
    local key = M.comment_identity_key(comment)
    if not (key and local_keys[key]) then issue_comments[#issue_comments + 1] = comment end
  end
  for _, comment in ipairs(local_comments or {}) do
    issue_comments[#issue_comments + 1] = comment
  end
  if #issue_comments == 0 then return nil end

  return {
    name = "pr:comments",
    title = "Comments",
    default_folded = false,
    files = {},
    files_by_name = {},
    issue_comments = issue_comments,
  }
end

---@param comment table
---@return "LEFT"|"RIGHT"
function M.comment_side(comment)
  return section_builder().comment_side(comment)
end

---@param hunk DiffReviewHunk
---@param comment table
---@return boolean
function M.hunk_contains_comment(hunk, comment)
  local target_line = tonumber(comment and comment.line)
  if not target_line then return false end
  local start_line = tonumber(comment.start_line) or target_line
  if start_line > target_line then start_line, target_line = target_line, start_line end
  local wanted_side = M.comment_side(comment)
  local old_line, new_line
  for diff_line in tostring(hunk.diff or ""):gmatch("[^\n]+") do
    local old_start, new_start = diff_line:match("^@@ %-(%d+),?%d* %+(%d+),?%d* @@")
    if old_start and new_start then
      old_line = tonumber(old_start)
      new_line = tonumber(new_start)
    elseif old_line and new_line then
      local prefix = diff_line:sub(1, 1)
      if prefix == " " then
        if wanted_side == "LEFT" and old_line >= start_line and old_line <= target_line then return true end
        if wanted_side == "RIGHT" and new_line >= start_line and new_line <= target_line then return true end
        old_line = old_line + 1
        new_line = new_line + 1
      elseif prefix == "-" then
        if wanted_side == "LEFT" and old_line >= start_line and old_line <= target_line then return true end
        old_line = old_line + 1
      elseif prefix == "+" then
        if wanted_side == "RIGHT" and new_line >= start_line and new_line <= target_line then return true end
        new_line = new_line + 1
      end
    end
  end
  return false
end

---@param hunk DiffReviewHunk
---@return DiffReviewReviewContextHunk?
function M.review_context_hunk(hunk)
  local header = nil
  local body = {}
  for _, line in ipairs(vim.split(tostring(hunk.diff or ""), "\n", { plain = true })) do
    if not header and line:match("^@@ ") then
      header = line
    elseif header then
      body[#body + 1] = line
    end
  end
  if not header then return nil end

  local old_line, _, new_line, _, context = parse_hunk_header(header)
  local records = {}
  for position, raw in ipairs(body) do
    local prefix = raw:sub(1, 1)
    if prefix == " " or prefix == "-" or prefix == "+" then
      local record = {
        raw = raw,
        prefix = prefix,
        old_before = old_line,
        new_before = new_line,
        position = position,
      }
      if prefix == " " then
        record.old_line = old_line
        record.new_line = new_line
        old_line = old_line + 1
        new_line = new_line + 1
      elseif prefix == "-" then
        record.old_line = old_line
        old_line = old_line + 1
      elseif prefix == "+" then
        record.new_line = new_line
        new_line = new_line + 1
      end
      records[#records + 1] = record
    end
  end
  return { context = context, records = records }
end

---@param context_hunk DiffReviewReviewContextHunk
---@param comment table
---@param radius integer
---@return table?
function M.review_comment_context_window(context_hunk, comment, radius)
  local target_line = tonumber(comment and comment.line)
  if not target_line then return nil end
  local start_line = tonumber(comment.start_line) or target_line
  if start_line > target_line then start_line, target_line = target_line, start_line end
  start_line = math.max(0, start_line - radius)
  target_line = target_line + radius

  local side = M.comment_side(comment)
  local start_index, end_index
  for index, record in ipairs(context_hunk.records or {}) do
    local line = side == "LEFT" and record.old_line or record.new_line
    if line and line >= start_line and line <= target_line then
      start_index = start_index or index
      end_index = index
    end
  end
  if not start_index and comment.position then
    local position = tonumber(comment.position)
    for index, record in ipairs(context_hunk.records or {}) do
      if position and record.position == position then
        start_index = math.max(1, index - radius)
        end_index = math.min(#context_hunk.records, index + radius)
        break
      end
    end
  end
  if not start_index then return nil end
  return { start_index = start_index, end_index = end_index }
end

---@param windows table[]
---@return table[]
function M.merge_context_windows(windows)
  table.sort(windows, function(left, right)
    return (left.start_index or 0) < (right.start_index or 0)
  end)
  local merged = {}
  for _, window in ipairs(windows) do
    local previous = merged[#merged]
    if previous and (window.start_index or 0) <= (previous.end_index or 0) + 1 then
      previous.end_index = math.max(previous.end_index or 0, window.end_index or 0)
    else
      merged[#merged + 1] = {
        start_index = window.start_index,
        end_index = window.end_index,
      }
    end
  end
  return merged
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param context_hunk DiffReviewReviewContextHunk
---@param window table
---@return DiffReviewHunk?
function M.context_window_hunk(file, hunk, context_hunk, window)
  local records = context_hunk.records or {}
  local first = records[window.start_index]
  if not first then return nil end

  local old_count, new_count = 0, 0
  local added, removed = 0, 0
  local body = {}
  for index = window.start_index, window.end_index do
    local record = records[index]
    if record then
      body[#body + 1] = record.raw
      if record.prefix ~= "+" then old_count = old_count + 1 end
      if record.prefix ~= "-" then new_count = new_count + 1 end
      if record.prefix == "+" then
        added = added + 1
      elseif record.prefix == "-" then
        removed = removed + 1
      end
    end
  end
  if #body == 0 then return nil end

  local suffix = context_hunk.context ~= "" and (" " .. context_hunk.context) or ""
  local lines = {
    ("@@ -%d,%d +%d,%d @@%s"):format(first.old_before or 0, old_count, first.new_before or 0, new_count, suffix),
  }
  vim.list_extend(lines, body)
  return {
    filename = file.filename,
    section_name = file.section_name,
    pos = first.new_before and first.new_before > 0 and first.new_before or first.old_before,
    diff = table.concat(lines, "\n"),
    staged = hunk.staged,
    context_text = hunk.context_text,
    added = added,
    removed = removed,
  }
end

---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param comments table[]
---@return DiffReviewHunk[]
function M.review_context_hunks_for_hunk(file, hunk, comments)
  local context_hunk = M.review_context_hunk(hunk)
  if not context_hunk then return {} end
  local windows = {}
  for _, comment in ipairs(comments or {}) do
    if M.hunk_contains_comment(hunk, comment) then
      local window = M.review_comment_context_window(context_hunk, comment, M.review_context_radius)
      if window then windows[#windows + 1] = window end
    end
  end
  local hunks = {}
  for _, window in ipairs(M.merge_context_windows(windows)) do
    local context_hunk_copy = M.context_window_hunk(file, hunk, context_hunk, window)
    if context_hunk_copy then hunks[#hunks + 1] = context_hunk_copy end
  end
  return hunks
end

---@param cwd string
---@param pr DiffReviewGhPR
---@param diff_text? string
---@param review DiffReviewGhSubmittedReview
---@return DiffReviewStatusFile[]
function M.review_context_files(cwd, pr, diff_text, review)
  local by_path = section_builder().comments_by_path(cwd, review.comments or {})
  local files = section_builder().files_from_diff(cwd, {
    section_name = M.review_key(review),
    default_status = "",
    files = pr.files,
  }, diff_text)
  local result = {}
  for _, file in ipairs(files) do
    local file_comments = by_path[file.relpath] or by_path[file.filename] or {}
    if #file_comments > 0 then
      local hunks = {}
      for _, hunk in ipairs(file.hunks or {}) do
        vim.list_extend(hunks, M.review_context_hunks_for_hunk(file, hunk, file_comments))
      end
      local copy = section_builder().file_with_hunks(file, hunks, file.section_name)
      copy.added = file.added
      copy.removed = file.removed
      copy.pr_review_comments = file_comments
      if #copy.hunks > 0 then result[#result + 1] = copy end
    end
  end
  return result
end

---@param raw_commit DiffReviewGhPRCommit|table
---@param branch? string
---@return DiffReviewStatusCommit?
function M.status_commit_from_pr_commit(raw_commit, branch)
  if type(raw_commit) ~= "table" then return nil end
  local oid = vim.trim(tostring(raw_commit.oid or raw_commit.sha or raw_commit.id or ""))
  if oid == "" then return nil end
  local cache = session.status and session.status.commit_file_cache and session.status.commit_file_cache[oid] or nil
  local short_oid = vim.trim(tostring(raw_commit.shortOid or raw_commit.short_oid or raw_commit.abbreviatedOid or ""))
  if short_oid == "" then short_oid = status_short_oid(oid) end
  return {
    oid = oid,
    short_oid = short_oid,
    branch = branch,
    subject = tostring(raw_commit.messageHeadline or raw_commit.subject or raw_commit.message or ""),
    committed_at = raw_commit.committedDate or raw_commit.committed_date or raw_commit.committed_at,
    authored_at = raw_commit.authoredDate or raw_commit.authored_date or raw_commit.authored_at,
    files = cache and cache.files or nil,
    files_loaded = cache and cache.files_loaded or false,
    files_loading = cache and cache.files_loading or false,
    files_error = cache and cache.files_error or nil,
  }
end

---@param pr DiffReviewGhPR
---@return DiffReviewStatusSection?
function M.commits_section(pr)
  local raw_commits = pr.commits or {}
  if type(raw_commits) ~= "table" or #raw_commits == 0 then return nil end
  local commits = {}
  for raw_index = #raw_commits, 1, -1 do
    local branch = #commits == 0 and pr.headRefName or nil
    local commit = M.status_commit_from_pr_commit(raw_commits[raw_index], branch)
    if commit then commits[#commits + 1] = commit end
  end
  if #commits == 0 then return nil end
  return {
    name = "pr_commits",
    title = "Recent Commits",
    default_folded = true,
    files = {},
    files_by_name = {},
    commits = commits,
  }
end

---@param pr DiffReviewGhPR?
---@return string?
function M.pr_base_url(pr)
  if not (pr and pr.url and pr.url ~= "") then return nil end
  return (pr.url:gsub("/$", ""))
end

---@param pr DiffReviewGhPR?
---@param entry DiffReviewStatusEntry?
---@return string?
function M.entry_url(pr, entry)
  if not entry then return nil end
  local base_url = M.pr_base_url(pr)
  local check = entry.pr_check
  if check and check.url and check.url ~= "" then return check.url end
  local reply = entry.pr_comment_reply or entry.review_reply
  if reply then
    if reply.url and reply.url ~= "" then return reply.url end
    if base_url and reply.remote_id then return ("%s/changes#r%s"):format(base_url, tostring(reply.remote_id)) end
  end

  local comment = entry.pr_comment
    or entry.review_comment
    or (entry.comment_box and entry.comment_box.source)
  if comment then
    if comment.url and comment.url ~= "" then return comment.url end
    if base_url and comment.remote_id then
      if comment.path and comment.path ~= "" then return ("%s/changes#r%s"):format(base_url, tostring(comment.remote_id)) end
      return ("%s#issuecomment-%s"):format(base_url, tostring(comment.remote_id))
    end
  end

  local review = entry.pr_review
  if review then
    if review.url and review.url ~= "" then return review.url end
    if base_url and review.id and review.id ~= 0 then return ("%s#pullrequestreview-%s"):format(base_url, tostring(review.id)) end
  end

  return nil
end

---@param review DiffReviewGhSubmittedReview
function M.render_review_context(review)
  local status = session.status
  if not (status and status.cwd and status.pr) then return end
  local comments = type(review.comments) == "table" and review.comments or {}
  if #comments == 0 then
    status_add_line("  No inline comments", { kind = "pr_review", pr_review = review }, "Comment")
    return
  end
  if not status.pr_diff_text or status.pr_diff_text == "" then
    status_add_line("  ...loading diff context...", { kind = "pr_review", pr_review = review }, "DiffReviewStatusFetching")
    return
  end

  local files = M.review_context_files(status.cwd, status.pr, status.pr_diff_text, review)
  if #files == 0 then
    status_add_line("  No matching diff context", { kind = "pr_review", pr_review = review }, "Comment")
    return
  end

  local review_key = M.review_key(review)
  local comment_index = section_builder().comment_anchor_index(comments)
  local previous_hook = status.review_after_row
  status.review_after_row = function(diff_line, indent)
    section_builder().emit_anchored_comments(status, diff_line, indent, { index = comment_index })
  end
  local ok, err = pcall(function()
    for _, file in ipairs(files) do
      status_render_file(
        file,
        "pr_review_file",
        "pr_review_hunk",
        status_provider_file_key(review_key, file.filename),
        function(hunk)
          return status_provider_hunk_key(review_key, file.filename, hunk.diff)
        end,
        { default_open = true }
      )
    end
  end)
  status.review_after_row = previous_hook
  if not ok then error(err) end
end

---@param review DiffReviewGhSubmittedReview
---@param alignment? { author_width?: integer, date_width?: integer }
function M.render_review(review, alignment)
  local review_key = M.review_key(review)
  local entry = { id = review_key, kind = "pr_review", pr_review = review, default_folded = true }
  local line_number = status_add_segment_line(M.review_summary_segments(review, alignment), entry)
  if status_folded(review_key, true) then
    fold_state()._status_register_fold_range(review_key, line_number, #session.status.lines, true, session.status.lines[line_number])
    return
  end
  M.render_review_context(review)
  fold_state()._status_register_fold_range(review_key, line_number, #session.status.lines, true, session.status.lines[line_number])
end

---@param comment table
---@param index integer
---@param alignment? { author_width?: integer, date_width?: integer }
function M.render_issue_comment(comment, index, alignment)
  local entry_id = M.issue_comment_entry_id(comment, index)
  local start_line = #session.status.lines + 1
  local fold_text = function()
    return M.issue_comment_line(comment, false, alignment)
  end
  for _, row in ipairs(M.issue_comment_rows({
    comment,
  }, {
    alignment = alignment,
    start_index = index,
  })) do
    local line_number = status_add_line(row.text, row.entry, row.line_hl_group)
    for _, highlight in ipairs(row.highlights or {}) do
      status_add_highlight(line_number, highlight.start_col, highlight.end_col, highlight.hl_group)
    end
  end
  fold_state()._status_register_fold_range(entry_id, start_line, #session.status.lines, true, fold_text)
end

---@param comments table[]
---@param opts? { alignment?: { author_width?: integer, date_width?: integer }, start_index?: integer, is_folded?: fun(entry_id: string, default: boolean, entry: table): boolean }
---@return table[]
function M.issue_comment_rows(comments, opts)
  opts = opts or {}
  comments = comments or {}
  local row_opts = M.issue_comment_row_options()
  row_opts.alignment = opts.alignment or M.issue_comment_alignment(comments)
  row_opts.start_index = opts.start_index
  row_opts.is_folded = opts.is_folded or function(entry_id, default)
    return status_folded(entry_id, default)
  end
  return comment_rows.rows(comments, row_opts)
end

---@param comments table[]
function M.render_issue_comments(comments)
  local alignment = M.issue_comment_alignment(comments)
  for index, comment in ipairs(comments or {}) do
    M.render_issue_comment(comment, index, alignment)
  end
end

---@param buf integer
---@return string?
function M.url_under_cursor(buf)
  if session.states and session.states[buf] then session.status = session.states[buf] end
  local status = session.status
  if not (status and status.view_kind == "pr") then return nil end
  local url = M.entry_url(status.pr, status_entry_under_cursor())
  if url then return url end
  return nil
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
function M.load_comments(pr, cwd, buf)
  local status = session.status
  if not (status and status.buf == buf and pr.repo and pr.repo ~= "") then return end
  status.pr_comments_request_id = (status.pr_comments_request_id or 0) + 1
  local request_id = status.pr_comments_request_id
  gh.pr_comments_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = session.states and session.states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_comments_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    session.status = latest_status
    if not result.ok then
      notify_error("GitHub PR comments failed: " .. (result.message or "gh failed"), "DiffReview")
      return
    end
    latest_status.pr_comments = result
    M.import_viewer_authored_comments(latest_status, result)
    render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
  end)
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
function M.load_checks(pr, cwd, buf)
  local status = session.status
  if not (status and status.buf == buf and pr.repo and pr.repo ~= "") then return end
  status.pr_checks_request_id = (status.pr_checks_request_id or 0) + 1
  status.pr_checks_loading = true
  status.pr_checks_error = nil
  local request_id = status.pr_checks_request_id
  render_pr_status(pr, cwd, buf, status.pr_diff_text)
  gh.pr_checks_async(cwd, pr.number, pr.repo, function(result)
    local latest_status = session.states and session.states[buf] or nil
    if not (
      latest_status
      and latest_status.pr_checks_request_id == request_id
      and latest_status.buf == buf
      and vim.api.nvim_buf_is_valid(buf)
    ) then return end
    session.status = latest_status
    latest_status.pr_checks_loading = false
    if not result.ok then
      latest_status.pr_checks_error = result.message or "gh failed"
      notify_error("GitHub PR checks failed: " .. latest_status.pr_checks_error, "DiffReview")
      render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
      return
    end
    latest_status.pr_checks_error = nil
    latest_status.pr_checks = result.checks or {}
    render_pr_status(pr, cwd, buf, latest_status.pr_diff_text)
  end)
end

---@param status table
---@return table[]
function M.editable_comments(status)
  local comments = {}
  for _, comment in ipairs(status.pr_standalone_comments or {}) do
    comments[#comments + 1] = comment
  end
  for _, comment in ipairs(status.pr_regular_comments or {}) do
    comments[#comments + 1] = comment
  end
  return comments
end

---@param status table
---@return table[]
function M.refresh_editable_comments(status)
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.pr_regular_comments = status.pr_regular_comments or {}
  status.review_comments = M.editable_comments(status)
  return status.review_comments
end

---@param buf integer
---@return table? status
function M.state(buf)
  local status = session.states and session.states[buf] or nil
  if not (status and status.view_kind == "pr" and status.pr) then return nil end
  M.refresh_editable_comments(status)
  return status
end

---@class DiffReviewPrReplyDraft
---@field parent table
---@field body string
---@field focused boolean
---@field syncing boolean?
---@field anchor_row integer?
---@field anchor_entry_id string?
---@field review_header_mark integer?
---@field review_body_region table?
---@field review_body_line_marks table[]?
---@field review_rendered_body_text string?

--- Resolve the top-level inline comment under the cursor from either PR render mode.
---@param buf integer
---@return table? comment
function M.reply_target_under_cursor(buf)
  local status = M.state(buf)
  if not (status and vim.api.nvim_get_current_buf() == buf) then return nil end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local comment = review_mod().rendered_comment_at_row(buf, row)
  if not comment or comment.pr_issue_comment or not comment.remote_id then return nil end
  return comment
end

--- Append a created reply to every in-memory projection of its parent comment.
---@param status table
---@param target table
---@param reply DiffReviewGhReviewCommentReply
function M.append_review_comment_reply(status, target, reply)
  local seen = {}
  local function append(comment)
    if not (comment and review_mod().same_comment(comment, target)) or seen[comment] then return end
    seen[comment] = true
    comment.replies = comment.replies or {}
    comment.replies[#comment.replies + 1] = reply
  end
  append(target)
  for _, comment in ipairs(status.pr_standalone_comments or {}) do append(comment) end
  local comments = status.pr_comments or {}
  for _, comment in ipairs(comments.code_comments or {}) do append(comment) end
  for _, submitted_review in ipairs(comments.reviews or {}) do
    for _, comment in ipairs(submitted_review.comments or {}) do append(comment) end
  end
end

--- Create or focus an inline reply draft below the selected PR comment thread.
---@param buf integer
---@return boolean handled
function M.reply_to_comment(buf)
  local status = M.state(buf)
  local comment = M.reply_target_under_cursor(buf)
  if not (status and comment) then return false end
  review_mod().sync_inline_comment_text(buf)
  M.sync_reply_draft_text(buf)
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local entry = status.entries and status.entries[row] or nil
  local anchor_row = entry and entry.comment_box_anchor_line or status.focused_comment_anchor_row
  local anchor_entry_id = entry and entry.comment_box_anchor_entry_id or status.focused_comment_entry_id
  local draft = status.pr_reply_draft
  if draft and not review_mod().same_comment(draft.parent, comment) then
    if review_mod().comment_body_for_sync(draft.body or "") ~= "" then
      vim.notify("Save the current reply before starting another", vim.log.levels.WARN, { title = "DiffReview" })
      review_mod().focus_reply_draft(buf, draft, { insert = true })
      return true
    end
    status.pr_reply_draft = nil
    draft = nil
  end
  if not draft then
    draft = {
      parent = comment,
      body = "",
      focused = true,
      anchor_row = anchor_row,
      anchor_entry_id = anchor_entry_id,
    }
    status.pr_reply_draft = draft
  end
  review_mod().focus_reply_draft(buf, draft, {
    insert = true,
    anchor_row = anchor_row,
    anchor_entry_id = anchor_entry_id,
  })
  return true
end

--- Read the inline reply editor back into its PR-owned draft.
---@param buf integer
---@return boolean changed
function M.sync_reply_draft_text(buf)
  local status = M.state(buf)
  local draft = status and status.pr_reply_draft or nil
  if not (draft and vim.api.nvim_buf_is_valid(buf)) then return false end
  local edited_text = review_mod().comment_body_text_from_buffer(buf, draft)
  if edited_text == nil then return false end
  edited_text = review_mod().normalize_comment_body_text(edited_text)
  if edited_text == review_mod().normalize_comment_body_text(draft.body or "") then return false end
  draft.body = edited_text
  draft.review_rendered_body_text = edited_text
  draft.syncing = nil
  vim.bo[buf].modified = true
  return true
end

--- Post the active inline reply draft through the review-comment reply endpoint.
---@param buf integer
---@return boolean handled
function M.sync_reply_draft(buf)
  local status = M.state(buf)
  if not status then return false end
  M.sync_reply_draft_text(buf)
  local draft = status.pr_reply_draft
  if not draft then return false end
  local body = review_mod().comment_body_for_sync(draft.body or "")
  if body == "" then
    vim.notify("Reply body cannot be empty", vim.log.levels.WARN, { title = "DiffReview" })
    return true
  end
  if draft.syncing then return true end
  if vim.fn.mode():sub(1, 1) == "i" then pcall(vim.cmd, "stopinsert") end
  draft.syncing = true
  M.refresh_modified(buf)
  local comment = draft.parent
  gh.create_review_comment_reply_async(status.cwd, status.pr.number, status.pr.repo, comment.remote_id, body, function(result)
    local latest = M.state(buf) or status
    if not result.ok or not result.reply then
      draft.syncing = nil
      if vim.api.nvim_buf_is_valid(buf) then M.refresh_modified(buf) end
      notify_error("GitHub review reply failed: " .. (result.message or "gh returned no reply"), "DiffReview")
      return
    end
    M.append_review_comment_reply(latest, comment, result.reply)
    if latest.pr_reply_draft == draft then latest.pr_reply_draft = nil end
    if vim.api.nvim_buf_is_valid(buf) and latest.buf == buf then
      session.status = latest
      render_pr_status(latest.pr, latest.cwd, buf, latest.pr_diff_text)
      M.refresh_modified(buf)
    end
    vim.notify("Reply posted", vim.log.levels.INFO, { title = "DiffReview" })
  end)
  return true
end

---@param comment table?
---@return string?
function M.comment_identity_key(comment)
  if not comment then return nil end
  if comment.remote_node_id and comment.remote_node_id ~= "" then return "node:" .. tostring(comment.remote_node_id) end
  if comment.remote_id then return "id:" .. tostring(comment.remote_id) end
  return nil
end

---@param target table
---@param remote table
function M.merge_remote_editable_comment(target, remote)
  target.remote_id = remote.remote_id or target.remote_id
  target.remote_node_id = remote.remote_node_id or target.remote_node_id
  target.remote_review_id = remote.review_id or target.remote_review_id
  target.review_node_id = remote.review_node_id or target.review_node_id
  target.review_state = remote.review_state or target.review_state
  target.position = remote.position or target.position
  target.line = remote.line or target.line
  target.start_line = remote.start_line or target.start_line
  target.start_side = remote.start_side or target.start_side
  if remote.path and remote.path ~= "" then target.path = remote.path end
  target.side = remote.side or target.side
  target.user = remote.user or target.user
  target.created_at = remote.created_at or target.created_at
  target.updated_at = remote.updated_at or target.updated_at
  target.url = remote.url or target.url
  target.resolved = remote.resolved
  target.outdated = remote.outdated
  target.viewer_did_author = remote.viewer_did_author
  target.replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or (target.replies or {})

  local remote_body = review_mod().normalize_comment_body_text(remote.body)
  local base_body = review_mod().normalize_comment_body_text(target.base_body)
  local local_body = review_mod().normalize_comment_body_text(target.body)
  local base_sync_body = review_mod().comment_body_for_sync(base_body)
  local local_sync_body = review_mod().comment_body_for_sync(local_body)
  if target.local_state == "dirty" or target.local_state == "new" then
    if remote_body == local_sync_body then
      target.base_body = remote_body
      target.local_state = "clean"
    elseif base_body ~= "" and remote_body ~= base_sync_body then
      target.remote_body = remote_body
      target.local_state = "conflict"
    end
  elseif target.local_state ~= "deleted" then
    target.body = remote_body
    target.base_body = remote_body
    target.remote_body = nil
    target.local_state = "clean"
  end
end

---@param status table
---@param list table[]
---@param remote table
---@param opts? { regular?: boolean }
function M.merge_viewer_authored_comment_into_list(status, list, remote, opts)
  if not (remote and remote.viewer_did_author == true) then return end
  local key = M.comment_identity_key(remote)
  if not key then return end
  for _, comment in ipairs(list) do
    if M.comment_identity_key(comment) == key then
      M.merge_remote_editable_comment(comment, remote)
      if opts and opts.regular then comment.pr_issue_comment = true else comment.standalone = true end
      review_mod().normalize_comment(status, comment)
      return
    end
  end

  local comment
  if opts and opts.regular then
    comment = vim.deepcopy(remote)
    comment.local_id = remote.remote_node_id or ("remote:" .. tostring(remote.remote_id or ""))
    comment.pr_issue_comment = true
    comment.local_state = "clean"
    comment.base_body = remote.body
    comment.replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or {}
    review_mod().normalize_comment(status, comment)
  else
    comment = review_mod().comment_from_remote(status, remote)
    comment.standalone = true
  end
  list[#list + 1] = comment
end

---@param status table
---@param comments DiffReviewGhPRCommentsResult?
function M.import_viewer_authored_comments(status, comments)
  if not (status and comments) then return end
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  status.pr_regular_comments = status.pr_regular_comments or {}
  for _, comment in ipairs(comments.code_comments or {}) do
    M.merge_viewer_authored_comment_into_list(status, status.pr_standalone_comments, comment)
  end
  for _, comment in ipairs(comments.issue_comments or {}) do
    M.merge_viewer_authored_comment_into_list(status, status.pr_regular_comments, comment, { regular = true })
  end
  M.refresh_editable_comments(status)
end

---@param status table
---@param target table?
---@return integer?
function M.regular_comment_index(status, target)
  if not (status and target) then return nil end
  for index, comment in ipairs(status.pr_regular_comments or {}) do
    if review_mod().same_comment(comment, target) then return index end
  end
  return nil
end

---@param status table
---@param comment table?
---@return boolean
function M.is_regular_comment(status, comment)
  return M.regular_comment_index(status, comment) ~= nil
end

---@param status table
---@param comment table?
---@return boolean
function M.is_standalone_comment(status, comment)
  if not (status and comment) then return false end
  for _, local_comment in ipairs(status.pr_standalone_comments or {}) do
    if review_mod().same_comment(local_comment, comment) then return true end
  end
  for _, local_comment in ipairs(status.pr_regular_comments or {}) do
    if review_mod().same_comment(local_comment, comment) then return true end
  end
  return false
end

--- Remove an inline comment from every PR-owned comment projection.
---@param status table
---@param target table
---@return boolean removed
function M.remove_standalone_comment(status, target)
  if not (status and target) then return false end
  local removed = false
  local function remove_matching_comment(list)
    for index = #(list or {}), 1, -1 do
      if review_mod().same_comment(list[index], target) then
        table.remove(list, index)
        removed = true
      end
    end
  end
  remove_matching_comment(status.pr_standalone_comments)
  local remote_comments = status.pr_comments
  if remote_comments then
    remove_matching_comment(remote_comments.code_comments)
    for _, submitted_review in ipairs(remote_comments.reviews or {}) do
      remove_matching_comment(submitted_review.comments)
    end
  end
  M.refresh_editable_comments(status)
  return removed
end

--- Open the PR conversation comment selected by the open command.
---@param buf integer
---@return boolean opened
function M.open_issue_comment_under_cursor(buf)
  local status = M.state(buf)
  if not (status and status.view_kind == "pr" and vim.api.nvim_buf_is_valid(buf)) then return false end
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  local entry = status.entries and status.entries[cursor_row] or nil
  if not (entry and entry.kind == "pr_comment" and entry.id and entry.pr_comment) then return false end
  if not status_folded(entry.id, true) then return true end

  set_status_folded(entry.id, false)
  if not fold_state()._status_set_native_fold_state(buf, entry.id, false) then
    render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
  end
  return true
end

---@param buf integer
---@return table? comment
function M.standalone_comment_under_cursor(buf)
  local status = M.state(buf)
  if not status then return nil end
  local comment = review_mod().comment_at_cursor(buf)
  if M.is_standalone_comment(status, comment) then return comment end
  return nil
end

---@param status table
---@return boolean
function M.selection_is_in_changes(status)
  local mode = vim.fn.mode()
  local start_row, end_row
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    start_row, end_row = vim.fn.line("v"), vim.fn.line(".")
  else
    start_row = vim.fn.line(".")
    end_row = start_row
  end
  if start_row > end_row then start_row, end_row = end_row, start_row end

  local changes_section_name = "pr:" .. tostring(status.pr and status.pr.number or "") .. ":changes"
  local found_diff_row = false
  for row = start_row, end_row do
    local entry = status.entries and status.entries[row] or nil
    if entry and entry.diff_line then
      if not (entry.kind == "pr_hunk" and entry.hunk and entry.hunk.section_name == changes_section_name) then
        return false
      end
      found_diff_row = true
    end
  end
  return found_diff_row
end

---@param status table
---@return table? payload
function M.selection_payload(status)
  if not M.selection_is_in_changes(status) then return nil end
  return review_mod().selection_payload(status)
end

---@param buf integer
function M.refresh_modified(buf)
  local status = M.state(buf)
  if not status or not vim.api.nvim_buf_is_valid(buf) then return end
  local title_dirty, desc_dirty, review_dirty, milestone_dirty = pr_edit.dirty_flags(buf, status)
  local comments_dirty = false
  for _, comment in ipairs(status.review_comments or {}) do
    if review_mod().comment_needs_sync(comment) then
      comments_dirty = true
      break
    end
  end
  local reply_draft = status.pr_reply_draft
  local reply_dirty = reply_draft ~= nil
    and not reply_draft.syncing
    and review_mod().comment_body_for_sync(reply_draft.body or "") ~= ""
  vim.bo[buf].modified = title_dirty or desc_dirty or review_dirty or milestone_dirty or comments_dirty or reply_dirty
end

---@param buf integer
function M.render_preserving_inline_cursor(buf)
  local status = M.state(buf)
  if not status then return end
  local comment
  local snapshot
  if vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    comment = review_mod().comment_body_at_row(buf, cursor[1])
    if comment then snapshot = review_mod().inline_comment_cursor_snapshot(buf, comment) end
  end
  render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
  if comment and snapshot and comment.local_state ~= "deleted" then
    review_mod().restore_inline_comment_cursor(buf, comment, snapshot)
  end
end

---@param status table
---@param comment table
---@param remote table?
---@param queued_body string
function M.apply_synced_standalone_comment(status, comment, remote, queued_body)
  if remote then
    comment.remote_id = remote.remote_id or comment.remote_id
    comment.remote_node_id = remote.remote_node_id or comment.remote_node_id
    comment.remote_review_id = remote.review_id or comment.remote_review_id
    comment.review_node_id = remote.review_node_id or comment.review_node_id
    comment.position = remote.position or comment.position
    comment.line = remote.line or comment.line
    if remote.path and remote.path ~= "" then comment.path = remote.path end
    comment.side = remote.side or comment.side
    comment.user = remote.user or comment.user
    comment.created_at = remote.created_at or comment.created_at
    comment.updated_at = remote.updated_at or comment.updated_at
    comment.url = remote.url or comment.url
    comment.replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or (comment.replies or {})
  end
  if review_mod().comment_body_for_sync(comment.body or "") == queued_body then
    comment.body = queued_body
    comment.base_body = queued_body
    comment.local_state = "clean"
  else
    comment.base_body = queued_body
    comment.local_state = "dirty"
  end
  comment.syncing = nil
  review_mod().normalize_comment(status, comment)
end

---@param status table
---@param comment table
---@param remote table?
---@param queued_body string
function M.apply_synced_regular_comment(status, comment, remote, queued_body)
  local comment_index = M.regular_comment_index(status, comment)
  local old_entry_id = comment_index and M.issue_comment_entry_id(comment, comment_index) or nil
  local old_folded = old_entry_id and status_folded(old_entry_id, true) or nil
  if remote then
    comment.remote_id = remote.remote_id or comment.remote_id
    comment.remote_node_id = remote.remote_node_id or comment.remote_node_id
    comment.user = remote.user or comment.user
    comment.created_at = remote.created_at or comment.created_at
    comment.updated_at = remote.updated_at or comment.updated_at
    comment.url = remote.url or comment.url
  end
  if review_mod().comment_body_for_sync(comment.body or "") == queued_body then
    comment.body = queued_body
    comment.base_body = queued_body
    comment.local_state = "clean"
  else
    comment.base_body = queued_body
    comment.local_state = "dirty"
  end
  comment.syncing = nil
  review_mod().normalize_comment(status, comment)
  M.refresh_editable_comments(status)
  if comment_index then
    local new_entry_id = M.issue_comment_entry_id(comment, comment_index)
    if old_entry_id and old_entry_id ~= new_entry_id and old_folded ~= nil then
      set_status_folded(new_entry_id, old_folded)
    end
  end
end

---@param status table
---@param operation fun(done: fun())
function M.enqueue_standalone_sync(status, operation)
  status.pr_standalone_sync = status.pr_standalone_sync or { queue = {}, running = false }
  status.pr_standalone_sync.queue[#status.pr_standalone_sync.queue + 1] = operation
  local function run_next()
    if status.pr_standalone_sync.running then return end
    local next_operation = table.remove(status.pr_standalone_sync.queue, 1)
    if not next_operation then return end
    status.pr_standalone_sync.running = true
    next_operation(function()
      status.pr_standalone_sync.running = false
      run_next()
    end)
  end
  run_next()
end

---@param buf integer
---@param comment table
function M.sync_regular_comment(buf, comment)
  local status = M.state(buf)
  if not (status and status.pr and comment) then return end
  local body = review_mod().comment_body_for_sync(comment.body or "")
  if body == "" then
    comment.syncing = nil
    M.refresh_modified(buf)
    return
  end
  local pr = status.pr
  local update_id = comment.remote_id
  M.enqueue_standalone_sync(status, function(done)
    local latest = M.state(buf)
    if not latest then
      done()
      return
    end
    local callback = function(result)
      latest = M.state(buf)
      if not latest then
        done()
        return
      end
      if not result.ok then
        comment.syncing = nil
        notify_error("GitHub PR comment sync failed: " .. (result.message or "gh failed"), "DiffReview")
        if vim.api.nvim_buf_is_valid(buf) then
          M.render_preserving_inline_cursor(buf)
          M.refresh_modified(buf)
        end
        done()
        return
      end
      M.apply_synced_regular_comment(latest, comment, result.issue_comments and result.issue_comments[1] or nil, body)
      if vim.api.nvim_buf_is_valid(buf) then
        M.render_preserving_inline_cursor(buf)
        M.refresh_modified(buf)
      end
      vim.notify("PR comment synced", vim.log.levels.INFO, { title = "DiffReview" })
      done()
    end
    if update_id then
      gh.update_issue_comment_async(latest.cwd, pr.repo, update_id, body, callback)
    else
      gh.create_issue_comment_async(latest.cwd, pr.number, pr.repo, body, callback)
    end
  end)
end

---@param buf integer
---@param comment table
function M.sync_standalone_comment(buf, comment)
  local status = M.state(buf)
  if not (status and status.pr and comment) then return end
  if M.is_regular_comment(status, comment) or comment.pr_issue_comment then
    M.sync_regular_comment(buf, comment)
    return
  end
  if comment.local_state == "deleted" then
    local comment_node_id = comment.remote_node_id
    if not comment_node_id or comment_node_id == "" then
      comment.syncing = nil
      M.remove_standalone_comment(status, comment)
      render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
      M.refresh_modified(buf)
      return
    end
    M.enqueue_standalone_sync(status, function(done)
      gh.delete_review_comment_async(status.cwd, comment_node_id, function(result)
        local latest = M.state(buf)
        if not latest then done() return end
        comment.syncing = nil
        if not result.ok then
          comment.local_state = comment.delete_previous_state or "clean"
          comment.delete_previous_state = nil
          notify_error("GitHub inline comment delete failed: " .. (result.message or "gh failed"), "DiffReview")
          if vim.api.nvim_buf_is_valid(buf) then
            render_pr_status(latest.pr, latest.cwd, buf, latest.pr_diff_text)
            M.refresh_modified(buf)
          end
          done()
          return
        end
        comment.delete_previous_state = nil
        M.remove_standalone_comment(latest, comment)
        if vim.api.nvim_buf_is_valid(buf) then
          render_pr_status(latest.pr, latest.cwd, buf, latest.pr_diff_text)
          M.refresh_modified(buf)
        end
        vim.notify("Inline comment deleted", vim.log.levels.INFO, { title = "DiffReview" })
        done()
      end)
    end)
    return
  end
  local body = review_mod().comment_body_for_sync(comment.body or "")
  if body == "" then
    comment.syncing = nil
    M.refresh_modified(buf)
    return
  end
  local pr = status.pr
  local opts = {
    body = body,
    commit_id = pr.headRefOid,
    path = comment.path,
    line = comment.line,
    side = comment.side or "RIGHT",
    start_line = comment.start_line,
    start_side = comment.start_side,
    position = comment.position,
  }
  local update_node_id = comment.remote_node_id
  M.enqueue_standalone_sync(status, function(done)
    local latest = M.state(buf)
    if not latest then
      done()
      return
    end
    local callback = function(result)
      latest = M.state(buf)
      if not latest then
        done()
        return
      end
      if not result.ok then
        comment.syncing = nil
        notify_error("GitHub inline comment sync failed: " .. (result.message or "gh failed"), "DiffReview")
        if vim.api.nvim_buf_is_valid(buf) then
          M.render_preserving_inline_cursor(buf)
          M.refresh_modified(buf)
        end
        done()
        return
      end
      M.apply_synced_standalone_comment(latest, comment, result.comments and result.comments[1] or nil, body)
      if vim.api.nvim_buf_is_valid(buf) then
        M.render_preserving_inline_cursor(buf)
        M.refresh_modified(buf)
      end
      vim.notify("Inline comment synced", vim.log.levels.INFO, { title = "DiffReview" })
      done()
    end
    if update_node_id and update_node_id ~= "" then
      gh.update_review_comment_async(latest.cwd, update_node_id, body, callback)
    else
      gh.create_pr_review_comment_async(latest.cwd, pr.number, pr.repo, opts, callback)
    end
  end)
end

---@param buf integer
---@return integer count
function M.sync_standalone_comments(buf)
  local status = M.state(buf)
  if not status then return 0 end
  review_mod().sync_inline_comment_text(buf)
  local comments = {}
  for _, comment in ipairs(status.review_comments or {}) do
    if review_mod().comment_needs_sync(comment) then
      comment.syncing = true
      comments[#comments + 1] = comment
    end
  end
  if #comments == 0 then return 0 end
  for _, comment in ipairs(comments) do
    review_mod().refresh_inline_comment_header(buf, comment)
  end
  M.refresh_modified(buf)
  for _, comment in ipairs(comments) do
    M.sync_standalone_comment(buf, comment)
  end
  return #comments
end

---@param status table
---@param payload table
---@return table comment
function M.create_inline_comment(status, payload)
  status.pr_standalone_comments = status.pr_standalone_comments or {}
  local comment = review_mod().inline_comment_from_payload(status, payload)
  comment.standalone = true
  status.pr_standalone_comments[#status.pr_standalone_comments + 1] = comment
  M.refresh_editable_comments(status)
  return comment
end

---@param status table
---@return table comment
function M.create_regular_comment(status)
  status.pr_regular_comments = status.pr_regular_comments or {}
  local created_at = os.date("%Y-%m-%d %H:%M")
  local comment = {
    body = "",
    user = "you",
    created_at = created_at,
    updated_at = created_at,
    local_state = "new",
  }
  review_mod().normalize_comment(status, comment)
  status.pr_regular_comments[#status.pr_regular_comments + 1] = comment
  set_status_folded(M.issue_comment_entry_id(comment, #status.pr_regular_comments), false)
  M.refresh_editable_comments(status)
  return comment
end

---@param buf integer
function M.add_standalone_comment(buf)
  local status = M.state(buf)
  if not status then return end
  session.status = status
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local entry = status.entries and status.entries[row] or nil
  if entry and (entry.kind == "comment_box" or entry.kind == "pr_comment" or review_mod().comment_at_row(buf, row)) then
    return
  end
  local payload = M.selection_payload(status)
  review_mod().leave_visual()
  if payload and not entry_nav._status_source_policy_allows_cursor(status, "comment") then return end
  local comment = payload
    and M.create_inline_comment(status, payload)
    or M.create_regular_comment(status)
  if payload then
    review_mod().focus_comment(buf, comment, { insert = true })
    M.refresh_modified(buf)
    return
  end
  render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
  review_mod().focus_inline_comment(buf, comment, { insert = true })
  M.refresh_modified(buf)
end

--- Delete the editable inline PR comment under the cursor.
---@param buf integer
function M.delete_standalone_comment(buf)
  local status = M.state(buf)
  if not status then return end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  local entry = status.entries and status.entries[row] or nil
  local reply_draft = status.pr_reply_draft
  if reply_draft
    and (review_mod().reply_draft_body_at_row(buf, row) == reply_draft
      or (entry and entry.comment_box and entry.comment_box.reply_draft == reply_draft)) then
    status.pr_reply_draft = nil
    render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
    M.refresh_modified(buf)
    vim.notify("Reply draft discarded", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  local comment = M.standalone_comment_under_cursor(buf)
  if not comment or M.is_regular_comment(status, comment) then
    vim.notify("Only your inline PR comments can be deleted here", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  if comment.remote_node_id then
    comment.delete_previous_state = comment.local_state
    comment.local_state = "deleted"
    vim.notify("Inline comment marked for deletion. Press Ctrl-S to sync", vim.log.levels.INFO, { title = "DiffReview" })
  else
    M.remove_standalone_comment(status, comment)
  end
  if status.focused_comment_id == comment.local_id then
    review_mod().clear_comment_focus(status)
  end
  M.refresh_editable_comments(status)
  render_pr_status(status.pr, status.cwd, buf, status.pr_diff_text)
  M.refresh_modified(buf)
end

return M
