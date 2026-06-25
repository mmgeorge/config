--- Drives batched PR review mode: draft comment CRUD, inline comment rendering, verdict
--- selection, and review submission, layering editable comment rows over the diff.
---@class DiffReviewReviewModule

local status_buffer = require("diff_review.views.status.status_buffer")
local region = require("diff_review.render.region")
local annotations = require("diff_review.render.annotations")
local notifications = require("diff_review.infra.notifications")
local gh = require("diff_review.integrations.gh")
local config = require("diff_review.infra.config")
local repo_relative = require("diff_review.infra.paths").repo_relative

local function dr()
  return require("diff_review")
end

-- Render-core shims: thread the active status into the state-passing buffer core.
local function status_add_line(text, entry, hl)
  return status_buffer.add_line(dr()._status, text, entry, hl)
end
local function status_add_highlight(line, start_col, end_col, hl_group, priority)
  return status_buffer.add_highlight(dr()._status, line, start_col, end_col, hl_group, priority)
end
local function status_folded(key, default, state)
  return status_buffer.folded(state or dr()._status or {}, key, default)
end
local function set_status_folded(key, folded, state)
  if not state then
    dr()._status = dr()._status or {}
    state = dr()._status
  end
  return status_buffer.set_folded(state, key, folded)
end
local function notify_error(message, title)
  return notifications.error(message, title)
end
local function notify_debug(...) return dr()._notify_debug(...) end

-- Seams to init-owned helpers the review view shares.
local function render_pr_status(...) return dr()._render_pr_status(...) end
local function status_command_visible(...) return dr()._status_command_visible(...) end
local function status_entry_under_cursor(...) return dr()._status_entry_under_cursor(...) end
local function status_keys_for(...) return dr()._status_keys_for(...) end
local function status_provider_file_key(...) return dr()._status_provider_file_key(...) end
local function status_render_loaded(...) return dr()._status_render_loaded(...) end

local M = {
  ns = vim.api.nvim_create_namespace("diff_review.views.pr.review"),
  comment_icon = nil,
  reply_icon = nil,
  -- Test seams: the comment-body input and the verdict picker. Defaults open
  -- real UI; tests override them. Mirrors the set_backend/set_reader pattern.
  input_provider = nil, ---@type (fun(title: string, on_submit: fun(text: string)))?
  verdict_provider = nil, ---@type (fun(on_choice: fun(event: string?)))?
  data_dir_for_test = nil,
}

---@param buf integer
---@return table? state the review-capable status-state, or nil if buf has no inline comment rows
function M.state(buf)
  local status = dr()._status_states and dr()._status_states[buf] or nil
  if status and status.view_kind == "review" then return status end
  if status and status.view_kind == "pr" then
    status.pr_standalone_comments = status.pr_standalone_comments or {}
    status.pr_regular_comments = status.pr_regular_comments or {}
    if dr()._pr_overview.editable_comments then
      status.review_comments = dr()._pr_overview.editable_comments(status)
    else
      status.review_comments = status.pr_standalone_comments
    end
    return status
  end
  return nil
end

---@return DiffReviewReviewKeymapConfig
function M.keymap_config()
  local options = M.config or config.options or config.defaults
  local keymaps = options.keymaps or config.defaults.keymaps
  return vim.tbl_deep_extend("force", vim.deepcopy(config.defaults.keymaps.review), keymaps.review or {})
end

---@param id string
---@return string[]
function M.keys_for(id)
  return status_keys_for(id)
end

---@param path string?
function M.set_data_dir_for_test(path)
  require("github.repo_cache").set_data_dir_for_test(path)
end

---@return string
function M.data_dir()
  return require("github.repo_cache").base_dir()
end

---@param state table
---@return string
function M.storage_repo(state)
  local pr = state.pr or {}
  local repo = type(pr.repo) == "string" and vim.trim(pr.repo) or ""
  if repo ~= "" then return repo end
  local url = type(pr.url) == "string" and pr.url or ""
  local owner, name = url:match("github%.com[:/]([^/]+)/([^/]+)/pull/%d+")
  if owner and name then return owner .. "/" .. name end
  return "unknown/" .. vim.fn.sha256(tostring(state.cwd or ""))
end

---@param segment string|number?
---@return string
function M.storage_segment(segment)
  local text = vim.trim(tostring(segment or ""))
  if text == "" then text = "current" end
  text = text:gsub("[<>:\"/\\|?*%c]", "-"):gsub("^%.+$", "-")
  return text
end

---@param state table
---@return string[]
function M.storage_segments(state)
  local segments = {}
  for _, segment in ipairs(vim.split(M.storage_repo(state), "/", { plain = true, trimempty = true })) do
    segments[#segments + 1] = M.storage_segment(segment)
  end
  segments[#segments + 1] = M.storage_segment((state.pr or {}).number)
  return segments
end

---@param state table
---@return string
function M.storage_path(state)
  return require("github.repo_cache").review_path(M.storage_repo(state), (state.pr or {}).number)
end

---@param state table
function M.load_draft(state)
  local path = M.storage_path(state)
  if vim.uv.fs_stat(path) == nil then return end
  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then return end
  local decoded_ok, draft = pcall(vim.json.decode, table.concat(lines, "\n"))
  if not decoded_ok or type(draft) ~= "table" then return end
  if type(draft.commit_id) == "string" then state.review_draft_commit_id = draft.commit_id end
  if type(draft.review_viewed) == "table" then state.review_viewed = draft.review_viewed end
  if type(draft.review_viewed_hunks) == "table" then state.review_viewed_hunks = draft.review_viewed_hunks end
  if type(draft.review_comment_text) == "string" then state.review_comment_text = draft.review_comment_text end
  if type(draft.review_comments) == "table" then state.review_comments = draft.review_comments end
  if type(draft.review_remote) == "table" then state.review_remote = draft.review_remote end
end

---@param state table
---@return table[]
function M.comments_for_storage(state)
  local comments = {}
  for _, comment in ipairs(state.review_comments or {}) do
    local copy = vim.deepcopy(comment)
    copy.syncing = nil
    copy.review_body_region = nil
    copy.review_header_mark = nil
    copy.review_footer_mark = nil
    copy.review_reply_header_marks = nil
    copy.review_body_line_marks = nil
    copy.review_body_render_rows = nil
    copy.review_body_render_row_count = nil
    copy.review_body_prefix_width = nil
    copy.review_markdown_ranges = nil
    copy.review_rendered_body_text = nil
    comments[#comments + 1] = copy
  end
  return comments
end

---@param state table
function M.save_draft(state)
  local path = M.storage_path(state)
  local directory = vim.fs.dirname(path) or M.data_dir()
  local mkdir_ok, mkdir_err = pcall(vim.fn.mkdir, directory, "p")
  if not mkdir_ok or mkdir_err == 0 then
    notify_error("Could not create review draft directory: " .. directory, "DiffReview")
    return
  end
  local payload = {
    cwd = state.cwd,
    repo = state.pr and state.pr.repo or nil,
    number = state.pr and state.pr.number or nil,
    commit_id = state.commit_id,
    review_viewed = state.review_viewed or {},
    review_viewed_hunks = state.review_viewed_hunks or {},
    review_comment_text = state.review_comment_text or "",
    review_comments = M.comments_for_storage(state),
    review_remote = state.review_remote or {},
  }
  local encode_ok, encoded = pcall(vim.json.encode, payload)
  if not encode_ok then
    notify_error("Could not encode review draft", "DiffReview")
    return
  end
  local write_ok, write_err = pcall(vim.fn.writefile, { encoded }, path)
  if not write_ok or write_err ~= 0 then
    notify_error("Could not write review draft", "DiffReview")
  end
end

---@param state table
function M.delete_draft(state)
  pcall(vim.fn.delete, M.storage_path(state))
end

---@param value any
---@return string
function M.normalize_comment_body_text(value)
  return tostring(value or ""):gsub("\r\n", "\n"):gsub("\r", "\n")
end

---@param state table
---@param comment table
---@return table
function M.normalize_comment(state, comment)
  comment.body = M.normalize_comment_body_text(comment.body)
  if comment.base_body ~= nil then comment.base_body = M.normalize_comment_body_text(comment.base_body) end
  if comment.remote_body ~= nil then comment.remote_body = M.normalize_comment_body_text(comment.remote_body) end
  if not comment.local_id or comment.local_id == "" then
    local seed = table.concat({
      tostring(state.pr and state.pr.repo or ""),
      tostring(state.pr and state.pr.number or ""),
      tostring(comment.path or ""),
      tostring(comment.line or ""),
      tostring(comment.position or ""),
      tostring(comment.body or ""),
      tostring(vim.uv.hrtime()),
    }, "\n")
    comment.local_id = vim.fn.sha256(seed):sub(1, 16)
  end
  if (not comment.abs_file or comment.abs_file == "") and comment.path and comment.path ~= "" then
    comment.abs_file = vim.fs.normalize(vim.fs.joinpath(state.cwd, comment.path))
  end
  comment.side = comment.side or "RIGHT"
  comment.local_state = comment.local_state or (comment.remote_id and "clean" or "new")
  if comment.base_body == nil then comment.base_body = comment.remote_id and comment.body or "" end
  if type(comment.user) == "table" then comment.user = comment.user.login end
  if not comment.user or comment.user == "" then
    local author = type(comment.author) == "table" and comment.author or nil
    local remote_user = type(state.review_remote) == "table" and state.review_remote.user or nil
    comment.user = comment.author_login or (author and author.login) or (remote_user and remote_user.login) or "you"
  end
  comment.created_at = comment.created_at or comment.createdAt or os.date("%Y-%m-%d %H:%M")
  comment.updated_at = comment.updated_at or comment.updatedAt or comment.created_at
  local replies = {}
  for _, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
    if type(reply) == "table" then
      reply.body = M.normalize_comment_body_text(reply.body)
      if type(reply.user) == "table" then reply.user = reply.user.login end
      if not reply.user or reply.user == "" then
        local reply_author = type(reply.author) == "table" and reply.author or nil
        reply.user = reply.author_login or (reply_author and reply_author.login) or "unknown"
      end
      reply.created_at = reply.created_at or reply.createdAt or comment.created_at
      reply.updated_at = reply.updated_at or reply.updatedAt or reply.created_at
      replies[#replies + 1] = reply
    end
  end
  comment.replies = replies
  return comment
end

---@param state table
function M.normalize_comments(state)
  for _, comment in ipairs(state.review_comments or {}) do
    M.normalize_comment(state, comment)
  end
end

---@param state table
---@param remote table
---@return table
function M.comment_from_remote(state, remote)
  local remote_body = M.normalize_comment_body_text(remote.body)
  return M.normalize_comment(state, {
    local_id = remote.remote_node_id or ("remote:" .. tostring(remote.remote_id or "")),
    remote_id = remote.remote_id,
    remote_node_id = remote.remote_node_id,
    remote_review_id = remote.review_id or (state.review_remote and state.review_remote.id),
    review_node_id = remote.review_node_id,
    review_state = remote.review_state,
    path = remote.path,
    abs_file = remote.path and vim.fs.normalize(vim.fs.joinpath(state.cwd, remote.path)) or nil,
    side = "RIGHT",
    line = remote.line,
    start_line = remote.start_line,
    start_side = remote.start_side,
    position = remote.position,
    body = remote_body,
    base_body = remote_body,
    viewer_did_author = remote.viewer_did_author,
    user = remote.user,
    created_at = remote.created_at,
    updated_at = remote.updated_at,
    url = remote.url,
    resolved = remote.resolved,
    outdated = remote.outdated,
    replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or {},
    local_state = "clean",
  })
end

---@param comment table
---@return string
function M.comment_fingerprint(comment)
  return table.concat({
    tostring(comment.path or ""),
    tostring(comment.side or ""),
    tostring(comment.line or ""),
    tostring(comment.position or ""),
  }, "\t")
end

---@param state table
---@param remote table
---@return table?
function M.find_comment_for_remote(state, remote)
  for _, comment in ipairs(state.review_comments or {}) do
    if remote.remote_node_id and comment.remote_node_id == remote.remote_node_id then return comment end
    if remote.remote_id and tonumber(comment.remote_id) == tonumber(remote.remote_id) then return comment end
  end
  local remote_fingerprint = M.comment_fingerprint({
    path = remote.path,
    side = "RIGHT",
    line = remote.line,
    position = remote.position,
  })
  for _, comment in ipairs(state.review_comments or {}) do
    if not comment.remote_id
      and comment.local_state ~= "deleted"
      and M.comment_fingerprint(comment) == remote_fingerprint
      and M.comment_body_for_sync(comment.body or "") == M.comment_body_for_sync(remote.body or "") then
      return comment
    end
  end
  return nil
end

---@param state table
---@param remote_comments table[]
---@return boolean changed
function M.merge_remote_comments(state, remote_comments)
  M.normalize_comments(state)
  local changed = false
  for _, remote in ipairs(remote_comments or {}) do
    local comment = M.find_comment_for_remote(state, remote)
    if comment then
      comment.remote_id = remote.remote_id or comment.remote_id
      comment.remote_node_id = remote.remote_node_id or comment.remote_node_id
      comment.remote_review_id = remote.review_id or comment.remote_review_id
      comment.position = remote.position or comment.position
      comment.line = remote.line or comment.line
      comment.user = remote.user or comment.user
      comment.created_at = remote.created_at or comment.created_at
      comment.updated_at = remote.updated_at or comment.updated_at
      comment.viewer_did_author = remote.viewer_did_author
      local remote_replies = type(remote.replies) == "table" and vim.deepcopy(remote.replies) or {}
      if not vim.deep_equal(comment.replies or {}, remote_replies) then
        comment.replies = remote_replies
        changed = true
      end
      local remote_body = M.normalize_comment_body_text(remote.body)
      local base_body = M.normalize_comment_body_text(comment.base_body)
      local local_body = M.normalize_comment_body_text(comment.body)
      local base_sync_body = M.comment_body_for_sync(base_body)
      local local_sync_body = M.comment_body_for_sync(local_body)
      if comment.local_state == "dirty" or comment.local_state == "new" then
        if remote_body == local_sync_body then
          comment.base_body = remote_body
          comment.local_state = "clean"
          changed = true
        elseif base_body ~= "" and remote_body ~= base_sync_body then
          comment.remote_body = remote_body
          comment.local_state = "conflict"
          changed = true
        end
      elseif comment.local_state ~= "deleted" and local_sync_body ~= remote_body then
        comment.body = remote_body
        comment.base_body = remote_body
        comment.local_state = "clean"
        changed = true
      end
    else
      state.review_comments[#state.review_comments + 1] = M.comment_from_remote(state, remote)
      changed = true
    end
  end
  return changed
end

---@param state table
---@param result table
---@return boolean ok
---@return boolean changed
function M.apply_remote_review_result(state, result)
  if not result.ok then return false, false end
  local comment_count = type(result.comments) == "table" and #result.comments or 0
  if result.review then
    notify_debug(
      ("Pending review %s found; importing %d comment%s"):format(
        tostring(result.review.id or result.review.node_id or "?"),
        comment_count,
        comment_count == 1 and "" or "s"
      ),
      vim.log.levels.INFO,
      { title = "DiffReview" }
    )
  else
    notify_debug("No pending review found", vim.log.levels.INFO, { title = "DiffReview" })
  end
  local changed = false
  if result.review then
    state.review_remote = result.review
    changed = true
  elseif type(state.review_remote) == "table" and (state.review_remote.id or state.review_remote.node_id) then
    state.review_remote = nil
    changed = true
  end
  if M.merge_remote_comments(state, result.comments or {}) then changed = true end
  return true, changed
end

---@param buf integer
---@param cb fun(ok: boolean)
function M.load_remote_before_open(buf, cb)
  local state = M.state(buf)
  if not (state and state.pr and state.pr.repo and state.pr.repo ~= "") then
    notify_debug("PR review sync skipped: missing owner/repo", vim.log.levels.WARN, { title = "DiffReview" })
    cb(true)
    return
  end
  state.review_sync_request_id = (state.review_sync_request_id or 0) + 1
  local request_id = state.review_sync_request_id
  gh.pending_review_async(state.cwd, state.pr.number, state.pr.repo, function(result)
    local latest = M.state(buf)
    if not (latest and latest.review_sync_request_id == request_id and vim.api.nvim_buf_is_valid(buf)) then return end
    local ok, changed = M.apply_remote_review_result(latest, result)
    if not ok then
      notify_error("PR review sync failed: " .. (result.message or "gh failed"), "DiffReview")
      cb(false)
      return
    end
    if changed then M.save_draft(latest) end
    cb(true)
  end)
end

---@param state table
---@param cb fun(ok: boolean)
function M.ensure_remote_review(state, cb)
  local remote = state.review_remote or {}
  if remote.id and remote.node_id and remote.node_id ~= "" then
    cb(true)
    return
  end
  gh.create_pending_review_async(state.cwd, state.pr.number, state.pr.repo, { commit_id = state.commit_id }, function(result)
    if result.ok and result.review then
      state.review_remote = result.review
      M.save_draft(state)
      cb(true)
      return
    end
    notify_error("PR review sync failed: " .. (result.message or "could not create pending review"), "DiffReview")
    cb(false)
  end)
end

---@param buf integer
---Run one queued comment sync operation against GitHub (create/update/delete), restoring
---local comment state on failure. Calls cb(ok) when the operation settles.
---@param buf integer
---@param item { op: "upsert"|"delete", comment: table, body: string? }
---@param cb fun(ok: boolean)
function M.run_sync_item(buf, item, cb)
  local state = M.state(buf)
  if not state then cb(false) return end
  local function finish(ok)
    if not ok then
      if item.op == "upsert" and item.comment then
        item.comment.syncing = nil
        M.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M.render_preserving_inline_cursor(buf) end
      elseif item.op == "delete" and item.comment and item.comment.local_state == "deleted" then
        item.comment.local_state = "clean"
        M.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M.render_preserving_inline_cursor(buf) end
      end
    end
    cb(ok)
  end
  M.ensure_remote_review(state, function(ok)
    if not ok then finish(false) return end
    local comment = item.comment
    if item.op == "delete" then
      if not comment.remote_node_id then finish(true) return end
      gh.delete_review_comment_async(state.cwd, comment.remote_node_id, function(result)
        if not result.ok then
          notify_error("PR review comment delete failed: " .. (result.message or "gh failed"), "DiffReview")
          finish(false) return
        end
        for index, existing in ipairs(state.review_comments or {}) do
          if existing == comment then table.remove(state.review_comments, index) break end
        end
        M.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M.render(buf) end
        finish(true)
      end)
      return
    end
    if comment.local_state == "deleted" then finish(true) return end
    local queued_body = item.body or M.comment_body_for_sync(comment.body or "")
    if comment.remote_node_id then
      gh.update_review_comment_async(state.cwd, comment.remote_node_id, queued_body, function(result)
        if not result.ok then
          notify_error("PR review comment update failed: " .. (result.message or "gh failed"), "DiffReview")
          finish(false) return
        end
        local remote = result.comments and result.comments[1] or nil
        if remote then
          comment.user = remote.user or comment.user
          comment.created_at = remote.created_at or comment.created_at
          comment.updated_at = remote.updated_at or comment.updated_at
        end
        comment.syncing = nil
        if M.comment_body_for_sync(comment.body or "") == queued_body then
          comment.local_state = "clean"
          comment.base_body = queued_body
        else
          comment.local_state = comment.remote_node_id and "dirty" or "new"
        end
        M.save_draft(state)
        if vim.api.nvim_buf_is_valid(buf) then M.render_preserving_inline_cursor(buf) end
        finish(true)
      end)
      return
    end
    if not comment.position then
      notify_error("PR review comment sync failed: missing diff position for " .. tostring(comment.path or ""), "DiffReview")
      finish(false) return
    end
    gh.add_pending_review_comment_async(state.cwd, state.review_remote.node_id, {
      body = queued_body,
      path = comment.path,
      position = comment.position,
      commit_id = state.commit_id,
    }, function(result)
      if not result.ok then
        notify_error("PR review comment create failed: " .. (result.message or "gh failed"), "DiffReview")
        finish(false) return
      end
      local remote = result.comments and result.comments[1] or nil
      if remote then
        comment.remote_id = remote.remote_id
        comment.remote_node_id = remote.remote_node_id
        comment.remote_review_id = state.review_remote.id
        comment.line = remote.line or comment.line
        comment.position = remote.position or comment.position
        comment.user = remote.user or comment.user
        comment.created_at = remote.created_at or comment.created_at
        comment.updated_at = remote.updated_at or comment.updated_at
      end
      comment.syncing = nil
      if M.comment_body_for_sync(comment.body or "") == queued_body then
        comment.local_state = "clean"
        comment.base_body = queued_body
      else
        comment.local_state = "dirty"
      end
      M.save_draft(state)
      if vim.api.nvim_buf_is_valid(buf) then M.render_preserving_inline_cursor(buf) end
      finish(true)
    end)
  end)
end

---Build (once) the per-buffer comment sync queue backed by the canonical annotation sync
---drain: serial execution, op-id stale rejection, and stop-on-failure with draft waiters.
---@param buf integer
---@param state table
---@return DiffReviewAnnotationSyncQueue
function M.ensure_sync_queue(buf, state)
  if state.review_sync then return state.review_sync end
  state.review_sync_waiters = {}
  local function resolve_waiters(ok)
    local waiters = state.review_sync_waiters or {}
    state.review_sync_waiters = {}
    for _, waiter in ipairs(waiters) do waiter(ok) end
  end
  state.review_sync = annotations.new_sync_queue(function(item, done)
    M.run_sync_item(buf, item, function(ok)
      if not ok then resolve_waiters(false) end
      done(ok)
    end)
  end, {
    on_success = function() end,
    on_failure = function() end,
    stop_on_failure = true,
    on_idle = function() resolve_waiters(true) end,
  })
  return state.review_sync
end

---@param buf integer
function M.process_sync_queue(buf)
  local state = M.state(buf)
  if not state then return end
  annotations.drain_sync_queue(M.ensure_sync_queue(buf, state))
end

---@param buf integer
---@param op "upsert"|"delete"
---@param comment table
function M.enqueue_sync(buf, op, comment)
  local state = M.state(buf)
  if not state then return end
  local queue = M.ensure_sync_queue(buf, state)
  local sync_body = op == "upsert" and M.comment_body_for_sync(comment.body or "") or nil
  if op == "upsert" then
    for _, queued in ipairs(queue.pending) do
      if queued.op == "upsert" and queued.comment == comment then
        queued.body = sync_body
        return
      end
    end
  end
  queue.pending[#queue.pending + 1] = { op = op, comment = comment, body = sync_body }
  annotations.drain_sync_queue(queue)
end

---@param comment table
---@return boolean
function M.comment_needs_sync(comment)
  if not comment or comment.syncing then return false end
  if comment.local_state == "deleted" then return comment.remote_node_id ~= nil end
  if not comment.remote_node_id and M.comment_body_for_sync(comment.body or "") == "" then return false end
  if comment.local_state == "new" or comment.local_state == "dirty" then return true end
  return not comment.remote_node_id
end

---@param comment table
---@return boolean
function M.comment_has_dirty_marker(comment)
  if not comment or comment.syncing or comment.local_state == "deleted" then return false end
  return M.comment_needs_sync(comment)
end

---@param buf integer
---@return integer count
function M.enqueue_dirty_comments(buf)
  local state = M.state(buf)
  if not state then return 0 end
  local items = {}
  local render_marker_change = false
  for _, comment in ipairs(state.review_comments or {}) do
    if M.comment_needs_sync(comment) then
      if comment.local_state == "deleted" then
        items[#items + 1] = { op = "delete", comment = comment }
      else
        comment.syncing = true
        render_marker_change = true
        items[#items + 1] = { op = "upsert", comment = comment }
      end
    end
  end
  if #items == 0 then return 0 end
  if render_marker_change then M.render_preserving_inline_cursor(buf) end
  for _, item in ipairs(items) do
    M.enqueue_sync(buf, item.op, item.comment)
  end
  return #items
end

---@param buf integer
---@param cb fun(ok: boolean)
function M.flush_sync(buf, cb)
  local state = M.state(buf)
  if not state then cb(false) return end
  M.enqueue_dirty_comments(buf)
  local queue = M.ensure_sync_queue(buf, state)
  if not queue.running and #queue.pending == 0 then
    cb(true)
    return
  end
  state.review_sync_waiters[#state.review_sync_waiters + 1] = cb
end

---@param buf integer
function M.sync_remote(buf)
  local state = M.state(buf)
  if not (state and state.pr and state.pr.repo and state.pr.repo ~= "") then return end
  state.review_sync_request_id = (state.review_sync_request_id or 0) + 1
  local request_id = state.review_sync_request_id
  gh.pending_review_async(state.cwd, state.pr.number, state.pr.repo, function(result)
    local latest = M.state(buf)
    if not (latest and latest.review_sync_request_id == request_id and vim.api.nvim_buf_is_valid(buf)) then return end
    if not result.ok then
      notify_error("PR review sync failed: " .. (result.message or "gh failed"), "DiffReview")
      return
    end
    local _, changed = M.apply_remote_review_result(latest, result)
    if changed then
      M.save_draft(latest)
      M.render(buf)
    end
  end)
end

function M.leave_visual()
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
  end
end

---@param dl table diff_line meta
---@return "LEFT"|"RIGHT"
function M.side_of(dl)
  return dl.side == "left" and "LEFT" or "RIGHT"
end

---@param path? string
---@param side? string
---@param line? integer|string
---@param end_line? integer|string
---@return string?
function M.github_diff_fragment(path, side, line, end_line)
  local line_number = tonumber(line)
  if not (path and path ~= "" and line_number) then return nil end
  local normalized_path = tostring(path):gsub("\\", "/")
  local side_prefix = side == "LEFT" and "L" or "R"
  local start_number = math.floor(line_number)
  local stop_number = tonumber(end_line)
  if stop_number then
    local finish_number = math.floor(stop_number)
    if finish_number ~= start_number then
      if finish_number < start_number then start_number, finish_number = finish_number, start_number end
      return ("diff-%s%s%d-%s%d"):format(vim.fn.sha256(normalized_path), side_prefix, start_number, side_prefix, finish_number)
    end
  end
  return ("diff-%s%s%d"):format(vim.fn.sha256(normalized_path), side_prefix, start_number)
end

---@param state table
---@return string?
function M.visual_browse_fragment(state)
  local mode = vim.fn.mode()
  if not (mode == "v" or mode == "V" or mode:byte() == 22) then return nil end

  local start_row, end_row = vim.fn.line("v"), vim.fn.line(".")
  if start_row > end_row then start_row, end_row = end_row, start_row end

  local first, last
  for row = start_row, end_row do
    local entry = state.entries and state.entries[row] or nil
    local diff_line = entry and entry.diff_line or nil
    if diff_line then
      first = first or diff_line
      last = diff_line
    end
  end
  if not (first and last and first.file == last.file and first.side == last.side) then return nil end

  local path = repo_relative(first.file, state.cwd) or first.file
  return M.github_diff_fragment(path, M.side_of(first), first.line, last.line)
end

---@param buf integer
---@return string?
function M.browse_fragment_under_cursor(buf)
  local state = M.state(buf)
  if not state then return nil end
  local visual_fragment = M.visual_browse_fragment(state)
  if visual_fragment then return visual_fragment end

  local row = vim.api.nvim_win_get_cursor(0)[1]
  local entry = state.entries and state.entries[row] or nil

  local function comment_fragment(comment)
    if not comment then return nil end
    local remote_id = tonumber(comment.remote_id)
    if remote_id then return ("r%d"):format(math.floor(remote_id)) end
    return M.github_diff_fragment(comment.path, comment.side, comment.line)
  end

  local body_comment = M.comment_body_at_row(buf, row)
  if body_comment then return comment_fragment(body_comment) end

  if not entry then return nil end

  if entry.kind == "review_comment" and entry.review_reply then
    return comment_fragment(entry.review_reply)
  end

  if entry.kind == "review_comment" and entry.review_comment then
    return comment_fragment(entry.review_comment)
  end

  local diff_line = entry.diff_line
  if diff_line then
    local path = repo_relative(diff_line.file, state.cwd) or diff_line.file
    return M.github_diff_fragment(path, M.side_of(diff_line), diff_line.line)
  end

  return nil
end

---@param state table
---@return DiffReviewStatusHeadLine[]
function M.head_lines(state)
  local pr = state.pr
  local title = pr.title ~= "" and pr.title or ("PR #" .. tostring(pr.number))
  local lines = {
    { segments = { { "Title: ", "DiffReviewStatusLabel" }, { title, "DiffReviewStatusPath" } } },
    { segments = { { "Review Comment:", "DiffReviewReviewCommentHeader" } } },
  }
  local comment_lines = vim.split(state.review_comment_text or "", "\n", { plain = true })
  if #comment_lines == 0 then comment_lines = { "" } end
  for _, line in ipairs(comment_lines) do
    lines[#lines + 1] = { segments = { { line, "DiffReviewReviewComment" } } }
  end
  return lines
end

---@param diff string?
---@return string
function M.normalized_hunk_diff(diff)
  local body = {}
  local in_hunk = false
  for line in tostring(diff or ""):gmatch("[^\n]+") do
    if line:match("^@@") then
      local context = line:match("^@@.-@@%s*(.*)$") or ""
      body[#body + 1] = "@@ " .. context
      in_hunk = true
    elseif in_hunk and line:match("^[ +%-]") then
      body[#body + 1] = line
    end
  end
  if #body == 0 then return tostring(diff or "") end
  return table.concat(body, "\n")
end

---@param state table
---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@return string
function M.hunk_view_key(state, file, hunk)
  local relpath = file.relpath or hunk.file or repo_relative(file.filename, state.cwd) or file.filename
  return "hunk:" .. vim.fn.sha256(relpath .. "\n" .. M.normalized_hunk_diff(hunk.diff))
end

---@param file DiffReviewStatusFile
---@param hunks DiffReviewHunk[]
---@param section_name string
---@return DiffReviewStatusFile
function M.file_with_hunks(file, hunks, section_name)
  return dr()._section_builder.file_with_hunks(file, hunks, section_name)
end

---@param state table
---@param file DiffReviewStatusFile
---@param hunk DiffReviewHunk
---@param key string
---@return boolean
function M.hunk_is_viewed(state, file, hunk, key)
  state.review_viewed_hunks = state.review_viewed_hunks or {}
  if state.review_viewed_hunks[key] ~= nil then return true end
  local relpath = file.relpath or hunk.file or repo_relative(file.filename, state.cwd) or file.filename
  if state.review_viewed[relpath] and state.review_draft_commit_id == state.commit_id then
    state.review_viewed_hunks[key] = {
      file = relpath,
      commit_id = state.commit_id,
    }
    state.review_viewed_dirty = true
    return true
  end
  return false
end

---@param state table
---@param active_keys table<string, boolean>
function M.prune_viewed_hunks(state, active_keys)
  state.review_viewed_hunks = state.review_viewed_hunks or {}
  for key in pairs(state.review_viewed_hunks) do
    if not active_keys[key] then
      state.review_viewed_hunks[key] = nil
      state.review_viewed_dirty = true
    end
  end
end

---@param state table
---@return DiffReviewStatusSection[]
function M.sections(state)
  local files = dr()._section_builder.files_from_diff(state.cwd, {
    section_name = "review",
    default_status = "",
    files = state.pr.files,
  }, state.diff_text or "")
  local unviewed, viewed = {}, {}
  local active_keys = {}
  for _, file in ipairs(files) do
    local file_viewed_hunks = {}
    local file_unviewed_hunks = {}
    local hunks = file.hunks or {}
    if #hunks == 0 then
      if state.review_viewed[file.relpath] then
        viewed[#viewed + 1] = file
      else
        unviewed[#unviewed + 1] = file
      end
    else
      for _, hunk in ipairs(hunks) do
        local key = M.hunk_view_key(state, file, hunk)
        active_keys[key] = true
        if M.hunk_is_viewed(state, file, hunk, key) then
          file_viewed_hunks[#file_viewed_hunks + 1] = hunk
        else
          file_unviewed_hunks[#file_unviewed_hunks + 1] = hunk
        end
      end
      if #file_unviewed_hunks > 0 then
        unviewed[#unviewed + 1] = M.file_with_hunks(file, file_unviewed_hunks, "review:unviewed")
      end
      if #file_viewed_hunks > 0 then
        viewed[#viewed + 1] = M.file_with_hunks(file, file_viewed_hunks, "review:viewed")
      end
    end
  end
  if state.diff_text and state.diff_text ~= "" then
    M.prune_viewed_hunks(state, active_keys)
  end
  return {
    dr()._section_builder.section_from_files("Unviewed Changes", unviewed, {
      name = "review:unviewed",
      keep_empty = true,
      file_key_prefix = "review:unviewed",
      file_entry_kind = "pr_review_file",
      hunk_entry_kind = "pr_review_hunk",
    }),
    dr()._section_builder.section_from_files("Viewed Changes", viewed, {
      name = "review:viewed",
      keep_empty = true,
      file_key_prefix = "review:viewed",
      file_entry_kind = "pr_review_file",
      hunk_entry_kind = "pr_review_hunk",
    }),
  }
end

--- Files fold by default; a review wants every diff open so the reviewer can
--- read and comment. Expand each file once (user <Tab> toggles then persist).
---@param state table
function M.ensure_expanded(state)
  if state.review_expanded or not state.diff_text or state.diff_text == "" then return end
  state.review_expanded = true
  local files = dr()._section_builder.files_from_diff(state.cwd, {
    section_name = "review",
    default_status = "",
    files = state.pr.files,
  }, state.diff_text)
  for _, file in ipairs(files) do
    for _, prefix in ipairs({ "review:unviewed", "review:viewed" }) do
      set_status_folded(status_provider_file_key(prefix, file.filename), false)
    end
  end
end

---@param buf integer
function M.render(buf)
  local state = M.state(buf)
  if not state then return end
  dr()._status = state
  M.ensure_expanded(state)
  state.head_lines = M.head_lines(state)
  state.sections = M.sections(state)
  if state.review_viewed_dirty then
    state.review_viewed_dirty = nil
    M.save_draft(state)
  end
  state.fancy_rows = {}
  state.review_rendered_comment_count = 0
  state.review_comment_anchor_index = dr()._section_builder.comment_anchor_index(state.review_comments)
  -- status_add_fancy_row consults this hook to interleave comment lines.
  state.review_after_row = function(diff_line, indent)
    M.emit_comments_for(state, diff_line, indent)
  end
  state.review_rendering = true
  status_render_loaded(buf, nil, nil, { reuse_sections = true }, state.head_lines, state.sections)
  state.review_rendering = nil
  local total_comments = 0
  for _, comment in ipairs(state.review_comments or {}) do
    if comment.local_state ~= "deleted" then total_comments = total_comments + 1 end
  end
  notify_debug(
    ("Rendered %d/%d review comment%s"):format(
      state.review_rendered_comment_count or 0,
      total_comments,
      total_comments == 1 and "" or "s"
    ),
    vim.log.levels.INFO,
    { title = "DiffReview" }
  )
end

---@param buf integer
function M.render_preserving_inline_cursor(buf)
  local comment
  local snapshot
  if buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf then
    local cursor = vim.api.nvim_win_get_cursor(0)
    comment = M.comment_body_at_row(buf, cursor[1])
    if comment then snapshot = M.inline_comment_cursor_snapshot(buf, comment) end
  end
  M.render(buf)
  if comment and snapshot and comment.local_state ~= "deleted" then
    M.restore_inline_comment_cursor(buf, comment, snapshot)
  end
end

---@param value any
---@return string
function M.format_comment_datetime(value)
  local relative = dr()._datetime.relative(value)
  if relative ~= "" then return relative end
  return "just now"
end

---@param text string
---@param width integer
---@return string
function M.truncate_display(text, width)
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(text) <= width then return text end
  local output = ""
  local output_width = 0
  local char_count = vim.fn.strchars(text)
  for char_index = 0, char_count - 1 do
    local char = vim.fn.strcharpart(text, char_index, 1)
    local char_width = vim.fn.strdisplaywidth(char)
    if output_width + char_width > width then break end
    output = output .. char
    output_width = output_width + char_width
  end
  return output
end

---@param comment table
---@return string left_text
---@return string right_text
function M.comment_header_text(comment)
  local user = tostring(comment.user or "you")
  local marker = M.comment_has_dirty_marker(comment) and "*" or ""
  local left_text = ("%s %s "):format(
    M.comment_icon,
    dr()._datetime.action_phrase(marker .. user, "commented", comment.updated_at or comment.created_at)
  )
  local right_text = ""
  if not comment.pr_issue_comment then
    local line_number = tonumber(comment.line)
    right_text = line_number and (" L%d"):format(line_number) or " L?"
  end
  return left_text, right_text
end

---@param reply table
---@return string left_text
---@return string right_text
function M.reply_header_text(reply)
  local user = tostring(reply.user or "unknown")
  return ("%s %s "):format(
    M.reply_icon,
    dr()._datetime.action_phrase(user, "replied", reply.updated_at or reply.created_at)
  ), ""
end

---@param win integer?
---@param buf integer?
---@return integer
function M.comment_rule_width(win, buf)
  local width = tonumber(vim.o.columns) or 80
  if win and win > 0 and vim.api.nvim_win_is_valid(win) then
    width = vim.api.nvim_win_get_width(win)
    local wininfo = vim.fn.getwininfo(win)[1]
    local textoff = tonumber(wininfo and wininfo.textoff) or 0
    width = width - textoff
  else
    local status = dr()._status
    buf = buf or (status and status.buf) or nil
    if buf and vim.api.nvim_buf_is_valid(buf) then
      local displayed_win = vim.fn.bufwinid(buf)
      if displayed_win and displayed_win > 0 and vim.api.nvim_win_is_valid(displayed_win) then
        width = vim.api.nvim_win_get_width(displayed_win)
        local wininfo = vim.fn.getwininfo(displayed_win)[1]
        local textoff = tonumber(wininfo and wininfo.textoff) or 0
        width = width - textoff
      end
    end
  end
  return math.max(40, width - 1)
end

---@param left_text string
---@param right_text string
---@param win integer?
---@param buf integer?
---@return string
function M.comment_rule_line(left_text, right_text, win, buf)
  local width = M.comment_rule_width(win, buf)
  left_text = tostring(left_text or "")
  right_text = tostring(right_text or "")
  local fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  if fixed_width > width then
    local available_left_width = math.max(0, width - vim.fn.strdisplaywidth(right_text))
    left_text = M.truncate_display(left_text, available_left_width)
    fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  end
  if fixed_width > width then
    right_text = M.truncate_display(right_text, math.max(0, width - vim.fn.strdisplaywidth(left_text)))
    fixed_width = vim.fn.strdisplaywidth(left_text .. right_text)
  end
  return left_text .. ("-"):rep(math.max(width - fixed_width, 0)) .. right_text
end

---@param comment table
---@param win integer?
---@param buf integer?
---@return string
function M.comment_header_line(comment, win, buf)
  local left_text, right_text = M.comment_header_text(comment)
  return M.comment_rule_line(left_text, right_text, win, buf)
end

---@param reply table
---@param win integer?
---@param buf integer?
---@return string
function M.reply_header_line(reply, win, buf)
  local left_text, right_text = M.reply_header_text(reply)
  return M.comment_rule_line(left_text, right_text, win, buf)
end

---@param win integer?
---@param buf integer?
---@return string
function M.comment_footer_line(win, buf)
  local width = M.comment_rule_width(win, buf)
  return ("-"):rep(width)
end

---@param body string
---@return string
function M.comment_preview_text(body)
  return dr()._comment_rows.preview_text(body)
end

---@param text string
---@param width integer
---@return string
function M.truncate_preview_text(text, width)
  text = tostring(text or "")
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(text) <= width then return text end
  if width <= 4 then return M.truncate_display(text, width) end
  return M.truncate_display(text, width - 4) .. " ..."
end

---@param comment table
---@param win integer?
---@param buf integer?
---@return string
function M.comment_folded_line(comment, win, buf)
  local user = tostring(comment.user or "you")
  local marker = M.comment_has_dirty_marker(comment) and "*" or ""
  local left_text = ("%s %s | "):format(
    M.comment_icon,
    dr()._datetime.action_phrase(marker .. user, "commented", comment.updated_at or comment.created_at)
  )
  local width = M.comment_rule_width(win, buf)
  local preview_width = math.max(0, width - vim.fn.strdisplaywidth(left_text))
  local preview = M.truncate_preview_text(M.comment_preview_text(comment.body or ""), preview_width)
  return M.truncate_display(left_text .. preview, width)
end

---@param line_number integer
---@param text string
function M.add_comment_rule_date_highlights(line_number, text)
  for _, range in ipairs(dr()._datetime.date_highlight_ranges(text)) do
    status_add_highlight(line_number, range.start_col, range.end_col, "DiffReviewStatusDate")
  end
end

---@param buf integer
---@param row0 integer
---@param text string
function M.set_comment_rule_date_extmarks(buf, row0, text)
  for _, range in ipairs(dr()._datetime.date_highlight_ranges(text)) do
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, row0, range.start_col, {
      end_col = range.end_col,
      hl_group = "DiffReviewStatusDate",
      priority = 95,
    })
  end
end

---@param body string
---@return string[]
function M.comment_body_lines(body)
  local lines = vim.split(M.normalize_comment_body_text(body), "\n", { plain = true })
  if #lines == 0 then lines = { "" } end
  return lines
end

---@param comment table
---@param index integer?
---@return string
function M.comment_fold_id(comment, index)
  local value = comment and (comment.local_id or comment.remote_node_id or comment.remote_id or comment.id) or nil
  if value and value ~= "" then return "review-comment:" .. tostring(value) end
  return "review-comment:" .. tostring(index or 1)
end

---@param comment table
---@param comment_index integer
---@param reply table
---@param reply_index integer
function M.emit_comment_reply(comment, comment_index, reply, reply_index)
  local fold_id = comment.review_fold_id or M.comment_fold_id(comment, comment_index)
  local header_entry = {
    id = fold_id,
    kind = "review_comment",
    review_comment = comment,
    comment_index = comment_index,
    review_reply = reply,
    review_reply_index = reply_index,
    review_boundary = "reply_header",
  }
  local header = M.reply_header_line(reply)
  local header_line = status_add_line(header, header_entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(header_line, 0, #header, "DiffReviewReviewCommentHeader")
  M.add_comment_rule_date_highlights(header_line, header)

  for body_index, line in ipairs(M.comment_body_lines(reply.body or "")) do
    local body_entry = {
      id = fold_id,
      kind = "review_comment",
      review_comment = comment,
      comment_index = comment_index,
      review_reply = reply,
      review_reply_index = reply_index,
      review_reply_body = true,
      review_reply_body_index = body_index,
    }
    local body_line = status_add_line(line, body_entry)
    if line ~= "" then status_add_highlight(body_line, 0, #line, "DiffReviewReviewComment") end
  end
end

---Emit a draft comment as plain, navigable buffer lines below its anchor diff
---row. Header/footer are read-only rule rows; body rows are full-width editable
---buffer lines with no prefixes, virtual borders, or render wrapping.
---@param comment table
---@param index integer comment number (1-based)
---@param indent integer unused; retained for the review_after_row hook shape
function M.emit_comment(comment, index, indent)
  local body_lines = M.comment_body_lines(comment.body or "")
  comment.review_rendered_body_text = table.concat(body_lines, "\n")
  local fold_id = M.comment_fold_id(comment, index)
  comment.review_fold_id = fold_id
  local start_line = #dr()._status.lines + 1
  if comment.review_folded == true then
    local folded_entry = {
      id = fold_id,
      kind = "review_comment",
      review_comment = comment,
      comment_index = index,
      review_first = true,
      review_boundary = "folded",
    }
    local folded_line = M.comment_folded_line(comment)
    local line_number = status_add_line(folded_line, folded_entry, "DiffReviewReviewCommentHeader")
    status_add_highlight(line_number, 0, #folded_line, "DiffReviewReviewCommentHeader")
    M.add_comment_rule_date_highlights(line_number, folded_line)
    dr()._status_register_fold_range(fold_id, start_line, line_number, true, folded_line)
    return
  end

  local header_entry = {
    id = fold_id,
    kind = "review_comment",
    review_comment = comment,
    comment_index = index,
    review_first = true,
    review_boundary = "header",
  }
  local header = M.comment_header_line(comment)
  local header_line = status_add_line(header, header_entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(header_line, 0, #header, "DiffReviewReviewCommentHeader")
  M.add_comment_rule_date_highlights(header_line, header)

  for body_index, line in ipairs(body_lines) do
    local body_entry = {
      id = fold_id,
      kind = "review_comment",
      review_comment = comment,
      comment_index = index,
      review_body = true,
      review_body_index = body_index,
    }
    local body_line = status_add_line(line, body_entry)
    if line ~= "" then status_add_highlight(body_line, 0, #line, "DiffReviewReviewComment") end
  end

  for reply_index, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
    if type(reply) == "table" then M.emit_comment_reply(comment, index, reply, reply_index) end
  end

  local footer_entry = {
    id = fold_id,
    kind = "review_comment",
    review_comment = comment,
    comment_index = index,
    review_boundary = "footer",
  }
  local footer = M.comment_footer_line()
  local footer_line = status_add_line(footer, footer_entry, "DiffReviewReviewCommentHeader")
  status_add_highlight(footer_line, 0, #footer, "DiffReviewReviewCommentHeader")
  dr()._status_register_fold_range(fold_id, start_line, footer_line, comment.review_folded == true, function()
    return M.comment_folded_line(comment)
  end)
end

---Renderer hook: emit any draft comments anchored on `diff_line`.
---@param state table
---@param diff_line table
---@param indent integer
function M.emit_comments_for(state, diff_line, indent)
  dr()._section_builder.emit_anchored_comments(state, diff_line, indent, {
    index_field = "review_comment_anchor_index",
    count_field = "review_rendered_comment_count",
    skip = function(comment) return comment == state.review_editing_comment end,
  })
end

---@param buf integer
---@param row integer
---@return table? comment
---@return integer? index
function M.comment_at_row(buf, row)
  local state = M.state(buf)
  if not state then return nil end
  local entry = state.entries and state.entries[row] or nil
  if entry and entry.kind == "review_comment" then
    return entry.review_comment, entry.comment_index
  end
  if entry and entry.kind == "pr_comment" and entry.pr_comment then
    for index, comment in ipairs(state.review_comments or {}) do
      if M.same_comment(comment, entry.pr_comment) then return comment, index end
    end
    return nil
  end
  local body_comment = M.comment_body_at_row(buf, row)
  if body_comment then
    for index, comment in ipairs(state.review_comments or {}) do
      if M.same_comment(comment, body_comment) then return comment, index end
    end
  end
  return nil
end

---@param buf integer
---@return table? comment
---@return integer? index
function M.comment_under_cursor(buf)
  local state = M.state(buf)
  if not state then return nil end
  dr()._status = state
  local cursor_row = vim.api.nvim_win_get_cursor(0)[1]
  for _, row in ipairs({ cursor_row, cursor_row - 1, cursor_row + 1 }) do
    local comment, index = M.comment_at_row(buf, row)
    if comment then return comment, index end
  end
  return nil
end

---@param buf integer
---@return table? comment
---@return integer? index
function M.comment_at_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local row = vim.api.nvim_win_get_cursor(0)[1]
  return M.comment_at_row(buf, row)
end

---@param buf integer
---@return boolean toggled
function M.toggle_comment_fold(buf)
  local state = M.state(buf)
  if not state then return false end
  dr()._status = state
  M.sync_inline_comment_text(buf)
  if state.view_kind == "pr" then
    local row = vim.api.nvim_win_get_cursor(0)[1]
    local entry = state.entries and state.entries[row] or nil
    if entry and entry.kind == "pr_comment" and entry.id and entry.pr_comment then
      local folded = not status_folded(entry.id, true)
      set_status_folded(entry.id, folded)
      if not dr()._status_set_native_fold_state(buf, entry.id, folded) then
        render_pr_status(state.pr, state.cwd, buf, state.pr_diff_text)
      end
      return true
    end
  end
  local comment, index = M.comment_at_cursor(buf)
  if not comment then return false end
  comment.review_folded = not comment.review_folded
  if state.view_kind == "review" then
    M.save_draft(state)
  end
  local fold_id = comment.review_fold_id or M.comment_fold_id(comment, index)
  set_status_folded(fold_id, comment.review_folded == true)
  if not dr()._status_set_native_fold_state(buf, fold_id, comment.review_folded == true) then
    if state.view_kind == "pr" then
      render_pr_status(state.pr, state.cwd, buf, state.pr_diff_text)
    else
      M.render(buf)
    end
  end
  local header_row0 = M.comment_header_row0(buf, comment)
  if header_row0 ~= nil and vim.api.nvim_get_current_buf() == buf then
    pcall(vim.api.nvim_win_set_cursor, 0, { header_row0 + 1, 0 })
    M.sync_modifiable(buf)
  end
  return true
end

---Re-anchor editable review regions after a render wipes the buffer. Inline
---comment body rows are emitted during the render and tracked with extmarks.
---@param buf integer
function M.on_render(buf)
  local state = M.state(buf)
  if not state then return end
  vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
  state.review_comment_region = nil
  for _, comment in ipairs(state.review_comments or {}) do
    comment.review_header_mark = nil
    comment.review_body_region = nil
    comment.review_footer_mark = nil
    comment.review_reply_header_marks = nil
    comment.review_body_line_marks = nil
    comment.review_body_render_rows = nil
    comment.review_body_render_row_count = nil
    comment.review_markdown_ranges = nil
  end

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local label_row
  for index, line in ipairs(lines) do
    if line == "Review Comment:" then
      label_row = index
      break
    end
  end
  if label_row then
    local count = #vim.split(state.review_comment_text or "", "\n", { plain = true })
    if count == 0 then count = 1 end
    -- Start mark: left gravity so retyping the first line keeps the mark on it.
    -- End mark: right gravity so it stays past the region and follows the
    -- region as it grows/shrinks (a left-gravity end mark at the boundary gets
    -- pulled into an edit of the adjacent line).
    state.review_comment_region = region.new(buf, M.ns, label_row, label_row + count, { region_kind = "review_comment", editable = true })
  end

  local range_comment, range_start
  local function close_comment_range(end_row)
    if range_comment and range_start and end_row >= range_start then
      range_comment.review_body_region = region.new(buf, M.ns, range_start - 1, end_row, { region_kind = "review_body", editable = true })
    end
    range_comment, range_start = nil, nil
  end
  local markdown_range_comment, markdown_range_start
  local function close_markdown_range(end_row)
    if markdown_range_comment and markdown_range_start and end_row >= markdown_range_start then
      markdown_range_comment.review_markdown_ranges = markdown_range_comment.review_markdown_ranges or {}
      markdown_range_comment.review_markdown_ranges[#markdown_range_comment.review_markdown_ranges + 1] = {
        start_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, markdown_range_start - 1, 0, { right_gravity = false }),
        end_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, end_row, 0, { right_gravity = true }),
      }
    end
    markdown_range_comment, markdown_range_start = nil, nil
  end

  for row = 1, #lines do
    local entry = state.entries and state.entries[row] or nil
    if entry
      and entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body then
      close_comment_range(row - 1)
      close_markdown_range(row - 1)
      entry.pr_comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, row - 1, 0, { right_gravity = false })
    elseif entry
      and entry.kind == "review_comment"
      and (entry.review_boundary == "header" or entry.review_boundary == "folded")
      and entry.review_comment then
      close_comment_range(row - 1)
      close_markdown_range(row - 1)
      entry.review_comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, row - 1, 0, { right_gravity = false })
    elseif entry
      and entry.kind == "review_comment"
      and entry.review_boundary == "reply_header"
      and entry.review_comment then
      close_comment_range(row - 1)
      close_markdown_range(row - 1)
      local comment = entry.review_comment
      comment.review_reply_header_marks = comment.review_reply_header_marks or {}
      if entry.review_reply_index then
        comment.review_reply_header_marks[entry.review_reply_index] = vim.api.nvim_buf_set_extmark(buf, M.ns, row - 1, 0, { right_gravity = false })
      end
    elseif entry and entry.kind == "review_comment" and entry.review_boundary == "footer" and entry.review_comment then
      close_comment_range(row - 1)
      close_markdown_range(row - 1)
      entry.review_comment.review_footer_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, row - 1, 0, { right_gravity = true })
    elseif entry and ((entry.review_body and entry.review_comment) or (entry.pr_comment_body and entry.pr_comment)) then
      close_markdown_range(row - 1)
      local comment = entry.review_comment or entry.pr_comment
      local body_index = entry.review_body_index or entry.pr_comment_body_index
      comment.review_body_line_marks = comment.review_body_line_marks or {}
      local mark_id = vim.api.nvim_buf_set_extmark(buf, M.ns, row - 1, 0, { right_gravity = false })
      comment.review_body_line_marks[#comment.review_body_line_marks + 1] = { id = mark_id, line_index = body_index }
      if comment ~= range_comment then
        close_comment_range(row - 1)
        range_comment, range_start = comment, row
      end
    elseif entry and entry.kind == "review_comment" and entry.review_reply_body and entry.review_comment then
      close_comment_range(row - 1)
      if entry.review_comment ~= markdown_range_comment then
        close_markdown_range(row - 1)
        markdown_range_comment, markdown_range_start = entry.review_comment, row
      end
    else
      close_comment_range(row - 1)
      close_markdown_range(row - 1)
    end
  end
  close_comment_range(#lines)
  close_markdown_range(#lines)
  state.review_comment_rule_width = M.comment_rule_width(nil, buf)
  if state.view_kind == "review" then M.sync_modifiable(buf) end
end

---@param buf integer
---@param id integer?
---@return integer? row0
function M.mark_row(buf, id)
  if not id then return nil end
  local pos = vim.api.nvim_buf_get_extmark_by_id(buf, M.ns, id, {})
  return pos and pos[1] or nil
end

---@param left table?
---@param right table?
---@return boolean
function M.same_comment(left, right)
  if not (left and right) then return false end
  if left == right then return true end
  if left.local_id and right.local_id and left.local_id == right.local_id then return true end
  if left.remote_node_id and right.remote_node_id and left.remote_node_id == right.remote_node_id then return true end
  if left.remote_id and right.remote_id and tonumber(left.remote_id) == tonumber(right.remote_id) then return true end
  return false
end

---@param buf integer
---@param comment table
---@return integer? row0
function M.comment_header_row0(buf, comment)
  local mark_row = M.mark_row(buf, comment.review_header_mark)
  if mark_row ~= nil then return mark_row end
  local state = M.state(buf)
  for row, entry in pairs((state and state.entries) or {}) do
    if entry.kind == "review_comment"
      and (entry.review_boundary == "header" or entry.review_boundary == "folded")
      and M.same_comment(entry.review_comment, comment) then
      return row - 1
    elseif entry.kind == "pr_comment"
      and entry.pr_comment
      and not entry.pr_comment_body
      and M.same_comment(entry.pr_comment, comment) then
      return row - 1
    end
  end
  return nil
end

---@param buf integer
---@param comment table
---@return integer? row0
function M.comment_footer_row0(buf, comment)
  local mark_row = M.mark_row(buf, comment.review_footer_mark)
  if mark_row ~= nil then return mark_row end
  local state = M.state(buf)
  for row, entry in pairs((state and state.entries) or {}) do
    if entry.kind == "review_comment" and entry.review_boundary == "footer" and M.same_comment(entry.review_comment, comment) then
      return row - 1
    end
  end
  return nil
end

---@param buf integer
---@param comment table
---@return integer? start_row0
---@return integer? end_row0
function M.comment_body_range0(buf, comment)
  if not comment.review_body_region then return nil, nil end
  local start_row0, end_row0 = region.bounds(comment.review_body_region)
  if start_row0 == nil or end_row0 == nil or end_row0 < start_row0 then return nil, nil end
  return start_row0, end_row0
end

---@param buf integer
---@param row integer 1-based
---@return boolean
function M.in_comment_region(buf, row)
  local state = M.state(buf)
  if not state then return false end
  local s0, e0
  if state.review_comment_region then s0, e0 = region.bounds(state.review_comment_region) end
  return s0 ~= nil and e0 ~= nil and row >= s0 + 1 and row <= e0
end

---@param buf integer
---@param row integer 1-based
---@return table? comment
function M.comment_body_at_row(buf, row)
  local state = M.state(buf)
  if not state then return nil end
  for _, comment in ipairs(state.review_comments or {}) do
    if comment.local_state ~= "deleted" then
      local start_row0, end_row0 = M.comment_body_range0(buf, comment)
      if start_row0 ~= nil and end_row0 ~= nil and row >= start_row0 + 1 and row <= end_row0 then
        return comment
      end
    end
  end
  return nil
end

---@param buf integer
---@param comment table
---@return integer? start_row 1-based
---@return integer? end_row 1-based
function M.comment_body_rows(buf, comment)
  local start_row0, end_row0 = M.comment_body_range0(buf, comment)
  if start_row0 == nil or end_row0 == nil or end_row0 < start_row0 then return nil, nil end
  return start_row0 + 1, end_row0
end

---@param buf integer
---@param row integer 1-based
---@param line? string
---@return integer
function M.comment_body_prefix_width(buf, row, line)
  return 0
end

---@param prefix_width integer
---@return string
function M.comment_body_prefix(prefix_width)
  return ""
end

---@param line string
---@param prefix_width integer
---@return boolean
function M.line_has_comment_body_prefix(line, prefix_width)
  return true
end

---@param buf integer
---@param row integer 1-based
---@param line string
---@param prefix_width integer
---@return string
function M.ensure_comment_body_prefix(buf, row, line, prefix_width)
  return tostring(line or "")
end

---@param line string
---@param prefix_width? integer
---@return integer
function M.comment_body_effective_prefix_width(line, prefix_width)
  return math.max(tonumber(prefix_width) or 0, 0)
end

---@param line string
---@param prefix_width? integer
---@return string
function M.comment_body_text_from_line(line, prefix_width)
  local text = tostring(line or "")
  local width = math.max(tonumber(prefix_width) or 0, 0)
  if width > 0 and #text >= width then
    local prefix = text:sub(1, width)
    if prefix:match("^%s*$") then text = text:sub(width + 1) end
  end
  return text
end

---@param lines string[]
---@return string[]
function M.trim_comment_body_lines(lines)
  local first, last = 1, #lines
  while first <= last and vim.trim(lines[first] or "") == "" do
    first = first + 1
  end
  while last >= first and vim.trim(lines[last] or "") == "" do
    last = last - 1
  end
  local trimmed = {}
  for index = first, last do
    trimmed[#trimmed + 1] = lines[index] or ""
  end
  return trimmed
end

---@param body string
---@return string
function M.comment_body_for_sync(body)
  local lines = M.trim_comment_body_lines(vim.split(M.normalize_comment_body_text(body), "\n", { plain = true }))
  return table.concat(lines, "\n")
end

---@param line string
---@param prefix_width? integer
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M.comment_body_text_bounds(line, prefix_width)
  local text = tostring(line or "")
  local start_col = M.comment_body_effective_prefix_width(text, prefix_width)
  local body_text = M.comment_body_text_from_line(text, start_col)
  if body_text == "" then return start_col, start_col end
  return start_col, start_col + #body_text
end

---@param line string
---@param prefix_width? integer
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M.comment_body_text_selection_bounds(line, prefix_width)
  local start_col, end_col = M.comment_body_text_bounds(line, prefix_width)
  if end_col > start_col then end_col = end_col - 1 end
  return start_col, end_col
end

---@param buf integer
---@param row integer 1-based
---@param line? string
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M.comment_body_text_selection_bounds_at_row(buf, row, line)
  local text = line or vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local prefix_width = M.comment_body_prefix_width(buf, row, text)
  return M.comment_body_text_selection_bounds(text, prefix_width)
end

---@param buf integer
---@param row integer 1-based
---@param line? string
---@return integer start_col 0-based byte column
---@return integer end_col 0-based byte column
function M.comment_body_text_bounds_at_row(buf, row, line)
  local text = line or vim.api.nvim_buf_get_lines(buf, row - 1, row, false)[1] or ""
  local prefix_width = M.comment_body_prefix_width(buf, row, text)
  return M.comment_body_text_bounds(text, prefix_width)
end

---@param buf integer
---@return integer? row 1-based current row after clamping
function M.clamp_comment_cursor(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local state = M.state(buf)
  if not state or state.review_clamping_cursor then return nil end
  state.review_clamping_cursor = true
  local row = dr()._normalize_status_cursor(buf) or vim.api.nvim_win_get_cursor(0)[1]
  state.review_clamping_cursor = nil
  state.review_last_cursor_row = row
  return row
end

---@param buf integer
---@param row integer 1-based
---@return boolean
function M.in_editable_region(buf, row)
  return M.in_comment_region(buf, row) or M.comment_body_at_row(buf, row) ~= nil
end

---@param buf integer
---@return boolean
function M.cursor_or_selection_in_editable_region(buf)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return false end
  local mode = vim.fn.mode()
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    local start_row, end_row = vim.fn.line("v"), vim.fn.line(".")
    if start_row > end_row then start_row, end_row = end_row, start_row end
    local found_editable = false
    for row = start_row, end_row do
      if M.in_editable_region(buf, row) then
        found_editable = true
      else
        return false
      end
    end
    return found_editable
  end
  return M.in_editable_region(buf, vim.api.nvim_win_get_cursor(0)[1])
end

---@param buf integer
---@return table? state
function M.command_map_state(buf)
  local state = dr()._status_states and dr()._status_states[buf] or dr()._status
  if not (state and state.view_kind == "review") then return nil end
  state.review_command_maps = state.review_command_maps or {}
  state.review_command_maps_by_key = state.review_command_maps_by_key or {}
  return state
end

---@param mode string|string[]
---@return string[]
function M.keymap_modes(mode)
  if type(mode) == "table" then return mode end
  return { mode }
end

---@param buf integer
---@param command_id string
---@return boolean
function M.command_map_should_be_active(buf, command_id)
  if command_id == "sync" then return true end
  if command_id == "browse" and vim.fn.mode():sub(1, 1) == "n" then return true end
  if command_id == "toggle" and vim.fn.mode():sub(1, 1) == "n" then return true end
  return not M.cursor_or_selection_in_editable_region(buf)
end

---@param map table
function M.install_command_map(map)
  if map.removed then return end
  if map.active then return end
  vim.keymap.set(map.modes, map.key, map.callback, map.opts)
  map.active = true
end

---@param map table
function M.delete_command_map(map)
  if not map.active then return end
  for _, mode in ipairs(map.mode_list) do
    pcall(vim.keymap.del, mode, map.key, { buffer = map.buf })
  end
  map.active = false
end

---@param buf integer
---@param command_id string
---@param modes string|string[]
---@param key string
---@param callback function
---@param opts table
function M.register_command_map(buf, command_id, modes, key, callback, opts)
  local state = M.command_map_state(buf)
  if not state then
    vim.keymap.set(modes, key, callback, opts)
    return
  end
  local mode_list = M.keymap_modes(modes)
  local signature = table.concat(mode_list, "\0") .. "\0" .. key
  local existing = state.review_command_maps_by_key[signature]
  if existing then
    M.delete_command_map(existing)
    existing.removed = true
  end
  local map = {
    buf = buf,
    command_id = command_id,
    modes = modes,
    mode_list = mode_list,
    key = key,
    callback = callback,
    opts = opts,
    active = false,
  }
  state.review_command_maps_by_key[signature] = map
  state.review_command_maps[#state.review_command_maps + 1] = map
  if M.command_map_should_be_active(buf, command_id) then M.install_command_map(map) end
end

---@param buf integer
function M.sync_command_keymaps(buf)
  local state = M.command_map_state(buf)
  if not state then return end
  for _, map in ipairs(state.review_command_maps or {}) do
    if map.removed then goto continue end
    if M.command_map_should_be_active(buf, map.command_id) then
      M.install_command_map(map)
    else
      M.delete_command_map(map)
    end
    ::continue::
  end
end

---Unlock the buffer only while the cursor sits in an editable review region.
---@param buf integer
function M.sync_modifiable(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if vim.api.nvim_get_current_buf() ~= buf then return end
  return dr()._status_perf_span("review.sync_modifiable", buf, nil, function()
    local row = dr()._status_perf_span("review.clamp_comment_cursor", buf, nil, function()
      return M.clamp_comment_cursor(buf)
    end) or vim.api.nvim_win_get_cursor(0)[1]
    local wanted = dr()._status_perf_span("review.in_editable_region", buf, { row = row }, function()
      return M.in_editable_region(buf, row)
    end)
    if vim.bo[buf].modifiable ~= wanted then
      vim.bo[buf].modifiable = wanted
    end
    dr()._status_perf_span("review.sync_command_keymaps", buf, nil, function()
      M.sync_command_keymaps(buf)
    end)
  end)
end

---Read edited review text back into the state.
---@param buf integer
function M.sync_comment_text(buf)
  local state = M.state(buf)
  if not state then return end
  local s0, e0
  if state.review_comment_region then s0, e0 = region.bounds(state.review_comment_region) end
  local changed = false
  if s0 and e0 then
    local review_comment_text = table.concat(vim.api.nvim_buf_get_lines(buf, s0, e0, false), "\n")
    if review_comment_text ~= state.review_comment_text then
      state.review_comment_text = review_comment_text
      changed = true
    end
  end
  if changed then M.save_draft(state) end
end

---@param buf integer
---@param comment table
---@return string?
function M.comment_body_text_from_buffer(buf, comment)
  local start_row0, end_row0 = M.comment_body_range0(buf, comment)
  if start_row0 == nil or end_row0 == nil or end_row0 < start_row0 then return nil end
  local raw_lines = vim.api.nvim_buf_get_lines(buf, start_row0, end_row0, false)
  return M.normalize_comment_body_text(table.concat(M.trim_comment_body_lines(raw_lines), "\n"))
end

---@param buf integer
---@param comment table
---@return table? snapshot
function M.inline_comment_cursor_snapshot(buf, comment)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return nil end
  local start_row, end_row = M.comment_body_rows(buf, comment)
  if not start_row or not end_row then return nil end
  local cursor = vim.api.nvim_win_get_cursor(0)
  if cursor[1] < start_row or cursor[1] > end_row then return nil end

  local offset = 0
  local raw_lines = vim.api.nvim_buf_get_lines(buf, start_row - 1, end_row, false)
  for index, raw_line in ipairs(raw_lines) do
    local row = start_row + index - 1
    local body_text = tostring(raw_line or "")
    if row == cursor[1] then
      return {
        offset = offset + math.min(math.max(cursor[2] or 0, 0), #body_text),
        insert = vim.api.nvim_get_mode().mode:sub(1, 1) == "i",
      }
    end
    offset = offset + #body_text + 1
  end
  return nil
end

---@param buf integer
---@param comment table
---@param snapshot table?
function M.restore_inline_comment_cursor(buf, comment, snapshot)
  if not (snapshot and buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
  local start_row, end_row = M.comment_body_rows(buf, comment)
  if not start_row or not end_row then return end

  local remaining = math.max(tonumber(snapshot.offset) or 0, 0)
  local target_row, target_col = start_row, 0
  local raw_lines = vim.api.nvim_buf_get_lines(buf, start_row - 1, end_row, false)
  for index, raw_line in ipairs(raw_lines) do
    target_row = start_row + index - 1
    local body_text = tostring(raw_line or "")
    target_col = math.min(remaining, #body_text)
    if remaining <= #body_text then break end
    remaining = remaining - #body_text - 1
  end
  pcall(vim.api.nvim_win_set_cursor, 0, { target_row, target_col })
  M.sync_modifiable(buf)
  if snapshot.insert then
    vim.schedule(function()
      if vim.api.nvim_get_current_buf() == buf then pcall(vim.cmd, "startinsert") end
    end)
  end
end

---@param buf integer
---@param comment table
---@param snapshot table?
function M.schedule_inline_comment_render(buf, comment, snapshot)
  local state = M.state(buf)
  if not state or state.review_inline_render_pending then
    if state then state.review_inline_render_pending = { comment = comment, snapshot = snapshot } end
    return
  end
  state.review_inline_render_pending = { comment = comment, snapshot = snapshot }
  local pending = state.review_inline_render_pending
  state.review_inline_render_pending = nil
  if not (pending and pending.comment and pending.comment.local_state ~= "deleted") then return end
  M.render(buf)
  M.restore_inline_comment_cursor(buf, pending.comment, pending.snapshot)
end

---@param buf integer
---@param row0 integer?
---@param line string
---@return boolean changed
function M.replace_comment_rule_line(buf, row0, line)
  if row0 == nil or row0 < 0 then return false end
  local old_line = vim.api.nvim_buf_get_lines(buf, row0, row0 + 1, false)[1]
  if old_line == nil or old_line == line then return false end
  pcall(vim.api.nvim_buf_clear_namespace, buf, dr()._status_ns, row0, row0 + 1)
  pcall(vim.api.nvim_buf_set_text, buf, row0, 0, row0, #old_line, { line })
  if line ~= "" then
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, row0, 0, {
      line_hl_group = "DiffReviewReviewCommentHeader",
      priority = 80,
    })
    pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, row0, 0, {
      end_col = #line,
      hl_group = "DiffReviewReviewCommentHeader",
      priority = 90,
    })
    M.set_comment_rule_date_extmarks(buf, row0, line)
  end
  local state = M.state(buf)
  if state and state.lines then state.lines[row0 + 1] = line end
  return true
end

---@param buf integer
---@param win integer?
---@param target_comment table?
function M.refresh_inline_comment_rules(buf, win, target_comment)
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local state = M.state(buf)
  if not state then return end

  local was_modifiable = vim.bo[buf].modifiable
  vim.bo[buf].modifiable = true
  for _, comment in ipairs(state.review_comments or {}) do
    if (not target_comment or M.same_comment(comment, target_comment)) and comment.local_state ~= "deleted" then
      local header_row0 = M.comment_header_row0(buf, comment)
      local footer_row0 = M.comment_footer_row0(buf, comment)
      local header = M.comment_header_line(comment, win, buf)
      local footer = M.comment_footer_line(win, buf)
      if M.replace_comment_rule_line(buf, header_row0, header) and header_row0 ~= nil then
        pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, comment.review_header_mark)
        comment.review_header_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, header_row0, 0, { right_gravity = false })
      end
      if M.replace_comment_rule_line(buf, footer_row0, footer) and footer_row0 ~= nil then
        pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, comment.review_footer_mark)
        comment.review_footer_mark = vim.api.nvim_buf_set_extmark(buf, M.ns, footer_row0, 0, { right_gravity = true })
      end
      for reply_index, reply in ipairs(type(comment.replies) == "table" and comment.replies or {}) do
        local reply_mark_id = comment.review_reply_header_marks and comment.review_reply_header_marks[reply_index] or nil
        local reply_row0 = M.mark_row(buf, reply_mark_id)
        local reply_header = M.reply_header_line(reply, win, buf)
        if M.replace_comment_rule_line(buf, reply_row0, reply_header) and reply_row0 ~= nil then
          pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, reply_mark_id)
          comment.review_reply_header_marks = comment.review_reply_header_marks or {}
          comment.review_reply_header_marks[reply_index] = vim.api.nvim_buf_set_extmark(buf, M.ns, reply_row0, 0, { right_gravity = false })
        end
      end
    end
    ::continue::
  end
  vim.bo[buf].modifiable = was_modifiable
end

---@param buf integer
---@param comment table
function M.refresh_inline_comment_header(buf, comment)
  M.refresh_inline_comment_rules(buf, nil, comment)
end

---@param buf integer
---@return table? comment
---@return string? edited_text
function M.changed_inline_comment_body(buf)
  local state = M.state(buf)
  if not state or state.review_rendering or state.review_editing_comment then return nil end
  for _, comment in ipairs(state.review_comments or {}) do
    if comment.local_state ~= "deleted" then
      local edited_text = M.comment_body_text_from_buffer(buf, comment)
      local rendered_text = M.normalize_comment_body_text(comment.body)
      if edited_text and edited_text ~= rendered_text then return comment, edited_text end
    end
  end
  return nil
end

---@param buf integer
---@return boolean changed
function M.sync_inline_comment_text(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return false end
  local state = M.state(buf)
  if not state or state.review_rendering or state.review_editing_comment then return false end
  local comment, edited_text = M.changed_inline_comment_body(buf)
  if not (comment and edited_text) then return false end
  edited_text = M.normalize_comment_body_text(edited_text)
  comment.body = edited_text
  comment.review_rendered_body_text = edited_text
  comment.updated_at = os.date("%Y-%m-%d %H:%M")
  comment.local_state = comment.remote_node_id and "dirty" or "new"
  comment.syncing = nil
  M.normalize_comment(state, comment)
  if state.view_kind == "review" then
    M.save_draft(state)
  elseif state.view_kind == "pr" then
    vim.bo[buf].modified = true
  end
  local is_regular_pr_comment = state.view_kind == "pr"
    and dr()._pr_overview.is_regular_comment
    and dr()._pr_overview.is_regular_comment(state, comment)
  if not is_regular_pr_comment then M.refresh_inline_comment_header(buf, comment) end
  return true
end

---@param buf integer
---@param viewed boolean
function M.toggle_viewed(buf, viewed)
  local state = M.state(buf)
  if not state then return end
  dr()._status = state
  local entry = status_entry_under_cursor()
  if not entry then return end
  state.review_viewed_hunks = state.review_viewed_hunks or {}

  local function apply_file(file, selected_hunk)
    local hunks = {}
    if selected_hunk then
      for _, hunk in ipairs(selected_hunk.raw_hunks or { selected_hunk }) do
        hunks[#hunks + 1] = hunk
      end
    else
      for _, hunk in ipairs(dr()._status_diff_hunks_for_file(file)) do
        hunks[#hunks + 1] = hunk
      end
    end
    if #hunks == 0 then
      state.review_viewed[file.relpath] = viewed or nil
    else
      state.review_viewed[file.relpath] = nil
      for _, hunk in ipairs(hunks) do
        local key = M.hunk_view_key(state, file, hunk)
        if viewed then
          state.review_viewed_hunks[key] = {
            file = file.relpath or hunk.file or repo_relative(file.filename, state.cwd) or file.filename,
            commit_id = state.commit_id,
          }
        else
          state.review_viewed_hunks[key] = nil
        end
      end
    end
  end

  if entry.file then
    apply_file(entry.file, entry.hunk)
  elseif entry.kind == "section" and entry.section then
    for _, file in ipairs(entry.section.files or {}) do
      apply_file(file)
    end
  else
    return
  end

  M.save_draft(state)
  M.render(buf)
end

---Build the comment target (path/side/line range) from the visual selection
---or the cursor line. Only changed (diff body) rows can carry a comment.
---@param state table
---@return table? payload
function M.selection_payload(state)
  local mode = vim.fn.mode()
  local srow, erow
  if mode == "v" or mode == "V" or mode:byte() == 22 then
    srow, erow = vim.fn.line("v"), vim.fn.line(".")
  else
    srow = vim.fn.line(".")
    erow = srow
  end
  if srow > erow then srow, erow = erow, srow end

  local first, last
  for row = srow, erow do
    local entry = state.entries[row]
    local dl = entry and entry.diff_line
    if dl then
      first = first or dl
      last = dl
    end
  end
  if not first then return nil end

  -- Diff rows carry the absolute path; GitHub wants it repo-relative. Keep the
  -- absolute path too so inline comments can re-anchor against rendered rows.
  local path = repo_relative(last.file, state.cwd) or last.file
  local payload = { path = path, abs_file = last.file, side = M.side_of(last), line = last.line, position = last.position }
  if first.file == last.file and not (first.line == last.line and first.side == last.side) then
    payload.start_line = first.line
    payload.start_side = M.side_of(first)
    payload.start_position = first.position
    -- GitHub requires start_line < line when both endpoints are on one side.
    if payload.start_side == payload.side and payload.start_line > payload.line then
      payload.start_line, payload.line = payload.line, payload.start_line
    end
  end
  return payload
end

---@param title string
---@param on_submit fun(text: string)
---@param prefill? string existing text to edit
---@param opts? { on_cancel?: fun(), cursor?: integer[] }
function M.open_input(title, on_submit, prefill, opts)
  if M.input_provider then
    M.input_provider(title, on_submit, prefill)
    return
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  if prefill and prefill ~= "" then
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(prefill, "\n", { plain = true }))
  end
  local width = math.min(72, math.max(40, math.floor(vim.o.columns * 0.6)))
  local content_height = math.max(1, #vim.api.nvim_buf_get_lines(buf, 0, -1, false))
  local max_height = math.max(1, vim.o.lines - 6)
  local height = math.min(content_height, max_height)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "cursor",
    row = 1,
    col = 0,
    width = width,
    height = height,
    style = "minimal",
    border = "rounded",
    title = " " .. title .. " (Ctrl-s submit, Ctrl-q cancel) ",
    title_pos = "center",
  })
  pcall(vim.api.nvim_set_current_win, win)
  if opts and opts.cursor then
    local popup_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    local target_row = math.min(math.max(opts.cursor[1] or 1, 1), math.max(#popup_lines, 1))
    local line = popup_lines[target_row] or ""
    local target_col = math.min(math.max(opts.cursor[2] or 0, 0), #line)
    pcall(vim.api.nvim_win_set_cursor, win, { target_row, target_col })
  end
  local done = false
  local function enter_insert()
    if done or not vim.api.nvim_win_is_valid(win) then return end
    pcall(vim.api.nvim_set_current_win, win)
    pcall(vim.cmd, "startinsert")
    vim.defer_fn(function()
      if done or not vim.api.nvim_win_is_valid(win) then return end
      pcall(vim.api.nvim_set_current_win, win)
      pcall(vim.cmd, "startinsert")
    end, 10)
  end
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  local function cancel()
    if done then return end
    done = true
    close()
    if opts and opts.on_cancel then opts.on_cancel() end
  end
  local function submit()
    if done then return end
    done = true
    vim.cmd("stopinsert")
    local text = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
    close()
    on_submit(text)
  end
  vim.keymap.set({ "n", "i" }, "<C-s>", submit, { buffer = buf })
  vim.keymap.set({ "n", "i" }, "<C-q>", cancel, { buffer = buf, nowait = true })
  vim.keymap.set("n", "<Esc>", cancel, { buffer = buf, nowait = true })
  enter_insert()
  vim.schedule(enter_insert)
end

---@param buf integer
---@param comment table
---@param opts? { insert?: boolean }
function M.focus_inline_comment(buf, comment, opts)
  if not (buf and vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_get_current_buf() == buf) then return end
  if comment.review_folded then
    local state = M.state(buf)
    comment.review_folded = false
    if state and state.view_kind == "review" then M.save_draft(state) end
    local fold_id = comment.review_fold_id or M.comment_fold_id(comment)
    set_status_folded(fold_id, false)
    if not dr()._status_set_native_fold_state(buf, fold_id, false) then
      if state and state.view_kind == "pr" then
        render_pr_status(state.pr, state.cwd, buf, state.pr_diff_text)
      else
        M.render(buf)
      end
    end
  end
  local start_row, end_row = M.comment_body_rows(buf, comment)
  if not start_row or not end_row then return end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local target_row = cursor[1]
  if target_row < start_row or target_row > end_row then target_row = start_row end
  local line = vim.api.nvim_buf_get_lines(buf, target_row - 1, target_row, false)[1] or ""
  local target_col = math.min(math.max(cursor[2] or 0, 0), #line)
  pcall(vim.api.nvim_win_set_cursor, 0, { target_row, target_col })
  M.sync_modifiable(buf)
  if opts and opts.insert then
    vim.schedule(function()
      if vim.api.nvim_get_current_buf() == buf then pcall(vim.cmd, "startinsert") end
    end)
  end
end

---@param state table
---@param payload table
---@return table comment
function M.inline_comment_from_payload(state, payload)
  local created_at = os.date("%Y-%m-%d %H:%M")
  local comment = {
    path = payload.path,
    abs_file = payload.abs_file,
    side = payload.side,
    line = payload.line,
    position = payload.position,
    start_line = payload.start_line,
    start_side = payload.start_side,
    start_position = payload.start_position,
    body = "",
    user = "you",
    created_at = created_at,
    updated_at = created_at,
    local_state = "new",
  }
  return M.normalize_comment(state, comment)
end

---@param state table
---@param payload table
---@return table comment
function M.create_inline_comment(state, payload)
  local comment = M.inline_comment_from_payload(state, payload)
  state.review_comments[#state.review_comments + 1] = comment
  return comment
end

---`C`: on an existing comment, focus its inline editable body; on a changed diff line, add a draft
---comment. On anything else it is a no-op. Comments are local until submit.
---@param buf integer
function M.add_comment(buf)
  local state = M.state(buf)
  if not state then return end
  dr()._status = state
  local existing = M.comment_under_cursor(buf)
  if existing then
    M.focus_inline_comment(buf, existing)
    return
  end
  local payload = M.selection_payload(state)
  M.leave_visual()
  if not payload then return end
  if not dr()._status_source_policy_allows_cursor(state, "comment") then return end
  local comment = M.create_inline_comment(state, payload)
  M.save_draft(state)
  M.render(buf)
  M.focus_inline_comment(buf, comment, { insert = true })
end

---`J`: remove the draft comment under the cursor.
---@param buf integer
function M.delete_comment(buf)
  local state = M.state(buf)
  if not state then return end
  local comment, index = M.comment_under_cursor(buf)
  if not (comment and index) then return end
  if comment.remote_node_id then
    comment.local_state = "deleted"
  else
    table.remove(state.review_comments, index)
  end
  M.save_draft(state)
  M.render(buf)
end

---Sync dirty inline comments to the pending GitHub review.
---@param buf integer
function M.sync(buf)
  local state = M.state(buf)
  if not state then return end
  dr()._status = state
  M.sync_comment_text(buf)
  M.sync_inline_comment_text(buf)
  local count = M.enqueue_dirty_comments(buf)
  if count == 0 then
    vim.notify("No review comments to sync", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  M.flush_sync(buf, function(ok)
    if not vim.api.nvim_buf_is_valid(buf) then return end
    if ok then
      vim.notify(("Synced %d review comment%s"):format(count, count == 1 and "" or "s"), vim.log.levels.INFO, { title = "DiffReview" })
    end
  end)
end

---@param buf integer
---@param direction 1|-1
function M.navigate(buf, direction)
  local state = M.state(buf)
  if not state then return end
  local rows = {}
  for row, entry in pairs(state.entries or {}) do
    if entry.kind == "review_comment" and entry.review_first then
      rows[#rows + 1] = row
    end
  end
  if #rows == 0 then
    vim.notify("No comments yet", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  table.sort(rows)
  local cursor = vim.api.nvim_win_get_cursor(0)[1]
  local target
  if direction == 1 then
    for _, row in ipairs(rows) do
      if row > cursor then target = row break end
    end
    target = target or rows[1]
  else
    for index = #rows, 1, -1 do
      if rows[index] < cursor then target = rows[index] break end
    end
    target = target or rows[#rows]
  end
  pcall(vim.api.nvim_win_set_cursor, 0, { target, 0 })
  vim.cmd("normal! zz")
end

---Submit the review. Inline comments are synced to a pending remote review as
---part of submission; the final review summary stays local-only until this call.
---@param buf integer
function M.submit(buf)
  local state = M.state(buf)
  if not state then return end
  dr()._status = state
  M.sync_comment_text(buf)
  local function with_event(event)
    if not event then return end
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local comments = {}
    for _, comment in ipairs(state.review_comments) do
      if comment.local_state ~= "deleted" then
        local body = M.comment_body_for_sync(comment.body or "")
        local item = {
          path = comment.path,
          body = body,
          line = comment.line,
          side = comment.side,
        }
        if body ~= "" then
          if comment.start_line then
            item.start_line = comment.start_line
            item.start_side = comment.start_side
          end
          comments[#comments + 1] = item
        end
      end
    end
    local count = #comments
    local function handle_submit_result(result)
      if not vim.api.nvim_buf_is_valid(buf) then return end
      if result.code ~= 0 then
        notify_error(
          "PR review submit failed: " .. (result.output ~= "" and result.output or ("gh exited " .. tostring(result.code))),
          "DiffReview"
        )
        return
      end
      state.review_comments = {}
      state.review_comment_text = ""
      state.review_remote = nil
      M.delete_draft(state)
      M.render(buf)
      vim.notify(
        ("Review submitted (%s, %d comment%s)"):format(event, count, count == 1 and "" or "s"),
        vim.log.levels.INFO,
        { title = "DiffReview" }
      )
    end
    M.flush_sync(buf, function(ok)
      if not ok then return end
      if state.review_remote and state.review_remote.id then
        gh.submit_pending_review_async(state.cwd, state.pr.number, state.pr.repo, state.review_remote.id, {
          body = state.review_comment_text or "",
          event = event,
        }, handle_submit_result)
      else
        gh.submit_pr_review_async(state.cwd, state.pr.number, state.pr.repo, {
          body = state.review_comment_text or "",
          event = event,
          commit_id = state.commit_id,
          comments = comments,
        }, handle_submit_result)
      end
    end)
  end
  if M.verdict_provider then
    M.verdict_provider(with_event)
    return
  end
  M.pick_verdict(with_event)
end

--- Verdict chooser as a small popup window (not vim.ui.select / snacks):
--- `c`/`a`/`r` pick, `q`/`<Esc>` cancels.
---@param on_choice fun(event: string?)
function M.pick_verdict(on_choice)
  local options = {
    { key = "c", event = "COMMENT", label = "Comment (no verdict)" },
    { key = "a", event = "APPROVE", label = "Approve" },
    { key = "r", event = "REQUEST_CHANGES", label = "Request changes" },
  }
  local lines = { "" }
  for _, opt in ipairs(options) do
    lines[#lines + 1] = ("  [%s]  %s"):format(opt.key, opt.label)
  end
  lines[#lines + 1] = "  [q]  cancel"
  lines[#lines + 1] = ""

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  local width = 0
  for _, line in ipairs(lines) do
    width = math.max(width, #line)
  end
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "cursor",
    row = 1,
    col = 0,
    width = width + 2,
    height = #lines,
    style = "minimal",
    border = "rounded",
    title = " Submit review ",
    title_pos = "center",
  })
  local chosen = false
  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end
  local function choose(event)
    if chosen then return end
    chosen = true
    close()
    on_choice(event)
  end
  for _, opt in ipairs(options) do
    vim.keymap.set("n", opt.key, function() choose(opt.event) end, { buffer = buf, nowait = true, silent = true })
  end
  vim.keymap.set("n", "q", function() choose(nil) end, { buffer = buf, nowait = true, silent = true })
  vim.keymap.set("n", "<Esc>", function() choose(nil) end, { buffer = buf, nowait = true, silent = true })
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buf, once = true, callback = function() choose(nil) end })
end

---@param buf integer
function M.attach(buf)
  local group = vim.api.nvim_create_augroup("DiffReviewReview" .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter", "ModeChanged" }, {
    group = group,
    buffer = buf,
    callback = function()
      dr()._status_perf_span("review.autocmd_sync_modifiable", buf, nil, function()
        M.sync_modifiable(buf)
      end)
    end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "InsertLeave" }, {
    group = group,
    buffer = buf,
    callback = function()
      dr()._status_perf_span("review.autocmd_text_changed", buf, nil, function()
        M.sync_comment_text(buf)
        M.sync_inline_comment_text(buf)
        dr()._pr_edit.render_markdown_regions(buf)
      end)
    end,
  })
end

---@param pr DiffReviewGhPR
---@param cwd string
---@param buf integer
function M.load_diff(pr, cwd, buf)
  local state = M.state(buf)
  if not state then return end
  state.pr_diff_request_id = (state.pr_diff_request_id or 0) + 1
  local request_id = state.pr_diff_request_id
  gh.pr_diff_async(cwd, pr.number, pr.repo, function(result)
    local latest = M.state(buf)
    if not (latest and latest.pr_diff_request_id == request_id and vim.api.nvim_buf_is_valid(buf)) then return end
    dr()._status = latest
    if result.code ~= 0 then
      notify_error("GitHub PR diff failed: " .. (result.output ~= "" and result.output or ("gh exited " .. result.code)), "DiffReview")
      return
    end
    latest.diff_text = result.stdout or ""
    M.render(buf)
  end)
end

--- Resolve the PR from the current status/PR buffer and open its review.
---@param buf integer
function M.start(buf)
  local status = dr()._status_states and dr()._status_states[buf] or dr()._status
  if not status then return end
  local pr
  if status.view_kind == "pr" then
    pr = status.pr
  elseif status.pr and status.pr.state == "ready" then
    pr = status.pr.pr
  end
  if not pr then
    vim.notify("No GitHub PR for this branch", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  dr().open_review(pr, { cwd = status.cwd })
end

---@param buf integer
function M.setup_keymaps(buf)
  local function map(id, fn)
    local spec = dr()._status_command_specs_by_id[id]
    if not (spec and status_command_visible(spec)) then return end
    for _, key in ipairs(status_keys_for(id)) do
      local mapped
      mapped = function()
        if dr()._status_states and dr()._status_states[buf] then dr()._status = dr()._status_states[buf] end
        fn()
      end
      M.register_command_map(buf, id, spec.modes, key, mapped, { buffer = buf, silent = true, nowait = true, desc = spec.desc })
    end
  end
  map("viewed", function() M.toggle_viewed(buf, true) end)
  map("unviewed", function() M.toggle_viewed(buf, false) end)
  map("comment", function() M.add_comment(buf) end)
  map("delete", function() M.delete_comment(buf) end)
  map("next_comment", function() M.navigate(buf, 1) end)
  map("prev_comment", function() M.navigate(buf, -1) end)
  map("sync", function() M.sync(buf) end)
  map("submit", function() M.submit(buf) end)
end

return M
