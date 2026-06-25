--- Owns the commit-message buffer and status dialogs: the AI commit message editor, the About
--- (commit) view, create-PR confirmation, the verdict/help popups, the remote push/pull action,
--- and the jump/toggle/collapse cursor actions over status entries.
---
--- Reads live status state, render/open entry points, and the commit-file loader through the init
--- module via dr(); uses the git backend as a direct require.

local git_backend = require("diff_review.git.git_backend")
local keymaps = require("diff_review.shared.keymaps")
local status_command_specs = require("diff_review.shared.command_specs").specs

--- Resolve the init module lazily so dialog/action handlers can reach orchestrator state and the
--- open/render seams without a load-time circular require.
local function dr()
  return require("diff_review")
end
local session = require("diff_review.session")

local M = {}

function M._status_confirm_create_pr(on_yes)
  local body = {
    "No GitHub PR found for this branch.",
    "",
    "Create a draft PR now?",
    "",
    "  [y] yes    [n] no",
  }
  local width = 40
  for _, line in ipairs(body) do
    width = math.max(width, #line + 4)
  end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, body)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = #body,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - #body) / 2),
    style = "minimal",
    border = "rounded",
    title = " GitStatus ",
    title_pos = "center",
  })

  local function close()
    if vim.api.nvim_win_is_valid(win) then pcall(vim.api.nvim_win_close, win, true) end
  end

  vim.keymap.set("n", "y", function()
    close()
    on_yes()
  end, { buffer = buf, nowait = true, silent = true, desc = "Create pull request" })

  vim.keymap.set("n", "n", close, { buffer = buf, nowait = true, silent = true, desc = "Cancel pull request creation" })
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true, desc = "Cancel pull request creation" })
  end
end
---@param entry DiffReviewStatusEntry?
local function status_open_about(entry)
  local about = entry and entry.about
  if not about and session.status then about = session.status.about end
  if not about or about.state == "none" then
    local status = session.status
    if status and status.cwd and status.buf and dr()._status_has_changes(status.sections) then
      dr()._status_ensure_about_state(status.cwd, status.buf, true, false, true)
      vim.notify("Generating commit message...", vim.log.levels.INFO, { title = "DiffReview" })
      return
    end
    vim.notify("No generated commit message", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  if about.state == "generating" then
    vim.notify("Commit message is still generating", vim.log.levels.INFO, { title = "DiffReview" })
    return
  end
  if about.state == "error" then
    vim.notify(about.error or "Commit message generation failed", vim.log.levels.WARN, { title = "DiffReview" })
    return
  end
  if not about.message or about.message == "" then return end

  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "gitcommit"
  local name = ("DiffReviewAbout://%s"):format(vim.fn.sha256(about.message):sub(1, 8))
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(about.message, "\n", { plain = true }))
  vim.bo[buf].modifiable = false
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)
end

function M._status_commit_message_target(entry, cursor_col)
  if not (entry and (entry.kind == "commit" or entry.kind == "commit_message") and entry.commit) then return nil end
  local start_col = entry.commit_subject_start_col
  local end_col = entry.commit_subject_end_col
  if type(start_col) == "number" and type(end_col) == "number" then
    if start_col >= end_col then return nil end
    if type(cursor_col) ~= "number" or cursor_col < start_col or cursor_col >= end_col then return nil end
  end
  return entry.commit
end

function M._status_commit_message_fallback_lines(commit)
  local message = tostring(commit and (commit.full_message or commit.messageBody or commit.message_body or commit.body or commit.message) or "")
  if message == "" then message = tostring(commit and (commit.subject or commit.messageHeadline) or "") end
  if message == "" then return { "No commit message." } end
  return vim.split(message:gsub("\r\n", "\n"), "\n", { plain = true })
end

function M._status_open_commit_message(commit, cwd)
  if type(commit) ~= "table" then return false end
  local oid = vim.trim(tostring(commit.oid or commit.sha or commit.id or ""))
  local short_oid = vim.trim(tostring(commit.short_oid or commit.shortOid or commit.abbreviatedOid or ""))
  if short_oid == "" then short_oid = dr()._status_short_oid(oid) end
  local title_id = short_oid ~= "" and short_oid or vim.fn.sha256(tostring(commit.subject or commit.messageHeadline or "")):sub(1, 8)

  local buf = vim.api.nvim_create_buf(true, false)
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "gitcommit"
  local name = ("GitCommit://%s"):format(title_id)
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "Loading commit message..." })
  vim.bo[buf].modifiable = false
  vim.keymap.set("n", "q", function()
    if vim.api.nvim_buf_is_valid(buf) then pcall(vim.api.nvim_buf_delete, buf, { force = true }) end
  end, { buffer = buf, nowait = true, silent = true, desc = "Close commit message" })
  vim.api.nvim_win_set_buf(vim.api.nvim_get_current_win(), buf)

  local function set_lines(lines)
    if not vim.api.nvim_buf_is_valid(buf) then return end
    if type(lines) ~= "table" or #lines == 0 then lines = dr()._status_commit_message_fallback_lines(commit) end
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
  end

  if oid == "" or not (cwd and cwd ~= "") then
    set_lines(dr()._status_commit_message_fallback_lines(commit))
    return true
  end

  git_backend.systemlist_async({ "git", "-C", cwd, "show", "-s", "--format=%B", oid }, function(output, code, stderr)
    if code ~= 0 then
      local message = vim.trim(stderr or "")
      dr()._notify_error("Commit message failed: " .. (message ~= "" and message or ("git exited " .. tostring(code))), "DiffReview")
      set_lines(dr()._status_commit_message_fallback_lines(commit))
      return
    end
    set_lines(output or {})
  end)
  return true
end

function M._status_open_commit_message_under_cursor(entry)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local commit = dr()._status_commit_message_target(entry, cursor and cursor[2])
  if not commit then return false end
  return dr()._status_open_commit_message(commit, session.status and session.status.cwd)
end

---@param span table
---@return "add"|"delete"|nil
function M._status_inline_span_kind(span)
  if not span then return nil end
  if span.kind == "add" or span.hl_group == "DiffReviewInlineAddBg" then return "add" end
  if span.kind == "delete" or span.hl_group == "DiffReviewInlineDeleteBg" then return "delete" end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@param prefix string
---@return table?
function M._status_diff_line_with_prefix(entry, prefix)
  for _, diff_line in ipairs(entry and entry.diff_lines or {}) do
    if diff_line.prefix == prefix then return diff_line end
  end
  if entry and entry.diff_line and entry.diff_line.prefix == prefix then return entry.diff_line end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@param cursor_col integer?
---@return table?
function M._status_jump_diff_line(entry, cursor_col)
  if not entry then return nil end
  local spans = entry.inline_jump_spans
  if type(spans) ~= "table" or #spans == 0 or type(entry.diff_lines) ~= "table" then return entry.diff_line end

  local has_add = false
  local has_delete = false
  local cursor_span = nil
  for _, span in ipairs(spans) do
    local kind = dr()._status_inline_span_kind(span)
    if kind == "add" then has_add = true end
    if kind == "delete" then has_delete = true end
    if type(cursor_col) == "number"
      and type(span.start_col) == "number"
      and type(span.end_col) == "number"
      and cursor_col >= span.start_col
      and cursor_col < span.end_col then
      cursor_span = span
    end
  end

  local cursor_kind = dr()._status_inline_span_kind(cursor_span)
  if cursor_kind == "add" then return dr()._status_diff_line_with_prefix(entry, "+") or entry.diff_line end
  if cursor_kind == "delete" then return dr()._status_diff_line_with_prefix(entry, "-") or entry.diff_line end
  if has_add and not has_delete then return dr()._status_diff_line_with_prefix(entry, "-") or entry.diff_line end
  if has_delete and not has_add then return dr()._status_diff_line_with_prefix(entry, "+") or entry.diff_line end
  return entry.diff_line
end

---@param entry DiffReviewStatusEntry?
---@param skip_revision boolean? open the working-tree file even for deleted lines
---@param selected_diff_line? table already resolved jump target for async fallbacks
local function status_jump(entry, skip_revision, selected_diff_line)
  if not (entry and entry.file and entry.file.filename) then return end
  local cursor = vim.api.nvim_win_get_cursor(0)
  local jump_diff_line = selected_diff_line or dr()._status_jump_diff_line(entry, cursor and cursor[2])
  if not skip_revision and jump_diff_line and jump_diff_line.side == "left" and jump_diff_line.line then
    local rev, path = dr()._file_revision.target(entry, session.status)
    if rev and path then
      dr()._file_revision.open({
        rev = rev,
        path = path,
        cwd = session.status.cwd,
        line = jump_diff_line.line,
        on_error = function()
          status_jump(entry, true, jump_diff_line)
        end,
      })
      return
    end
  end
  vim.cmd.edit(vim.fn.fnameescape(entry.file.filename))
  local target_line
  if jump_diff_line and jump_diff_line.line then
    if jump_diff_line.side == "left" and entry.hunk and entry.hunk.diff then
      target_line = dr()._closest_current_line_for_deleted_diff_line(entry.hunk.diff, jump_diff_line.line)
    else
      target_line = jump_diff_line.line
    end
  end
  if entry.kind == "hunk" and entry.hunk.diff then
    target_line = target_line or tonumber(entry.hunk.diff:match("@@ %-%d+,?%d* %+(%d+)"))
  end
  if target_line then
    local max_line = vim.api.nvim_buf_line_count(0)
    target_line = math.min(math.max(target_line, 1), max_line)
    pcall(vim.api.nvim_win_set_cursor, 0, { target_line, 0 })
    vim.cmd("normal! zz")
  end
end

---@param entry_id string?
---@return DiffReviewStatusEntry?
function M._status_entry_by_id(entry_id)
  if not (entry_id and session.status and session.status.entries) then return nil end
  local viewport = session.status.diff_viewport
  if viewport and viewport.enabled and viewport.logical_entries then
    for _, entry in pairs(viewport.logical_entries) do
      if entry and entry.id == entry_id then return entry end
    end
  end
  for _, entry in pairs(session.status.entries) do
    if entry and entry.id == entry_id then return entry end
  end
  return nil
end

---@param entry DiffReviewStatusEntry?
---@return boolean
function M._status_entry_default_folded(entry)
  if not entry then return false end
  if entry.default_folded ~= nil then return entry.default_folded == true end
  local ranges = dr()._status_fold_ranges_for_id(session.status, entry.id)
  local range = ranges[1]
  if range and range.default_folded ~= nil then return range.default_folded == true end
  if entry.kind == "file" or entry.kind == "pr_file" then
    return true
  elseif entry.kind == "commit" or entry.kind == "commit_file" then
    return true
  elseif entry.kind == "pr_comment" and entry.pr_comment then
    return true
  elseif entry.kind == "pr_review" then
    return true
  elseif entry.kind == "pr_review_file" then
    return false
  elseif entry.kind == "section" and entry.section then
    local section_config = status_section_by_name[entry.section.name]
    if section_config then return section_config.default_folded end
    return entry.section.default_folded
  end
  return false
end

---@param entry DiffReviewStatusEntry?
---@param state? table
local function status_toggle(entry, state)
  state = state or session.status
  if not state then return end
  if not entry then return end
  local fold_id = entry.fold_target_id or entry.id
  if not fold_id then return end
  if state and state.buf and dr()._pr_edit.blocks_render(state.buf) then return end
  local fold_entry = entry
  if entry.fold_target_id then
    fold_entry = dr()._status_entry_by_id(fold_id) or { id = fold_id, kind = "pr_head_section" }
  end
  local default = dr()._status_entry_default_folded(fold_entry)
  local current_folded = dr()._status_folded(fold_id, default, state)
  local native_state = dr()._status_native_folded(state.buf, dr()._status_fold_ranges_for_id(state, fold_id), vim.api.nvim_get_current_win())
  if native_state ~= nil then current_folded = native_state end
  local next_folded = not current_folded
  if not next_folded then
    dr()._status_prewarm_entry_syntax(fold_entry)
  end
  dr()._set_status_folded(fold_id, next_folded, state)
  if fold_entry.kind == "commit" and not next_folded and fold_entry.commit then
    dr()._status_load_commit_files(fold_entry.commit)
  end
  local native_folded = dr()._status_set_native_fold_state(state.buf, fold_id, next_folded)
  if not native_folded then
    dr()._render_status_or_notify(state.buf, fold_id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
  elseif state.view_kind == "status" then
    local walkthrough = package.loaded["diff_review.views.walkthrough"]
    if walkthrough and walkthrough.on_status_rendered then walkthrough.on_status_rendered(state.buf) end
  end
  if state.view_kind == "pr" then
    dr()._pr_edit.sync_modifiable(state.buf)
  elseif state.view_kind == "review" then
    dr()._review.sync_modifiable(state.buf)
  end
end

function M._status_collapse_parent()
  local line, entry = dr()._status_entry_line_under_cursor()
  if not (line and entry) then return end
  local parent = dr()._status_parent_entry(line, entry)
  local target = parent or entry
  dr()._set_status_folded(target.id, true)
  if not dr()._status_set_native_fold_state(session.status.buf, target.id, true) then
    dr()._render_status_or_notify(session.status.buf, target.id, vim.api.nvim_win_get_cursor(0)[1], { reuse_sections = true })
  elseif session.status.view_kind == "status" then
    local walkthrough = package.loaded["diff_review.views.walkthrough"]
    if walkthrough and walkthrough.on_status_rendered then walkthrough.on_status_rendered(session.status.buf) end
  end
end

---@param title string
---@param lines string[]
---@return integer
local function status_open_popup(title, lines)
  local width = #title + 4
  for _, line in ipairs(lines) do
    width = math.max(width, #line + 4)
  end
  width = math.min(math.max(width, 44), math.max(vim.o.columns - 4, 20))
  local height = math.min(#lines, math.max(vim.o.lines - 6, 1))

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].filetype = "DiffReviewHelp"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    width = width,
    height = height,
    col = math.floor((vim.o.columns - width) / 2),
    row = math.floor((vim.o.lines - height) / 2),
    style = "minimal",
    border = "rounded",
    title = " " .. title .. " ",
    title_pos = "center",
  })

  local function close()
    if vim.api.nvim_win_is_valid(win) then
      pcall(vim.api.nvim_win_close, win, true)
    end
  end
  for _, key in ipairs({ "q", "<Esc>", dr()._status_keys.primary_key("help") }) do
    if key ~= "" then
      vim.keymap.set("n", key, close, { buffer = buf, nowait = true, silent = true, desc = "Close help" })
    end
  end

  local key_width = 0
  for _, spec in ipairs(status_command_specs) do
    if keymaps.status_command_visible(spec) then
      key_width = math.max(key_width, #dr()._status_keys.key_text(keymaps.status_keys_for(spec.id)))
    end
  end
  local line_index = 0
  for _, spec in ipairs(status_command_specs) do
    if not keymaps.status_command_visible(spec) then goto continue end
    local key_text = dr()._status_keys.key_text(keymaps.status_keys_for(spec.id))
    if key_text ~= "" then
      line_index = line_index + 1
      pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, line_index - 1, 2, {
        end_col = 2 + #key_text,
        hl_group = "DiffReviewStatusHintKey",
        priority = 90,
      })
      pcall(vim.api.nvim_buf_set_extmark, buf, dr()._status_ns, line_index - 1, key_width + 6, {
        end_col = #lines[line_index],
        hl_group = "DiffReviewStatusHint",
        priority = 80,
      })
    end
    ::continue::
  end

  return buf
end

local function status_show_help()
  local key_width = 0
  for _, spec in ipairs(status_command_specs) do
    if keymaps.status_command_visible(spec) then
      key_width = math.max(key_width, #dr()._status_keys.key_text(keymaps.status_keys_for(spec.id)))
    end
  end

  local lines = {}
  for _, spec in ipairs(status_command_specs) do
    if not keymaps.status_command_visible(spec) then goto continue end
    local key_text = dr()._status_keys.key_text(keymaps.status_keys_for(spec.id))
    if key_text ~= "" then
      lines[#lines + 1] = ("  %-" .. key_width .. "s  %s"):format(key_text, spec.desc)
    end
    ::continue::
  end
  status_open_popup("DiffReview Commands", lines)
end

---@param buf integer
---@param action "push"|"pull"
local function status_remote_action(buf, action)
  git_backend.git_root_async(function(cwd, root_err)
    if not cwd then
      dr()._notify_error(root_err or "Unable to find git root")
      return
    end

    local title = action == "push" and "Push" or "Pull"
    local running_status = action == "push" and "Pushing..." or "Pulling..."
    local function update_remote_status(line)
      line = vim.trim(line or "")
      if line == "" then return end
      if session.status then
        session.status.remote_action = session.status.remote_action or { action = action, state = "running" }
        session.status.remote_action.status = line
        if session.status.head_values then
          session.status.head_lines = dr()._status_build_head_lines(session.status.head_values, session.status.pr, session.status.about, session.status.issues)
          if vim.api.nvim_buf_is_valid(buf) then
            dr().render_status(buf, nil, nil, { reuse_sections = true })
          end
        end
      end
    end
    if session.status then
      session.status.remote_action = { action = action, state = "running", status = running_status }
      if session.status.head_values then
        session.status.head_lines = dr()._status_build_head_lines(session.status.head_values, session.status.pr, session.status.about, session.status.issues)
        if vim.api.nvim_buf_is_valid(buf) then
          dr().render_status(buf, nil, nil, { reuse_sections = true })
        end
      end
    end
    dr()._notify_debug(title .. "ing changes...", vim.log.levels.INFO, { title = "DiffReview" })
    git_backend.system_text_stream_async({ "git", "-C", cwd, action, "--progress" }, nil, update_remote_status, function(result)
      local compact = {}
      for _, line in ipairs(git_backend.text_to_lines((result.stdout or "") .. (result.stderr or ""))) do
        if line ~= "" then
          compact[#compact + 1] = line
        end
      end
      if result.code == 0 then
        dr()._notify_debug(title .. " complete", vim.log.levels.INFO, { title = "DiffReview" })
      else
        dr()._notify_error(title .. " failed: " .. (#compact > 0 and table.concat(compact, "\n") or ("git exited " .. result.code)))
      end
      if vim.api.nvim_buf_is_valid(buf) then
        if session.status then session.status.remote_action = nil end
        dr()._render_status_or_notify(buf)
      end
    end)
  end)
end

-- Expose the bare-local action handlers that keymaps and other modules dispatch by name.
M._status_open_about = status_open_about
M._status_jump = status_jump
M._status_toggle = status_toggle
M._status_show_help = status_show_help
M._status_remote_action = status_remote_action

return M
