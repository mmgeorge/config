local M = {}

local client = require("diff_review.harness.client")
local command_set = require("diff_review.shared.view_command_set")
local config = require("diff_review.infra.config")
local keymaps = require("diff_review.shared.keymaps")
local notifications = require("diff_review.infra.notifications")
local renderer = require("diff_review.render.harness.interactions")
local annotations = require("diff_review.render.annotations")
local comment_box = require("diff_review.render.comment_box")
local session = require("diff_review.session")

local namespace = vim.api.nvim_create_namespace("DiffReviewInteractions")

---@return table?
local function view_state() return session.harness.interactions end

---@return table?
local function row_under_cursor()
  local state = assert(view_state())
  if not state then return nil end
  return state.row[vim.api.nvim_win_get_cursor(0)[1]]
end

local function apply_folds(state)
  if not (state.win and vim.api.nvim_win_is_valid(state.win)) then return end
  vim.api.nvim_win_call(state.win, function()
    vim.cmd("silent! normal! zE")
    for _, fold in ipairs(state.fold or {}) do
      if fold.last > fold.first then
        vim.cmd(fold.first .. "," .. fold.last .. "fold")
        if not fold.folded then vim.cmd("silent! " .. fold.first .. "foldopen") end
      end
    end
  end)
end

local function render(interaction)
  local state = view_state()
  if not state or not vim.api.nvim_buf_is_valid(state.buf) or not vim.api.nvim_win_is_valid(state.win) then return end
  local output = renderer.build(interaction)
  state.interaction = interaction
  state.row = output.row
  state.fold = output.fold
  vim.bo[state.buf].modifiable = true
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, output.lines)
  vim.api.nvim_buf_clear_namespace(state.buf, namespace, 0, -1)
  for _, highlight in ipairs(output.highlight) do
    vim.api.nvim_buf_add_highlight(state.buf, namespace, highlight.group, highlight.line - 1, 0, -1)
  end
  for _, comment in ipairs(output.comment) do
    local box_line = comment_box.build_box_lines({
      heading = "Review comment",
      body_lines = vim.split(comment.value.body or "", "\n", { plain = true }),
    }, vim.api.nvim_win_get_width(state.win))
    vim.api.nvim_buf_set_extmark(state.buf, namespace, comment.line - 1, 0, { virt_lines = box_line })
  end
  vim.bo[state.buf].modifiable = false
  apply_folds(state)
  local checkpoint = session.harness.no_checkpoint and "NO CHECKPOINT • rollback unavailable" or "Tracked per user interaction"
  keymaps.apply_view_winbar(state.win, config.options.harness.interactions_buffer_name, "interactions", state.command_set, checkpoint)
end

function M.refresh()
  client.request("interaction.list", {}, function(result, request_error)
    if request_error then notifications.error(request_error, "Interactions") return end
    render(result or {})
  end)
end

local function toggle()
  local row = row_under_cursor()
  if row and (row.kind == "interaction" or row.kind == "file") then vim.cmd("normal! za") end
end

local function add_comment()
  local state = view_state()
  local row = row_under_cursor()
  if not (state and row and row.kind == "diff" and row.file_path) then
    notifications.warn("Move to a changed line before adding an interaction comment", "Interactions")
    return
  end
  vim.ui.input({ prompt = "Interaction comment: " }, function(body)
    if not body or vim.trim(body) == "" then return end
    local comment = {
      id = ("nvim-%s-%s"):format(vim.fn.getpid(), vim.uv.hrtime()),
      interaction_id = row.interaction.id,
      file_path = row.file_path,
      old_line = row.old_line,
      new_line = row.new_line,
      body = vim.trim(body),
      created_at_ms = math.floor(vim.uv.hrtime() / 1000000),
    }
    annotations.upsert(state.annotation_index, {
      id = comment.id,
      kind = "harness_interaction",
      source_id = row.interaction.id,
      path = row.file_path,
      side = row.new_line and "RIGHT" or "LEFT",
      line = row.new_line or row.old_line or 0,
      body = comment.body,
      author = "You",
      editable = true,
      state = "new",
    })
    client.request("interaction.comment.save", comment, function(saved_comment, request_error)
      if request_error then notifications.error(request_error, "Interactions") return end
      row.interaction.comment = row.interaction.comment or {}
      row.interaction.comment[#row.interaction.comment + 1] = saved_comment or comment
      render(state.interaction)
    end)
  end)
end

local function request_changes()
  local row = row_under_cursor()
  if not row or not row.interaction then return end
  client.request("interaction.request_changes", { interaction_id = row.interaction.id }, function(_, request_error)
    if request_error then notifications.error(request_error, "Interactions") return end
    notifications.info("Interaction review queued as a new user interaction", "Harness")
  end)
end

local function rollback()
  local row = row_under_cursor()
  if not row or not row.interaction then return end
  if session.harness.no_checkpoint then
    notifications.warn("Rollback is unavailable because this session has NO CHECKPOINT", "Interactions")
    return
  end
  vim.ui.select({ "Cancel", "Rollback interaction " .. tostring(row.interaction.ordinal) }, {
    prompt = "Restore the worktree before this interaction and supersede later interactions?",
  }, function(choice)
    if not choice or choice == "Cancel" then return end
    client.request("interaction.rollback", { interaction_id = row.interaction.id }, function(_, request_error)
      if request_error then notifications.error(request_error, "Interactions rollback") return end
      notifications.info("Interaction rollback completed", "Interactions")
      M.refresh()
    end)
  end)
end

local function close()
  if vim.fn.tabpagenr("$") > 1 then vim.cmd("tabclose") else vim.cmd("enew") end
end

---@return DiffReviewViewCommandSet
local function commands()
  local set = command_set.new()
  command_set.register(set, "toggle", toggle)
  command_set.register(set, "comment", add_comment)
  command_set.register(set, "request_changes", request_changes)
  command_set.register(set, "rollback", rollback, { enabled = function() return not session.harness.no_checkpoint end })
  command_set.register(set, "refresh", M.refresh)
  command_set.register(set, "close", close)
  command_set.register(set, "help", function() keymaps.show_view_help("interactions", set, "Interactions") end)
  return set
end

local function open_ready()
  local name = config.options.harness.interactions_buffer_name
  local buf = vim.fn.bufnr(name)
  local existing_win = buf >= 0 and vim.fn.win_findbuf(buf)[1] or nil
  if existing_win and vim.api.nvim_win_is_valid(existing_win) then
    vim.api.nvim_set_current_tabpage(vim.api.nvim_win_get_tabpage(existing_win))
    vim.api.nvim_set_current_win(existing_win)
    M.refresh()
    return
  end
  vim.cmd("tabnew")
  if buf < 0 or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, name)
  end
  local win = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win, buf)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "DiffReviewInteractions"
  vim.bo[buf].modifiable = false
  vim.wo[win].foldmethod = "manual"
  vim.wo[win].foldenable = true
  session.harness.interactions = { buf = buf, win = win, row = {}, fold = {}, interaction = {} }
  local state = assert(view_state())
  state.command_set = commands()
  state.annotation_index = annotations.new_index()
  keymaps.setup_view_keymaps(buf, "interactions", state.command_set)
  M.refresh()
end

function M.open()
  client.start(function(_, start_error)
    if start_error then notifications.error(start_error, "Interactions") return end
    open_ready()
  end)
end

return M
