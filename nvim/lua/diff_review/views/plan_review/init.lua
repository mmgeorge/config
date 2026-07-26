local M = {}

local client = require("diff_review.harness.client")
local command_set = require("diff_review.shared.view_command_set")
local keymaps = require("diff_review.shared.keymaps")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local task_tree = require("diff_review.render.task_tree")
local session = require("diff_review.session")
local comment_view = require("diff_review.views.plan_review.comment")
local plan_fold = require("diff_review.views.plan_review.fold")
local plan_task_model = require("diff_review.views.plan_review.task_model")

local awaiting_review_status = "Awaiting review • read-only projection • C adds comments"

---@param plan table
---@return table[]
local function annotation_list(plan)
  local revision_key = plan.id .. ":" .. tostring(plan.model_revision or 0)
  session.harness.plan_annotations[revision_key] = session.harness.plan_annotations[revision_key] or {}
  return session.harness.plan_annotations[revision_key]
end

---@param buf integer
---@return table[]
local function serialized_annotation(buf)
  return comment_view.serialize(buf)
end

---@param plan table
---@param buf integer
local function open_source(plan, buf)
  local index_path = vim.fs.joinpath(vim.fs.dirname(plan.working_path), "working.index.json")
  local ok, source = pcall(vim.fn.readfile, index_path)
  if not ok then
    notifications.error("Failed to read plan navigation index", "PlanReview")
    return
  end
  local decoded_ok, index = pcall(vim.json.decode, table.concat(source, "\n"))
  if not decoded_ok then
    notifications.error("Failed to decode plan navigation index", "PlanReview")
    return
  end
  local line = comment_view.source_line_at_cursor(buf) or vim.api.nvim_win_get_cursor(0)[1]
  local candidate = nil
  for _, anchor in ipairs(index.anchor or {}) do
    if anchor.path and line == anchor.line then candidate = anchor end
  end
  if not candidate then
    notifications.info("This plan line has no source boundary", "PlanReview")
    return
  end
  vim.cmd("edit " .. vim.fn.fnameescape(vim.fs.joinpath(vim.fn.getcwd(), candidate.path)))
end

local function refresh_review_winbar(buf, status)
  local review = session.harness.plan_review
  if not review or review.buf ~= buf or not review.command_set then return end
  if not (review.win and vim.api.nvim_win_is_valid(review.win)) then return end
  keymaps.apply_view_winbar(review.win, "PlanReview", "plan_review", review.command_set, status)
end

local function review_request_available()
  if not session.harness.busy then return true end
  notifications.info("A Harness request is already running", "PlanReview")
  return false
end

local function begin_review_request(buf, status)
  if not review_request_available() then return false end
  session.harness.busy = true
  refresh_review_winbar(buf, status)
  require("diff_review.views.harness.controller").refresh_winbar()
  return true
end

local function finish_review_request(buf)
  session.harness.busy = false
  refresh_review_winbar(buf, awaiting_review_status)
  require("diff_review.views.harness.controller").refresh_winbar()
end

---@param win integer
local function configure_review_window(win)
  if not vim.api.nvim_win_is_valid(win) then return end
  vim.wo[win].number = true
  vim.wo[win].relativenumber = false
  vim.wo[win].statuscolumn = vim.go.statuscolumn
end

---@param buf integer
---@param win integer
local function attach_review_window_options(buf, win)
  configure_review_window(win)
  local group = vim.api.nvim_create_augroup("DiffReviewPlanReviewWindow" .. tostring(buf), { clear = true })
  vim.api.nvim_create_autocmd("BufWinEnter", {
    group = group,
    buffer = buf,
    callback = function() configure_review_window(vim.api.nvim_get_current_win()) end,
  })
end

---@class DiffReviewPlanReviewSession
---@field plan table
---@field buf integer
---@field win integer
---@field tab integer
---@field return_tab integer
---@field return_win integer
---@field comment_state table
---@field task_model DiffReviewPlanTaskModel?
---@field fold_controller DiffReviewPlanFoldController
---@field command_set DiffReviewViewCommandSet?

---@param review DiffReviewPlanReviewSession
local function close_review(review)
  review.fold_controller:detach(review.buf)
  if vim.api.nvim_buf_is_valid(review.buf) then comment_view.detach(review.buf) end
  if vim.api.nvim_tabpage_is_valid(review.tab) and review.tab ~= review.return_tab
      and vim.fn.tabpagenr("$") > 1 then
    vim.api.nvim_set_current_tabpage(review.tab)
    vim.cmd("tabclose")
  end
  if vim.api.nvim_buf_is_valid(review.buf) then vim.api.nvim_buf_delete(review.buf, { force = true }) end
  if vim.api.nvim_tabpage_is_valid(review.return_tab) then
    vim.api.nvim_set_current_tabpage(review.return_tab)
    if vim.api.nvim_win_is_valid(review.return_win) then vim.api.nvim_set_current_win(review.return_win) end
  end
  if session.harness.plan_review == review then session.harness.plan_review = nil end
end

---@param plan table
---@param review DiffReviewPlanReviewSession
local function accept(plan, review)
  if not begin_review_request(review.buf, "Starting plan approval…") then return end
  close_review(review)
  client.request("plan.acceptance.begin", {
    plan_id = plan.id,
    digest = plan.review_digest,
  }, function(result, request_error)
    finish_review_request(review.buf)
    if request_error then
      notifications.error(request_error, "PlanReview")
      return
    end
    if result then
      session.harness.session = result.session or session.harness.session
      session.harness.capability = result.capability or session.harness.capability
      session.harness.active_plan = result.active_plan or session.harness.active_plan
      session.harness.active_elicitation = result.active_elicitation
    end
    local controller = require("diff_review.views.harness.controller")
    controller.render()
    vim.schedule(function() controller.present_plan_question(true) end)
  end)
end

---@param plan table
---@param buf integer
local function request_changes(plan, review)
  if not review_request_available() then return end
  local annotation = serialized_annotation(review.buf)
  popup_window.input({ prompt = "Overall plan review comment (optional): " }, function(comment)
    if comment == nil then return end
    if not begin_review_request(review.buf, "Revising plan…") then return end
    close_review(review)
    client.request("plan.request_changes", {
      plan_id = plan.id,
      annotations = annotation,
      comment = vim.trim(comment),
    }, function(_, request_error)
      finish_review_request(review.buf)
      if request_error then
        notifications.error(request_error, "PlanReview")
        return
      end
      local revision_key = plan.id .. ":" .. tostring(plan.model_revision or 0)
      session.harness.plan_annotations[revision_key] = nil
      notifications.info("Plan revision requested", "PlanReview")
      require("diff_review.views.harness.controller").render()
    end)
  end)
end

---@param plan table
---@param buf integer
---@return DiffReviewViewCommandSet
local function commands(plan, review)
  local set = command_set.new()
  command_set.register(set, "toggle", function()
    review.fold_controller:toggle(review.buf, review.win)
  end)
  command_set.register(set, "open", function() open_source(plan, review.buf) end)
  command_set.register(set, "comment", function() comment_view.add_at_cursor(review.buf) end)
  command_set.register(set, "accept", function() accept(plan, review) end)
  command_set.register(set, "request_changes", function() request_changes(plan, review) end)
  command_set.register(set, "close", function() close_review(review) end)
  command_set.register(set, "help", function() keymaps.show_view_help("plan_review", set, "PlanReview") end)
  return set
end

---@param plan table
function M.open(plan)
  assert(type(plan) == "table" and type(plan.working_path) == "string", "PlanReview requires a physical plan path")
  local origin_win = session.harness.transcript_win
  if not (origin_win and vim.api.nvim_win_is_valid(origin_win)) then origin_win = vim.api.nvim_get_current_win() end
  local origin_tab = vim.api.nvim_win_get_tabpage(origin_win)
  local existing_buf = vim.fn.bufnr(plan.working_path)
  local existing_win = existing_buf >= 0 and vim.fn.win_findbuf(existing_buf)[1] or nil
  if existing_win and vim.api.nvim_win_is_valid(existing_win) then
    vim.api.nvim_set_current_tabpage(vim.api.nvim_win_get_tabpage(existing_win))
    vim.api.nvim_set_current_win(existing_win)
    vim.cmd("silent edit! " .. vim.fn.fnameescape(plan.working_path))
  else
    vim.cmd("tabnew")
    vim.cmd("edit " .. vim.fn.fnameescape(plan.working_path))
  end
  local buf = vim.api.nvim_get_current_buf()
  local win = vim.api.nvim_get_current_win()
  local tab = vim.api.nvim_get_current_tabpage()
  attach_review_window_options(buf, win)
  local source_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  vim.bo[buf].filetype = "markdown"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].modifiable = false
  local task_model, task_model_error = plan_task_model.load(plan.working_path)
  if task_model_error then notifications.error(task_model_error, "PlanReview") end
  local fold_controller = plan_fold.new()
  local comment_options = nil
  if task_model then
    comment_options = {
      source_provider = function(width)
        return task_model:compose(task_tree.render(task_model:task_nodes(), width), width)
      end,
      before_render = function(render_buf, render_win)
        fold_controller:capture(render_buf, render_win)
      end,
      after_render = function(render_buf, render_win, projection)
        fold_controller:apply(render_buf, render_win, projection)
      end,
    }
  end
  local comment_state = comment_view.attach(buf, win, source_lines, annotation_list(plan), comment_options)
  local review = {
    plan = plan,
    buf = buf,
    win = win,
    tab = tab,
    return_tab = origin_tab,
    return_win = origin_win,
    comment_state = comment_state,
    task_model = task_model,
    fold_controller = fold_controller,
  }
  local set = commands(plan, review)
  review.command_set = set
  session.harness.plan_review = review
  keymaps.setup_view_keymaps(buf, "plan_review", set)
  refresh_review_winbar(buf, awaiting_review_status)
end

return M
