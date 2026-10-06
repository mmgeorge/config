local M = {}
local client = require("forge.client")
local session = require("forge.session")
local command_set = require("forge.shared.view_command_set")
local keymaps = require("forge.shared.keymaps")
local notifications = require("forge.infra.notifications")
local popup = require("forge.infra.popup_window")
local buffer = require("forge.buffer")

local function notice(message) notifications.error(message, "ForgePlanReview") end

---@class ForgeNativePlanFold
---@field id string
---@field owner string
---@field heading_start_line integer
---@field start_line integer
---@field end_line integer
---@field record table

---@param review table
---@return ForgeNativePlanFold[]
local function task_folds(review, owner)
  local replica = owner and owner.replica or (review.owner and review.owner.replica)
  local state = replica and replica.fold
  local result = {}
  for id, record in pairs(state and state.record or {}) do
    if id:match("^plan:design:") or id:match("^plan:section:") then
      local _, block_row = replica.sequence:position(record.owner)
      block_row = buffer.physical_row(replica, block_row)
      local heading_row = block_row
      if record.fold.heading_start then
        heading_row = buffer.physical_row(replica, select(2, replica.sequence:position(record.fold.heading_start.block)) + record.fold.heading_start.position.row)
      end
      local finish_block_row = buffer.physical_row(replica, select(2, replica.sequence:position(record.fold["end"].block)))
      result[#result + 1] = {
        id = id,
        owner = record.owner,
        heading_start_line = heading_row + 1,
        start_line = buffer.physical_row(replica, select(2, replica.sequence:position(record.owner)) + record.fold.start.row) + 1,
        end_line = buffer.physical_row(replica, select(2, replica.sequence:position(record.fold["end"].block)) + record.fold["end"].position.row + 1),
        record = record,
      }
    end
  end
  table.sort(result, function(left, right)
    if left.start_line == right.start_line then return left.end_line < right.end_line end
    return left.start_line < right.start_line
  end)
  return result
end

---@param review table
---@param owner table?
---@return ForgeNativePlanFold[]
local function default_closed_folds(review, owner)
  local replica = owner and owner.replica or (review.owner and review.owner.replica)
  local state = replica and replica.fold
  local result = {}
  for id, record in pairs(state and state.record or {}) do
    if record.fold.closed and not (id:match("^plan:design:") or id:match("^plan:section:")) then
      local _, block_row = replica.sequence:position(record.owner)
      local finish_block_row = buffer.physical_row(replica, select(2, replica.sequence:position(record.fold["end"].block)))
      result[#result + 1] = {
        id = id,
        owner = record.owner,
        heading_start_line = buffer.physical_row(replica, block_row) + 1,
        start_line = buffer.physical_row(replica, block_row + record.fold.start.row) + 1,
        end_line = buffer.physical_row(replica, select(2, replica.sequence:position(record.fold["end"].block)) + record.fold["end"].position.row + 1),
        record = record,
      }
    end
  end
  return result
end

---@param review table
---@param id string
---@param folded boolean
local function set_task_folded(review, id, folded)
  review.task_folded_by_id[id] = folded
end

---@param review table
---@param window integer
local function apply_task_folds(review, window, owner)
  if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= review.buf then return end
  vim.api.nvim_win_call(window, function()
    local saved_view = vim.fn.winsaveview()
    vim.cmd("silent! normal! zx")
    local folds = task_folds(review, owner)
    vim.list_extend(folds, default_closed_folds(review, owner))
    table.sort(folds, function(left, right) return left.start_line > right.start_line end)
    for _, fold in ipairs(folds) do
      local closed = review.task_folded_by_id[fold.id]
      if closed == nil then closed = fold.record.fold.closed end
      if closed then
        vim.api.nvim_win_set_cursor(window, { fold.start_line, 0 })
        vim.cmd("silent! normal! zc")
      end
    end
    vim.fn.winrestview(saved_view)
  end)
end

---@param review table
---@param on_projected fun()
local function toggle_task_fold(review, on_projected)
  local view = review.owner.current_view()
  if not view then return end
  require("forge.folds").toggle_heading(review.owner.replica, view.window, {
    on_projected = on_projected,
    on_toggled = function(id, closed) set_task_folded(review, id, closed) end,
  })
end

local function hide_review(review)
  if vim.api.nvim_tabpage_is_valid(review.tab) and review.tab ~= review.return_tab and vim.fn.tabpagenr("$") > 1 then
    vim.api.nvim_set_current_tabpage(review.tab)
    vim.cmd("tabclose")
  end
  if vim.api.nvim_tabpage_is_valid(review.return_tab) then
    vim.api.nvim_set_current_tabpage(review.return_tab)
    if vim.api.nvim_win_is_valid(review.return_win) then vim.api.nvim_set_current_win(review.return_win) end
  end
end

local function close_review(review)
  if not review.owner.close() then notice("Plan review has an explicit operation in progress") return false end
  hide_review(review)
  if session.harness.plan_review == review and not vim.bo[review.buf].modified then session.harness.plan_review = nil end
  return true
end

local function discard_failed_attachment(review)
  if session.harness.plan_review == review then session.harness.plan_review = nil end
  if vim.api.nvim_tabpage_is_valid(review.tab) and review.tab ~= review.return_tab and vim.fn.tabpagenr("$") > 1 then
    vim.api.nvim_set_current_tabpage(review.tab)
    vim.cmd("tabclose")
  end
  if vim.api.nvim_tabpage_is_valid(review.return_tab) then
    vim.api.nvim_set_current_tabpage(review.return_tab)
    if vim.api.nvim_win_is_valid(review.return_win) then vim.api.nvim_set_current_win(review.return_win) end
  end
end

local function show_document(review, snapshot, title, filetype, selection)
  if type(snapshot) ~= "table" then return end
  local view = review.owner.current_view()
  if not view then return end
  local rows = 0
  for _, block in ipairs(snapshot.block) do rows = rows + #block.text end
  local native_buffer, window = popup.open({ parent_win = view.window, width = math.max(20, math.min(80, vim.api.nvim_win_get_width(view.window) - 4)),
    height = math.max(1, math.min(rows, 24)), title = title, filetype = filetype or "markdown" })
  local replica = buffer.open(snapshot.document, { buffer = native_buffer, generated = true,
    expected_changedtick = vim.api.nvim_buf_get_changedtick(native_buffer), filetype = filetype or "markdown", notice = notice })
  if buffer.apply_snapshot(replica, snapshot).kind ~= "Applied" then buffer.close(replica) return end
  if selection then
    vim.api.nvim_win_set_cursor(window, { selection.row + 1, selection.column })
    vim.api.nvim_win_call(window, function() vim.cmd("normal! zv") end)
  end
  local closed = false
  local function close()
    if closed then return end
    closed = true
    popup.close(window, true)
    buffer.close(replica)
  end
  for _, key in ipairs({ "q", "<Esc>" }) do vim.keymap.set("n", key, close, { buffer = native_buffer, silent = true }) end
  vim.api.nvim_create_autocmd("BufWipeout", { buffer = native_buffer, once = true, callback = close })
end

local function effect(review, captured, fields)
  local view = review.owner.current_view()
  if not view or view.id ~= captured.view then return end
  require("forge.effects").apply(review.owner.replica, view, vim.tbl_extend("force", {
    id = "plan:" .. captured.action, document = captured.document, revision = captured.revision,
    view = captured.view, sequence = captured.sequence,
  }, fields))
end

local function rustdoc(review, result, captured, source)
  local target = result.anchor.target
  if target.target_type ~= "flow_edge" or target.reference_kind ~= "external_entity" then
    notifications.info("This plan target has no external Rust documentation", "ForgePlanReview") return
  end
  if review.owner.is_current(captured) then
    client.request_for(review.session_id, source and "plan.rustdoc.source" or "plan.rustdoc.hover", {
      plan_id = review.plan.id, expected_version = result.version, json_path = result.anchor.json_path,
      selection = result.rustdoc_selection,
    }, function(resolved, failure)
      if not review.owner.is_current(captured) then return end
      if failure then notice(failure) return end
      if source then effect(review, captured, { kind = "open_file", path = resolved.path,
        row = math.max(0, resolved.line - 1), column = math.max(0, resolved.column - 1) })
      else show_document(review, resolved.document, "Rust documentation") end
    end)
  end
end

local function refresh_winbar(review)
  local plan = review.plan
  local detail = plan.historical_revision and ("Revision " .. plan.historical_revision .. " • historical • read-only")
    or plan.state == "accepted" and "Accepted plan • read-only projection • C adds comments"
    or "Awaiting review • C comment • A question • Ctrl-S save / ask"
  detail = detail .. " • Showing: " .. (review.public_only and "Public" or "All")
  keymaps.apply_view_winbar(review.win, "PlanReview", "plan_review", review.command_set, detail)
end

local function action(review, name)
  if name == "rename_entity" and (review.plan.historical_revision or review.plan.state ~= "awaiting_review") then
    notice("Only plans awaiting review can be renamed") return
  end
  if review.plan.historical_revision and (name == "comment" or name == "question" or name == "delete") then return end
  local opening_cursor = vim.api.nvim_win_get_cursor(review.win)
  review.owner.action(name, function(result, failure, captured)
    if (name == "rename_entity" or name == "jump_entity" or name == "references" or name:find("reveal_reference:", 1, true) == 1) and (not review.owner.is_current(captured)
        or not vim.api.nvim_win_is_valid(review.win)
        or vim.api.nvim_win_get_buf(review.win) ~= review.buf
        or not vim.deep_equal(vim.api.nvim_win_get_cursor(review.win), opening_cursor)) then return end
    if failure then notice(failure) return end
    if name == "toggle_declaration" then
      effect(review, captured, { kind = "cursor", block = result.jump.block, position = result.jump.position, viewport = result.viewport })
    elseif name == "toggle_public" then
      review.public_only = result.public_only
      review.owner.public_only = result.public_only
      refresh_winbar(review)
    elseif name == "comment" or name == "question" then
      if result.local_draft then
        vim.cmd("silent! normal! zv")
        vim.cmd("startinsert")
        return
      end
      local _, row = review.owner.replica.sequence:position(result.block)
      local view = review.owner.current_view()
      if row and view then
        vim.api.nvim_set_current_win(view.window)
        vim.api.nvim_win_set_cursor(view.window, { row + result.row + 1, 0 })
        vim.cmd("silent! normal! zv")
        vim.cmd("startinsert")
      end
    elseif name == "delete" then
      return
    elseif name == "rename_entity" and type(result.rename) == "table" then
      require("forge.views.plan_review.entity_rename").proposed(review, result.rename, captured, function(name)
        if session.harness.busy then notice("A Harness request is already running") return end
        session.harness.busy = true
        review.owner.submit("plan.entity.rename", { symbol = result.rename.symbol, name = name,
          expected_version = result.rename.expected_version }, function(renamed, failure)
          session.harness.busy = false
          if failure then notice(failure) return end
          session.harness.active_plan = renamed.plan
          close_review(review)
          vim.schedule(function() M.open(renamed.plan) end)
        end)
      end)
    elseif name == "references" and type(result.references) == "table" then
      require("forge.views.plan_review.references").open(review, result.references, captured)
    elseif type(result.message) == "string" then notifications.info(result.message, "ForgePlanReview")
    elseif type(result.declarations) == "table" then show_document(review, result.declarations, "Declarations", result.filetype, result.selection)
    elseif name == "schema" then show_document(review, result.schema, "Canonical plan", "json")
    elseif name == "entity_info" then
      if type(result.info) == "table" then show_document(review, result.info, "Plan entity") else rustdoc(review, result, captured, false) end
    elseif (name == "jump_entity" or name:find("reveal_reference:", 1, true) == 1) and type(result.jump) == "table" then
      effect(review, captured, { kind = "cursor", jump = true, block = result.jump.block, position = result.jump.position })
    elseif name == "open" and type(result.anchor) == "table" and type(result.anchor.target) == "table"
      and result.anchor.target.target_type == "dependency" then
      require("forge.views.plan_review.dependency_browser").open(result.anchor.target.name)
    elseif type(result.source) == "table" then
      effect(review, captured, { kind = "open_file", path = result.source.path, row = result.source.line - 1, column = result.source.column or 0 })
    else rustdoc(review, result, captured, true) end
  end)
end

local function submit(review, method, params)
  if review.plan.historical_revision then return end
  if session.harness.busy then notifications.info("A Harness request is already running", "ForgePlanReview") return end
  session.harness.busy = true
  local controller = require("forge.views.harness.controller")
  controller.refresh_winbar()
  local queued = review.owner.submit(method, params, function(result, failure)
    session.harness.busy = false
    controller.refresh_winbar()
    if failure then
      notice(failure)
      if session.harness.session and session.harness.session.id == review.session_id
        and (not session.harness.plan_review or session.harness.plan_review == review) then M.open(review.plan) end
      return
    end
    local current = session.harness.plan_review
    local completed = current and current.buf == review.buf and current.plan.id == review.plan.id
      and current.plan.review_digest == review.plan.review_digest
      and current.plan.historical_revision == review.plan.historical_revision and current or review
    if not completed.owner.closed then close_review(completed) end
    if result then controller.activate_snapshot(result) end
    controller.render()
    if method == "plan.acceptance.begin" then vim.schedule(function() controller.present_plan_question(true) end) end
  end)
  if queued and (review.owner.submission_pending or review.owner.pending_operation) then hide_review(review) end
end

local function commands(review)
  local set = command_set.new()
  command_set.register(set, "toggle", function() toggle_task_fold(review, function() action(review, "toggle_declaration") end) end)
  command_set.register(set, "visual_line_with_gutter", review.owner.gutter_selection.start)
  for _, name in ipairs({ "open", "jump_entity", "references", "rename_entity", "entity_info", "schema", "comment", "question", "delete", "toggle_public" }) do command_set.register(set, name, function() action(review, name) end) end
  command_set.register(set, "save", function() if not review.plan.historical_revision then vim.cmd("write") end end)
  command_set.register(set, "accept", function() submit(review, "plan.acceptance.begin", {}) end)
  command_set.register(set, "abort_plan", function()
    if not review.plan.historical_revision and session.harness.active_plan
      and session.harness.active_plan.id == review.plan.id then
      require("forge.views.harness.controller").abort_plan()
    end
  end)
  command_set.register(set, "request_changes", function()
    if review.plan.historical_revision then return end
    local tick = vim.api.nvim_buf_get_changedtick(review.buf)
    popup.input({ prompt = "Overall plan review comment (optional): " }, function(comment)
      if not comment then return end
      if not vim.api.nvim_buf_is_valid(review.buf) or tick ~= vim.api.nvim_buf_get_changedtick(review.buf) then
        notice("Plan annotations changed while the review comment was open") return
      end
      submit(review, "plan.request_changes", { comment = vim.trim(comment) })
    end)
  end)
  command_set.register(set, "close", function() close_review(review) end)
  command_set.register(set, "help", function() keymaps.show_view_help("plan_review", set, "PlanReview") end)
  return set
end

function M.open(plan)
  assert(type(plan) == "table" and type(plan.working_path) == "string", "PlanReview requires a physical plan path")
  local previous = session.harness.plan_review
  local recovery
  if previous and previous.plan.id == plan.id and previous.plan.review_digest == plan.review_digest
    and previous.plan.historical_revision == plan.historical_revision and vim.api.nvim_buf_is_valid(previous.buf) then
    if previous.owner.generation == client.host_generation() and not previous.owner.closed and previous.owner.attached() then
      local window = vim.fn.win_findbuf(previous.buf)[1]
      if window then vim.api.nvim_set_current_win(window) else vim.cmd("tabnew") vim.api.nvim_win_set_buf(0, previous.buf) end
      previous.win = vim.api.nvim_get_current_win()
      previous.tab = vim.api.nvim_get_current_tabpage()
      previous.owner.refresh_views()
      return
    end
    if not previous.owner.closed then
      local failure
      recovery, failure = previous.owner.recovery()
      if not recovery then notice(failure) return end
      previous.owner.close()
    end
  elseif previous and not close_review(previous) then return end
  local origin = session.harness.transcript_win
  if not origin or not vim.api.nvim_win_is_valid(origin) then origin = vim.api.nvim_get_current_win() end
  local origin_tab = vim.api.nvim_win_get_tabpage(origin)
  local native_buffer = vim.fn.bufnr(plan.working_path)
  if native_buffer >= 0 and vim.api.nvim_buf_is_loaded(native_buffer) and vim.bo[native_buffer].modified and not recovery then
    notice("The physical plan buffer has unsaved changes") return
  end
  if native_buffer < 0 then
    native_buffer = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(native_buffer, plan.working_path)
  end
  if not recovery then
    vim.bo[native_buffer].buftype = "nofile"
    vim.bo[native_buffer].filetype = "forge"
    vim.bo[native_buffer].modifiable = true
    vim.api.nvim_buf_set_lines(native_buffer, 0, -1, false, { "Loading plan review…" })
    vim.bo[native_buffer].modified = false
    vim.bo[native_buffer].modifiable = false
  end
  vim.cmd("tabnew")
  vim.api.nvim_win_set_buf(0, native_buffer)
  local window = vim.api.nvim_get_current_win()
  vim.bo[native_buffer].bufhidden = "hide"
  vim.bo[native_buffer].swapfile = false
  local review = { plan = plan, buf = native_buffer, win = window, tab = vim.api.nvim_get_current_tabpage(),
    return_win = origin, return_tab = origin_tab, session_id = session.harness.session.id, task_folded_by_id = {} }
  session.harness.plan_review = review
  local loading_commands = command_set.new()
  command_set.register(loading_commands, "close", function() close_review(review) end)
  keymaps.setup_view_keymaps(native_buffer, "plan_review", loading_commands)
  keymaps.apply_view_winbar(window, "PlanReview", "plan_review", loading_commands, "Loading review")
  review.owner = require("forge.views.plan_review.document").attach({ plan = plan, session_id = review.session_id,
    buffer = native_buffer, window = window, recovery = recovery, notice = notice,
    recovery_provider = recovery and function() return previous.owner.recovery() end or nil,
    configure_view = function(view, owner) apply_task_folds(review, view.window, owner) end }, function(owner, failure)
    if failure then
      discard_failed_attachment(review)
      if recovery then session.harness.plan_review = previous end
      notice(failure)
      return
    end
    review.owner = owner
    review.public_only = owner.public_only
    apply_task_folds(review, owner.view.window)
    local set = commands(review)
    review.command_set = set
    keymaps.setup_view_keymaps(native_buffer, "plan_review", set)
    refresh_winbar(review)
  end)
end

return M
