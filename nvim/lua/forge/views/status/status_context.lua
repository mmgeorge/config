local M = {}
local client = require("forge.client")
local ai_commit = require("forge.integrations.ai_commit")
local gh = require("forge.integrations.gh")
local notifications = require("forge.infra.notifications")

local function confirm_create_pull_request(callback)
  require("forge.infra.confirm").open({ "No GitHub PR found for this branch.", "", "Create a draft PR now?" },
    callback, nil, { title = "ForgeStatus", min_width = 40 })
end

---@param prs ForgeGhPR[]? Array of pull request descriptors.
---@return ForgeGhPR? active Newest active pull request.
---@return ForgeGhPR? closed Newest closed pull request fallback.
local function select_branch_pr(prs)
  local ordered = vim.deepcopy(prs or {})
  table.sort(ordered, function(left, right)
    return tonumber(left.number) > tonumber(right.number)
  end)
  local active
  local closed
  for _, pr in ipairs(ordered) do
    local state = tostring(pr.state or "OPEN"):upper()
    if not active and state == "OPEN" then active = pr end
    if not closed and state == "CLOSED" then closed = pr end
  end
  return active, closed
end

function M.producer_handlers(options)
  local function refresh()
    if options.is_alive() then options.refresh_status() end
  end
  local function window()
    return type(options.window) == "function" and options.window() or options.window
  end
  local function open_pull_request(review)
    if not options.is_alive() then return end
    local context = options.context and options.context()
    if not context then notifications.error("Pull request context is still loading") return end
    context.open_pull_request(window(), review)
  end
  local function write(action)
    if not options.is_alive() then return end
    local console = vim.api.nvim_create_buf(false, true)
    vim.bo[console].bufhidden = "wipe"
    vim.bo[console].filetype = "git"
    local rows = 0
    local function progress(text)
      if not vim.api.nvim_buf_is_valid(console) then return end
      local lines = vim.split(text, "\n", { plain = true, trimempty = true })
      if #lines == 0 then return end
      vim.bo[console].modifiable = true
      vim.api.nvim_buf_set_lines(console, rows, -1, false, lines)
      rows = rows + #lines
      vim.bo[console].modifiable = false
    end
    local host_window = window()
    local previous = host_window and vim.api.nvim_win_is_valid(host_window) and vim.api.nvim_win_get_buf(host_window)
    if previous then vim.api.nvim_win_set_buf(host_window, console) end
    require("forge.git.write").execute(options.workspace, action, function(result)
      if result.ok then
        if previous and vim.api.nvim_win_is_valid(host_window) and vim.api.nvim_win_get_buf(host_window) == console
            and vim.api.nvim_buf_is_valid(previous) then vim.api.nvim_win_set_buf(host_window, previous) end
        if vim.api.nvim_buf_is_valid(console) then vim.api.nvim_buf_delete(console, { force = true }) end
      else
        progress(result.output)
        notifications.error("Git " .. action.kind .. " failed: " .. result.output)
        if vim.api.nvim_buf_is_valid(console) then
          vim.keymap.set("n", "q", function()
            if previous and vim.api.nvim_win_is_valid(host_window) and vim.api.nvim_buf_is_valid(previous) then
              vim.api.nvim_win_set_buf(host_window, previous)
            end
            if vim.api.nvim_buf_is_valid(console) then vim.api.nvim_buf_delete(console, { force = true }) end
          end, { buffer = console, nowait = true, silent = true })
        end
      end
      refresh()
    end, progress)
  end
  return {
    commit = function()
      if options.is_alive() then
        require("forge.integrations.commit").commit({ win = window(), workspace = options.workspace, on_done = refresh })
      end
    end,
    push = function() write({ kind = "push" }) end,
    pull = function() write({ kind = "pull" }) end,
    pr = function() open_pull_request(false) end,
    review = function() open_pull_request(true) end,
    branch_create = function()
      if not options.is_alive() then return end
      local info = type(options.context_info) == "function" and options.context_info() or options.context_info
      if not info then notifications.error("Repository context is still loading") return end
      local prefix = info.branch_prefix or ""
      require("forge.views.status.dialogs").branch_name(prefix, function(name)
        if not name or not options.is_alive() then return end
        name = vim.trim(name)
        if name ~= "" and name ~= prefix then write({ kind = "create_branch", name = name }) end
      end)
    end,
  }
end

function M.attach(options)
  local context_request = options.request or function(params, callback)
    client.request_host("status.context", params, callback)
  end
  assert(type(options.document_id) == "string" and type(options.workspace) == "string", "missing Status context identity")
  assert(type(options.present) == "function" and type(options.is_alive) == "function", "missing Status context owner")
  assert(type(options.capture_input) == "function" and type(options.is_input_current) == "function", "missing Status input owner")
  local owner = { closed = false, revision = 0,
    presentation = { pr = { state = "fetching", text = "" }, about = { state = "none", text = "" } } }
  local function alive(revision)
    return not owner.closed and options.is_alive() and (revision == nil or revision == owner.revision)
  end
  local function stop_lookup_timers()
    if owner.pr_timer then
      pcall(function() owner.pr_timer:stop() owner.pr_timer:close() end)
      owner.pr_timer = nil
    end
  end
  local function publish()
    if alive() then options.present(vim.deepcopy(owner.presentation)) end
  end

  function owner.generate_about(force)
    if not alive() then return end
    owner.about_started = true
    owner.about_revision = (owner.about_revision or 0) + 1
    local revision = owner.about_revision
    local function accept_about(result)
      if not alive() or revision ~= owner.about_revision then return end
      if result.state == "error" and result.error then notifications.error("Commit generation failed: " .. result.error) end
      owner.about = result
      owner.presentation.about = { state = result.state, text = result.message or "" }
      publish()
    end
    local first_request = not owner.about_requested
    owner.about_requested = true
    ai_commit.ensure(options.workspace, { ref = "HEAD", force = force == true or first_request, ignored_paths = options.ignored_paths and options.ignored_paths() or {}, on_start = accept_about }, accept_about)
  end

  local function open_commit(action, _, captured)
    require("forge.source_document").open_commit({ workspace = options.workspace, oid = action.oid, window = options.input_window(captured),
      is_current = function() return alive() and options.is_input_current(captured) end })
  end

  local function open_about(_, _, captured)
    if not owner.about or owner.about.state == "none" then owner.generate_about(true) return end
    if owner.about.state == "generating" then vim.notify("Commit message is still generating", vim.log.levels.INFO, { title = "Forge" }) return end
    if owner.about.state == "error" then notifications.error(owner.about.error or "Commit message generation failed") return end
    require("forge.source_document").open_about({ workspace = options.workspace, status_document = options.document_id, message = owner.about.message, window = options.input_window(captured),
      is_current = function() return alive() and options.is_input_current(captured) end })
  end

  local function open_pr(_, _, captured, command)
    local target_window = captured and options.input_window(captured) or command.window
    local revision = owner.revision
    local function current()
      return alive(revision) and vim.api.nvim_win_is_valid(target_window)
        and (not captured or options.is_input_current(captured))
    end
    local function create(confirmed)
      if not current() then return end
      local function open_creation()
        if current() then vim.api.nvim_win_call(target_window, function() require("github.open_pr").open({ cwd = options.workspace }) end) end
      end
      if confirmed then open_creation() else confirm_create_pull_request(open_creation) end
    end
    if owner.presentation.pr.state == "fetching" then vim.notify("Pull request lookup is still running", vim.log.levels.INFO, { title = "Forge" }) return end
    if owner.presentation.pr.state == "error" or owner.presentation.pr.state == "unavailable" then notifications.error("Pull request lookup is " .. owner.presentation.pr.state) return end
    if not owner.pr then create() return end
    local pr = owner.pr
    local function open()
      if not current() then return end
      vim.api.nvim_win_call(target_window, function()
        local commands = require("forge.views.commands")
        local open = command and command.review and commands.open_review or commands.open_pr
        open(pr, { cwd = options.workspace })
      end)
    end
    if owner.presentation.pr.state == "closed" then
      require("forge.infra.choice_popup").open({ title = "Closed pull request", relative = "editor", min_width = 38,
        options = { { key = "o", value = "open", label = "Open closed PR #" .. tostring(pr.number) }, { key = "c", value = "create", label = "Create a new draft PR" } },
        on_choice = function(choice) if choice == "open" then open() elseif choice == "create" then create(true) end end })
    else open() end
  end

  function owner.open_pull_request(window, review)
    if alive() then open_pr(nil, nil, nil, { window = window, review = review }) end
  end

  function owner.refresh(refresh)
    if not alive() then return end
    refresh = refresh or {}
    if owner.pr_timer then
      pcall(function() owner.pr_timer:stop() owner.pr_timer:close() end)
      owner.pr_timer = nil
    end
    owner.revision = owner.revision + 1
    local revision = owner.revision
    owner.info = assert(options.get_info(), "Missing native status context")
    refresh.branch = type(owner.info.branch) == "string" and owner.info.branch or nil
    owner.presentation.pr = { state = "fetching", text = "" }
    publish()
    local function accept_pr(result, state)
      if not alive(revision) then return end
      owner.pr = result.pr
      owner.presentation.pr = { state = state or (result.unavailable and "unavailable" or not result.ok and "error" or result.pr and "ready" or "none"), text = result.pr and result.pr.title or "" }
      if result.error then notifications.error("Status pull request lookup failed: " .. tostring(result.error)) end
      publish()
    end
    local config = require("forge.infra.config")
    local settings = config.options or config.defaults
    if settings.pr_lookup_mode == "mock-delay" then
      owner.pr_timer = vim.defer_fn(function()
        owner.pr_timer = nil
        accept_pr({ ok = true })
      end, math.max(1, tonumber(settings.pr_mock_delay_ms) or 5000))
    elseif refresh.branch and refresh.branch ~= "" then
      gh.prs_for_branch_async(options.workspace, refresh.branch, nil, function(result)
        if not alive(revision) then return end
        if not result.ok or result.unavailable then accept_pr(result) return end
        local active, closed = select_branch_pr(result.prs)
        local candidate = active or closed
        if not candidate then accept_pr({ ok = true }) return end
        gh.pr_async(options.workspace, candidate.number, candidate.repo, function(detail)
          accept_pr(detail, detail.ok and (active and "ready" or "closed") or nil)
        end)
      end)
    else
      gh.current_pr_async(options.workspace, accept_pr)
    end
    if settings.about_auto_generate ~= false and not owner.about_started then
      owner.generate_about(refresh.force)
    end
  end

  function owner.activate(target_id)
    if not alive() then return end
    local captured, capture_error = options.capture_input(target_id)
    if not captured then notifications.error(capture_error or "Status context input is unavailable") return end
    context_request({ operation = "action", input = captured }, function(action, failure)
      if not alive() then return end
      if failure then notifications.error("Status context action failed: " .. failure) return end
      if not action then return end
      if not options.is_input_current(captured) then return end
      local handler = ({ commit = options.open_commit or open_commit, pull_request = options.open_pr or open_pr, about = options.open_about or open_about, issues = options.edit_issues })[action.kind]
      if not handler then notifications.error("Status context action is unavailable: " .. tostring(action.kind)) return end
      local edit_input
      if action.kind == "issues" then
        edit_input, capture_error = options.capture_input(target_id, captured)
        if not edit_input then notifications.error(capture_error or "Issues edit input is unavailable") return end
      end
      handler(action, owner, edit_input or captured)
    end)
  end

  function owner.save_issues(captured, text, callback)
    if not alive() then return end
    context_request({ operation = "issues", input = captured, text = text }, function(outcome, failure)
      if failure then
        notifications.error("Issues save failed: " .. failure)
        if callback then callback(false, failure) end
        return
      end
      local targets = type(outcome) == "table" and type(outcome.target) == "table" and outcome.target or {}
      local known = #targets > 0
      for _, target in ipairs(targets) do
        if type(target) ~= "table" or not ({ completed = true, rejected = true, not_started = true })[target.completion] then known = false end
      end
      if type(outcome) ~= "table" or not outcome.success then notifications.error("Issues save failed, repository refresh is required") end
      if known and outcome.operation_id then
        client.request_host("repository.write", { operation = "acknowledge", operation_id = outcome.operation_id }, function(_, acknowledge_error)
          if acknowledge_error then notifications.error("Issues receipt acknowledgement failed: " .. acknowledge_error) end
        end)
      end
      if alive() and options.refresh_status then options.refresh_status() end
      if callback then callback(known and outcome.success == true, outcome) end
    end)
  end

  function owner.close()
    stop_lookup_timers()
    owner.closed = true
    owner.revision = owner.revision + 1
  end
  return owner
end

return M
