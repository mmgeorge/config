local M = {}

local popup_window = require("diff_review.views.harness.popup.window")

local function status_group(status)
  if status == "failed" or status == "interrupted" then return "DiagnosticError" end
  if status == "completed" or status == "closed" then return "Comment" end
  return "DiffReviewHarnessGoal"
end

local function entry_list(agent)
  local entry = {
    { kind = "main", label = "Main", detail = "Parent conversation" },
  }
  for _, run in ipairs(agent.run or {}) do
    if run.status == "starting" or run.status == "running" or run.status == "waiting" then
      entry[#entry + 1] = {
        kind = "run",
        run = run,
        label = run.nickname or run.definition,
        detail = run.status,
        section = "Active",
      }
    end
  end
  for _, run in ipairs(agent.run or {}) do
    if run.status ~= "starting" and run.status ~= "running" and run.status ~= "waiting" then
      entry[#entry + 1] = {
        kind = "run",
        run = run,
        label = run.nickname or run.definition,
        detail = run.status,
        section = "Done",
      }
    end
  end
  for _, definition in ipairs(agent.definition or {}) do
    entry[#entry + 1] = {
      kind = "definition",
      definition = definition,
      label = definition.name,
      detail = definition.description,
      section = "Available",
    }
  end
  return entry
end

local function close_window(win)
  if win and vim.api.nvim_win_is_valid(win) then vim.api.nvim_win_close(win, true) end
end

local function open_task_input(parent_win, definition, on_spawn)
  local parent = vim.api.nvim_win_get_config(parent_win)
  local width = math.max(36, parent.width)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].filetype = "diffreview-harness-agent-task"
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    row = parent.row + parent.height + 2,
    col = parent.col,
    width = width,
    height = 3,
    style = "minimal",
    border = "rounded",
    title = " Task for " .. definition.name .. " ",
    title_pos = "center",
  })
  vim.wo[win].wrap = true
  vim.wo[win].winbar = ""
  vim.keymap.set({ "n", "i" }, "<C-s>", function()
    local task = vim.trim(table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"))
    if task == "" then return end
    close_window(win)
    close_window(parent_win)
    on_spawn(definition.name, task)
  end, { buffer = buf, nowait = true })
  vim.keymap.set("n", "q", function() close_window(win) end, { buffer = buf, nowait = true })
  vim.cmd("startinsert")
end

---Open the child-agent selector without changing the Harness split layout.
---@param options { agent: table, selected_run_id: string?, on_select: function, on_spawn: function }
function M.open(options)
  local entries = entry_list(options.agent or {})
  local width = math.min(76, math.max(44, vim.o.columns - 12))
  local height = math.min(#entries + 4, math.max(8, vim.o.lines - 8))
  local buf, win = popup_window.open({
    width = width,
    height = height,
    title = "Harness timelines",
    filetype = "diffreview-harness-agent-picker",
    cursorline = true,
  })
  vim.bo[buf].modifiable = false
  local lines = { "  Select a timeline or start another agent", "" }
  local row_by_entry = {}
  local previous_section = nil
  for index, item in ipairs(entries) do
    if item.section and item.section ~= previous_section then
      lines[#lines + 1] = "  " .. item.section
      previous_section = item.section
    end
    row_by_entry[index] = #lines + 1
    local marker = item.run and item.run.id == options.selected_run_id and "●" or " "
    lines[#lines + 1] = (" %s %-18s %s"):format(marker, item.label, item.detail or "")
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  local namespace = vim.api.nvim_create_namespace("DiffReviewHarnessAgentPicker")
  for index, item in ipairs(entries) do
    local row = row_by_entry[index] - 1
    vim.api.nvim_buf_add_highlight(buf, namespace, "DiffReviewStatusLabel", row, 3, 21)
    if item.run then
      vim.api.nvim_buf_add_highlight(buf, namespace, status_group(item.run.status), row, 22, -1)
    end
  end
  local selected = 1
  for index, item in ipairs(entries) do
    if item.run and item.run.id == options.selected_run_id then selected = index end
  end
  local function focus(index)
    selected = math.max(1, math.min(#entries, index))
    vim.api.nvim_win_set_cursor(win, { row_by_entry[selected], 1 })
  end
  local function choose()
    local item = entries[selected]
    if item.kind == "definition" then
      open_task_input(win, item.definition, options.on_spawn)
      return
    end
    close_window(win)
    options.on_select(item.run and item.run.id or nil)
  end
  for _, key in ipairs({ "<Down>", "t" }) do
    vim.keymap.set("n", key, function() focus(selected + 1) end, { buffer = buf, nowait = true })
  end
  for _, key in ipairs({ "<Up>", "s" }) do
    vim.keymap.set("n", key, function() focus(selected - 1) end, { buffer = buf, nowait = true })
  end
  vim.keymap.set("n", "<CR>", choose, { buffer = buf, nowait = true })
  vim.keymap.set("n", "q", function() close_window(win) end, { buffer = buf, nowait = true })
  focus(selected)
end

return M
