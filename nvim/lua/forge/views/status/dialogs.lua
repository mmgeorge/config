local M = {}
local popup_window = require("forge.infra.popup_window")

---@param prefix string
---@param on_submit fun(name: string?)
---@return integer buffer
function M.branch_name(prefix, on_submit)
  local width = math.min(60, math.max(30, math.floor(vim.o.columns * 0.4)))
  local buffer, window = popup_window.open({ relative = "editor", width = width, height = 1,
    title = "New branch", filetype = "ForgeBranchPrompt" })
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { prefix })
  local finished = false
  local function finish(value)
    if finished then return end
    finished = true
    popup_window.close(window)
    on_submit(value)
  end
  vim.keymap.set({ "i", "n" }, "<CR>", function()
    vim.cmd("stopinsert")
    finish(vim.api.nvim_buf_get_lines(buffer, 0, -1, false)[1] or "")
  end, { buffer = buffer })
  vim.keymap.set("i", "<C-c>", function() finish(nil) end, { buffer = buffer })
  for _, key in ipairs({ "<Esc>", "q" }) do
    vim.keymap.set("n", key, function() finish(nil) end, { buffer = buffer, nowait = true })
  end
  vim.api.nvim_create_autocmd("BufLeave", { buffer = buffer, once = true, callback = function() finish(nil) end })
  return buffer
end

---@param replica ForgeStatusReplica
---@param captured ForgeStatusInput
---@param selection? {target: table[]}
---@return string[]?
function M.discard_message(replica, captured, selection)
  local target = selection and selection.target or { captured.location }
  local selected, path = {}, {}
  local function add(record, kind)
    if not record then return end
    selected[#selected + 1] = { record = record, kind = kind }
    path[record.path] = true
  end
  for _, location in ipairs(target) do
    if location.kind == "section" then
      for _, record in ipairs(replica.inventory.file) do
        if record.section == location.section or location.section == "unstaged" and record.section == "untracked" then add(record, "file") end
      end
    else
      local model = replica.file[location.id or location.file]
      add(model and model.record, location.kind)
    end
  end
  if #selected == 0 then return nil end
  if #selected == 1 then
    local item = selected[1]
    local prompt = item.kind == "body" and "Discard this hunk?"
      or item.record.untracked and "Delete untracked file?" or "Discard ALL changes to file?"
    return { prompt, "  " .. item.record.path }
  end
  return { ("Discard changes in %d file(s)?"):format(vim.tbl_count(path)) }
end

return M
