local M = {}
local key = "status:context:issues"

---@class ForgeStatusIssuesEditor
---@field text? string Unsaved Issues row retained across generated updates.
---@field dirty boolean
---@field sequence integer
---@field saving? boolean
---@field repairing? boolean
---@field sync fun()
---@field focus fun(window: integer)
---@field save fun()

---@param replica ForgeStatusReplica
---@param save fun(text: string, callback: fun(success: boolean))
---@return ForgeStatusIssuesEditor editor
function M.attach(replica, save)
  local buffer = replica.buffer
  ---@type ForgeStatusIssuesEditor
  local editor = { dirty = false, sequence = 0 }
  replica.issues_editor = editor
  local function row()
    if not replica.block[key] then return nil end
    return select(2, replica.root:position(key))
  end
  local function on_row()
    return vim.api.nvim_get_current_buf() == buffer and vim.api.nvim_win_get_cursor(0)[1] - 1 == row()
  end
  function editor.sync()
    if not vim.api.nvim_buf_is_valid(buffer) then return end
    vim.bo[buffer].modifiable = replica.status == "Applied" and not editor.repairing and on_row()
    vim.bo[buffer].modified = editor.dirty
  end
  function editor.focus(window)
    local target = row()
    if target == nil or not window or not vim.api.nvim_win_is_valid(window) then return end
    vim.api.nvim_win_set_cursor(window, { target + 1, 8 })
    editor.sync()
    vim.cmd("startinsert!")
  end
  function editor.save()
    if not editor.dirty or editor.saving then return end
    editor.saving = true
    local sequence = editor.sequence
    save(editor.text or "", function(success)
      editor.saving = false
      if success and sequence == editor.sequence then editor.text, editor.dirty = nil, false end
      editor.sync()
    end)
  end
  local function reject_edit()
    if editor.repairing then return end
    editor.repairing = true
    vim.schedule(function()
      if not vim.api.nvim_buf_is_valid(buffer) or replica.status == "Closed" then return end
      local lines = {}
      for index = 0, replica.sequence:rows() - 1 do
        local node = replica.sequence:locate(index)
        local _, start = replica.sequence:position(node.id)
        lines[#lines + 1] = node.entry.text[index - start + 1] or ""
      end
      replica.applying = true
      vim.bo[buffer].modifiable = true
      vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
      replica.changedtick = vim.api.nvim_buf_get_changedtick(buffer)
      replica.applying, editor.repairing = nil, nil
      editor.sync()
      local notify = replica.notice or vim.notify
      notify("Only the single Issues line is editable")
    end)
  end
  vim.api.nvim_buf_attach(buffer, false, { on_lines = function(_, _, tick, first, last, next_last)
    if replica.applying or editor.repairing then return end
    local target = row()
    local text = first == target and last == target + 1 and next_last == last
      and vim.api.nvim_buf_get_lines(buffer, first, next_last, false)[1]
    if not text or #text > 65536 or text:find("%z") then reject_edit() return end
    local value = vim.tbl_extend("force", replica.block[key], {
      text = { text }, chunk = { { { text, "ForgeStatusPR" } } },
    })
    replica.block[key] = value
    replica.root:update(key, value)
    replica.changedtick = tick
    editor.text, editor.dirty, editor.sequence = text, true, editor.sequence + 1
    vim.schedule(editor.sync)
  end })
  local group = vim.api.nvim_create_augroup("ForgeStatusIssues" .. buffer, { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "CursorMoved", "CursorMovedI", "ModeChanged", "InsertLeave" }, {
    group = group, buffer = buffer, callback = editor.sync,
  })
  vim.api.nvim_create_autocmd("BufWriteCmd", { group = group, buffer = buffer, callback = editor.save })
  vim.api.nvim_create_autocmd("BufWipeout", { group = group, buffer = buffer, once = true,
    callback = function() vim.api.nvim_del_augroup_by_id(group) end })
  vim.keymap.set("i", "<CR>", function() return on_row() and "" or "\r" end, { buffer = buffer, expr = true })
  vim.bo[buffer].buftype = "acwrite"
  editor.sync()
  return editor
end

return M
