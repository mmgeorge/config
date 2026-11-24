local M = {}
local popup = require("forge.infra.popup_window")

--- Open the composer directly below the selected card or source line. A buffer-relative
--- anchor follows the parent viewport as Neovim scrolls it to reveal the insertion point.
---@param target ForgeCommentInputTarget
---@param question boolean
---@param save fun()
function M.open(target, question, save)
  local parent = target.window
  local row, indent, width = target.placement()
  if not row or not indent or not width then return end
  if vim.api.nvim_get_mode().mode:match("^[vVsS\22\19]") then vim.cmd("normal! " .. vim.keycode("<Esc>")) end
  local heading = question and "Plan question" or target.heading
  local function reveal(footer)
    local parent_height = vim.api.nvim_win_get_height(parent)
    local height = math.max(1, math.min(4, parent_height - 4))
    vim.api.nvim_win_set_cursor(parent, { footer + 1, 0 })
    vim.api.nvim_win_call(parent, function()
      vim.cmd("normal! zz")
      if parent_height - vim.fn.winline() < height + 2 then vim.cmd("normal! zt") end
    end)
    return height
  end
  local height = reveal(row)
  local buf, win = popup.open({ parent_win = parent, bufpos = { row, 0 }, row = 1, col = indent,
    width = width, height = height, title = heading, filetype = "ForgePlanCommentInput", wrap = true, linebreak = true })
  local function reposition()
    if not vim.api.nvim_win_is_valid(win) then return end
    local footer, left, interior = target.placement()
    if footer and left and interior then
      vim.api.nvim_win_set_config(win, { relative = "win", win = parent, bufpos = { footer, 0 },
        col = left, row = 1, width = interior, height = reveal(footer) })
    end
  end
  target.state.reposition_input = reposition
  vim.api.nvim_create_autocmd("WinClosed", { pattern = tostring(win), once = true, callback = function()
    if target.state.reposition_input == reposition then target.state.reposition_input = nil end
  end })
  local function finish(submit)
    local body = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
    if vim.trim(body) == "" then popup.close(win) return end
    target.state.reposition_input = nil
    if not target.append(body, question and "question" or nil, heading) then
      target.state.reposition_input = reposition
      return
    end
    popup.close(win)
    if submit then save() end
  end
  local function cancel() popup.close(win) end
  vim.bo[buf].buftype = "acwrite"
  vim.api.nvim_buf_set_name(buf, "forge://plan-comment/" .. buf)
  vim.api.nvim_create_autocmd("BufWriteCmd", { buffer = buf, callback = function() finish(true) end })
  for _, key in ipairs(require("forge.shared.keymaps").view_keys_for("plan_review", "save")) do
    vim.keymap.set({ "n", "i" }, key, function() finish(true) end, { buffer = buf, desc = "Save plan feedback" })
  end
  vim.keymap.set("n", "<CR>", function() finish(false) end, { buffer = buf, desc = "Keep draft in plan review" })
  vim.keymap.set("n", "<Esc>", cancel, { buffer = buf, desc = "Cancel draft" })
  vim.keymap.set({ "n", "i" }, "<C-c>", cancel, { buffer = buf, desc = "Cancel draft" })
  vim.cmd("startinsert")
end

return M
