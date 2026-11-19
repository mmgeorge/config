local M = {}

---Return whether blink.cmp's ghost text would show the selected item's real insertion.
---
---blink.cmp draws ghost text as the item's new text minus as many characters as the user typed
---inside its edit range, which is correct only when the new text begins with the typed text. A
---fuzzy match such as `@src/money.ts` for the typed `@money` would render as `@moneyoney.ts`, so
---ghost text is hidden for such items and kept for every item that extends what was typed.
---@return boolean
function M.enabled()
  local loaded, blink = pcall(require, "blink.cmp")
  if not loaded then return true end
  local item = blink.get_selected_item()
  if not item then return true end
  -- The ghost text derives its range through this library; reading the same edit keeps the two
  -- in step, including blink's adjustment for cursor movement since the items were requested.
  local text_edit = require("blink.cmp.lib.text_edits").get_from_item(item)
  if not (text_edit and text_edit.range and type(text_edit.newText) == "string") then return true end
  local range = text_edit.range
  local typed_end = range.start.line ~= range["end"].line and vim.api.nvim_win_get_cursor(0)[2]
    or range["end"].character
  local typed = vim.api.nvim_get_current_line():sub(range.start.character + 1, typed_end)
  return vim.startswith(text_edit.newText, typed)
end

return M
