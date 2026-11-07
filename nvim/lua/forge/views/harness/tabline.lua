--- Tab labels for Harness views.
---
--- A label is never stored: `render` resolves each tab to the Harness view whose transcript
--- window lies in it and shows that view's transcript buffer, then names that view's session.
--- Session switches that move a view between tabs, renames published for background views,
--- and deletions therefore change the label at the next tabline redraw without bookkeeping.
local session = require("forge.session")

local M = {}

---@type string?
local previous_tabline = nil
local name_width = 30

---@param text string
---@return string
local function escape(text)
  return (text:gsub("%%", "%%%%"))
end

---@param state ForgeHarnessPresentationState
---@param tab integer
---@return boolean
local function displays_in(state, tab)
  local window = state.transcript_win
  return window ~= nil and state.transcript_buf ~= nil and vim.api.nvim_win_is_valid(window)
    and vim.api.nvim_win_get_tabpage(window) == tab
    and vim.api.nvim_win_get_buf(window) == state.transcript_buf
end

--- Find the Harness view presented in `tab`, or nil when the tab shows no Harness transcript.
---@param tab integer
---@return ForgeHarnessPresentationState?
local function displayed_state(tab)
  if displays_in(session.harness, tab) then return session.harness end
  for _, state in pairs(session.harness_by_id) do
    if displays_in(state, tab) then return state end
  end
  return nil
end

---@param state ForgeHarnessPresentationState
---@return string
local function session_label(state)
  local name = not state.initial_snapshot_pending and state.session and state.session.name or nil
  if not name or name == "" then return "[unnamed]" end
  return vim.fn.strcharpart((name:gsub("%s+", " ")), 0, name_width)
end

---@param tab integer
---@return string
local function label_for(tab)
  local state = displayed_state(tab)
  if state then return session_label(state) end
  local buffer = vim.api.nvim_win_get_buf(vim.api.nvim_tabpage_get_win(tab))
  local name = vim.api.nvim_buf_get_name(buffer)
  return name == "" and "[No Name]" or vim.fn.fnamemodify(name, ":t")
end

---@return string
function M.render()
  local parts = {}
  local current = vim.api.nvim_get_current_tabpage()
  for index, tab in ipairs(vim.api.nvim_list_tabpages()) do
    local group = tab == current and "TabLineSel" or "TabLine"
    parts[#parts + 1] = ("%%#%s#%%%dT %s %%T%%%dXx%%X "):format(group, index, escape(label_for(tab)), index)
  end
  parts[#parts + 1] = "%#TabLineFill#%T"
  return table.concat(parts)
end

--- Install the Harness tabline while a Harness view is displayed outside `closing_tab`,
--- restore the user's tabline otherwise, and redraw the installed one.
---@param closing_tab? integer
local function synchronize(closing_tab)
  local displayed = false
  for _, tab in ipairs(vim.api.nvim_list_tabpages()) do
    if tab ~= closing_tab and displayed_state(tab) then displayed = true break end
  end
  if displayed and previous_tabline == nil then
    previous_tabline = vim.o.tabline
    vim.o.tabline = "%!v:lua.require'forge.views.harness.tabline'.render()"
  elseif not displayed and previous_tabline ~= nil then
    vim.o.tabline = previous_tabline
    previous_tabline = nil
  end
  if displayed then vim.cmd.redrawtabline() end
end

--- Redraw labels after any session change (switch, rename, deletion), whichever view made it.
function M.refresh() synchronize(nil) end

--- Restore the user's tabline once no Harness view outside `closing_tab` remains displayed.
---@param closing_tab? integer Tab whose Harness view is closing.
function M.clear(closing_tab) synchronize(closing_tab) end

return M
