--- Tab labels for Harness views.
---
--- A label is never stored: `render` resolves each tab to the Harness view whose transcript
--- window lies in it and shows that view's transcript buffer, or failing that to the view whose
--- transcript buffer a visible window in it shows, then names that view's session.
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
  -- A hidden transcript window prepares a session switch; the tab still shows the session it replaces.
  return window ~= nil and state.transcript_buf ~= nil and vim.api.nvim_win_is_valid(window)
    and not vim.api.nvim_win_get_config(window).hide
    and vim.api.nvim_win_get_tabpage(window) == tab
    and vim.api.nvim_win_get_buf(window) == state.transcript_buf
end

---@param state ForgeHarnessPresentationState
---@param tab integer
---@return boolean
local function shows_transcript_in(state, tab)
  -- A view rebuilt behind its panes for a new host still shows its retired transcript buffer.
  local rebuild = state.pending_rebuild
  for _, transcript_buffer in ipairs({ state.transcript_buf or false, rebuild and rebuild.transcript_buf or false }) do
    if transcript_buffer and vim.api.nvim_buf_is_valid(transcript_buffer)
      and vim.iter(vim.fn.win_findbuf(transcript_buffer)):any(function(window)
        return vim.api.nvim_win_get_tabpage(window) == tab and not vim.api.nvim_win_get_config(window).hide
      end) then return true end
  end
  return false
end

--- Find the Harness view presented in `tab`, or nil when the tab shows no Harness transcript.
--- A view that a session switch is moving into another tab owns only the hidden window it is
--- prepared in, while its former tab keeps showing its transcript until the switch closes that
--- tab; that tab is still named after the view's session rather than its buffer.
---@param tab integer
---@return ForgeHarnessPresentationState?
local function displayed_state(tab)
  if displays_in(session.harness, tab) then return session.harness end
  for _, state in pairs(session.harness_by_id) do
    if displays_in(state, tab) then return state end
  end
  for _, state in pairs(session.harness_by_id) do
    if shows_transcript_in(state, tab) then return state end
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
  -- A focused popup holds a scratch buffer; the tab is named after the window it floats over.
  local window = vim.api.nvim_tabpage_get_win(tab)
  if vim.api.nvim_win_get_config(window).relative ~= "" then
    local tab_number = vim.api.nvim_tabpage_get_number(tab)
    local previous = vim.fn.win_getid(vim.fn.tabpagewinnr(tab_number, "#"), tab_number)
    window = previous ~= 0 and vim.api.nvim_win_get_config(previous).relative == "" and previous
      or vim.iter(vim.api.nvim_tabpage_list_wins(tab)):find(function(candidate)
        return vim.api.nvim_win_get_config(candidate).relative == ""
      end) or window
  end
  local buffer = vim.api.nvim_win_get_buf(window)
  local name = vim.api.nvim_buf_get_name(buffer)
  return name == "" and "[No Name]" or vim.fn.fnamemodify(name, ":t")
end

---@type string? Tabline text at the last redraw this module requested.
local drawn_tabline

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
  -- Views refresh on every host event; the tabline is redrawn only when its text would differ.
  local rendered = displayed and M.render() or nil
  if rendered ~= drawn_tabline then
    drawn_tabline = rendered
    if displayed then vim.cmd.redrawtabline() end
  end
end

--- Redraw labels after any session change (switch, rename, deletion), whichever view made it.
function M.refresh() synchronize(nil) end

--- Restore the user's tabline once no Harness view outside `closing_tab` remains displayed.
---@param closing_tab? integer Tab whose Harness view is closing.
function M.clear(closing_tab) synchronize(closing_tab) end

return M
