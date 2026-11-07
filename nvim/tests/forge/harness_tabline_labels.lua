vim.loader.enable(false)
-- Each tab's Harness label names the session its transcript window shows now, whichever view
-- is active and however the session got there.
local session = require("forge.session")
local controller = require("forge.views.harness.controller")
local original_state = session.harness

---@param name string
---@return integer buffer
local function scratch_buffer(name)
  local buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(buffer, name)
  return buffer
end

---@param session_id string
---@param session_name string
---@return ForgeHarnessPresentationState
local function harness_view(session_id, session_name)
  local state = session.new_harness_state()
  state.command_set = { order = {}, action_by_id = {} }
  state.session = { id = session_id, name = session_name }
  state.transcript_buf = scratch_buffer("forge://harness/" .. session_id)
  state.transcript_win = vim.api.nvim_get_current_win()
  state.timeline_tab = vim.api.nvim_get_current_tabpage()
  vim.api.nvim_win_set_buf(state.transcript_win, state.transcript_buf)
  session.register_harness(session_id, state)
  session.activate_harness(state)
  controller.refresh_winbar()
  return state
end

---@param tab integer
---@return string
local function tab_label(tab)
  local rendered = vim.api.nvim_eval_statusline(vim.o.tabline, { use_tabline = true, maxwidth = 300 }).str
  local labels = {}
  for label in rendered:gmatch(" (.-) x ") do labels[#labels + 1] = vim.trim(label) end
  return labels[vim.api.nvim_tabpage_get_number(tab)]
end

local succeeded, failure = xpcall(function()
  local alpha = harness_view("alpha", "Alpha")
  local alpha_tab = alpha.timeline_tab
  vim.cmd("tabnew")
  local fork = harness_view("bravo-fork", "Bravo Fork")
  local fork_tab = fork.timeline_tab
  assert(tab_label(alpha_tab) == "Alpha" and tab_label(fork_tab) == "Bravo Fork", vim.o.tabline)

  -- A rename published for a background view changes its tab without that view being active.
  session.activate_harness(alpha)
  fork.session.name = "Bravo Renamed"
  controller.refresh_winbar()
  assert(tab_label(fork_tab) == "Bravo Renamed", "background tab kept a stale name: " .. tostring(tab_label(fork_tab)))

  -- /sessions reuses the fork's windows for Alpha; the fork view loses its windows and the
  -- tab Alpha left behind shows an ordinary buffer.
  vim.api.nvim_win_set_buf(fork.transcript_win, alpha.transcript_buf)
  alpha.transcript_win, alpha.timeline_tab = fork.transcript_win, fork_tab
  fork.transcript_win, fork.timeline_tab = nil, nil
  vim.api.nvim_set_current_tabpage(alpha_tab)
  vim.api.nvim_win_set_buf(0, scratch_buffer("notes.txt"))
  session.activate_harness(alpha)
  controller.refresh_winbar()
  assert(tab_label(fork_tab) == "Alpha", "switched tab kept the previous session: " .. tostring(tab_label(fork_tab)))
  assert(tab_label(alpha_tab) == "notes.txt", "vacated tab kept a Harness label: " .. tostring(tab_label(alpha_tab)))

  -- Deleting the fork leaves no tab naming it.
  session.harness_by_id["bravo-fork"] = nil
  local rendered = vim.api.nvim_eval_statusline(vim.o.tabline, { use_tabline = true, maxwidth = 300 }).str
  assert(not rendered:find("Bravo", 1, true) and tab_label(fork_tab) == "Alpha", rendered)
end, debug.traceback)

session.activate_harness(original_state)
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("Harness tab labels follow the displayed session passed")
vim.cmd("qa!")
