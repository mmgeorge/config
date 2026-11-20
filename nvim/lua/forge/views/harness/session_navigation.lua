local M = {}

local client = require("forge.client")
local layout = require("forge.views.harness.layout")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")

local group = vim.api.nvim_create_augroup("ForgeHarnessSessionNavigation", { clear = false })
local pending_sequence = 0

---@type table<string, ForgeHarnessPresentationState>
local state_by_session_id = {}
---@type table<integer, ForgeHarnessPresentationState>
local state_by_buffer = {}

local function controller() return require("forge.views.harness.controller") end

---@param state ForgeHarnessPresentationState
local function valid_view(state)
  -- A hidden transcript window belongs to a session switch still being prepared, not to a tab.
  if state and not state.timeline_tab and state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)
    and not vim.api.nvim_win_get_config(state.transcript_win).hide then
    state.timeline_tab = vim.api.nvim_win_get_tabpage(state.transcript_win)
  end
  return state
    and state.timeline_tab
    and vim.api.nvim_tabpage_is_valid(state.timeline_tab)
    and state.transcript_buf
    and vim.api.nvim_buf_is_valid(state.transcript_buf)
end

---@param state ForgeHarnessPresentationState
local function activate_state(state)
  session.select_harness(state)
end

---@param state ForgeHarnessPresentationState
---@param insert? boolean Return the composer to Insert mode, as the picker that chose the session
---  was invoked from it. Entering a composer otherwise leaves Insert mode.
local function focus_state(state, insert)
  if not valid_view(state) then return false end
  local target = state.composer_win
  local function enter()
    vim.api.nvim_set_current_tabpage(state.timeline_tab)
    if target and vim.api.nvim_win_is_valid(target) then vim.api.nvim_set_current_win(target) end
  end
  if insert and target and vim.api.nvim_win_is_valid(target) then
    -- Entering the composer keeps Insert mode when the user chose the session from it; a mode
    -- that has already ended, as after a picker, starts again.
    layout.enter_composer_in_insert(enter)
    if vim.api.nvim_get_mode().mode:sub(1, 1) ~= "i" then vim.cmd("startinsert!") end
  else
    -- The composer's entry guard leaves Insert mode only when focus moves into it. A composer that
    -- already has focus keeps its mode: the host's reply can arrive seconds after the user has
    -- started typing there, and leaving Insert mode then turns the next keys into Normal-mode
    -- commands. A command that replaces the focused composer's session, as `/clear` does, leaves
    -- Insert mode when it is submitted instead.
    enter()
  end
  activate_state(state)
  return true
end

-- True while a view's buffers move into windows of a tab the user is not in. Placing a buffer in
-- any window runs its `BufEnter` autocommands, even when focus never reaches that window, and the
-- activation autocommand would then select a session the user is not looking at.
local background_placement = false

---Place buffers into another tab's windows without selecting the session they belong to.
---@param place fun()
local function place_in_background(place)
  background_placement = true
  local placed, failure = pcall(place)
  background_placement = false
  if not placed then error(failure, 0) end
end

---@param state ForgeHarnessPresentationState
local function attach_activation(state)
  for _, buf in ipairs({ state.transcript_buf, state.composer_buf }) do
    state_by_buffer[buf] = state
    vim.api.nvim_clear_autocmds({ group = group, buffer = buf })
    vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter" }, {
      group = group,
      buffer = buf,
      callback = function() if not background_placement then activate_state(state) end end,
      desc = "Activate the Harness state owned by this timeline tab",
    })
  end
end

---@param timeline_key string
---@param open_mode? "current"|"tab"
---@return ForgeHarnessPresentationState
local function create_view(timeline_key, open_mode)
  local state = session.new_harness_state()
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    layout.open(timeline_key, open_mode)
  layout.attach_auto_height(state.composer_buf, state.composer_win)
  layout.attach_scroll_boundary(state.transcript_buf, state.transcript_win)
  attach_activation(state)
  activate_state(state)
  return state
end

---@param state ForgeHarnessPresentationState
---@param session_id string
local function register(state, session_id)
  local previous_session_id = state.session and state.session.id
  if previous_session_id and state_by_session_id[previous_session_id] == state then
    state_by_session_id[previous_session_id] = nil
    session.harness_by_id[previous_session_id] = nil
  end
  state_by_session_id[session_id] = state
  session.register_harness(session_id, state)
  layout.rename_session_buffers(state.transcript_buf, state.composer_buf, session_id)
end

---Close a presentation's window-bound resources while retaining its buffers, reporting a failure.
---@param owner HarnessPresentation
---@return boolean
local function close_presentation(owner)
  local ok, closed = pcall(owner.close, { preserve_buffer = true })
  if not ok or not closed then
    notifications.error("Cannot transfer Harness panes: " .. (ok and "presentation cleanup did not complete" or tostring(closed)), "ForgeHarness")
    return false
  end
  return true
end

---Release window-bound presentation resources while retaining the session's buffers.
---@param state ForgeHarnessPresentationState
---@return boolean
local function release_presentation(state)
  local owner = state.presentation
  if not owner then return true end
  if not close_presentation(owner) then return false end
  if state.presentation == owner then state.presentation = nil end
  return true
end

---Stop a view's event subscription and document synchronization, whose session no longer exists.
---Without this, the presentation retries its document reads against the missing session forever.
---@param state ForgeHarnessPresentationState
local function detach_session(state)
  if state.unsubscribe then state.unsubscribe() state.unsubscribe = nil end
  release_presentation(state)
end

---Close a view whose session no longer exists, or never came to exist, together with its tab,
---its buffers and their mappings and autocommands, and forget the view.
---@param state ForgeHarnessPresentationState
local function discard_view(state)
  detach_session(state)
  require("forge.views.harness.workspace").release(state)
  local tab = valid_view(state) and state.timeline_tab
  if tab and #vim.api.nvim_list_tabpages() > 1 then
    vim.cmd("tabclose " .. vim.api.nvim_tabpage_get_number(tab))
  end
  if state.transcript_buf and state.composer_buf then layout.detach(state.transcript_buf, state.composer_buf) end
  for _, buffer in ipairs({ state.transcript_buf, state.composer_buf }) do
    if buffer then
      state_by_buffer[buffer] = nil
      if vim.api.nvim_buf_is_valid(buffer) then vim.api.nvim_buf_delete(buffer, { force = true }) end
    end
  end
  state.session, state.client, state.input = nil, nil, nil
  state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
    nil, nil, nil, nil, nil
  if session.harness == state then session.select_harness(session.new_harness_state()) end
end

---Close every view of a deleted session and forget its identity, whether the view is displayed
---in another tab or retained without one.
---@param session_id string
function M.session_deleted(session_id)
  local deleted_state_set = {}
  if state_by_session_id[session_id] then deleted_state_set[state_by_session_id[session_id]] = true end
  if session.harness_by_id[session_id] then deleted_state_set[session.harness_by_id[session_id]] = true end
  for _, state in pairs(state_by_buffer) do
    if state.session and state.session.id == session_id then deleted_state_set[state] = true end
  end
  state_by_session_id[session_id] = nil
  session.harness_by_id[session_id] = nil
  for state in pairs(deleted_state_set) do discard_view(state) end
  require("forge.views.harness.tabline").refresh()
end

---Forget deleted session identities while retaining unsent composer text.
function M.history_clear()
  for _, state in pairs(state_by_session_id) do
    detach_session(state)
    state.session, state.client, state.input = nil, nil, nil
    state.ready, state.timeline, state.queue = false, {}, {}
    state.prompt_history, state.prompt_history_index = {}, 0
    state.host_error = "History cleared. Open ForgeHarness to start a new session."
    local buffer = state.transcript_buf
    if buffer and vim.api.nvim_buf_is_valid(buffer) then
      vim.bo[buffer].modifiable = true
      vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { state.host_error })
      vim.bo[buffer].modifiable = false
    end
  end
  state_by_session_id = {}
  session.harness_by_id = {}
  require("forge.views.harness.tabline").refresh()
  session.select_harness(session.new_harness_state())
end

---@class ForgeHarnessSessionActivation
---@field state? ForgeHarnessPresentationState
---@field open_mode? "current"|"tab"
---@field interaction_mode? "reconcile"
---@field focus? boolean Preserve the selected session when completing a background request.
---@field source_state? ForgeHarnessPresentationState The view that issued the request, captured when it
---  was issued; defaults to `state`, then to the selected view at completion.
---@field insert? boolean The composer was in Insert mode when the user chose this session, so the
---  composer that ends focused returns to Insert mode.
---@field on_settled? fun() Runs once the chosen session is on screen or the activation has ended
---  without it, so the picker that chose it stays over the source until the target replaces it.

---Abandon a session switch still being prepared: its hidden window closes and the target keeps its
---buffers without a view, as a session that was switched away from does.
---@param switch table
local function abandon_switch(switch)
  if switch.source.pending_switch == switch then switch.source.pending_switch = nil end
  if switch.on_settled then switch.on_settled() switch.on_settled = nil end
  local target = switch.target
  if target.transcript_win == switch.window then
    release_presentation(target)
    require("forge.views.harness.workspace").release(target)
    target.transcript_win, target.composer_win, target.timeline_tab = nil, nil, nil
  end
  if vim.api.nvim_win_is_valid(switch.window) then vim.api.nvim_win_close(switch.window, true) end
end

---Put a prepared session switch on screen. Everything happens in this one callback, so the next
---redraw shows the target rendered and positioned, and no frame shows the panes in between.
---@param switch table
local function complete_switch(switch)
  local source, target, window = switch.source, switch.target, switch.window
  if source.pending_switch ~= switch then return end
  source.pending_switch = nil
  local transcript_win, composer_win, tab = source.transcript_win, source.composer_win, source.timeline_tab
  if not (valid_view(source) and composer_win and vim.api.nvim_win_is_valid(composer_win)
    and vim.api.nvim_win_is_valid(window) and target.transcript_win == window) then
    abandon_switch(switch)
    return
  end
  -- The switch usually completes inside a host reply, which `forge.client` delivers with the
  -- replying session's view active; only the recorded selection says where the user is.
  local selected = session.selected_harness() == source
  if not release_presentation(source) then abandon_switch(switch) return end
  local workspace = require("forge.views.harness.workspace")
  workspace.release(source)
  workspace.release(target)
  local function place()
    vim.api.nvim_win_set_buf(composer_win, target.composer_buf)
    layout.place_transcript(window, transcript_win)
  end
  if not selected then
    place_in_background(place)
  elseif switch.insert then
    layout.enter_composer_in_insert(place)
  else
    place()
  end
  -- The picker that chose the session closes in this same callback, before focus settles.
  if switch.on_settled then switch.on_settled() switch.on_settled = nil end
  source.transcript_win, source.composer_win, source.timeline_tab = nil, nil, nil
  target.transcript_win, target.composer_win, target.timeline_tab = window, composer_win, tab
  layout.attach_auto_height(target.composer_buf, composer_win)
  -- A composer buffer created for the hidden build has never had a window, so the input gutter
  -- that draws its prompt marker does not know it yet; the buffer swap released the source's.
  require("forge.shared.input_gutter").apply(composer_win)
  if target.presentation then target.presentation.bind_composer(composer_win) end
  local active = session.harness
  session.activate_harness(target)
  workspace.attach(target)
  controller().refresh_winbar()
  if selected then
    focus_state(target, switch.insert)
    controller().present_held_prompts()
  else
    session.activate_harness(active)
  end
  if switch.previous_tab and switch.previous_tab ~= tab and vim.api.nvim_tabpage_is_valid(switch.previous_tab) then
    vim.cmd("tabclose " .. vim.api.nvim_tabpage_get_number(switch.previous_tab))
  end
  require("forge.views.harness.tabline").refresh()
end

---Switch the panes of `source` to another session, as `/sessions` does in the current tab.
---Swapping buffers into the visible panes drew every intermediate state: the session being left
---without its decorations, then the target's raw Markdown from its first row, then from its last,
---before its rendered view. The target is instead opened in a hidden window over the transcript
---while the panes keep showing the source, and replaces them once its transcript has rendered.
---@param result table
---@param source ForgeHarnessPresentationState
---@param existing? ForgeHarnessPresentationState The target's retained or displayed view, if any.
---@param options ForgeHarnessSessionActivation
local function switch_in_place(result, source, existing, options)
  local session_id = result.session.id
  if source.pending_switch then abandon_switch(source.pending_switch) end
  local target = existing or session.new_harness_state()
  local switch = { source = source, target = target, insert = options.insert, on_settled = options.on_settled }
  if valid_view(target) then
    -- The target is displayed in another tab, which closes once its panes move into this one.
    if not release_presentation(target) then
      if switch.on_settled then switch.on_settled() end
      return
    end
    require("forge.views.harness.workspace").release(target)
    switch.previous_tab = target.timeline_tab
  end
  if not (target.transcript_buf and vim.api.nvim_buf_is_valid(target.transcript_buf)
    and target.composer_buf and vim.api.nvim_buf_is_valid(target.composer_buf)) then
    target.transcript_buf = layout.create_transcript_buffer(session_id)
    target.composer_buf = layout.create_composer_buffer(session_id)
  end
  -- The source's status split hides the separator above it; the target restores its own when its
  -- status split opens, so it starts from the separator the transcript had before.
  local separator = source.presentation and source.presentation.status_view
    and source.presentation.status_view.separator
  switch.window = layout.open_hidden_transcript(target.transcript_buf, source.transcript_win,
    separator and { fillchars = separator.fillchars, winhighlight = separator.winhighlight } or nil)
  -- The target has no tab until it is shown, so the tab keeps the source's label meanwhile.
  target.transcript_win, target.composer_win, target.timeline_tab = switch.window, nil, nil
  layout.attach_scroll_boundary(target.transcript_buf, switch.window)
  source.pending_switch = switch
  attach_activation(target)
  register(target, session_id)
  target.ready = source.ready
  -- The controller acts on the active view; the source stays selected while the target prepares.
  local selected = session.harness
  session.activate_harness(target)
  controller().activate_snapshot(result, { present_questions = false })
  controller().attach()
  controller().attach_transcript(target.transcript_buf)
  controller().refresh_winbar()
  session.activate_harness(selected)
  if not target.presentation then complete_switch(switch) return end
  -- A target whose transcript fails to open is still shown, with the failure its presentation
  -- reports, because the user chose it; a superseded or deleted target is abandoned instead.
  target.presentation.when_rendered(function() complete_switch(switch) end)
end

---@class ForgeHarnessPresentationRebuild
---@field presentation? HarnessPresentation The presentation the rebuild waits for.
---@field retired HarnessPresentation The previous host's presentation, still on screen.
---@field transcript_buf integer The retired presentation's transcript buffer.
---@field transcript_win integer The visible transcript window, which the rebuilt one replaces.
---@field window integer The hidden window the rebuilt transcript renders in.

---Whether `state` still draws a presentation that an earlier host generation opened for
---`session_id`. A view the host stopped serving keeps such a presentation on screen, with its
---decorations, until its session comes back on a new host.
---@param state ForgeHarnessPresentationState
---@param session_id string
---@return boolean
local function stale_presentation(state, session_id)
  local owner = state.presentation
  return owner ~= nil and not owner.closed and owner.ready == true and owner.session_id == session_id
    and owner.host_generation ~= client.host_get_generation()
end

---Give a view whose presentation belongs to an earlier host a fresh transcript buffer in a
---hidden window over its transcript, where the new host's presentation renders. Closing the old
---presentation in place cleared every decoration and gutter marker a host round trip before the
---new one drew, and replacing it on the same buffer would let two replicas share one buffer, so
---the old one stays on screen untouched until `complete_rebuild` swaps the panes.
---@param state ForgeHarnessPresentationState
---@return ForgeHarnessPresentationRebuild
local function begin_rebuild(state)
  if state.pending_switch then abandon_switch(state.pending_switch) end
  local retired = assert(state.presentation)
  local rebuild = { retired = retired, transcript_buf = state.transcript_buf, transcript_win = state.transcript_win }
  -- The rebuilt buffer takes the session's buffer name when the activation registers it, so the
  -- retired one gives that name up first. The rebuilt buffer starts under a key no other buffer
  -- name contains, because a buffer lookup by name also matches a name that merely contains it.
  layout.rename_buffer(rebuild.transcript_buf, vim.api.nvim_buf_get_name(rebuild.transcript_buf) .. "?retired")
  local transcript_buf = layout.create_transcript_buffer("rebuild-pending-" .. tostring(vim.uv.hrtime()))
  local separator = retired.status_view and retired.status_view.separator
  rebuild.window = layout.open_hidden_transcript(transcript_buf, rebuild.transcript_win,
    separator and { fillchars = separator.fillchars, winhighlight = separator.winhighlight } or nil)
  layout.attach_scroll_boundary(transcript_buf, rebuild.window)
  -- The retired presentation no longer belongs to the view: renders build the rebuilt one, which
  -- binds the composer only once the retired status split has closed.
  state.presentation, state.pending_rebuild = nil, rebuild
  state.transcript_buf, state.transcript_win = transcript_buf, rebuild.window
  return rebuild
end

---Abandon a rebuild whose view closed meanwhile: the session keeps the rebuilt buffers without a view.
---@param state ForgeHarnessPresentationState
---@param rebuild ForgeHarnessPresentationRebuild
local function abandon_rebuild(state, rebuild)
  if state.pending_rebuild == rebuild then state.pending_rebuild = nil end
  close_presentation(rebuild.retired)
  if state.transcript_win == rebuild.window then
    release_presentation(state)
    state.transcript_win, state.composer_win, state.timeline_tab = nil, nil, nil
  end
  require("forge.views.harness.workspace").release(state)
  if vim.api.nvim_win_is_valid(rebuild.window) then vim.api.nvim_win_close(rebuild.window, true) end
  state_by_buffer[rebuild.transcript_buf] = nil
  layout.detach_transcript(rebuild.transcript_buf)
  if vim.api.nvim_buf_is_valid(rebuild.transcript_buf) then vim.api.nvim_buf_delete(rebuild.transcript_buf, { force = true }) end
end

local finish_rebuild

---Put a rebuilt transcript on screen in place of the retired one. Everything happens in this one
---callback, so the next redraw shows the rebuilt view and no frame shows the panes in between.
---@param state ForgeHarnessPresentationState
---@param rebuild ForgeHarnessPresentationRebuild
local function complete_rebuild(state, rebuild)
  if state.pending_rebuild ~= rebuild then return end
  local current = state.presentation
  if current and current ~= rebuild.presentation and not current.closed then
    -- A later render replaced the presentation the rebuild waited for; the replacement is awaited.
    rebuild.presentation = current
    current.when_rendered(function(rendered) finish_rebuild(state, rebuild, rendered) end)
    return
  end
  local composer_win = state.composer_win
  if not (composer_win and vim.api.nvim_win_is_valid(composer_win) and vim.api.nvim_win_is_valid(rebuild.window)
    and vim.api.nvim_win_is_valid(rebuild.transcript_win) and state.transcript_win == rebuild.window) then
    abandon_rebuild(state, rebuild)
    return
  end
  state.pending_rebuild = nil
  local workspace = require("forge.views.harness.workspace")
  workspace.release(state)
  -- The retired status split closes before the rebuilt one binds the composer it sits above.
  close_presentation(rebuild.retired)
  local focused = vim.api.nvim_get_current_win() == rebuild.transcript_win
  local function place()
    layout.place_transcript(rebuild.window, rebuild.transcript_win)
    if focused then vim.api.nvim_set_current_win(rebuild.window) end
  end
  if session.selected_harness() == state then place() else place_in_background(place) end
  state_by_buffer[rebuild.transcript_buf] = nil
  layout.detach_transcript(rebuild.transcript_buf)
  if vim.api.nvim_buf_is_valid(rebuild.transcript_buf) then vim.api.nvim_buf_delete(rebuild.transcript_buf, { force = true }) end
  if state.presentation then state.presentation.bind_composer(composer_win) end
  workspace.attach(state)
  controller().with_state(state, controller().refresh_winbar)
  require("forge.views.harness.tabline").refresh()
end

---Complete `rebuild` once its presentation has rendered, or once it can no longer render. A
---presentation that closed may be replaced by the render that closed it, so that case waits for
---the render to return before looking.
---@param state ForgeHarnessPresentationState
---@param rebuild ForgeHarnessPresentationRebuild
---@param rendered boolean
finish_rebuild = function(state, rebuild, rendered)
  if rendered then complete_rebuild(state, rebuild) return end
  vim.schedule(function() complete_rebuild(state, rebuild) end)
end

---@param result table
---@param options? ForgeHarnessSessionActivation
function M.activate(result, options)
  options = options or {}
  local next_session = result and result.session
  if not (next_session and next_session.id) then
    notifications.error("Harness session activation returned no session", "ForgeHarness")
    return
  end
  local existing = state_by_session_id[next_session.id]
  local requesting_state = options.source_state or session.harness
  if options.open_mode == "current" and not options.state and existing ~= requesting_state
    and valid_view(requesting_state) and requesting_state.composer_win
    and vim.api.nvim_win_is_valid(requesting_state.composer_win)
    and not (requesting_state.session and requesting_state.session.id == next_session.id) then
    switch_in_place(result, requesting_state, existing, options)
    return
  end
  if options.on_settled then options.on_settled() end
  -- A reply can arrive after the user selected another tab. The view that issued the request is
  -- the source whose panes and tab the activation may reuse or close; the selected view is only
  -- restored when this activation does not take focus.
  local selected_state = session.harness
  local source_state = options.source_state or options.state or selected_state
  -- Opening a session in the current tab targets the current view's windows. A retained session
  -- would otherwise reopen through `layout.open` in a new last tab while the source tab closes. A
  -- session already displayed in another tab moves into the current tab, its former tab closes,
  -- and the session the current tab showed stays loaded without a view.
  local state = options.state
    or (options.open_mode == "current" and not (existing and valid_view(existing))
      and valid_view(source_state) and source_state or nil)
  local reusable_buffer = existing and existing.transcript_buf and existing.composer_buf
    and vim.api.nvim_buf_is_valid(existing.transcript_buf) and vim.api.nvim_buf_is_valid(existing.composer_buf)
  if existing and existing ~= state and (valid_view(existing)
    or (reusable_buffer and state and options.open_mode == "current" and valid_view(state))) then
    if state and options.open_mode == "current" and valid_view(state) then
      if not release_presentation(existing) or not release_presentation(state) then return end
      local previous_tab = existing.timeline_tab
      require("forge.views.harness.workspace").release(existing)
      require("forge.views.harness.workspace").release(state)
      local function swap_buffers()
        vim.api.nvim_win_set_buf(state.transcript_win, existing.transcript_buf)
        vim.api.nvim_win_set_buf(state.composer_win, existing.composer_buf)
      end
      if options.focus == false then
        place_in_background(swap_buffers)
      elseif options.insert then
        layout.enter_composer_in_insert(swap_buffers)
      else
        swap_buffers()
      end
      existing.transcript_win, existing.composer_win, existing.timeline_tab =
        state.transcript_win, state.composer_win, state.timeline_tab
      state.transcript_win, state.composer_win, state.timeline_tab = nil, nil, nil
      layout.attach_auto_height(existing.composer_buf, existing.composer_win)
      layout.attach_scroll_boundary(existing.transcript_buf, existing.transcript_win)
      if options.focus ~= false then vim.api.nvim_set_current_tabpage(existing.timeline_tab) end
      if previous_tab and previous_tab ~= existing.timeline_tab and vim.api.nvim_tabpage_is_valid(previous_tab) then
        vim.cmd("tabclose " .. vim.api.nvim_tabpage_get_number(previous_tab))
      end
    end
    state = existing
  end
  if not state and existing and reusable_buffer then
    if not release_presentation(existing) then return end
    existing.transcript_buf, existing.transcript_win, existing.composer_buf, existing.composer_win, existing.timeline_tab =
      layout.open(next_session.id)
    layout.attach_auto_height(existing.composer_buf, existing.composer_win)
    layout.attach_scroll_boundary(existing.transcript_buf, existing.transcript_win)
    state = existing
  end
  if not state then
    state = options.open_mode == "current" and source_state or create_view(next_session.id)
  end
  -- A view returning to its own session on a new host renders the new host's presentation behind
  -- the one it still shows.
  local rebuild = valid_view(state) and state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win)
    and not vim.api.nvim_win_get_config(state.transcript_win).hide
    and state.composer_win and vim.api.nvim_win_is_valid(state.composer_win)
    and stale_presentation(state, next_session.id) and begin_rebuild(state) or nil
  attach_activation(state)
  register(state, next_session.id)
  state.ready = source_state.ready
  if options.focus == false then session.activate_harness(state) else activate_state(state) end
  controller().activate_snapshot(result, { present_questions = options.focus ~= false })
  controller().attach()
  controller().attach_transcript(state.transcript_buf)
  controller().refresh_winbar()
  if options.focus == false then session.activate_harness(selected_state) else focus_state(state, options.insert) end
  if rebuild then
    rebuild.presentation = state.presentation
    if not state.presentation then complete_rebuild(state, rebuild)
    else state.presentation.when_rendered(function(rendered) finish_rebuild(state, rebuild, rendered) end) end
  end

  if options.open_mode == "current"
    and source_state ~= state
    and source_state.timeline_tab
    and vim.api.nvim_tabpage_is_valid(source_state.timeline_tab)
  then
    if not release_presentation(source_state) then return end
    require("forge.views.harness.workspace").release(source_state)
    vim.cmd("tabclose " .. vim.api.nvim_tabpage_get_number(source_state.timeline_tab))
  end
end

---A view opened before the host answers a new-session request, so the tab appears at once. A
---refusal removes it again (`fail_pending`); success binds it to the created session. A fork opens
---no such view: its tab opens only once the host accepts the fork (`open_fork`), because the host
---refuses a fork of a source with running children or of a fork still in its first turn, and a
---tab opened for such a request was drawn before the refusal closed it.
---@class ForgeHarnessPendingSession
---@field state ForgeHarnessPresentationState
---@field source_state ForgeHarnessPresentationState The view that issued the request.
---@field child_name? string

---Open the tab of a fork the host created and return its view. The tab takes focus only while
---the view that issued the fork is still selected; a user who moved elsewhere meanwhile stays
---there, and the fork opens behind them.
---@param result table The host's `session.fork` result.
---@param source_state ForgeHarnessPresentationState The view `/fork` was typed in.
---@return ForgeHarnessPresentationState?
function M.open_fork(result, source_state)
  local child_session = result and result.session
  if not (child_session and child_session.id) then
    notifications.error("Harness fork returned no session", "Harness fork")
    return nil
  end
  local source_session_id = source_state.session and source_state.session.id
  if source_session_id and state_by_session_id[source_session_id] ~= source_state then
    register(source_state, source_session_id)
  end
  local focus = session.selected_harness() == source_state
  local selected_state, previous_window = session.selected_harness(), vim.api.nvim_get_current_win()
  if focus and source_state.transcript_win and vim.api.nvim_win_is_valid(source_state.transcript_win) then
    vim.api.nvim_win_call(source_state.transcript_win, function() vim.cmd("normal! m'") end)
  end
  local state = create_view(child_session.id)
  M.activate(result, { state = state, source_state = source_state, focus = focus })
  if not focus then
    if vim.api.nvim_win_is_valid(previous_window) then
      vim.api.nvim_set_current_tabpage(vim.api.nvim_win_get_tabpage(previous_window))
      vim.api.nvim_set_current_win(previous_window)
    end
    session.select_harness(selected_state)
  end
  require("forge.views.harness.tabline").refresh()
  return state
end

---@param name? string
---@param open_mode? "current"|"tab"
---@return ForgeHarnessPendingSession
function M.begin_new(name, open_mode)
  pending_sequence = pending_sequence + 1
  local source_state = session.harness
  local state = create_view("new-pending-" .. pending_sequence, open_mode)
  -- The tab and its focused composer are drawn before the host creates the session, which can take
  -- seconds. Keys typed there meanwhile belong to that composer, so it takes the session's
  -- mappings now; a submit before the session exists keeps the draft and says why.
  require("forge.shared.keymaps").setup_view_keymaps(state.composer_buf, "harness", controller().composer_command_set())
  local pending = { state = state, source_state = source_state, child_name = name }
  M.render_pending(pending)
  return pending
end

---@param pending ForgeHarnessPendingSession
function M.render_pending(pending)
  local state = pending.state
  local buf = state and state.transcript_buf
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
  local line = pending.child_name and pending.child_name ~= "" and ("Creating " .. pending.child_name .. "...")
    or "Creating a new session..."
  line = line:gsub("[\r\n]+", " ")
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "", "  " .. line })
  vim.bo[buf].modifiable = false
  vim.bo[buf].modified = false
end

---@param pending ForgeHarnessPendingSession
---@param result table
function M.complete_pending(pending, result)
  if not valid_view(pending.state) then return end
  M.activate(result, { state = pending.state, focus = session.selected_harness() == pending.state })
end

---Remove the view of a request the host refused, which created no session, and report the
---refusal in the view that issued it. Focus returns there only from the pending view itself; a
---user who moved to another tab meanwhile stays where they are.
---@param pending ForgeHarnessPendingSession
---@param message string
---@param title string
function M.fail_pending(pending, message, title)
  local state, source_state = pending.state, pending.source_state
  local displayed = valid_view(state) and vim.api.nvim_get_current_tabpage() == state.timeline_tab
  if displayed and source_state and source_state ~= state then focus_state(source_state) end
  discard_view(state)
  require("forge.views.harness.tabline").refresh()
  notifications.error(message, title)
end

---@param session_id string
---@param options? ForgeHarnessSessionActivation
function M.resume(session_id, options)
  local source_state = session.harness
  local existing = state_by_session_id[session_id] or session.harness_by_id[session_id]
  -- An unavailable session's owner failed for good, so choosing it asks the host to reopen the
  -- session from storage instead of focusing the view that can no longer run anything.
  local reopen = existing and require("forge.views.harness.activity").unavailable_reason(existing) ~= nil
  local settled = options and options.on_settled
  if settled then
    local notify_settled, fired = settled, false
    settled = function() if not fired then fired = true notify_settled() end end
    options = vim.tbl_extend("force", {}, options, { on_settled = settled })
  end
  if existing and not reopen and (existing == source_state or not options or options.open_mode ~= "current") then
    if settled then settled() end
    if focus_state(existing, options and options.insert) then return end
  end
  client.request({ session_id = session_id, method = "session.resume", params = { session_id = session_id } }, function(result, request_error)
    if request_error then
      if settled then settled() end
      notifications.error(request_error, "Harness session")
      return
    end
    -- The activation targets the tab where the user chose the session, and takes focus only
    -- when that tab is still the selected one.
    local activation = vim.tbl_extend("force", {}, options or {}, { source_state = source_state })
    -- Reopening the session the tab already shows keeps its view; another session is prepared
    -- out of sight and replaces the tab's panes once rendered.
    if activation.open_mode == "current" and valid_view(source_state) and existing == source_state then
      activation.state = source_state
    end
    if session.selected_harness() ~= source_state then activation.focus = false end
    M.activate(result, activation)
  end)
end

---@param session_id string
function M.open_parent(session_id)
  local state = session.harness
  if state.transcript_win and vim.api.nvim_win_is_valid(state.transcript_win) then
    vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! m'") end)
  end
  local parent_state = state_by_session_id[session_id] or session.harness_by_id[session_id]
  if parent_state then
    if valid_view(parent_state) then
      vim.api.nvim_set_current_tabpage(parent_state.timeline_tab)
      if parent_state.composer_win and vim.api.nvim_win_is_valid(parent_state.composer_win) then
        vim.api.nvim_set_current_win(parent_state.composer_win)
      end
    end
    activate_state(parent_state)
    return
  end
  M.resume(session_id, { open_mode = "tab" })
end

---@return table<string, ForgeHarnessPresentationState>, table<integer, ForgeHarnessPresentationState>
function M._registry_for_test()
  return state_by_session_id, state_by_buffer
end

return M
