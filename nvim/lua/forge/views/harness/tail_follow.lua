local M = {}
local navigation_grace_ms = 500

---@class ForgeHarnessTailSample
---@field topline integer
---@field topfill integer
---@field skipcol integer
---@field leftcol integer
---@field line integer
---@field column integer
---@field height integer
---@field width integer
---@field rows integer
---@field bottom boolean

---@class ForgeHarnessTail
---@field observe fun(window: integer)
---@field capture fun(window: integer)
---@field reset fun(window: integer)
---@field follow fun(window: integer)
---@field publish fun(window: integer)
---@field remove fun(window: integer)

---@class ForgeHarnessTailWindow
---@field sample? ForgeHarnessTailSample Last viewport observed after native publication or navigation.
---@field resume_at number Earliest monotonic time for automatic following.
---@field eligible boolean Bottom visibility captured immediately before publication.

---Own per-window tail following independently of focus and configured navigation keys.
---@param buffer integer
---@param clock? fun(): number Monotonic milliseconds, injectable for deterministic tests.
---@return ForgeHarnessTail
function M.new(buffer, clock)
  local now = clock or function() return vim.uv.hrtime() / 1000000 end
  ---@type table<integer, ForgeHarnessTailWindow>
  local window_state = {}
  ---@type ForgeHarnessTail
  local controller = {}

  ---@param window integer
  ---@return ForgeHarnessTailSample?
  local function sample(window)
    if not vim.api.nvim_win_is_valid(window) or vim.api.nvim_win_get_buf(window) ~= buffer then return nil end
    return vim.api.nvim_win_call(window, function()
      local view = vim.fn.winsaveview()
      local height = math.max(1, vim.api.nvim_win_get_height(window) - (vim.wo[window].winbar ~= "" and 1 or 0))
      local remaining = vim.api.nvim_win_text_height(window, {
        start_row = view.topline - 1, start_vcol = view.skipcol or 0, max_height = height + 1,
      }).all + (view.topfill or 0)
      return { topline = view.topline, topfill = view.topfill, skipcol = view.skipcol, leftcol = view.leftcol,
        line = view.lnum, column = view.col, height = height, width = vim.api.nvim_win_get_width(window),
        rows = vim.api.nvim_buf_line_count(buffer), bottom = remaining <= height }
    end)
  end

  ---Observe native viewport movement without intercepting or identifying its keybinding.
  ---@param window integer
  function controller.observe(window)
    local current = sample(window)
    if not current then window_state[window] = nil return end
    local state = window_state[window] or { resume_at = 0, eligible = false }
    local previous = state.sample
    if previous and previous.rows == current.rows and previous.height == current.height
      and previous.width == current.width
      and (previous.topline ~= current.topline or previous.topfill ~= current.topfill
        or previous.skipcol ~= current.skipcol or previous.leftcol ~= current.leftcol
        or previous.line ~= current.line or previous.column ~= current.column) then
      state.resume_at, state.eligible = now() + navigation_grace_ms, false
    end
    state.sample, window_state[window] = current, state
  end

  ---Capture bottom visibility at the synchronous pre-publication boundary.
  ---@param window integer
  function controller.capture(window)
    controller.observe(window)
    local state = window_state[window]
    if state then state.eligible = state.sample.bottom and now() >= state.resume_at end
  end

  ---Record programmatic viewport changes without treating them as user navigation.
  ---@param window integer
  function controller.reset(window)
    local state = window_state[window] or { resume_at = 0, eligible = false }
    state.sample, state.eligible = sample(window), false
    window_state[window] = state.sample and state or nil
  end

  ---Resume following for an explicit submission or agent action without changing focus.
  ---@param window integer
  function controller.follow(window)
    if not sample(window) then return end
    vim.api.nvim_win_set_cursor(window, { vim.api.nvim_buf_line_count(buffer), 0 })
    vim.api.nvim_win_call(window, function() vim.cmd("normal! $zb") end)
    window_state[window] = { sample = sample(window), resume_at = 0, eligible = false }
  end

  ---Follow only windows that were at the bottom before this atomic publication.
  ---@param window integer
  function controller.publish(window)
    local state = window_state[window]
    if state and state.eligible and now() >= state.resume_at then controller.follow(window)
    else controller.reset(window) end
  end

  ---Release viewport state when its native window leaves the presentation.
  ---@param window integer
  function controller.remove(window)
    window_state[window] = nil
  end

  return controller
end

return M
