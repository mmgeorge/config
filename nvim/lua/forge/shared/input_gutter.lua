local M = {}

local presentation = {
  number = false,
  relativenumber = false,
  signcolumn = "no",
  foldcolumn = "2",
  statuscolumn = "%#ForgeHarnessPrompt#%{v:lnum == 1 ? '❯ ' : '  '}%*",
}
local input_state = {}
local group

local function release(state, window)
  local previous = state.window[window]
  if previous and vim.api.nvim_win_is_valid(window) then
    for option, value in pairs(previous) do
      if vim.wo[window][option] == presentation[option] then vim.wo[window][option] = value end
    end
  end
  state.window[window] = nil
end

local function attach(state, window)
  if state.window[window] then return end
  local previous = {}
  for option, value in pairs(presentation) do
    local current = vim.wo[window][option]
    previous[option] = current
    if current == value and state.baseline then previous[option] = state.baseline[option] end
    vim.wo[window][option] = value
  end
  state.baseline = state.baseline or previous
  state.window[window] = previous
end

---@param window integer
function M.apply(window)
  if window == 0 then window = vim.api.nvim_get_current_win() end
  local buffer = vim.api.nvim_win_get_buf(window)
  local state = input_state[buffer]
  if not state then
    state = { window = {} }
    input_state[buffer] = state
  end
  if not group then
    group = vim.api.nvim_create_augroup("ForgeInputGutter", { clear = true })
    vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter", "WinEnter" }, {
      group = group, callback = function()
        for input_buffer, owner in pairs(input_state) do
          for displayed in pairs(owner.window) do
            if not vim.api.nvim_win_is_valid(displayed)
              or vim.api.nvim_win_get_buf(displayed) ~= input_buffer then release(owner, displayed) end
          end
        end
        for input_buffer, owner in pairs(input_state) do
          for _, displayed in ipairs(vim.fn.win_findbuf(input_buffer)) do attach(owner, displayed) end
        end
      end,
    })
    vim.api.nvim_create_autocmd("BufWinLeave", {
      group = group, callback = function(event)
        local owner = input_state[event.buf]
        if owner then release(owner, vim.api.nvim_get_current_win()) end
      end,
    })
    vim.api.nvim_create_autocmd("WinClosed", {
      group = group, callback = function(event)
        for _, owner in pairs(input_state) do owner.window[tonumber(event.match)] = nil end
      end,
    })
    vim.api.nvim_create_autocmd("BufWipeout", {
      group = group, callback = function(event)
        local owner = input_state[event.buf]
        if not owner then return end
        for displayed in pairs(owner.window) do release(owner, displayed) end
        input_state[event.buf] = nil
      end,
    })
  end
  attach(state, window)
end

return M
