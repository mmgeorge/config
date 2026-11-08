-- Invokes the Lua callback a mapping is bound to, as pressing its key would, without feeding keys.
-- Cases use it to drive buffer-local mappings deterministically; it fails when the key has no
-- callback mapping, so a removed or renamed mapping fails the case instead of doing nothing.
--
--   local keymap = dofile("nvim/lua/forge/tests/support/keymap.lua")
--   keymap.invoke("<CR>", { buffer = state.composer_buf })
--   keymap.invoke("gq", { window = picker_window, mode = "x" })
local keymap = {}

---@class KeymapInvokeOptions
---@field mode? string defaults to "n"
---@field buffer? integer resolve the mapping in this buffer; the callback runs in the current window
---@field window? integer make this window current first

---@param lhs string
---@param options? KeymapInvokeOptions
---@return any result the callback's return value
function keymap.invoke(lhs, options)
  options = options or {}
  local mode = options.mode or "n"
  if options.window then vim.api.nvim_set_current_win(options.window) end
  local mapping = keymap.find(lhs, { mode = mode, buffer = options.buffer }) or {}
  assert(type(mapping.callback) == "function", ("no %s-mode callback mapping for %s"):format(mode, lhs))
  return mapping.callback()
end

---@param lhs string
---@param options? KeymapInvokeOptions
---@return table mapping the `maparg` dictionary, asserting the mapping exists
function keymap.get(lhs, options)
  local mapping = keymap.find(lhs, options)
  assert(mapping, ("no %s-mode mapping for %s"):format(options and options.mode or "n", lhs))
  return mapping
end

---@param lhs string
---@param options? KeymapInvokeOptions
---@return table? mapping the `maparg` dictionary of a buffer-local mapping when `buffer` is set or
---  of the mapping in effect otherwise, or nil when `lhs` has none
function keymap.find(lhs, options)
  options = options or {}
  local mode = options.mode or "n"
  local function read() return vim.fn.maparg(lhs, mode, false, true) end
  local mapping = options.buffer and vim.api.nvim_buf_call(options.buffer, read) or read()
  if next(mapping) == nil then return nil end
  return mapping
end

---Feeds keys as typed, translating key notation such as `<CR>`, and processes them before returning.
---@param keys string
---@param flags? string `nvim_feedkeys` mode flags; defaults to "xt"
function keymap.feed(keys, flags)
  vim.api.nvim_feedkeys(vim.keycode(keys), flags or "xt", false)
end

return keymap
