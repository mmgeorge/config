-- Replaces a field for the rest of the current case and restores the original when the case ends,
-- whether or not it failed. Call it inside a case body: it registers the restore with
-- `MiniTest.finally`, which a hook runs as soon as the hook returns.
--
--   local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
--   local original_stdpath = stub(vim.fn, "stdpath", function(kind)
--     return kind == "data" and data or original_stdpath(kind)
--   end)
---@generic T
---@param target table
---@param key any
---@param value any
---@return any original the replaced value
return function(target, key, value)
  local original = rawget(target, key)
  if original == nil then original = target[key] end
  target[key] = value
  MiniTest.finally(function() target[key] = original end)
  return original
end
