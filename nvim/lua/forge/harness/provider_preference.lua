local M = {}

---@param provider string
---@param callback fun(ok: boolean, failure?: string)
function M.save(provider, callback)
  require("forge.client").request({ method = "settings.provider.set", params = { provider = provider } },
    function(_, failure) callback(failure == nil, failure) end)
end

return M
