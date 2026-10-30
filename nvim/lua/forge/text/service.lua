local M = {}
local client = require("forge.client")
local profiles = {}
local prepared_profiles = {}

local function initialize_profile(owner,config,allowed,callback)
  local prepared=prepared_profiles[owner.root]
  if not prepared or prepared.config~=config or prepared.allowed~=allowed then
    local terminology = vim.list_extend(vim.deepcopy(config.terminology),config.project_terms[owner.root] or {})
    vim.list_extend(terminology,allowed)
    table.sort(terminology)
    prepared={config=config,allowed=allowed,terminology=terminology,
      key=owner.root..":"..vim.fn.sha256(vim.json.encode({terminology=terminology,assets=config.assets}))}
    prepared_profiles[owner.root]=prepared
  end
  local key=prepared.key
  local generation = client.host_get_generation()
  local previous=profiles[key]
  if previous and previous.generation==generation and previous.ready then callback(previous.ready) return end
  if previous and previous.pending then previous.callbacks[#previous.callbacks+1]=callback return end
  local profile={pending=true,callbacks={callback}}
  profiles[key]=profile
  local started=vim.uv.hrtime()
  client.request({method="text.initialize",params={profile=key,terminology=prepared.terminology,assets=config.assets}},function(result,failure)
    if profiles[key]~=profile then return end
    require("forge.infra.perf").event("diff","text.initialize",{elapsed_ms=(vim.uv.hrtime()-started)/1e6,status=failure and "failed" or "ready"})
    profile.pending=false
    profile.ready=not failure and result or nil
    profile.generation=client.host_get_generation()
    local callbacks=profile.callbacks
    profile.callbacks={}
    for _,complete in ipairs(callbacks) do complete(profile.ready,failure) end
  end)
end

---@param owner table
---@param callback fun(profile?: table, failure?: string)
function M.initialize(owner,callback)
  local config = require("forge.infra.config").options.text_correction
  require("forge.text.wordlist").vocabulary(config.wordlist,function(terms,failure)
    if failure then callback(nil,failure) return end
    initialize_profile(owner,config,terms,callback)
  end)
end

---@param owner table
---@param occurrence table
---@param current fun(): boolean
---@param callback fun(result?: table, failure?: string)
function M.correct(owner,occurrence,current,callback)
  M.initialize(owner,function(profile,failure)
    if failure then callback(nil,failure) return end
    if not current() then return end
    occurrence.profile=profile
    client.request({method="text.correct",params={profile=profile.profile,profile_revision=profile.profile_revision,
      occurrence=occurrence.id,context=occurrence.context.text,target=occurrence.context.target,protected=occurrence.context.protected}},function(result,error)
      if not current() then
        require("forge.infra.perf").event("diff","text.correction",{elapsed_ms=(vim.uv.hrtime()-occurrence.started)/1e6,status="stale"})
        return
      end
      callback(result,error)
    end)
  end)
end

return M
