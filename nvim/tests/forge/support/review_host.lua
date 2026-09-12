---@class ReviewHostFixtureOptions
---@field observe fun(target: table): ForgeReviewText
---@field submit? fun(target: table, edit: table, callback: fun(ok: boolean, failure?: string))

local M = {}

---@param options ReviewHostFixtureOptions
---@return fun(method: string, params: table, callback: ForgeCallback)
function M.runner(options)
  local document = {}
  local next_id = 0
  local function snapshot(state)
    local result = { document = state.id, field = {}, saving = false, uncertain = false }
    for _, region in ipairs({ "title", "body" }) do
      local field = state.field[region]
      result.field[#result.field + 1] = { region = region, revision = field.revision, sequence = field.sequence,
        text = field.text, baseline = field.baseline, dirty = field.text ~= field.baseline, uncertain = false }
    end
    return result
  end
  return function(method, params, callback)
    vim.schedule(function()
      if method == "review.open_pr" then
        next_id = next_id + 1
        local text = options.observe(params.target)
        local state = { id = "fixture-review-" .. next_id, target = params.target, field = {} }
        for _, region in ipairs({ "title", "body" }) do
          state.field[region] = { text = text[region], baseline = text[region], revision = 0, sequence = 0 }
        end
        document[state.id] = state
        callback(snapshot(state))
        return
      end
      local state = document[params.document]
      if not state then callback(nil, "unknown review document") return end
      if method == "review.close" then document[state.id] = nil callback({ closed = true }) return end
      if method == "review.snapshot" then callback(snapshot(state)) return end
      if method == "review.region_edit" then
        local field = assert(state.field[params.region])
        assert(params.base == field.revision and params.sequence > field.sequence, "stale fixture edit")
        field.text, field.revision, field.sequence = params.text, params.base + 1, params.sequence
        callback({ document = state.id, region = params.region, revision = field.revision, sequence = field.sequence })
        return
      end
      assert(method == "review.save", method)
      local edit = {}
      for _, region in ipairs({ "title", "body" }) do
        if state.field[region].text ~= state.field[region].baseline then edit[region] = state.field[region].text end
      end
      if not next(edit) then callback({ snapshot = snapshot(state) }) return end
      assert(options.submit, "unexpected fixture save")
      options.submit(state.target, edit, function(ok, failure)
        if ok then for region, text in pairs(edit) do state.field[region].baseline = text end end
        callback({ snapshot = snapshot(state), remote = { ok = ok, outcome = ok and "confirmed" or "rejected",
          matches_submission = ok, message = failure } })
      end)
    end)
  end
end

return M
