vim.loader.enable(false)
local completion = require("forge.completion")
local original_schedule, original_notify, original_now = vim.schedule, vim.notify, vim.uv.now
---@type (fun())[]
local scheduled = {}
---@type string[]
local notices = {}
local now = 1000
vim.schedule = function(callback) scheduled[#scheduled + 1] = callback end
vim.notify = function(message) notices[#notices + 1] = message end
vim.uv.now = function() return now end

local function drain()
  local count = 0
  while #scheduled > 0 do
    count = count + 1
    assert(count < 100, "completion scheduled an unbounded callback chain")
    table.remove(scheduled, 1)()
  end
end

---@param revision integer
---@param values string[]
---@param generation? string
---@return ForgeCompletionSnapshot
local function snapshot(revision, values, generation)
  return { identity = "repository", generation = generation or "host-one", revision = revision, values = values, truncated = false }
end

local ok, failure = xpcall(function()
  local requests = 0
  ---@type ForgeCompletionDone?
  local respond
  local cache = completion.new("repository", "host-one", function(identity, generation, done)
    assert(identity == "repository" and generation == "host-one")
    requests = requests + 1
    respond = done
  end)
  for _ = 1, 100 do assert(#completion.values(cache, "") == 0) end
  assert(requests == 0 and #scheduled == 1, "cold callback performed I/O or duplicated refresh")
  drain()
  assert(requests == 1 and respond)
  respond(snapshot(1, { "alpha", "beta", "origin/main" }), nil)
  drain()
  assert(vim.deep_equal(completion.values(cache, "o"), { "origin/main" }))
  for _ = 1, 100 do completion.values(cache, "b") end
  assert(requests == 1 and #scheduled == 0, "warm completion scheduled repository work")

  completion.invalidate(cache)
  assert(vim.deep_equal(completion.values(cache, "a"), { "alpha" }))
  drain()
  local previous = cache.snapshot
  respond(nil, "Git fixture failed")
  drain()
  assert(cache.snapshot == previous and cache.pending == nil)
  assert(notices[#notices]:find("Git fixture failed", 1, true))
  completion.values(cache, "")
  assert(#scheduled == 0, "failed refresh retried before its backoff")
  now = now + 1000
  completion.values(cache, "")
  drain()
  respond(snapshot(2, {}), nil)
  drain()
  assert(cache.snapshot and #cache.snapshot.values == 0 and not cache.stale)

  local large = {}
  for index = 1, 20000 do large[index] = string.format("branch-%05d", index) end
  assert(completion.replace(cache, snapshot(3, large)))
  assert(#completion.values(cache, "branch-") == 200)
  assert(vim.deep_equal(completion.values(cache, "branch-19999"), { "branch-19999" }))
  assert(cache.accounted_bytes <= 2 * 1024 * 1024)
  large[1] = "mutated"
  assert(completion.values(cache, "branch-00001")[1] == "branch-00001", "cache retained a mutable input list")
  local accepted = cache.snapshot
  large[1] = "branch-00001"
  large[20001] = "branch-20001"
  assert(not completion.replace(cache, snapshot(4, large)))
  assert(not completion.replace(cache, snapshot(4, { string.rep("x", 2 * 1024 * 1024) })))
  assert(not completion.replace(cache, snapshot(4, { "z", "a" })))
  assert(not completion.replace(cache, snapshot(4, { "a", "a" })))
  assert(not completion.replace(cache, snapshot(4, { "a\0b" })))
  assert(not completion.replace(cache, snapshot(4, { "a b" })))
  assert(not completion.replace(cache, snapshot(3, { "old" })))
  assert(cache.snapshot == accepted)

  completion.invalidate(cache)
  completion.values(cache, "")
  drain()
  completion.invalidate(cache)
  respond(snapshot(4, { "outdated" }), nil)
  drain()
  assert(cache.snapshot == accepted and cache.stale)
  assert(notices[#notices]:find("repository invalidation", 1, true))

  completion.values(cache, "")
  drain()
  local old_response = respond
  completion.clear_generation(cache, "host-two")
  assert(cache.snapshot == nil and cache.accounted_bytes == 0)
  old_response(snapshot(4, { "outdated" }), nil)
  drain()
  assert(cache.snapshot == nil)
  assert(notices[#notices]:find("superseded host generation", 1, true))
  assert(completion.replace(cache, snapshot(1, { "raw-" .. string.char(255) }, "host-two")))
  assert(completion.values(cache, "raw-")[1] == "raw-" .. string.char(255))

  local duplicate = completion.new("repository", "host-one", function(_, _, done)
    done(snapshot(1, { "once" }), nil)
    done(snapshot(2, { "twice" }), nil)
  end)
  completion.values(duplicate, "")
  drain()
  assert(duplicate.snapshot and duplicate.snapshot.revision == 1)
  local thrown = completion.new("repository", "host-one", function() error("request launch failure") end)
  completion.values(thrown, "")
  drain()
  assert(thrown.pending == nil and thrown.snapshot == nil)
  assert(notices[#notices]:find("request launch failure", 1, true))
end, debug.traceback)

vim.schedule, vim.notify, vim.uv.now = original_schedule, original_notify, original_now
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
