vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = data .. "/forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), binary))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local errors = {}
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
local function request(method, params)
  local settled, value, failure
  client.request(method, params or {}, function(result, error) value, failure, settled = result, error, true end)
  assert(vim.wait(10000, function() return settled end, 10), method .. " timeout")
  assert(not failure, method .. ": " .. tostring(failure))
  return value
end
local function status()
  local transcript = state.presentation and state.presentation.transcript
  if not transcript then return nil end
  for index = transcript.sequence:count() - 1, 0, -1 do
    local metadata = transcript.sequence:at(index).entry.metadata.status
    if metadata and metadata ~= vim.NIL then return metadata end
  end
end
local function hints()
  return vim.inspect(vim.api.nvim_buf_get_extmarks(state.transcript_buf,
    vim.api.nvim_create_namespace("ForgeHarnessStatusHint"), 0, -1, { details = true }))
end
local succeeded, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false,
    harness = { backend = "mock", backends = { mock = { command = { "approval-blocking" }, selectable = true } } } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.session ~= nil end, 10), "startup timeout")
  for iteration = 1, 2 do
    local settled, result, problem
    client.request("prompt.submit", { text = "Resume after approval " .. iteration }, function(value, error)
      settled, result, problem = true, value, error
    end)
    assert(vim.wait(10000, function()
      local current = status()
      return #(state.approval or {}) == 1 and current and current.hint == "permission"
    end, 10), "approval never reached the timeline: " .. vim.inspect(errors))
    assert(vim.wait(1000, function() return hints():find("review permission", 1, true) ~= nil end, 10),
      "permission action was not rendered: " .. hints())
    local approval = state.approval[1]
    local answers = {}
    for _, item in ipairs(approval.item_list) do answers[#answers + 1] = { item_id = item.id, choice_id = "allow_once" } end
    request("approval.resolve", { approval_id = approval.id, answer_list = answers })
    assert(vim.wait(1500, function()
      local current = status()
      return #(state.approval or {}) == 0 and current and current.animated and current.hint == "working"
    end, 10), "approval cleared without resuming timeline activity: " .. vim.inspect(status()))
    assert(not settled, "provider finished before the resumed status was observed")
    assert(not hints():find("review permission", 1, true), "stale permission action remained visible")
    assert(vim.wait(10000, function() return settled end, 10), "provider did not complete")
    assert(not problem, tostring(problem))
    assert(result.exchange.state == "complete", "approved exchange did not complete")
  end
  assert(#errors == 0, table.concat(errors, "\n"))
end, debug.traceback)
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
assert(succeeded and collected, failure or "approval host was not collected")
print("harness_approval_resume_host: passed")
