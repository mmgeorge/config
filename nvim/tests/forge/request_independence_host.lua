vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
local executable = require("forge.builder").binary_path()
assert(vim.fn.executable(executable) == 1, "build the Forge host before running this fixture")
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local git = vim.system({ "git", "-C", workspace, "init", "--quiet" }, { text = true }):wait()
assert(git.code == 0, git.stderr)
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback) callback({ ok = true, path = executable }) end }
require("forge").setup({ harness = { backend = "mock", backends = { mock = { command = { "fork-blocking" } } } } })
vim.fn.chdir(workspace)
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)

local function begin(specification)
  local response = {}
  client.request(specification, function(result, failure, detail)
    response.result, response.failure, response.detail, response.done = result, failure, detail, true
  end)
  return response
end
local function finish(response, timeout)
  assert(vim.wait(timeout or 10000, function() return response.done end, 5), "request did not settle")
  assert(not response.failure, response.failure)
  return response.result
end
local function request(specification)
  return finish(begin(specification))
end
local function measure_reads()
  local samples = {}
  for index = 1, 30 do
    local started = vim.uv.hrtime()
    request({ method = "repository.revision.list", params = { workspace = workspace } })
    samples[index] = (vim.uv.hrtime() - started) / 1e6
  end
  table.sort(samples)
  return { p50_ms = samples[15], p95_ms = samples[29], p99_ms = samples[30] }
end

local succeeded, failure = xpcall(function()
  local startup = {}
  client.session_initialize(function(result, problem)
    startup.result, startup.failure, startup.done = result, problem, true
  end, { new_session_name = "request owner" })
  local parent = finish(startup).session.id
  local idle = measure_reads()
  local fork = request({ method = "session.fork", session_id = parent, params = { name = "held fork" } })
  local child = fork.session.id
  local waiting = begin({ method = "session.prompt.submit", session_id = child, params = { text = "Complete after the fork is ready" } })
  local parent_read = begin({ method = "session.state.get", session_id = parent })
  local health = begin({ method = "session.health.get", session_id = parent })
  local invalid = begin({ method = "session.state.get", session_id = "missing-session" })
  assert(finish(parent_read, 1500).session.id == parent)
  finish(health, 1500)
  assert(vim.wait(1500, function() return invalid.done end, 5) and invalid.failure,
    "invalid-session failure did not remain local to its request")
  assert(not waiting.done, "held preparation did not overlap independent reads")
  local contention = measure_reads()
  finish(waiting)

  local settings = request({ method = "settings.get" })
  local first = begin({ method = "settings.update", params = {
    expected_revision = settings.revision, scope = "global", desired = { plan_compact = true },
  } })
  local second = begin({ method = "settings.update", params = {
    expected_revision = settings.revision, scope = "global", desired = { plan_compact = false },
  } })
  assert(vim.wait(3000, function() return first.done and second.done end, 5), "conflicting writes did not settle")
  assert((first.failure == nil) ~= (second.failure == nil), "same-revision writes both committed or both failed")
  local final = request({ method = "settings.get" })
  assert(final.revision == settings.revision + 1, "conflicting write advanced the revision twice")
  print(vim.json.encode({ idle = idle, contention = contention }))
end, debug.traceback)
client.host_stop()
vim.wait(6000, function() return not client._client or not client._client.process end, 10)
client._host_reset_for_test()
vim.fn.stdpath = original_stdpath
vim.fn.chdir(original_directory)
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("request_independence_host: passed")
vim.cmd("qa!")
