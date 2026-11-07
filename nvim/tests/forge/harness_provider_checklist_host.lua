-- Exercise provider checklists and deferred native creation through a real host and mock provider.
vim.loader.enable(false)
local original_directory = vim.fn.getcwd()
vim.opt.runtimepath:prepend(original_directory .. "/nvim")
local fixture_evidence = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/evidence_directory.lua")("harness-provider-checklist")
local evidence = fixture_evidence.path
local workspace, data = evidence .. "/workspace", evidence .. "/data"
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local function git(...)
  local command = { "git", "-C", workspace }
  vim.list_extend(command, { ... })
  local result = vim.system(command, { text = true }):wait()
  assert(result.code == 0, result.stderr)
end
git("init", "--quiet")
vim.fn.writefile({ "Checklist fixture" }, workspace .. "/README.md")
git("add", "README.md")
git("-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid", "commit", "--quiet", "-m", "Fixture baseline")
local binary = data .. "/forge-host" .. (vim.fn.has("win32") == 1 and ".exe" or "")
-- Launch the shared pinned executable lease rather than a freshly scanned per-run copy.
binary = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/host_executable.lua")()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind) return (kind == "data" or kind == "config") and data or original_stdpath(kind) end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local diagnostics, last_snapshot = {}, nil
local notifications = require("forge.infra.notifications")
local original_error = notifications.error
notifications.error = function(message, title)
  diagnostics[#diagnostics + 1] = { title = title, message = tostring(message) }
  return original_error(message, title)
end

---@param method string
---@param params? table
---@return any
local state_snapshot = dofile(vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)) .. "/support/state.lua")
local function request(method, params)
  -- Snapshot reads replay their publication page; see support/state.lua.
  if method == "session.state.get" then last_snapshot = state_snapshot.get() return last_snapshot end
  local completion = {}
  client.request({ method = method, params = params or {},
    session_id = require("forge.event_contract").method_policy(method).session_context ~= "none"
      and assert(state.session).id or nil }, function(value, failure)
    completion.value, completion.failure, completion.done = value, failure, true
  end)
  assert(vim.wait(10000, function() return completion.done end, 10), method .. " did not finish")
  assert(not completion.failure, tostring(completion.failure))
  if method == "session.state.get" then last_snapshot = completion.value end
  return completion.value
end

---@param entries { content: string, status: "pending"|"in_progress"|"completed" }[]
---@return table
local function checklist_step(entries)
  local entry_list = {}
  for ordinal, entry in ipairs(entries) do
    entry_list[ordinal] = { provider_id = "entry-" .. ordinal, content = entry.content,
      priority = vim.NIL, status = entry.status, provider_ordinal = ordinal - 1 }
  end
  return { kind = "checklist", update = { scope_id = "fixture-plan", name = "Fixture plan",
    complete = false, replace_entries = true, entry_list = entry_list } }
end

---@return string[]
local function transcript()
  return vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)
end

---@param pattern string
---@return integer
local function count(pattern)
  local total = 0
  for _, line in ipairs(transcript()) do
    if line:find(pattern, 1, true) then total = total + 1 end
  end
  return total
end

---@param steps table[]
---@param text string
local function prompt(steps, text)
  local reply = "Fixture reply: " .. text
  table.insert(steps, { kind = "message", content = "response", text = reply })
  request("session.provider.mock_response", { operation = "script",
    script = { steps = steps, terminal = "completed" } })
  request("session.prompt.submit", { text = text })
  assert(vim.wait(10000, function()
    return count(reply) > 0 and request("session.state.get").status.kind == "idle"
  end, 25), "scripted prompt did not settle: " .. text)
end

---@param pattern string
---@param message string
local function await_line(pattern, message)
  assert(vim.wait(10000, function() return count(pattern) > 0 end, 20), message)
end

-- Finished exchanges show sections as headings; open one through the transcript's real mapping.
---@param heading string
local function open_section(heading)
  local window = vim.fn.bufwinid(state.transcript_buf)
  assert(window ~= -1, "transcript window is not displayed")
  local row
  for index, line in ipairs(transcript()) do
    if line:find(heading, 1, true) then row = index end
  end
  assert(row, "section heading is missing: " .. heading)
  vim.api.nvim_set_current_win(window)
  vim.api.nvim_win_set_cursor(window, { row, 0 })
  local mapping = vim.fn.maparg("<Tab>", "n", false, true)
  assert(type(mapping.callback) == "function", "transcript <Tab> mapping is missing")
  mapping.callback()
end

local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = true, harness = { provider = "mock" } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return state.ready and state.session ~= nil end, 10), "startup timeout")

  -- Startup keeps the new chat local: no native conversation exists before the first input.
  local opened = request("session.state.get")
  assert(opened.session.provider_session_id == nil or opened.session.provider_session_id == vim.NIL,
    "startup allocated a native conversation: " .. vim.inspect(opened.session.provider_session_id))

  prompt({ checklist_step({
    { content = "Read the owner", status = "completed" },
    { content = "Move the types", status = "in_progress" },
    { content = "Document it", status = "pending" },
  }) }, "Report the checklist.")
  local first = request("session.state.get")
  assert(type(first.session.provider_session_id) == "string" and first.session.provider_session_id ~= "",
    "the first input did not open a native conversation")
  await_line("Provider-reported checklist · 1/3 completed · Fixture plan", "checklist heading did not render")
  open_section("Provider-reported checklist · 1/3 completed · Fixture plan")
  await_line("[x] Read the owner", "checklist rows did not open")
  for _, row in ipairs({ "[x] Read the owner", "[>] Move the types · in progress", "[ ] Document it" }) do
    assert(count(row) == 1, "missing checklist row " .. row .. "\n" .. table.concat(transcript(), "\n"))
  end

  -- A later exchange without a checklist neither repeats nor fabricates progress.
  local transcript_window = vim.fn.bufwinid(state.transcript_buf)
  assert(transcript_window ~= -1, "transcript window is not displayed")
  vim.api.nvim_win_set_cursor(transcript_window, { 1, 0 })
  prompt({}, "Answer without a checklist.")
  assert(count("Provider-reported checklist") == 1, "a checklist-free exchange fabricated a checklist section")
  assert(vim.api.nvim_win_get_cursor(transcript_window)[1] == 1, "transcript refresh moved the cursor")

  -- A provider replacement updates the canonical state rather than appending stale rows.
  prompt({
    checklist_step({ { content = "Ship it", status = "in_progress" }, { content = "Verify it", status = "in_progress" } }),
    checklist_step({ { content = "Ship it", status = "completed" }, { content = "Verify it", status = "in_progress" } }),
  }, "Update the checklist.")
  await_line("Provider-reported checklist · 1/2 completed · Fixture plan", "replaced checklist did not render")
  open_section("Provider-reported checklist · 1/2 completed · Fixture plan")
  await_line("[x] Ship it", "replaced checklist rows did not open")
  assert(count("[x] Ship it") == 1 and count("[>] Ship it") == 0, "replacement duplicated an entry")
  assert(count("[>] Verify it · in progress") == 1, "in-progress entry was not shown")
  assert(#diagnostics == 0, vim.inspect(diagnostics))
end, debug.traceback)
if not success then
  local diagnostic = { workspace = workspace, data = data, snapshot = last_snapshot, errors = diagnostics,
    host_stderr = client._client.stderr, failure = failure,
    transcript = state.transcript_buf and vim.api.nvim_buf_is_valid(state.transcript_buf) and transcript() or nil }
  vim.fn.writefile(vim.split(vim.inspect(diagnostic), "\n", { plain = true }), evidence .. "/failure.txt")
  failure = tostring(failure) .. "\nRetained diagnostics: " .. evidence .. "/failure.txt"
end
notifications.error = original_error
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(original_directory)
vim.fn.stdpath = original_stdpath
-- Remove passing evidence only after the host exits and releases its files.
local release_failure = fixture_evidence.release(success and collected)
assert(success and collected, failure or "host was not collected")
assert(not release_failure, release_failure)
print("harness_provider_checklist_host: passed")
