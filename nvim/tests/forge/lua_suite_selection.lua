local root = vim.fn.getcwd()
local live_fixture = "nvim/tests/forge/harness_codex_activity.lua"
local mock_fixture = "nvim/tests/forge/plan_review_question_response_host.lua"

local function select_suite(suite, files)
  local selection = files and { kind = "files", files = files } or { kind = "all" }
  local result = vim.system({
    "nu", "--no-config-file", "--stdin", ".forge/checks/lua-tests.nu", suite, "--list-only",
  }, {
    cwd = root,
    text = true,
    stdin = vim.json.encode({ version = 1, selection = selection }),
  }):wait(10000)
  return result
end

local function selected(suite, files)
  local result = select_suite(suite, files)
  assert(result.code == 0, result.stderr)
  local report = vim.json.decode(result.stdout)
  assert(report.suite == suite and report.fixtures, "missing suite selection report")
  return report.fixtures
end

local function rejected(suite, files, message)
  local result = select_suite(suite, files)
  assert(result.code ~= 0, "accepted an incompatible fixture selection")
  assert(result.stderr:find(message, 1, true), result.stderr)
end

local live = selected("live-integration")
local mock = selected("mock-integration")
local ordinary = selected("all")
assert(#live == 4, "live suite no longer matches the audited provider fixtures")
assert(vim.list_contains(live, live_fixture))
assert(vim.list_contains(mock, mock_fixture), "plan regression is not in the mock suite")
for _, fixture in ipairs(live) do
  assert(not vim.list_contains(mock, fixture), "live fixture leaked into mock suite: " .. fixture)
  assert(not vim.list_contains(ordinary, fixture), "live fixture leaked into ordinary tests: " .. fixture)
end
for _, fixture in ipairs(mock) do
  assert(not vim.list_contains(ordinary, fixture), "integration fixture leaked into ordinary tests: " .. fixture)
end
assert(vim.deep_equal(selected("mock-integration", { mock_fixture }), { mock_fixture }))
assert(vim.deep_equal(selected("live-integration", { live_fixture }), { live_fixture }))
rejected("mock-integration", { live_fixture }, "Live provider fixture requires the live-integration suite")
rejected("live-integration", { mock_fixture }, "Fixture does not belong to live-integration")
rejected("affected", { live_fixture }, "Use the explicit integration or benchmark check")
rejected("affected", { mock_fixture }, "Use the explicit integration or benchmark check")
rejected("benchmarks", { live_fixture }, "Live provider fixture requires the live-integration suite")
local override = vim.system({
  "nu", "--no-config-file", "--stdin", "nvim/tools/checks/lua.nu", "affected",
}, {
  cwd = root,
  text = true,
  stdin = vim.json.encode({ version = 1, workspace = root, inputs = { suite = "live-integration" } }),
}):wait(10000)
assert(override.code ~= 0 and override.stderr:find("Use lua-test-mock-integration", 1, true), override.stderr)
print("lua_suite_selection: mock/live isolation and exact selections passed")
