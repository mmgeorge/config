local root = vim.fn.getcwd()
local live_fixture = "nvim/rust/forge/crates/forge-providers/tests/fixtures/activity_host.lua"
local mock_fixture = "nvim/tests/forge/harness_status_split_host.lua"
local scenario_path = "nvim/tests/forge/plan_review_question_response_host.lua"
local scenario_fixture = scenario_path .. "::answer while question remains focused"

local function select_suite(suite, files, affected)
  local command = { "nu", "--no-config-file", ".forge/checks/lua-tests.nu", "--suite", suite, "--list" }
  if affected then table.insert(command, "--affected") end
  vim.list_extend(command, files or {})
  return vim.system(command, { cwd = root, text = true }):wait(10000)
end

local function selected(suite, files)
  local result = select_suite(suite, files)
  assert(result.code == 0, result.stderr)
  local report = vim.json.decode(result.stdout)
  assert(report.suite == suite and report.fixtures, "missing suite selection report")
  return report.fixtures
end

local function rejected(suite, files, message, affected)
  local result = select_suite(suite, files, affected)
  assert(result.code ~= 0, "accepted an incompatible fixture selection")
  assert(result.stderr:find(message, 1, true), result.stderr)
end

local live = selected("live-integration")
local mock = selected("mock-integration")
local ordinary = selected("ordinary")
local default = selected("all")
assert(#live == 3, "live suite no longer matches the audited provider fixtures")
assert(vim.list_contains(live, live_fixture))
assert(vim.list_contains(mock, mock_fixture), "plan regression is not in the mock suite")
assert(vim.list_contains(default, mock_fixture), "default run omits mock integration fixtures")
assert(not vim.list_contains(mock, scenario_path), "a scenario fixture was listed as one process")
assert(vim.list_contains(mock, scenario_fixture), "scenario fixture is not listed once per scenario")
assert(vim.deep_equal(selected("mock-integration", { scenario_fixture }), { scenario_fixture }))
for _, fixture in ipairs(live) do
  assert(not vim.list_contains(mock, fixture), "live fixture leaked into mock suite: " .. fixture)
  assert(not vim.list_contains(ordinary, fixture), "live fixture leaked into ordinary tests: " .. fixture)
  assert(not vim.list_contains(default, fixture), "live fixture leaked into the default run: " .. fixture)
end
for _, fixture in ipairs(mock) do
  assert(not vim.list_contains(ordinary, fixture), "integration fixture leaked into ordinary tests: " .. fixture)
end
assert(vim.deep_equal(selected("mock-integration", { mock_fixture }), { mock_fixture }))
assert(vim.deep_equal(selected("live-integration", { live_fixture }), { live_fixture }))
rejected("mock-integration", { live_fixture }, "Live provider fixture requires the live-integration suite")
rejected("live-integration", { mock_fixture }, "Fixture does not belong to live-integration")
rejected("all", { live_fixture }, "Live provider fixture requires the live-integration suite")
rejected("ordinary", { live_fixture }, "Use the explicit integration or benchmark check", true)
rejected("ordinary", { mock_fixture }, "Use the explicit integration or benchmark check", true)
rejected("benchmarks", { live_fixture }, "Live provider fixture requires the live-integration suite")
local override = vim.system({
  "nu", "--no-config-file", "--stdin", ".forge/checks/lua-tests.nu", "check", "affected",
}, {
  cwd = root,
  text = true,
  stdin = vim.json.encode({ version = 1, workspace = root, inputs = { suite = "live-integration" } }),
}):wait(10000)
assert(override.code ~= 0 and override.stderr:find("Use lua-test-mock-integration", 1, true), override.stderr)
print("lua_suite_selection: mock/live isolation and exact selections passed")
