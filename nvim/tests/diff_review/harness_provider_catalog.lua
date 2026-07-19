vim.loader.enable(false)

require("diff_review").setup()

local client = require("diff_review.harness.client")
local picker = require("diff_review.views.picker")
local picker_state = require("diff_review.views.picker.state")
local provider_catalog = require("diff_review.views.harness.provider_catalog")
local provider_picker = require("diff_review.views.harness.provider_picker")
local session = require("diff_review.session")

local function fail(message)
  vim.api.nvim_err_writeln(message)
  vim.cmd("cquit")
end

local function assert_true(value, message)
  if not value then fail(message) end
end

local original_request = client.request
local skill_enabled = { walkthrough = true, imagegen = false }
local mcp_enabled = false
local request_method_list = {}

client.request = function(method, params, callback)
  request_method_list[#request_method_list + 1] = method
  if method == "backend.skills" then
    callback({
      {
        name = "walkthrough",
        description = "Build a review walkthrough",
        enabled = skill_enabled.walkthrough,
        user_invocable = true,
      },
      {
        name = "imagegen",
        description = "Generate an image",
        enabled = skill_enabled.imagegen,
        user_invocable = true,
      },
      {
        name = "internal-helper",
        description = "Provider internal skill",
        enabled = true,
        user_invocable = false,
      },
    })
  elseif method == "backend.skills.set_enabled" then
    skill_enabled[params.name] = params.enabled
    callback({ name = params.name, enabled = params.enabled, restart_required = false })
  elseif method == "backend.mcp" then
    callback({
      {
        name = "github-mcp",
        transport = "stdio",
        enabled = mcp_enabled,
        status = mcp_enabled and "connected" or "disabled",
        token_count = 704,
        token_estimated = true,
        tools = {
          { name = "actions_list" },
          { name = "user_get" },
        },
      },
    })
  elseif method == "backend.mcp.set_enabled" then
    mcp_enabled = params.enabled
    callback({ name = params.name, enabled = params.enabled, restart_required = false })
  else
    callback(nil, "unexpected request " .. method)
  end
end

session.harness.session = {
  id = "provider-catalog-test",
  backend = "codex",
  workspace = "D:/repo",
}
session.harness.capability = {
  catalog = { skill = true, mcp = true, live_mcp_mutation = false },
}
provider_catalog.clear()

local input_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_win_set_buf(0, input_buf)
vim.api.nvim_buf_set_lines(input_buf, 0, -1, false, { "$wa" })
vim.api.nvim_win_set_cursor(0, { 1, 3 })
local command_source = require("diff_review.views.harness.completion.command_source").new()
assert_true(command_source:enabled(), "$ completion should activate for the first token")
local completion
command_source:get_completions({}, function(result) completion = result end)
assert_true(completion ~= nil, "$ completion should return provider skills")
assert_true(#completion.items == 1, "$ completion should hide disabled skills")
assert_true(completion.items[1].label == "$walkthrough", "$ completion should label the enabled skill")
assert_true(completion.items[1].textEdit.newText == "walkthrough ", "$ completion should preserve the leading $")

vim.api.nvim_buf_set_lines(input_buf, 0, -1, false, { "use $wa" })
vim.api.nvim_win_set_cursor(0, { 1, 7 })
assert_true(not command_source:enabled(), "$ completion should not activate after ordinary text")
vim.api.nvim_buf_set_lines(input_buf, 0, -1, false, { "$walkthrough explain" })
vim.api.nvim_win_set_cursor(0, { 1, 20 })
assert_true(not command_source:enabled(), "$ completion should stop after the skill selector")

local host = { window_list = { 0 }, control_win = 0 }
local inserted_text
provider_picker.open_skills({
  host = host,
  on_insert = function(text) inserted_text = text end,
})
local skill_picker = picker._state_for_test()
assert_true(skill_picker ~= nil, "/skills should open the shared picker")
local skill_page = picker_state.page(skill_picker.state, skill_picker.spec)
assert_true(skill_page.option_list[1].label == "✓ walkthrough", "enabled skills should use the check icon")
assert_true(skill_page.option_list[2].label == "× imagegen", "disabled skills should use the cross icon")
assert_true(#skill_page.option_list == 2, "/skills should hide provider-internal skills")
skill_picker.spec.on_confirm({ option = skill_page.option_list[1] })
assert_true(inserted_text == "$walkthrough ", "Enter should insert an enabled skill selector")

skill_picker.spec.action_list[1].callback({ option = skill_page.option_list[1] })
skill_picker = picker._state_for_test()
skill_page = picker_state.page(skill_picker.state, skill_picker.spec)
assert_true(skill_page.option_list[1].label == "× walkthrough", "Space should disable an enabled skill")
inserted_text = nil
assert_true(skill_picker.spec.on_confirm({ option = skill_page.option_list[1] }) == false,
  "Enter should reject a disabled skill")
assert_true(inserted_text == nil, "disabled skills should not enter the composer")
vim.api.nvim_buf_set_lines(input_buf, 0, -1, false, { "$wa" })
vim.api.nvim_win_set_cursor(0, { 1, 3 })
completion = nil
command_source:get_completions({}, function(result) completion = result end)
assert_true(completion ~= nil and #completion.items == 0,
  "disabling a skill should remove it from $ completion")
picker.close(false)

local mutation
provider_picker.open_mcp({
  host = host,
  on_mutation = function(result) mutation = result end,
})
local mcp_picker = picker._state_for_test()
assert_true(mcp_picker ~= nil, "/mcp should open the shared picker")
local mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
assert_true(mcp_page.option_list[1].label == "○ github-mcp", "disabled MCPs should use the empty-circle icon")
assert_true(mcp_page.option_list[1].detail:find("stdio", 1, true) ~= nil, "MCP rows should show transport")
assert_true(mcp_page.option_list[1].detail:find("~704 tokens", 1, true) ~= nil,
  "MCP rows should distinguish estimated token counts")

mcp_picker.spec.action_list[2].callback({ option = mcp_page.option_list[1] })
mcp_picker = picker._state_for_test()
assert_true(vim.iter(mcp_picker.frame.lines):any(function(line)
  return line:find("Tools: actions_list, user_get", 1, true) ~= nil
end), "Tab should unfold cached tools into real picker lines")

mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
mcp_picker.spec.action_list[1].callback({ option = mcp_page.option_list[1] })
assert_true(mutation and mutation.enabled, "Space should report the completed MCP enable mutation")
mcp_picker = picker._state_for_test()
mcp_page = picker_state.page(mcp_picker.state, mcp_picker.spec)
assert_true(mcp_page.option_list[1].label == "✓ github-mcp", "connected MCPs should use the check icon")
assert_true(vim.tbl_contains(request_method_list, "backend.mcp.set_enabled"),
  "MCP toggles should use the normalized broker operation")

picker.close(false)
client.request = original_request
provider_catalog.clear()
io.write("harness_provider_catalog OK\n")
vim.cmd("qa!")
