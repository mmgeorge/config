local recorded = {}
local notifications = {}
local function record(name)
  return function(...) recorded[#recorded + 1] = { name = name, arguments = { ... } } end
end

package.loaded.forge = {
  setup = record("setup"),
  open = record("status"),
  open_harness = record("harness"),
  new_harness_session = record("new_session"),
  open_branch_diff = record("branch"),
  open_file_revision = record("revision"),
  open_compact_preview = record("compact"),
}
package.loaded["forge.views.permissions"] = { open = record("permissions") }
package.loaded["forge.client"] = { request = record("request") }

local ok, failure = xpcall(function()
  local original_notify = vim.notify
  vim.notify = function(message, level, options)
    notifications[#notifications + 1] = { message = message, level = level, options = options }
  end
  local plugin = require("plugins.forge")[1]
  plugin.config(nil, plugin.opts)
  local commands = vim.api.nvim_get_commands({})
  local expected_commands = {
    "ForgeStatus", "ForgeBranchDiff", "ForgeBranchDiffFile", "ForgeFileRevision", "ForgeDiffCompactPreview",
    "ForgeHarness", "ForgeHarnessNew", "ForgeHarnessLog", "ForgePermissions", "ForgeStartupLog",
    "ForgeGitConfigCacheReset",
  }
  assert(vim.deep_equal(plugin.cmd, expected_commands), "lazy command inventory changed")
  for _, name in ipairs(expected_commands) do assert(commands[name], name .. " was not registered") end
  for _, name in ipairs({ "GitStatus", "GitBranchDiff", "GitBranchDiffFile", "GitFileRevision",
    "GitDiffCompactPreview", "Harness", "HarnessNew", "HarnessLog", "Permissions", "ForgeBuildLog" }) do
    assert(not commands[name], "Obsolete command registered: " .. name)
  end
  vim.cmd("ForgeStatus")
  assert(recorded[#recorded].name == "status")
  vim.cmd("ForgeHarness")
  assert(recorded[#recorded].name == "harness")
  vim.cmd("ForgeHarnessNew")
  assert(recorded[#recorded].name == "new_session")
  vim.cmd("ForgePermissions")
  assert(recorded[#recorded].name == "permissions")
  vim.cmd("ForgeBranchDiff HEAD~1")
  assert(recorded[#recorded].arguments[1] == "HEAD~1")
  vim.cmd("ForgeBranchDiffFile file.rs HEAD ignored")
  assert(recorded[#recorded].arguments[1] == "HEAD")
  assert(recorded[#recorded].arguments[2].file == "file.rs")
  vim.cmd("ForgeFileRevision file.rs HEAD ignored")
  assert(vim.deep_equal(recorded[#recorded].arguments, { "file.rs", "HEAD" }))
  vim.cmd("ForgeDiffCompactPreview!")
  assert(recorded[#recorded].arguments[1].staged == true)
  vim.cmd("ForgeDiffCompactPreview")
  assert(recorded[#recorded].arguments[1].staged == false)
  vim.cmd("ForgeHarnessLog on")
  assert(recorded[#recorded].arguments[1] == "trace.configure")
  assert(recorded[#recorded].arguments[2].enabled == true)
  vim.cmd("ForgeHarnessLog")
  assert(recorded[#recorded].arguments[1] == "trace.status")
  vim.cmd("ForgeHarnessLog invalid")
  assert(notifications[#notifications].message == "Usage: ForgeHarnessLog [on|off|toggle|clear]")
  vim.cmd("ForgeBranchDiffFile only-file")
  assert(notifications[#notifications].message == "Usage: ForgeBranchDiffFile <file> <branch>")
  vim.cmd("ForgeFileRevision only-file")
  assert(notifications[#notifications].message == "Usage: ForgeFileRevision <file> <commit>")
  assert(commands.ForgeStatus.nargs == "0" and commands.ForgeStatus.definition == "Review git changes")
  assert(commands.ForgeHarness.nargs == "0" and commands.ForgeHarness.definition == "Open the Forge AI Harness")
  assert(commands.ForgeHarnessNew.nargs == "0" and commands.ForgeHarnessNew.definition == "Create a fresh Harness session")
  assert(commands.ForgePermissions.nargs == "0" and commands.ForgePermissions.definition == "Edit Harness permissions")
  assert(commands.ForgeHarnessLog.nargs == "?" and commands.ForgeHarnessLog.definition == "Open or control the Harness protocol trace")
  assert(commands.ForgeBranchDiff.nargs == "1" and commands.ForgeBranchDiff.definition == "Diff the working tree against a branch or revision")
  assert(commands.ForgeBranchDiffFile.nargs == "+")
  assert(commands.ForgeFileRevision.nargs == "+")
  assert(commands.ForgeBranchDiffFile.definition == "Diff one file in the working tree against a branch or revision")
  assert(commands.ForgeFileRevision.definition == "Open a file read-only as it exists at a git revision")
  assert(commands.ForgeDiffCompactPreview.bang and commands.ForgeDiffCompactPreview.definition == "Preview compacted git diff; use bang for staged diff")
  vim.notify = original_notify
  require("plugins.github")[1].init()
  commands = vim.api.nvim_get_commands({})
  for _, suffix in ipairs({ "PRCreate", "Link", "Blame", "Issue", "IssueCreate", "IssueSync",
    "PR", "Review", "Notifications", "DeleteRepoCache" }) do
    assert(commands["ForgeGithub" .. suffix], "Missing GitHub command: " .. suffix)
    assert(not commands["Github" .. suffix], "Obsolete GitHub command: " .. suffix)
  end
end, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit")
end
io.write("public_commands OK\n")
vim.cmd("qa!")
