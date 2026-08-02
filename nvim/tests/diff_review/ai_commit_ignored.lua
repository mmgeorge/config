package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local ai_commit = require("diff_review.integrations.ai_commit")
local ignored_path_store = require("diff_review.views.status.ignored_path_store")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function run()
  local root = "D:/ai-commit-ignored"
  local command_list = {}
  local generated_context = nil

ignored_path_store.reset_for_test()
ignored_path_store.set_io_for_test({
  read_async = function(_, callback)
    local stored_root = vim.fn.has("win32") == 1 and root:lower() or root
    callback(vim.json.encode({ version = 1, root = stored_root, ignored_paths = { "ignored.lua" } }), nil)
  end,
  write_atomic_async = function(_, _, callback) callback(nil) end,
})

ai_commit.reset_backend()
ai_commit.set_backend({
  systemlist_async = function(command, callback)
    command_list[#command_list + 1] = vim.deepcopy(command)
    if vim.tbl_contains(command, "--summary") then
      callback({ " visible.lua | 1 +" }, 0)
    else
      callback({
        "diff --git a/visible.lua b/visible.lua",
        "--- a/visible.lua",
        "+++ b/visible.lua",
        "@@ -0,0 +1 @@",
        "+visible",
      }, 0)
    end
  end,
  generate_async = function(context, callback)
    generated_context = context
    callback({ ok = true, message = "feat: include visible change" })
  end,
})

local result = nil
ai_commit.ensure(root, { force = true }, function(state) result = state end)
assert_true(vim.wait(3000, function() return result ~= nil end, 10), "AI commit generation did not finish")
assert_true(result.state == "ready", "AI commit generation did not succeed")
assert_true(generated_context and generated_context:find("visible.lua", 1, true), "visible file missing from AI context")
assert_true(not generated_context:find("ignored.lua", 1, true), "ignored file leaked into AI context")
assert_true(#command_list == 3, "AI commit generation did not run fingerprint, stat, and diff commands")
for _, command in ipairs(command_list) do
  assert_true(vim.tbl_contains(command, "."), "AI diff command missing inclusive pathspec")
  assert_true(vim.tbl_contains(command, ":(exclude)ignored.lua"), "AI diff command missing ignored pathspec")
end

  ai_commit.reset_backend()
  ignored_path_store.set_io_for_test(nil)
  ignored_path_store.reset_for_test()
end

local ok, error_message = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit")
end
vim.cmd("qa!")
