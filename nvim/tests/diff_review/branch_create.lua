vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local root = "D:/diffreview-branch-create-root"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local original_notify = vim.notify
local captured_notifications = {}
local function capture_notify(message, level, opts)
  captured_notifications[#captured_notifications + 1] = { message = tostring(message), level = level, opts = opts }
end

local function saw_notification_containing(needle)
  for _, notification in ipairs(captured_notifications) do
    if notification.message:find(needle, 1, true) then return true end
  end
  return false
end

---@type DiffReviewGhBackend
local gh_backend = {}
function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local switch_calls = {}
local switch_should_fail = false

---@type DiffReviewGitBackend
local backend = {}

function backend.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "init" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  vim.defer_fn(function()
    local output, code = backend.systemlist(command)
    cb(output, code)
  end, 3)
end

function backend.system(command)
  local key = command_key(command)
  if key == "git\t-C\t" .. root .. "\tswitch\t-c\tmatt9222/feature" or key == "git\t-C\t" .. root .. "\tswitch\t-c\tteam/feature" then
    switch_calls[#switch_calls + 1] = key
    if switch_should_fail then return "fatal: branch exists", 128 end
    return "", 0
  end
  return "", 0
end

function backend.system_async(command, input, cb)
  vim.defer_fn(function()
    local output, code = backend.system(command)
    cb({ code = code, stdout = output, stderr = "", output = output })
  end, 3)
end

function backend.delete() return 0 end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function trigger(buf, key)
  vim.api.nvim_win_set_buf(0, buf)
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing buffer mapping for " .. key)
  mapping.callback()
end

local original_prompt = diff_review._prompt_branch_name
---@type { default: string? }
local input_capture = {}
local function stub_input(response)
  diff_review._prompt_branch_name = function(prefix, on_submit)
    input_capture.default = prefix
    on_submit(response)
  end
end

local function run()
  vim.notify = capture_notify
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })

  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(buf, "master") end, "status did not render")

  -- ── default prefix matt9222/ is prefilled, branch is created ───────────────
  stub_input("matt9222/feature")
  trigger(buf, "bc")
  assert_true(input_capture.default == "matt9222/", "branch prompt not prefilled with the default prefix: " .. tostring(input_capture.default))
  wait_for(function() return #switch_calls == 1 end, "git switch -c was not run")
  assert_true(switch_calls[1]:find("switch\t-c\tmatt9222/feature", 1, true) ~= nil, "wrong switch command: " .. switch_calls[1])
  wait_for(function() return saw_notification_containing("Created branch matt9222/feature") end, "branch creation not notified")

  -- ── cancel (nil input) does nothing ────────────────────────────────────────
  switch_calls = {}
  stub_input(nil)
  trigger(buf, "bc")
  vim.wait(40, function() return false end, 10)
  assert_true(#switch_calls == 0, "cancelled prompt must not create a branch")

  -- ── leaving just the prefix does nothing ───────────────────────────────────
  stub_input("matt9222/")
  trigger(buf, "bc")
  vim.wait(40, function() return false end, 10)
  assert_true(#switch_calls == 0, "submitting only the prefix must not create a branch")

  -- ── per-repo .diffreview.json overrides the prefix ─────────────────────────
  diff_review._repo_config.set_reader(function(path)
    if path:find(".diffreview.json", 1, true) then return '{ "branch_prefix": "team/" }' end
    return nil
  end)
  stub_input("team/feature")
  trigger(buf, "bc")
  assert_true(input_capture.default == "team/", "repo config prefix not used: " .. tostring(input_capture.default))
  wait_for(function() return #switch_calls == 1 end, "git switch -c was not run for repo-config prefix")
  diff_review._repo_config.reset_reader()

  -- ── failed switch surfaces the git error ───────────────────────────────────
  switch_calls = {}
  switch_should_fail = true
  captured_notifications = {}
  stub_input("matt9222/feature")
  trigger(buf, "bc")
  wait_for(function() return saw_notification_containing("Create branch failed") end, "failed branch creation not notified")
end

local ok, err = xpcall(run, debug.traceback)
vim.notify = original_notify
diff_review._prompt_branch_name = original_prompt
diff_review._repo_config.reset_reader()
diff_review.reset_git_backend()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
