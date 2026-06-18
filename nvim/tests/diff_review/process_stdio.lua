vim.loader.enable(false)

local diff_review = require("diff_review")
local ai_commit = require("diff_review.ai_commit")
local gh = require("diff_review.gh")
local github_gh = require("github.gh")
local issue_index = require("github.issue_index")

local original_system = vim.system
local original_notify = vim.notify
local root = "D:/diffreview-stdio-root"
local calls = {}
local notifications = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function buffer_contains(buf, needle)
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function assert_production_process_calls_are_safe()
  local root_dir = vim.fn.getcwd()
  local files = vim.fn.globpath(root_dir .. "/nvim/lua", "**/*.lua", false, true)
  local failures = {}
  for _, file in ipairs(files) do
    local lines = vim.fn.readfile(file)
    for line_number, line in ipairs(lines) do
      if line:find("os%.execute", 1, false) then
        failures[#failures + 1] = file .. ":" .. tostring(line_number) .. ": os.execute bypasses stdio capture: " .. vim.trim(line)
      end
      if line:find("vim%.system", 1, false) then
        local block = {}
        for offset = 0, 14 do
          block[#block + 1] = lines[line_number + offset] or ""
        end
        local text = table.concat(block, "\n")
        local captures_text = text:find("stdout%s*=%s*true") and text:find("stderr%s*=%s*true")
        local streams_text = text:find("stdout%s*=%s*function") and text:find("stderr%s*=%s*function")
        if not (captures_text or streams_text) then
          failures[#failures + 1] = file .. ":" .. tostring(line_number) .. ": " .. vim.trim(line)
        end
      end
    end
  end
  assert_true(#failures == 0, "unsafe production process calls:\n" .. table.concat(failures, "\n"))
end

local function git_result(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then
    return { code = 0, stdout = root .. "\n", stderr = "" }
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { code = 0, stdout = "abc1234\n", stderr = "" }
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { code = 0, stdout = "main\n", stderr = "" }
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { code = 0, stdout = "stdio test\n", stderr = "" }
  end
  if key == "gh\trepo\tview\t--json\tnameWithOwner" then
    return { code = 0, stdout = vim.json.encode({ nameWithOwner = "owner/repo" }), stderr = "" }
  end
  return { code = 0, stdout = "", stderr = "" }
end

local function is_status_startup_process(command)
  local key = command_key(command)
  return key == "git\trev-parse\t--show-toplevel"
    or key:sub(1, #("git\t-C\t" .. root .. "\t")) == "git\t-C\t" .. root .. "\t"
    or key:sub(1, #"gh\t") == "gh\t"
end

local function run()
  assert_production_process_calls_are_safe()

  diff_review.reset_git_backend()
  ai_commit.reset_backend()
  gh.reset_backend()
  github_gh.reset_backend()
  issue_index._reset_for_test()
  issue_index._set_progress_enabled_for_test(false)
  issue_index._set_runner_for_test(function(_, _, callback)
    local stdout = vim.json.encode({})
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)
  issue_index._set_gh_runner_for_test(function(_, _, callback)
    local stdout = vim.json.encode({
      data = {
        rateLimit = { remaining = 5000 },
        repository = {
          issues = {
            totalCount = 0,
            pageInfo = { hasNextPage = false },
            nodes = {},
          },
        },
      },
    })
    callback({ code = 0, stdout = stdout, stderr = "", output = stdout })
  end)
  vim.notify = function(message, level, opts)
    notifications[#notifications + 1] = {
      message = tostring(message),
      level = level,
      opts = opts,
    }
  end
  vim.system = function(command, opts, on_exit)
    calls[#calls + 1] = {
      command = vim.deepcopy(command),
      opts = vim.deepcopy(opts or {}),
    }
    vim.defer_fn(function()
      if command[1] == "git" or command[1] == "gh" then
        on_exit(git_result(command))
      else
        on_exit({ code = 1, stdout = "", stderr = "unexpected command: " .. command_key(command) })
      end
    end, 1)
    return {
      kill = function() end,
    }
  end

  assert_true(
    diff_review._system_output("stdout", "stderr") == "stdout\nstderr",
    "system output should keep both stdout and stderr"
  )

  diff_review.setup({
    about_auto_generate = false,
    pr_lookup_mode = "mock-delay",
    pr_mock_delay_ms = 10000,
  })
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function() return buffer_contains(buf, "No changes") end, "status buffer did not render")

  local checked = 0
  for _, call in ipairs(calls) do
    if not is_status_startup_process(call.command) then goto continue end
    checked = checked + 1
    assert_true(call.opts.text == true, "process did not request text mode: " .. command_key(call.command))
    assert_true(call.opts.stdout == true, "process did not capture stdout: " .. command_key(call.command))
    assert_true(call.opts.stderr == true, "process did not capture stderr: " .. command_key(call.command))
    ::continue::
  end
  assert_true(checked > 0, "status open did not spawn any DiffReview git process")
  assert_true(#notifications == 0, "stdio status open should not notify: " .. vim.inspect(notifications))
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
ai_commit.reset_backend()
gh.reset_backend()
github_gh.reset_backend()
issue_index._reset_for_test()
vim.system = original_system
vim.notify = original_notify
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
