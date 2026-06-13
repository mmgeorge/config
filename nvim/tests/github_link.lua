vim.loader.enable(false)

local gh = require("github.gh")

local root = "D:/mock/github"
local opened_urls = {}
local system_calls = {}
local original_system = vim.system
local original_notify = vim.notify

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local function wait_for(predicate, message)
  local ok = vim.wait(1000, predicate, 10)
  assert_true(ok, message)
end

local function reset()
  opened_urls = {}
  system_calls = {}
end

local function set_test_buffer()
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(buf, root .. "/AGENTS.md")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
  })
  vim.api.nvim_set_current_buf(buf)
  return buf
end

---@type GithubGhBackend
local backend = {}

function backend.open_url(url)
  opened_urls[#opened_urls + 1] = url
  return true
end

vim.system = function(command, opts, callback)
  system_calls[#system_calls + 1] = {
    command = vim.deepcopy(command),
    key = command_key(command),
    cwd = opts and opts.cwd or nil,
  }
  local key = command_key(command)
  local stdout = ""
  local code = 0

  if key:find("git\t-C\t", 1, true) and key:find("\trev-parse\t--show-toplevel", 1, true) then
    stdout = root .. "\n"
  elseif key == "git\t-C\t" .. root .. "\tremote\tget-url\torigin" then
    stdout = "git@github.com:mmgeorge/config.git\n"
  elseif key == "git\t-C\t" .. root .. "\tbranch\t--show-current" then
    stdout = "master\n"
  else
    code = 1
  end

  callback({
    code = code,
    stdout = stdout,
    stderr = code == 0 and "" or "unexpected command: " .. key,
  })
end

gh.set_backend(backend)
vim.notify = function() end

local plugin_spec = require("plugins.github")[1]
plugin_spec.init()

local function cleanup()
  gh.reset_backend()
  vim.system = original_system
  vim.notify = original_notify
end

local function run_tests()
  local buf = set_test_buffer()

  reset()
  vim.api.nvim_win_set_cursor(0, { 6, 0 })
  vim.cmd.GithubLink()
  wait_for(function() return #opened_urls == 1 end, "single-line GithubLink did not open")
  assert_true(
    opened_urls[1] == "https://github.com/mmgeorge/config/blob/master/AGENTS.md?plain=1#L6",
    "wrong single-line GithubLink URL: " .. tostring(opened_urls[1])
  )

  reset()
  vim.cmd("4,6GithubLink")
  wait_for(function() return #opened_urls == 1 end, "range GithubLink did not open")
  assert_true(
    opened_urls[1] == "https://github.com/mmgeorge/config/blob/master/AGENTS.md?plain=1#L4-L6",
    "wrong range GithubLink URL: " .. tostring(opened_urls[1])
  )

  reset()
  vim.cmd("4,6GithubBlame")
  wait_for(function() return #opened_urls == 1 end, "range GithubBlame did not open")
  assert_true(
    opened_urls[1] == "https://github.com/mmgeorge/config/blame/master/AGENTS.md#L4-L6",
    "wrong range GithubBlame URL: " .. tostring(opened_urls[1])
  )

  assert_true(#system_calls >= 3, "git commands were not used")
  vim.api.nvim_buf_delete(buf, { force = true })
end

local ok, err = xpcall(run_tests, debug.traceback)
cleanup()
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("github_link: ok")
vim.cmd("qa!")
