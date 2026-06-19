vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")
local root = "D:/mock/project"

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

local pending = {}
local generation = "first"

local backend = {} ---@type DiffReviewGitBackend

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

function backend.systemlist(command)
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then
    return { root }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then
    return { generation == "first" and "1111111" or "2222222" }, 0
  end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then
    return { "master" }, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then
    return { generation .. " subject" }, 0
  end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then
    return {}, 1
  end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return {}, 0
  end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  local request_generation = generation
  local output, code = backend.systemlist(command)
  pending[#pending + 1] = {
    generation = request_generation,
    run = function()
      cb(output, code)
    end,
  }
end

local function flush(target_generation)
  local flushed = true
  while flushed do
    flushed = false
    for index = #pending, 1, -1 do
      local item = pending[index]
      if item.generation == target_generation then
        table.remove(pending, index)
        item.run()
        flushed = true
      end
    end
  end
end

local function buffer_contains(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for _, line in ipairs(lines) do
    if line:find(needle, 1, true) then return true end
  end
  return false
end

local function run()
  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })

  generation = "first"
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  assert_true(buffer_contains(buf, "Loading DiffReview..."), "first render did not enter loading state")

  generation = "second"
  diff_review.render_status(buf)
  flush("second")
  assert_true(vim.wait(1000, function()
    return buffer_contains(buf, "second subject")
  end, 10), "second render did not complete")

  flush("first")
  vim.wait(50)
  assert_true(buffer_contains(buf, "second subject"), "stale first render replaced the second render")
  assert_true(not buffer_contains(buf, "first subject"), "stale first render became visible")
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
