vim.loader.enable(false)

local diff_review = require("diff_review")
local syntax_engine = require("diff_review.render.syntax_engine")
local git_data = require("diff_review.git.git_data")
local gh = require("diff_review.integrations.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local root = vim.fs.normalize(original_cwd .. "/.diffreview-pr-syntax-test")
local original_compute_hunk_context_async = git_data.compute_hunk_context_async
local original_compute_file_syntax_async = git_data.compute_file_syntax_async
local original_compute_diff_syntax_async = git_data.compute_diff_syntax_async
local original_debug_log_path = diff_review._gitstatus_debug_log_path
local original_debug_force = diff_review._gitstatus_debug_force
local debug_log_path = vim.fn.tempname()

local function assert_true(condition, message)
  if not condition then error(message, 2) end
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

local function find_row(buf, needle)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  for index, line in ipairs(lines) do
    if line:find(needle, 1, true) then return index end
  end
  error("missing row: " .. needle .. "\n" .. table.concat(lines, "\n"), 2)
end

local function trigger(buf, key, row)
  vim.api.nvim_set_current_buf(buf)
  if row then vim.api.nvim_win_set_cursor(0, { row, 0 }) end
  local mapping = vim.api.nvim_buf_call(buf, function()
    return vim.fn.maparg(key, "n", false, true)
  end)
  assert_true(type(mapping.callback) == "function", "missing mapping for " .. key)
  mapping.callback()
end

local pr_diff_text = table.concat({
  "diff --git a/src/a.ts b/src/a.ts",
  "index 1111111..2222222 100644",
  "--- a/src/a.ts",
  "+++ b/src/a.ts",
  "@@ -1,5 +1,5 @@",
  " alpha",
  " beta",
  "-old line",
  "+NEW LINE",
  " omega",
  " done",
}, "\n")

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(command, input, cb)
  local key = table.concat(command, " ")
  vim.defer_fn(function()
    if key:find("gh pr diff", 1, true) then
      cb({ code = 0, stdout = pr_diff_text, stderr = "", output = pr_diff_text })
      return
    end
    if key == "gh api graphql --input -" and tostring(input or ""):find("reviewThreads", 1, true) then
      local stdout = vim.json.encode({
        data = {
          repository = {
            pullRequest = {
              reviews = { nodes = {} },
              comments = { nodes = {} },
              reviewThreads = { nodes = {} },
            },
          },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    if key == "gh api graphql --input -" and tostring(input or ""):find("statusCheckRollup", 1, true) then
      local stdout = vim.json.encode({
        data = {
          repository = {
            pullRequest = {
              commits = {
                nodes = {
                  {
                    commit = {
                      statusCheckRollup = {
                        contexts = { nodes = {} },
                      },
                    },
                  },
                },
              },
            },
          },
        },
      })
      cb({ code = 0, stdout = stdout, stderr = "", output = stdout })
      return
    end
    cb({ code = 1, stdout = "", stderr = "unexpected: " .. key, output = "unexpected: " .. key })
  end, 3)
end

function gh_backend.open_url()
  return true
end

---@type DiffReviewGitBackend
local git_backend = {}

function git_backend.systemlist()
  return {}, 1
end

function git_backend.systemlist_async(_, cb)
  vim.defer_fn(function() cb({}, 1) end, 3)
end

function git_backend.system(command)
  return "unexpected git command: " .. table.concat(command, " "), 1
end

function git_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "", output = "" })
  end, 3)
end

function git_backend.delete()
  return 0
end

local pr = {
  number = 22,
  title = "PR syntax",
  body = "",
  url = "https://github.com/owner/repo/pull/22",
  repo = "owner/repo",
  headRefName = "feature",
  headRefOid = "2222222222222222222222222222222222222222",
  commits = {},
  files = {
    { path = "src/a.ts", additions = 1, deletions = 1 },
  },
  changedFiles = 1,
  additions = 1,
  deletions = 1,
}

local function run()
  vim.fn.delete(root, "rf")
  vim.fn.delete(debug_log_path)
  assert_true(vim.fn.mkdir(vim.fs.joinpath(root, "src"), "p") == 1, "mkdir failed")
  local file_path = vim.fs.joinpath(root, "src", "a.ts")
  assert_true(vim.fn.writefile({
    "alpha",
    "beta",
    "NEW LINE",
    "omega",
    "done",
  }, file_path) == 0, "writefile failed")

  local file_syntax_requests = {}
  local hunk_context_requests = {}
  git_data.compute_hunk_context_async = function(filename, line, cb)
    hunk_context_requests[#hunk_context_requests + 1] = {
      filename = vim.fs.normalize(filename),
      line = line,
    }
    cb(nil)
  end
  git_data.compute_file_syntax_async = function(filename, cb)
    file_syntax_requests[#file_syntax_requests + 1] = vim.fs.normalize(filename)
    cb(nil)
  end
  git_data.compute_diff_syntax_async = function(filename, lines_for_syntax, cb)
    cb(nil)
  end

  diff_review.set_git_backend(git_backend)
  gh.set_backend(gh_backend)
  diff_review._gitstatus_debug_log_path = function() return debug_log_path end
  diff_review._gitstatus_debug_force = true
  diff_review.setup({ about_auto_generate = false })

  assert_true(syntax_engine.status_syntax_source_for_entry_kind("pr_file") == "file", "PR files should use file syntax")
  assert_true(syntax_engine.status_syntax_source_for_entry_kind("pr_hunk") == "file", "PR hunks should use file syntax")
  assert_true(syntax_engine.status_syntax_source_for_entry_kind("pr_review_hunk") == "diff", "PR review hunks should use diff syntax")
  assert_true(syntax_engine.status_syntax_source_for_entry_kind("commit_hunk") == "diff", "commit hunks should use diff syntax")

  local buf = diff_review.open_pr(pr, { cwd = root })
  assert_true(buf ~= nil, "open_pr did not return a buffer")
  local win = vim.fn.bufwinid(buf)
  assert_true(win ~= -1, "PR buffer is not visible")
  assert_true(vim.wo[win].wrap == true, "PR overview should reuse GitStatus soft-wrap window formatting")
  assert_true(vim.wo[win].linebreak == true, "PR overview should enable linebreak for word-aware wrap")
  assert_true(vim.wo[win].conceallevel == 0, "PR overview should not conceal diff code markers")
  wait_for(function() return buffer_contains(buf, "src/a.ts +1 -1") end, "PR changed file did not render")
  trigger(buf, "<Tab>", find_row(buf, "src/a.ts +1 -1"))
  wait_for(function() return buffer_contains(buf, "NEW LINE") end, "PR changed file did not expand")

  local expected = vim.fs.normalize(file_path)
  wait_for(function()
    return vim.tbl_contains(file_syntax_requests, expected)
  end, "PR hunk did not request file syntax")

  wait_for(function()
    if vim.fn.filereadable(debug_log_path) ~= 1 then return false end
    local content = table.concat(vim.fn.readfile(debug_log_path), "\n")
    return content:find("load_pr_diff.done", 1, true)
      and content:find("render_pr_status.start", 1, true)
      and content:find("status_render_hunk.before", 1, true)
      and content:find("build_fancy_diff_rows.syntax", 1, true)
      and content:find("pr_hunk", 1, true)
      and content:find("new_side_matches_file = true", 1, true)
  end, "PR hunk debug instrumentation did not write expected events")

  file_syntax_requests = {}
  hunk_context_requests = {}
  assert_true(vim.fn.writefile({
    "alpha",
    "beta",
    "LOCAL ONLY",
    "omega",
    "done",
  }, file_path) == 0, "rewrite mismatched file failed")

  local mismatch_buf = diff_review.open_pr(pr, { cwd = root })
  assert_true(mismatch_buf ~= nil, "open_pr did not return a mismatch buffer")
  wait_for(function() return buffer_contains(mismatch_buf, "src/a.ts +1 -1") end, "mismatched PR changed file did not render")
  trigger(mismatch_buf, "<Tab>", find_row(mismatch_buf, "src/a.ts +1 -1"))
  wait_for(function() return buffer_contains(mismatch_buf, "NEW LINE") end, "mismatched PR hunk did not expand")
  assert_true(not buffer_contains(mismatch_buf, "LOCAL ONLY"), "mismatched PR hunk rendered local-only source context")
  assert_true(
    not vim.tbl_contains(file_syntax_requests, expected),
    "mismatched PR hunk should not request file syntax"
  )
  for _, request in ipairs(hunk_context_requests) do
    assert_true(request.filename ~= expected, "mismatched PR hunk should not request local Tree-sitter context")
  end

end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
git_data.compute_hunk_context_async = original_compute_hunk_context_async
git_data.compute_file_syntax_async = original_compute_file_syntax_async
git_data.compute_diff_syntax_async = original_compute_diff_syntax_async
diff_review._gitstatus_debug_log_path = original_debug_log_path
diff_review._gitstatus_debug_force = original_debug_force
vim.fn.delete(root, "rf")
vim.fn.delete(debug_log_path)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
