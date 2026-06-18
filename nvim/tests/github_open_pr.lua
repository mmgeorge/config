vim.loader.enable(false)

local open_pr = require("github.open_pr")
local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local root = "D:/mock/github"
local other_root = "D:/mock/other-github"
local state_path = vim.fn.tempname() .. ".json"
local captured_picker = nil
local system_calls = {}
local branch_list_output = nil
local original_system = vim.system
local original_notify = vim.notify
local original_snacks = _G.Snacks
local original_picker_pick = original_snacks and original_snacks.picker and original_snacks.picker.pick
local original_ai = package.loaded["ai"]
local original_ai_adapters = package.loaded["ai.adapters"]
local created_pr_url = "https://github.example.test/org/repo/pull/42"
local generated_model = nil

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
  captured_picker = nil
  system_calls = {}
  generated_model = nil
  branch_list_output = table.concat({
    "main",
    "feature/current",
    "remotes/origin/HEAD -> origin/main",
    "remotes/origin/develop",
    "remotes/origin/main",
    "origin/release/2026",
    "develop",
  }, "\n")
end

local function count_system_calls(key)
  local count = 0
  for _, call in ipairs(system_calls) do
    if call.key == key then count = count + 1 end
  end
  return count
end

open_pr._set_state_path_for_test(state_path)
reset()
diff_review.setup({ pr_buffer_name = "DiffReviewPRTest" })
gh.set_backend({
  system_async = function(command, _, cb)
    local key = command_key(command)
    if key == "gh\tpr\tview\t" .. created_pr_url .. "\t--json\tid,number,title,body,url,headRefName,headRefOid,commits,files,changedFiles,additions,deletions,reviewRequests,milestone,isDraft" then
      cb({
        code = 0,
        stdout = vim.json.encode({
          id = "PR_kwTEST42",
          number = 42,
          title = "feat: create draft pr",
          body = "Created by GithubPRCreate.",
          url = created_pr_url,
          headRefName = "feature/current",
          headRefOid = "abc123456789",
          commits = {
            { oid = "abc123456789", messageHeadline = "feat: create draft pr" },
          },
          files = {},
          changedFiles = 0,
          additions = 0,
          deletions = 0,
          reviewRequests = {},
          milestone = vim.NIL,
          isDraft = true,
        }),
        stderr = "",
        output = "",
      })
      return
    end
    if key == "gh\tpr\tdiff\t42\t--patch\t--color\tnever" then
      cb({ code = 0, stdout = "", stderr = "", output = "" })
      return
    end
    cb({ code = 1, stdout = "", stderr = "unexpected gh command: " .. key, output = "unexpected gh command: " .. key })
  end,
})
package.loaded["ai.adapters"] = {
  get = function()
    return {
      commit = "test-commit-model",
      pr_create = "test-pr-create-model",
    }
  end,
}
package.loaded["ai"] = {
  generate = function(opts, cb)
    generated_model = opts.model
    cb({
      ok = true,
      content = "```json\n" .. vim.json.encode({
        title = "feat: create draft pr",
        body = "Created by GithubPRCreate.",
      }) .. "\n```",
    })
  end,
}
vim.notify = function() end
if not _G.Snacks then _G.Snacks = {} end
if not Snacks.picker then Snacks.picker = {} end
Snacks.picker.pick = function(opts)
  captured_picker = opts
  return opts
end

vim.system = function(command, opts, callback)
  system_calls[#system_calls + 1] = {
    command = vim.deepcopy(command),
    key = command_key(command),
    cwd = opts and opts.cwd or nil,
  }
  local key = command_key(command)
  if key == "git\trev-parse\t--show-toplevel" then
    callback({ code = 0, stdout = root .. "\n", stderr = "" })
    return
  end
  if key == "git\tbranch\t--show-current" then
    callback({ code = 0, stdout = "feature/current\n", stderr = "" })
    return
  end
  if key == "git\trev-parse\t--verify\torigin/main" then
    callback({ code = 0, stdout = "abc123456789\n", stderr = "" })
    return
  end
  if key == "git\tlog\t--reverse\t--format=%h %s%n%b%n---END-COMMIT---\torigin/main..HEAD" then
    callback({ code = 0, stdout = "abc1234 feat: create draft pr\n---END-COMMIT---\n", stderr = "" })
    return
  end
  if key == "git\trev-parse\t--abbrev-ref\t--symbolic-full-name\t@{u}" then
    callback({ code = 1, stdout = "", stderr = "no upstream" })
    return
  end
  if key == "git\tpush\t-u\torigin\tfeature/current" then
    callback({ code = 0, stdout = "", stderr = "" })
    return
  end
  if key == "gh\tpr\tcreate\t--draft\t--base\tmain\t--head\tfeature/current\t--title\tfeat: create draft pr\t--body\tCreated by GithubPRCreate." then
    callback({ code = 0, stdout = created_pr_url .. "\n", stderr = "" })
    return
  end
  callback({
    code = 0,
    stdout = branch_list_output,
    stderr = "",
  })
end

local function cleanup()
  open_pr._set_state_path_for_test(nil)
  gh.reset_backend()
  diff_review.reset_git_backend()
  package.loaded["ai"] = original_ai
  package.loaded["ai.adapters"] = original_ai_adapters
  pcall(vim.fn.delete, state_path)
  if original_snacks then
    _G.Snacks = original_snacks
    if original_snacks.picker then original_snacks.picker.pick = original_picker_pick end
  else
    _G.Snacks = nil
  end
  vim.system = original_system
  vim.notify = original_notify
end

local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "x", false)
end

local function run_tests()
  reset()
  local selected = nil
  open_pr._choose_base_branch_for_test(root, "feature/current", function(branch)
    selected = branch
  end)
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: main")
  end, "base confirmation did not render")
  press("y")
  wait_for(function() return selected == "main" end, "yes did not keep the last selected base branch")
  assert_true(
    count_system_calls("git\tbranch\t--list\t--all\t--format=%(refname:short)") == 1,
    "default base selection should list branches once"
  )

  reset()
  selected = nil
  open_pr._choose_base_branch_for_test(root, "feature/current", function(branch)
    selected = branch
  end)
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: main")
  end, "base confirmation before picker did not render")
  press("n")
  wait_for(function() return captured_picker ~= nil end, "no did not open Snacks branch picker")
  assert_true(captured_picker.title == "Select PR base branch", "unexpected picker title")
  assert_true(#captured_picker.items == 3, "branch picker should deduplicate and exclude current/HEAD")
  assert_true(captured_picker.items[1].branch == "develop", "expected sorted develop branch")
  assert_true(captured_picker.items[2].branch == "main", "expected sorted main branch")
  assert_true(captured_picker.items[3].branch == "release/2026", "expected normalized release branch")
  captured_picker.confirm({ close = function() end }, captured_picker.items[1])
  wait_for(function() return selected == "develop" end, "selected branch was not returned")
  assert_true(open_pr._get_base_branch_for_test(root) == "develop", "selected base was not cached per repo")
  open_pr._set_state_path_for_test(state_path)
  assert_true(open_pr._get_base_branch_for_test(root) == "develop", "selected base was not persisted to state")
  assert_true(open_pr._get_base_branch_for_test(other_root) == nil, "base selection leaked across repos")
  assert_true(system_calls[1].cwd == root, "branch picker did not list branches from repo root")

  reset()
  selected = nil
  open_pr._choose_base_branch_for_test(root, "feature/current", function(branch)
    selected = branch
  end)
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: develop")
  end, "persisted base confirmation did not render")
  press("y")
  wait_for(function() return selected == "develop" end, "yes did not use the persisted repo base branch")
  assert_true(#system_calls == 0, "confirming the persisted base should not list branches")

  reset()
  branch_list_output = table.concat({
    "feature/current",
    "remotes/origin/HEAD -> origin/master",
    "remotes/origin/master",
    "master",
  }, "\n")
  selected = "unset"
  open_pr._choose_base_branch_for_test(other_root, "feature/current", function(branch)
    selected = branch
  end)
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: master")
  end, "master fallback base confirmation did not render")
  press("y")
  wait_for(function() return selected == "master" end, "yes did not use master fallback base branch")

  reset()
  branch_list_output = table.concat({
    "feature/current",
    "remotes/origin/release/2026",
    "release/2026",
  }, "\n")
  selected = "unset"
  open_pr._choose_base_branch_for_test(other_root, "feature/current", function(branch)
    selected = branch
  end)
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: ")
  end, "empty fallback base confirmation did not render")
  press("y")
  wait_for(function() return selected == nil end, "yes with no default base should not select a branch")

  reset()
  open_pr._set_base_branch_for_test("main", root)
  open_pr.open()
  wait_for(function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false), "Base: main")
  end, "create flow base confirmation did not render")
  press("y")
  wait_for(function() return generated_model == "test-pr-create-model" end, "PR create did not use pr_create adapter")
  wait_for(function()
    return vim.bo[vim.api.nvim_get_current_buf()].filetype == "GitStatus"
      and vim.api.nvim_buf_get_name(0):find("DiffReviewPRTest://42", 1, true) ~= nil
  end, "created PR did not open in DiffReview PR view")
  assert_true(vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Title:  feat: create draft pr"), "created PR view missing title")
  assert_true(
    not vim.tbl_contains(vim.tbl_map(function(call) return call.key end, system_calls), "gh\tpr\tview\t" .. created_pr_url .. "\t--web"),
    "create flow still opened the browser"
  )
end

local ok, err = xpcall(run_tests, debug.traceback)
cleanup()
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("github_open_pr: ok")
vim.cmd("qa!")
