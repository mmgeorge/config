vim.loader.enable(false)

local open_pr = require("github.open_pr")
local forge = require("forge")
local gh = require("forge.integrations.gh")

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
local generated_prompt = nil
local generated_system = nil
local captured_head = string.rep("1", 40)
local original_write = require("forge.git.write").execute
local original_forge_open_pr = forge.open_pr
local original_host_request = require("forge.client").request_host
local creation_draft
local creation_capture
local opened_pull_request
local generated_pr_body = table.concat({
  "Related #",
  "Created by ForgeGithubPRCreate.",
  table.concat({
    "## Testing",
    "- [ ] Automated (integration, performance, screenshot, unit)",
    "- [ ] Manual (test app)",
  }, "\n"),
}, "\n\n")

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
  generated_prompt = nil
  generated_system = nil
  creation_draft, creation_capture = nil, nil
  opened_pull_request = nil
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
forge.setup({ pr_buffer_name = "ForgePRTest" })
gh.set_backend({
  system_async = function(command, _, cb)
    local key = command_key(command)
    if key == "gh\tpr\tview\t" .. created_pr_url .. "\t--json\tid,number,title,body,url,headRefName,headRefOid,commits,files,changedFiles,additions,deletions,reviewRequests,milestone,isDraft,state,createdAt,updatedAt,closedAt" then
      cb({
        code = 0,
        stdout = vim.json.encode({
          id = "PR_kwTEST42",
          number = 42,
          title = "feat: create draft pr",
          body = generated_pr_body,
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
    generated_prompt = opts.prompt
    generated_system = opts.system
    cb({
      ok = true,
      content = "Created by ForgeGithubPRCreate.",
    })
  end,
}
vim.notify = function() end
require("forge.git.write").execute = function(directory, action, callback)
  assert_true(directory == root and action.kind == "publish_branch" and action.name == "feature/current"
    and action.head == captured_head, "publish branch lost captured workspace/head")
  callback({ ok = true, output = "" })
end
local function native_request(method, params, callback)
  if method == "github.creation.context" then
    assert_true(params.directory == root, "creation context lost workspace")
    callback({ repository = { hostname = require("github.repo_cache").hostname(), owner = "org", name = "repo" },
      repository_node_id = "R_test", branch = "feature/current", head_commit = captured_head })
  elseif method == "github.review.draft" then callback(creation_draft)
  elseif method == "github.review.draft.write" then creation_draft = vim.deepcopy(params.draft) callback(true)
  elseif method == "github.recovery.inspect" then callback(nil)
  elseif method == "github.actor" then callback({ node_id = "ACTOR_test", login = "viewer" })
  elseif method == "github.review.mutate" then
    creation_capture = vim.deepcopy(params.request)
    assert_true(creation_capture.resource.number == 0 and creation_capture.resource.kind == "repository", "creation used a PR-number scope")
    assert_true(creation_capture.mutation.head_commit == captured_head and creation_capture.mutation.body == generated_pr_body,
      "creation lost captured generation/head")
    callback({ version = 1, resource = params.request.resource,
      capture = { operation_id = params.request.operation_id, actor = params.request.actor_node_id },
      state = { phase = "confirmed", result = { html_url = created_pr_url, number = 42 } } })
  elseif method == "github.recovery.settle_draft" then
    assert_true(creation_capture ~= nil and params.operation_id == creation_capture.operation_id, "creation settlement lost operation identity")
    creation_draft = vim.deepcopy(params.draft)
    callback(true)
  else callback(nil, "unavailable fixture route " .. method) end
end
require("forge.client").request_host = native_request
require("github.pr_creation")._set_runner_for_test(native_request)
require("github.mutation")._set_runner_for_test(native_request)
forge.open_pr = function(pr, options)
  opened_pull_request = { pr = vim.deepcopy(pr), options = vim.deepcopy(options) }
end
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
  if key == "git\tlog\t--reverse\t--format=%s%n%b%n---END-COMMIT---\torigin/main.." .. captured_head then
    callback({ code = 0, stdout = "feat: create draft pr\n---END-COMMIT---\n", stderr = "" })
    return
  end
  if key == "git\tdiff\t--stat\t--summary\torigin/main.." .. captured_head then
    callback({ code = 0, stdout = " nvim/lua/github/open_pr.lua | 12 +++++++-----\n", stderr = "" })
    return
  end
  if key == "git\tdiff\t--no-ext-diff\t--no-color\torigin/main.." .. captured_head then
    callback({
      code = 0,
      stdout = table.concat({
        "diff --git a/nvim/lua/github/open_pr.lua b/nvim/lua/github/open_pr.lua",
        "--- a/nvim/lua/github/open_pr.lua",
        "+++ b/nvim/lua/github/open_pr.lua",
        "@@ -1 +1 @@",
        "-old prompt",
        "+new prompt",
      }, "\n"),
      stderr = "",
    })
    return
  end
  callback({
    code = 0,
    stdout = branch_list_output,
    stderr = "",
  })
end

local function cleanup()
  require("forge.git.write").execute = original_write
  require("forge.client").request_host = original_host_request
  require("github.pr_creation")._set_runner_for_test(nil)
  require("github.mutation")._set_runner_for_test(nil)
  forge.open_pr = original_forge_open_pr
  open_pr._set_state_path_for_test(nil)
  gh.reset_backend()
  forge.reset_git_backend()
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
  assert_true(generated_system:find("Return ONLY the PR description paragraph", 1, true) ~= nil, "PR prompt should request only a description")
  assert_true(
    generated_prompt:find("Compact diff context:", 1, true) ~= nil,
    "PR prompt should include compact diff context"
  )
  assert_true(
    generated_prompt:find("Provide a PR description paragraph for the changes above", 1, true) ~= nil,
    "PR prompt should include description instructions"
  )
  wait_for(function()
    return opened_pull_request ~= nil
  end, "created PR did not open in Forge PR view")
  assert_true(opened_pull_request.pr.title == "feat: create draft pr", "created PR view missing title")
  assert_true(opened_pull_request.options.cwd == root, "created PR view lost its repository root")
  assert_true(creation_capture and creation_capture.mutation.operation == "pull_request_create", "PR creation bypassed durable native capture")
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
