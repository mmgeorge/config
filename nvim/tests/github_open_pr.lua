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
local original_background = package.loaded["forge.integrations.background_settings"]
local created_pr_url = "https://github.example.test/org/repo/pull/42"
local generated_model = nil
local generated_prompt = nil
local generation_failure = nil
local captured_head = string.rep("1", 40)
local base_commit = string.rep("2", 40)
local compact_context = "Changed files:\n  moved.rs (Renamed from original.rs)\n  source.rs (+1 -1)\n\nsource.rs (+1 -1)\n\n- old();\n+ new();\n"
local context_failure
local context_conflicts
local context_calls
local fetch_failure
local notices = {}
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
  assert_true(ok, message .. "\n" .. table.concat(notices, "\n"))
end

local function reset()
  captured_picker = nil
  system_calls = {}
  generated_model = nil
  generated_prompt = nil
  generation_failure = nil
  creation_draft, creation_capture = nil, nil
  context_failure, context_calls = nil, 0
  context_conflicts = false
  fetch_failure = nil
  notices = {}
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
    if key == "gh\tpr\tview\t" .. created_pr_url .. "\t--json\tid,number,title,body,url,headRefName,headRefOid,baseRefOid,baseRefName,headRepository,headRepositoryOwner,commits,files,changedFiles,additions,deletions,reviewRequests,milestone,isDraft,state,createdAt,updatedAt,closedAt" then
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
package.loaded["ai"] = { generate = function() error("PR generation bypassed Harness") end }
package.loaded["forge.integrations.background_settings"] = {
  request = function(_, _, callback) callback({ settings = {} }) end,
  model = function() return { model = "test-background-model", thinking = "low" } end,
}
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
require("forge.git.write").execute = function(directory, action, callback)
  assert_true(directory == root and action.kind == "publish_branch" and action.name == "feature/current"
    and action.head == captured_head, "publish branch lost captured workspace/head")
  callback({ ok = true, output = "" })
end
local function native_request(method, params, callback)
  if method == "repository.generate" then
    if params.operation == "pull_request" then
      assert_true(params.workspace == root and params.comparison == "pull_request", "PR generation lost workspace")
      assert_true(params.head == captured_head, "PR generation lost its captured HEAD")
      assert_true(params.generation ~= nil and params.generation.reasoning_effort == "low", "PR generation lost Harness configuration")
      generated_model = params.generation.model
      generated_prompt = params.input
      if generation_failure then callback(nil, generation_failure) return end
      callback({ state = "ready", message = "Created by ForgeGithubPRCreate." })
      return
    end
    context_calls = context_calls + 1
    assert_true(params.operation == "context" and params.comparison == "pull_request", "PR did not request native range context")
    assert_true(params.workspace == root and params.base == base_commit and params.head == captured_head,
      "native context lost the selected base or pinned head")
    assert_true(params.max_bytes > 0 and params.max_bytes < 500000, "PR did not reserve instruction bytes")
    if context_failure then callback(nil, context_failure) return end
    if context_conflicts then callback({ state = "conflicts", source_requests = 0, diff_pairs = 0 }) return end
    callback({ state = "ready", prompt = { system = "", user = compact_context } })
  elseif method == "github.creation.context" then
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
  if key == "git\tremote\tget-url\torigin" then
    callback({ code = 0, stdout = "fixture-origin\n", stderr = "" })
    return
  end
  if key:match("^git\tfetch\t") then
    callback({ code = fetch_failure and 1 or 0, stdout = "", stderr = fetch_failure or "" })
    return
  end
  if key == "git\trev-parse\t--verify\trefs/remotes/origin/main^{commit}"
    or key == "git\trev-parse\t--verify\trefs/remotes/origin/develop^{commit}" then
    callback({ code = 0, stdout = base_commit .. "\n", stderr = "" })
    return
  end
  if key == "git\tlog\t--reverse\t--format=%s%n%b%n---END-COMMIT---\t" .. base_commit .. ".." .. captured_head then
    callback({ code = 0, stdout = "feat: create draft pr\n---END-COMMIT---\n", stderr = "" })
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
  package.loaded["forge.integrations.background_settings"] = original_background
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
  wait_for(function() return generated_model == "test-background-model" end, "PR create did not use saved Background model")
  assert_true(generated_prompt:find(open_pr._pr_system_prompt_for_test, 1, true), "PR generation lost its response instructions")
  assert_true(
    generated_prompt:find("Compact diff context:", 1, true) ~= nil,
    "PR prompt should include compact diff context"
  )
  assert_true(
    generated_prompt:find(compact_context, 1, true) ~= nil,
    "PR prompt rewrote native compact context"
  )
  wait_for(function()
    return opened_pull_request ~= nil
  end, "created PR did not open in Forge PR view")
  assert_true(opened_pull_request.pr.title == "feat: create draft pr", "created PR view missing title")
  assert_true(opened_pull_request.options.cwd == root, "created PR view lost its repository root")
  assert_true(creation_capture and creation_capture.mutation.operation == "pull_request_create", "PR creation bypassed durable native capture")
  assert_true(context_calls == 1 and creation_capture.mutation.base == "main", "PR base differed from context selection")
  for _, call in ipairs(system_calls) do
    assert_true(not call.key:find("git\tdiff\t", 1, true), "PR still used the old raw diff path")
  end
  assert_true(
    not vim.tbl_contains(vim.tbl_map(function(call) return call.key end, system_calls), "gh\tpr\tview\t" .. created_pr_url .. "\t--web"),
    "create flow still opened the browser"
  )

  reset()
  open_pr._set_base_branch_for_test("develop", root)
  open_pr.open()
  wait_for(function() return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: develop") end,
    "selected develop base confirmation did not render")
  press("y")
  wait_for(function() return creation_capture ~= nil end, "selected-base PR was not created")
  assert_true(creation_capture.mutation.base == "develop", "PR did not use the selected base")
  assert_true(generated_prompt:find("Base branch: develop", 1, true), "context named a different base")

  reset()
  context_failure = "selected base has no merge base"
  open_pr.open()
  wait_for(function() return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: develop") end,
    "failure fixture base confirmation did not render")
  press("y")
  wait_for(function() return context_calls == 1 end, "failure fixture did not request native context")
  assert_true(generated_prompt == nil and creation_capture == nil, "failed context reached generation or PR creation")

  reset()
  generation_failure = "Harness provider unavailable"
  open_pr.open()
  wait_for(function() return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: develop") end,
    "generation failure confirmation missing")
  press("y")
  wait_for(function() return vim.tbl_contains(notices, "Failed to generate PR description: Harness provider unavailable") end,
    "Harness failure was not surfaced")
  assert_true(creation_capture == nil, "failed Harness generation reached PR creation")

  reset()
  fetch_failure = "origin unavailable"
  open_pr.open()
  wait_for(function() return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: develop") end,
    "fetch failure fixture base confirmation did not render")
  press("y")
  wait_for(function() return #notices > 1 end, "fetch failure was not surfaced")
  assert_true(context_calls == 0 and generated_prompt == nil and creation_capture == nil,
    "fetch failure silently reused a stale base")

  reset()
  context_conflicts = true
  open_pr.open()
  wait_for(function() return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: develop") end,
    "conflict fixture base confirmation did not render")
  press("y")
  wait_for(function() return context_calls == 1 end, "conflict fixture did not request the merge check")
  assert_true(generated_prompt == nil and creation_capture == nil, "conflict reached generation or PR creation")
  assert_true(vim.tbl_contains(notices, "Merge conflicts. Resolve them before creating the PR."),
    "conflict instruction was not displayed")
end

local ok, err = xpcall(run_tests, debug.traceback)
cleanup()
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("github_open_pr: ok")
vim.cmd("qa!")
