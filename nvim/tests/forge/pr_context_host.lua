vim.loader.enable(false)
local fixture = vim.fn.tempname() .. "-pr-context"
assert(vim.fn.mkdir(fixture, "p") == 1)
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return vim.trim(result.stdout)
end
local client = require("forge.client")
local notices, generated, created, published, opened = {}, nil, nil, nil, nil
local original_notify = vim.notify
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
local success, failure = xpcall(function()
  git({ "init", "--quiet", "--initial-branch=main" })
  git({ "config", "user.name", "Forge Fixture" })
  git({ "config", "user.email", "forge@example.invalid" })
  git({ "config", "core.autocrlf", "false" })
  vim.fn.writefile({ "fn original() {}" }, fixture .. "/original.rs")
  vim.fn.writefile({ "original" }, fixture .. "/shared.txt")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "initial" })
  git({ "checkout", "--quiet", "-b", "feature" })
  vim.fn.writefile({ "fn feature_marker() { hidden_body(); }" }, fixture .. "/feature.rs")
  git({ "mv", "original.rs", "renamed.rs" })
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "feat: introduce feature" })
  local head = git({ "rev-parse", "HEAD" })
  git({ "checkout", "--quiet", "main" })
  vim.fn.writefile({ "fn main_only_marker() {}" }, fixture .. "/main_only.rs")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "advance main" })
  git({ "checkout", "--quiet", "-b", "release" })
  vim.fn.writefile({ "fn release_only_marker() {}" }, fixture .. "/release_only.rs")
  git({ "add", "." })
  git({ "commit", "--quiet", "-m", "advance release" })
  git({ "update-ref", "refs/remotes/origin/release", "HEAD" })
  git({ "remote", "add", "origin", fixture })
  git({ "checkout", "--quiet", "feature" })
  vim.fn.writefile({ "fn uncommitted_marker() {}" }, fixture .. "/uncommitted.rs")
  git({ "add", "uncommitted.rs" })

  package.loaded["ai"] = { generate = function(options, callback)
    generated = options
    callback({ ok = true, content = "Introduces the feature." })
  end }
  package.loaded["ai.adapters"] = { get = function() return { pr_create = "fixture" } end }
  local creation = require("github.pr_creation")
  creation.context = function(_, callback) callback({ branch = "feature", head_commit = head }) end
  creation.create = function(_, context, base, metadata, callback)
    created = { context = context, base = base, metadata = metadata }
    callback({ html_url = "https://example.invalid/pull/1" })
  end
  require("forge.git.write").execute = function(_, action, callback)
    published = action
    callback({ ok = true, output = "" })
  end
  require("forge.integrations.gh").pr_async = function(_, _, _, callback)
    callback({ ok = true, pr = { number = 1 } })
  end
  require("forge").open_pr = function(value) opened = value end
  local open_pr = require("github.open_pr")
  open_pr._set_state_path_for_test(fixture .. "/selection.json")
  open_pr._set_base_branch_for_test("release", fixture)
  open_pr.open({ cwd = fixture })
  assert(vim.wait(10000, function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: release")
  end, 10), "base confirmation missing")
  vim.api.nvim_feedkeys("y", "x", false)
  assert(vim.wait(15000, function() return opened ~= nil or #notices > 0 end, 10), "PR context did not complete")
  assert(#notices == 0, table.concat(notices, "\n"))
  assert(created and created.base == "release" and created.metadata.title == "feat: introduce feature")
  assert(published.head == head and created.context.head_commit == head)
  assert(generated.prompt:find("Base branch: release", 1, true))
  assert(generated.prompt:find("+ fn feature_marker() {", 1, true), generated.prompt)
  assert(generated.prompt:find("renamed.rs (Renamed from original.rs)", 1, true), generated.prompt)
  for _, excluded in ipairs({ "main_only", "release_only", "uncommitted", "hidden_body", "conventional commit" }) do
    assert(not generated.prompt:find(excluded, 1, true), "unexpected context: " .. excluded)
  end
  assert(#generated.system + #generated.prompt <= 500000)
  assert(not client._client.harness_ready, "PR context opened a Harness conversation")

  git({ "checkout", "--quiet", "release" })
  vim.fn.writefile({ "base change" }, fixture .. "/shared.txt")
  git({ "add", "shared.txt" })
  git({ "commit", "--quiet", "-m", "change shared base", "--only", "--", "shared.txt" })
  git({ "checkout", "--quiet", "feature" })
  vim.fn.writefile({ "head change" }, fixture .. "/shared.txt")
  git({ "add", "shared.txt" })
  git({ "commit", "--quiet", "-m", "change shared head", "--only", "--", "shared.txt" })
  head = git({ "rev-parse", "HEAD" })
  local status_before = git({ "status", "--porcelain=v2" })
  local index_before = git({ "diff", "--cached", "--binary" })
  local worktree_before = git({ "diff", "--binary" })
  generated, created, published, opened = nil, nil, nil, nil
  notices = {}
  local context_response
  local request_host = client.request_host
  client.request_host = function(method, params, callback, ...)
    return request_host(method, params, function(response, request_failure)
      if method == "repository.generate" then context_response = response end
      callback(response, request_failure)
    end, ...)
  end
  open_pr.open({ cwd = fixture })
  assert(vim.wait(10000, function()
    return vim.tbl_contains(vim.api.nvim_buf_get_lines(0, 0, -1, false), "Base: release")
  end, 10))
  vim.api.nvim_feedkeys("y", "x", false)
  assert(vim.wait(15000, function() return #notices > 0 end, 10), "merge conflict was not reported")
  assert(notices[1] == "Merge conflicts. Resolve them before creating the PR.", table.concat(notices, "\n"))
  assert(context_response.state == "conflicts" and context_response.prompt == nil)
  assert(context_response.source_requests == 0 and context_response.diff_pairs == 0)
  assert(not generated and not created and not published and not opened, "conflict did not stop PR preparation")
  assert(git({ "rev-parse", "HEAD" }) == head, "conflict check moved HEAD")
  assert(git({ "status", "--porcelain=v2" }) == status_before, "conflict check changed repository state")
  assert(git({ "diff", "--cached", "--binary" }) == index_before, "conflict check changed the index")
  assert(git({ "diff", "--binary" }) == worktree_before, "conflict check changed the worktree")
  client.request_host = request_host
end, debug.traceback)
client.stop()
assert(vim.wait(10000, function() return client._client.process == nil end, 20))
vim.notify = original_notify
vim.fn.delete(fixture, "rf")
assert(success, failure)
print("PR creation: selected base, compact context, and conflict rejection before generation/publication passed")
