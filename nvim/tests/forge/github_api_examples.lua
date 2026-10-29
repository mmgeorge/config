vim.loader.enable(false)
local root, directory = vim.fn.getcwd(), vim.fn.tempname()
local workspace, data, executable_directory = directory .. "/workspace", directory .. "/data", directory .. "/bin"
for _, path in ipairs({ workspace, data, executable_directory }) do assert(vim.fn.mkdir(path, "p") == 1) end
local fixture = executable_directory .. (vim.fn.has("win32") == 1 and "/gh-base.exe" or "/gh-base")
local compiled = vim.system({ "rustup", "run", "1.94.0", "rustc", "--edition=2024",
  root .. "/nvim/rust/forge/crates/forge-github/tests/fixtures/gh.rs", "-o", fixture },
  { text = true, timeout = 300000 }):wait()
assert(compiled.code == 0, compiled.stderr)
compiled = vim.system({ "rustup", "run", "1.94.0", "rustc", "--edition=2024", "-D", "warnings",
  root .. "/nvim/tests/forge/support/github_api_fixture.rs", "-o",
  executable_directory .. (vim.fn.has("win32") == 1 and "/gh.exe" or "/gh") },
  { text = true, timeout = 300000 }):wait()
assert(compiled.code == 0, compiled.stderr)
vim.fn.writefile({ "enterprise.example" }, workspace .. "/host", "b")
vim.fn.writefile({ "pr_success" }, workspace .. "/mode", "b")
local executable = require("forge.builder").binary_path()
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  callback({ ok = true, path = executable })
  return function() end
end }
vim.fn.chdir(workspace)
require("forge").setup({ harness_logging = false, harness = { backend = "mock", config_dir = data } })
local client = require("forge.client")
client._host_set_launcher_for_test(function(command, options, callback)
  options.env = vim.tbl_extend("force", options.env or {}, { PATH = executable_directory })
  return vim.system(command, options, callback)
end)
local function request(method, params)
  local result, failure, completed
  client.request({ method = method, params = params }, function(value, problem)
    result, failure, completed = value, problem, true
  end)
  assert(vim.wait(10000, function() return completed end, 5), method .. " timed out")
  assert(not failure, method .. ": " .. tostring(failure))
  return result
end
local success, failure = xpcall(function()
  local initialized, problem
  client.session_initialize(function(_, error) initialized, problem = true, error end, { services_only = true })
  assert(vim.wait(10000, function() return initialized end, 5), "shared initialization timed out")
  assert(not problem, problem)
  local repository = { hostname = "enterprise.example", owner = "Owner", name = "Repo" }
  local target = { repository = repository, number = 7, node_id = "PR_fixture" }
  local resource = { repository = repository, number = 7, kind = "pull_request" }
  assert(request("github.actor.get", { directory = workspace, repository = repository }).login == "fixture")
  local opened = request("github.review.document.open", { directory = workspace, target = target })
  local document = opened.document
  local initial = {
    id = "PR_fixture", number = 7, title = "Fixture pull request", body = "Fixture description",
    url = "https://enterprise.example/Owner/Repo/pull/7", state = "CLOSED", isDraft = false,
    baseRefOid = string.rep("a", 40), baseRefName = "main",
    headRefOid = string.rep("b", 40), headRefName = "feature",
    headRepository = { name = "Repo", nameWithOwner = "Owner/Repo" }, headRepositoryOwner = { login = "Owner" },
    commits = {}, reviewRequests = {},
  }
  assert(request("github.review.document.header.load", { document = document, directory = workspace, initial = initial }).ready)
  assert(type(request("github.review.document.view.set", { document = document, view = "example-review", width = { columns = 80, tabstop = 4 } })) == "table")
  assert(type(request("github.review.document.batch.begin", { document = document })) == "table")
  local draft = request("github.review.draft.get", { resource = resource })
  assert(request("github.review.draft.save", { resource = resource, draft = draft }).saved)
  local viewed = request("github.review.document.file.viewed.set", { document = document, path = "src/lib.rs", viewed = true })
  assert(viewed.viewed_file[1] == "src/lib.rs")
  local transitioned = request("github.review.document.transition", { document = document, desired = "OPEN" })
  assert(transitioned.lifecycle.state == "OPEN")
  assert(request("github.review.document.lifecycle.reconcile", { document = document }).lifecycle.state == "OPEN")
  assert(request("github.pull_request", { directory = workspace, request = { target = target, request = { operation = "reconcile" } } }).state == "OPEN")
  vim.fn.writefile({ "pr_uncertain" }, workspace .. "/mode", "b")
  request("github.review.document.transition", { document = document, desired = "CLOSED" })
  local recovery = request("github.recovery.inspect", { resource = resource })
  assert(recovery.state.phase == "outcome_unknown")
  local resolved = request("github.review.document.lifecycle.resolve", { document = document,
    operation_id = recovery.capture.operation_id, resolution = { resolution = "close_unknown" } })
  assert(resolved.fresh_required)
  assert(request("github.recovery.acknowledge", { resource = resource, operation_id = recovery.capture.operation_id }).acknowledged)
  request("github.review.document.transition", { document = document, desired = "OPEN" })
  recovery = request("github.recovery.inspect", { resource = resource })
  local settled = request("github.recovery.resolve", { directory = workspace, resource = resource,
    operation_id = recovery.capture.operation_id, resolution = { resolution = "close_unknown" } })
  assert(settled.state.phase == "user_closed_unknown")
  request("github.recovery.acknowledge", { resource = resource, operation_id = recovery.capture.operation_id })
  local function fixture_json(name, value)
    vim.fn.writefile({ vim.json.encode(value) }, workspace .. "/" .. name, "b")
  end
  local base, head = string.rep("a", 40), string.rep("b", 40)
  local old_source = string.rep("pub fn before() {}\n", 160)
  local new_source = string.rep("pub fn after() {}\n", 160)
  vim.fn.writefile(vim.split(old_source, "\n", { plain = true }), workspace .. "/base.rs", "b")
  vim.fn.writefile(vim.split(new_source, "\n", { plain = true }), workspace .. "/head.rs", "b")
  fixture_json("comparison.json", { base_commit = { sha = base }, merge_base_commit = { sha = base } })
  for _, item in ipairs({ { "base", base, string.rep("d", 40), old_source }, { "head", head, string.rep("e", 40), new_source } }) do
    fixture_json("source-" .. item[1] .. ".json", { data = { repository = {
      commit = { __typename = "Commit", oid = item[2] }, source = { __typename = "Blob", oid = item[3], byteSize = #item[4] },
    } } })
  end
  fixture_json("files.json", { { filename = "src/lib.rs", status = "modified", sha = string.rep("e", 40), additions = 1, deletions = 1, changes = 2 } })
  fixture_json("commits.json", {})
  fixture_json("comments.json", {})
  local page = { hasNextPage = false, endCursor = vim.NIL }
  local comment = { id = "COMMENT_fixture", databaseId = 1, body = "Review fixture", viewerDidAuthor = false,
    url = "https://enterprise.example/owner/repo/pull/7#discussion_r1", author = { login = "fixture" } }
  fixture_json("checks.json", { data = { repository = { pullRequest = { commits = { nodes = { {
    commit = { statusCheckRollup = vim.NIL },
  } } } } } } })
  fixture_json("threads.json", { data = { repository = { pullRequest = { reviewThreads = {
    nodes = { { id = "THREAD_fixture", isResolved = false, isOutdated = false, path = "src/lib.rs", line = 1,
      diffSide = "RIGHT", viewerCanReply = true, viewerCanResolve = true, viewerCanUnresolve = false,
      comments = { nodes = { comment }, totalCount = 1, pageInfo = page } } }, pageInfo = page,
  } } } } })
  fixture_json("thread.json", { data = { node = { pullRequest = { number = 7, repository = { nameWithOwner = "owner/repo" } },
    comments = { nodes = { comment }, pageInfo = page } } } })
  fixture_json("actor.json", { login = "fixture", node_id = "ACTOR_fixture" })
  fixture_json("overview.json", { number = 7, node_id = "PR_fixture", state = "open", draft = false,
    head = { sha = head }, base = { sha = base } })
  vim.fn.writefile({ "review_api" }, workspace .. "/mode", "b")
  local loaded = request("github.review.document.load", { document = document, directory = workspace })
  assert(#loaded.diagnostic == 0, vim.inspect(loaded))
  request("github.review.document.section.get", { document = document, directory = workspace, section = "threads" })
  request("github.review.document.render", { document = document, width = { columns = 80, tabstop = 4 } })
  local thread = request("github.review.document.thread.get", { document = document, directory = workspace, thread_node_id = "THREAD_fixture" })
  assert(thread.thread.loaded_comments == 1)
  assert(type(request("github.review.document.file.get", { document = document, directory = workspace, path = "src/lib.rs" })) == "table")
  assert(type(request("github.review.document.file.page.get", { document = document, path = "src/lib.rs" })) == "table")
  local rendered = request("github.review.document.render", { document = document, width = { columns = 80, tabstop = 4 } })
  local input
  for _, block in ipairs(rendered.snapshot.block) do
    local target_entry = block.metadata.target[1]
    if target_entry then
      input = { document = document, view = "example-review", revision = rendered.snapshot.revision, sequence = 1,
        block = block.id, target = target_entry.id, position = target_entry.range.start, action = "activate" }
      break
    end
  end
  assert(input, "review fixture has no actionable target")
  assert(type(request("github.review.document.input.apply", { input = input, directory = workspace })) == "table")
  local context = request("github.review.submission.context", {
    directory = workspace, repository = repository, number = 7, expected = {},
  })
  assert(context.commit_id == head)
  local inline = request("github.review.document.comment", { document = document,
    command = { operation = "draft_inline", anchor = { revision = head, path = "src/lib.rs", side = "right", first_line = 1, last_line = 1 } } })
  local submission = request("github.review.document.batch.submit", { document = document, verdict = "comment", capture = {
    { document = document, region = inline.snapshot.region, base = inline.snapshot.revision,
      sequence = inline.snapshot.sequence + 1, text = "Review fixture" },
  } })
  local submission_recovery = request("github.recovery.inspect", { resource = resource })
  assert(submission_recovery.state.phase == "outcome_unknown", vim.inspect(submission))
  assert(request("github.review.document.batch.recover", { document = document,
    operation_id = submission_recovery.capture.operation_id, resolution = { resolution = "close_unknown" } }).fresh_required)
  assert(request("github.review.document.close", { document = document }).closed)
  vim.fn.writefile({ "success" }, workspace .. "/mode", "b")
  vim.fn.writefile({ "[[]]" }, workspace .. "/contributors.json", "b")
  vim.fn.writefile({ "[[]]" }, workspace .. "/collaborators.json", "b")
  local metadata = request("github.repository.metadata", { directory = workspace, cache_directory = data .. "/enterprise.example/repos/owner/repo",
    request = { repository = repository, ttl_seconds = 600 } })
  assert(metadata.hostname == "enterprise.example")
  assert(vim.uv.fs_copyfile(root .. "/nvim/rust/forge/crates/forge-github/tests/fixtures/detail.json", workspace .. "/response.json"))
  local issue = request("github.issue.get", { directory = workspace, database = data .. "/issues.redb",
    request = { repository = repository, number = 7 } })
  assert(issue.item.title == "Unicode é issue")
end, debug.traceback)
client.host_stop()
local stopped = vim.wait(6000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and stopped, failure or "GitHub API fixture host did not stop")
print("github_api_examples: native review lifecycle and recovery passed")
