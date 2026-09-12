local config = vim.fn.getcwd()
vim.opt.runtimepath:prepend(config .. "/nvim")
if vim.g.forge_manual_artifact_root then
  require("forge.builder")._set_artifact_root_for_test(vim.g.forge_manual_artifact_root)
end
local fixture = vim.fn.tempname() .. "-forge-commit-reuse"
vim.fn.mkdir(fixture, "p")
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
  return result.stdout
end
git({ "init", "--quiet" })
git({ "config", "user.name", "Forge Fixture" })
git({ "config", "user.email", "forge@example.invalid" })
git({ "config", "core.autocrlf", "false" })
vim.fn.writefile({ "ignored.txt" }, fixture .. "/.gitignore")
vim.fn.writefile({ "original renamed content", "retained second line" }, fixture .. "/old-name.txt")
vim.fn.writefile({ "before" }, fixture .. "/tracked.txt")
vim.fn.writefile({ "removed" }, fixture .. "/deleted.txt")
git({ "add", "." })
git({ "commit", "--quiet", "-m", "initial" })
assert(vim.uv.fs_rename(fixture .. "/old-name.txt", fixture .. "/new-name.txt"))
vim.fn.writefile({ "after" }, fixture .. "/tracked.txt")
vim.fn.delete(fixture .. "/deleted.txt")
vim.fn.writefile({ "new content" }, fixture .. "/untracked.txt")
vim.fn.writefile({ "ignored content" }, fixture .. "/ignored.txt")
vim.fn.chdir(fixture)
_G.forge_reuse = { fixture = fixture, model_calls = 0, message = "refactor: rename source and update fixtures" }
package.loaded.ai = { resolve = function() return { provider = { name = "fixture" }, model = "fixture-model" } end }
package.loaded["ai.adapters"] = { get = function() return { commit = "fixture-model" } end }
local client = require("forge.client")
local ai_commit = require("forge.integrations.ai_commit")
ai_commit.set_backend({ request_async = function(params, callback)
  local native = vim.deepcopy(params)
  client.request_host("repository.generate", native, function(result, failure)
    if failure == "unsupported detached generation provider fixture" then
      forge_reuse.model_calls = forge_reuse.model_calls + 1
      result = { state = "ready", message = params.comparison == "staged" and "refactor: describe staged fixture" or forge_reuse.message }
      failure = nil
    end
    if vim.g.forge_manual_hold_regeneration and params.comparison == "staged" then
      forge_reuse.release_regeneration = function() callback(result, failure) end
    else callback(result, failure) end
  end)
end })
local commit = require("forge.integrations.commit")
local editor = commit.editor
commit.editor = function(target, address)
  forge_reuse.editor_address = address
  editor(target, address)
end
local gh = require("forge.integrations.gh")
gh.prs_for_branch_async = function(_, _, _, callback)
  vim.schedule(function() callback({ ok = true, prs = {} }) end)
end
gh.current_pr_async = function(_, callback)
  vim.schedule(function() callback({ ok = true }) end)
end
require("forge").setup()
forge_reuse.state = require("forge.views.commands").open()
vim.api.nvim_create_user_command("ForgeReuseAudit", function()
  assert(forge_reuse.model_calls == 1, "duplicate generation")
  assert(vim.b.forge_commit_buffer, "not in commit buffer")
  assert(vim.api.nvim_get_current_line() == forge_reuse.message, "draft was not populated")
  print("About message reused, model calls = 1")
end, {})
