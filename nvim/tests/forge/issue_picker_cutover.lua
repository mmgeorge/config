vim.opt.runtimepath:append("nvim")

local cache = require("github.repo_cache")
local gh = require("github.gh")
local issue_index = require("github.issue_index")
local captured = {}
local current_cwd = vim.fn.getcwd()
local original = {
  hostname = cache.hostname,
  completion_repo = cache.completion_repo,
  ensure_repo = issue_index.ensure_repo,
  list = issue_index.list,
  prefetch_details = issue_index.prefetch_details,
  current_repo_async = gh.current_repo_async,
  create_issue_async = gh.create_issue_async,
  open_url = gh.open_url,
  issue_document = package.loaded["github.issue_document"],
  snacks = _G.Snacks,
  notify = vim.notify,
}

local function capture(options)
  captured[#captured + 1] = vim.deepcopy(options)
end

package.loaded["github.issue_document"] = { open = capture }
vim.notify = function() end
cache.hostname = function() return "enterprise.example" end
cache.completion_repo = function() return "cache-owner/cache-repo" end
issue_index.ensure_repo = function() end
issue_index.list = function()
  return { { kind = "issue", repo = "picker-owner/picker-repo", number = 19, title = "Picker", comments_count = 0 } }
end
issue_index.prefetch_details = function() end
_G.Snacks = { picker = { pick = function(options)
  options.confirm({ close = function() end }, options.items[1])
end } }

local pickers = require("github.pickers")
local issue_create = require("github.issue_create")

pickers.issues("explicit-owner/explicit-repo 17")
assert(#captured == 1, "explicit issue target did not use the native adapter")
assert(captured[1].repository.hostname == "enterprise.example")
assert(captured[1].repository.owner == "explicit-owner" and captured[1].repository.name == "explicit-repo")
assert(captured[1].number == 17 and captured[1].cwd == current_cwd)

pickers.issues()
assert(#captured == 2, "synced issue picker did not use the native adapter")
assert(captured[2].repository.hostname == "enterprise.example")
assert(captured[2].repository.owner == "picker-owner" and captured[2].repository.name == "picker-repo")
assert(captured[2].number == 19 and captured[2].cwd == current_cwd)

cache.completion_repo = function() return nil end
gh.current_repo_async = function(cwd, callback)
  assert(cwd == current_cwd, "current-repository lookup changed the picker cwd")
  callback({ ok = true, repo = "resolved-owner/resolved-repo" })
end
pickers.issues("23")
assert(#captured == 3, "current repository issue target did not use the native adapter")
assert(captured[3].repository.hostname == "enterprise.example")
assert(captured[3].repository.owner == "resolved-owner" and captured[3].repository.name == "resolved-repo")
assert(captured[3].number == 23 and captured[3].cwd == current_cwd)

gh.create_issue_async = function(cwd, title, body, _, callback)
  assert(cwd == current_cwd, "issue creation changed the request cwd")
  assert(title == "Created issue" and body == "", "issue creation changed the submitted fields")
  callback({ ok = true, url = "https://Enterprise.Example/created-owner/created-repo/issues/29" })
end
issue_create.open("Created issue")
assert(#captured == 4, "created issue did not use the native adapter")
assert(captured[4].repository.hostname == "enterprise.example")
assert(captured[4].repository.owner == "created-owner" and captured[4].repository.name == "created-repo")
assert(captured[4].number == 29 and captured[4].cwd == current_cwd)

local opened_url
gh.open_url = function(url) opened_url = url end
gh.create_issue_async = function(_, _, _, _, callback)
  callback({ ok = true, url = "invalid issue URL" })
end
issue_create.open("Browser fallback")
assert(opened_url == "invalid issue URL", "unparseable create response did not retain the browser fallback")

cache.hostname = original.hostname
cache.completion_repo = original.completion_repo
issue_index.ensure_repo = original.ensure_repo
issue_index.list = original.list
issue_index.prefetch_details = original.prefetch_details
gh.current_repo_async = original.current_repo_async
gh.create_issue_async = original.create_issue_async
gh.open_url = original.open_url
package.loaded["github.issue_document"] = original.issue_document
_G.Snacks = original.snacks
vim.notify = original.notify

print("issue_picker_cutover: picker and created issues retain hostname, repository, number, and cwd through the native adapter")
