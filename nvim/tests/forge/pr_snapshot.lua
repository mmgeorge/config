vim.opt.runtimepath:append("nvim")
local gh = require("forge.integrations.gh")
local raw = { id = "PR_test", number = 7, title = "Title", body = "Paragraph\r\n\r\n```ts\r\nconst value = 1;\r\n```",
  url = "https://github.com/owner/repo/pull/7", headRefName = "feature", headRefOid = string.rep("b", 40),
  baseRefOid = string.rep("a", 40), baseRefName = "main", headRepository = { name = "repo" },
  headRepositoryOwner = { login = "fork" }, reviewRequests = {}, commits = {}, state = "OPEN" }
gh.set_backend({ system_async = function(command, _, callback)
  assert(table.concat(command, " "):find("baseRefOid", 1, true), "shared lookup omitted comparison identity")
  callback({ code = 0, stdout = vim.json.encode(raw), output = "" })
end })
local result
gh.pr_async(vim.fn.getcwd(), 7, "owner/repo", function(value) result = value end)
assert(result.ok and result.pr.snapshot.body == raw.body, "snapshot normalized editable Markdown source")
local opened
require("forge.review_document").open = function(options) opened = options end
require("forge.views.commands").open_pr(result.pr, { cwd = vim.fn.getcwd() })
assert(opened.number == 7 and vim.deep_equal(opened.initial, raw), "PR command discarded the shared snapshot")
raw.reviewRequests, raw.commits = nil, nil
gh.pr_async(vim.fn.getcwd(), 7, "owner/repo", function(value) result = value end)
assert(result.pr.snapshot == nil, "partial lookup was treated as a complete PR snapshot")
gh.reset_backend()
print("pr_snapshot: shared lookup, command handoff, raw Markdown and incomplete-response fallback passed")
