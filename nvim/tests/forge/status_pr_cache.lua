vim.opt.runtimepath:append("nvim")
local context = require("forge.views.status.status_context")
local gh = require("forge.integrations.gh")
require("forge.infra.config").options.about_auto_generate = false
local request = {}
gh.prs_for_branch_async = function(_, branch, _, callback)
  request[#request + 1] = { branch = branch, callback = callback }
end
gh.pr_async = function(_, number, _, callback)
  callback({ ok = true, pr = { number = number, title = "Cached title", repo = "owner/repo", snapshot = { title = "Cached title" } } })
end
local branch, shown, opened = "feature", nil, nil
require("forge.views.commands").open_pr = function(pr) opened = pr end
local function attach()
  return context.attach({ document_id = "cache-test", workspace = vim.fn.getcwd(),
    window = vim.api.nvim_get_current_win(), is_alive = function() return true end,
    present = function(value) shown = value.pr end,
    get_info = function() return { branch = branch } end,
    capture_input = function() return {} end, is_input_current = function() return true end })
end
local first = attach()
first.refresh()
assert(shown.state == "fetching")
request[1].callback({ ok = true, prs = { { number = 7, state = "OPEN" } } })
assert(shown.text == "Cached title")
first.close()
local second = attach()
second.refresh()
assert(shown.text == "Cached title" and shown.state == "ready", "refresh hid cached title")
second.open_pull_request(vim.api.nvim_get_current_win(), false)
assert(opened and opened.snapshot.title == "Cached title", "open waited for refresh or discarded the snapshot")
request[2].callback({ ok = false })
assert(shown.text == "Cached title", "failed refresh discarded cached title")
branch = "another-branch"
second.refresh()
assert(shown.state == "fetching" and shown.text == "", "branch change reused a foreign PR")
request[3].callback({ ok = true, prs = {} })
assert(shown.state == "none")
branch = "feature"
second.refresh()
assert(shown.text == "Cached title")
request[4].callback({ ok = true, prs = {} })
second.refresh()
assert(shown.state == "none" and shown.text == "", "confirmed absence retained the old PR")
second.close()
print("status_pr_cache: cached opening, background refresh, failure retention, branch isolation and invalidation passed")
