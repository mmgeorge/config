vim.opt.runtimepath:append("nvim")

-- Public PR entry points construct one native review-document identity. The overview
-- route loads the default sections, while the review route enters batched mode only
-- after the native document owns the window.
local commands = require("forge.views.commands")
local gh = require("forge.integrations.gh")
local review_document = require("forge.review_document")

local original_pr_async = gh.pr_async
local original_open = review_document.open
local original_begin_batched = review_document.begin_batched
local requested, opened, batched = nil, {}, {}

gh.pr_async = function(cwd, number, repo, callback)
  requested = { cwd = cwd, number = number, repo = repo }
  callback({
    ok = true,
    pr = { number = tonumber(number), repo = repo, title = "native parity boundary" },
  })
end

review_document.open = function(options)
  opened[#opened + 1] = vim.deepcopy(options)
  local state = { active = true, document = "native-review-" .. #opened }
  if options.on_open then vim.schedule(function() options.on_open(state) end) end
  return state
end

review_document.begin_batched = function(state)
  batched[#batched + 1] = state.document
  return true
end

local ok, failure = xpcall(function()
  commands.open_pr_number("17", { cwd = "D:/review-workspace", repo = "owner/repo" })
  assert(vim.deep_equal(requested, { cwd = "D:/review-workspace", number = "17", repo = "owner/repo" }))
  assert(#opened == 1, "public PR command did not open the native overview after discovery")
  assert(vim.deep_equal(opened[1], {
    directory = "D:/review-workspace",
    repository = { hostname = require("github.repo_cache").hostname(), owner = "owner", name = "repo" },
    number = 17,
  }))
  assert(#batched == 0, "overview unexpectedly entered batched review mode")

  local state = commands.open_review({ number = 18, repo = "owner/repo" }, { cwd = "D:/review-workspace" })
  assert(state and state.document == "native-review-2", "public review did not return the native owner")
  assert(vim.wait(1000, function() return vim.deep_equal(batched, { "native-review-2" }) end),
    "public review did not enter batched mode after native ownership")
end, debug.traceback)

gh.pr_async = original_pr_async
review_document.open = original_open
review_document.begin_batched = original_begin_batched

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  print("pr_entry_cutover: public PR overview and review use the native document owner")
  vim.cmd("qa!")
end
