vim.opt.rtp:append("D:/config/nvim")

local fixture = "D:/config/nvim/rust/forge/target/notification-fixture.json"
assert(vim.fn.filereadable(fixture) == 1, "run forge-review notification native test to generate snapshot fixture")

local original_forge = package.loaded["forge"]
local original_issue_document = package.loaded["github.issue_document"]
local original_ui_open = vim.ui.open
local pr_open, issue_open, browsed_url

package.loaded["forge"] = {
  open_pr_number = function(number, options)
    pr_open = { number = number, options = options }
  end,
}
package.loaded["github.issue_document"] = {
  open = function(options)
    issue_open = options
  end,
}
vim.ui.open = function(url)
  browsed_url = url
end

local snapshot = vim.json.decode(table.concat(vim.fn.readfile(fixture), "\n"))
local notifications = require("forge.notifications")
local workspace = "D:/work/notifications"
local state = notifications.open({
  workspace = workspace,
  request = function(params, callback)
    if params.operation == "open" then
      local opened = vim.deepcopy(snapshot)
      opened.document = params.document
      callback({ snapshot = opened, more = false })
    elseif params.operation == "close" then
      callback(true)
    elseif params.operation == "act" then
      callback({ more = false })
    else
      callback(nil)
    end
  end,
  on_error = function(message)
    error(message)
  end,
})

assert(vim.wait(1000, function() return not state.pending and #state.queue == 0 end, 5), "notification document did not open")
state.open_effect({ kind = "open", subject_kind = "PullRequest", number = 42, repository = "acme/widgets" }, vim.api.nvim_get_current_win(), function() return true end)
assert(pr_open and pr_open.number == 42, "notification PR did not open the public PR overview")
assert(pr_open.options.repo == "acme/widgets", "notification PR did not preserve repository identity")
assert(pr_open.options.cwd == workspace, "notification PR did not preserve workspace identity")

state.open_effect({ kind = "open", subject_kind = "Issue", number = 7, repository = "acme/widgets" }, vim.api.nvim_get_current_win(), function() return true end)
assert(issue_open and issue_open.kind == "issue", "notification Issue did not remain native")
assert(issue_open.number == 7 and issue_open.repository == "acme/widgets" and issue_open.cwd == workspace,
  "notification Issue did not preserve its explicit identity")

state.open_effect({ kind = "browse", url = "https://github.example/acme/widgets/pull/42" }, vim.api.nvim_get_current_win(), function() return true end)
assert(browsed_url == "https://github.example/acme/widgets/pull/42", "notification browse fallback changed")

notifications.close(state)
package.loaded["forge"] = original_forge
package.loaded["github.issue_document"] = original_issue_document
vim.ui.open = original_ui_open
print("notification subject opening passed")
vim.cmd("qa!")
