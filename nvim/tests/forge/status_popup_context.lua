vim.opt.runtimepath:prepend("nvim")
vim.ui.select = function() error("Status context opened a Snacks picker") end
vim.ui.input = function() error("Status context opened a Snacks input") end
local config = require("forge.infra.config")
config.options.about_auto_generate = false
config.options.pr_lookup_mode = "live"
local creation, written = 0, {}
package.loaded["github.open_pr"] = { open = function() creation = creation + 1 end }
package.loaded["forge.git.write"] = { execute = function(_, action, callback)
  written[#written + 1] = action
  callback({ ok = true })
end }
local response = { ok = true }
local gh = require("forge.integrations.gh")
gh.prs_for_branch_async = function(_, _, _, callback) callback({ ok = true, prs = response.pr and { response.pr } or {} }) end
gh.pr_async = function(_, _, _, callback) callback(response) end
local context = require("forge.views.status.status_context")
local origin = vim.api.nvim_get_current_win()
local owner = context.attach({ document_id = "popup-context", workspace = vim.fn.getcwd(),
  window = origin, is_alive = function() return true end, present = function() end,
  capture_input = function() return {} end, is_input_current = function() return true end,
  get_info = function() return { branch = "main" } end,
})
local function press(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), "xt", false)
end
owner.refresh()
owner.open_pull_request(origin)
assert(vim.bo.filetype == "ForgeConfirm")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), {
  "No GitHub PR found for this branch.", "", "Create a draft PR now?", "", "  [y] yes    [n] no",
}))
press("n")
assert(creation == 0 and vim.api.nvim_get_current_win() == origin)
owner.open_pull_request(origin)
press("y")
assert(creation == 1)
response = { ok = true, pr = { number = 7, title = "closed", state = "CLOSED" } }
owner.refresh()
owner.open_pull_request(origin)
assert(vim.bo.filetype == "ForgeChoicePopup")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), {
  "", "  [o]  Open closed PR #7", "  [c]  Create a new draft PR", "  [q]  cancel", "",
}))
press("q")
assert(creation == 1)
owner.open_pull_request(origin)
press("c")
assert(creation == 2 and vim.api.nvim_get_current_win() == origin, "closed-PR create added a second confirmation")
local handler = context.producer_handlers({ workspace = vim.fn.getcwd(), window = origin,
  is_alive = function() return true end, refresh_status = function() end, context_info = { branch_prefix = "feature/" } })
handler.branch_create()
assert(vim.bo.filetype == "ForgeBranchPrompt")
press("q")
assert(#written == 0)
handler.branch_create()
vim.api.nvim_buf_set_lines(0, 0, -1, false, { "feature/parity" })
press("<CR>")
assert(#written == 1 and written[1].kind == "create_branch" and written[1].name == "feature/parity")
assert(vim.api.nvim_get_current_win() == origin)
owner.close()
print("missing/closed PR menus and branch command popup parity passed")
vim.cmd("qa!")
