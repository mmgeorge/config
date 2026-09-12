vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local commands = require("forge.document_commands")
local buffer = require("forge.buffer")
local config = require("forge.infra.config")
vim.o.columns = 160
config.setup({ keymaps = { status = { stage = "gs", ignore = false, open = { "go", "gO" } } } })
local source = vim.api.nvim_get_current_buf()
local window = vim.api.nvim_get_current_win()
vim.wo[window].winbar = "original header"
local replica = buffer.open("winbar-status")
vim.api.nvim_win_set_buf(window, replica.buffer)
local handler = {}
for _, id in ipairs({ "stage", "unstage", "ignore", "discard", "commit", "open", "refresh", "close" }) do handler[id] = function() end end
local owner = commands.attach(replica, { view = "status", title = "Status 100%", handler = handler })
local function text(target)
  return vim.api.nvim_eval_statusline(vim.wo[target].winbar, {
    winid = target, use_winbar = true, maxwidth = vim.api.nvim_win_get_width(target),
  }).str
end
local hint = text(window)
assert(hint:find("Status 100%", 1, true), "hint title did not escape statusline percent syntax")
assert(hint:find("gs stage", 1, true) and hint:find("go open", 1, true))
assert(not hint:find("ignore", 1, true) and not hint:find("gO open", 1, true))
assert(hint:find("cc commit", 1, true) and hint:find("? help", 1, true))
assert(not hint:find("push", 1, true), "hint exposed a command without a handler")
vim.api.nvim_win_set_buf(window, source)
assert(vim.wo[window].winbar == "original header", "hint leaked into ordinary buffer")
vim.api.nvim_win_set_buf(window, replica.buffer)
assert(text(window):find("gs stage", 1, true), "hint did not return on redisplay")
vim.cmd("vsplit")
local split = vim.api.nvim_get_current_win()
vim.api.nvim_win_set_width(split, 28)
vim.api.nvim_exec_autocmds("WinResized", {})
assert(text(split):find("? help", 1, true), "split lost compact help hint")
vim.api.nvim_win_set_buf(split, source)
assert(vim.wo[split].winbar == "original header", "split restored an inherited Forge hint")
vim.api.nvim_win_close(split, true)
vim.wo[window].winbar = "new owner header"
owner.close()
assert(vim.wo[window].winbar == "new owner header", "close replaced another owner's header")
local explicit = commands.attach(replica, { view = "diff", winbar = false, handler = { close = function() end } })
assert(vim.wo[window].winbar == "new owner header", "source header opt-out was ignored")
explicit.close()
vim.wo[window].winbar = "original header"
local review = commands.attach(replica, { view = "review", title = "Review", keymaps = { viewed = "gv", unviewed = false },
  handler = { viewed = function() end, unviewed = function() end, close = function() end } })
assert(text(window):find("gv viewed", 1, true) and not text(window):find("unviewed", 1, true))
review.close()
assert(vim.wo[window].winbar == "original header")
vim.api.nvim_set_current_win(window)
vim.o.columns = 100
local pr_handler = {}
for _, id in ipairs({ "browse", "review", "comment", "delete", "open", "reply", "sync", "close" }) do
  pr_handler[id] = function() end
end
local pr = commands.attach(replica, {
  view = "pr", title = "PR #7", narrow_title = "< #7", keymaps = { open = "o" }, handler = pr_handler,
})
local pr_hint = text(window)
assert(pr_hint:sub(1, #"< #7<Tab> toggle") == "< #7<Tab> toggle", "narrow PR header spacing changed: " .. pr_hint)
assert(pr_hint:find("R reply", 1, true), "narrow PR header omitted reply: " .. pr_hint)
assert(pr_hint:find("o open | R reply", 1, true), "narrow PR header changed open/reply order: " .. pr_hint)
assert(pr_hint:find("close", 1, true) and pr_hint:find("help", 1, true), "narrow PR header lost essential shortcuts: " .. pr_hint)
pr.close()
assert(vim.wo[window].winbar == "original header")
buffer.close(replica)
print("native document hint lifecycle passed")
