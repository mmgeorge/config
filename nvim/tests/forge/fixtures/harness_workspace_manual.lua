local layout = require("forge.views.harness.layout")
local state = require("forge.session").harness
require("forge.client").subscribe = function() return function() end end
vim.keymap.set("n", "gks", "<Cmd>close!<CR>")
vim.keymap.set("n", "gkb", "<Cmd>bd<CR>")
state.transcript_buf, state.transcript_win, state.composer_buf, state.composer_win, state.timeline_tab =
  layout.open("workspace-manual")
state.session = { id = "workspace-manual" }
vim.bo[state.transcript_buf].modifiable = true
vim.api.nvim_buf_set_lines(state.transcript_buf, 0, -1, false, { "Harness timeline", "q exits both panes" })
vim.bo[state.transcript_buf].modifiable = false
require("forge.views.harness.controller").attach()
