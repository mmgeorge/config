vim.loader.enable(false)
local markdown_root = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", "render-markdown.nvim")
assert(vim.fn.isdirectory(markdown_root) == 1, "historical formatting requires installed render-markdown")
vim.opt.runtimepath:append(markdown_root)
vim.opt.runtimepath:append(vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", "nvim-treesitter"))
local source = require("forge.source_document")
local native_syntax = require("forge.native_syntax")
local markdown_options = require("plugins.markdown")[1].opts()
require("render-markdown").setup(markdown_options)
vim.cmd("runtime plugin/render-markdown.lua")
local lines = { "# Historical fixture", "", "```rust", 'fn main() { println!("historical"); }', "```", "", "- First item", "- Second item" }
local revision = "3388dc3ff7e9b4a441363e080f066533adbb9d36"
source._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then
    callback({ title = "history.md @ " .. revision, object = revision, revision = revision,
      more = false, state = { state = "ready" }, snapshot = { document = params.document, revision = 1,
        block = { { id = "source", text = lines, metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
  elseif params.operation == "commit_message" then
    callback({ title = "Commit " .. revision, object = "text",
      more = false, state = { state = "ready" }, snapshot = { document = params.document, revision = 1,
        block = { { id = "commit-message", text = { "Add historical Markdown fixture", "" },
          metadata = { target = {}, decoration = {}, editable_region = {} } } } } })
  else callback({}) end
end)
local success, failure = xpcall(function()
  vim.wo.number = true
  vim.wo.signcolumn = "yes"
  vim.wo.foldcolumn = "1"
  vim.wo.foldmethod = "manual"
  vim.wo.foldtext = "foldtext()"
  vim.wo.conceallevel = 2
  local presentation = require("forge.window_presentation")
  local status_owner = {}
  presentation.retain(vim.api.nvim_get_current_win(), status_owner, presentation.capture(vim.api.nvim_get_current_win()))
  vim.wo.number = false
  vim.wo.signcolumn = "no"
  vim.wo.foldcolumn = "0"
  vim.wo.foldmethod = "expr"
  vim.wo.foldexpr = "0"
  vim.wo.conceallevel = 0
  local origin = vim.api.nvim_get_current_buf()
  vim.b[origin].forge_native_document = true
  vim.bo[origin].filetype = "markdown"
  assert(markdown_options.ignore(origin), "mixed native Markdown was admitted")
  local owner = source.open({ workspace = vim.fn.getcwd(), path = "history.md", revision = revision, line = 4 })
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == owner.replica.buffer end))
  assert(vim.wo.number and vim.wo.signcolumn == "yes" and vim.wo.foldcolumn == "1", "source columns were hidden")
  assert(vim.wo.foldmethod == "manual" and vim.wo.foldtext == "foldtext()", "source inherited document folding")
  assert(not owner.view[vim.api.nvim_get_current_win()].input.document_folds, "source acquired document fold ownership")
  assert(vim.wo.conceallevel == 2, "source inherited mixed Markdown conceal policy")
  assert(vim.wo.winbar:find("history.md @ 3388dc3 —", 1, true), "historical header lost seven-character revision")
  assert(not vim.wo.winbar:find("3388dc3ff", 1, true), "historical header retained long revision")
  assert(vim.api.nvim_buf_get_name(owner.replica.buffer):find("@3388dc3#", 1, true), "buffer display revision was not abbreviated")
  assert(vim.api.nvim_win_get_cursor(0)[1] == 4, "source anchor moved")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), lines), "presentation rewrote historical source")
  assert(vim.bo.readonly and not vim.bo.modifiable, "historical source became editable")
  assert(not native_syntax.global_parser_allowed(owner.replica.buffer), "source admitted global syntax")
  assert(not markdown_options.ignore(owner.replica.buffer), "exact Markdown source formatting was excluded")
  assert(require("render-markdown.core.manager").attached(owner.replica.buffer), "formatter did not attach")
  assert(vim.treesitter.get_parser(owner.replica.buffer, "markdown"), "Markdown parser unavailable")
  require("render-markdown.core.ui").updater.new(owner.replica.buffer, vim.api.nvim_get_current_win(), true):run()
  vim.cmd("redraw")
  assert(vim.wait(2000, function()
    for name, namespace in pairs(vim.api.nvim_get_namespaces()) do
      if name:find("render%-markdown") and #vim.api.nvim_buf_get_extmarks(owner.replica.buffer, namespace, 0, -1, {}) > 0 then
        return true
      end
    end
  end), "historical Markdown formatter produced no decorations")
  local namespace = require("render-markdown.core.ui").ns
  local fence, bullet = false, false
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(owner.replica.buffer, namespace, 0, -1, { details = true })) do
    if mark[2] == 2 then fence = true end
    if mark[2] == 6 then bullet = true end
  end
  assert(fence and bullet, "historical fences and bullets were not formatted")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), lines), "Markdown formatting changed source coordinates")
  assert(vim.treesitter.highlighter.active[owner.replica.buffer] == nil, "Markdown formatting started duplicate syntax")
  source.close(owner)
  assert(vim.api.nvim_get_current_buf() == origin, "source close lost origin")
  assert(not vim.wo.number and vim.wo.signcolumn == "no" and vim.wo.foldcolumn == "0", "source close leaked source columns")
  assert(vim.wo.foldmethod == "expr" and vim.wo.foldexpr == "0", "source close leaked source folding")
  assert(vim.wo.conceallevel == 0, "source close leaked source conceal")
  vim.wo.winbar = "%f"
  vim.wo.number = true
  local commit = source.open_commit({ workspace = vim.fn.getcwd(), oid = revision })
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == commit.replica.buffer end))
  assert(vim.wo.winbar == "%#WinBar# 󰈔 %*%#DropBarFileName#3388dc3%*",
    "commit message lost the legacy short-SHA header")
  assert(vim.api.nvim_buf_get_name(commit.replica.buffer):match("GitCommit://3388dc3$"), "commit buffer lost seven-character title")
  assert(not vim.api.nvim_buf_get_name(commit.replica.buffer):find("3388dc3ff", 1, true), "commit title retained long revision")
  assert(vim.bo.filetype == "gitcommit", "commit message lost gitcommit syntax")
  assert(vim.wo.number and vim.wo.signcolumn == "no" and vim.wo.foldcolumn == "0", "commit message changed inherited columns")
  assert(vim.bo.readonly and not vim.bo.modifiable, "commit message became editable")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(0, 0, -1, false), { "Add historical Markdown fixture", "" }),
    "commit presentation changed native message text")
  vim.v.errmsg = ""
  vim.api.nvim_feedkeys("q", "x", false)
  assert(vim.wait(1000, function() return vim.api.nvim_get_current_buf() == origin end), "commit q mapping lost origin")
  assert(vim.wo.number, "commit return leaked window presentation")
  assert(vim.v.errmsg == "", vim.v.errmsg)
end, debug.traceback)
source._set_runner_for_test(nil)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("source_document_presentation OK")
vim.cmd("qa!")
