-- Guards the PR/review markdown segfault fix. The status reconcile edits buffer lines in
-- place, so the markdown LanguageTree keeps stale trees whose byte offsets no longer match
-- the shifted included regions. Parsing those reused nodes against the edited buffer
-- segfaults treesitter (reproduced live on the PR overview when expanding a file).
--
-- apply_markdown_parser_regions is the single chokepoint every markdown region (description
-- and comment bodies, in both the PR and review views) flows through, so forcing a full
-- re-parse there with parser:invalidate(true) fixes it everywhere. This test pins that call.

vim.loader.enable(false)

local pr_edit = require("diff_review.views.pr.pr_edit")

local function fail(message)
  vim.api.nvim_err_writeln(message)
  vim.cmd("cquit")
end

local calls = { set = 0, invalidate = 0, reload = nil, order = {} }
local fake_parser = {
  set_included_regions = function()
    calls.set = calls.set + 1
    calls.order[#calls.order + 1] = "set"
  end,
  invalidate = function(_, reload)
    calls.invalidate = calls.invalidate + 1
    calls.reload = reload
    calls.order[#calls.order + 1] = "invalidate"
  end,
}

pr_edit.apply_markdown_parser_regions(fake_parser, { { first0 = 0, after0 = 3 }, { first0 = 10, after0 = 13 } })

if calls.set ~= 1 then fail("apply_markdown_parser_regions did not set included regions") end
if calls.invalidate ~= 1 then
  fail("apply_markdown_parser_regions must invalidate the markdown parser after a reconcile edit, else treesitter segfaults reusing stale trees")
end
if calls.reload ~= true then fail("markdown parser invalidate must request a full reload (reload=true)") end
if calls.order[1] ~= "set" or calls.order[2] ~= "invalidate" then
  fail("invalidate must run after set_included_regions so the fresh parse covers the new regions")
end

-- A parser exposing no invalidate (older API) must still be tolerated, not errored.
local ok = pcall(pr_edit.apply_markdown_parser_regions, { set_included_regions = function() end }, {})
if not ok then fail("apply_markdown_parser_regions must tolerate a parser without invalidate") end

io.write("markdown_parser_reconcile OK\n")
vim.cmd("qa!")
