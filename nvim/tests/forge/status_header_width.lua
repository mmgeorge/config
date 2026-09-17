vim.opt.runtimepath:append("nvim")
local render = require("forge.status_render")
local input = require("forge.input")
local fixture = dofile("nvim/tests/forge/support/status_fixture.lua")
local replica = render.open("header-width", { notice = error })
vim.cmd("vsplit")
vim.api.nvim_win_set_width(0, 40)
vim.api.nvim_set_current_buf(replica.buffer)
local subject = "refactor: " .. string.rep("界 change ", 30)
local head = { oid = string.rep("a", 40), reference = "feature", subject = subject }
local remote = { oid = head.oid, reference = "origin/feature", subject = subject }
local context = { workspace = "fixture", branch = "feature", head = head, upstream = remote, push = remote,
  recent = {}, issues = {} }
replica.width = 40
assert(render.apply_snapshot(replica, fixture.snapshot(replica.document, { context = context })).kind == "Applied")
local view = input.open(replica, 0, { margin = 0 })
assert(vim.wo.wrap and vim.wo.linebreak and not vim.wo.breakindent and vim.wo.statuscolumn == "")

local original = vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)
local function verify()
  for _, role in ipairs({ "head", "upstream", "push" }) do
    local block = replica.block["status:context:" .. role]
    local _, start = replica.sequence:position("status:context:" .. role)
    assert(#block.chunk == 1, "header was split into physical buffer lines")
    local parts = {}
    for _, chunk in ipairs(block.chunk[1]) do parts[#parts + 1] = chunk[1] end
    local line = table.concat(parts)
    local value = line:sub(9)
    local reference = role == "head" and "feature       " or "origin/feature"
    assert(value == vim.fn.strcharpart("aaaaaaa " .. reference .. " " .. subject, 0, 99) .. "…", value)
    assert(vim.fn.strchars(value) == 100)
    assert(vim.api.nvim_buf_get_lines(replica.buffer, start, start + 1, false)[1] == line)
    assert(block.chunk[1][2][2] == "ForgeStatusObjectId", "SHA highlight was lost")
    assert(replica.inventory.context[role].subject == subject, "clamping changed the retained commit")
  end
end
verify()
local _, start = replica.sequence:position("status:context:head")
local height = vim.api.nvim_win_text_height(0, { start_row = start, end_row = start }).all
assert(height > 1, "narrow window did not soft-wrap the header")
vim.api.nvim_win_set_cursor(0, { start + 1, 0 })
vim.cmd("normal! j")
assert(vim.api.nvim_win_get_cursor(0)[1] == start + 2, "native down movement stopped in a generated continuation")
vim.cmd("only")
render.resize(replica, vim.api.nvim_get_current_win())
verify()
assert(vim.deep_equal(original, vim.api.nvim_buf_get_lines(replica.buffer, 0, -1, false)), "resizing rewrote header text")
input.close(view)
require("forge.buffer").close(replica)
print("status_header_width: Unicode cap, reference alignment, native wrapping and cursor movement passed")
