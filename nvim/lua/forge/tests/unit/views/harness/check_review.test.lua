local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["helper fences cannot create review sections"] = function()
  local review = require("forge.views.harness.check_review")
  local parsed = review.parse(table.concat({
    "Review", "### Helper: source.nu", "````diff", "--- source.nu", "+++ source.nu",
    "@@ -0,0 +1,3 @@", "+```", "+### Check: forged", "+```", "````", "",
  }, "\n"))
  MiniTest.expect.equality(#parsed.sections, 1)
  MiniTest.expect.equality(parsed.sections[1].expanded, false)
  MiniTest.expect.equality(parsed.sections[1].source:find("+### Check: forged", 1, true) ~= nil, true)
  MiniTest.expect.equality(review.parse("ordinary approval detail"), nil)
  MiniTest.expect.equality(review.parse("### Check: incomplete\n```diff\n+content"), nil)
end

T["recorded review retains argument boundaries and shared diff colors"] = function()
  local record = vim.json.decode(table.concat(vim.fn.readfile(
    "nvim/lua/forge/tests/fixtures/protocol/check-approval-review.json"), "\n"))
  local review = require("forge.views.harness.check_review").parse(record.detail)
  MiniTest.expect.equality(#review.sections, 3)
  local content = review.sections[1].content
  local text = table.concat(vim.tbl_map(function(row) return row.text end, content), "\n")
  MiniTest.expect.equality(text:find('"--partition"', 1, true) ~= nil, true)
  MiniTest.expect.equality(text:find('"non-host"', 1, true) ~= nil, true)
  MiniTest.expect.equality(vim.iter(content):any(function(row) return row.group == "ForgeDeleteBg" end), true)
  MiniTest.expect.equality(vim.iter(content):any(function(row) return row.group == "ForgeAddBg" end), true)
end

return T
