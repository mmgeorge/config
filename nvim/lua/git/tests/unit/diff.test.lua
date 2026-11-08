-- Unit tests for `git.diff` (diff.lua).
local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["diff_compact"] = function()
  local diff = require("git.diff")
  local function build_large_diff()
    local lines = {
      "diff --git a/src/main.lua b/src/main.lua",
      "index 1111111..2222222 100644",
      "--- a/src/main.lua",
      "+++ b/src/main.lua",
      "@@ -1,30 +1,30 @@",
    }
    for i = 1, 30 do
      lines[#lines + 1] = ("-old line %02d"):format(i)
      lines[#lines + 1] = ("+new line %02d"):format(i)
    end
    lines[#lines + 1] = "diff --git a/src/small.lua b/src/small.lua"
    lines[#lines + 1] = "index 3333333..4444444 100644"
    lines[#lines + 1] = "--- a/src/small.lua"
    lines[#lines + 1] = "+++ b/src/small.lua"
    lines[#lines + 1] = "@@ -1 +1 @@"
    lines[#lines + 1] = "-x"
    lines[#lines + 1] = "+y"
    return table.concat(lines, "\n")
  end

  local small_diff = table.concat({
    "diff --git a/a.lua b/a.lua",
    "--- a/a.lua",
    "+++ b/a.lua",
    "@@ -1 +1 @@",
    "-old",
    "+new",
  }, "\n")

  local small_text, small_compacted, small_metrics = diff.compact(small_diff, {
    max_hunks = 10,
    max_changed_lines = 10,
  })
  eq(small_text, small_diff, { fail_reason = "small diffs should be returned unchanged" })
  eq(small_compacted, false, { fail_reason = "small diffs should not be marked compacted" })
  eq(small_metrics.hunks, 1, { fail_reason = "small diff hunk count" })
  eq(small_metrics.changed, 2, { fail_reason = "small diff changed count" })

  local compacted, was_compacted, metrics = diff.compact(build_large_diff(), {
    max_hunks = 1,
    max_changed_lines = 10,
    min_hunk_changed_lines = 8,
    hunk_head_lines = 4,
  })
  local compacted_lines, lines_were_compacted, line_metrics = diff.compact_lines(vim.split(build_large_diff(), "\n", { plain = true }), {
    max_hunks = 1,
    max_changed_lines = 10,
    min_hunk_changed_lines = 8,
    hunk_head_lines = 4,
  })
  eq(was_compacted, true, { fail_reason = "large diff should be compacted" })
  eq(lines_were_compacted, true, { fail_reason = "large line-array diff should be compacted" })
  eq(compacted_lines, compacted, { fail_reason = "string and line-array compaction should match" })
  eq(metrics.hunks, 2, { fail_reason = "large diff hunk count" })
  eq(metrics.added, 31, { fail_reason = "large diff added count" })
  eq(metrics.removed, 31, { fail_reason = "large diff removed count" })
  eq(line_metrics.hunks, metrics.hunks, { fail_reason = "line-array hunk count should match string compaction" })
  eq(line_metrics.added, metrics.added, { fail_reason = "line-array added count should match string compaction" })
  eq(line_metrics.removed, metrics.removed, { fail_reason = "line-array removed count should match string compaction" })
  assert(compacted:find("Compact diff: 2 hunks, +31 -31 changed lines", 1, true), "summary is included")
  assert(compacted:find("diff --git a/src/main.lua b/src/main.lua", 1, true), "large hunk header is included")
  assert(compacted:find("-old line 01", 1, true), "large hunk first lines are included")
  assert(not compacted:find("-old line 30", 1, true), "large hunk tail is omitted")
  assert(compacted:find("... omitted 56 lines from this hunk ...", 1, true), "omitted body count is included")
  assert(not compacted:find("diff --git a/src/small.lua b/src/small.lua", 1, true), "small hunk body is skipped")
  assert(compacted:find("Skipped 1 small hunks %(2 changed lines total%)"), "small hunk summary is included")

  local parsed = diff.parse_hunks(build_large_diff())
  local parsed_metrics = diff.summarize_hunks(parsed)
  eq(parsed_metrics.hunks, metrics.hunks, { fail_reason = "stream metrics should match parsed hunk count" })
  eq(parsed_metrics.changed, metrics.changed, { fail_reason = "stream metrics should match parsed changed count" })

  local no_large_hunks, no_large_was_compacted, no_large_metrics = diff.compact_lines(vim.split(build_large_diff(), "\n", { plain = true }), {
    max_hunks = 1,
    max_changed_lines = 10,
    min_hunk_changed_lines = 100,
    hunk_head_lines = 4,
  })
  eq(no_large_was_compacted, true, { fail_reason = "diff with only small hunks should still compact when over limits" })
  eq(no_large_metrics.changed, 62, { fail_reason = "no-large-hunks metrics changed count" })
  assert(no_large_hunks:find("No hunks have at least 100 changed lines.", 1, true), "no-large-hunks message is included")
  assert(no_large_hunks:find("Skipped 2 small hunks (62 changed lines total)", 1, true), "all-small skipped summary is included")
end

return T
