vim.loader.enable(false)

local diff = require("git.diff")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function assert_equal(actual, expected, message)
  if actual ~= expected then
    error(("%s\nexpected: %s\nactual: %s"):format(message, tostring(expected), tostring(actual)), 2)
  end
end

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
assert_equal(small_text, small_diff, "small diffs should be returned unchanged")
assert_equal(small_compacted, false, "small diffs should not be marked compacted")
assert_equal(small_metrics.hunks, 1, "small diff hunk count")
assert_equal(small_metrics.changed, 2, "small diff changed count")

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
assert_equal(was_compacted, true, "large diff should be compacted")
assert_equal(lines_were_compacted, true, "large line-array diff should be compacted")
assert_equal(compacted_lines, compacted, "string and line-array compaction should match")
assert_equal(metrics.hunks, 2, "large diff hunk count")
assert_equal(metrics.added, 31, "large diff added count")
assert_equal(metrics.removed, 31, "large diff removed count")
assert_equal(line_metrics.hunks, metrics.hunks, "line-array hunk count should match string compaction")
assert_equal(line_metrics.added, metrics.added, "line-array added count should match string compaction")
assert_equal(line_metrics.removed, metrics.removed, "line-array removed count should match string compaction")
assert_true(compacted:find("Compact diff: 2 hunks, +31 -31 changed lines", 1, true), "summary is included")
assert_true(compacted:find("diff --git a/src/main.lua b/src/main.lua", 1, true), "large hunk header is included")
assert_true(compacted:find("-old line 01", 1, true), "large hunk first lines are included")
assert_true(not compacted:find("-old line 30", 1, true), "large hunk tail is omitted")
assert_true(compacted:find("... omitted 56 lines from this hunk ...", 1, true), "omitted body count is included")
assert_true(not compacted:find("diff --git a/src/small.lua b/src/small.lua", 1, true), "small hunk body is skipped")
assert_true(compacted:find("Skipped 1 small hunks %(2 changed lines total%)"), "small hunk summary is included")

local parsed = diff.parse_hunks(build_large_diff())
local parsed_metrics = diff.summarize_hunks(parsed)
assert_equal(parsed_metrics.hunks, metrics.hunks, "stream metrics should match parsed hunk count")
assert_equal(parsed_metrics.changed, metrics.changed, "stream metrics should match parsed changed count")

local no_large_hunks, no_large_was_compacted, no_large_metrics = diff.compact_lines(vim.split(build_large_diff(), "\n", { plain = true }), {
  max_hunks = 1,
  max_changed_lines = 10,
  min_hunk_changed_lines = 100,
  hunk_head_lines = 4,
})
assert_equal(no_large_was_compacted, true, "diff with only small hunks should still compact when over limits")
assert_equal(no_large_metrics.changed, 62, "no-large-hunks metrics changed count")
assert_true(no_large_hunks:find("No hunks have at least 100 changed lines.", 1, true), "no-large-hunks message is included")
assert_true(no_large_hunks:find("Skipped 2 small hunks (62 changed lines total)", 1, true), "all-small skipped summary is included")

print("git.diff compact tests passed")
