package.path = "nvim/lua/?.lua;nvim/lua/?/init.lua;" .. package.path

local parser = require("forge.render.diff_parse")

local function run()
  local source = [=[diff --git "a/old name \351\233\252.txt" "b/new\tname\n\"\\.txt"
--- "a/old name \351\233\252.txt"
+++ "b/new\tname\n\"\\.txt"
@@ -1,1 +1,1 @@
-before
+after
diff --git "a/deleted name.txt" "b/deleted name.txt"
--- "a/deleted name.txt"
+++ /dev/null
@@ -1,1 +0,0 @@
-deleted
diff --git a/old plain.txt "b/new quoted.txt"
--- a/old plain.txt
+++ "b/new quoted.txt"
@@ -1,1 +1,1 @@
-old
+new
diff --git "a/old quoted.txt" b/new plain.txt
--- "a/old quoted.txt"
+++ b/new plain.txt
@@ -1,1 +1,1 @@
-old
+new
diff --git "a/old\ttab.bin" "b/new\ttab.bin"
Binary files differ
]=]
  local block = parser.parse_unified_diff(source)
  assert(#block == 5, "quoted headings must start separate file blocks")
  assert(block[1].old_file == "old name 雪.txt")
  assert(block[1].file == 'new\tname\n"\\.txt')
  assert(block[1].new_file == block[1].file)
  assert(block[1].hunks[1].old_count == 1 and block[1].hunks[1].new_count == 1)
  assert(block[1].hunks[1].lines[2].code == "after")
  assert(block[2].file == "deleted name.txt" and block[2].new_file == "/dev/null")
  assert(block[3].old_file == "old plain.txt" and block[3].new_file == "new quoted.txt")
  assert(block[4].old_file == "old quoted.txt" and block[4].new_file == "new plain.txt")
  assert(block[5].old_file == "old\ttab.bin" and block[5].new_file == "new\ttab.bin")
  assert(parser.diff_path_without_prefix('"a/space 雪.txt"') == "space 雪.txt")
  assert(parser.diff_path_without_prefix("a/file.txt\t2026-01-01") == "file.txt")
  assert(not pcall(parser.diff_path_without_prefix, '"a/unterminated'))
  assert(not pcall(parser.diff_path_without_prefix, '"a/\\777"'))
end

local ok, failure = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit")
end
vim.cmd("qa!")
