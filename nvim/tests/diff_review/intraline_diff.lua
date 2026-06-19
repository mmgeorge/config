vim.loader.enable(false)

local intraline = require("diff_review.intraline_diff")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function line(prefix, code, old_line, new_line, position)
  return {
    prefix = prefix,
    code = code,
    old_line = old_line,
    new_line = new_line,
    position = position or 1,
  }
end

local function run()
  local prefix_len, suffix_len = intraline.common_affix("normal,", "normal.clone(),")
  assert_true(prefix_len == #"normal", "prefix should stop before inserted .clone()")
  assert_true(suffix_len == 1, "suffix should keep trailing comma")

  local insertion = intraline.compact_pair(
    line("-", "      normal_texture: normal,", 69, nil, 1),
    line("+", "      normal_texture: normal.clone(),", nil, 69, 2)
  )
  assert_true(insertion ~= nil, "clone insertion should compact")
  assert_true(insertion.display_line.prefix == "+", "insertion should display the new line")
  assert_true(#insertion.inline_spans == 1, "insertion should have one inline span")
  local added = insertion.inline_spans[1]
  assert_true(added.hl_group == "DiffReviewInlineAddBg", "insertion should use inline add highlight")
  assert_true(insertion.display_line.code:sub(added.start_col + 1, added.end_col) == ".clone()", "insertion span should cover .clone()")

  local deletion = intraline.compact_pair(
    line("-", "      normal_texture: normal.clone(),", 69, nil, 1),
    line("+", "      normal_texture: normal,", nil, 69, 2)
  )
  assert_true(deletion ~= nil, "clone deletion should compact")
  assert_true(deletion.display_line.prefix == "-", "deletion should display the old line")
  local removed = deletion.inline_spans[1]
  assert_true(removed.hl_group == "DiffReviewInlineDeleteBg", "deletion should use inline delete highlight")
  assert_true(deletion.display_line.code:sub(removed.start_col + 1, removed.end_col) == ".clone()", "deletion span should cover .clone()")

  local substitution = intraline.compact_pair(
    line("-", "      value: old_name,", 10, nil, 1),
    line("+", "      value: new_name,", nil, 10, 2)
  )
  assert_true(substitution == nil, "substitutions should fall back to full diff rows")

  local hunk_items = intraline.compact_hunk_lines({
    line("-", "      normal_texture: normal,", 69, nil, 1),
    line("-", "      roughness_metallic_texture: metallic_roughness,", 70, nil, 2),
    line("+", "      normal_texture: normal.clone(),", nil, 69, 3),
    line("+", "      roughness_metallic_texture: metallic_roughness.clone(),", nil, 70, 4),
  })
  assert_true(#hunk_items == 2, "two replacement pairs should collapse to two render items")
  assert_true(hunk_items[1].kind == "replacement" and hunk_items[2].kind == "replacement", "collapsed items should be replacements")

  local fallback_items = intraline.compact_hunk_lines({
    line("-", "      value: old_name,", 10, nil, 1),
    line("-", "      other: keep,", 11, nil, 2),
    line("+", "      value: new_name,", nil, 10, 3),
    line("+", "      other: keep.clone(),", nil, 11, 4),
  })
  assert_true(#fallback_items == 4, "mixed substitution groups should stay canonical")
  assert_true(fallback_items[1].kind == "line" and fallback_items[4].kind == "line", "fallback should return normal line items")
end

local ok, err = xpcall(run, debug.traceback)
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
