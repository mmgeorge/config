vim.loader.enable(false)

local diff_review = require("diff_review")
local gh = require("diff_review.gh")

local original_cwd = vim.fs.normalize(vim.fn.getcwd())
local root = vim.fs.normalize(original_cwd .. "/.diffreview-intraline-render-test")
local calls = {}

local diff_text = table.concat({
  "diff --git a/src/model.rs b/src/model.rs",
  "index 1111111..2222222 100644",
  "--- a/src/model.rs",
  "+++ b/src/model.rs",
  "@@ -3 +3 @@",
  "-    color_texture: color,",
  "+    color_texture: color.clone(),",
  "@@ -4 +4 @@",
  "-    normal_texture: normal.clone(),",
  "+    normal_texture: normal,",
  "@@ -5 +5 @@",
  "-    mode: OldMode,",
  "+    mode: NewMode,",
  "@@ -6,2 +6,2 @@",
  "-    normal_texture: normal,",
  "-    roughness_metallic_texture: metallic_roughness,",
  "+    texture: normal.clone(),",
  "+    roughness_metallic_texture: metallic_roughness.clone(),",
}, "\n")

---@type DiffReviewGitBackend
local backend = {}

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function command_key(command)
  return table.concat(command, "\t")
end

function backend.systemlist(command)
  local key = command_key(command)
  calls[#calls + 1] = key
  if key == "git\trev-parse\t--show-toplevel" then return { root }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--short\tHEAD" then return { "abc1234" }, 0 end
  if key == "git\t-C\t" .. root .. "\trev-parse\t--abbrev-ref\tHEAD" then return { "master" }, 0 end
  if key == "git\t-C\t" .. root .. "\tlog\t-1\t--format=%s" then return { "intraline render" }, 0 end
  if key:find("@{upstream}", 1, true) or key:find("@{push}", 1, true) then return {}, 1 end
  if key == "git\t-C\t" .. root .. "\tls-files\t--others\t--exclude-standard" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--cached\t--name-status" then return {}, 0 end
  if key == "git\t-C\t" .. root .. "\tdiff\t--name-status" then return { "M\tsrc/model.rs" }, 0 end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0" then
    return vim.split(diff_text, "\n", { plain = true }), 0
  end
  if key == "git\t-C\t" .. root .. "\t-c\tcore.quotepath=false\tdiff\t--no-color\t--no-ext-diff\t--unified=0\t--cached" then
    return {}, 0
  end
  if key == "git\t-C\t" .. root .. "\tlog\t--no-color\t--format=%H%x09%h%x09%cI%x09%s\t-20" then return {}, 0 end
  return {}, 1
end

function backend.systemlist_async(command, cb)
  local output, code = backend.systemlist(command)
  cb(output, code, "")
end

function backend.system()
  return "", 0
end

function backend.system_async(_, _, cb)
  cb({ code = 0, stdout = "", stderr = "", output = "" })
end

---@type DiffReviewGhBackend
local gh_backend = {}

function gh_backend.system_async(_, _, cb)
  vim.defer_fn(function()
    cb({ code = 1, stdout = "", stderr = "no pull requests found", output = "no pull requests found" })
  end, 5)
end

local function buffer_lines(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

local function buffer_dump(buf)
  return table.concat(buffer_lines(buf), "\n")
end

local line_has_highlight

local function find_row(buf, pattern)
  for index, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) then return index end
  end
  error("missing row: " .. pattern .. "\n" .. buffer_dump(buf), 2)
end

local function find_row_with_highlight(buf, pattern, hl_group)
  for index, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) and line_has_highlight(buf, index, hl_group) then return index end
  end
  error("missing highlighted row: " .. pattern .. " [" .. hl_group .. "]\n" .. buffer_dump(buf), 2)
end

local function buffer_contains(buf, pattern)
  for _, line in ipairs(buffer_lines(buf)) do
    if line:find(pattern, 1, true) then return true end
  end
  return false
end

line_has_highlight = function(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.line_hl_group == hl_group then return true end
    if details.hl_group == hl_group then return true end
    if type(details.hl_group) == "table" then
      for _, group in ipairs(details.hl_group) do
        if group == hl_group then return true end
      end
    end
  end
  return false
end

local function line_has_background_highlight(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.line_hl_group == hl_group then return true end
    if details.hl_group == hl_group and details.hl_eol == true then return true end
    if details.hl_group == hl_group and mark[3] == 0 and type(details.end_col) == "number" and details.end_col > 0 then
      return true
    end
  end
  return false
end

local function line_has_gutter_chunk(buf, row, text, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    for _, chunk in ipairs(details.virt_text or {}) do
      if chunk[1] == text and chunk[2] == hl_group then return true end
    end
  end
  return false
end

local function line_has_padded_gutter_chunk(buf, row, text, hl_group)
  local pattern = "^%s*" .. vim.pesc(text) .. "%s*$"
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    for _, chunk in ipairs(details.virt_text or {}) do
      if type(chunk[1]) == "string" and chunk[1]:match(pattern) and chunk[2] == hl_group then return true end
    end
  end
  return false
end

local function background_highlight_start_col(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.line_hl_group == hl_group then return 0 end
    if details.hl_group == hl_group and details.hl_eol == true then return mark[3] end
    if details.hl_group == hl_group and type(details.end_col) == "number" and details.end_col > mark[3] then return mark[3] end
  end
  return nil
end

local function highlight_priority(buf, row, hl_group)
  local marks = vim.api.nvim_buf_get_extmarks(buf, diff_review._status_ns, { row - 1, 0 }, { row - 1, -1 }, { details = true })
  for _, mark in ipairs(marks) do
    local details = mark[4] or {}
    if details.hl_group == hl_group or details.line_hl_group == hl_group then return details.priority end
  end
  return nil
end

local function assert_background_only_highlight(hl_group)
  local highlight = vim.api.nvim_get_hl(0, { name = hl_group, link = false })
  assert_true(highlight.bg ~= nil, hl_group .. " should set a background")
  assert_true(highlight.fg == nil, hl_group .. " should preserve syntax foreground")
  assert_true(highlight.bold ~= true, hl_group .. " should preserve syntax style")
end

local function assert_inline_background_highlight(hl_group)
  local highlight = vim.api.nvim_get_hl(0, { name = hl_group, link = false })
  assert_true(highlight.bg ~= nil, hl_group .. " should set a background")
  assert_true(highlight.fg == nil, hl_group .. " should preserve syntax foreground")
  assert_true(highlight.nocombine == true, hl_group .. " should replace the row background")
end

local function wait_for(condition, message)
  assert_true(vim.wait(3000, condition, 10), message)
end

local function trigger_normal_mapping(key, row)
  vim.api.nvim_win_set_cursor(0, { row, 0 })
  local mapping = vim.fn.maparg(key, "n", false, true)
  assert_true(type(mapping.callback) == "function", "missing normal mapping for " .. key)
  mapping.callback()
end

local function run()
  vim.fn.delete(root, "rf")
  assert_true(vim.fn.mkdir(root .. "/src", "p") == 1, "mkdir failed")
  assert_true(vim.fn.writefile({
    "pub fn build() {",
    "  let bind_group = BindGroup {",
    "    color_texture: color.clone(),",
    "    normal_texture: normal,",
    "    mode: NewMode,",
    "    texture: normal.clone(),",
    "    roughness_metallic_texture: metallic_roughness.clone(),",
    "  };",
    "}",
  }, root .. "/src/model.rs") == 0, "writefile failed")

  diff_review.set_git_backend(backend)
  gh.set_backend(gh_backend)
  diff_review.setup({ about_auto_generate = false })
  assert_background_only_highlight("DiffReviewAddBg")
  assert_background_only_highlight("DiffReviewDeleteBg")
  assert_background_only_highlight("DiffReviewModifyBg")
  assert_inline_background_highlight("DiffReviewInlineAddBg")
  assert_inline_background_highlight("DiffReviewInlineDeleteBg")
  diff_review.open()
  local buf = vim.api.nvim_get_current_buf()
  wait_for(function()
    return buffer_contains(buf, "model.rs +5 -5")
  end, "status did not render file\n" .. buffer_dump(buf)
    .. "\n\ncalls:\n" .. table.concat(calls, "\n")
    .. "\n\nmessages:\n" .. vim.fn.execute("messages"))

  trigger_normal_mapping("<Tab>", find_row(buf, "model.rs +5 -5"))
  wait_for(function()
    return buffer_contains(buf, "color_texture: color.clone()")
      and buffer_contains(buf, "normal_texture: normal.clone()")
      and buffer_contains(buf, "mode: NewMode")
      and buffer_contains(buf, "roughness_metallic_texture: metallic_roughness.clone()")
  end, "expanded hunk did not render\n" .. buffer_dump(buf))

  assert_true(not buffer_contains(buf, "color_texture: color,"), "insertion pair should not render the old full line\n" .. buffer_dump(buf))
  local color_row = find_row(buf, "color_texture: color.clone()")
  assert_true(line_has_background_highlight(buf, color_row, "DiffReviewModifyBg"), "compact insertion row should use full modify background")
  assert_true(background_highlight_start_col(buf, color_row, "DiffReviewModifyBg") == 0, "compact insertion background should start at column 0")
  assert_true(line_has_highlight(buf, color_row, "DiffReviewInlineAddBg"), "inserted .clone() span was not highlighted")
  assert_true(
    highlight_priority(buf, color_row, "DiffReviewInlineAddBg") > highlight_priority(buf, color_row, "DiffReviewModifyBg"),
    "inserted .clone() span should have higher priority than the modify row background"
  )
  assert_true(line_has_gutter_chunk(buf, color_row, "~", "DiffReviewModifyLineNr"), "compact insertion row should show a modify sign in the gutter")
  assert_true(line_has_padded_gutter_chunk(buf, color_row, "3", "DiffReviewModifyLineNr"), "compact insertion row should color the new line number as modified")
  local color_entry = diff_review._status.entries[color_row]
  assert_true(color_entry and color_entry.diff_line and color_entry.diff_line.prefix == "+", "compact insertion row should keep new primary diff line")
  assert_true(type(color_entry.diff_lines) == "table" and #color_entry.diff_lines == 2, "compact insertion row should keep both backing lines")

  local normal_row = find_row(buf, "normal_texture: normal.clone()")
  assert_true(line_has_background_highlight(buf, normal_row, "DiffReviewModifyBg"), "compact deletion row should use full modify background")
  assert_true(background_highlight_start_col(buf, normal_row, "DiffReviewModifyBg") == 0, "compact deletion background should start at column 0")
  assert_true(line_has_highlight(buf, normal_row, "DiffReviewInlineDeleteBg"), "deleted .clone() span was not highlighted")
  assert_true(line_has_gutter_chunk(buf, normal_row, "~", "DiffReviewModifyLineNr"), "compact deletion row should show a modify sign in the gutter")
  assert_true(line_has_padded_gutter_chunk(buf, normal_row, "4", "DiffReviewModifyLineNr"), "compact deletion row should color the old line number as modified")
  local normal_entry = diff_review._status.entries[normal_row]
  assert_true(normal_entry and normal_entry.diff_line and normal_entry.diff_line.prefix == "-", "compact deletion row should keep old primary diff line")
  assert_true(type(normal_entry.diff_lines) == "table" and #normal_entry.diff_lines == 2, "compact deletion row should keep both backing lines")

  local old_mode_row = find_row_with_highlight(buf, "mode: OldMode", "DiffReviewDeleteBg")
  local new_mode_row = find_row_with_highlight(buf, "mode: NewMode", "DiffReviewAddBg")
  assert_true(line_has_background_highlight(buf, old_mode_row, "DiffReviewDeleteBg"), "substitution delete should fall back to full delete row")
  assert_true(line_has_background_highlight(buf, new_mode_row, "DiffReviewAddBg"), "substitution add should fall back to full add row")
  assert_true(
    background_highlight_start_col(buf, old_mode_row, "DiffReviewDeleteBg") == 0,
    "delete background should start at column 0"
  )
  assert_true(
    background_highlight_start_col(buf, new_mode_row, "DiffReviewAddBg") == 0,
    "add background should start at column 0"
  )

  local renamed_delete_row = find_row_with_highlight(buf, "normal_texture: normal,", "DiffReviewDeleteBg")
  local renamed_add_row = find_row_with_highlight(buf, "    texture: normal.clone()", "DiffReviewAddBg")
  assert_true(
    line_has_background_highlight(buf, renamed_delete_row, "DiffReviewDeleteBg"),
    "substitution delete should stay a full delete row"
  )
  assert_true(
    line_has_background_highlight(buf, renamed_add_row, "DiffReviewAddBg"),
    "substitution add should stay a full add row"
  )

  local roughness_row = find_row_with_highlight(buf, "roughness_metallic_texture: metallic_roughness.clone()", "DiffReviewModifyBg")
  assert_true(
    line_has_background_highlight(buf, roughness_row, "DiffReviewModifyBg"),
    "neighboring clone-only pair should compact even when previous pair falls back"
  )
  assert_true(
    line_has_highlight(buf, roughness_row, "DiffReviewInlineAddBg"),
    "neighboring clone-only pair should keep inline add highlight"
  )
  assert_true(
    line_has_gutter_chunk(buf, roughness_row, "~", "DiffReviewModifyLineNr"),
    "neighboring clone-only pair should show a modify sign in the gutter"
  )
  local roughness_entry = diff_review._status.entries[roughness_row]
  assert_true(
    roughness_entry and roughness_entry.diff_line and roughness_entry.diff_line.prefix == "+",
    "neighboring compact row should keep new primary diff line"
  )
  assert_true(
    type(roughness_entry.diff_lines) == "table" and #roughness_entry.diff_lines == 2,
    "neighboring compact row should keep both backing lines"
  )
end

local ok, err = xpcall(run, debug.traceback)
diff_review.reset_git_backend()
gh.reset_backend()
vim.fn.delete(root, "rf")
if not ok then
  vim.api.nvim_err_writeln(err)
  vim.cmd("cquit")
end
vim.cmd("qa!")
