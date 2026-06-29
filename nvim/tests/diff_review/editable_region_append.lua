-- Guards that opening a line at the bottom of an editable region (o / trailing <CR>) keeps
-- the new row inside the region. region_kind_at (description), in_comment_region (review
-- summary), and comment_body_at_row (inline comments) all gate editability with the same
-- 1-based formula `row >= first0 + 1 and row <= after0` over region.bounds(). This pins the
-- end-anchor gravity each region is created with so the O/o "new line is not modifiable" bug
-- cannot regress.
vim.loader.enable(false)

local region = require("diff_review.render.region")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local ns = vim.api.nvim_create_namespace("editable_region_append_test")

--- The exact editability test shared by region_kind_at / in_comment_region / comment_body_at_row.
local function row_editable(reg, row1)
  local first0, after0 = region.bounds(reg)
  if not (first0 and after0) then return false end
  return row1 >= first0 + 1 and row1 <= after0
end

--- Build a buffer + window with a single-line editable body at 1-based row 3, a label above
--- and a trailing field below, then return the region created with the given opts.
local function setup(opts)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
    "Label:", "Editable header", "the editable body line", "Trailing: field",
  })
  -- Body is 1-based row 3 (0-based 2); the region spans that single body row.
  local reg = region.new(buf, ns, 2, 3, opts)
  local win = vim.api.nvim_open_win(buf, true, { relative = "editor", row = 1, col = 1, width = 40, height = 8 })
  return buf, reg, win
end

--- Run a normal-mode edit command at 1-based body row 3, then return the cursor's new row.
local function press_at_body(keys)
  vim.api.nvim_win_set_cursor(0, { 3, 0 })
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), "x", false)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "x", false)
  return vim.api.nvim_win_get_cursor(0)[1]
end

-- ── description region: exclusive one-past anchor with right gravity ──────────────────────
-- end_right_gravity lets a line opened at the bottom join the body; end_exclusive keeps
-- read_text/current_values reading the correct line span.
do
  local _, reg = setup({ end_exclusive = true, end_right_gravity = true })
  local above = press_at_body("O")
  assert_true(row_editable(reg, above), "description: O above the body must stay editable")
end
do
  local _, reg = setup({ end_exclusive = true, end_right_gravity = true })
  local below = press_at_body("o")
  assert_true(row_editable(reg, below), "description: o below the body must stay editable (the O/o bug)")
end
do
  -- The exclusive end anchor must never absorb the trailing field below the region.
  local _, reg = setup({ end_exclusive = true, end_right_gravity = true })
  assert_true(row_editable(reg, 3), "description: the body row must be editable")
  assert_true(not row_editable(reg, 4), "description: the trailing field must stay locked")
end

-- ── proof the gravity matters: the old exclusive + left-gravity config fails o-below ──────
do
  local _, reg = setup({ end_exclusive = true })
  local below = press_at_body("o")
  assert_true(not row_editable(reg, below),
    "guard: exclusive + left gravity must reproduce the o-below bug, else the test cannot catch a regression")
end

-- ── review summary + inline comment regions: inclusive (default right gravity) ────────────
-- review_comment_region and review_body_region are created without end_exclusive, so the
-- default right-gravity end anchor already extends to a line opened at the bottom.
do
  local _, reg = setup({ region_kind = "review_comment", editable = true })
  local below = press_at_body("o")
  assert_true(row_editable(reg, below), "review summary / comment: o below the body must stay editable")
end
do
  local _, reg = setup({ region_kind = "review_comment", editable = true })
  assert_true(not row_editable(reg, 4), "review summary / comment: the trailing field must stay locked")
end

print("PASS  editable_region_append")
vim.cmd("qa!")
