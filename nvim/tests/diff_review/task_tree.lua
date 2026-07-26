vim.loader.enable(false)

local function assert_true(value, message)
  if not value then error(message or "expected truthy value", 2) end
end

local function row_with_id(row_list, id)
  for _, row in ipairs(row_list) do
    if row.id == id then return row end
  end
  error("missing row " .. id, 2)
end

local function segment_with_highlight(segment_list, text, highlight)
  for _, segment in ipairs(segment_list or {}) do
    if segment[1] == text and segment[2] == highlight then return true end
  end
  return false
end

local ok, failure = pcall(function()
  require("diff_review").setup()
  local task_tree = require("diff_review.render.task_tree")
  local task_tree_style = require("diff_review.render.task_tree_style")
  local row_list = task_tree.render({
    {
      id = "task",
      text = "Own plan tasks through one shared renderer.",
      branch = false,
      first_prefix = "1. ",
      continuation_prefix = "   ",
      foldable = true,
      default_folded = true,
      children = {
        {
          id = "file",
          text = "file src/plan.rs",
          branch = false,
          foldable = false,
          child_prefix = "",
          children = {
            {
              id = "subtask",
              text = "Create the renderer boundary.",
              branch = true,
              foldable = true,
              default_folded = true,
              children = {
                {
                  id = "first",
                  text = "Add Resource FirstOwner — Retains a following sibling.",
                  segments_for_line = task_tree_style.change("Add", "Resource", "FirstOwner"),
                  branch = true,
                  foldable = false,
                },
                {
                  id = "last",
                  text = "Add Resource FinalOwner — Wrap this final child onto another aligned row.",
                  branch = true,
                  foldable = false,
                },
              },
            },
          },
        },
      },
    },
  }, 52)

  assert_true(row_with_id(row_list, "task").fold_id == "task", "task identity should anchor its fold")
  assert_true(row_with_id(row_list, "subtask").fold_id == "subtask",
    "subtask identity should anchor its fold")
  assert_true(row_with_id(row_list, "subtask").text:find("   └─ ", 1, true) == 1,
    "file children should render at their own depth")
  assert_true(row_with_id(row_list, "first").text:find("      ├─ ", 1, true) == 1,
    "intermediate leaves should retain an open branch")
  assert_true(row_with_id(row_list, "last").text:find("      └─ ", 1, true) == 1,
    "final leaves should use a closing branch")
  local first_row = row_with_id(row_list, "first")
  assert_true(segment_with_highlight(first_row.segments, "Add", "DiffReviewWalkthroughActionAdd"),
    "change actions should use the shared status highlight")
  assert_true(segment_with_highlight(first_row.segments, "Resource", "@keyword"),
    "change kinds should use Tree-sitter keyword highlighting")
  assert_true(segment_with_highlight(first_row.segments, "FirstOwner", "@type"),
    "change targets should use Tree-sitter type highlighting")

  local prose_segments = task_tree_style.entity_references(nil, { FirstOwner = true })(
    "FirstOwner coordinates FirstOwnership.",
    1
  )
  assert_true(segment_with_highlight(prose_segments, "FirstOwner", "@type"),
    "canonical entity tokens should use Tree-sitter type highlighting in prose")
  assert_true(not segment_with_highlight(prose_segments, "FirstOwnership", "@type"),
    "entity highlighting should not match identifier substrings")

  local wrapped_final = nil
  for _, row in ipairs(row_list) do
    if row.id == "last:line:2" then wrapped_final = row end
  end
  assert_true(wrapped_final ~= nil, "final leaf should wrap at the requested display width")
  assert_true(wrapped_final.text:find("         ", 1, true) == 1,
    "wrapped final leaves should align beneath their content")
  assert_true(wrapped_final.text:find("│", 1, true) == nil,
    "wrapped final leaves should not retain a dangling continuation branch")
  assert_true(vim.fn.strdisplaywidth(wrapped_final.text) <= 52,
    "wrapped rows should respect display-cell width")
end)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
