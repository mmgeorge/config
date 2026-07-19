local interaction_tree = require("diff_review.render.harness.interaction_tree")

local rendered = interaction_tree.build({
  {
    kind = "plan_resolution",
    id = "resolution",
    resolution = {
      id = "resolution",
      kind = "completed",
      task_summary = { completed = 2, blocked = 0, total = 2 },
      test_summary = { passed = 3, failed = 0, skipped = 1, not_run = 0 },
    },
    deviation = {
      {
        kind = "informational",
        summary = "Used the existing parser owner",
        reason = "Repository inspection found the responsibility there.",
        disposition = "recorded",
      },
    },
    audit = {
      unplanned_paths = {},
      unchanged_planned_paths = { { path = "src/unchanged.rs" } },
    },
  },
})

local text = table.concat(rendered.lines, "\n")
assert(text:find("Plan Completed", 1, true), "terminal timeline label is missing")
assert(text:find("Deviations", 1, true), "nested deviations are missing")
assert(text:find("Audit", 1, true), "nested audit is missing")

vim.cmd("qa!")
