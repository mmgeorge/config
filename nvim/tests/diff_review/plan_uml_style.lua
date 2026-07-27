vim.loader.enable(false)

local function assert_equals(actual, expected, message)
  if not vim.deep_equal(actual, expected) then
    error((message or "values differ") .. "\nexpected: " .. vim.inspect(expected) .. "\nactual: " .. vim.inspect(actual), 2)
  end
end

local function highlight_for(segments, text)
  for _, segment in ipairs(segments or {}) do
    if segment[1] == text then return segment[2] end
  end
  return nil
end

local uml_style = require("diff_review.views.plan_review.uml_style")
require("diff_review.infra.highlights").setup()
assert_equals(
  vim.api.nvim_get_hl(0, { name = "DiffReviewDependencyName", link = false }).fg,
  tonumber("9dffe7", 16),
  "dependency names should use the shared teal palette color"
)
assert_equals(
  vim.api.nvim_get_hl(0, { name = "DiffReviewPlanMetadata", link = false }).fg,
  vim.api.nvim_get_hl(0, { name = "Normal", link = false }).fg,
  "plan metadata should use the regular text color"
)
local file_status_highlight = {
  New = "DiffReviewFileStatusNew",
  Modified = "DiffReviewFileStatusModified",
  Deleted = "DiffReviewFileStatusDeleted",
  Renamed = "DiffReviewFileStatusRenamed",
}
for status, highlight in pairs(file_status_highlight) do
  local segments = uml_style.segments("│  └─ inspection.rs    " .. status, {
    target_type = "file",
  })
  assert_equals(
    highlight_for(segments, status),
    highlight,
    status .. " should use its semantic file-status highlight"
  )
end

local aligned_entity = uml_style.align_owner(
  "*struct InspectionSummary [hello/src/inspection.rs]",
  { target_type = "entity" },
  80
)
assert_equals(
  vim.fn.strdisplaywidth(aligned_entity),
  80,
  "UML owners should align with the right edge of the PlanReview text canvas"
)

local aligned_flow = uml_style.align_owner(
  "*Parse input path [hello/src/main.rs]",
  { target_type = "flow_step" },
  80
)
assert_equals(
  vim.fn.strdisplaywidth(aligned_flow),
  80,
  "flow owners should align with the right edge of the PlanReview text canvas"
)

local declaration = uml_style.segments(
  "*struct InspectionSummary           [hello/src/inspection.rs]",
  { target_type = "entity" }
)
assert_equals(highlight_for(declaration, "struct"), "@keyword", "struct should use the Tree-sitter keyword group")
assert_equals(
  highlight_for(declaration, "InspectionSummary"),
  "@type",
  "entity names should use the Tree-sitter type group"
)
assert_equals(
  highlight_for(declaration, "[hello/src/inspection.rs]"),
  "DiffReviewPlanMetadata",
  "UML paths should use the dark-gray plan metadata highlight"
)

local operation = uml_style.segments(
  "  + inspect(input_path: &std::path::Path): Result<InspectionSummary, InspectionError>",
  { target_type = "entity_member" }
)
assert_equals(highlight_for(operation, "inspect"), "@function.method", "operations should use method highlighting")
assert_equals(
  highlight_for(operation, "input_path"),
  "@variable.parameter",
  "parameters should use parameter highlighting"
)
assert_equals(highlight_for(operation, "Path"), "@type", "qualified type names should use type highlighting")
assert_equals(highlight_for(operation, "Result"), "@type", "return types should use type highlighting")
assert_equals(
  highlight_for(operation, "InspectionSummary"),
  "@type",
  "generic arguments should use type highlighting"
)

local field = uml_style.segments("  + geometry_column: String", { target_type = "entity_member" })
assert_equals(
  highlight_for(field, "geometry_column"),
  "@variable.member",
  "fields should use member-variable highlighting"
)

local variant = uml_style.segments("  Metadata", { target_type = "enum_variant" })
assert_equals(highlight_for(variant, "Metadata"), "@variable", "enum variants should use variable highlighting")

local plain_text = uml_style.segments("The ownership model crosses one boundary.", { target_type = "section" })
assert_equals(plain_text, nil, "non-UML projection rows should retain Markdown highlighting")

local flow_step = uml_style.segments(
  "*Parse input path             [hello/src/main.rs]",
  { target_type = "flow_step" }
)
assert_equals(
  highlight_for(flow_step, "[hello/src/main.rs]"),
  "DiffReviewPlanMetadata",
  "code-flow paths should use the dark-gray plan metadata highlight"
)
assert_equals(
  highlight_for(flow_step, "*Parse input path"),
  nil,
  "code-flow actions should retain their existing presentation styling"
)

local flow_edge = uml_style.segments(
  "  ├─ Read schema() from SessionContext",
  {
    target_type = "flow_edge",
    callable_kind = "method",
    callable_name = "schema",
    target_name = "SessionContext",
    target_is_type = true,
  }
)
assert_equals(
  highlight_for(flow_edge, "schema"),
  "@function.method.call",
  "typed flow methods should use Tree-sitter method-call highlighting"
)
local function_edge = uml_style.segments(
  "  └─ Call decode() on GeoMetadata",
  {
    target_type = "flow_edge",
    callable_kind = "function",
    callable_name = "decode",
    target_name = "GeoMetadata",
    target_is_type = true,
  }
)
assert_equals(
  highlight_for(function_edge, "decode"),
  "@function.call",
  "typed flow functions should use Tree-sitter function-call highlighting"
)
assert_equals(
  highlight_for(flow_edge, "SessionContext"),
  "@type",
  "typed flow receivers should use Tree-sitter type highlighting"
)
local endpoint_edge = uml_style.segments(
  "  └─ Emit to terminal stdout",
  {
    target_type = "flow_edge",
    target_name = "terminal stdout",
    target_is_type = false,
  }
)
assert_equals(
  highlight_for(endpoint_edge, "terminal stdout"),
  nil,
  "endpoint names should retain standard text highlighting"
)

local entity_value = uml_style.segments(
  "        └─ InspectionSummary",
  { target_type = "flow_edge_result", value_kind = "type" }
)
assert_equals(
  highlight_for(entity_value, "InspectionSummary"),
  "@type",
  "flow values matching declared entities should use type highlighting"
)

local plain_flow_value = uml_style.segments(
  "        └─ stdout and exit status",
  { target_type = "flow_edge_result", value_kind = "text" }
)
assert_equals(
  highlight_for(plain_flow_value, "        └─ stdout and exit status"),
  "Normal",
  "non-entity flow values should use the standard text color"
)

local integration_test = uml_style.segments(
  "├─ IntegrationTest prints_metadata",
  { target_type = "test", category = "integration" }
)
assert_equals(
  highlight_for(integration_test, "IntegrationTest"),
  "@type",
  "test inventory categories should use Tree-sitter type highlighting"
)

local dependency = uml_style.segments(
  "└─ Add datafusion (54.0, Apache-2.0) - Register the local Parquet file.",
  { target_type = "dependency", dependency_id = "datafusion" }
)
assert_equals(
  highlight_for(dependency, "Add"),
  "DiffReviewWalkthroughActionAdd",
  "dependency actions should use the shared change-action highlight"
)
assert_equals(
  highlight_for(dependency, "datafusion"),
  "DiffReviewDependencyName",
  "dependency names should use the teal dependency highlight"
)
assert_equals(
  highlight_for(dependency, "(54.0, Apache-2.0)"),
  "DiffReviewPlanMetadata",
  "dependency versions and licenses should use the dark-gray plan metadata highlight"
)
local dependency_continuation = uml_style.segments(
  "│  schema access, and aggregate row-count query.",
  { target_type = "dependency", dependency_id = "datafusion" }
)
assert_equals(
  highlight_for(dependency_continuation, "│  schema access, and aggregate row-count query."),
  "Normal",
  "wrapped dependency prose should retain the standard text color"
)
assert_equals(
  highlight_for(dependency_continuation, "access,"),
  nil,
  "wrapped dependency prose must not reinterpret its second word as a package name"
)
local action_word_continuation = uml_style.segments(
  "│  Add support without starting another dependency.",
  { target_type = "dependency", dependency_id = "datafusion" }
)
assert_equals(
  highlight_for(action_word_continuation, "│  Add support without starting another dependency."),
  "Normal",
  "only a branch-opening dependency row may introduce an action and package"
)

local fixture_directory = vim.fn.tempname()
vim.fn.mkdir(fixture_directory, "p")
local working_path = vim.fs.joinpath(fixture_directory, "working.md")
vim.fn.writefile({
  "# Highlight UML",
  "",
  "# Diagrams",
  "```text",
  "*struct InspectionSummary           [hello/src/inspection.rs]",
  "```",
  "# Dependencies",
  "",
  "file hello/Cargo.toml",
  "└─ Add datafusion (54.0, Apache-2.0) - Register the local Parquet file.",
  "",
  "# Tasks",
  "# Tests",
  "file hello/tests/geoparquet_inspection.rs",
}, working_path)
vim.fn.writefile(
  {
    vim.json.encode({
      entity_changes = {},
      dependencies = {
        {
          dependency_id = "datafusion",
          action = "add",
          name = "datafusion",
          version = "54.0",
          manifest = "hello/Cargo.toml",
          license = "Apache-2.0",
          justification = "Register the local Parquet file. The standard library cannot query it.",
        },
      },
      tasks = {
        {
          title = "Verify unit rendering.",
          description = "Keep test categories semantic.",
          files = {
            {
              path = "tests/unit.rs",
              subtasks = {
                {
                  operation = "test",
                  action = "add",
                  category = "unit",
                  name = "renders_unit_category",
                  behavior = "Renders UnitTest as one type token.",
                },
              },
            },
          },
        },
      },
    }),
  },
  vim.fs.joinpath(fixture_directory, "working.json")
)
vim.fn.writefile({
  vim.json.encode({
    anchor = {
      {
        line = 5,
        target = { target_type = "entity", entity_id = "inspection_summary" },
      },
      {
        line = 9,
        target = { target_type = "dependency_manifest", manifest = "hello/Cargo.toml" },
      },
      {
        line = 10,
        target = { target_type = "dependency", dependency_id = "datafusion" },
      },
    },
  }),
}, vim.fs.joinpath(fixture_directory, "working.index.json"))

local task_model, load_error = require("diff_review.views.plan_review.task_model").load(working_path)
assert_equals(load_error, nil, "fixture plan should load")
local unit_test_node = task_model:task_nodes()[1].children[1].children[1]
assert_equals(
  unit_test_node.text:find("Add UnitTest `renders_unit_category`", 1, true) ~= nil,
  true,
  "unit test categories should render as one type name"
)
assert_equals(
  highlight_for(unit_test_node.segments_for_line(unit_test_node.text, 1), "UnitTest"),
  "@type",
  "UnitTest should use Tree-sitter type highlighting"
)
local projection_row_list = task_model:compose({}, 80)
assert_equals(
  vim.fn.strdisplaywidth(projection_row_list[5].text),
  80,
  "PlanReview composition should align UML owners using the active buffer width"
)
assert_equals(
  highlight_for(projection_row_list[5].segments, "[hello/src/inspection.rs]"),
  "DiffReviewPlanMetadata",
  "PlanReview composition should carry dark-gray UML paths into the source row"
)
assert_equals(
  highlight_for(projection_row_list[9].segments, "file"),
  "DiffReviewFileKeyword",
  "dependency manifests should reuse the shared file keyword highlight"
)
assert_equals(
  highlight_for(projection_row_list[9].segments, "hello/Cargo.toml"),
  "DiffReviewWalkthroughItemTitle",
  "dependency manifest paths should reuse the white file-path highlight"
)
assert_equals(
  highlight_for(projection_row_list[10].segments, "Add"),
  "DiffReviewWalkthroughActionAdd",
  "dependency rows should preserve semantic action highlights in PlanReview"
)
assert_equals(
  highlight_for(projection_row_list[10].segments, "datafusion"),
  "DiffReviewDependencyName",
  "dependency rows should preserve the teal library highlight in PlanReview"
)
assert_equals(
  highlight_for(projection_row_list[#projection_row_list].segments, "file"),
  "DiffReviewFileKeyword",
  "Tests file rows should reuse the shared file keyword highlight"
)
assert_equals(
  highlight_for(projection_row_list[#projection_row_list].segments, "hello/tests/geoparquet_inspection.rs"),
  "DiffReviewWalkthroughItemTitle",
  "Tests paths should reuse the white task-file path highlight"
)
vim.fn.delete(fixture_directory, "rf")

print("plan UML style tests passed")
vim.cmd("qa!")
