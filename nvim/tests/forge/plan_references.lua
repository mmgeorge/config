vim.loader.enable(false)
local picker = require("forge.views.picker")
local original_open = picker.open
local specification
picker.open = function(value) specification = value end
local reference = {
  id = "method", path = "src/arena.rs", line = 30, owner = "RoundPlugin::@impl:0::new",
  owner_capture = { "@type", vim.NIL, "@function" }, name = "ArenaConfig", kind = "type",
  text = "  pub fn new(config: ArenaConfig) -> Self {",
  text_highlight = { { range = { start = { row = 0, column = 2 }, ["end"] = { row = 0, column = 5 } },
    capture = "@keyword.rust", priority = 100 },
    { range = { start = { row = 0, column = 0 }, ["end"] = { row = 0, column = 2 } },
      capture = "@comment", priority = 100 } },
}
local success, failure = xpcall(function()
  require("forge.views.plan_review.references").open({ win = vim.api.nvim_get_current_win() }, {
    reference,
    { id = "field", path = "src/arena.rs", line = 24, owner = "ArenaPlugin::config",
      owner_capture = { "@type", "@variable.member" }, name = "ArenaConfig", kind = "type",
      text = "\t  config: ArenaConfig,", text_highlight = {} },
    { id = "import", path = "src/arena.rs", line = 3, owner = "",
      owner_capture = { vim.NIL, vim.NIL, "@type" }, name = "crate::config::ArenaConfig", kind = "import",
      text = "use crate::config::{ArenaConfig, ConfigError};", text_highlight = {} },
  }, { position = { row = 0, column = 0 } })
  local page = specification.page_list[1]
  assert(page.show_item_counter == true, "references must enable the shared item counter")
  assert(page.highlight_selected_line == true and page.highlight_selected_text == false,
    "references must select by background without changing syntax foregrounds")
  assert(vim.deep_equal(page.column_headers, { "Location", "Caller", "Text" }))
  local method = page.option_list[1]
  assert(method.columns[3] == reference.text:sub(3) and method.label:find(method.columns[3], 1, true),
    "reference text must trim leading whitespace and remain searchable")
  assert(reference.text:sub(1, 2) == "  ", "display trimming must not modify the reference source")
  assert(vim.deep_equal(method.column_spans[3], { { first = 0, last = 3, group = "@keyword.rust", priority = 100 } }),
    "reference text did not retain its source syntax capture")
  assert(method.columns[2] == "RoundPlugin::new" and not method.label:find("@impl", 1, true))
  assert(method.value == reference and reference.owner == "RoundPlugin::@impl:0::new",
    "caller display changed the navigation identity")
  assert(vim.deep_equal(method.column_segments[2], {
    { "RoundPlugin", "@type" }, { "::", "@punctuation.delimiter" }, { "new", "@function" },
  }), "impl display removal shifted caller captures")
  assert(page.option_list[2].column_segments[2][3][2] == "@variable.member",
    "field owners inherited callable highlighting")
  assert(page.option_list[2].columns[3] == "config: ArenaConfig,", "Text must trim tabs as well as spaces")
  assert(page.option_list[3].column_segments[2][5][2] == "@type",
    "imported targets lost their declaration capture")
  assert(vim.deep_equal(method.column_segments[1], {
    { "src/", "ForgeDirName" }, { "arena.rs", "ForgeFileName" }, { ":30", "ForgePickerHint" },
  }))
end, debug.traceback)
picker.open = original_open
assert(success, failure)
print("plan_references: passed")
