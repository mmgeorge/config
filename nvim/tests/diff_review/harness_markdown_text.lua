vim.loader.enable(false)

local markdown_text = require("diff_review.render.harness.markdown_text")
local blocks = require("markdown_math.blocks")

local function fail(message)
  vim.api.nvim_err_writeln(message)
  vim.cmd("cquit")
end

local function assert_equals(actual, expected, message)
  if actual ~= expected then
    fail(("%s\nexpected: %q\nactual:   %q"):format(message, expected, actual))
  end
end

local multiline = table.concat({
  "The equation is:",
  "",
  "$$",
  "L_o(x,\\omega_o)",
  "=",
  "L_e(x,\\omega_o)",
  "+",
  "\\int_{\\Omega} L_i(x,\\omega_i)",
  "$$",
}, "\n")
assert_equals(
  markdown_text.normalize_math(multiline),
  "The equation is:\n\n$$ L_o(x,\\omega_o) = L_e(x,\\omega_o) + \\int_{\\Omega} L_i(x,\\omega_i) $$",
  "multiline display math must become one parser-safe node"
)

assert_equals(
  markdown_text.normalize_math("Where \\(L_o\\) meets \\(L_i\\)."),
  "Where $L_o$ meets $L_i$.",
  "parenthesized inline math must use Markdown delimiters"
)

assert_equals(
  markdown_text.normalize_math("\\[\na = b\n\\]"),
  "$$ a = b $$",
  "bracketed display math must use a parser-safe Markdown node"
)

local fenced = "```latex\n$$\na = b\n$$\n```"
assert_equals(markdown_text.normalize_math(fenced), fenced, "code-fenced math must remain literal")

local incomplete = "Before\n$$\na = b"
assert_equals(markdown_text.normalize_math(incomplete), incomplete, "streaming display math must remain untouched until closed")

local block_list = blocks.find(vim.split(multiline, "\n", { plain = true }))
assert_equals(#block_list, 1, "complete display math must produce one physical-buffer block")
assert_equals(block_list[1].start_row, 2, "display block start must use a zero-based row")
assert_equals(block_list[1].end_row, 9, "display block end must exclude the closing-delimiter row")
assert_equals(
  block_list[1].input,
  "L_o(x,\\omega_o) = L_e(x,\\omega_o) + \\int_{\\Omega} L_i(x,\\omega_i)",
  "display block input must collapse Markdown-hostile newlines before conversion"
)
assert_equals(#blocks.find(vim.split(fenced, "\n", { plain = true })), 0, "fenced code must not produce display blocks")

io.write("harness_markdown_text OK\n")
vim.cmd("qa!")
