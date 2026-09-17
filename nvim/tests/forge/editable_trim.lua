vim.opt.runtimepath:append("nvim")
local editable = require("forge.editable")
for _, sample in ipairs({
  { source = "test\n\n", expected = "test" },
  { source = "first\n\nlast\n", expected = "first\n\nlast" },
  { source = "first\r\nlast\r\n\r\n", expected = "first\r\nlast" },
  { source = "text  \n\n", expected = "text  " },
  { source = "\n\n", expected = "" },
  { source = "", expected = "" },
  { source = "λ🙂\n", expected = "λ🙂" },
  { source = " unchanged ", expected = " unchanged " },
}) do
  local source = vim.split(sample.source, "\n", { plain = true })
  local buffer = vim.api.nvim_create_buf(false, true)
  local lines = vim.deepcopy(source)
  lines[1] = "Label: " .. lines[1]
  lines[#lines] = lines[#lines] .. " suffix"
  lines[#lines + 1] = "Other input"
  vim.api.nvim_buf_set_lines(buffer, 0, -1, false, lines)
  local state = editable.new("trim")
  editable.register(state, "body", 0)
  editable.register(state, "other", 0)
  editable.attach(state, buffer, {
    body = { start = { row = 0, column = 7 },
      finish = { row = #source - 1, column = #source[#source] + (#source == 1 and 7 or 0) } },
    other = { start = { row = #source, column = 0 }, finish = { row = #source, column = 11 } },
  }, { send = function() return true end })
  vim.bo[buffer].modifiable = false
  local text, failure = editable.trim_trailing_newlines(state, "body")
  assert(text == sample.expected, failure or vim.inspect(text))
  assert(not vim.bo[buffer].modifiable and not state.fault, state.fault)
  local expected = vim.split("Label: " .. sample.expected .. " suffix\nOther input", "\n", { plain = true })
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), expected))
  assert(state.native.anchor.other.start.row == #expected - 1, "trim corrupted the next input's anchor")
  if sample.source ~= sample.expected then
    assert(table.concat(editable.recoverable_text(state, "body"), "\n") == sample.expected)
  end
  local sequence = state.sequence
  assert(editable.trim_trailing_newlines(state, "body") == sample.expected)
  assert(sequence == state.sequence, "repeated trim generated another edit")
  editable.detach(state)
  vim.api.nvim_buf_delete(buffer, { force = true })
end
print("editable_trim: trailing line breaks, internal paragraphs, Unicode, empty inputs, and adjacent boundaries passed")
