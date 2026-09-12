vim.loader.enable(false)
local buffer = require("forge.buffer")
local input = require("forge.input")
local session = buffer.open("conceal-test")
local source = { "**日本語**", "```", "body" }
local function range(first, last)
  return { start = { row = 0, column = first }, ["end"] = { row = 0, column = last } }
end
local metadata = {
  target = {}, decoration = {}, editable_region = {},
  visible_decoration = { { range = range(2, 11), capture = "String", priority = 100 } },
  conceal = {
    { range = range(0, 2), replacement = "", line = false, priority = 100 },
    { range = range(11, 13), replacement = "", line = false, priority = 100 },
    { range = { start = { row = 1, column = 0 }, ["end"] = { row = 1, column = 3 } },
      replacement = "", line = true, priority = 100 },
  },
  fold = { { id = "task", start = { row = 0, column = 0 },
    ["end"] = { block = "source", position = { row = 3, column = 0 } }, closed = true } },
}
local snapshot = { document = session.document, revision = 0,
  block = { { id = "source", text = source, metadata = metadata } } }
local success, failure = xpcall(function()
  local result = buffer.apply_snapshot(session, snapshot)
  assert(result.kind == "Applied", vim.inspect(result))
  vim.api.nvim_set_current_buf(session.buffer)
  vim.wo.conceallevel = 2
  local view = input.open(session, 0, { conceal = { level = 2, cursor = "" } })
  assert(vim.fn.foldtextresult(1) == "日本語", "fold summary retained Markdown markers: " .. vim.inspect({ text = vim.fn.foldtextresult(1), level = vim.wo.conceallevel }))
  vim.wo.conceallevel = 0
  assert(vim.fn.foldtextresult(1) == source[1], "disabled concealment changed fold text")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), source))
  local marks = vim.api.nvim_buf_get_extmarks(session.buffer, session.namespace, 0, -1, { details = true })
  local concealed = 0
  for _, mark in ipairs(marks) do if mark[4].conceal ~= nil then concealed = concealed + 1 end end
  assert(concealed == 3, "native conceal ranges are missing")
  local invalid = vim.deepcopy(snapshot)
  invalid.revision = 1
  invalid.block[1].metadata.conceal[1].range.start.column = 3
  invalid.block[1].metadata.conceal[1].range["end"].column = 4
  assert(buffer.apply_snapshot(session, invalid).kind == "Desynchronized", "conceal accepted a split Unicode character")
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, true), source))
  input.close(view)
end, debug.traceback)
buffer.close(session)
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("Native concealment, source preservation, fold display, and invalid-range rejection passed")
vim.cmd("qa!")
