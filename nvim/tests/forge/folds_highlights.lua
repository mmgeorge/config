vim.loader.enable(false)
local replica = require("forge.buffer")
local input = require("forge.input")
local session = replica.open("fold-highlights")
local function span(capture, priority, first, last)
  return { capture = capture, priority = priority,
    range = { start = first, ["end"] = last } }
end
local source = { document = session.document, revision = 0, block = {} }
for index, background in ipairs({ "ForgeAddBg", "ForgeDeleteBg", "RenderMarkdownCode" }) do
  local id = "container" .. index
  source.block[#source.block + 1] = { id = id,
    text = { "pub enum Error {", "  Invalid,", "}" }, metadata = {
      decoration = { span(background, 50, { row = 0, column = 0 }, { row = 3, column = 0 }) },
      visible_decoration = {
        span("Keyword", 100, { row = 0, column = 0 }, { row = 0, column = 8 }),
        span("ForgeInlineAddBg", 200, { row = 0, column = 9 }, { row = 0, column = 14 }),
        span("Identifier", 200, { row = 0, column = 9 }, { row = 0, column = 14 }),
      },
      gutter = { { position = { row = 0, column = 0 }, priority = 100,
        chunk = { { text = "1 + ", capture = "LineNr" } } } },
      fold = { { id = id .. ":fold", closed = true, collapsed_suffix = "...}",
        start = { row = 0, column = 0 }, ["end"] = { block = id, position = { row = 3, column = 0 } } } },
      target = {}, editable_region = {},
    } }
end
local view
local success, failure = xpcall(function()
  vim.api.nvim_set_hl(0, "Normal", { fg = "#ffffff", bg = "#000000" })
  vim.api.nvim_set_hl(0, "ForgeAddBg", { bg = "#003300" })
  vim.api.nvim_set_hl(0, "ForgeDeleteBg", { bg = "#330000" })
  vim.api.nvim_set_hl(0, "RenderMarkdownCode", { bg = "#002244" })
  vim.api.nvim_set_hl(0, "ForgeInlineAddBg", { bg = "#005500" })
  vim.api.nvim_set_hl(0, "Keyword", { fg = "#ffff00" })
  vim.api.nvim_set_hl(0, "Identifier", { fg = "#00ffff" })
  assert(replica.apply_snapshot(session, source).kind == "Applied")
  vim.api.nvim_set_current_buf(session.buffer)
  view = input.open(session, 0)
  vim.wo.number = true
  vim.wo.numberwidth = 4
  _G.forge_capture_fold = function()
    local chunks = require("forge.folds").text()
    _G.forge_fold_chunks = chunks
    return chunks
  end
  vim.wo.foldtext = "v:lua.forge_capture_fold()"
  for index, background in ipairs({ "ForgeAddBg", "ForgeDeleteBg", "RenderMarkdownCode" }) do
    local row = (index - 1) * 3 + 1
    assert(vim.fn.foldclosed(row) == row)
    local summary = vim.fn.foldtextresult(row)
    assert(summary:find("pub enum Error {...}", 1, true), "fold summary changed declaration text")
    local chunks, width, keyword, inline, suffix = _G.forge_fold_chunks, 0, false, false, false
    for _, chunk in ipairs(chunks) do
      width = width + vim.fn.strdisplaywidth(chunk[1], width)
      local capture = type(chunk[2]) == "table" and chunk[2] or { chunk[2] }
      assert(capture[1] == background, "fold chunk lost its row background")
      if chunk[1]:find("pub enum", 1, true) then keyword = vim.tbl_contains(capture, "Keyword") end
      if chunk[1] == "Error" then
        inline = vim.deep_equal(capture, { background, "ForgeInlineAddBg", "Identifier" })
      end
      if chunk[1] == "...}" then suffix = vim.tbl_contains(capture, "Comment") end
    end
    assert(keyword and inline and suffix, "fold highlight precedence lost syntax or body marker")
    assert(width == vim.api.nvim_win_get_width(0) - vim.fn.getwininfo(vim.api.nvim_get_current_win())[1].textoff,
      "fold background did not fill the available text width")
  end
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(session.buffer, 0, -1, false),
    { "pub enum Error {", "  Invalid,", "}", "pub enum Error {", "  Invalid,", "}", "pub enum Error {", "  Invalid,", "}" }),
    "fold presentation inserted source padding")
end, debug.traceback)
assert(success, failure)
if not vim.g.forge_test_interactive then
  input.close(view)
  replica.close(session)
  print("folds_highlights: passed")
else
  _G.forge_fold_test = { session = session, view = view }
end
