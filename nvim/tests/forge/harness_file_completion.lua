vim.loader.enable(false)
for _, plugin in ipairs({ "blink.cmp", "snacks.nvim" }) do
  local root = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", plugin)
  assert(vim.fn.isdirectory(root) == 1, "harness_file_completion requires " .. plugin)
  vim.opt.runtimepath:append(root)
end

local config = require("plugins.blink")[1].opts
require("blink.cmp.config").merge_with(config)
local draw = require("blink.cmp.config").completion.menu.draw
local label = draw.components.label
local session = require("forge.session")
local source = require("forge.views.harness.completion.file_source")
local column = require("blink.cmp.completion.windows.render.column")
local draw_context = require("blink.cmp.completion.windows.render.context")
local original_system = vim.system
local original_session = session.harness.session
local root = vim.fs.joinpath(vim.fn.getcwd(), "fixture-workspace")
local path = "nvim/lua/forge/features/completion/providers/file_completion.lua"

local ok, failure = xpcall(function()
  session.harness.session = { workspace = root }
  vim.o.columns = 100
  vim.o.virtualedit = "onemore"
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { "inspect @completion" })
  vim.api.nvim_win_set_cursor(0, { 1, #"inspect @completion" })
  source._clear_cache()
  vim.system = function(_, options, callback)
    assert(options.cwd == root, "file discovery used Neovim cwd instead of the workspace")
    vim.schedule(function() callback({ code = 0, stdout = path .. "\n", stderr = "" }) end)
  end
  local result
  source.new():get_completions({}, function(value) result = value end)
  assert(vim.wait(1000, function() return result ~= nil end), "file completion timed out")
  local completion = result.items[1]
  assert(completion.label == "@" .. path)
  assert(completion.filterText == "@" .. path)
  completion.source_id = "harness_files"
  completion.source_name = "Workspace files"
  local context = draw_context.new(draw, 1, completion, { 1, #completion.label - 1 })
  local rendered = column.new({ "label" }, { label }, 0, false)
  rendered:render({}, { context })
  local text = vim.trim(rendered:get_line_text(1))
  local expected = "@" .. require("snacks").picker.util.truncpath(
    vim.fs.joinpath(root, path), 59, { cwd = root, kind = "center" }
  )
  assert(text == expected, "Blink did not render Snacks middle shortening: " .. text)
  assert(text:match("^@nvim/") and text:match("/file_completion%.lua$"))
  assert(not text:find(root, 1, true), "workspace prefix leaked into the label")
  local highlights = label.highlight(context, text)
  assert(highlights[#highlights - 1][1] == 1, "prefix match highlight moved")
  assert(highlights[#highlights][1] == #text - 1, "filename match highlight moved")

  vim.o.columns = 35
  local narrow = label.text(context)
  assert(narrow:match("^@nvim/") and narrow:match("lua$"), narrow)
  assert(vim.api.nvim_strwidth(narrow) <= 29, "narrow label exceeded the available width")
  assert(narrow ~= text, "label did not adapt to the narrower viewport")

  completion.label = "@README.md"
  context = draw_context.new(draw, 1, completion, {})
  assert(label.text(context) == "@README.md", "short basename changed")
  completion.label = "@src/" .. string.rep("long_filename", 10) .. ".lua"
  context = draw_context.new(draw, 1, completion, { 10, #completion.label - 1 })
  local long = label.text(context)
  assert(long:match("^@src/") and long:match("%.lua$"), long)
  assert(vim.api.nvim_strwidth(long) <= 29, "long filename exceeded the available width")
  highlights = label.highlight(context, long)
  assert(#highlights == 3, "removed filename characters retained match highlights")
  assert(highlights[#highlights][1] == #long - 1, "truncated filename match highlight moved")

  local ordinary = draw_context.new(draw, 1, {
    label = "ordinary", labelDetails = { detail = " detail" },
    source_id = "lsp", source_name = "LSP", kind = vim.lsp.protocol.CompletionItemKind.Text,
  }, { 0 })
  local default = require("blink.cmp.config.completion.menu").default.draw.components.label
  assert(label.text(ordinary) == default.text(ordinary), "ordinary completion rendering changed")
  assert(vim.deep_equal(label.highlight(ordinary, "ordinary detail"), default.highlight(ordinary)),
    "ordinary completion highlighting changed")

  vim.lsp.util.apply_text_edits({ completion.textEdit }, vim.api.nvim_get_current_buf(), "utf-8")
  assert(vim.api.nvim_get_current_line() == "inspect @" .. path, "short label changed the inserted path")
end, debug.traceback)
vim.system = original_system
session.harness.session = original_session
source._clear_cache()
assert(ok, failure)
print("harness_file_completion OK")
vim.cmd("qa!")
