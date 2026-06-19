vim.loader.enable(false)

local otter_calls = {}
local otter_rafts = {}
local register_calls = {}

vim.treesitter.language.register = function(language, filetype)
  register_calls[#register_calls + 1] = {
    language = language,
    filetype = filetype,
  }
end

package.loaded["otter.keeper"] = { rafts = otter_rafts }
package.loaded["otter"] = {
  activate = function(languages, completion, diagnostics, tsquery)
    local buf = vim.api.nvim_get_current_buf()
    otter_calls[#otter_calls + 1] = {
      kind = "activate",
      buf = buf,
      name = vim.api.nvim_buf_get_name(buf),
      languages = languages,
      completion = completion,
      diagnostics = diagnostics,
      tsquery = tsquery,
    }
    local lines = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n")
    if lines:find("```ts", 1, true) then otter_rafts[buf] = { languages = { "typescript" } } end
  end,
  sync_raft = function(buf)
    otter_calls[#otter_calls + 1] = {
      kind = "sync",
      buf = buf,
      name = vim.api.nvim_buf_get_name(buf),
    }
  end,
  deactivate = function()
    local buf = vim.api.nvim_get_current_buf()
    otter_calls[#otter_calls + 1] = {
      kind = "deactivate",
      buf = buf,
      name = vim.api.nvim_buf_get_name(buf),
    }
    otter_rafts[buf] = nil
  end,
}

local markdown_code = require("markdown_code")

local function assert_true(condition, message)
  if not condition then error(message, 2) end
end

local function set_lines(buf, lines)
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
end

local function reset()
  markdown_code._reset_for_test()
  otter_calls = {}
  otter_rafts = {}
  register_calls = {}
  package.loaded["otter.keeper"].rafts = otter_rafts
end

local function run_tests()
  reset()
  local markdown_buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(markdown_buf, "D:/tmp/markdown-code-test.md")
  vim.bo[markdown_buf].filetype = "markdown"
  set_lines(markdown_buf, {
    "# Markdown",
    "",
    "```ts",
    "interface MyFooBar {",
    "  value: number;",
    "}",
    "```",
  })
  assert_true(markdown_code.activate(markdown_buf, { filetype = "markdown" }), "markdown buffer did not activate")
  assert_true(#otter_calls == 1, "markdown buffer should activate otter once")
  assert_true(#register_calls == 0, "markdown buffer should not re-register the markdown filetype")
  assert_true(otter_calls[1].name == "D:/tmp/markdown-code-test.md", "markdown buffer should keep its normal file name")
  assert_true(otter_calls[1].completion == true, "markdown otter completion should default on")
  assert_true(otter_calls[1].diagnostics == true, "markdown otter diagnostics should default on")

  reset()
  local issue_buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(issue_buf, "github://issue/org/repo/12")
  vim.bo[issue_buf].filetype = "GithubIssue"
  set_lines(issue_buf, {
    "Description:",
    "```ts",
    "interface MyFooBar {",
    "  value: number;",
    "}",
    "```",
  })
  assert_true(
    markdown_code.activate(issue_buf, { filetype = "GithubIssue", register_as_markdown = true }),
    "custom markdown buffer did not activate"
  )
  assert_true(#otter_calls == 1, "custom markdown buffer should activate otter once")
  assert_true(#register_calls == 1, "custom markdown buffer should register its filetype once")
  assert_true(register_calls[1].language == "markdown", "custom markdown buffer should register markdown language")
  assert_true(register_calls[1].filetype == "GithubIssue", "custom markdown buffer registered the wrong filetype")
  assert_true(not otter_calls[1].name:find("://", 1, true), "custom markdown buffer should activate with temp name")
  assert_true(
    vim.api.nvim_buf_get_name(issue_buf) == "github://issue/org/repo/12",
    "custom markdown buffer name should be restored after otter activation"
  )

  assert_true(markdown_code.activate(issue_buf, { filetype = "GithubIssue", register_as_markdown = true }), "active buffer did not sync")
  assert_true(#otter_calls == 1, "unchanged active buffer should not resync")
  set_lines(issue_buf, {
    "Description:",
    "```ts",
    "interface MyFooBar {",
    "  value: number;",
    "  label: string;",
    "}",
    "```",
  })
  assert_true(markdown_code.activate(issue_buf, { filetype = "GithubIssue", register_as_markdown = true }), "changed active buffer did not sync")
  assert_true(otter_calls[#otter_calls].kind == "sync", "changed active buffer should sync otter raft")
  set_lines(issue_buf, {
    "Description:",
    "```ts",
    "interface MyFooBar {",
    "  value: number;",
    "}",
    "```",
    "```python",
    "print('hi')",
    "```",
  })
  assert_true(
    markdown_code.activate(issue_buf, { filetype = "GithubIssue", register_as_markdown = true }),
    "new fenced language did not reactivate otter"
  )
  assert_true(otter_calls[#otter_calls - 1].kind == "deactivate", "new fenced language should deactivate old otter raft")
  assert_true(otter_calls[#otter_calls].kind == "activate", "new fenced language should activate a new otter raft")

  reset()
  local status_buf = vim.api.nvim_create_buf(true, false)
  vim.bo[status_buf].filetype = "GitStatus"
  set_lines(status_buf, {
    "Modified src/main.rs +1 -0",
    "    let value = 1;",
    "```ts",
    "interface ShouldNotActivate {",
    "  value: number;",
    "}",
    "```",
  })
  assert_true(not markdown_code.activate(status_buf, { filetype = "GitStatus" }), "GitStatus should not activate markdown code by default")
  assert_true(#register_calls == 0, "GitStatus should not register as markdown by default")
  assert_true(#otter_calls == 0, "GitStatus should not activate otter by default")

  assert_true(
    markdown_code.activate(status_buf, { filetype = "GitStatus", register_as_markdown = true }),
    "explicit GitStatus markdown region should activate"
  )
  assert_true(#register_calls == 1, "explicit GitStatus markdown region should register once")
  assert_true(register_calls[1].filetype == "GitStatus", "explicit GitStatus registration used the wrong filetype")
end

local ok, err = xpcall(run_tests, debug.traceback)
if not ok then
  print(err)
  vim.cmd("cquit")
end

print("markdown_code: ok")
