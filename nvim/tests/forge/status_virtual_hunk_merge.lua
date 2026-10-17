vim.loader.enable(false)
local config = vim.fn.getcwd()
if not vim.g.forge_context_grid then
  local test_path = debug.getinfo(1, "S").source:sub(2)
  local child = vim.fn.jobstart({ vim.v.progpath, "--embed", "--headless", "-u", "NONE", "-i", "NONE" }, { rpc = true })
  assert(child > 0)
  local success, failure = xpcall(function()
    vim.rpcrequest(child, "nvim_ui_attach", 120, 32, { rgb = true })
    vim.rpcrequest(child, "nvim_exec_lua", "vim.g.forge_context_grid = true; dofile(...)", { test_path })
  end, debug.traceback)
  vim.fn.jobstop(child)
  if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
  print("status_virtual_hunk_merge: syntax-highlighted context and omission cells passed at 120x32")
  vim.cmd("qa!")
  return
end
vim.opt.runtimepath:prepend(config .. "/nvim")
local builder = require("forge.builder")
if vim.g.forge_manual_artifact_root then builder._set_artifact_root_for_test(vim.g.forge_manual_artifact_root) end
assert(vim.fn.executable(builder.binary_path()) == 1, "build the release Forge host before this test")

local fixture = vim.fn.tempname() .. "-forge-semantic-hunks"
vim.fn.mkdir(fixture, "p")

---@param arguments string[]
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end

---@type { path: string, declaration: string, binding: string, outer?: string }[]
local source = {
  { path = "function.ts", declaration = "export function foo(): number {", binding = "const" },
  { path = "arrow.ts", declaration = "export const foo = (): number => {", binding = "const" },
  { path = "function.rs", declaration = "pub fn foo() -> i32 {", binding = "let" },
  { path = "method.rs", outer = "impl Foo {", declaration = "    fn foobar(&self) -> i32 {", binding = "let" },
}

---@param entry { declaration: string, binding: string, outer?: string }
---@param changed boolean
---@return string[]
local function source_rows(entry, changed)
  local rows = { entry.declaration }
  if entry.outer then table.insert(rows, 1, entry.outer) end
  local indent = entry.outer and "        " or "    "
  for row = 1, 44 do
    local value = changed and vim.tbl_contains({ 8, 13, 22, 36 }, row) and 900 + row or row
    rows[#rows + 1] = ("%s%s value_%d = %d;"):format(indent, entry.binding, row, value)
  end
  vim.list_extend(rows, { indent .. "return value_44;", entry.outer and "    }" or "}" })
  if entry.outer then rows[#rows + 1] = "}" end
  return rows
end

git({ "init", "--quiet" })
git({ "config", "user.name", "Forge Fixture" })
git({ "config", "user.email", "forge@example.invalid" })
git({ "config", "core.autocrlf", "false" })
for _, entry in ipairs(source) do vim.fn.writefile(source_rows(entry, false), fixture .. "/" .. entry.path) end
git({ "add", "." })
git({ "commit", "--quiet", "-m", "semantic hunk fixture" })
for _, entry in ipairs(source) do vim.fn.writefile(source_rows(entry, true), fixture .. "/" .. entry.path) end

local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local github = require("forge.integrations.gh")
github.prs_for_branch_async = function(_, _, _, callback) vim.schedule(function() callback({ ok = true, prs = {} }) end) end
github.current_pr_async = function(_, callback) vim.schedule(function() callback({ ok = true }) end) end
---@type string[]
local notices = {}
local original_notify = vim.notify
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
end
require("forge").setup({ about_auto_generate = false })
local status = require("forge.status")
local state = status.open({ workspace = fixture })
---@type table<integer, boolean>
local context_ready = {}
local unsubscribe = client.subscribe_document(state.document, function(event, payload)
  if event == "status.update" and payload.phase == "context" then
    for _, body in ipairs(payload.body or {}) do context_ready[body.file] = true end
  end
end)

local success, failure = xpcall(function()
  vim.api.nvim_set_hl(0, "Normal", { fg = 0xffffff, bg = 0 })
  vim.api.nvim_set_hl(0, "ForgeHunkHeader", { fg = 0xaa0000 })
  vim.api.nvim_set_hl(0, "ForgeHunkBoundary", { fg = 0xaaaaaa })
  local function inspect_source(text, expected_color)
    vim.cmd("normal! zR")
    local source_row
    for row, line in ipairs(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)) do
      if line == text then source_row = row break end
    end
    assert(source_row, "missing physical context: " .. text)
    vim.api.nvim_win_set_cursor(0, { source_row, 0 })
    vim.cmd("normal! zz")
    vim.cmd("redraw!")
    local screen_row = vim.fn.screenpos(vim.api.nvim_get_current_win(), source_row, 1).row
    local cells = {}
    for column = 1, 120 do cells[#cells + 1] = vim.fn.screenstring(screen_row, column) end
    local screen_text = table.concat(cells)
    local byte_column = assert(screen_text:find(vim.trim(text), 1, true), "source text missing from rendered cells")
    local column = vim.fn.strdisplaywidth(screen_text:sub(1, byte_column - 1)) + 1
    local cell = vim.api.nvim__inspect_cell(1, screen_row - 1, column - 1)
    assert(cell[2].foreground == expected_color, "context foreground differs: " .. vim.inspect(cell))
  end
  assert(vim.wait(10000, function() return state.ready and state.replica.status == "Applied" end, 10), "status did not open")
  for _, entry in ipairs(source) do
    local file
    for _, candidate in ipairs(state.replica.inventory.file) do
      if candidate.path == entry.path then file = candidate break end
    end
    assert(file, "missing file " .. entry.path)
    local _, row = state.replica.sequence:position("file:" .. file.id)
    vim.api.nvim_win_set_cursor(0, { row + 1, 0 })
    require("forge.nodes").set_open(state.replica, 0, "file:" .. file.id, true)
    status.demand(state)
    assert(vim.wait(10000, function()
      return context_ready[file.id] and state.replica.file[file.id].body and state.done[file.id] and not state.request_active
    end, 10), "semantic context did not load for " .. entry.path)
    local body = state.replica.file[file.id].body
    ---@type string[]
    local headers = {}
    local declaration_count, closing_count, bridge_count, hidden_count, gap_count = 0, 0, 0, 0, 0
    local gap_text
    local indent = entry.outer and "        " or "    "
    for _, block in pairs(body.block) do
      for index, text in ipairs(block.text) do
        if text:match("^@@") then headers[#headers + 1] = text end
        if text == entry.declaration then declaration_count = declaration_count + 1 end
        if text == "}" then closing_count = closing_count + 1 end
        if text == ("%s%s value_18 = 18;"):format(indent, entry.binding) then bridge_count = bridge_count + 1 end
        if text == ("%s%s value_29 = 29;"):format(indent, entry.binding) then hidden_count = hidden_count + 1 end
        if vim.trim(text) == "..." then
          gap_count = gap_count + 1
          gap_text = gap_text or text
          for _, target in ipairs(block.metadata.target or {}) do
            assert(target.range.start.row ~= index - 1, "omission marker has an action target")
          end
        end
      end
      for _, capture in ipairs(block.metadata.visible_decoration or {}) do
        if capture.capture:sub(1, 1) == "@" then
          vim.api.nvim_set_hl(0, capture.capture, { fg = 0x22ddaa })
        end
      end
    end
    table.sort(headers)
    local expected = { "@@ +1 -1", "@@ +3 -3" }
    assert(vim.deep_equal(headers, expected), entry.path .. ": " .. vim.inspect(headers))
    assert(declaration_count == 1 and closing_count == 1, entry.path .. ": repeated or missing scope boundaries")
    assert(bridge_count == 1, entry.path .. ": missing or duplicated bridge context")
    assert(hidden_count == 0, entry.path .. ": distant changes merged")
    assert(gap_count > 0, entry.path .. ": omitted lines have no visible marker")
    inspect_source(entry.outer or entry.declaration, 0x22ddaa)
    inspect_source(entry.declaration, 0x22ddaa)
    inspect_source(gap_text, 0xaaaaaa)
  end
  assert(#notices == 0, table.concat(notices, "\n"))
end, debug.traceback)

unsubscribe()
status.close(state)
client.stop()
assert(vim.wait(10000, function() return client._client.process == nil end, 20), "Forge host did not stop")
vim.notify = original_notify
if not success then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
end
print("status_virtual_hunk_merge: real TypeScript, arrow, Rust, and impl semantic grouping passed")
