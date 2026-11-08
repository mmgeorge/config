-- Status renders real TypeScript, arrow-function, Rust, and impl semantic hunks with
-- syntax-highlighted context and omission cells in a 120x32 grid. The case attaches a UI of that
-- size to a child Neovim, which reruns this file with `vim.g.forge_context_grid` set.
if vim.g.forge_context_grid then
  vim.loader.enable(false)
  local config = vim.fn.getcwd()
  vim.opt.runtimepath:prepend(config .. "/nvim")
  local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
  local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
  local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
  local host = dofile("nvim/lua/forge/tests/support/host.lua")
  local builder = require("forge.builder")
  assert(vim.fn.executable(builder.binary_path()) == 1, "build the release Forge host before this test")

  local fixture = vim.fn.tempname() .. "-forge-semantic-hunks"
  local repository = git_repository.create({ path = fixture })

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

  repository.run({ "config", "core.autocrlf", "false" })
  for _, entry in ipairs(source) do vim.fn.writefile(source_rows(entry, false), fixture .. "/" .. entry.path) end
  repository.run({ "add", "." })
  repository.run({ "commit", "--quiet", "-m", "semantic hunk fixture" })
  for _, entry in ipairs(source) do vim.fn.writefile(source_rows(entry, true), fixture .. "/" .. entry.path) end

  local client = require("forge.client")
  client._host_set_launcher_for_test(vim.system)
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

  local success, failure = xpcall(function()
    vim.api.nvim_set_hl(0, "Normal", { fg = 0xffffff, bg = 0 })
    vim.api.nvim_set_hl(0, "ForgeHunkHeader", { fg = 0xaa0000 })
    vim.api.nvim_set_hl(0, "ForgeHunkBoundary", { fg = 0xaaaaaa })
    vim.api.nvim_set_hl(0, "ForgeAddBg", { bg = 0x123425 })
    vim.api.nvim_set_hl(0, "ForgeDeleteBg", { bg = 0x351a22 })
    local function inspect_source(text, expected_color, expected_background)
      vim.cmd("normal! zR")
      local source_row
      for row, line in ipairs(buffer.lines(state.replica.buffer)) do
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
      if expected_background then
        local edge = vim.api.nvim__inspect_cell(1, screen_row - 1, 119)
        assert(edge[2].background == expected_background, "status diff background stopped before the window edge: " .. vim.inspect(edge))
      end
    end
    wait_until(function() return state.ready and state.replica.status == "Applied" end, "status did not open",
      { timeout_ms = 4000 })
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
      wait_until(function()
        return state.replica.file[file.id].body and state.done[file.id] and not state.request_active
      end, "semantic context did not load for " .. entry.path, { timeout_ms = 4000 })
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
      inspect_source(("%s%s value_8 = 8;"):format(indent, entry.binding), 0x22ddaa, 0x351a22)
      inspect_source(("%s%s value_8 = 908;"):format(indent, entry.binding), 0x22ddaa, 0x123425)
    end
    assert(#notices == 0, table.concat(notices, "\n"))
  end, debug.traceback)

  status.close(state)
  local stopped = host.stop()
  vim.notify = original_notify
  vim.fn.delete(fixture, "rf")
  if not success then error(failure, 0) end
  assert(stopped, "Forge host did not stop")
  return
end

local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["status_virtual_hunk_merge"] = function()
  local test_path = debug.getinfo(1, "S").source:sub(2)
  local child = MiniTest.new_child_neovim()
  MiniTest.finally(function() pcall(child.stop) end)
  child.start({ "-i", "NONE" })
  child.api.nvim_ui_attach(120, 32, { rgb = true })
  child.lua("vim.g.forge_context_grid = true; dofile(...)", { test_path })
end

return T
