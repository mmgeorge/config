if vim.g.forge_language_grid then
  local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
  local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
  local git_repository = dofile("nvim/lua/forge/tests/support/git_repository.lua")
  local function render_language_grid()
    vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
    local fixture = vim.fn.tempname() .. "-forge-languages"
    ---@type { path: string, language: string, rows: string[], changed: string, highlighted: string, changed_row: integer }[]
    local source = {
      { path = "client.py", language = "python", rows = { "def calculate(value: int) -> int:", "    return value + 1" },
        changed_row = 2, changed = "    return value + 2", highlighted = "def calculate(value: int) -> int:" },
      { path = "schema.cue", language = "cue", rows = { "package schema", "#Server: {", "    port: int & >0", "}" },
        changed_row = 3, changed = "    port: int & >1024", highlighted = "    port: int & >1024" },
      { path = "client.c", language = "c", rows = { "int calculate(int value) {", "    return value + 1;", "}" },
        changed_row = 2, changed = "    return value + 2;", highlighted = "    return value + 2;" },
      { path = "client.cpp", language = "cpp", rows = { "template<typename Value>", "Value calculate(Value value) {", "    return value + 1;", "}" },
        changed_row = 3, changed = "    return value + 2;", highlighted = "    return value + 2;" },
      { path = "highlights.scm", language = "query", rows = { "((identifier) @variable", ' (#eq? @variable "old"))' },
        changed_row = 2, changed = ' (#eq? @variable "new"))', highlighted = ' (#eq? @variable "new"))' },
      { path = "client.nu", language = "nu", rows = { "def calculate [value: int] {", "    $value + 1", "}" },
        changed_row = 2, changed = "    $value + 2", highlighted = "    $value + 2" },
      { path = "shader.slang", language = "slang", rows = { "float calculate(float value) {", "    return value + 1.0;", "}" },
        changed_row = 2, changed = "    return value + 2.0;", highlighted = "    return value + 2.0;" },
    }
    local repository = git_repository.create({ path = fixture })
    repository.run({ "config", "core.autocrlf", "false" })
    for _, entry in ipairs(source) do vim.fn.writefile(entry.rows, fixture .. "/" .. entry.path) end
    repository.run({ "add", "." })
    repository.run({ "commit", "--quiet", "-m", "Language fixture" })
    for _, entry in ipairs(source) do
      local rows = vim.deepcopy(entry.rows)
      rows[entry.changed_row] = entry.changed
      vim.fn.writefile(rows, fixture .. "/" .. entry.path)
    end

    local client = require("forge.client")
    client._host_set_launcher_for_test(vim.system)
    local github = require("forge.integrations.gh")
    github.prs_for_branch_async = function(_, _, _, callback) callback({ ok = true, prs = {} }) end
    github.current_pr_async = function(_, callback) callback({ ok = true }) end
    local notices, original_notify = {}, vim.notify
    vim.notify = function(message, level)
      if level == vim.log.levels.ERROR then notices[#notices + 1] = tostring(message) end
    end
    require("forge").setup({ about_auto_generate = false })
    local status = require("forge.status")
    local state = status.open({ workspace = fixture })
    local success, failure = xpcall(function()
      wait_until(function() return state.ready and state.replica.status == "Applied" end,
        "status document did not open", { timeout_ms = 4000 })
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
        local captures = {}
        wait_until(function()
          local body = state.replica.file[file.id].body
          if not body or not state.done[file.id] or state.request_active then return false end
          for _, block in pairs(body.block) do
            for _, capture in ipairs(block.metadata.visible_decoration or {}) do
              if capture.capture:match("%." .. entry.language .. "$") then
                captures[capture.capture] = true
              end
            end
          end
          return next(captures) ~= nil
        end, "native syntax missing for " .. entry.path, { timeout_ms = 4000 })
        for capture in pairs(captures) do vim.api.nvim_set_hl(0, capture, { fg = 0x22ddaa }) end
        vim.cmd("normal! zR")
        local physical_row
        for index, text in ipairs(buffer.lines(state.replica.buffer)) do
          if text == entry.highlighted then physical_row = index break end
        end
        assert(physical_row, "missing rendered source for " .. entry.path .. "\n" .. buffer.text(state.replica.buffer))
        vim.api.nvim_win_set_cursor(0, { physical_row, 0 })
        vim.cmd("normal! zz")
        vim.cmd("redraw!")
        local screen_row = vim.fn.screenpos(vim.api.nvim_get_current_win(), physical_row, 1).row
        local cells = {}
        for column = 1, 120 do cells[#cells + 1] = vim.fn.screenstring(screen_row, column) end
        local screen_text = table.concat(cells)
        local column = assert(screen_text:find(vim.trim(entry.highlighted), 1, true), screen_text)
        local cell = vim.api.nvim__inspect_cell(1, screen_row - 1, column - 1)
        assert(cell[2].foreground == 0x22ddaa, entry.path .. ": " .. vim.inspect(cell))
      end
      assert(#notices == 0, table.concat(notices, "\n"))
    end, debug.traceback)
    status.close(state)
    client.host_stop()
    local collected = vim.wait(4000, function() return client._client.process == nil end, 5)
    vim.notify = original_notify
    vim.fn.delete(fixture, "rf")
    assert(success, failure)
    assert(collected, "language grid host process was not collected")
  end
  render_language_grid()
  return
end

local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local test_path = debug.getinfo(1, "S").source:sub(2)

local T = MiniTest.new_set({ hooks = isolation.hooks() })

T["language_syntax"] = function()
  local child = MiniTest.new_child_neovim()
  child.start()
  MiniTest.finally(function() pcall(child.stop) end)
  child.api.nvim_ui_attach(120, 32, { rgb = true })
  child.lua("vim.g.forge_language_grid = true; dofile(...)", { test_path })
end

return T
