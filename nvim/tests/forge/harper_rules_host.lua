local executable = vim.fn.exepath("harper-ls")
if executable == "" then
  executable = vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "packages", "harper-ls",
    "harper-ls" .. (vim.fn.has("win32") == 1 and ".exe" or ""))
end
assert(vim.fn.executable(executable) == 1, "Harper must be installed for rule integration")
local root = vim.fn.tempname()
vim.fn.mkdir(root .. "/first", "p")
vim.fn.mkdir(root .. "/second", "p")
local path = root .. "/rules.json"
vim.fn.writefile({ "{}" }, path)
local fixture = dofile("nvim/tests/forge/support/text_fixture.lua").open()
local success, failure = xpcall(function()
  fixture.exec([[
    local executable, root, path = ...
    local previous = vim.fn.stdpath
    vim.fn.stdpath = function(kind) return kind == "cache" and root or previous(kind) end
    require("forge").setup({ text_correction = { enabled = false }, harper = {
      enabled = true, cmd = { executable, "--stdio" }, disabled_rules = path,
      settings = { linters = { SpellCheck = true } },
    } })
    rule_buffers = {}
    for _, project in ipairs({ "first", "second" }) do
      local buffer = vim.api.nvim_create_buf(true, false)
      vim.api.nvim_buf_set_name(buffer, root .. "/" .. project .. "/text.md")
      vim.b[buffer].forge_workspace = root .. "/" .. project
      vim.api.nvim_buf_set_lines(buffer, 0, -1, false, { "The the sentence.", "Use teh spelling." })
      vim.bo[buffer].filetype = "markdown"
      rule_buffers[#rule_buffers + 1] = buffer
    end
  ]], { executable, root, path })
  assert(vim.wait(10000, function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local found = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then found = true end
        end
        if not found then return false end
      end
      return true
    ]])
  end, 10), "Harper did not publish rule identifiers in both projects")
  fixture.exec([[
    local buffer = rule_buffers[1]
    local client = assert(vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1])
    local other = assert(vim.lsp.get_clients({ bufnr = rule_buffers[2], name = "harper_ls" })[1])
    assert(client.id ~= other.id, "fixture did not create independent project clients")
    client:request("textDocument/codeAction", {
      textDocument = { uri = vim.uri_from_bufnr(buffer) },
      range = { start = { line = 0, character = 4 }, ["end"] = { line = 0, character = 4 } },
      context = { diagnostics = {} },
    }, function(error, actions) rule_error = error rule_actions = actions end, buffer)
  ]])
  assert(vim.wait(3000, function() return fixture.exec("return rule_actions ~= nil") end, 10))
  fixture.exec([[
    assert(not rule_error, vim.inspect(rule_error))
    local selected
    for _, action in ipairs(rule_actions) do
      if action.title == 'Disable Harper rule "RepeatedWords" globally' then selected = action end
    end
    assert(selected, vim.inspect(rule_actions))
    local client = assert(vim.lsp.get_clients({ bufnr = rule_buffers[1], name = "harper_ls" })[1])
    client.commands[selected.command.command](selected.command)
  ]])
  assert(vim.wait(5000, function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
        if client.settings["harper-ls"].linters.RepeatedWords ~= false then return false end
        local other_rule = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then return false end
          if diagnostic.code == "The" then other_rule = true end
        end
        if not other_rule then return false end
      end
      return true
    ]])
  end, 10), "Disabling a rule did not refresh both clients while preserving other rules: " .. vim.inspect(fixture.exec([[
    local result = { errors = text_errors, buffers = {} }
    for _, buffer in ipairs(rule_buffers) do
      local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
      result.buffers[#result.buffers + 1] = {
        diagnostics = vim.diagnostic.get(buffer), linters = client and client.settings["harper-ls"].linters,
      }
    end
    return result
  ]])))
  assert(vim.json.decode(table.concat(vim.fn.readfile(path), "\n")).RepeatedWords == false)
  fixture.exec([[
    local buffer = rule_buffers[1]
    local harper = require("forge.text.harper")
    local client = vim.lsp.get_clients({ bufnr = buffer, name = "harper_ls" })[1]
    harper.detach(buffer)
    client:stop(true)
    harper.attach(buffer, { kind = "markdown", root = ... })
  ]], { root .. "/first" })
  assert(vim.wait(5000, function()
    return fixture.exec([[
      local client = vim.lsp.get_clients({ bufnr = rule_buffers[1], name = "harper_ls" })[1]
      return client and client.initialized and client.settings["harper-ls"].linters.RepeatedWords == false
    ]])
  end, 10), "A new Harper client did not load persisted rules")
  fixture.exec([[
    local path = ...
    vim.fn.writefile({ "{}" }, path)
    vim.api.nvim_exec_autocmds("BufWritePost", { pattern = path })
  ]], { path })
  assert(vim.wait(5000, function()
    return fixture.exec([[
      for _, buffer in ipairs(rule_buffers) do
        local found = false
        for _, diagnostic in ipairs(vim.diagnostic.get(buffer)) do
          if diagnostic.code == "RepeatedWords" then found = true end
        end
        if not found then return false end
      end
      return true
    ]])
  end, 10), "Removing a disabled rule did not restore diagnostics")
  assert(#fixture.exec("return text_errors") == 0, vim.inspect(fixture.exec("return text_errors")))
end, debug.traceback)
fixture.exec([[for _, client in ipairs(vim.lsp.get_clients({name="harper_ls"})) do client:stop(true) end]])
fixture.close()
vim.fn.delete(root, "rf")
assert(success, failure)
print("Harper rule integration: metadata action, two projects, persistence, reconnect, and re-enable passed")
vim.cmd("qa!")
