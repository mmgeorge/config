local config = vim.fn.getcwd()
vim.opt.runtimepath:prepend(config .. "/nvim")
local builder = require("forge.builder")
if vim.g.forge_manual_artifact_root then builder._set_artifact_root_for_test(vim.g.forge_manual_artifact_root) end
assert(vim.fn.executable(builder.binary_path()) == 1)
vim.o.swapfile = false
vim.o.wrap = true
vim.o.termguicolors = true
local fixture = vim.fn.tempname() .. "-forge-contention"
vim.fn.mkdir(fixture, "p")
local function git(arguments)
  local result = vim.system(vim.list_extend({ "git", "-C", fixture }, arguments), { text = true, timeout = 10000 }):wait()
  assert(result.code == 0, result.stderr)
end
git({ "init", "--quiet" })
git({ "config", "user.name", "Forge Fixture" })
git({ "config", "user.email", "forge@example.invalid" })
git({ "config", "core.autocrlf", "false" })
local function source(ordinal, changed)
  local rows = { "impl Source" .. ordinal .. " {", "    fn render(&self) {" }
  for row = 1, 500 do
    rows[#rows + 1] = ("        let value_%d = %d;"):format(row, changed and row % 31 == 0 and row + 1000 or row)
  end
  rows[#rows + 1], rows[#rows + 2] = "    }", "}"
  return rows
end
for ordinal = 1, 72 do vim.fn.writefile(source(ordinal, false), ("%s/source_%02d.rs"):format(fixture, ordinal)) end
if vim.g.forge_manual_edge_cases then
  vim.fn.writefile({ "fn deleted() {}", "fn retained_in_head() {}" }, fixture .. "/deleted.rs")
  vim.fn.writefile({ "ignored.txt" }, fixture .. "/.gitignore")
end
git({ "add", "." })
git({ "commit", "--quiet", "-m", "Manual contention fixture" })
for ordinal = 1, 72 do vim.fn.writefile(source(ordinal, true), ("%s/source_%02d.rs"):format(fixture, ordinal)) end
if vim.g.forge_manual_edge_cases then
  assert(vim.fn.delete(fixture .. "/deleted.rs") == 0)
  vim.fn.writefile({ "first new line", "second new line" }, fixture .. "/new file.txt")
  vim.fn.writefile({ "ignored content" }, fixture .. "/ignored.txt")
  local binary = assert(io.open(fixture .. "/binary.bin", "wb"))
  binary:write("before\0after")
  binary:close()
end
vim.fn.chdir(fixture)
local original_notify = vim.notify
_G.forge_manual = { fixture = fixture, notices = {}, config = config }
vim.notify = function(message, level, options)
  table.insert(forge_manual.notices, { message = tostring(message), level = level })
  original_notify(message, level, options)
end
require("forge").setup()
forge_manual.state = require("forge.views.commands").open()
vim.api.nvim_create_user_command("ForgeFixtureCompare", function()
  require("forge.client").request_host("repository.generate", {
    operation = "fingerprint", workspace = fixture, comparison = "head",
  }, function(result, failure)
    forge_manual.comparison = { result = result, failure = failure }
    if failure then vim.notify(failure, vim.log.levels.ERROR) end
  end)
end, {})
vim.api.nvim_create_user_command("ForgeFixtureAudit", function()
  local replica = forge_manual.state.replica
  local misplaced, count = {}, 0
  for id, handles in pairs(replica.marks) do
    local entry = replica.block[id]
    local _, start = replica.sequence:position(id)
    local expected = {}
    for _, gutter in ipairs(entry and entry.metadata and entry.metadata.gutter or {}) do
      expected[start + gutter.position.row] = true
    end
    for _, handle in ipairs(handles) do
      local mark = vim.api.nvim_buf_get_extmark_by_id(replica.buffer, replica.namespace, handle, { details = true })
      if mark[3] and mark[3].virt_text then
        count = count + 1
        if not expected[mark[1]] then misplaced[#misplaced + 1] = { block = id, row = mark[1], expected = start } end
      end
    end
  end
  local installed = 0
  for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(replica.buffer, replica.namespace, 0, -1, { details = true })) do
    if mark[4].virt_text then installed = installed + 1 end
  end
  local report = { fixture = fixture, gutters = count, installed = installed, orphans = installed - count, misplaced = misplaced, notices = forge_manual.notices, comparison = forge_manual.comparison }
  vim.fn.writefile({ vim.json.encode(report) }, builder.artifact_root() .. "/manual-status-audit.json")
  print(("gutters=%d misplaced=%d notices=%d"):format(count, #misplaced, #forge_manual.notices))
end, {})
