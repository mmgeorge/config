vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")
local notices, recoveries = {}, 0
local owner = buffer.open("fold-failure", {
  notice = function(message) notices[#notices + 1] = message end,
  recover = function() recoveries = recoveries + 1 end,
})
vim.api.nvim_set_current_buf(owner.buffer)
local definition = { id = "fold", start = { row = 0, column = 0 },
  ["end"] = { block = "body", position = { row = 3, column = 0 } }, closed = false }
local metadata = { target = {}, decoration = {}, editable_region = {}, fold = { definition } }
local source = { document = owner.document, revision = 0, block = {
  { id = "body", text = { "heading", "body", "tail" }, metadata = metadata },
} }
assert(buffer.apply_snapshot(owner, source).kind == "Applied")
folds.attach(owner, vim.api.nvim_get_current_win())

local function inject(predicate, operation)
  local original = vim.cmd
  local injected = false
  vim.cmd = setmetatable({}, { __index = original, __call = function(_, command)
    if not injected and predicate(command) then
      injected = true
      error("injected native fold failure")
    end
    return original(command)
  end })
  local ok, result = pcall(operation)
  vim.cmd = original
  assert(injected, "failure injection did not reach the editor command")
  assert(ok, "native fold failure escaped document recovery: " .. tostring(result))
  assert(result.kind == "Desynchronized" and result.diagnostic:find("injected native fold failure", 1, true))
  assert(owner.status == "Desynchronized", "failed fold update stayed Applied")
  assert(not owner.applying and not vim.bo[owner.buffer].modifiable, "failed fold update left mutation enabled")
  assert(vim.wo.foldminlines == 1, "failed fold deletion changed the window's minimum fold size")
end

local cases = {
  { snapshot = false, predicate = function(command) return command == "normal! zD" end },
  { snapshot = false, predicate = function(command) return command:match("^%d+,%d+fold$") end },
  { snapshot = true, predicate = function(command) return command == "silent! normal! zE" end },
  { snapshot = true, predicate = function(command) return command:match("^%d+,%d+fold$") end },
}
for index, case in ipairs(cases) do
  inject(case.predicate, function()
    if case.snapshot then
      source.revision = owner.revision + 1
      return buffer.apply_snapshot(owner, source)
    end
    local replacement = vim.deepcopy(metadata)
    replacement.fold[1].closed = true
    return buffer.apply_patch(owner, {
      document = owner.document, base = owner.revision, next = owner.revision + 1,
      base_rows = 3, next_rows = 3, base_blocks = 1, next_blocks = 1,
      block_edit = {}, removed_block = {}, text_edit = {},
      metadata_edit = { { block = "body", row_count = 3, metadata = replacement } },
    })
  end)
  assert(#notices == index and recoveries == index, "fold failure was not surfaced and admitted to recovery exactly once")
  source.revision = owner.revision + 1
  assert(buffer.apply_snapshot(owner, source).kind == "Applied")
  assert(vim.fn.foldlevel(1) == 1 and vim.fn.foldlevel(3) == 1, "recovery left missing or duplicate folds")
end
buffer.close(owner)
print("fold failure recovery: patch deletion/creation and snapshot reset/creation passed")
vim.cmd("qa!")
