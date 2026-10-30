vim.loader.enable(false)
local blink_root = vim.fs.joinpath(vim.fn.stdpath("data"), "lazy", "blink.cmp")
assert(vim.fn.isdirectory(blink_root) == 1, "harness_file_ranking requires Blink")
vim.opt.runtimepath:append(blink_root)
local config = require("plugins.blink")[1].opts
require("blink.cmp.config").merge_with(config)
local fuzzy = require("blink.cmp.fuzzy")
local sorting = require("blink.cmp.fuzzy.sort")
local rank = require("forge.views.harness.completion.file_rank")
local source = require("forge.views.harness.completion.file_source")
local session = require("forge.session")
local original_system = vim.system
local root = vim.fs.joinpath(vim.fn.getcwd(), "ranking-workspace")
local skill = "rulesync-global/.rulesync/skills/rust/SKILL.md"
local candidate_paths = {
  "other/rust/example.lua", "src/rust_utils.lua", "src/rust.lua", "rust",
  skill, "src/robust.lua", "src/rusty/SKILL.md", "src/rust/deep/SKILL.md",
  "src/rust/skill_notes.md", "src/rust/SKILL.md.bak", "src/Rust.lua",
  "source/rust/guide/SKILL.md", "source/rust/guide/internal/SKILL.md",
  "source/rust/guidebook/SKILL.md", "docs/rust/index.md", "a/rust.rs", "b/rust.rs",
}

---@param query string
---@return table[]
local function search(query)
  local text = "inspect @" .. query
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { text })
  vim.api.nvim_win_set_cursor(0, { 1, #text })
  local response
  source.new():get_completions({}, function(result) response = result end)
  assert(vim.wait(1000, function() return response ~= nil end), "completion timed out")
  local candidates = response.items
  assert(#candidates == #candidate_paths, "ranking discarded source candidates")
  for _, candidate in ipairs(candidates) do
    candidate.source_id = "harness_files"
    candidate.source_name = "Workspace files"
  end
  return fuzzy.fuzzy(text, #text, { harness_files = candidates }, "prefix")
end

---@param results table[]
---@param path string
---@return integer
local function position(results, path)
  for index, item in ipairs(results) do
    if item.label == "@" .. path then return index end
  end
  error("missing candidate: " .. path .. " in " .. vim.inspect(results))
end

---@param results table[]
---@param first string
---@param second string
local function before(results, first, second)
  assert(position(results, first) < position(results, second), first .. " must rank before " .. second)
end

local ok, failure = xpcall(function()
  session.harness.session = { workspace = root }
  vim.bo.filetype = "ForgeHarnessInput"
  vim.o.virtualedit = "onemore"
  vim.system = function(_, options, callback)
    assert(options.cwd == session.harness.session.workspace)
    vim.schedule(function()
      callback({ code = 0, stdout = table.concat(candidate_paths, "\n") .. "\n", stderr = "" })
    end)
  end
  source._clear_cache()
  -- Exercise the real filtering/sorting boundary with both installed Blink engines.
  for _, implementation in ipairs({ "lua", "rust" }) do
    fuzzy.set_implementation(implementation)
    fuzzy.has_init_db = false
    fuzzy.haystacks_by_provider_cache = {}
    require("blink.cmp.config").fuzzy.frecency.enabled = false
    local results = search("rust")
    before(results, "rust", "src/rust.lua")
    before(results, "src/rust.lua", "src/rust_utils.lua")
    before(results, "src/rust_utils.lua", skill)
    before(results, skill, "src/robust.lua")
    before(results, "src/rust.lua", "src/Rust.lua")
    assert(results[position(results, skill)].textEdit.newText == "@" .. skill)

    results = search("rust/skill")
    assert(results[1].label == "@" .. skill, "trailing rust/SKILL.md did not rank first")
    before(results, skill, "src/rust/skill_notes.md")
    before(results, "src/rust/skill_notes.md", "src/rusty/SKILL.md")
    before(results, "src/rusty/SKILL.md", "src/rust/deep/SKILL.md")
    results = search("rust/guide/skill")
    before(results, "source/rust/guide/SKILL.md", "source/rust/guidebook/SKILL.md")
    before(results, "source/rust/guidebook/SKILL.md", "source/rust/guide/internal/SKILL.md")
    results = search("rust/")
    before(results, skill, "src/rust/deep/SKILL.md")
    results = search(skill)
    assert(results[1].label == "@" .. skill, "exact workspace path did not rank first")

    results = search("rust")
    local selected = results[position(results, "b/rust.rs")]
    local executed, completed = false, false
    source.new():execute({}, selected, function() completed = true end, function() executed = true end)
    assert(executed and completed, "selection hook bypassed Blink execution or acknowledgement")
    results = search("rust")
    before(results, "b/rust.rs", "a/rust.rs")
    before(results, "src/rust_utils.lua", skill)
    results = search("")
    assert(results[1].label == "@b/rust.rs", "bare @ ignored recent workspace selections")
    session.harness.session.workspace = root .. "-other"
    results = search("rust")
    before(results, "a/rust.rs", "b/rust.rs")
    session.harness.session.workspace = root

    results = search("RUST/SKILL")
    assert(results[1].label == "@" .. skill, "qualified matching was case sensitive")
    results = search("rust\\skill")
    assert(results[1].label == "@" .. skill, "Windows query separators changed the ranking")
  end

  local buffer = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(buffer, vim.fs.joinpath(root, skill))
  local results = search("")
  before(results, skill, "rust")
  vim.api.nvim_buf_delete(buffer, { force = true })

  -- Re-sort cached candidates after the query changes, without another source request.
  local candidates = search("rust")
  local text = "inspect @rust/skill"
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { text })
  vim.api.nvim_win_set_cursor(0, { 1, #text })
  local sorted = sorting.sort(candidates, config.fuzzy.sorts())
  assert(sorted[1].label == "@" .. skill, "cached completion ranks survived a changed query")
  local synthetic = {
    { source_id = "harness_files", label = "@" .. skill, score = -100000 },
    { source_id = "harness_files", label = "@src/rusty/SKILL.md", score = 100000 },
  }
  assert(sorting.sort(synthetic, config.fuzzy.sorts())[1].label == "@" .. skill,
    "generic score overrode semantic tiers")
  vim.bo.filetype = "lua"
  assert(vim.deep_equal(config.fuzzy.sorts(), { "score", "sort_text" }), "ordinary buffers changed sorting")
  vim.bo.filetype = "ForgeHarnessInput"
  vim.api.nvim_buf_set_lines(0, 0, -1, false, { "/model rust" })
  vim.api.nvim_win_set_cursor(0, { 1, #"/model rust" })
  assert(vim.deep_equal(config.fuzzy.sorts(), { "score", "sort_text" }), "slash completion changed sorting")
end, debug.traceback)
vim.system = original_system
source._clear_cache()
assert(ok, failure)
print("harness_file_ranking OK")
vim.cmd("qa!")
